extern crate proc_macro;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments,
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed,
    GenericArgument, LitStr, Meta, NestedMeta, Path, PathArguments, Type, TypePath, Variant,
};

use inflector::cases::snakecase::to_snake_case;
use inflector::Inflector;

macro_rules! let_fields {
    ($fmt:tt, $it:expr) => {
        $it.iter()
            .enumerate()
            .map(|(i, _)| format_ident!("{}_{}", $fmt, i))
            .collect::<Punctuated<Ident, Comma>>()
    };
}

fn extract_type_path(ty: Type) -> TypePath {
    match ty {
        Type::Path(typ) => {
            let TypePath {
                path: Path { ref segments, .. },
                ..
            } = &typ;
            let segment = segments.last().unwrap().clone();
            if segment.ident == format_ident!("Option") {
                match segment.arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        mut args,
                        ..
                    }) => match args.pop().unwrap().into_value() {
                        GenericArgument::Type(Type::Path(typ)) => typ,
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            } else {
                typ
            }
        }
        _ => unimplemented!(),
    }
}

#[derive(Default)]
struct MergePolicy {
    /// Whether the field can be changed through merging or not.
    /// Trying to merge into an immutable field triggers `MergeError::Immutable`.
    immutable: bool,
}

#[inline]
fn take_ident(path: Path) -> Ident {
    path.segments.into_iter().next().unwrap().ident
}

impl From<Vec<Attribute>> for MergePolicy {
    fn from(attrs: Vec<Attribute>) -> Self {
        let mut policy = Self::default();
        for attr in attrs {
            match attr.parse_meta().unwrap() {
                Meta::List(meta_list) => {
                    if take_ident(meta_list.path) == format_ident!("merge") {
                        for nested in meta_list.nested.into_iter() {
                            match nested {
                                NestedMeta::Meta(Meta::Path(path)) => {
                                    if take_ident(path) == format_ident!("immutable") {
                                        policy.immutable = true;
                                    }
                                }
                                _ => panic!("not a valid merge policy"),
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        policy
    }
}

#[proc_macro_derive(Merge)]
pub fn derive_merge(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let mut soft_output = proc_macro2::TokenStream::new();
    let mut hard_output = proc_macro2::TokenStream::new();
    match input.data {
        Data::Struct(data_struct) => match data_struct.fields {
            Fields::Named(fields_named) => {
                soft_output.extend(quote! {
                    if self == Self::default() {
                        Ok(other)
                    } else {
                        Err(
                            swamp::MergeError::Immutable
                        )
                    }
                });
                for field in fields_named.named.into_iter() {
                    let policy = MergePolicy::from(field.attrs);
                    let ident = field.ident.unwrap();
                    let nested_merge_op = if policy.immutable {
                        quote! { self.#ident = self.#ident.soft_merge(other.#ident)?; }
                    } else {
                        quote! { self.#ident = self.#ident.hard_merge(other.#ident)?; }
                    };
                    hard_output.extend(nested_merge_op);
                }
                hard_output.extend(quote! { Ok(self) })
            }
            _ => unimplemented!(),
        },
        Data::Enum(data_enum) => {
            soft_output.extend(quote! {
                Err(swamp::MergeError::Incompatible)
            });
            let mut arms = proc_macro2::TokenStream::new();
            for variant in data_enum.variants.into_iter() {
                let ident = variant.ident;
                match variant.fields {
                    Fields::Unnamed(fields_unnamed) => {
                        let mine = let_fields!("me", fields_unnamed.unnamed);
                        let his = let_fields!("his", fields_unnamed.unnamed);
                        let merged = mine
                            .iter()
                            .zip(his.iter())
                            .map(|(m, h)| {
                                quote! { #m.hard_merge(#h)?, }
                            })
                            .collect::<proc_macro2::TokenStream>();
                        arms.extend(quote! {
                            Self::#ident(#mine) => {
                                match other {
                                    Self::#ident(#his) => Ok(Self::#ident(#merged)),
                                    _ => Err(swamp::MergeError::Incompatible)
                                }
                            },
                        })
                    }
                    Fields::Unit => arms.extend(quote! {
                        Self::#ident => {
                            match other {
                                Self::#ident => Ok(self),
                                _ => Err(swamp::MergeError::Incompatible)
                            }
                        },
                    }),
                    Fields::Named(fields_named) => unimplemented!("named field variants"),
                }
            }
            hard_output.extend(quote! {
                match self {
                    #arms
                }
            })
        }
        Data::Union(_) => panic!("unions are not supported"),
    };
    let output_stream = quote! {
        impl swamp::Merge for #ident {
            fn soft_merge(mut self, other: Self) -> Result<Self, swamp::MergeError> {
                #soft_output
            }
            fn hard_merge(mut self, other: Self) -> Result<Self, swamp::MergeError> {
                #hard_output
            }
        }
    };
    output_stream.into()
}

fn derive_fields<'a, I>(mut fields: I) -> proc_macro2::TokenStream
where
    I: Iterator<Item = &'a Ident>,
{
    let mut output = proc_macro2::TokenStream::new();
    for field in fields {
        output.extend(quote! {
            #field.digest(hasher);
        });
    }
    output
}

#[proc_macro_derive(Compress)]
pub fn derive_compress(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;

    let digested_fields = match input.data {
        Data::Struct(data_struct) => {
            let mut output = proc_macro2::TokenStream::new();
            match data_struct.fields {
                Fields::Unnamed(fields_unnamed) => {
                    for (i, _) in fields_unnamed.unnamed.iter().enumerate() {
                        output.extend(quote! {
                            self.#i.digest(hasher);
                        });
                    }
                }
                Fields::Named(fields_named) => {
                    for field in fields_named.named.into_iter() {
                        let ident = field.ident.unwrap();
                        output.extend(quote! {
                            self.#ident.digest(hasher);
                        });
                    }
                }
                Fields::Unit => {}
            }
            output
        }
        Data::Enum(data_enum) => {
            let mut output = proc_macro2::TokenStream::new();
            for (i, variant) in data_enum.variants.into_iter().enumerate() {
                let variant_ident = variant.ident;
                match variant.fields {
                    Fields::Named(fields_named) => {
                        let let_fields = fields_named
                            .named
                            .into_iter()
                            .map(|f| f.ident.unwrap())
                            .collect::<Punctuated<Ident, Comma>>();
                        let derived_fields = derive_fields(let_fields.iter());
                        output.extend(quote! {
                            Self::#variant_ident { #let_fields } => {
                                hasher.input(#i.to_ne_bytes());
                                #derived_fields
                            },
                        });
                    }
                    Fields::Unnamed(fields_unnamed) => {
                        let let_fields = let_fields!(variant_ident, fields_unnamed.unnamed);
                        let derived_fields = derive_fields(let_fields.iter());
                        output.extend(quote! {
                            Self::#variant_ident(#let_fields) => {
                                hasher.input(#i.to_ne_bytes());
                                #derived_fields
                            },
                        });
                    }
                    Fields::Unit => output.extend(quote! {
                        Self::#variant_ident => {
                            hasher.input(#i.to_ne_bytes());
                        },
                    }),
                }
            }
            quote! {
                match self {
                    #output
                }
            }
        }
        _ => panic!("union structs are not supported"),
    };

    let output_stream = quote! {
        impl swamp::Compress for #ident {
            fn digest<I: digest::Input>(&self, hasher: &mut I) {
                #digested_fields
            }
        }
    };

    output_stream.into()
}

#[proc_macro_derive(Label)]
pub fn derive_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident.clone();
    let (label_output, parse_label_output) = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { mut named, .. }),
            ..
        }) => {
            let mut output = None;
            for Field { ident, .. } in named.iter() {
                if *ident.as_ref().unwrap() == format_ident!("name") {
                    output = Some((
                        quote! {
                            builder.push_label(&self.name);
                            Ok(())
                        },
                        quote! {
                            chunks.take()
                        },
                    ));
                }
            }
            output.unwrap_or_else(|| {
                // struct is passthrough (such as that generated by prost for oneofs)
                // it is essentially ignored in deriving Label
                if named.len() != 1 {
                    // This is a hack; instead it should trigger by looking up an attribute
                    // like #[swamp(leaf)] or something
                    let label_as_str = ident.to_string().to_snake_case();
                    (
                        quote! {
                            builder.push_label(#label_as_str);
                            Ok(())
                        },
                        quote! {
                            chunks.parse(#label_as_str)
                        },
                    )
                } else {
                    let field = named.pop().unwrap().into_value();
                    let field_ident = field.ident.unwrap();
                    let field_type = extract_type_path(field.ty);
                    let field_ident_as_str = field_ident.to_string();
                    (
                        quote! {
                            self.#field_ident
                                .as_ref()
                                .ok_or(swamp::block::TypeError::Missing(#field_ident_as_str))?
                                .label(builder)
                        },
                        quote! {
                            chunks.pass::<#field_type>()
                        },
                    )
                }
            })
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let mut label_output = Punctuated::<proc_macro2::TokenStream, Comma>::new();
            let mut parse_label_output = Punctuated::<proc_macro2::TokenStream, Comma>::new();
            for Variant { ident, fields, .. } in variants.into_iter() {
                let ident_snake_case_as_str =
                    LitStr::new(&ident.to_string().to_snake_case(), Span::call_site());
                match fields {
                    Fields::Unnamed(FieldsUnnamed { mut unnamed, .. }) => {
                        let field = unnamed.pop().unwrap().into_value();
                        let field_type = extract_type_path(field.ty);
                        if !unnamed.is_empty() {
                            panic!("unnamed variants with more than one field not supported")
                        }
                        label_output.push(quote! {
                            Self::#ident(v) => {
                                builder.push_label(#ident_snake_case_as_str);
                                v.label(builder)?;
                                Ok(())
                            }
                        });
                        parse_label_output.push(quote! {
                            #ident_snake_case_as_str => {
                                chunks.parse(#ident_snake_case_as_str)?
                                    .pass::<#field_type>()
                            }
                        });
                    }
                    _ => panic!("derive Scoped only works on enums with unnamed variants"),
                }
            }
            (
                quote! {
                    match self {
                        #label_output
                    }
                },
                quote! {
                    let next = chunks.peek()?;
                    match next {
                        #parse_label_output,
                        _ => Err(swamp::TypeError::Invalid(next.to_string()))
                    }
                },
            )
        }
        _ => panic!("unsupported input data type"),
    };

    let ident = input.ident;

    (quote! {
        impl swamp::block::Label for #ident {
            fn label(
                &self,
                builder: &mut swamp::BlockType
            ) -> std::result::Result<(), swamp::TypeError> {
                #label_output
            }

            fn parse_label(
                mut chunks: swamp::TokenStream
            ) -> Result<swamp::TokenStream, swamp::TypeError> {
                #parse_label_output
            }
        }
    })
    .into()
}

#[proc_macro_derive(Block)]
pub fn derive_block(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { mut named, .. }),
            ..
        }) => {
            let (block_type, parse_block_type) = if named.len() != 1 {
                // assuming leaf block, again an assumption that should be confirmed by the
                // user using attributes
                let ident_as_str = ident.to_string().to_snake_case();
                (
                    quote! {
                        Ok(swamp::BlockType::build(#ident_as_str))
                    },
                    quote! {
                        stream.parse(#ident_as_str)?.done()
                    },
                )
            } else {
                // assuming passthrough struct (such as that generated by prost for oneof)
                // probably an assumption that should be confirmed by attributes,
                // but we'll roll with it for now
                let field = named.pop().unwrap().into_value();
                let field_ident = field.ident.unwrap();
                let field_type = extract_type_path(field.ty);
                let field_ident_as_str = field_ident.to_string();
                (
                    quote! {
                        let mut out = swamp::BlockType::build(#field_ident_as_str);
                        let inner = self.#field_ident
                            .as_ref()
                            .ok_or(swamp::TypeError::Missing(#field_ident_as_str))?;
                        inner.label(&mut out)?;
                        Ok(out)
                    },
                    quote! {
                        stream
                            .parse(#field_ident_as_str)?
                            .pass::<#field_type>()?
                            .done()
                    },
                )
            };
            quote! {
                impl swamp::Block for #ident {
                    fn block_type(
                        &self
                    ) -> Result<swamp::BlockType, swamp::TypeError> {
                        #block_type
                    }
                    fn parse_block_type(
                        stream: swamp::TokenStream
                    ) -> Result<(swamp::BlockType, swamp::TokenStream), swamp::TypeError> {
                        #parse_block_type
                    }
                }
            }
        }
        _ => {
            // unsupported type for block derive... but silently fails for now due to
            // not having a way to single out dependent type in prost
            quote! {}
        }
    }
    .into()
}
