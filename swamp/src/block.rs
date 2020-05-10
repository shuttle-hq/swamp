use regex::Regex;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeError {
    /// Block is invalid type because a required field is missing
    Missing(&'static str),
    /// Malformed type from string
    Invalid(String),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Missing(ty) => write!(f, "malformed encoded block: missing {}", ty),
            Self::Invalid(reason) => write!(f, "invalid block type: {}", reason),
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug, Clone, Hash, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct BlockType {
    ty: String,
    labels: Vec<String>,
}

impl BlockType {
    pub fn build(ty: &str) -> Self {
        Self {
            ty: ty.to_string(),
            labels: Vec::new(),
        }
    }
    pub fn ty(&self) -> &str {
        &self.ty
    }
    pub fn labels(&self) -> &[String] {
        self.labels.as_slice()
    }
    pub fn new<R: AsRef<str>>(ty: &str, labels: &[R]) -> Self {
        Self {
            ty: ty.to_string(),
            labels: labels.iter().map(|r| r.as_ref().to_string()).collect(),
        }
    }
    pub fn matches_labels<R: AsRef<str>>(&self, labels: &[R]) -> bool {
        self.labels
            .iter()
            .zip(labels.iter().map(|l| l.as_ref()))
            .all(|(me, his)| me == his)
    }
    pub fn push_label(&mut self, label: &str) {
        self.labels.push(label.to_owned())
    }
    pub fn parse<B: Block>(s: &str) -> Result<(Self, TokenStream), TypeError> {
        Self::parse_stream::<B>(TokenStream::from_str(s))
    }
    pub fn parse_stream<B: Block>(s: TokenStream) -> Result<(Self, TokenStream), TypeError> {
        B::parse_block_type(s)
    }
    pub fn validate_part(part: &str) -> Result<String, TypeError> {
        let valid = part
            .chars()
            .all(|c| c.is_alphanumeric() || "*-_@.".contains(c));
        if valid {
            Ok(part.to_owned())
        } else {
            let err = format!(
                "`{}` is invalid type part (only alphanum, @, ., -, * or _ allowed)",
                part
            );
            Err(TypeError::Invalid(err))
        }
    }
    pub fn matches(&self, other: &Self) -> bool {
        let ty_matches = Self::part_matches(&self.ty, &other.ty);
        let labels_match = self
            .labels
            .iter()
            .zip(other.labels.iter())
            .all(|(l, r)| Self::part_matches(&l, &r));
        ty_matches && labels_match
    }
    pub fn part_matches(part: &str, other: &str) -> bool {
        part == "*" || other == "*" || part == other
    }

    pub fn into_iter(self) -> impl Iterator<Item = String> {
        std::iter::once(self.ty)
            .chain(self.labels.into_iter())
            .map(|s| s)
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.ty.as_str()).chain(self.labels.iter().map(|s| s.as_str()))
    }

    pub fn append(mut self, other: Self) -> Self {
        self.labels.extend(other.into_iter());
        self
    }
}
impl std::str::FromStr for BlockType {
    type Err = TypeError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let parts_regex =
            Regex::new(r#""(?P<first>[^"]+)"|(?P<second>[^.]+)"#).expect("Invalid regex");
        let mut ty = None;
        let mut labels = Vec::new();

        for part in parts_regex.captures_iter(s) {
            let full_part;
            if part.name("first").is_some() {
                full_part = &part["first"];
            } else {
                full_part = &part["second"];
            }
            Self::validate_part(full_part)?;

            if ty.is_none() {
                ty = Some(full_part.to_owned());
            } else {
                labels.push(full_part.to_owned());
            }
        }
        Ok(Self {
            ty: ty.ok_or(TypeError::Invalid(
                "empty string not a valid type".to_string(),
            ))?,
            labels,
        })
    }
}

impl std::fmt::Display for BlockType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.ty.contains(".") {
            true => write!(f, "\"{}\"", self.ty)?,
            false => write!(f, "{}", self.ty)?,
        }
        for label in self.labels.iter() {
            match label.contains(".") {
                true => write!(f, ".\"{}\"", label)?,
                false => write!(f, ".{}", label)?,
            }
        }
        Ok(())
    }
}

pub struct TokenStream<'a> {
    stream: std::iter::Peekable<std::str::Split<'a, &'a str>>,
    ty: Vec<&'a str>,
}

impl<'a> TokenStream<'a> {
    pub fn from_str(s: &'a str) -> Self {
        Self {
            stream: s.split(".").peekable(),
            ty: Vec::new(),
        }
    }

    /// peeks at the next element in the stream, without consuming it.
    /// It is an error if the next element does not exist.
    pub fn peek(&mut self) -> Result<&'a str, TypeError> {
        self.stream
            .peek()
            .ok_or(TypeError::Missing("token"))
            .map(|tok| *tok)
    }

    /// take the next element in the stream, no matter what it is. It is an error
    /// if the element does not exist.
    pub fn take(mut self) -> Result<Self, TypeError> {
        let tok = self.stream.next().ok_or(TypeError::Missing("token"))?;
        self.ty.push(tok);
        Ok(self)
    }

    /// Takes the next element in the stream and compare it with `tok`. If they
    /// match, it is consumed. Otherwise this fails.
    pub fn parse(mut self, tok: &str) -> Result<Self, TypeError> {
        self.stream
            .next()
            .and_then(|v| {
                if v == tok {
                    self.ty.push(v);
                    Some(())
                } else {
                    None
                }
            })
            .ok_or(TypeError::Invalid(tok.to_string()))?;
        Ok(self)
    }

    pub fn done(self) -> Result<(BlockType, Self), TypeError> {
        let mut iter = self.ty.into_iter();
        let ty = iter.next().ok_or(TypeError::Invalid("empty".to_string()))?;

        let mut bt = BlockType::build(ty);
        for label in iter {
            bt.push_label(label);
        }

        Ok((
            bt,
            Self {
                stream: self.stream,
                ty: Vec::new(),
            },
        ))
    }

    pub fn pass<L: Label>(self) -> Result<Self, TypeError> {
        L::parse_label(self)
    }
}

pub trait Label {
    fn label(&self, builder: &mut BlockType) -> Result<(), TypeError>;
    fn parse_label(s: TokenStream) -> Result<TokenStream, TypeError>;
}

pub trait Block {
    fn block_type(&self) -> Result<BlockType, TypeError>;
    fn parse_block_type(s: TokenStream) -> Result<(BlockType, TokenStream), TypeError>;
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::block_type;
    use pretty_assertions::assert_eq;
    use std::str::FromStr;

    #[test]
    fn test_block_type_from_str() {
        let expected_block_type = BlockType {
            ty: "resource".to_owned(),
            labels: vec!["user".to_string(), "john.smith@openquery.io".to_string()],
        };
        // Test from_str
        assert_eq!(
            expected_block_type,
            BlockType::from_str(r#"resource.user."john.smith@openquery.io""#).unwrap()
        );
        // Test invalid input for from_str
        assert_eq!(
            BlockType::from_str(r#"resource.user.john.smith@openquery.io""#),
            Err(TypeError::Invalid(
                "`io\"` is invalid type part (only alphanum, @, ., -, * or _ allowed)".to_owned()
            ))
        );
        // Test macro
        assert_eq!(
            expected_block_type,
            block_type!("resource"."user"."john.smith@openquery.io")
        );

        // Edge case where the input feeded in the macro is just one string
        assert_eq!(
            block_type!("resource.user.\"john.smith@openquery.io\""),
            BlockType {
                ty: "resource.user.\"john.smith@openquery.io\"".to_string(),
                labels: vec![],
            }
        );
    }

    #[test]
    fn test_block_type_display() {
        // No character escaping needed
        let block_type = BlockType {
            ty: "resource".to_owned(),
            labels: vec!["user".to_string(), "johnsmith".to_string()],
        };
        assert_eq!(format!("{}", block_type), "resource.user.johnsmith");

        // User's email address should be in quote as it contains periods
        let block_type = BlockType {
            ty: "resource".to_owned(),
            labels: vec!["user".to_string(), "john.smith@openquery.io".to_string()],
        };
        assert_eq!(
            format!("{}", block_type),
            "resource.user.\"john.smith@openquery.io\""
        );

        // Resource name also contains a period
        let block_type = BlockType {
            ty: "main.resource".to_owned(),
            labels: vec!["user".to_string(), "john.smith@openquery.io".to_string()],
        };
        assert_eq!(
            format!("{}", block_type),
            "\"main.resource\".user.\"john.smith@openquery.io\""
        );
    }

    #[test]
    fn test_block_type_roundtrip() {
        // No escaping
        assert_eq!(
            BlockType {
                ty: "resource".to_owned(),
                labels: vec!["user".to_string(), "john".to_string()],
            },
            block_type!("resource"."user"."john")
        );

        // Some escaping required
        assert_eq!(
            BlockType {
                ty: "resource".to_owned(),
                labels: vec!["user".to_string(), "john.smith@openquery.io".to_string()],
            },
            block_type!("resource"."user"."john.smith@openquery.io")
        );
    }
}
