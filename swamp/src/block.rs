#[derive(Debug, Clone)]
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
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '*');
        if valid {
            Ok(part.to_owned())
        } else {
            let err = format!(
                "`{}` is invalid type part (only alphanum, - or _ allowed)",
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
        let mut ty = None;
        let mut labels = Vec::new();
        for part in s.split(".") {
            Self::validate_part(part)?;

            if ty.is_none() {
                ty = Some(part.to_owned());
            } else {
                labels.push(part.to_owned());
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
    /// FIXME: sanitize
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ty)?;
        for label in self.labels.iter() {
            write!(f, ".{}", label)?;
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

    pub fn done(mut self) -> Result<(BlockType, Self), TypeError> {
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
