//! Source bytes retain their representation and newline state independently of generated rows.

use std::fmt;
use std::sync::{Arc, OnceLock};

use sha2::{Digest, Sha256};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Representation {
    Raw,
    GitCanonical,
    DisplayOnly,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SourceSide {
    Old,
    New,
}

/// Uses a zero-based source line and UTF-8 byte column, excluding line terminators.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceCoordinate {
    pub side: SourceSide,
    pub line: usize,
    pub byte_column: usize,
}

/// Counts LF-terminated source lines without adding a row for a terminal newline.
/// `crlf_count` is a subset of `newline_count`. A bare CR remains source content.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct NewlineMetadata {
    pub line_count: usize,
    pub newline_count: usize,
    pub crlf_count: usize,
    pub has_final_newline: bool,
}

/// Content hashes alone do not authorize sharing analyses across text representations.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SourceIdentity {
    pub content_hash: [u8; 32],
    pub representation: Representation,
}

/// Owns validated immutable UTF-8 content. Clones share the original byte allocation.
#[derive(Clone, Debug)]
pub struct SourceVersion {
    content: Arc<String>,
    identity: Arc<OnceLock<SourceIdentity>>,
    representation: Representation,
    newline: NewlineMetadata,
}

#[derive(Clone, Debug)]
pub struct SourcePair {
    pub old: SourceVersion,
    pub new: SourceVersion,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SourceError {
    Binary,
    UnsupportedEncoding,
    InconsistentMetadata,
    InconsistentIdentity,
}

impl fmt::Display for SourceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary => formatter.write_str("source contains binary NUL bytes"),
            Self::UnsupportedEncoding => formatter.write_str("source is not valid UTF-8"),
            Self::InconsistentMetadata => {
                formatter.write_str("source newline metadata does not match its bytes")
            }
            Self::InconsistentIdentity => {
                formatter.write_str("source content identity does not match its bytes")
            }
        }
    }
}

impl std::error::Error for SourceError {}

impl SourceIdentity {
    /// Hashes acquired bytes independently of text decoding, retaining their representation.
    pub fn from_bytes(content: &[u8], representation: Representation) -> Self {
        Self {
            content_hash: Sha256::digest(content).into(),
            representation,
        }
    }
}

impl SourceVersion {
    /// Admits immutable UTF-8 bytes and preserves line endings without computing a content hash.
    pub fn new(content: Vec<u8>, representation: Representation) -> Result<Self, SourceError> {
        if content.contains(&0) {
            return Err(SourceError::Binary);
        }
        let text = String::from_utf8(content).map_err(|_| SourceError::UnsupportedEncoding)?;
        Ok(Self::from_text(Arc::new(text), representation))
    }

    /// Shares validated content. The first identity request hashes it once across all clones.
    pub fn from_shared(
        content: Arc<String>,
        representation: Representation,
    ) -> Result<Self, SourceError> {
        if content.as_bytes().contains(&0) {
            return Err(SourceError::Binary);
        }
        Ok(Self::from_text(content, representation))
    }

    /// Constructs an empty source without a decoding or binary-content failure path.
    pub fn empty(representation: Representation) -> Self {
        Self::from_text(Arc::new(String::new()), representation)
    }

    fn from_text(content: Arc<String>, representation: Representation) -> Self {
        let newline = newline_metadata(content.as_bytes());
        Self {
            content,
            identity: Arc::new(OnceLock::new()),
            representation,
            newline,
        }
    }

    /// Validates supplied acquisition metadata before allowing it to identify an analysis.
    pub fn from_declared(
        content: Vec<u8>,
        identity: SourceIdentity,
        newline: NewlineMetadata,
    ) -> Result<Self, SourceError> {
        let source = Self::new(content, identity.representation)?;
        if source.identity() != identity {
            return Err(SourceError::InconsistentIdentity);
        }
        if source.newline != newline {
            return Err(SourceError::InconsistentMetadata);
        }
        Ok(source)
    }

    pub fn bytes(&self) -> &[u8] {
        self.content.as_bytes()
    }

    pub fn text(&self) -> &str {
        self.content.as_str()
    }

    pub fn identity(&self) -> SourceIdentity {
        *self
            .identity
            .get_or_init(|| SourceIdentity::from_bytes(self.bytes(), self.representation))
    }

    pub fn newline(&self) -> NewlineMetadata {
        self.newline
    }

    pub fn retained_bytes(&self) -> usize {
        self.content.capacity()
    }

    pub(crate) fn shares_bytes(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.content, &other.content)
    }
}

fn newline_metadata(content: &[u8]) -> NewlineMetadata {
    let newline_count = memchr::memchr_iter(b'\n', content).count();
    let has_final_newline = content.last() == Some(&b'\n');
    NewlineMetadata {
        line_count: newline_count + usize::from(!content.is_empty() && !has_final_newline),
        newline_count,
        crlf_count: memchr::memchr_iter(b'\r', content)
            .filter(|position| content.get(position + 1) == Some(&b'\n'))
            .count(),
        has_final_newline,
    }
}
