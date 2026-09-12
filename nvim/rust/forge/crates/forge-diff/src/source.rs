//! Source bytes retain their representation and newline state independently of generated rows.

use std::fmt;
use std::sync::{Arc, OnceLock};

use sha2::{Digest, Sha256};

pub const MAX_SOURCE_BYTES: usize = 8 * 1024 * 1024;

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
    content: Arc<Vec<u8>>,
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
    TooLarge { bytes: usize, limit: usize },
    Binary,
    UnsupportedEncoding,
    InconsistentMetadata,
    InconsistentIdentity,
}

impl fmt::Display for SourceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooLarge { bytes, limit } => write!(
                formatter,
                "source has {bytes} bytes, exceeding the {limit}-byte limit"
            ),
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
        Self::from_shared(Arc::new(content), representation)
    }

    /// Shares validated content. The first identity request hashes it once across all clones.
    pub fn from_shared(
        content: Arc<Vec<u8>>,
        representation: Representation,
    ) -> Result<Self, SourceError> {
        if content.capacity() > MAX_SOURCE_BYTES {
            return Err(SourceError::TooLarge {
                bytes: content.capacity(),
                limit: MAX_SOURCE_BYTES,
            });
        }
        validate_text(&content)?;
        let newline = newline_metadata(&content);
        Ok(Self {
            content,
            identity: Arc::new(OnceLock::new()),
            representation,
            newline,
        })
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
        &self.content
    }

    pub fn text(&self) -> &str {
        std::str::from_utf8(&self.content).expect("source construction validates UTF-8")
    }

    pub fn identity(&self) -> SourceIdentity {
        *self
            .identity
            .get_or_init(|| SourceIdentity::from_bytes(&self.content, self.representation))
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

/// Validates both sides without converting bytes or treating mixed representations as write-safe.
/// Mutation eligibility belongs to the repository owner, independently of analysis validity.
pub fn validate_source_pair(pair: &SourcePair) -> Result<(), SourceError> {
    validate_text(pair.old.bytes())?;
    validate_text(pair.new.bytes())?;
    Ok(())
}

fn validate_text(content: &[u8]) -> Result<(), SourceError> {
    if content.len() > MAX_SOURCE_BYTES {
        return Err(SourceError::TooLarge {
            bytes: content.len(),
            limit: MAX_SOURCE_BYTES,
        });
    }
    if content.contains(&0) {
        return Err(SourceError::Binary);
    }
    std::str::from_utf8(content).map_err(|_| SourceError::UnsupportedEncoding)?;
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_do_not_hash_sources_and_clones_share_lazy_identity() {
        let source = SourcePair {
            old: SourceVersion::new(b"first\nold\n".to_vec(), Representation::GitCanonical)
                .unwrap(),
            new: SourceVersion::new(
                b"first\nnew\nextra\n".to_vec(),
                Representation::GitCanonical,
            )
            .unwrap(),
        };
        let counts = crate::raw::compute_counts(&source);
        assert_eq!((counts.added, counts.deleted), (2, 1));
        assert!(source.old.identity.get().is_none());
        assert!(source.new.identity.get().is_none());
        let cloned = source.new.clone();
        let expected = SourceIdentity::from_bytes(cloned.bytes(), Representation::GitCanonical);
        assert_eq!(source.new.identity(), expected);
        assert_eq!(cloned.identity.get(), Some(&expected));
    }
}
