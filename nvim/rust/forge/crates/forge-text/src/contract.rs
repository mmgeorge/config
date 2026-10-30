use crate::TextError;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Identify an isolated terminology profile within one host.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ProfileId(String);

impl ProfileId {
    /// Create a nonempty profile identity.
    pub fn new(value: String) -> Result<Self, TextError> {
        Self::try_from(value)
    }
    /// Get the wire identity without exposing mutation.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
impl TryFrom<String> for ProfileId {
    type Error = TextError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.trim().is_empty() {
            return Err(TextError::InvalidInput("empty profile identity".into()));
        }
        Ok(Self(value))
    }
}
impl From<ProfileId> for String {
    fn from(value: ProfileId) -> Self {
        value.0
    }
}

/// Name one lexical occurrence, independently of transport request identifiers.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct OccurrenceId(String);
impl TryFrom<String> for OccurrenceId {
    type Error = TextError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(TextError::InvalidInput("empty occurrence identity".into()));
        }
        Ok(Self(value))
    }
}
impl From<OccurrenceId> for String {
    fn from(value: OccurrenceId) -> Self {
        value.0
    }
}

/// Carry a half-open byte range, validated against its context before evaluation.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}
impl ByteRange {
    /// Validate ordering and Unicode boundaries against the owning string.
    pub(crate) fn validate(&self, context: &str) -> Result<(), TextError> {
        if self.start > self.end
            || self.end > context.len()
            || !context.is_char_boundary(self.start)
            || !context.is_char_boundary(self.end)
        {
            return Err(TextError::InvalidInput(
                "range is outside context or splits UTF-8".into(),
            ));
        }
        Ok(())
    }
    /// Check whether two ranges cover any common byte.
    pub(crate) fn overlaps(&self, other: &Self) -> bool {
        self.start < other.end && other.start < self.end
    }
}

/// Select external model files explicitly, without runtime downloads.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AssetPaths {
    pub unigram: PathBuf,
    pub bigram: PathBuf,
}

/// Initialize or replace one terminology profile using a shared immutable base model.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InitializeRequest {
    pub profile: ProfileId,
    #[serde(default)]
    pub terminology: Vec<String>,
    pub assets: Option<AssetPaths>,
}

/// Evaluate exactly one completed word using all available sentence context.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CorrectRequest {
    pub profile: ProfileId,
    pub profile_revision: String,
    pub occurrence: OccurrenceId,
    pub context: String,
    pub target: ByteRange,
    #[serde(default)]
    pub protected: Vec<ByteRange>,
}

/// Describe the exact immutable model and terminology selected by initialization.
#[derive(Clone, Debug, Serialize)]
pub struct ProfileReady {
    pub profile: ProfileId,
    pub profile_revision: String,
    pub model_revision: String,
    pub dictionary_revision: String,
}

/// Correlate a decision with its editor occurrence and immutable profile.
#[derive(Clone, Debug, Serialize)]
pub struct CorrectionIdentity {
    pub profile: ProfileId,
    pub profile_revision: String,
    pub occurrence: OccurrenceId,
    pub model_revision: String,
    pub dictionary_revision: String,
}

/// Explain only supported target-level changes, without introducing grammar rewriting.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CorrectionReason {
    Spelling,
    ContextualChoice,
    StandaloneI,
    FirstPersonContraction,
    SentenceStart,
}

/// Preserve text or propose one replacement while keeping all surrounding bytes authoritative.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "decision", rename_all = "snake_case")]
pub enum CorrectionDecision {
    Unchanged {
        #[serde(flatten)]
        identity: CorrectionIdentity,
    },
    Replace {
        #[serde(flatten)]
        identity: CorrectionIdentity,
        original: String,
        replacement: String,
        reasons: Vec<CorrectionReason>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_decisions_preserve_identity_and_reasons() {
        let identity = || CorrectionIdentity {
            profile: ProfileId::try_from("project".to_owned()).unwrap(),
            profile_revision: "profile-revision".into(),
            occurrence: OccurrenceId::try_from("occurrence".to_owned()).unwrap(),
            model_revision: "model-revision".into(),
            dictionary_revision: "dictionary-revision".into(),
        };
        let unchanged = serde_json::to_value(CorrectionDecision::Unchanged {
            identity: identity(),
        })
        .unwrap();
        assert_eq!(unchanged["decision"], "unchanged");
        assert_eq!(unchanged["occurrence"], "occurrence");
        assert!(unchanged.get("replacement").is_none());
        let replacement = serde_json::to_value(CorrectionDecision::Replace {
            identity: identity(),
            original: "recieve".into(),
            replacement: "Receive".into(),
            reasons: vec![
                CorrectionReason::Spelling,
                CorrectionReason::ContextualChoice,
                CorrectionReason::StandaloneI,
                CorrectionReason::SentenceStart,
            ],
        })
        .unwrap();
        assert_eq!(replacement["decision"], "replace");
        assert_eq!(replacement["profile"], "project");
        assert_eq!(
            replacement["reasons"],
            serde_json::json!([
                "spelling",
                "contextual_choice",
                "standalone_i",
                "sentence_start"
            ])
        );
        assert!(
            serde_json::from_value::<CorrectRequest>(serde_json::json!({
                "profile": "project", "profile_revision": "revision", "occurrence": "target",
                "context": "é", "target": {"start": 0, "end": 2}, "extra": true
            }))
            .is_err()
        );
    }
}
