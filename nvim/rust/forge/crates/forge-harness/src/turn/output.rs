use serde::{Deserialize, Deserializer, Serialize, Serializer};
use sha2::{Digest, Sha256};
use std::{
    fmt,
    ops::Deref,
    sync::{Arc, OnceLock},
};

/// Shares immutable output between timeline snapshots and memoizes its content identity.
/// Mutation invalidates the identity before changing text. Clones retain their previous content.
#[derive(Clone, Debug, Default)]
pub struct ToolOutputText {
    text: Arc<String>,
    digest: Arc<OnceLock<String>>,
    hash: Sha256,
    characters: usize,
}

impl ToolOutputText {
    pub(crate) fn digest(&self) -> &str {
        self.digest
            .get_or_init(|| hex::encode(self.hash.clone().finalize()))
    }

    pub(crate) fn characters(&self) -> usize {
        self.characters
    }

    pub(crate) fn push_str(&mut self, text: &str) {
        if !text.is_empty() {
            self.digest = Arc::new(OnceLock::new());
            self.hash.update(text.as_bytes());
            self.characters += text.chars().count();
            Arc::make_mut(&mut self.text).push_str(text);
        }
    }

    pub(crate) fn replace(&mut self, text: &str) {
        if self.as_ref() != text {
            *self = text.to_owned().into();
        }
    }
}

impl From<String> for ToolOutputText {
    fn from(text: String) -> Self {
        let hash = Sha256::new().chain_update(text.as_bytes());
        let characters = text.chars().count();
        Self {
            text: Arc::new(text),
            digest: Arc::new(OnceLock::new()),
            hash,
            characters,
        }
    }
}

impl From<&str> for ToolOutputText {
    fn from(text: &str) -> Self {
        text.to_owned().into()
    }
}

impl Deref for ToolOutputText {
    type Target = str;
    fn deref(&self) -> &str {
        self.text.as_str()
    }
}

impl AsRef<str> for ToolOutputText {
    fn as_ref(&self) -> &str {
        self
    }
}

impl fmt::Display for ToolOutputText {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self)
    }
}

impl PartialEq for ToolOutputText {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

impl PartialEq<&str> for ToolOutputText {
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl PartialEq<String> for ToolOutputText {
    fn eq(&self, other: &String) -> bool {
        self.as_ref() == other
    }
}

impl Serialize for ToolOutputText {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self)
    }
}

impl<'de> Deserialize<'de> for ToolOutputText {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        String::deserialize(deserializer).map(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn output_identity_tracks_append_replace_and_shared_snapshots() {
        let mut output: ToolOutputText = "λ\0".into();
        let previous = output.clone();
        assert!(Arc::ptr_eq(&output.text, &previous.text));
        assert_eq!(output.digest(), crate::plan::digest("λ\0".as_bytes()));
        output.push_str("🙂");
        assert_eq!(previous.as_ref(), "λ\0");
        assert_eq!(output.characters(), 3);
        assert_eq!(output.digest(), crate::plan::digest("λ\0🙂".as_bytes()));
        output.replace("different");
        assert_eq!(output.digest(), crate::plan::digest(b"different"));
        let restored: ToolOutputText =
            serde_json::from_str(&serde_json::to_string(&output).unwrap()).unwrap();
        assert_eq!(restored, output);
        assert_eq!(restored.digest(), output.digest());
    }
}
