//! Immutable source evidence for saved multi-file diffs, independent of source acquisition.

use crate::source::SourcePair;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct SavedDiffSource {
    pub text: Arc<String>,
    pub file: Vec<SavedFileSource>,
    pub diagnostic: Option<String>,
}

#[derive(Clone, Debug)]
pub struct SavedFileSource {
    pub old_path: Option<String>,
    pub new_path: Option<String>,
    pub source: Result<SourcePair, String>,
}
