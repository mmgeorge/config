//! View-scoped input identifies a block target without parsing generated text.

use serde::{Deserialize, Serialize};

use crate::ContractError;
use crate::block::TextPosition;
use crate::identity::{BlockId, DocumentId, DocumentRevision, InputSequence, TargetId, ViewId};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DocumentInput {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: ViewId,
    pub sequence: InputSequence,
    pub action: String,
    pub block: BlockId,
    pub position: TextPosition,
    pub target: Option<TargetId>,
}

impl DocumentInput {
    pub fn validate(&self) -> Result<(), ContractError> {
        self.document.validate()?;
        self.revision.validate()?;
        self.view.validate()?;
        self.sequence.validate()?;
        self.block.validate()?;
        if let Some(target) = &self.target {
            target.validate()?;
        }
        if self.action.is_empty()
            || self.action.len() > 256
            || self.action.chars().any(char::is_control)
        {
            return Err(ContractError(
                "input action must contain 1..=256 bytes without controls",
            ));
        }
        Ok(())
    }
}
