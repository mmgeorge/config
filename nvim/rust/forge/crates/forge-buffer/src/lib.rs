//! Versioned generated text shared by Forge features and the Neovim replica.
//!
//! Coordinates use zero-based rows and UTF-8 byte columns. Source-file newline
//! metadata never enters this contract.

pub mod admission;
pub mod block;
pub mod document;
pub mod editable;
pub mod identity;
pub mod input;
pub mod markdown;
pub mod patch;
pub mod sequence;
pub mod text;
pub mod view;
pub mod width;

use std::fmt;

pub const MAX_COUNTER: u64 = (1 << 53) - 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractError(pub &'static str);

impl fmt::Display for ContractError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.0)
    }
}

impl std::error::Error for ContractError {}
