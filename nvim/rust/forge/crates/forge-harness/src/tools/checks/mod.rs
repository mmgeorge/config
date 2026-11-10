mod cancel;
mod catalog;
mod definition_details;
mod definition_edit;
mod definition_list;
mod detail;
mod list;
mod run;
mod status;

pub(in crate::tools) use catalog::{definitions, execute, is_tool, record};

/// Name of the check-editing tool, which also names its toolset.
pub(in crate::tools) const DEFINITION_EDIT: &str = definition_edit::NAME;
