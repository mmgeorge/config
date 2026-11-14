mod catalog;
mod contract;
pub mod events;
mod interface;
mod process;
mod result;
mod rust_nextest;
mod script;

pub use catalog::{
    AdapterCatalogEntry, AdapterConfig, CategoryAdapter, DiagnosticAdapter, catalog,
};
pub use contract::{
    AdapterError, CheckDiscoverRequest, DiscoverMode, DiscoverRequest, Discovery, PathSelection,
    RunExit, TestItem, TestItemId, TestSelection,
};
pub use interface::{
    AdapterContext, BuildAdapter, CheckAdapter, FormatAdapter, LintAdapter, TestAdapter,
    TypeCheckAdapter,
};
pub use process::{AdapterProcess, AdapterProcesses, ProcessSpec};
pub use result::{AdapterProgress, AdapterResult};
pub use rust_nextest::{RustNextestAdapter, RustNextestOptions};
pub use script::{ScriptAdapter, ScriptAdapterConfig};
