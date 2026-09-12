pub mod completion;
pub mod config_location;
pub mod content;
pub mod coordinator;
pub mod identity;
pub mod mutation;
pub mod read_pool;
pub mod reader;
pub mod repository;
pub mod revision;
pub mod snapshot;
pub mod store;
pub mod writer;

pub use identity::{
    GitStorageId, RepositoryIdentity, RepositoryPath, WorktreeId, discover_identity,
    resolve_argument, validate_path,
};
pub mod command;

#[cfg(test)]
extern crate self as forge_git;
#[cfg(test)]
#[path = "../tests/support/mod.rs"]
mod test_support;
