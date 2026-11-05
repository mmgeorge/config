use super::super::{HarnessService, InitializeRequest};
use forge_diff::cache::CacheLimits;
use forge_diff::engine::DiffEngine;
use forge_diff::syntax::SyntaxEngine;
use forge_git::store::RepositoryStore;
use serde_json::json;
use std::sync::Arc;

/// Create the isolated service runtime used by owner workflows.
pub(super) fn service() -> HarnessService {
    let diff = DiffEngine::new(CacheLimits::default(), 1);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
    HarnessService::new(Arc::new(RepositoryStore::default()), diff, syntax)
}

/// Create mock session settings rooted in the scenario directory.
pub(super) fn initialize(fixture: &tempfile::TempDir, command: &str) -> InitializeRequest {
    serde_json::from_value(json!({
        "workspace":fixture.path(),
        "data_root":fixture.path().join("data"),
        "client_id":"service-test",
        "provider":{"kind":"mock","command":[command]},
    }))
    .unwrap()
}
