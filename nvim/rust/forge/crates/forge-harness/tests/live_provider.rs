//! Opt-in integration tests for installed provider CLIs and real model services.
//!
//! Every case is ignored because it depends on machine-local installations,
//! authentication, model availability, or configured skills and MCP servers.
//! Some cases make billable model requests and modify temporary repositories.
//! They cannot provide deterministic, offline unit-test coverage.
//!
//! `#[ignore]` keeps all cases compiled and discoverable without executing them
//! during normal runs or requiring a Cargo feature. Provider-specific modules
//! below belong to this single integration-test target, not the library suite.
//!
//! List cases from the Rust workspace without starting providers:
//! `cargo test --profile dev --locked -p forge-harness --test live_provider -- --list`
//!
//! After verifying a case's prerequisites and authorizing its external requests,
//! select it explicitly, for example:
//! `cargo test --profile dev --locked -p forge-harness --test live_provider codex_cli::lists_backend_owned_codex_model_metadata -- --ignored --exact --nocapture`
//! Do not add `--ignored` or `--include-ignored` to routine checks.

use forge_harness::backend::{BackendCatalogRequest, BackendInput, BackendRequest, PromptMode};
use forge_harness::session::ApprovalMode;
use std::path::Path;

#[path = "live_provider/catalog.rs"]
mod catalog;
#[path = "live_provider/codex_cli.rs"]
mod codex_cli;
#[path = "live_provider/codex_real.rs"]
mod codex_real;
#[path = "live_provider/copilot_real.rs"]
mod copilot_real;
#[path = "live_provider/text_generation.rs"]
mod text_generation;

fn catalog_request(workspace: &Path, session_id: &str) -> BackendCatalogRequest {
    BackendCatalogRequest {
        execution_owner: None,
        harness_session_id: session_id.into(),
        workspace: workspace.to_string_lossy().into_owned(),
        backend_session_id: None,
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
    }
}

fn prompt_request(request: &BackendCatalogRequest) -> BackendRequest {
    BackendRequest {
        execution_owner: request.execution_owner.clone(),
        harness_session_id: request.harness_session_id.clone(),
        workspace: request.workspace.clone(),
        input: BackendInput::from_text(""),
        mode: PromptMode::Chat,
        model: "default".into(),
        effort: "medium".into(),
        context_window: None,
        service_tier: Default::default(),
        access: request.access.clone(),
        approval_mode: request.approval_mode,
        backend_session_id: request.backend_session_id.clone(),
        submission: None,
        recovered_input: Vec::new(),
        control_context: None,
    }
}
