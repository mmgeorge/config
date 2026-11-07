//! Opt-in integration tests for installed provider CLIs and real model services.
//!
//! Every case is ignored because it depends on machine-local installations,
//! authentication, model availability, or configured skills and MCP servers.
//! Some cases make billable model requests and modify temporary repositories.
//! They cannot provide deterministic, offline unit-test coverage.
//!
//! `#[ignore]` keeps all cases compiled and discoverable without executing them
//! during normal runs or requiring a Cargo feature. Provider-specific modules
//! below belong to this internal test module, using the production execution engine.
//!
//! List cases from the Rust workspace without starting providers:
//! `cargo test --profile dev --locked -p forge-harness --lib live_provider:: -- --list`
//!
//! After verifying a case's prerequisites and authorizing its external requests,
//! select it explicitly, for example:
//! `cargo test --profile dev --locked -p forge-harness --lib live_provider::codex_cli::lists_provider_owned_codex_model_metadata -- --ignored --exact --nocapture`
//! Do not add `--ignored` or `--include-ignored` to routine checks.

use crate::provider::test_support as collect;

use anyhow::Result;
use forge_harness::provider::{PromptMode, ProviderCatalogRequest, ProviderInput, ProviderRequest};
use forge_harness::provider::{Provider, approval::PermissionCoordinator};
use forge_harness::session::ApprovalMode;
use forge_harness::trace::TraceStore;
use std::{path::Path, sync::Arc};

#[path = "catalog.rs"]
mod catalog;
#[path = "codex_cli.rs"]
mod codex_cli;
#[path = "codex_real.rs"]
mod codex_real;
#[path = "copilot_real.rs"]
mod copilot_real;
#[path = "session_resume.rs"]
mod session_resume;
#[path = "text_generation.rs"]
mod text_generation;
#[path = "workflows.rs"]
mod workflows;

/// Select serializable host activity; receipt presentation stays with the production input owner.
fn event_observation(
    observation: forge_harness::provider::session_execution::OwnerObservation,
) -> Option<forge_harness::provider::HarnessEvent> {
    match observation {
        forge_harness::provider::session_execution::OwnerObservation::Event(event) => Some(event),
        forge_harness::provider::session_execution::OwnerObservation::InputAcceptance {
            ..
        }
        // The production collector commits physical facts separately from serializable activity.
        | forge_harness::provider::session_execution::OwnerObservation::HostFileChange { .. } => None,
    }
}

/// Inspect only the native observation inside an already resolved host publication.
fn native_event(
    event: &forge_harness::provider::HarnessEvent,
) -> Option<&forge_provider_api::ProviderEvent> {
    match event {
        forge_harness::provider::HarnessEvent::Native { event, .. } => Some(event),
        forge_harness::provider::HarnessEvent::Host { .. } => None,
    }
}

fn build_provider(
    kind: &str,
    command: Vec<String>,
    root: &Path,
    workspace: &Path,
) -> Result<Arc<Provider>> {
    let launch = forge_provider_api::ProviderLaunch {
        kind: forge_providers::registry::select(kind)?,
        command,
    };
    let native = forge_providers::registry::create(launch, root.to_owned())?;
    let permissions = PermissionCoordinator::transient(workspace)?;
    let trace = Arc::new(TraceStore::open(root)?);
    Ok(Arc::new(Provider::new(
        native,
        permissions,
        trace,
        root.to_owned(),
    )))
}

fn provider_session(
    kind: &str,
    command: Vec<String>,
    root: &Path,
    workspace: &Path,
) -> Result<collect::ProviderTestSession> {
    collect::ProviderTestSession::from_dyn(build_provider(kind, command, root, workspace)?)
}

fn catalog_request(workspace: &Path, session_id: &str) -> ProviderCatalogRequest {
    ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: session_id.into(),
        workspace: workspace.to_string_lossy().into_owned(),
        provider_session_id: None,
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
    }
}

fn prompt_request(request: &ProviderCatalogRequest) -> ProviderRequest {
    ProviderRequest {
        dispatch: forge_provider_api::submission::DispatchTracker::default(),
        execution_owner: request.execution_owner.clone(),
        harness_session_id: request.harness_session_id.clone(),
        workspace: request.workspace.clone(),
        input: ProviderInput::from_text(""),
        mode: PromptMode::Chat,
        model: "default".into(),
        effort: "medium".into(),
        context_window: None,
        service_tier: Default::default(),
        access: request.access.clone(),
        approval_mode: request.approval_mode,
        provider_session_id: request.provider_session_id.clone(),
        submission: None,
        acknowledged_child_reports: Vec::new(),
        recovered_input: Vec::new(),
        control_context: None,
    }
}
