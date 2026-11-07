use std::sync::Arc;

use super::mutation_error::{MutationError, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::service::LspScope;
use crate::tools::{ControlToolDefinition, ControlToolInvocation, ControlToolResult};

pub(super) const SYMBOL: &str = "lsp_rename_symbol";
pub(super) const FILE: &str = "lsp_rename_file";

/// Describe symbol and resource renames without exposing arbitrary server commands.
pub(super) fn definitions() -> Vec<ControlToolDefinition> {
    let (mut fields, _) = super::schema::base_schema("definition");
    fields.remove("limit");
    fields.remove("offset");
    fields.remove("token_budget");
    fields.insert("new_name".into(), json!({"type":"string","minLength":1}));
    vec![
        ControlToolDefinition {
            name: SYMBOL,
            description: "Rename a symbol using its language server. Accepts the same exact target selectors as lsp_definition. Applies all returned text and file operations to disk. Requires existing write permission for every affected path; fails without prompting on denied access or unsaved affected buffers.",
            input_schema: json!({"type":"object","additionalProperties":false,"required":["new_name"],"properties":fields}),
        },
        ControlToolDefinition {
            name: FILE,
            description: "Move a file or directory and update references using language servers. Requires willRenameFiles support, a vacant destination, existing read/write permission on all affected paths, and clean affected buffers. No permission prompts or plain filesystem fallback.",
            input_schema: json!({"type":"object","additionalProperties":false,"required":["old_path","new_path"],"properties":{
                "old_path":{"type":"string","minLength":1},"new_path":{"type":"string","minLength":1}
            }}),
        },
    ]
}

/// Identify workspace-mutating LSP tools independently of read-only queries.
pub(crate) fn is_mutation(name: &str) -> bool {
    matches!(name, SYMBOL | FILE)
}

/// Exclude mutations from every read-only interaction mode.
pub(crate) fn allowed_mode(mode: crate::provider::PromptMode) -> bool {
    use crate::provider::PromptMode;
    match mode {
        PromptMode::Chat
        | PromptMode::ExecutePlan
        | PromptMode::GoalContinuation
        | PromptMode::RequestChanges => true,
        PromptMode::Background
        | PromptMode::Plan
        | PromptMode::PlanDiscussion
        | PromptMode::PlanQuestion => false,
    }
}

/// Carry only the fields required by each editor preparation operation.
#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(super) enum Request {
    Symbol {
        target: super::request::Target,
        new_name: String,
    },
    File {
        old_path: String,
        new_path: String,
    },
}

/// Decode public arguments through the existing exact target validator.
fn parse(name: &str, mut value: Value) -> Result<Request> {
    if name == SYMBOL {
        let new_name = value
            .as_object_mut()
            .ok_or(MutationError::Argument("expected object"))?
            .remove("new_name")
            .and_then(|value| value.as_str().map(str::to_owned))
            .ok_or(MutationError::Argument("new_name is required"))?;
        if new_name.trim().is_empty() {
            return Err(MutationError::Argument("new_name must be nonempty"));
        }
        super::request::Arguments::parse("lsp_definition", value.clone())
            .map_err(MutationError::Service)?;
        Ok(Request::Symbol {
            target: serde_json::from_value(value)?,
            new_name,
        })
    } else {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Paths {
            old_path: String,
            new_path: String,
        }
        let Paths { old_path, new_path } = serde_json::from_value(value)?;
        if old_path.trim().is_empty() || new_path.trim().is_empty() {
            return Err(MutationError::Argument("paths must be nonempty"));
        }
        Ok(Request::File { old_path, new_path })
    }
}

/// Keep editor holds paired with release across errors and caller cancellation.
pub(super) struct EditorHold {
    pub(super) scope: Arc<LspScope>,
    pub(super) id: String,
    pub(super) committed: bool,
}

impl Drop for EditorHold {
    fn drop(&mut self) {
        self.scope.release_mutation(&self.id, self.committed);
    }
}

/// Apply one authorized rename, retaining ownership through commit and rollback.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: ControlToolInvocation,
) -> crate::tools::ToolResult<ControlToolResult> {
    crate::tools::error::ensure!(
        allowed_mode(runtime.context.mode) && !runtime.context.question_discussion,
        "access_denied: LSP rename is unavailable in a read-only interaction."
    );
    let scope = runtime.context.lsp.clone().ok_or(MutationError::Access(
        "LSP tools require an enabled active editor interaction",
    ))?;
    let original_scope =
        runtime
            .context
            .question_invocation
            .clone()
            .ok_or(MutationError::Access(
                "Rename requires its original authenticated host invocation.",
            ))?;
    let call_id = invocation.call_id.clone().ok_or(MutationError::Access(
        "Rename requires its original authenticated host call.",
    ))?;
    let name = invocation.name.clone();
    let arguments = invocation.arguments;
    let request = parse(&name, arguments.clone())?;
    let policy = scope
        .mutation_policy
        .read()
        .map_err(|_| MutationError::Poisoned)?
        .clone()
        .ok_or(MutationError::Access("Rename has no write authority."))?;
    if policy.access.write_access == crate::session::WriteScope::Off {
        return Err(MutationError::Access("Write permissions are disabled.").into());
    }
    scope.check().map_err(MutationError::Service)?;
    let guard = scope.service.mutations.clone().lock_owned().await;
    let mut request = request;
    match &mut request {
        Request::Symbol { target, .. } => {
            if let Some(path) = target.path_mut() {
                *path = super::transaction::authorize(&scope, &policy, path, false)?
                    .to_string_lossy()
                    .into_owned();
            }
        }
        Request::File { old_path, new_path } => {
            *old_path = super::transaction::authorize(&scope, &policy, old_path, true)?
                .to_string_lossy()
                .into_owned();
            *new_path = super::transaction::authorize(&scope, &policy, new_path, true)?
                .to_string_lossy()
                .into_owned();
        }
    }
    let id = uuid::Uuid::new_v4().to_string();
    let hold = EditorHold {
        scope: scope.clone(),
        id: id.clone(),
        committed: false,
    };
    let prepared = scope
        .mutation_request(&id, serde_json::to_value(request)?)
        .await
        .map_err(MutationError::Service)?;
    let prepared: super::transaction::Prepared =
        serde_json::from_value(prepared).map_err(MutationError::Json)?;
    let publication = runtime.context.provider_completion.clone();
    let live = runtime.context.live_host_effects.clone();
    let observed = runtime.context.observed_file_changes.clone();
    let live_scope = original_scope.clone();
    let live_call = call_id.clone();
    // The worker owns both guards: dropping the provider future cannot release buffers during a write.
    let (result, batch) = runtime
        .lsp_mutations
        .run(
            move || -> Result<(String, crate::provider::HostEffectBatch)> {
                let _guard = guard;
                let mut hold = hold;
                let mut transaction =
                    super::transaction::Transaction::prepare(&scope, &policy, prepared)?;
                scope.check().map_err(MutationError::Service)?;
                transaction.commit()?;
                hold.committed = true;
                let mut completion = crate::provider::ProviderCompletion::default();
                completion.host_file_changes.push(super::HostFileChange {
                    scope: original_scope,
                    call_id,
                    native_item_id: None,
                    changes: transaction.changes(),
                });
                completion.native.evidence.workspace_changed = transaction.changed();
                completion.native.evidence.tool_called = true;
                let batch = publication
                    .publish_batch(completion)
                    .map_err(MutationError::Publication)?;
                Ok((transaction.summary(), batch))
            },
        )
        .await?;
    if let Some(observed) = observed {
        for change in &batch.completion.host_file_changes {
            observed
                .send_host_file_change(change.clone())
                .map_err(|error| MutationError::Publication(error.into()))?;
        }
    } else if let Some(live) = live {
        live.commit(live_scope, live_call, batch)
            .await
            .map_err(MutationError::Publication)?;
    }
    Ok(ControlToolResult {
        invocation: None,
        message: result,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn validates_rename_inputs_and_read_only_modes() {
        assert!(parse(SYMBOL, json!({"symbol":"Store","new_name":"Shelf"})).is_ok());
        assert!(parse(SYMBOL, json!({"path":"a.rs","line":1,"new_name":"Shelf"})).is_err());
        assert!(parse(SYMBOL, json!({"symbol":"Store","new_name":" "})).is_err());
        assert!(
            parse(
                FILE,
                json!({"old_path":"a","new_path":"b","overwrite":true})
            )
            .is_err()
        );
        assert!(!allowed_mode(crate::provider::PromptMode::Plan));
        assert!(!allowed_mode(crate::provider::PromptMode::PlanDiscussion));
        assert!(!allowed_mode(crate::provider::PromptMode::PlanQuestion));
        assert!(allowed_mode(crate::provider::PromptMode::Chat));
    }
}
