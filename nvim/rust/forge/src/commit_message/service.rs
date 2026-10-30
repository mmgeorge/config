use super::context;

use std::time::Duration;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::{Context, Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_diff::syntax::SyntaxEngine;
use forge_git::store::RepositoryStore;
use forge_harness::{
    backend::TextGeneration,
    background::CancellationToken,
    service::{HarnessService, TextGenerationConfig},
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::{Notify, OwnedSemaphorePermit, Semaphore};

pub(crate) struct GenerationLease {
    request_id: u64,
    cancellation: CancellationToken,
    registry: Arc<Mutex<HashMap<u64, CancellationToken>>>,
}

impl Drop for GenerationLease {
    fn drop(&mut self) {
        self.registry
            .lock()
            .expect("generation cancellation registry poisoned")
            .remove(&self.request_id);
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CancelRequest {
    operation: String,
    request_id: u64,
}

pub(super) struct GenerationPermit {
    permit: Option<OwnedSemaphorePermit>,
    completed: Arc<Notify>,
}

impl Drop for GenerationPermit {
    fn drop(&mut self) {
        drop(self.permit.take());
        self.completed.notify_waiters();
    }
}

#[derive(Clone, Copy, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Comparison {
    /// Staged changes when present, otherwise non-ignored worktree changes.
    Status,
    Head,
    Staged,
    LatestCommit,
    /// Committed changes from the selected base's merge base to a pinned head.
    PullRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GenerationRequest {
    pub operation: String,
    pub workspace: String,
    pub comparison: Comparison,
    /// Base reference required by provider-free pull request context requests.
    pub base: Option<String>,
    /// Head reference pinned by the pull request workflow before context collection.
    pub head: Option<String>,
    /// Context byte allowance after the caller reserves its own instruction text.
    pub max_bytes: Option<usize>,
    #[serde(default)]
    pub ignored_paths: Vec<String>,
    pub generation: Option<TextGenerationConfig>,
    /// Complete PR instructions and evidence for Harness-owned generation.
    pub input: Option<String>,
}

#[derive(Clone, Serialize)]
/// Exact model input prepared without selecting or contacting a provider.
pub struct PromptPreview {
    pub system: &'static str,
    pub user: String,
    pub input_bytes: usize,
    pub limit_bytes: usize,
}

#[derive(Clone, Serialize)]
pub struct GenerationResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    pub state: String,
    pub message: Option<String>,
    pub source_requests: usize,
    pub diff_pairs: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt: Option<PromptPreview>,
}

/// Owns model generation and provider-free compact comparison context.
pub struct GenerationService {
    repository: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
    admission: Arc<Semaphore>,
    completed: Arc<Notify>,
    cancellation: Arc<Mutex<HashMap<u64, CancellationToken>>>,
}

impl GenerationService {
    pub fn new(
        repository: Arc<RepositoryStore>,
        diff: Arc<DiffEngine>,
        syntax: Arc<SyntaxEngine>,
    ) -> Result<Self> {
        Ok(Self {
            repository,
            diff,
            syntax,
            admission: Arc::new(Semaphore::new(4)),
            completed: Arc::new(Notify::new()),
            cancellation: Arc::default(),
        })
    }

    /// Register or cancel generation in transport order before request tasks can run.
    pub(crate) fn admit(
        &self,
        request: &forge_protocol::message::Request,
    ) -> Result<Option<GenerationLease>> {
        let mut registry = self
            .cancellation
            .lock()
            .expect("generation cancellation registry poisoned");
        match request.params.get("operation").and_then(Value::as_str) {
            Some("cancel") => {
                let cancel: CancelRequest = serde_json::from_value(request.params.clone())?;
                ensure!(
                    cancel.operation == "cancel",
                    "invalid generation cancellation operation"
                );
                if let Some(cancellation) = registry.get(&cancel.request_id) {
                    cancellation.cancel();
                }
                Ok(None)
            }
            Some("generate" | "pull_request") => {
                let cancellation = CancellationToken::new();
                ensure!(
                    !registry.contains_key(&request.id),
                    "duplicate generation request identity"
                );
                registry.insert(request.id, cancellation.clone());
                Ok(Some(GenerationLease {
                    request_id: request.id,
                    cancellation,
                    registry: Arc::clone(&self.cancellation),
                }))
            }
            _ => Ok(None),
        }
    }

    pub(crate) async fn dispatch(
        &self,
        params: Value,
        harness: &HarnessService,
        delivery: Option<(forge_protocol::outbound::MessageSender, u64)>,
        lease: Option<GenerationLease>,
    ) -> Result<GenerationResponse> {
        if params.get("operation").and_then(Value::as_str) == Some("cancel") {
            return Ok(GenerationResponse {
                session_id: None,
                state: "none".into(),
                message: None,
                source_requests: 0,
                diff_pairs: 0,
                prompt: None,
            });
        }
        let cancellation = lease
            .as_ref()
            .map(|lease| lease.cancellation.clone())
            .unwrap_or_default();
        ensure!(
            !cancellation.is_cancelled(),
            "Background generation cancelled"
        );
        let request: GenerationRequest = serde_json::from_value(params)?;
        let preview = request.operation == "preview";
        let context_only = request.operation == "context";
        let pull_request = request.operation == "pull_request";
        ensure!(
            request.operation == "generate" || preview || context_only || pull_request,
            "unknown detached generation operation"
        );
        ensure!(
            preview || context_only || request.generation.is_some(),
            "generation requires Harness provider configuration"
        );
        ensure!(
            !(preview || context_only) || request.generation.is_none(),
            "prompt preview does not accept provider configuration"
        );
        ensure!(
            (context_only || pull_request) == (request.comparison == Comparison::PullRequest),
            "pull request comparison requires PR context or generation"
        );
        ensure!(
            pull_request == request.input.is_some(),
            "only PR generation accepts prepared input"
        );
        if context_only {
            ensure!(
                request.base.is_some() && request.head.is_some(),
                "pull request context requires base and head"
            );
            ensure!(
                request
                    .max_bytes
                    .is_some_and(|limit| limit > 0 && limit <= super::excerpt::MAX_INPUT_BYTES),
                "invalid pull request context byte limit"
            );
        } else if pull_request {
            ensure!(
                request.head.is_some(),
                "PR generation requires the captured head"
            );
            ensure!(
                request.base.is_none() && request.max_bytes.is_none(),
                "PR generation accepts prepared evidence, not context options"
            );
        } else {
            ensure!(
                request.base.is_none() && request.head.is_none() && request.max_bytes.is_none(),
                "generation does not accept pull request context options"
            );
        }
        let _permit = Arc::new(GenerationPermit {
            permit: Some(
                Arc::clone(&self.admission)
                    .try_acquire_owned()
                    .context("detached generation admission is full or closed")?,
            ),
            completed: Arc::clone(&self.completed),
        });
        if pull_request {
            let input = request
                .input
                .context("PR generation requires prepared input")?;
            ensure!(
                !input.trim().is_empty(),
                "PR generation input cannot be empty"
            );
            let context = forge_harness::background::BackgroundContext::read(
                &self.repository,
                &request.workspace,
            )
            .await?;
            ensure!(
                context.head == request.head,
                "repository HEAD changed before PR generation; run the command again"
            );
            let generated = harness
                .background
                .generate(forge_harness::background::BackgroundRequest {
                    cancellation,
                    purpose: TextGeneration::PullRequestDescription,
                    config: request
                        .generation
                        .context("Missing Background provider configuration")?,
                    context,
                    input,
                    delivery,
                })
                .await?;
            return Ok(GenerationResponse {
                session_id: Some(generated.session_id),
                state: "ready".into(),
                message: Some(generated.text),
                source_requests: 0,
                diff_pairs: 0,
                prompt: None,
            });
        }
        let context =
            match context::collect(&self.repository, &self.diff, &self.syntax, &request).await {
                Ok(context) => context,
                Err(failure)
                    if context_only
                        && failure.is::<forge_git::revision::comparison::MergeConflict>() =>
                {
                    return Ok(GenerationResponse {
                        session_id: None,
                        state: "conflicts".into(),
                        message: None,
                        source_requests: 0,
                        diff_pairs: 0,
                        prompt: None,
                    });
                }
                Err(failure) if context::generation_was_superseded(&failure) => {
                    return Ok(GenerationResponse {
                        session_id: None,
                        state: "retry".into(),
                        message: None,
                        source_requests: 0,
                        diff_pairs: 0,
                        prompt: None,
                    });
                }
                Err(failure) => return Err(failure),
            };
        let mut response = GenerationResponse {
            session_id: None,
            state: if context.prompt.is_some() {
                "ready"
            } else {
                "none"
            }
            .into(),
            message: None,
            source_requests: context.source_requests,
            diff_pairs: context.diff_pairs,
            prompt: None,
        };
        if context.prompt.is_none() {
            return Ok(response);
        }
        let prompt = context.prompt.as_deref().unwrap_or_default();
        if preview || context_only {
            let system = if context_only {
                ""
            } else {
                forge_harness::backend::BACKGROUND_SYSTEM_MESSAGE
            };
            let user = if context_only {
                prompt.to_owned()
            } else {
                TextGeneration::CommitMessage.background_input(prompt)
            };
            response.prompt = Some(PromptPreview {
                system,
                input_bytes: system.len() + user.len(),
                user,
                limit_bytes: request.max_bytes.unwrap_or(super::excerpt::MAX_INPUT_BYTES),
            });
            return Ok(response);
        }
        let generated = harness
            .background
            .generate(forge_harness::background::BackgroundRequest {
                cancellation,
                purpose: TextGeneration::CommitMessage,
                config: request
                    .generation
                    .context("Missing Background provider configuration")?,
                context: context
                    .background
                    .context("Missing Background Git context")?,
                input: prompt.to_owned(),
                delivery,
            })
            .await?;
        response.session_id = Some(generated.session_id);
        let message = generated.text;
        response.message = Some(message);
        Ok(response)
    }

    pub fn close(&self) {
        self.admission.close();
    }

    pub async fn shutdown(&self, timeout: Duration) -> Result<()> {
        self.close();
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            let completed = self.completed.notified();
            tokio::pin!(completed);
            completed.as_mut().enable();
            if self.admission.available_permits() == 4 {
                return Ok(());
            }
            tokio::time::timeout_at(deadline, completed)
                .await
                .context("detached generation drain deadline exceeded")?;
        }
    }
}

#[cfg(test)]
mod cancellation_tests {
    use super::*;
    use forge_protocol::message::Request;
    use serde_json::json;

    #[test]
    fn cancellation_targets_admitted_request_before_execution_and_releases_registry() {
        let repository = Arc::new(RepositoryStore::default());
        let diff = DiffEngine::new(Default::default(), 1);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let service = GenerationService::new(repository, diff, syntax).unwrap();
        let request = |id, operation, target| -> Request {
            serde_json::from_value(json!({"id":id,"method":"repository.generation",
                "params": if operation == "cancel" { json!({"operation":operation,"request_id":target}) }
                    else { json!({"operation":operation}) }})).unwrap()
        };
        let first = service.admit(&request(1, "generate", 0)).unwrap().unwrap();
        let second = service.admit(&request(2, "generate", 0)).unwrap().unwrap();
        service.admit(&request(3, "cancel", 1)).unwrap();
        assert!(first.cancellation.is_cancelled());
        assert!(!second.cancellation.is_cancelled());
        drop(first);
        service.admit(&request(4, "cancel", 1)).unwrap();
        assert!(!second.cancellation.is_cancelled());
        drop(second);
        assert!(service.cancellation.lock().unwrap().is_empty());
    }
}
