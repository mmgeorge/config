mod context;
mod provider;

use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_git::store::RepositoryStore;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::{Notify, OwnedSemaphorePermit, Semaphore};

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
    Head,
    Staged,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ModelSpec {
    pub provider: String,
    pub model: String,
    pub thinking: Option<Value>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GenerationRequest {
    pub operation: String,
    pub workspace: String,
    pub comparison: Comparison,
    #[serde(default)]
    pub ignored_paths: Vec<String>,
    pub model: Option<ModelSpec>,
}

#[derive(Clone, Serialize)]
pub struct GenerationResponse {
    pub state: String,
    pub message: Option<String>,
    pub source_requests: usize,
    pub diff_pairs: usize,
}

pub struct CommitMessageService {
    repository: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    http: reqwest::Client,
    admission: Arc<Semaphore>,
    completed: Arc<Notify>,
}

impl CommitMessageService {
    pub fn new(repository: Arc<RepositoryStore>, diff: Arc<DiffEngine>) -> Result<Self> {
        Ok(Self {
            repository,
            diff,
            http: reqwest::Client::builder()
                .timeout(Duration::from_secs(120))
                .redirect(reqwest::redirect::Policy::none())
                .pool_max_idle_per_host(4)
                .build()?,
            admission: Arc::new(Semaphore::new(4)),
            completed: Arc::new(Notify::new()),
        })
    }

    pub async fn dispatch(&self, params: Value) -> Result<GenerationResponse> {
        let request: GenerationRequest = serde_json::from_value(params)?;
        ensure!(
            request.operation == "generate",
            "unknown detached generation operation"
        );
        let permit = Arc::new(GenerationPermit {
            permit: Some(
                Arc::clone(&self.admission)
                    .try_acquire_owned()
                    .context("detached generation admission is full or closed")?,
            ),
            completed: Arc::clone(&self.completed),
        });
        let context = match context::collect(&self.repository, &self.diff, &request).await {
            Ok(context) => context,
            Err(failure) if context::generation_was_superseded(&failure) => {
                return Ok(GenerationResponse {
                    state: "retry".into(),
                    message: None,
                    source_requests: 0,
                    diff_pairs: 0,
                });
            }
            Err(failure) => return Err(failure),
        };
        let mut response = GenerationResponse {
            state: if context.prompt.is_some() {
                "ready"
            } else {
                "none"
            }
            .into(),
            message: None,
            source_requests: context.source_requests,
            diff_pairs: context.diff_pairs,
        };
        if context.prompt.is_none() {
            return Ok(response);
        }
        let model = request
            .model
            .as_ref()
            .context("detached generation requires a resolved model")?;
        ensure!(
            !model.model.is_empty() && model.model.len() <= 256 && model.provider.len() <= 32,
            "invalid generation model identity"
        );
        ensure!(
            model.thinking.as_ref().is_none_or(|value| value
                .as_str()
                .is_some_and(|value| value.len() <= 32)
                || value.as_u64().is_some_and(|value| value <= 1_000_000)),
            "invalid generation thinking configuration"
        );
        let message = provider::generate(
            &self.http,
            model,
            context.prompt.as_deref().unwrap_or_default(),
            &permit,
        )
        .await?;
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
