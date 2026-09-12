use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, ensure};
use base64::{Engine, engine::general_purpose::STANDARD};
use forge_git::RepositoryPath;
use forge_git::command::{CommandProgress, CommandStream};
use forge_git::mutation::OperationId;
use forge_git::store::RepositoryStore;
use forge_git::writer::{
    DiscardSource, GitWriteAction, GitWriteIntent, GitWriteService, TargetCompletion, WriteOutcome,
};
use forge_protocol::message::{Message, RequestEvent};
use forge_protocol::outbound::MessageSender;
use serde::Deserialize;
use serde_json::{Value, json};

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum WriteRequest {
    Prepare {
        workspace: PathBuf,
        action: WriteAction,
    },
    Submit {
        intent: String,
    },
    Acknowledge {
        operation_id: u64,
    },
    Cancel {
        intent: Option<String>,
        operation_id: Option<u64>,
    },
    Outcome {
        operation_id: u64,
    },
}

#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum WriteAction {
    Stage {
        paths: Vec<String>,
    },
    Unstage {
        paths: Vec<String>,
    },
    Discard {
        paths: Vec<String>,
        source: DiscardBase,
    },
    CreateBranch {
        name: String,
    },
    Commit {
        message: String,
    },
    CommitEditor {
        command: String,
        nvim_server: String,
    },
    Push,
    PublishBranch {
        name: String,
        head: Option<String>,
    },
    Pull,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DiscardBase {
    Index,
    Head,
}

#[derive(Default)]
struct PreparedState {
    sequence: u64,
    closed: bool,
    intent: BTreeMap<String, GitWriteIntent>,
}

pub(crate) struct RepositoryWriteGateway {
    repository: Arc<RepositoryStore>,
    pub(crate) writer: Arc<GitWriteService>,
    epoch: String,
    prepared: Mutex<PreparedState>,
}

impl RepositoryWriteGateway {
    pub(crate) fn new(repository: Arc<RepositoryStore>) -> Result<Self> {
        let epoch = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
        Ok(Self {
            writer: Arc::new(GitWriteService::new(Arc::clone(&repository))),
            repository,
            epoch: format!("{}:{epoch}", std::process::id()),
            prepared: Mutex::new(PreparedState::default()),
        })
    }

    pub(crate) fn close(&self) {
        let mut state = self.prepared.lock().expect("prepared Git intent lock");
        state.closed = true;
        state.intent.clear();
    }

    pub(crate) async fn request(
        &self,
        request: WriteRequest,
        request_id: u64,
        sink: &MessageSender,
    ) -> Result<Value> {
        match request {
            WriteRequest::Prepare { workspace, action } => {
                {
                    let state = self.prepared.lock().expect("prepared Git intent lock");
                    ensure!(
                        !state.closed && state.intent.len() < 64,
                        "prepared Git intent admission is closed or full"
                    );
                }
                let repository = self
                    .repository
                    .open(workspace)
                    .await?
                    .context("workspace is not a Git repository")?;
                let intent = self
                    .writer
                    .prepare(repository, action.into_action()?)
                    .await?;
                let mut state = self.prepared.lock().expect("prepared Git intent lock");
                ensure!(
                    !state.closed && state.intent.len() < 64,
                    "prepared Git intent admission is closed or full"
                );
                state.sequence = state
                    .sequence
                    .checked_add(1)
                    .context("prepared intent sequence exhausted")?;
                let token = format!("{}:{}", self.epoch, state.sequence);
                state.intent.insert(token.clone(), intent);
                Ok(json!({"intent": token}))
            }
            WriteRequest::Submit { intent } => {
                let mut intent = self
                    .prepared
                    .lock()
                    .expect("prepared Git intent lock")
                    .intent
                    .remove(&intent)
                    .context("prepared Git intent is absent or belongs to another host")?;
                let sink = sink.clone();
                let runtime = tokio::runtime::Handle::current();
                intent.set_progress(Arc::new(move |progress: CommandProgress| {
                    let stream = match progress.stream { CommandStream::Stdout => "stdout", CommandStream::Stderr => "stderr" };
                    let message = Message::RequestEvent(RequestEvent {
                        request_id,
                        event: "repository.write.progress".into(),
                        payload: json!({"stream":stream,"sequence":progress.sequence,"bytes":STANDARD.encode(progress.bytes)}),
                    });
                    runtime.block_on(async {
                        tokio::time::timeout(Duration::from_secs(2), sink.send_wait(message)).await
                            .context("Git progress delivery exceeded its bounded wait")??;
                        Ok(())
                    })
                }));
                let outcome = self.writer.submit(intent)?.finish().await?;
                Ok(outcome_value(&outcome))
            }
            WriteRequest::Acknowledge { operation_id } => {
                self.writer
                    .acknowledge(OperationId::from_value(operation_id)?)
                    .context("Git operation has no adoptable receipt")?;
                Ok(json!({"acknowledged": true}))
            }
            WriteRequest::Cancel {
                intent,
                operation_id,
            } => {
                ensure!(
                    intent.is_some() != operation_id.is_some(),
                    "cancel requires exactly one intent or operation identity"
                );
                if let Some(intent) = intent {
                    self.prepared
                        .lock()
                        .expect("prepared Git intent lock")
                        .intent
                        .remove(&intent)
                        .context("prepared Git intent is absent")?;
                } else if let Some(operation_id) = operation_id {
                    self.repository
                        .writes
                        .cancel(OperationId::from_value(operation_id)?)?;
                }
                Ok(json!({"cancellation_requested":true}))
            }
            WriteRequest::Outcome { operation_id } => {
                let outcome = self
                    .writer
                    .outcome(OperationId::from_value(operation_id)?)
                    .context("Git operation has no collected outcome")?;
                Ok(outcome_value(&outcome))
            }
        }
    }
}

impl WriteAction {
    fn into_action(self) -> Result<GitWriteAction> {
        Ok(match self {
            Self::Stage { paths } => GitWriteAction::Stage {
                path: decode_paths(paths)?,
            },
            Self::Unstage { paths } => GitWriteAction::Unstage {
                path: decode_paths(paths)?,
            },
            Self::Discard { paths, source } => GitWriteAction::Discard {
                path: decode_paths(paths)?,
                source: match source {
                    DiscardBase::Index => DiscardSource::Index,
                    DiscardBase::Head => DiscardSource::Head,
                },
            },
            Self::CreateBranch { name } => GitWriteAction::CreateBranch { name },
            Self::Commit { message } => GitWriteAction::Commit { message },
            Self::CommitEditor {
                command,
                nvim_server,
            } => GitWriteAction::CommitWithEditor {
                command,
                nvim_server,
            },
            Self::Push => GitWriteAction::Push,
            Self::PublishBranch { name, head } => GitWriteAction::PublishBranch {
                name,
                expected_head: head,
            },
            Self::Pull => GitWriteAction::Pull,
        })
    }
}

fn decode_paths(paths: Vec<String>) -> Result<Vec<RepositoryPath>> {
    ensure!(
        paths.len() <= forge_git::writer::MAX_TARGETS,
        "Git path target limit exceeded"
    );
    let encoded_limit = (forge_git::writer::MAX_PATH_BYTES.div_ceil(3) + paths.len()) * 4;
    ensure!(
        paths.iter().map(String::len).sum::<usize>() <= encoded_limit,
        "encoded Git paths exceed input limit"
    );
    let mut decoded_bytes = 0;
    paths
        .into_iter()
        .map(|path| {
            let path = STANDARD.decode(path)?;
            decoded_bytes += path.len();
            ensure!(
                decoded_bytes <= forge_git::writer::MAX_PATH_BYTES,
                "Git path bytes exceed input limit"
            );
            RepositoryPath::new(path)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_admission_matches_native_writer_limits() {
        let accepted = (0..400)
            .map(|index| STANDARD.encode(format!("source-{index}.rs")))
            .collect();
        assert_eq!(decode_paths(accepted).unwrap().len(), 400);
        assert!(
            decode_paths(vec![
                STANDARD.encode("source");
                forge_git::writer::MAX_TARGETS + 1
            ])
            .is_err()
        );
        assert!(
            decode_paths(vec![STANDARD.encode(vec![
                b'x';
                forge_git::writer::MAX_PATH_BYTES
                    + 1
            ])])
            .is_err()
        );
        assert!(decode_paths(vec![STANDARD.encode("../outside")]).is_err());
    }
}

pub(crate) fn outcome_value(outcome: &WriteOutcome) -> Value {
    let target: Vec<_> = outcome
        .target
        .iter()
        .map(|target| {
            json!({
                "path":target.path.as_ref().map(|path| STANDARD.encode(path.raw())),
                "completion":match target.completion {
                    TargetCompletion::Completed => "completed",
                    TargetCompletion::Rejected => "rejected",
                    TargetCompletion::OutcomeUnknown => "outcome_unknown",
                    TargetCompletion::NotStarted => "not_started",
                },
                "diagnostic":target.diagnostic,
                "exit_code":target.exit_code,
            })
        })
        .collect();
    json!({
        "operation_id":outcome.operation.value(),
        "success":outcome.target.iter().all(|target| target.completion == TargetCompletion::Completed),
        "target":target,
        "affected":outcome.affected.iter().map(|path| STANDARD.encode(path.raw())).collect::<Vec<_>>(),
        "settlement_diagnostic":outcome.settlement_diagnostic,
        "requires_refresh":outcome.settled.is_none(),
    })
}
