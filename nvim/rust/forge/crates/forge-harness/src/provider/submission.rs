use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, OnceLock, Weak},
};

use anyhow::{Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};
use serde::{Deserialize, Serialize};
use serde_json::json;

use super::{Provider, ProviderRequest};
use crate::trace::TraceStore;
use forge_provider_api::{RecoveryToken, SubmissionId};

const SUBMISSION_FORMAT_VERSION: u32 = 2;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
/// Durable delivery evidence. Only Prepared is safe to include in a later submission.
pub enum SubmissionState {
    /// No adapter has crossed the dispatch boundary.
    Prepared,
    /// Dispatch may have happened. Absence of an acknowledgment does not permit replay.
    Sending { target: Option<RecoveryToken> },
    /// Provider acknowledgment or correlated native history proves acceptance.
    Accepted {
        receipt: Option<RecoveryToken>,
        target: Option<RecoveryToken>,
    },
    /// The user explicitly cancelled this submission.
    Cancelled,
}

/// Certify the journal state at the atomic attempt to prevent prepared input dispatch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SubmissionRetirement {
    /// Prepared input was durably cancelled before dispatch could begin.
    Prevented,
    /// An earlier cancellation already excluded this input from dispatch.
    AlreadyCancelled,
    /// Dispatch may already have happened; retirement does not authorize replay.
    Sending,
    /// Durable native acceptance already proves delivery.
    Accepted,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum SubmissionPhase {
    Prepared,
    Sending,
    Accepted,
    Cancelled,
}

impl From<&SubmissionState> for SubmissionPhase {
    fn from(state: &SubmissionState) -> Self {
        match state {
            SubmissionState::Prepared => Self::Prepared,
            SubmissionState::Sending { .. } => Self::Sending,
            SubmissionState::Accepted { .. } => Self::Accepted,
            SubmissionState::Cancelled => Self::Cancelled,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
/// Full input retained independently of exchange completion and diagnostic logging.
pub struct SubmissionRecord {
    format_version: u32,
    #[serde(default)]
    pub input_message_id: Option<String>,
    /// Exact task observation authority captured before this input can dispatch.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) task_receipt: Option<crate::task::TaskReceiptPermit>,
    /// Host-generated UUID, also used for native input correlation.
    pub id: String,
    /// Original request, including typed input and durable attachment references.
    pub request: ProviderRequest,
    /// Current delivery evidence.
    pub delivery: SubmissionState,
    /// Earlier undelivered input included in this native message.
    pub included: Vec<String>,
    /// Last failure, preserved even when the next prompt cannot reconcile delivery.
    pub error: Option<String>,
}

/// Read old delivery envelopes without interpreting the provider's checkpoint payload.
impl<'de> Deserialize<'de> for SubmissionRecord {
    fn deserialize<D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> std::result::Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct StoredRecord {
            format_version: Option<u32>,
            #[serde(default)]
            input_message_id: Option<String>,
            #[serde(default)]
            task_receipt: Option<crate::task::TaskReceiptPermit>,
            id: String,
            request: ProviderRequest,
            delivery: serde_json::Value,
            included: Vec<String>,
            error: Option<String>,
        }
        #[derive(Deserialize)]
        #[serde(tag = "state", rename_all = "snake_case")]
        enum LegacyDelivery {
            Prepared,
            Sending {
                target: serde_json::Value,
            },
            Accepted {
                receipt: serde_json::Value,
                target: Option<serde_json::Value>,
            },
            Cancelled,
        }
        let stored = StoredRecord::deserialize(deserializer)?;
        let legacy = stored.format_version.is_none();
        let delivery = match stored.format_version {
            Some(SUBMISSION_FORMAT_VERSION) => {
                serde_json::from_value(stored.delivery).map_err(serde::de::Error::custom)?
            }
            None => {
                let token =
                    |value: serde_json::Value| -> std::result::Result<RecoveryToken, D::Error> {
                        let bytes = serde_json::to_vec(&value).map_err(serde::de::Error::custom)?;
                        RecoveryToken::new(bytes).map_err(serde::de::Error::custom)
                    };
                match serde_json::from_value::<LegacyDelivery>(stored.delivery)
                    .map_err(serde::de::Error::custom)?
                {
                    LegacyDelivery::Prepared => SubmissionState::Prepared,
                    LegacyDelivery::Sending { target } => SubmissionState::Sending {
                        target: Some(token(target)?),
                    },
                    LegacyDelivery::Accepted { receipt, target } => SubmissionState::Accepted {
                        receipt: Some(token(receipt)?),
                        target: target.map(token).transpose()?,
                    },
                    LegacyDelivery::Cancelled => SubmissionState::Cancelled,
                }
            }
            Some(_) => {
                return Err(serde::de::Error::custom(
                    "unsupported submission journal format",
                ));
            }
        };
        Ok(Self {
            format_version: SUBMISSION_FORMAT_VERSION,
            input_message_id: stored.input_message_id,
            task_receipt: stored.task_receipt,
            id: stored.id,
            request: stored.request,
            delivery,
            included: stored.included,
            error: if legacy {
                stored
                    .error
                    .map(|_| "Earlier provider operation failed".to_owned())
            } else {
                stored.error
            },
        })
    }
}

#[derive(Clone, Copy)]
enum PreparationIdentity {
    Unidentified,
    Task,
    EditableInput,
}

/// Bind native receipt identity to the host input whose content must survive retries.
pub(crate) enum SubmissionOrigin {
    /// Editable queued input; changed content may replace only a provably unsent receipt.
    QueuedInput(crate::exchange::command::InputId),
    /// Task-owned input with immutable content at its stable task input identity.
    TaskInput(crate::task::TaskInputId),
}

/// SQLite delivery journal. Commits complete before any corresponding provider write.
pub(crate) struct SubmissionStore {
    connection: Mutex<Connection>,
    live: Mutex<HashMap<String, Weak<Submission>>>,
    trace: Arc<TraceStore>,
}

impl SubmissionStore {
    pub(crate) fn submission(self: &Arc<Self>, id: &str) -> Result<Arc<Submission>> {
        if let Some(submission) = self
            .live
            .lock()
            .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?
            .get(id)
            .and_then(Weak::upgrade)
        {
            return Ok(submission);
        }
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let mut live = self
            .live
            .lock()
            .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?;
        if let Some(submission) = live.get(id).and_then(Weak::upgrade) {
            return Ok(submission);
        }
        let payload: String =
            connection.query_row("SELECT payload FROM submission WHERE id=?1", [id], |row| {
                row.get(0)
            })?;
        let record: SubmissionRecord = serde_json::from_str(&payload)?;
        let accepted = matches!(record.delivery, SubmissionState::Accepted { .. });
        let submission = Arc::new(Submission {
            store: self.clone(),
            id: id.into(),
            receipt_id: SubmissionId::try_from(id.to_owned())?,
            record: Mutex::new(record),
            acceptance: tokio::sync::watch::channel(accepted).0,
            steering_queued: tokio::sync::watch::channel(false).0,
            dispatch_observer: Mutex::new(None),
            dispatch: Mutex::new(None),
        });
        live.retain(|_, entry| entry.strong_count() > 0);
        live.insert(id.to_owned(), Arc::downgrade(&submission));
        Ok(submission)
    }
    /// Opens a crash-recoverable journal without depending on trace enablement.
    pub fn open(root: &Path, trace: Arc<TraceStore>) -> Result<Arc<Self>> {
        static STORE: OnceLock<Mutex<HashMap<PathBuf, Weak<SubmissionStore>>>> = OnceLock::new();
        std::fs::create_dir_all(root)?;
        let key = std::fs::canonicalize(root)?;
        let mut store = STORE
            .get_or_init(Default::default)
            .lock()
            .map_err(|_| anyhow::anyhow!("submission store registry lock poisoned"))?;
        store.retain(|_, entry| entry.strong_count() > 0);
        if let Some(existing) = store.get(&key).and_then(Weak::upgrade) {
            return Ok(existing);
        }
        let connection = Connection::open(root.join("provider-submissions.sqlite3"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        let _bootstrap = crate::storage::bootstrap_lock(root, "provider-submissions")?;
        connection.execute_batch("PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL;
            CREATE TABLE IF NOT EXISTS submission (
                id TEXT PRIMARY KEY, runtime TEXT NOT NULL, provider TEXT NOT NULL, payload TEXT NOT NULL
            );
            CREATE INDEX IF NOT EXISTS submission_runtime_provider ON submission(runtime,provider);
            CREATE INDEX IF NOT EXISTS submission_input_identity ON submission(runtime,json_extract(payload,'$.input_message_id'));
            CREATE INDEX IF NOT EXISTS submission_unfinished ON submission(runtime,provider)
                WHERE json_extract(payload,'$.delivery.state') IN ('prepared','sending');
            CREATE INDEX IF NOT EXISTS submission_sending ON submission(runtime,provider)
                WHERE json_extract(payload,'$.delivery.state')='sending';")?;
        let created = Arc::new(Self {
            connection: Mutex::new(connection),
            live: Mutex::new(HashMap::new()),
            trace,
        });
        store.insert(key, Arc::downgrade(&created));
        Ok(created)
    }

    /// Runs durable journal work on the blocking pool. Completion follows the SQLite commit.
    pub(crate) async fn persist<Output: Send + 'static>(
        self: &Arc<Self>,
        operation: impl FnOnce(&Arc<Self>) -> Result<Output> + Send + 'static,
    ) -> Result<Output> {
        let store = self.clone();
        tokio::task::spawn_blocking(move || operation(&store))
            .await
            .map_err(|error| anyhow::anyhow!("submission persistence worker failed: {error}"))?
    }

    fn publish(&self, record: &SubmissionRecord) -> Result<()> {
        if let Some(submission) = self
            .live
            .lock()
            .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?
            .get(&record.id)
            .and_then(Weak::upgrade)
        {
            *submission
                .record
                .lock()
                .map_err(|_| anyhow::anyhow!("submission state lock poisoned"))? = record.clone();
        }
        Ok(())
    }

    /// Saves the original input before host admission or provider preparation.
    pub fn prepare(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderId,
    ) -> Result<Arc<Submission>> {
        self.prepare_identified(request, provider, None)
    }

    fn prepare_identified(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderId,
        input_message_id: Option<String>,
    ) -> Result<Arc<Submission>> {
        self.prepare_record(
            request,
            provider,
            input_message_id,
            PreparationIdentity::Unidentified,
        )
    }

    fn prepare_record(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderId,
        input_message_id: Option<String>,
        identity: PreparationIdentity,
    ) -> Result<Arc<Submission>> {
        let provider = serde_json::to_string(&provider)?;
        let record = SubmissionRecord {
            format_version: SUBMISSION_FORMAT_VERSION,
            input_message_id,
            task_receipt: None,
            id: uuid::Uuid::new_v4().to_string(),
            request: request.invocation_snapshot(),
            delivery: SubmissionState::Prepared,
            included: Vec::new(),
            error: None,
        };
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let mut superseded = None;
        if !matches!(identity, PreparationIdentity::Unidentified)
            && let Some(input_id) = &record.input_message_id
        {
            let existing: Option<(String, String, String)> = transaction.query_row(
                "SELECT id,provider,payload FROM submission WHERE runtime=?1 AND json_extract(payload,'$.input_message_id')=?2 ORDER BY rowid DESC LIMIT 1",
                params![request.runtime_key()?, input_id],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            ).optional()?;
            if let Some((id, existing_provider, payload)) = existing {
                let mut existing: SubmissionRecord = serde_json::from_str(&payload)?;
                ensure!(
                    existing_provider == provider,
                    super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::RecordInput,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    ))
                );
                let same_input = serde_json::to_value(&existing.request.input)?
                    == serde_json::to_value(&request.input)?;
                if !same_input {
                    ensure!(
                        matches!(identity, PreparationIdentity::EditableInput)
                            && matches!(
                                existing.delivery,
                                SubmissionState::Prepared | SubmissionState::Cancelled
                            ),
                        super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::RecordInput,
                            forge_provider_api::ProviderFailureKind::Protocol,
                        ))
                    );
                    existing.delivery = SubmissionState::Cancelled;
                    transaction.execute(
                        "UPDATE submission SET payload=?2 WHERE id=?1",
                        params![id, serde_json::to_string(&existing)?],
                    )?;
                    superseded = Some(existing);
                } else {
                    if existing.delivery == SubmissionState::Cancelled {
                        existing.delivery = SubmissionState::Prepared;
                        existing.error = None;
                        transaction.execute(
                            "UPDATE submission SET payload=?2 WHERE id=?1",
                            params![id, serde_json::to_string(&existing)?],
                        )?;
                        transaction.commit()?;
                        self.publish(&existing)?;
                    } else {
                        drop(transaction);
                    }
                    drop(connection);
                    return self.submission(&id);
                }
            }
        }
        transaction.execute(
            "INSERT INTO submission(id,runtime,provider,payload) VALUES(?1,?2,?3,?4)",
            params![
                record.id,
                request.runtime_key()?,
                provider,
                serde_json::to_string(&record)?
            ],
        )?;
        transaction.commit()?;
        if let Some(superseded) = superseded {
            self.publish(&superseded)?;
        }
        self.log(&record);
        let submission = Arc::new(Submission {
            store: self.clone(),
            id: record.id.clone(),
            receipt_id: SubmissionId::try_from(record.id.clone())?,
            record: Mutex::new(record),
            acceptance: tokio::sync::watch::channel(false).0,
            steering_queued: tokio::sync::watch::channel(false).0,
            dispatch_observer: Mutex::new(None),
            dispatch: Mutex::new(None),
        });
        let mut live = self
            .live
            .lock()
            .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?;
        live.retain(|_, entry| entry.strong_count() > 0);
        live.insert(submission.id.clone(), Arc::downgrade(&submission));
        drop(live);
        drop(connection);
        Ok(submission)
    }

    /// Reads all unfinished inputs for the same execution owner and provider in admission order.
    pub fn pending(
        &self,
        request: &ProviderRequest,
        provider: super::ProviderId,
        before: Option<&str>,
    ) -> Result<Vec<SubmissionRecord>> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let mut statement = connection.prepare(
            "SELECT payload FROM submission WHERE runtime=?1 AND provider=?2 AND json_extract(payload,'$.delivery.state') IN ('prepared','sending') AND (?3 IS NULL OR rowid < (SELECT rowid FROM submission WHERE id=?3)) ORDER BY rowid",
        )?;
        let rows = statement.query_map(
            params![
                request.runtime_key()?,
                serde_json::to_string(&provider)?,
                before
            ],
            |row| row.get::<_, String>(0),
        )?;
        rows.map(|row| Ok(serde_json::from_str::<SubmissionRecord>(&row?)?))
            .collect()
    }

    /// Persist identified input with the actual invocation preferences selected by its owner.
    pub(crate) fn prepare_input_request(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderId,
        origin: SubmissionOrigin,
    ) -> std::result::Result<Arc<Submission>, super::ProviderError> {
        let (input_message_id, identity) = match origin {
            SubmissionOrigin::QueuedInput(id) => {
                (Some(id.as_str().into()), PreparationIdentity::EditableInput)
            }
            SubmissionOrigin::TaskInput(id) => {
                (Some(id.as_str().into()), PreparationIdentity::Task)
            }
        };
        self.prepare_record(request, provider, input_message_id, identity)
            .map_err(journal_failure)
    }

    /// Persist a complete invocation snapshot off the owner loop with typed receipt failures.
    pub(crate) async fn persist_input_request(
        self: &Arc<Self>,
        request: ProviderRequest,
        provider: super::ProviderId,
        origin: SubmissionOrigin,
    ) -> std::result::Result<Arc<Submission>, super::ProviderError> {
        let store = self.clone();
        tokio::task::spawn_blocking(move || store.prepare_input_request(&request, provider, origin))
            .await
            .map_err(|_| {
                super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::RecordInput,
                    forge_provider_api::ProviderFailureKind::Persistence,
                ))
            })?
    }

    /// Construct legacy catalog-only steering receipts for journal and transport fixtures.
    #[cfg(test)]
    pub(crate) fn prepare_steering(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderId,
    ) -> Result<Arc<Submission>> {
        self.prepare_input(catalog, input, provider, None)
    }

    /// Construct catalog-only input for fixtures that do not select an invocation profile.
    #[cfg(test)]
    pub(crate) fn prepare_input(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderId,
        input_message_id: Option<String>,
    ) -> Result<Arc<Submission>> {
        self.prepare_record(
            &Self::input_request(catalog, input),
            provider,
            input_message_id,
            PreparationIdentity::EditableInput,
        )
    }

    /// Construct catalog-only task receipts for existing recovery fixtures.
    #[cfg(test)]
    pub(crate) fn prepare_task_input(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderId,
        id: &crate::task::TaskInputId,
    ) -> Result<Arc<Submission>> {
        self.prepare_record(
            &Self::input_request(catalog, input),
            provider,
            Some(id.as_str().into()),
            PreparationIdentity::Task,
        )
    }

    /// Build a test request without adding invocation choices to discovery contracts.
    #[cfg(test)]
    fn input_request(
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
    ) -> ProviderRequest {
        ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: catalog.execution_owner.clone(),
            harness_session_id: catalog.harness_session_id.clone(),
            workspace: catalog.workspace.clone(),
            input,
            mode: super::PromptMode::Chat,
            model: "default".into(),
            effort: String::new(),
            context_window: None,
            service_tier: Default::default(),
            access: catalog.access.clone(),
            approval_mode: catalog.approval_mode,
            provider_session_id: catalog.provider_session_id.clone(),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    fn log(&self, record: &SubmissionRecord) {
        self.trace.record(
            &record.request.harness_session_id,
            "provider.submission",
            json!({
                "submission_id": record.id, "owner": record.request.execution_owner,
                "delivery": SubmissionPhase::from(&record.delivery), "included": record.included, "error": record.error,
            }),
        );
    }

    fn receipts(
        &self,
        request: &ProviderRequest,
        provider: super::ProviderId,
    ) -> Result<Vec<RecoveryToken>> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let mut statement = connection.prepare("SELECT payload FROM submission WHERE runtime=?1 AND provider=?2 AND json_extract(payload,'$.delivery.state')='accepted'")?;
        let records = statement
            .query_map(
                params![request.runtime_key()?, serde_json::to_string(&provider)?],
                |row| row.get::<_, String>(0),
            )?
            .collect::<std::result::Result<Vec<_>, _>>()?;
        records
            .into_iter()
            .map(|payload| serde_json::from_str::<SubmissionRecord>(&payload))
            .collect::<std::result::Result<Vec<_>, _>>()
            .map(|records| {
                records
                    .into_iter()
                    .filter_map(|record| match record.delivery {
                        SubmissionState::Accepted { receipt, .. } => receipt,
                        _ => None,
                    })
                    .collect()
            })
            .map_err(Into::into)
    }
}

impl std::fmt::Debug for SubmissionStore {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("SubmissionStore")
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
/// Adapter-owned capability to commit one submission's delivery evidence.
pub struct Submission {
    store: Arc<SubmissionStore>,
    id: String,
    receipt_id: SubmissionId,
    record: Mutex<SubmissionRecord>,
    acceptance: tokio::sync::watch::Sender<bool>,
    steering_queued: tokio::sync::watch::Sender<bool>,
    dispatch_observer: Mutex<Option<forge_provider_api::submission::DispatchObserver>>,
    dispatch: Mutex<Option<SubmissionDispatch>>,
}

/// Distinguish the worker retaining input from its independently owned native prompt.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SubmissionDispatch {
    Prompt,
    Steering,
    SteeringPrompt,
}

/// Identify the lifetime released by one dispatch guard.
#[derive(Clone, Copy)]
enum DispatchOperation {
    Prompt,
    Steering,
}

/// Retain a live dispatch without changing its durable delivery evidence.
pub(crate) struct SubmissionFlight {
    submission: Arc<Submission>,
    operation: DispatchOperation,
}

impl Drop for SubmissionFlight {
    fn drop(&mut self) {
        let mut dispatch = self
            .submission
            .dispatch
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *dispatch = match (*dispatch, self.operation) {
            (Some(SubmissionDispatch::SteeringPrompt), DispatchOperation::Prompt) => {
                Some(SubmissionDispatch::Steering)
            }
            (Some(SubmissionDispatch::SteeringPrompt), DispatchOperation::Steering) => {
                Some(SubmissionDispatch::Prompt)
            }
            (Some(SubmissionDispatch::Prompt), DispatchOperation::Prompt)
            | (Some(SubmissionDispatch::Steering), DispatchOperation::Steering) => None,
            (None, DispatchOperation::Prompt | DispatchOperation::Steering) => None,
            (Some(SubmissionDispatch::Prompt), DispatchOperation::Steering) => {
                Some(SubmissionDispatch::Prompt)
            }
            (Some(SubmissionDispatch::Steering), DispatchOperation::Prompt) => {
                Some(SubmissionDispatch::Steering)
            }
        };
    }
}

impl Submission {
    /// Bind receipt authority once while native delivery is still provably unstarted.
    pub(crate) fn bind_task_receipt(&self, permit: crate::task::TaskReceiptPermit) -> Result<()> {
        self.update(|record| {
            if let Some(existing) = &record.task_receipt {
                ensure!(
                    existing == &permit,
                    "submission task receipt authority changed"
                );
                return Ok(());
            }
            ensure!(
                record.delivery == SubmissionState::Prepared,
                "task receipt authority must be bound before dispatch"
            );
            record.task_receipt = Some(permit);
            Ok(())
        })
    }

    /// Commit final invocation preferences while delivery is still provably prepared.
    /// Preserve the original input used for journal identity and never retain live capabilities.
    pub(crate) fn seal_request(
        &self,
        request: ProviderRequest,
    ) -> std::result::Result<(), super::ProviderError> {
        self.update(move |record| {
            ensure!(
                record.request.owner()? == request.owner()?
                    && record.request.harness_session_id == request.harness_session_id
                    && record.request.workspace == request.workspace,
                super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::RecordInput,
                    forge_provider_api::ProviderFailureKind::Protocol,
                ))
            );
            match &record.delivery {
                SubmissionState::Prepared => {}
                SubmissionState::Sending { .. } => {
                    return Err(super::ProviderError::DeliveryUncertain.into());
                }
                SubmissionState::Accepted { .. } => {
                    return Err(super::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::RecordInput,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                    )
                    .into());
                }
                SubmissionState::Cancelled => return Err(super::ProviderError::Cancelled.into()),
            }
            let ProviderRequest {
                dispatch: _,
                execution_owner,
                harness_session_id: _,
                workspace: _,
                input: _,
                mode,
                model,
                effort,
                context_window,
                service_tier,
                access,
                approval_mode,
                provider_session_id,
                submission: _,
                recovered_input: _,
                control_context: _,
            } = request;
            record.request.execution_owner = execution_owner;
            record.request.mode = mode;
            record.request.model = model;
            record.request.effort = effort;
            record.request.context_window = context_window;
            record.request.service_tier = service_tier;
            record.request.access = access;
            record.request.approval_mode = approval_mode;
            record.request.provider_session_id = provider_session_id;
            Ok(())
        })
        .map_err(journal_failure)
    }

    /// Install a scoped handoff observer without changing durable delivery evidence.
    /// Restoring accepted input never manufactures a live dispatch confirmation.
    pub(crate) fn prepare_dispatch(
        &self,
        tracker: &forge_provider_api::submission::DispatchTracker,
        binding: forge_provider_api::BindingId,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::submission::DispatchReporter, forge_provider_api::ProviderError>
    {
        let invalid = || {
            forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::AdmitInput,
                forge_provider_api::ProviderFailureKind::Protocol,
            ))
        };
        let record = self.record().map_err(|_| invalid())?;
        if record.request.owner()? != scope.owner {
            return Err(invalid());
        }
        let reporter = tracker
            .track_input(binding, scope, self.receipt_id.clone())
            .map_err(|_| invalid())?;
        *self.dispatch_observer.lock().map_err(|_| invalid())? = Some(tracker.observer());
        Ok(reporter)
    }

    /// Get this live attempt's handoff observation separately from native acceptance.
    #[cfg(test)]
    pub(crate) fn dispatch_observer(
        &self,
    ) -> Result<
        Option<forge_provider_api::submission::DispatchObserver>,
        forge_provider_api::ProviderError,
    > {
        self.dispatch_observer
            .lock()
            .map(|observer| observer.clone())
            .map_err(|_| {
                forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::AdmitInput,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    ),
                )
            })
    }

    /// Own this live steering attempt without authorizing replay of uncertain input.
    pub(crate) fn begin_steering(self: &Arc<Self>) -> Result<SubmissionFlight> {
        let flight = self.begin_dispatch(DispatchOperation::Steering)?;
        self.steering_queued.send_replace(false);
        Ok(flight)
    }

    /// Retain a pending prompt response as live work without inventing acceptance.
    pub(crate) fn begin_prompt(self: &Arc<Self>) -> Result<SubmissionFlight> {
        self.begin_dispatch(DispatchOperation::Prompt)
    }

    /// Admit one operation under the journal's write gate before its first native write.
    fn begin_dispatch(self: &Arc<Self>, operation: DispatchOperation) -> Result<SubmissionFlight> {
        let _journal = self
            .store
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        ensure!(
            self.record()?.delivery == SubmissionState::Prepared,
            "input was already dispatched"
        );
        let mut dispatch = self
            .dispatch
            .lock()
            .map_err(|_| anyhow::anyhow!("submission dispatch lock poisoned"))?;
        *dispatch = Some(match (*dispatch, operation) {
            (None, DispatchOperation::Prompt) => SubmissionDispatch::Prompt,
            (None, DispatchOperation::Steering) => SubmissionDispatch::Steering,
            (Some(SubmissionDispatch::Steering), DispatchOperation::Prompt) => {
                SubmissionDispatch::SteeringPrompt
            }
            (
                Some(SubmissionDispatch::Prompt | SubmissionDispatch::SteeringPrompt),
                DispatchOperation::Prompt,
            )
            | (
                Some(
                    SubmissionDispatch::Prompt
                    | SubmissionDispatch::Steering
                    | SubmissionDispatch::SteeringPrompt,
                ),
                DispatchOperation::Steering,
            ) => anyhow::bail!("input already has a dispatch worker"),
        });
        Ok(SubmissionFlight {
            submission: self.clone(),
            operation,
        })
    }

    /// Observe FIFO handoff independently from the provider's acknowledgment.
    pub(crate) fn steering_queued(&self) -> tokio::sync::watch::Receiver<bool> {
        self.steering_queued.subscribe()
    }

    /// Publish handoff only after the active turn's FIFO owns this input.
    pub(crate) fn mark_steering_queued(&self) {
        self.steering_queued.send_replace(true);
    }

    /// Publish durable native evidence; host task activation belongs to the exchange owner.
    fn notify_accepted(&self) {
        self.acceptance.send_replace(true);
    }
    pub(crate) fn acceptance(&self) -> tokio::sync::watch::Receiver<bool> {
        self.acceptance.subscribe()
    }
    pub(crate) async fn reconcile(
        self: &Arc<Self>,
        provider: &Provider,
    ) -> Result<SubmissionRecord> {
        let record = self.record()?;
        if matches!(record.delivery, SubmissionState::Accepted { .. }) {
            self.notify_accepted();
            return Ok(record);
        }
        if matches!(record.delivery, SubmissionState::Sending { .. }) {
            let request = record.request.clone();
            let provider_kind = provider.descriptor().kind;
            let known = self
                .store
                .persist(move |store| store.receipts(&request, provider_kind))
                .await?;
            let target = match &record.delivery {
                SubmissionState::Sending { target } => target.as_ref(),
                _ => None,
            };
            match provider
                .reconcile_submission(&self.receipt_id, target, &known)
                .await?
            {
                forge_provider_api::SubmissionEvidence::Accepted {
                    receipt,
                    provider_session_id,
                } => {
                    self.persist(move |submission| {
                        submission.accepted(receipt)?;
                        submission.update(|record| {
                            record.request.provider_session_id = provider_session_id;
                            Ok(())
                        })
                    })
                    .await?;
                    return self.record();
                }
                forge_provider_api::SubmissionEvidence::NotAccepted => {
                    self.persist(|submission| submission.rejected("Input was not accepted".into()))
                        .await?;
                }
                forge_provider_api::SubmissionEvidence::Unknown => {}
            }
        }
        Ok(record)
    }
    /// Returns the stable host identity used for correlation, never a deduplication promise.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get the validated host receipt identity used by owner observation events.
    pub(crate) fn receipt_id(&self) -> &SubmissionId {
        &self.receipt_id
    }

    /// Returns the in-memory evidence published after the last successful journal commit.
    pub fn record(&self) -> Result<SubmissionRecord> {
        Ok(self
            .record
            .lock()
            .map_err(|_| anyhow::anyhow!("submission state lock poisoned"))?
            .clone())
    }

    /// Preserves commit-before-send ordering without blocking an asynchronous provider task.
    pub(crate) async fn persist(
        self: &Arc<Self>,
        operation: impl FnOnce(&Self) -> Result<()> + Send + 'static,
    ) -> Result<()> {
        let submission = self.clone();
        self.store.persist(move |_| operation(&submission)).await
    }

    /// Persists the uncertain state before polling the external send future.
    pub fn sending(&self, target: Option<RecoveryToken>) -> Result<()> {
        self.update(|record| {
            ensure!(
                record.delivery == SubmissionState::Prepared,
                "submission already dispatched"
            );
            record.delivery = SubmissionState::Sending { target };
            Ok(())
        })
    }

    /// Atomically acknowledges this message and every earlier input it carried.
    pub fn accepted(&self, receipt: Option<RecoveryToken>) -> Result<()> {
        let mut connection = self
            .store
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let mut record = self.record()?;
        if matches!(record.delivery, SubmissionState::Accepted { .. }) {
            drop(transaction);
            drop(connection);
            self.notify_accepted();
            return Ok(());
        }
        ensure!(
            !matches!(record.delivery, SubmissionState::Cancelled),
            "cancelled submission cannot be acknowledged"
        );
        let target = match &record.delivery {
            SubmissionState::Sending { target } => target.clone(),
            SubmissionState::Accepted { target, .. } => target.clone(),
            _ => None,
        };
        record.delivery = SubmissionState::Accepted {
            receipt: receipt.clone(),
            target: target.clone(),
        };
        let mut changed = vec![record.clone()];
        for id in &record.included {
            let payload: String = transaction.query_row(
                "SELECT payload FROM submission WHERE id=?1",
                [id],
                |row| row.get(0),
            )?;
            let mut earlier: SubmissionRecord = serde_json::from_str(&payload)?;
            ensure!(
                earlier.delivery == SubmissionState::Prepared,
                "included submission is no longer undelivered"
            );
            earlier.delivery = SubmissionState::Accepted {
                receipt: receipt.clone(),
                target: target.clone(),
            };
            changed.push(earlier);
        }
        for changed in &changed {
            transaction.execute(
                "UPDATE submission SET payload=?2 WHERE id=?1",
                params![changed.id, serde_json::to_string(changed)?],
            )?;
        }
        transaction.commit()?;
        for record in &changed {
            self.store.publish(record)?;
        }
        drop(connection);
        for changed in &changed {
            let live = self
                .store
                .live
                .lock()
                .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?
                .get(&changed.id)
                .and_then(Weak::upgrade);
            if let Some(submission) = live {
                submission.notify_accepted();
            }
        }
        for changed in &changed {
            self.store.log(changed);
        }
        Ok(())
    }

    /// Records a proven pre-admission rejection. Transport errors must never call this method.
    pub fn rejected(&self, error: String) -> Result<()> {
        self.update(|record| {
            ensure!(
                matches!(
                    record.delivery,
                    SubmissionState::Prepared | SubmissionState::Sending { .. }
                ),
                "accepted or cancelled submission cannot be rejected"
            );
            record.delivery = SubmissionState::Prepared;
            record.error = Some(error);
            Ok(())
        })
    }

    /// Retains the actual failure without converting uncertain delivery into permission to replay.
    pub fn failed(&self, error: String) -> Result<()> {
        self.update(|record| {
            record.error = Some(error);
            Ok(())
        })
    }

    /// Excludes explicitly cancelled input from automatic recovery.
    pub fn cancel(&self) -> Result<()> {
        self.retire_prepared().map(drop).map_err(Into::into)
    }

    /// Prevent prepared dispatch and report evidence from the same durable journal transaction.
    pub(crate) fn retire_prepared(
        &self,
    ) -> std::result::Result<SubmissionRetirement, super::ProviderError> {
        self.update(|record| {
            Ok(match record.delivery {
                SubmissionState::Prepared => {
                    record.delivery = SubmissionState::Cancelled;
                    SubmissionRetirement::Prevented
                }
                SubmissionState::Cancelled => SubmissionRetirement::AlreadyCancelled,
                SubmissionState::Sending { .. } => SubmissionRetirement::Sending,
                SubmissionState::Accepted { .. } => SubmissionRetirement::Accepted,
            })
        })
        .map_err(|_| {
            super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::RecordInput,
                forge_provider_api::ProviderFailureKind::Persistence,
            ))
        })
    }

    /// Commit a receipt change and expose its result only after durable publication.
    fn update<Output>(
        &self,
        change: impl FnOnce(&mut SubmissionRecord) -> Result<Output>,
    ) -> Result<Output> {
        let mut connection = self
            .store
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let mut record = self.record()?;
        let was_prepared = record.delivery == SubmissionState::Prepared;
        let output = change(&mut record)?;
        if was_prepared && matches!(record.delivery, SubmissionState::Sending { .. }) {
            let mut statement = transaction.prepare(
                "SELECT other.id FROM submission AS other JOIN submission AS current ON current.id=?1 WHERE other.id<>current.id AND other.runtime=current.runtime AND other.provider=current.provider AND json_extract(other.payload,'$.delivery.state')='sending'",
            )?;
            let sending = statement
                .query_map([&self.id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?;
            let live = self
                .store
                .live
                .lock()
                .map_err(|_| anyhow::anyhow!("submission registry lock poisoned"))?;
            let steering = match *self
                .dispatch
                .lock()
                .map_err(|_| anyhow::anyhow!("submission dispatch lock poisoned"))?
            {
                Some(SubmissionDispatch::Steering | SubmissionDispatch::SteeringPrompt) => true,
                None | Some(SubmissionDispatch::Prompt) => false,
            };
            let mut unresolved = false;
            for id in sending {
                let active = match live.get(&id).and_then(Weak::upgrade) {
                    Some(other) => other
                        .dispatch
                        .lock()
                        .map_err(|_| anyhow::anyhow!("submission dispatch lock poisoned"))?
                        .is_some(),
                    None => false,
                };
                unresolved |= !steering || !active;
            }
            if unresolved {
                return Err(forge_provider_api::ProviderError::DeliveryUncertain.into());
            }
        }
        transaction.execute(
            "UPDATE submission SET payload=?2 WHERE id=?1",
            params![self.id, serde_json::to_string(&record)?],
        )?;
        transaction.commit()?;
        self.store.publish(&record)?;
        drop(connection);
        self.store.log(&record);
        Ok(output)
    }

    /// Reconciles prior uncertain input, then attaches only definitely undelivered context.
    pub async fn recover(&self, provider: &Provider, request: &mut ProviderRequest) -> Result<()> {
        let provider_kind = provider.descriptor().kind;
        for pending in self
            .store
            .pending(request, provider_kind.clone(), Some(&self.id))?
        {
            if pending.id == self.id {
                continue;
            }
            if matches!(pending.delivery, SubmissionState::Sending { .. }) {
                let handle = self.store.submission(&pending.id)?;
                let reconciled = handle.reconcile(provider).await?;
                if matches!(reconciled.delivery, SubmissionState::Accepted { .. }) {
                    if request.provider_session_id.is_none() {
                        request.provider_session_id = reconciled.request.provider_session_id;
                    }
                } else if matches!(reconciled.delivery, SubmissionState::Sending { .. }) {
                    return Err(forge_provider_api::ProviderError::DeliveryUncertain.into());
                }
            }
        }
        let pending = self.store.pending(request, provider_kind, Some(&self.id))?;
        let earlier = pending
            .into_iter()
            .filter(|pending| pending.id != self.id && pending.input_message_id.is_none())
            .collect::<Vec<_>>();
        request.recovered_input = earlier
            .iter()
            .map(|pending| pending.request.input.clone())
            .collect();
        self.update(|record| {
            record.included = earlier.iter().map(|pending| pending.id.clone()).collect();
            Ok(())
        })
    }
}

#[async_trait::async_trait]
impl forge_provider_api::submission::InputReceipt for Submission {
    fn id(&self) -> &SubmissionId {
        &self.receipt_id
    }

    async fn record_sending(
        self: Arc<Self>,
        target: Option<RecoveryToken>,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.sending(target))
            .await
            .map_err(journal_failure)
    }

    async fn record_accepted(
        self: Arc<Self>,
        receipt: Option<RecoveryToken>,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.accepted(receipt))
            .await
            .map_err(journal_failure)
    }

    async fn record_rejected(
        self: Arc<Self>,
        reason: forge_provider_api::ProviderError,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.rejected(reason.to_string()))
            .await
            .map_err(journal_failure)
    }

    async fn record_failure(
        self: Arc<Self>,
        reason: forge_provider_api::ProviderError,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.failed(reason.to_string()))
            .await
            .map_err(journal_failure)
    }
}

/// Preserve normalized receipt failures at the legacy persistence boundary.
pub(crate) fn journal_failure(error: anyhow::Error) -> super::ProviderError {
    match error.downcast::<super::ProviderError>() {
        Ok(error) => error,
        Err(error) => super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::RecordInput,
            if error.is::<forge_provider_api::execution::ExecutionOwnerError>() {
                forge_provider_api::ProviderFailureKind::Protocol
            } else {
                forge_provider_api::ProviderFailureKind::Persistence
            },
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{PromptMode, ProviderInput};
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

    fn token(label: &str) -> RecoveryToken {
        RecoveryToken::new(label.as_bytes().to_vec()).expect("fixture token is nonempty")
    }

    fn fixture_provider_id() -> super::super::ProviderId {
        forge_providers::registry::nonselectable_id().expect("registry supplies fixture identity")
    }

    /// Keep handoff separate from durable acceptance and reject a foreign owner.
    #[tokio::test]
    async fn dispatch_confirmation_preserves_unconfirmed_delivery() -> Result<()> {
        use forge_provider_api::submission::InputReceipt;
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let request = request("pending input");
        let scope = forge_provider_api::InvocationScope::new(request.owner()?, "exchange");
        let input = store.prepare(&request, fixture_provider_id())?;
        let binding = forge_provider_api::BindingId::new("binding")?;
        assert!(
            input
                .prepare_dispatch(
                    &forge_provider_api::submission::DispatchTracker::default(),
                    binding.clone(),
                    forge_provider_api::InvocationScope::new(
                        forge_provider_api::ExecutionOwner::root("foreign"),
                        "exchange",
                    )
                )
                .is_err()
        );
        let reporter = input.prepare_dispatch(&request.dispatch, binding.clone(), scope.clone())?;
        let mut observer = input
            .dispatch_observer()?
            .expect("prepared dispatch has an observer");
        input.clone().record_sending(None).await?;
        reporter.confirm();
        let confirmation = observer.wait().await?;
        assert_eq!(confirmation.binding(), &binding);
        assert_eq!(confirmation.scope(), &scope);
        assert!(matches!(
            input.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        assert!(!*input.acceptance().borrow());
        Ok(())
    }

    /// Preserve old input and delivery state while wrapping native evidence without decoding it.
    #[test]
    fn legacy_delivery_preserves_uncertainty_and_opaque_evidence() -> Result<()> {
        let evidence = json!({"opaque_old_format":"private-native-canary"});
        for state in ["prepared", "sending", "accepted", "cancelled"] {
            let mut delivery = json!({"state":state});
            if state == "sending" || state == "accepted" {
                delivery["target"] = evidence.clone();
            }
            if state == "accepted" {
                delivery["receipt"] = json!("private-receipt-canary");
            }
            let value = json!({
                "id":"legacy-input", "request":request("saved input"), "delivery":delivery,
                "included":[], "error":"private-native-error-canary",
            });
            let record: SubmissionRecord = serde_json::from_value(value)?;
            assert_eq!(record.format_version, SUBMISSION_FORMAT_VERSION);
            assert_eq!(record.request.input.text(), "saved input");
            match record.delivery {
                SubmissionState::Prepared => assert_eq!(state, "prepared"),
                SubmissionState::Sending { target } => {
                    assert_eq!(state, "sending");
                    assert_eq!(
                        serde_json::from_slice::<serde_json::Value>(target.unwrap().as_bytes())?,
                        evidence
                    );
                }
                SubmissionState::Accepted { receipt, .. } => {
                    assert_eq!(state, "accepted");
                    assert!(receipt.is_some());
                }
                SubmissionState::Cancelled => assert_eq!(state, "cancelled"),
            }
            assert_eq!(
                record.error.as_deref(),
                Some("Earlier provider operation failed")
            );
        }
        let future = json!({
            "format_version":SUBMISSION_FORMAT_VERSION + 1,
            "id":"future", "request":request("saved input"),
            "delivery":{"state":"prepared"}, "included":[], "error":null,
        });
        assert!(serde_json::from_value::<SubmissionRecord>(future).is_err());
        Ok(())
    }

    /// Keep definite pre-dispatch rejection retryable without reopening accepted input.
    #[tokio::test]
    async fn receipt_records_rejection_before_dispatch_without_resetting_acceptance() -> Result<()>
    {
        use forge_provider_api::submission::InputReceipt;
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let input = store.prepare(&request("rejected input"), fixture_provider_id())?;
        let reason = || {
            forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::AdmitInput,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        };
        input.clone().record_rejected(reason()).await?;
        assert_eq!(input.record()?.delivery, SubmissionState::Prepared);
        input.clone().record_sending(None).await?;
        input.clone().record_rejected(reason()).await?;
        assert_eq!(input.record()?.delivery, SubmissionState::Prepared);
        input.clone().record_sending(None).await?;
        input.clone().record_accepted(None).await?;
        assert!(input.clone().record_rejected(reason()).await.is_err());
        assert!(matches!(
            input.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    /// Preserve uncertain delivery through the public receipt callback instead of reporting persistence failure.
    #[tokio::test]
    async fn receipt_preserves_uncertain_predecessor_category() -> Result<()> {
        use forge_provider_api::submission::InputReceipt;
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let predecessor =
            store.prepare(&request("first"), forge_providers::registry::default_id())?;
        predecessor.sending(Some(token("first-target")))?;
        let next = store.prepare(&request("next"), forge_providers::registry::default_id())?;
        let error = next
            .clone()
            .record_sending(Some(token("next-target")))
            .await
            .unwrap_err();
        assert!(matches!(
            error,
            forge_provider_api::ProviderError::DeliveryUncertain
        ));
        assert_eq!(next.record()?.delivery, SubmissionState::Prepared);
        Ok(())
    }

    /// Keep a live prompt distinct from lost delivery and preserve a continuation after its caller drops.
    #[test]
    fn live_prompt_allows_steering_until_its_dispatch_lifetime_ends() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let original = store.prepare(
            &request("original"),
            forge_providers::registry::alternate_id(&forge_providers::registry::default_id())
                .expect("registry supplies an alternate identity"),
        )?;
        let caller = original.begin_steering()?;
        let prompt = original.begin_prompt()?;
        assert!(original.begin_prompt().is_err());
        let target = Some(token("target"));
        original.sending(target.clone())?;
        drop(caller);
        let steering = store.prepare(
            &request("steering"),
            forge_providers::registry::alternate_id(&forge_providers::registry::default_id())
                .expect("registry supplies an alternate identity"),
        )?;
        let _steering = steering.begin_steering()?;
        steering.sending(target.clone())?;
        steering.accepted(Some(token("injected")))?;
        let next = store.prepare(
            &request("next normal prompt"),
            forge_providers::registry::alternate_id(&forge_providers::registry::default_id())
                .expect("registry supplies an alternate identity"),
        )?;
        let _next = next.begin_prompt()?;
        assert!(
            next.sending(target.clone()).is_err(),
            "a normal prompt overlapped live work"
        );
        drop(prompt);
        let later = store.prepare(
            &request("later steering"),
            forge_providers::registry::alternate_id(&forge_providers::registry::default_id())
                .expect("registry supplies an alternate identity"),
        )?;
        let _later = later.begin_steering()?;
        assert!(
            later.sending(target.clone()).is_err(),
            "lost acknowledgment was treated as live"
        );
        original.accepted(Some(token("terminal prompt response")))?;
        later.sending(target)?;
        Ok(())
    }

    #[test]
    fn live_steering_can_overlap_but_lost_acknowledgments_block_new_dispatch() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let first = store.prepare(&request("first"), forge_providers::registry::default_id())?;
        let second = store.prepare(&request("second"), forge_providers::registry::default_id())?;
        let third = store.prepare(&request("third"), forge_providers::registry::default_id())?;
        let first_flight = first.begin_steering()?;
        assert!(first.begin_steering().is_err());
        let second_flight = second.begin_steering()?;
        let third_flight = third.begin_steering()?;
        let target = Some(token("target"));
        first.sending(target.clone())?;
        second.sending(target.clone())?;
        second.accepted(Some(token("second accepted first")))?;
        drop(second_flight);
        assert!(matches!(
            first.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        drop(first_flight);
        first.failed("connection dropped".into())?;
        assert_eq!(first.record()?.error.as_deref(), Some("connection dropped"));
        assert!(
            first.begin_steering().is_err(),
            "uncertain input cannot be replayed"
        );
        let error = third.sending(target.clone()).unwrap_err();
        assert!(matches!(
            error.downcast_ref::<forge_provider_api::ProviderError>(),
            Some(forge_provider_api::ProviderError::DeliveryUncertain)
        ));
        assert_eq!(third.record()?.delivery, SubmissionState::Prepared);
        first.accepted(Some(token("late first acknowledgment")))?;
        third.sending(target)?;
        drop(third_flight);
        Ok(())
    }

    #[test]
    fn pending_uses_unfinished_index_and_does_not_decode_completed_history() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let request = request("pending");
        let pending = store.prepare(&request, forge_providers::registry::default_id())?;
        let connection = store.connection.lock().unwrap();
        let runtime = request.runtime_key()?;
        let provider = serde_json::to_string(&forge_providers::registry::default_id())?;
        for state in ["accepted", "cancelled"] {
            connection.execute(
                "INSERT INTO submission VALUES(?1,?2,?3,?4)",
                params![
                    state,
                    runtime,
                    provider,
                    serde_json::json!({"delivery":{"state":state},"request":"not a request"})
                        .to_string()
                ],
            )?;
        }
        connection.execute(
            "INSERT INTO submission VALUES('foreign','other',?1,?2)",
            params![
                provider,
                serde_json::json!({"delivery":{"state":"prepared"},"request":"not a request"})
                    .to_string()
            ],
        )?;
        let mut plan = connection.prepare("EXPLAIN QUERY PLAN SELECT payload FROM submission WHERE runtime=?1 AND provider=?2 AND json_extract(payload,'$.delivery.state') IN ('prepared','sending') ORDER BY rowid")?;
        let details = plan
            .query_map(params![runtime, provider], |row| row.get::<_, String>(3))?
            .collect::<rusqlite::Result<Vec<_>>>()?
            .join("\n");
        assert!(details.contains("submission_unfinished"), "{details}");
        drop(plan);
        drop(connection);
        let result = store.pending(&request, forge_providers::registry::default_id(), None)?;
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].id, pending.id());
        Ok(())
    }

    #[tokio::test]
    async fn live_submission_handles_share_committed_state_without_reopening_storage() -> Result<()>
    {
        let root = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(root.path())?);
        let store = SubmissionStore::open(root.path(), trace.clone())?;
        let second = SubmissionStore::open(root.path(), trace)?;
        assert!(Arc::ptr_eq(&store, &second));
        let submission =
            store.prepare(&request("message"), forge_providers::registry::default_id())?;
        let alias = second.submission(submission.id())?;
        assert!(Arc::ptr_eq(&submission, &alias));
        let mut acceptance = alias.acceptance();
        submission
            .persist(|submission| submission.sending(Some(token("target"))))
            .await?;
        store.connection.lock().unwrap().execute_batch("CREATE TRIGGER fail_accept BEFORE UPDATE ON submission WHEN json_extract(NEW.payload,'$.delivery.state')='accepted' BEGIN SELECT RAISE(ABORT,'accept failed'); END;")?;
        assert!(
            submission
                .persist(|submission| submission.accepted(Some(token("receipt"))))
                .await
                .is_err()
        );
        assert!(matches!(
            alias.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        assert!(!*acceptance.borrow());
        store
            .connection
            .lock()
            .unwrap()
            .execute_batch("DROP TRIGGER fail_accept")?;
        submission
            .persist(|submission| submission.accepted(Some(token("receipt"))))
            .await?;
        acceptance.changed().await?;
        assert!(*acceptance.borrow());
        assert!(matches!(
            alias.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        store
            .connection
            .lock()
            .unwrap()
            .execute_batch("ALTER TABLE submission RENAME TO unavailable_submission")?;
        assert!(matches!(
            alias.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    struct ReceiptControl {
        fail_before_send: AtomicBool,
        lose_ack: AtomicBool,
        history_accepts: AtomicBool,
        send_count: AtomicUsize,
        input: Mutex<Vec<forge_provider_api::session::PromptRequest>>,
    }

    fn receipt_provider() -> (
        Arc<crate::provider::test_support::EventFixtureProvider>,
        Arc<ReceiptControl>,
    ) {
        let control = Arc::new(ReceiptControl {
            fail_before_send: AtomicBool::new(false),
            lose_ack: AtomicBool::new(false),
            history_accepts: AtomicBool::new(false),
            send_count: AtomicUsize::new(0),
            input: Mutex::new(Vec::new()),
        });
        let log = control.clone();
        let provider = crate::provider::test_support::EventFixtureProvider::new(
            forge_provider_api::ProviderDescriptor {
                kind: fixture_provider_id(),
                label: "Generic fixture".into(),
                capability: Default::default(),
            },
            move |request, sink, _cancelled| {
                let log = log.clone();
                async move {
                    log.input.lock().unwrap().push(request.clone());
                    let count = log.send_count.fetch_add(1, Ordering::SeqCst);
                    let address = forge_provider_api::TurnAddress {
                        scope: request.scope.clone(),
                        thread_id: "fixture-session".into(),
                        turn_id: format!("turn-{count}").into(),
                    };
                    sink.send_wait(forge_provider_api::ProviderEvent::Execution {
                        observation: forge_provider_api::ExecutionObservation::new(
                            request.scope.clone(),
                            forge_provider_api::ExecutionUpdate::Started {
                                address: address.clone(),
                            },
                        )?,
                    })
                    .await?;
                    sink.send_wait(forge_provider_api::ProviderEvent::Execution {
                        observation: forge_provider_api::ExecutionObservation::new(
                            request.scope.clone(),
                            forge_provider_api::ExecutionUpdate::Finished {
                                address,
                                covered_submissions: vec![request.submission_id],
                                completed_at_ms: 1,
                                result: forge_provider_api::ExecutionResult::Completed {
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion::default(
                                        )
                                        .into(),
                                },
                            },
                        )?,
                    })
                    .await?;
                    Ok(())
                }
            },
        );
        let admission = control.clone();
        provider.with_admission(move |_request| {
            if admission.fail_before_send.swap(false, Ordering::SeqCst) {
                return Err(forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::AdmitInput,
                        forge_provider_api::ProviderFailureKind::Rejected,
                    ),
                ));
            }
            if admission.lose_ack.swap(false, Ordering::SeqCst) {
                return Err(forge_provider_api::ProviderError::DeliveryUncertain);
            }
            Ok(Some(
                forge_provider_api::RecoveryToken::new(b"accepted-fixture".to_vec())
                    .expect("nonempty token"),
            ))
        });
        let history = control.clone();
        provider.with_reconcile(move |_submission, _token, _accepted| {
            if history.history_accepts.load(Ordering::SeqCst) {
                forge_provider_api::SubmissionEvidence::Accepted {
                    receipt: Some(
                        forge_provider_api::RecoveryToken::new(b"history-fixture".to_vec())
                            .expect("nonempty token"),
                    ),
                    provider_session_id: Some(
                        "fixture-session"
                            .try_into()
                            .expect("fixture ID is nonempty"),
                    ),
                }
            } else {
                forge_provider_api::SubmissionEvidence::Unknown
            }
        });
        (provider, control)
    }

    fn recovery_bridge() -> Result<crate::provider::test_support::ProviderTestSession> {
        crate::provider::test_support::ProviderTestSession::from_native(receipt_provider().0)
    }

    fn request(text: &str) -> ProviderRequest {
        ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "submission-test".into(),
            workspace: "workspace".into(),
            input: ProviderInput::from_text(text),
            mode: PromptMode::Chat,
            model: "model".into(),
            effort: "high".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    /// Seal late execution choices without changing stable input identity or retaining live receipts.
    #[test]
    fn sealed_request_preserves_input_identity_and_releases_capabilities() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let original = request("stored input");
        let receipt = store.prepare_input_request(
            &original,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("input")?),
        )?;
        let weak = Arc::downgrade(&receipt);
        let id = receipt.id().to_owned();
        let mut final_request = original.clone();
        final_request.model = "selected-model".into();
        final_request.effort = "medium".into();
        final_request.mode = PromptMode::ExecutePlan;
        final_request.input = ProviderInput::from_text("decorated execution prompt");
        final_request.submission = Some(receipt.clone());
        receipt.seal_request(final_request)?;
        drop(receipt);
        assert!(
            weak.upgrade().is_none(),
            "journal snapshots must not retain live receipt capabilities"
        );

        let restored = store.prepare_input_request(
            &original,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("input")?),
        )?;
        assert_eq!(restored.id(), id);
        let record = restored.record()?;
        assert_eq!(record.request.model, "selected-model");
        assert_eq!(record.request.effort, "medium");
        assert_eq!(record.request.mode, PromptMode::ExecutePlan);
        assert_eq!(record.request.input.text(), original.input.text());
        assert!(record.request.submission.is_none());
        assert_eq!(record.delivery, SubmissionState::Prepared);
        Ok(())
    }

    /// Refuse late metadata changes once dispatch, acceptance, or cancellation has been committed.
    #[test]
    fn sealed_request_cannot_rewrite_delivery_evidence_or_owner() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        for phase in [
            SubmissionPhase::Sending,
            SubmissionPhase::Accepted,
            SubmissionPhase::Cancelled,
        ] {
            let original = request("input");
            let receipt = store.prepare(&original, fixture_provider_id())?;
            match phase {
                SubmissionPhase::Sending => receipt.sending(Some(token("target")))?,
                SubmissionPhase::Accepted => receipt.accepted(Some(token("receipt")))?,
                SubmissionPhase::Cancelled => receipt.cancel()?,
                SubmissionPhase::Prepared => {
                    unreachable!("fixture only uses settled or uncertain evidence")
                }
            }
            let before = serde_json::to_value(receipt.record()?)?;
            let mut changed = original;
            changed.model = "must-not-rewrite".into();
            let error = receipt.seal_request(changed).unwrap_err();
            match phase {
                SubmissionPhase::Sending => assert!(matches!(
                    error,
                    super::super::ProviderError::DeliveryUncertain
                )),
                SubmissionPhase::Accepted => assert!(
                    matches!(error, super::super::ProviderError::Failure(ref failure)
                    if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)
                ),
                SubmissionPhase::Cancelled => {
                    assert!(matches!(error, super::super::ProviderError::Cancelled))
                }
                SubmissionPhase::Prepared => {
                    unreachable!("fixture only uses settled or uncertain evidence")
                }
            }
            assert_eq!(serde_json::to_value(receipt.record()?)?, before);
        }
        let original = request("scoped input");
        let receipt = store.prepare(&original, fixture_provider_id())?;
        let mut foreign = original.clone();
        foreign.harness_session_id = "other-session".into();
        let error = receipt.seal_request(foreign).unwrap_err();
        assert!(
            matches!(error, super::super::ProviderError::Failure(ref failure)
            if failure.kind() == forge_provider_api::ProviderFailureKind::Protocol)
        );
        assert_eq!(
            receipt.record()?.request.harness_session_id,
            original.harness_session_id
        );
        assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);
        Ok(())
    }

    /// Carry the final invocation configuration through production preparation into the durable receipt.
    #[tokio::test]
    async fn dispatch_seals_selected_configuration_before_native_acceptance() -> Result<()> {
        let root = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(root.path())?);
        let store = SubmissionStore::open(root.path(), trace.clone())?;
        let (native, control) = receipt_provider();
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let original = request("original input");
        let receipt = store.prepare_input_request(
            &original,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("input")?),
        )?;
        let mut invocation = original;
        invocation.model = "selected-model".into();
        invocation.effort = "medium".into();
        invocation.submission = Some(receipt.clone());
        provider
            .session_prompt_with_retry(invocation, None, &trace, &store)
            .await?;
        assert_eq!(control.send_count.load(Ordering::SeqCst), 1);
        assert_eq!(receipt.record()?.request.model, "selected-model");
        assert_eq!(receipt.record()?.request.effort, "medium");
        assert!(matches!(
            receipt.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    /// Refuse native dispatch when the receipt's final invocation configuration cannot be committed.
    #[tokio::test]
    async fn failed_request_seal_prevents_native_dispatch() -> Result<()> {
        let root = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(root.path())?);
        let store = SubmissionStore::open(root.path(), trace.clone())?;
        let (native, control) = receipt_provider();
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let mut invocation = request("input");
        let receipt = store.prepare_input_request(
            &invocation,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("input")?),
        )?;
        invocation.model = "selected-model".into();
        invocation.submission = Some(receipt.clone());
        store.connection.lock().unwrap().execute_batch(
            "CREATE TRIGGER fail_request_seal BEFORE UPDATE ON submission
             WHEN json_extract(NEW.payload,'$.request.model') <> json_extract(OLD.payload,'$.request.model')
             BEGIN SELECT RAISE(FAIL,'fixture storage failure'); END;",
        )?;
        let error = provider
            .session_prompt_with_retry(invocation, None, &trace, &store)
            .await
            .unwrap_err();
        let error = anyhow::Error::new(error);
        assert!(error.chain().any(
            |cause| matches!(cause.downcast_ref::<super::super::ProviderError>(),
            Some(super::super::ProviderError::Failure(failure))
                if failure.kind() == forge_provider_api::ProviderFailureKind::Persistence)
        ));
        assert_eq!(control.send_count.load(Ordering::SeqCst), 0);
        assert_eq!(receipt.record()?.request.model, "model");
        assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);
        Ok(())
    }

    #[tokio::test]
    async fn submission_restart_recovers_only_input_that_never_reached_dispatch() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (native, control) = receipt_provider();
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        control.fail_before_send.store(true, Ordering::SeqCst);
        let earlier = request("earlier instruction\nwith exact whitespace\n");
        assert!(
            crate::provider::test_support::retry_prompt(
                &provider,
                earlier.clone(),
                None,
                &trace,
                &store
            )
            .await
            .is_err()
        );
        drop(store);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        crate::provider::test_support::retry_prompt(
            &provider,
            request("new instruction replaces earlier scope"),
            None,
            &trace,
            &store,
        )
        .await?;
        let sent = control.input.lock().unwrap();
        assert_eq!(sent.len(), 1);
        assert_eq!(
            sent[0]
                .recovered_inputs
                .iter()
                .map(|input| input.input.clone())
                .collect::<Vec<_>>(),
            vec![earlier.input.normalized()]
        );
        assert_eq!(
            sent[0].input,
            forge_provider_api::PromptInput::Text("new instruction replaces earlier scope".into())
        );
        assert!(
            store
                .pending(&earlier, fixture_provider_id(), None)?
                .is_empty()
        );
        Ok(())
    }

    #[tokio::test]
    async fn recovery_does_not_replay_dispatcher_owned_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let provider =
            crate::provider::test_support::ProviderTestSession::from_native(receipt_provider().0)?;
        let owned = store.prepare_identified(
            &request("owned input"),
            fixture_provider_id(),
            Some("message-id".into()),
        )?;
        let mut next = request("next input");
        let submission = store.prepare(&next, fixture_provider_id())?;
        submission
            .recover(provider.as_provider(), &mut next)
            .await?;
        assert!(next.recovered_input.is_empty());
        assert_eq!(owned.record()?.delivery, SubmissionState::Prepared);
        assert_eq!(
            owned.record()?.input_message_id.as_deref(),
            Some("message-id")
        );
        Ok(())
    }

    #[test]
    fn cancellation_preserves_delivery_evidence() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let prepared = store.prepare(
            &request("not sent"),
            forge_providers::registry::default_id(),
        )?;
        assert_eq!(prepared.retire_prepared()?, SubmissionRetirement::Prevented);
        assert_eq!(
            prepared.retire_prepared()?,
            SubmissionRetirement::AlreadyCancelled
        );
        prepared.cancel()?;
        assert_eq!(prepared.record()?.delivery, SubmissionState::Cancelled);
        let dispatched = store.prepare(
            &request("dispatched"),
            forge_providers::registry::default_id(),
        )?;
        let stale = dispatched.record()?;
        assert_eq!(stale.delivery, SubmissionState::Prepared);
        dispatched.sending(Some(token("target")))?;
        assert_eq!(dispatched.retire_prepared()?, SubmissionRetirement::Sending);
        dispatched.cancel()?;
        assert!(matches!(
            dispatched.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        dispatched.accepted(Some(token("receipt")))?;
        assert_eq!(
            dispatched.retire_prepared()?,
            SubmissionRetirement::Accepted
        );
        dispatched.cancel()?;
        assert!(matches!(
            dispatched.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    #[test]
    fn task_entry_identity_reuses_durable_delivery_and_notifies_after_receipt_commit() -> Result<()>
    {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let first = store.prepare_record(
            &request("entry"),
            forge_providers::registry::default_id(),
            Some("entry-id".into()),
            PreparationIdentity::Task,
        )?;
        let id = first.id().to_owned();
        let accepted = first.acceptance();
        first.sending(Some(token("target")))?;
        first.accepted(Some(token("receipt")))?;
        assert!(*accepted.borrow());
        assert!(matches!(
            first.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        let duplicate = store.prepare_record(
            &request("entry"),
            forge_providers::registry::default_id(),
            Some("entry-id".into()),
            PreparationIdentity::Task,
        )?;
        assert_eq!(duplicate.id(), id);
        assert!(
            store
                .prepare_record(
                    &request("changed"),
                    forge_providers::registry::default_id(),
                    Some("entry-id".into()),
                    PreparationIdentity::Task,
                )
                .is_err()
        );
        drop(accepted);
        drop(duplicate);
        drop(first);
        drop(store);
        let reopened = SubmissionStore::open(directory.path(), trace)?;
        let recovered = reopened.prepare_record(
            &request("entry"),
            forge_providers::registry::default_id(),
            Some("entry-id".into()),
            PreparationIdentity::Task,
        )?;
        assert_eq!(recovered.id(), id);
        assert!(matches!(
            recovered.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    /// Native acceptance is evidence only; reconciling it does not run host task callbacks.
    #[tokio::test]
    async fn accepted_receipt_reconciliation_preserves_notification_and_identity() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let store = SubmissionStore::open(
            directory.path(),
            Arc::new(TraceStore::open(directory.path())?),
        )?;
        let submission =
            store.prepare(&request("entry"), forge_providers::registry::default_id())?;
        let accepted = submission.acceptance();
        submission.accepted(Some(token("receipt")))?;
        let before = submission.record()?;
        let (native, _) = receipt_provider();
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let after = submission.reconcile(provider.as_provider()).await?;
        assert!(*accepted.borrow());
        assert_eq!(after.id, before.id);
        assert_eq!(after.delivery, before.delivery);
        Ok(())
    }

    #[tokio::test]
    async fn submission_lost_ack_blocks_replay_and_retains_every_new_prompt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (native, control) = receipt_provider();
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        control.lose_ack.store(true, Ordering::SeqCst);
        assert!(
            crate::provider::test_support::retry_prompt(
                &provider,
                request("first"),
                None,
                &trace,
                &store
            )
            .await
            .is_err()
        );
        for text in ["second", "third"] {
            let mut input = request(text);
            input.submission = Some(store.prepare(&input, fixture_provider_id())?);
            let error =
                crate::provider::test_support::retry_prompt(&provider, input, None, &trace, &store)
                    .await
                    .unwrap_err();
            assert!(
                error.chain().any(|cause| matches!(
                cause.downcast_ref::<forge_provider_api::ProviderError>(),
                Some(forge_provider_api::ProviderError::DeliveryUncertain
                    | forge_provider_api::ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::DeliveryUncertain))
            )),
                "{error:?}"
            );
            assert!(!error.to_string().contains("connection lost after write"));
        }
        assert_eq!(control.send_count.load(Ordering::SeqCst), 0);
        assert_eq!(
            store
                .pending(&request(""), fixture_provider_id(), None)?
                .len(),
            3
        );
        control.history_accepts.store(true, Ordering::SeqCst);
        provider.recover(request("").owner()?).await?;
        crate::provider::test_support::retry_prompt(
            &provider,
            request("fourth"),
            None,
            &trace,
            &store,
        )
        .await?;
        let sent = control.input.lock().unwrap();
        assert_eq!(sent.len(), 1);
        assert_eq!(
            sent[0]
                .recovered_inputs
                .iter()
                .map(|input| input.input.clone())
                .collect::<Vec<_>>(),
            vec![
                ProviderInput::from_text("second").normalized(),
                ProviderInput::from_text("third").normalized()
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn submission_ack_commit_is_atomic_with_recovered_inputs() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let original = store.prepare(&request("first"), fixture_provider_id())?;
        let next = store.prepare(&request("second"), fixture_provider_id())?;
        let mut combined = request("second");
        let provider = recovery_bridge()?;
        next.recover(provider.as_provider(), &mut combined).await?;
        next.sending(Some(token("target")))?;
        store.connection.lock().unwrap().execute_batch(&format!(
            "CREATE TRIGGER fail_ack BEFORE UPDATE ON submission WHEN NEW.id='{}' BEGIN SELECT RAISE(ABORT,'disk commit failure'); END;", original.id()))?;
        assert!(next.accepted(Some(token("ack"))).is_err());
        assert_eq!(original.record()?.delivery, SubmissionState::Prepared);
        assert!(matches!(
            next.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        store
            .connection
            .lock()
            .unwrap()
            .execute_batch("DROP TRIGGER fail_ack")?;
        next.accepted(Some(token("ack")))?;
        next.accepted(Some(token("duplicate ack")))?;
        assert!(matches!(
            original.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        assert!(matches!(
            next.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    #[tokio::test]
    async fn submission_recovery_does_not_consume_future_queued_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let earlier = store.prepare(&request("earlier"), fixture_provider_id())?;
        let current = store.prepare(&request("current"), fixture_provider_id())?;
        let later = store.prepare(&request("later"), fixture_provider_id())?;
        let mut input = request("current");
        let provider = recovery_bridge()?;
        current.recover(provider.as_provider(), &mut input).await?;
        assert_eq!(
            input.recovered_input,
            vec![ProviderInput::from_text("earlier")]
        );
        current.sending(Some(token("target")))?;
        current.accepted(Some(token("ack")))?;
        assert!(matches!(
            earlier.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        assert_eq!(later.record()?.delivery, SubmissionState::Prepared);
        Ok(())
    }

    #[tokio::test]
    async fn submission_restart_retains_skill_and_attachment_bytes() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let source = directory.path().join("source.txt");
        let bytes = "retained attachment\n".repeat(4096).into_bytes();
        std::fs::write(&source, &bytes)?;
        let attachment = crate::attachment::Attachment::import(directory.path(), &source)?;
        let attachments = crate::attachment::resolve_input(
            directory.path(),
            &json!({"attachments":[attachment.selection(1)?]}),
        )
        .await?;
        std::fs::remove_file(&source)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let mut original = request("");
        original.input = ProviderInput::Skill {
            name: "review".into(),
            arguments: "  exact arguments\n".into(),
            attachments,
        };
        let earlier = store.prepare(&original, fixture_provider_id())?;
        drop(earlier);
        drop(store);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let next = store.prepare(&request("latest"), fixture_provider_id())?;
        let mut input = request("latest");
        let provider = recovery_bridge()?;
        next.recover(provider.as_provider(), &mut input).await?;
        assert_eq!(input.recovered_input, vec![original.input]);
        let files = input
            .input_attachments()
            .iter()
            .map(crate::attachment::InputAttachment::prepare)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        assert_eq!(files[0].contents.as_ref(), bytes);
        Ok(())
    }

    #[tokio::test]
    async fn submission_acknowledged_stream_failure_is_not_missing_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let original = store.prepare(&request("accepted"), fixture_provider_id())?;
        original.sending(Some(token("target")))?;
        original.accepted(Some(token("native ack")))?;
        original.failed("stream timed out".into())?;
        let next = store.prepare(&request("latest"), fixture_provider_id())?;
        let mut input = request("latest");
        let provider = recovery_bridge()?;
        next.recover(provider.as_provider(), &mut input).await?;
        assert!(input.recovered_input.is_empty());
        assert_eq!(
            original.record()?.error.as_deref(),
            Some("stream timed out")
        );
        Ok(())
    }

    #[tokio::test]
    async fn submission_cancellation_does_not_resurrect_user_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let original = store.prepare(&request("cancel me"), fixture_provider_id())?;
        original.cancel()?;
        let next = store.prepare(&request("new"), fixture_provider_id())?;
        let mut new = request("new");
        let provider = recovery_bridge()?;
        next.recover(provider.as_provider(), &mut new).await?;
        assert!(new.recovered_input.is_empty());
        next.sending(None)?;
        next.cancel()?;
        assert!(matches!(
            next.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        let following = store.prepare(&request("following"), fixture_provider_id())?;
        assert!(
            following
                .recover(provider.as_provider(), &mut request("following"))
                .await
                .is_err()
        );
        Ok(())
    }
}
