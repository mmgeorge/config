use crate::provider::ScopeOwner;
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
use forge_provider_api::{DeliveryAttemptId, RecoveryToken, SubmissionId};

const SUBMISSION_FORMAT_VERSION: u32 = 4;

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
    /// The user discarded input whose delivery stayed uncertain. The provider may or may not
    /// hold it, so the record is terminal: it never returns to Prepared, never accepts later
    /// evidence, and no longer blocks dispatch of other input.
    Discarded,
}

impl SubmissionState {
    /// Require unsent input while preserving cancellation and previous delivery evidence.
    fn require_prepared(&self) -> Result<(), super::ProviderError> {
        match self {
            Self::Prepared => Ok(()),
            Self::Cancelled | Self::Discarded => Err(super::ProviderError::Cancelled),
            Self::Sending { .. } => Err(super::ProviderError::DeliveryUncertain),
            Self::Accepted { .. } => Err(super::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::WrongInvocation,
                ),
            )),
        }
    }
}

/// Certify the journal state at the atomic attempt to prevent prepared input dispatch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SubmissionRetirement {
    /// Input is durably cancelled and cannot have been dispatched, including repeated retirement.
    Cancelled,
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
    Discarded,
}

impl From<&SubmissionState> for SubmissionPhase {
    fn from(state: &SubmissionState) -> Self {
        match state {
            SubmissionState::Prepared => Self::Prepared,
            SubmissionState::Sending { .. } => Self::Sending,
            SubmissionState::Accepted { .. } => Self::Accepted,
            SubmissionState::Cancelled => Self::Cancelled,
            SubmissionState::Discarded => Self::Discarded,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
/// Full input retained independently of exchange completion and diagnostic logging.
pub struct SubmissionRecord {
    format_version: u32,
    revision: u64,
    /// Exact delivery attempt retained across process loss and read-only reconciliation.
    pub(crate) attempt_id: DeliveryAttemptId,
    /// Distinguish Harness recovery instructions from original user input.
    origin: JournalOrigin,
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

impl SubmissionRecord {
    /// Get whether this journal record came from explicit user input.
    pub(crate) fn is_user_input(&self) -> bool {
        matches!(self.origin, JournalOrigin::User)
    }
}

/// Preserve the author of a logical input independently of its native delivery attempt.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum JournalOrigin {
    User,
    HostContinuation,
}

/// Retain read-only evidence until the owning exchange validates its journal revision.
#[derive(Debug)]
pub(crate) struct SubmissionReconciliation {
    snapshot: SubmissionRecord,
    evidence: forge_provider_api::SubmissionEvidence,
}

/// Reject incompatible journals before decoding the changed invocation payload.
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
            request: serde_json::Value,
            revision: Option<u64>,
            attempt_id: Option<DeliveryAttemptId>,
            origin: Option<JournalOrigin>,
            delivery: serde_json::Value,
            included: Vec<String>,
            error: Option<String>,
        }
        let stored = StoredRecord::deserialize(deserializer)?;
        if stored.format_version != Some(SUBMISSION_FORMAT_VERSION) {
            return Err(serde::de::Error::custom(
                "This session uses an older host contract; create a new session",
            ));
        }
        let delivery = serde_json::from_value(stored.delivery).map_err(serde::de::Error::custom)?;
        Ok(Self {
            format_version: SUBMISSION_FORMAT_VERSION,
            revision: stored
                .revision
                .ok_or_else(|| serde::de::Error::custom("missing submission revision"))?,
            attempt_id: stored
                .attempt_id
                .ok_or_else(|| serde::de::Error::custom("missing delivery attempt"))?,
            origin: stored
                .origin
                .ok_or_else(|| serde::de::Error::custom("missing input origin"))?,
            input_message_id: stored.input_message_id,
            task_receipt: stored.task_receipt,
            id: stored.id,
            request: serde_json::from_value(stored.request).map_err(serde::de::Error::custom)?,
            delivery,
            included: stored.included,
            error: stored.error,
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
    _lease: crate::storage::state::StateLease,
}

impl SubmissionStore {
    /// Journal a newly authored recovery instruction separately from the original user input.
    pub(crate) fn prepare_continuation(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderId,
    ) -> Result<Arc<Submission>> {
        let submission = self.prepare(request, provider)?;
        submission.update(|record| {
            record.origin = JournalOrigin::HostContinuation;
            Ok(())
        })?;
        Ok(submission)
    }
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
        let lease = crate::storage::state::StateLease::open(root)?;
        let directory = crate::storage::state::directory(root);
        #[expect(
            clippy::disallowed_methods,
            reason = "the submission journal is a separate database with its own writer"
        )]
        let connection = Connection::open(directory.join("provider-submissions.sqlite3"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        let _bootstrap = crate::storage::bootstrap_lock(&directory, "provider-submissions")?;
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
            _lease: lease,
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
            revision: 0,
            attempt_id: DeliveryAttemptId::new(uuid::Uuid::new_v4().to_string())?,
            origin: JournalOrigin::User,
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
                        forge_provider_api::ProviderOperation::TurnStart,
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
                            forge_provider_api::ProviderOperation::TurnStart,
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

    /// Prove that every retained attempt and every selected local input remained unconsumed.
    /// Accepted delivery never returns to Prepared. Sending returns only on positive rejection;
    /// retirement changes only Prepared to Cancelled, so these states also cover earlier attempts.
    pub(crate) fn proves_session_unconsumed(
        &self,
        owner: &super::ThreadOwner,
        workspace: &str,
        provider: super::ProviderId,
        input_ids: &[String],
    ) -> Result<bool> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let mut statement = connection.prepare(
            "SELECT runtime,provider,payload FROM submission WHERE runtime=?1 OR json_extract(payload,'$.request.harness_session_id')=?2",
        )?;
        let expected_runtime = owner.runtime_key();
        let expected_provider = serde_json::to_string(&provider)?;
        let records = statement.query_map(
            params![expected_runtime, owner.session_id().as_str()],
            |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, String>(2)?,
                ))
            },
        )?;
        let mut covered = std::collections::HashSet::new();
        let mut records_seen = std::collections::HashSet::new();
        let mut included = Vec::new();
        for record in records {
            let (runtime, provider, payload) = record?;
            let record: SubmissionRecord = serde_json::from_str(&payload)?;
            if runtime != expected_runtime
                || provider != expected_provider
                || record.request.owner()? != *owner
                || record.request.harness_session_id != owner.session_id().as_str()
                || record.request.workspace != workspace
                || !matches!(
                    record.delivery,
                    SubmissionState::Prepared | SubmissionState::Cancelled
                )
            {
                return Ok(false);
            }
            records_seen.insert(record.id);
            included.extend(record.included);
            if let Some(id) = record.input_message_id {
                covered.insert(id);
            }
        }
        Ok(input_ids.iter().all(|id| covered.contains(id))
            && included.iter().all(|id| records_seen.contains(id)))
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
                    forge_provider_api::ProviderOperation::TurnStart,
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
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
            child_agent: None,
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

/// Bind native receipt callbacks to the immutable attempt captured before dispatch.
struct AttemptReceipt {
    submission: Arc<Submission>,
    attempt_id: DeliveryAttemptId,
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
    /// Capture receipt authority which cannot acknowledge or reject a later delivery attempt.
    pub(crate) fn receipt_for_attempt(
        self: &Arc<Self>,
    ) -> Result<Arc<dyn forge_provider_api::submission::InputReceipt>, super::ProviderError> {
        let attempt_id = self.record().map_err(journal_failure)?.attempt_id;
        Ok(Arc::new(AttemptReceipt {
            submission: self.clone(),
            attempt_id,
        }))
    }
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
                    forge_provider_api::ProviderOperation::TurnStart,
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
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                    )
                    .into());
                }
                SubmissionState::Cancelled | SubmissionState::Discarded => {
                    return Err(super::ProviderError::Cancelled.into());
                }
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
                acknowledged_child_reports,
                control_context: _,
                child_agent,
            } = request;
            record.request.execution_owner = execution_owner;
            record.request.child_agent = child_agent;
            record.request.mode = mode;
            record.request.model = model;
            record.request.effort = effort;
            record.request.context_window = context_window;
            record.request.service_tier = service_tier;
            record.request.access = access;
            record.request.approval_mode = approval_mode;
            record.request.provider_session_id = provider_session_id;
            record.request.acknowledged_child_reports = acknowledged_child_reports;
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
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Protocol,
            ))
        };
        let record = self.record().map_err(|_| invalid())?;
        if record.request.owner()? != scope.owner() {
            return Err(invalid());
        }
        let reporter = tracker
            .track_input(binding, scope, self.receipt_id.clone(), record.attempt_id)
            .map_err(|_| invalid())?;
        *self.dispatch_observer.lock().map_err(|_| invalid())? = Some(tracker.observer());
        Ok(reporter)
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
        self.record()?.delivery.require_prepared()?;
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
    /// Observe delivery without changing eligibility or publishing acceptance from a worker.
    pub(crate) async fn observe_reconciliation(
        self: &Arc<Self>,
        provider: &Provider,
    ) -> Result<SubmissionReconciliation> {
        let record = self.record()?;
        let evidence = if matches!(record.delivery, SubmissionState::Sending { .. }) {
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
            provider
                .input_reconcile(forge_provider_api::InputReconcileRequest {
                    submission_id: self.receipt_id.clone(),
                    attempt_id: record.attempt_id.clone(),
                    provider_session_id: record.request.provider_session_id.clone(),
                    token: target.cloned(),
                    accepted_receipts: known,
                })
                .await?
        } else {
            forge_provider_api::SubmissionEvidence::Unknown
        };
        Ok(SubmissionReconciliation {
            snapshot: record,
            evidence,
        })
    }

    /// Apply evidence only while the exact journal revision, attempt, and history remain current.
    pub(crate) fn apply_reconciliation(
        &self,
        observation: SubmissionReconciliation,
    ) -> Result<SubmissionRecord> {
        let SubmissionReconciliation { snapshot, evidence } = observation;
        if let forge_provider_api::SubmissionEvidence::Accepted {
            receipt,
            provider_session_id,
        } = evidence
        {
            self.commit_acceptance(receipt, Some((&snapshot, provider_session_id)), None)
                .map_err(uncertain_journal_failure)?;
            return self.record();
        }
        self.update(|record| {
            if record.id != snapshot.id
                || record.revision != snapshot.revision
                || record.attempt_id != snapshot.attempt_id
                || record.delivery != snapshot.delivery
                || record.request.provider_session_id != snapshot.request.provider_session_id
            {
                return Ok(());
            }
            match evidence {
                forge_provider_api::SubmissionEvidence::Accepted { .. } => {
                    unreachable!("accepted evidence is committed atomically above")
                }
                forge_provider_api::SubmissionEvidence::NotAccepted => {
                    if matches!(record.delivery, SubmissionState::Sending { .. }) {
                        record.delivery = SubmissionState::Prepared;
                        record.attempt_id =
                            DeliveryAttemptId::new(uuid::Uuid::new_v4().to_string())?;
                        record.error = Some("Input was not accepted".into());
                    }
                }
                forge_provider_api::SubmissionEvidence::Unknown => {}
            }
            Ok(())
        })
        .map_err(journal_failure)?;
        let record = self.record()?;
        if matches!(record.delivery, SubmissionState::Accepted { .. }) {
            self.notify_accepted();
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

    /// Load the original IDs and delivery attempts of every journaled input grouped into this request.
    pub(crate) fn included_inputs(
        &self,
    ) -> Result<Vec<forge_provider_api::CoveredInput>, super::ProviderError> {
        let record = self.record().map_err(journal_failure)?;
        let owner = record.request.owner()?;
        record
            .included
            .iter()
            .map(|id| {
                let original = self
                    .store
                    .submission(id)
                    .map_err(journal_failure)?
                    .record()
                    .map_err(journal_failure)?;
                if original.id != *id || original.request.owner()? != owner {
                    return Err(super::ProviderError::Observation(
                        forge_provider_api::ObservationFailure::OwnershipConflict,
                    ));
                }
                Ok(forge_provider_api::CoveredInput::new(
                    original.id.try_into()?,
                    original.attempt_id,
                ))
            })
            .collect()
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
            record.delivery.require_prepared()?;
            record.delivery = SubmissionState::Sending { target };
            Ok(())
        })
    }

    /// Atomically acknowledges this message and every earlier input it carried.
    pub fn accepted(&self, receipt: Option<RecoveryToken>) -> Result<()> {
        self.commit_acceptance(receipt, None, None)
    }

    /// Validate recovered acceptance under the same transaction that commits included inputs.
    fn commit_acceptance(
        &self,
        receipt: Option<RecoveryToken>,
        recovered: Option<(
            &SubmissionRecord,
            Option<forge_provider_api::ProviderSessionId>,
        )>,
        attempt: Option<&DeliveryAttemptId>,
    ) -> Result<()> {
        let mut connection = self
            .store
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let mut record = self.record()?;
        ensure!(
            attempt.is_none_or(|attempt| attempt == &record.attempt_id),
            super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::WrongInvocation,
            ))
        );
        ensure!(
            attempt.is_none()
                || matches!(
                    record.delivery,
                    SubmissionState::Sending { .. } | SubmissionState::Accepted { .. }
                ),
            super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Protocol,
            ))
        );
        if let Some((snapshot, _)) = &recovered
            && (record.id != snapshot.id
                || record.revision != snapshot.revision
                || record.attempt_id != snapshot.attempt_id
                || record.delivery != snapshot.delivery
                || record.request.provider_session_id != snapshot.request.provider_session_id)
        {
            return Ok(());
        }
        if matches!(record.delivery, SubmissionState::Accepted { .. }) {
            drop(transaction);
            drop(connection);
            self.notify_accepted();
            return Ok(());
        }
        ensure!(
            !matches!(
                record.delivery,
                SubmissionState::Cancelled | SubmissionState::Discarded
            ),
            "cancelled or discarded submission cannot be acknowledged"
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
        if let Some((_, provider_session_id)) = recovered {
            record.request.provider_session_id = provider_session_id;
        }
        record.revision = record
            .revision
            .checked_add(1)
            .ok_or_else(|| anyhow::anyhow!("submission revision exhausted"))?;
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
            earlier.revision = earlier
                .revision
                .checked_add(1)
                .ok_or_else(|| anyhow::anyhow!("submission revision exhausted"))?;
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
            record.attempt_id = DeliveryAttemptId::new(uuid::Uuid::new_v4().to_string())?;
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

    /// Retire uncertain input at the user's explicit request without replaying it.
    ///
    /// A `Sending` record may already be in the provider's conversation, so it becomes the
    /// terminal `Discarded` state instead of `Cancelled`, which a later preparation of the same
    /// input identity could revive. Prepared input retires to `Cancelled` as usual, and accepted
    /// or already discarded input is left unchanged.
    pub(crate) fn discard(&self) -> Result<()> {
        self.update(|record| {
            match record.delivery {
                SubmissionState::Sending { .. } => record.delivery = SubmissionState::Discarded,
                SubmissionState::Prepared => record.delivery = SubmissionState::Cancelled,
                SubmissionState::Accepted { .. }
                | SubmissionState::Cancelled
                | SubmissionState::Discarded => {}
            }
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
        self.retire_attempt(None)
    }

    /// Prevent only the exact registered delivery attempt without cancelling a successor attempt.
    pub(crate) fn retire_prepared_attempt(
        &self,
        attempt: &DeliveryAttemptId,
    ) -> std::result::Result<SubmissionRetirement, super::ProviderError> {
        self.retire_attempt(Some(attempt))
    }

    /// Validate attempt authority and commit prepared retirement in one journal transaction.
    fn retire_attempt(
        &self,
        attempt: Option<&DeliveryAttemptId>,
    ) -> std::result::Result<SubmissionRetirement, super::ProviderError> {
        self.update(|record| {
            ensure!(
                attempt.is_none_or(|attempt| attempt == &record.attempt_id),
                super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::WrongInvocation,
                ))
            );
            Ok(match record.delivery {
                SubmissionState::Prepared => {
                    record.delivery = SubmissionState::Cancelled;
                    SubmissionRetirement::Cancelled
                }
                SubmissionState::Cancelled => SubmissionRetirement::Cancelled,
                // A discarded record keeps its uncertainty; retirement never authorizes replay.
                SubmissionState::Sending { .. } | SubmissionState::Discarded => {
                    SubmissionRetirement::Sending
                }
                SubmissionState::Accepted { .. } => SubmissionRetirement::Accepted,
            })
        })
        .map_err(journal_failure)
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
        record.revision = record
            .revision
            .checked_add(1)
            .ok_or_else(|| anyhow::anyhow!("submission revision exhausted"))?;
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

    /// Reconcile fixture recovery under its test body's explicit journal ownership.
    #[cfg(test)]
    pub async fn recover(&self, provider: &Provider, request: &mut ProviderRequest) -> Result<()> {
        self.recover_with_owner(provider, request, None).await
    }

    /// Observe uncertain deliveries and submit their evidence to the retained execution owner.
    pub(crate) async fn recover_owned(
        &self,
        provider: &Provider,
        request: &mut ProviderRequest,
        owner: &super::session_execution::SessionExecution,
    ) -> Result<()> {
        self.recover_with_owner(provider, request, Some(owner))
            .await
    }

    /// Attach only inputs whose evidence was revalidated at the owning admission boundary.
    async fn recover_with_owner(
        &self,
        provider: &Provider,
        request: &mut ProviderRequest,
        owner: Option<&super::session_execution::SessionExecution>,
    ) -> Result<()> {
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
                let observation = handle.observe_reconciliation(provider).await?;
                let reconciled = match owner {
                    Some(owner) => owner.reconcile_input(handle, observation).await?,
                    None => handle.apply_reconciliation(observation)?,
                };
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
        if request.mode == super::PromptMode::Background {
            // Each background request contains the current complete snapshot. An older
            // unconsumed snapshot is superseded, not another input to concatenate.
            for previous in &pending {
                if previous.id != self.id
                    && previous.input_message_id.is_none()
                    && previous.request.mode == super::PromptMode::Background
                {
                    let previous = self.store.submission(&previous.id)?;
                    if matches!(previous.retire_prepared()?, SubmissionRetirement::Sending) {
                        return Err(super::ProviderError::DeliveryUncertain.into());
                    }
                }
            }
        }
        let pending = self
            .store
            .pending(request, provider.descriptor().kind, Some(&self.id))?;
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

#[cfg(test)]
#[async_trait::async_trait]
impl forge_provider_api::submission::InputReceipt for Submission {
    fn id(&self) -> &SubmissionId {
        &self.receipt_id
    }

    /// Get the journaled attempt which must match the immutable dispatch request.
    fn attempt_id(&self) -> Result<DeliveryAttemptId, forge_provider_api::ProviderError> {
        self.record()
            .map(|record| record.attempt_id)
            .map_err(journal_failure)
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
            .map_err(uncertain_journal_failure)
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
            .map_err(uncertain_journal_failure)
    }
}

#[async_trait::async_trait]
impl forge_provider_api::submission::InputReceipt for AttemptReceipt {
    /// Get the stable logical input independently of this attempt's delivery identity.
    fn id(&self) -> &SubmissionId {
        self.submission.receipt_id()
    }

    /// Get the immutable attempt captured before native preparation and dispatch.
    fn attempt_id(&self) -> Result<DeliveryAttemptId, super::ProviderError> {
        Ok(self.attempt_id.clone())
    }

    /// Persist sending only while this exact receipt still owns a prepared journal entry.
    async fn record_sending(
        self: Arc<Self>,
        token: Option<RecoveryToken>,
    ) -> Result<(), super::ProviderError> {
        let attempt_id = self.attempt_id.clone();
        self.submission
            .persist(move |submission| {
                submission.update(|record| {
                    ensure!(
                        record.attempt_id == attempt_id,
                        super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::WrongInvocation,
                        ))
                    );
                    record.delivery.require_prepared()?;
                    record.delivery = SubmissionState::Sending { target: token };
                    Ok(())
                })
            })
            .await
            .map_err(journal_failure)
    }

    /// Atomically acknowledge this attempt and every earlier input it carried.
    async fn record_accepted(
        self: Arc<Self>,
        receipt: Option<RecoveryToken>,
    ) -> Result<(), super::ProviderError> {
        let attempt_id = self.attempt_id.clone();
        self.submission
            .persist(move |submission| {
                submission.commit_acceptance(receipt, None, Some(&attempt_id))
            })
            .await
            .map_err(uncertain_journal_failure)
    }

    /// Rotate delivery authority only after a positive rejection for this exact attempt.
    async fn record_rejected(
        self: Arc<Self>,
        reason: super::ProviderError,
    ) -> Result<(), super::ProviderError> {
        let attempt_id = self.attempt_id.clone();
        self.submission
            .persist(move |submission| {
                submission.update(|record| {
                    ensure!(
                        record.attempt_id == attempt_id
                            && matches!(
                                record.delivery,
                                SubmissionState::Prepared | SubmissionState::Sending { .. }
                            ),
                        "receipt no longer owns rejectable input"
                    );
                    record.delivery = SubmissionState::Prepared;
                    record.attempt_id = DeliveryAttemptId::new(uuid::Uuid::new_v4().to_string())?;
                    record.error = Some(reason.to_string());
                    Ok(())
                })
            })
            .await
            .map_err(journal_failure)
    }

    /// Retain uncertainty without allowing an old worker to overwrite a replacement attempt.
    async fn record_failure(
        self: Arc<Self>,
        reason: super::ProviderError,
    ) -> Result<(), super::ProviderError> {
        let attempt_id = self.attempt_id.clone();
        self.submission
            .persist(move |submission| {
                submission.update(|record| {
                    ensure!(
                        record.attempt_id == attempt_id,
                        super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::WrongInvocation,
                        ))
                    );
                    record.error = Some(reason.to_string());
                    Ok(())
                })
            })
            .await
            .map_err(uncertain_journal_failure)
    }
}

/// Preserve normalized receipt failures at the persistence boundary.
pub(crate) fn journal_failure(error: anyhow::Error) -> super::ProviderError {
    match error.downcast::<super::ProviderError>() {
        Ok(error) => error,
        Err(error) => {
            let failure = if error.is::<crate::provider::ThreadOwnerError>() {
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            } else {
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Persistence,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Persistence,
                    forge_provider_api::FailureDisposition::Unchanged,
                )
            };
            super::ProviderError::Failure(failure)
        }
    }
}

/// Preserve accepted-or-uncertain delivery when recording its evidence fails after native effects.
fn uncertain_journal_failure(error: anyhow::Error) -> super::ProviderError {
    match error.downcast::<super::ProviderError>() {
        Ok(super::ProviderError::Failure(failure)) => super::ProviderError::Failure(
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                failure.kind(),
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Persistence,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
        ),
        Ok(error) => super::ProviderError::Combined {
            primary: Box::new(super::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Persistence,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Persistence,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
            )),
            secondary: vec![error],
        },
        Err(_) => super::ProviderError::Failure(
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Persistence,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Persistence,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
        ),
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

    /// Positive rejection certifies an older attempt; unknown or accepted attempts remain barriers.
    #[test]
    fn session_nonconsumption_requires_positive_evidence_for_every_input_and_attempt() -> Result<()>
    {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let request = request("retained input");
        let owner = request.owner()?;
        let ids = vec!["retained-input".to_owned()];
        let proof = || {
            store.proves_session_unconsumed(&owner, &request.workspace, fixture_provider_id(), &ids)
        };
        assert!(
            !proof()?,
            "missing journal evidence authorized identity reset"
        );
        let submission = store.prepare_input_request(
            &request,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new(&ids[0])?),
        )?;
        assert!(proof()?);
        let old_attempt = submission.record()?.attempt_id;
        submission.sending(None)?;
        submission.failed("unknown native delivery".into())?;
        submission.cancel()?;
        assert!(
            !proof()?,
            "retirement converted uncertain delivery into nonconsumption"
        );
        submission.rejected("positive native NotAccepted evidence".into())?;
        assert_ne!(old_attempt, submission.record()?.attempt_id);
        assert!(
            proof()?,
            "positive rejection did not certify the earlier attempt"
        );
        submission.cancel()?;
        assert!(
            proof()?,
            "positive prepared retirement lost nonconsumption evidence"
        );
        let successor = store.prepare_input_request(
            &request,
            fixture_provider_id(),
            SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new(&ids[0])?),
        )?;
        successor.sending(None)?;
        successor.accepted(None)?;
        assert!(
            successor
                .rejected("cannot undo accepted input".into())
                .is_err()
        );
        successor.cancel()?;
        assert!(
            !proof()?,
            "a prior accepted attempt authorized identity reset"
        );
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
            move |request, _sink, start, _cancelled| {
                let log = log.clone();
                async move {
                    log.input.lock().unwrap().push(request.clone());
                    log.send_count.fetch_add(1, Ordering::SeqCst);
                    start.observed(1).await?;
                    Ok(crate::provider::test_support::native_result(
                        &request,
                        forge_provider_api::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                    ))
                }
            },
        );
        let preparation = control.clone();
        provider.with_pre_dispatch(move |_request| {
            let preparation = preparation.clone();
            async move {
                if preparation.fail_before_send.swap(false, Ordering::SeqCst) {
                    return Err(forge_provider_api::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                    ));
                }
                Ok(())
            }
        });
        let admission = control.clone();
        provider.with_admission(move |_request| {
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

    fn request(text: &str) -> ProviderRequest {
        ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "submission-test".into(),
            workspace: "workspace".into(),
            input: ProviderInput::from_user_text(text),
            mode: PromptMode::Chat,
            model: "model".into(),
            effort: "high".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
            child_agent: None,
        }
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
        assert_eq!(control.send_count.load(Ordering::SeqCst), 0);
        let pending = store.pending(&earlier, fixture_provider_id(), None)?;
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0].delivery, SubmissionState::Prepared);
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
            forge_provider_api::forge::TurnInput::from("new instruction replaces earlier scope")
        );
        assert!(
            store
                .pending(&earlier, fixture_provider_id(), None)?
                .is_empty()
        );
        Ok(())
    }

    /// Keep a late native acknowledgment authoritative when cancellation follows durable dispatch.
    #[tokio::test]
    async fn cancellation_before_late_acceptance_preserves_durable_consumption() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let input = store.prepare(&request("late accepted input"), fixture_provider_id())?;
        let id = input.id().to_owned();
        let adapter = input.receipt_for_attempt()?;
        adapter
            .clone()
            .record_sending(Some(token("dispatch-target")))
            .await?;
        let (entered, captured) = tokio::sync::oneshot::channel();
        let (release, resumed) = tokio::sync::oneshot::channel();
        let acknowledgment = tokio::spawn(async move {
            entered.send(()).expect("test owns the receipt barrier");
            resumed.await.expect("test releases the receipt barrier");
            adapter.record_accepted(Some(token("late-receipt"))).await
        });
        captured.await?;
        input.cancel()?;
        assert!(matches!(
            input.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        release
            .send(())
            .expect("acknowledgment retains its barrier");
        acknowledgment.await??;
        assert!(matches!(
            input.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        drop(input);
        drop(store);
        let reopened =
            SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let restored = reopened.submission(&id)?;
        assert!(matches!(
            restored.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        assert!(
            restored
                .receipt_for_attempt()?
                .record_sending(None)
                .await
                .is_err()
        );
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
        let pending = store.pending(&request(""), fixture_provider_id(), None)?;
        assert_eq!(pending.len(), 1);
        assert!(matches!(
            pending[0].delivery,
            SubmissionState::Sending { .. }
        ));
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
                ProviderInput::from_user_text("second").normalized(),
                ProviderInput::from_user_text("third").normalized()
            ]
        );
        Ok(())
    }
}
