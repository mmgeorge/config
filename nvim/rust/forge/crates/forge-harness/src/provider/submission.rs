use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{
        Arc, Mutex, OnceLock, Weak,
        atomic::{AtomicBool, Ordering},
    },
};

use anyhow::{Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};
use serde::{Deserialize, Serialize};
use serde_json::json;

use super::{Provider, ProviderRequest};
use crate::trace::TraceStore;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
/// Durable delivery evidence. Only Prepared is safe to include in a later submission.
pub enum SubmissionState {
    /// No adapter has crossed the dispatch boundary.
    Prepared,
    /// Dispatch may have happened. Absence of an acknowledgment does not permit replay.
    Sending { target: SubmissionTarget },
    /// Provider acknowledgment or correlated native history proves acceptance.
    Accepted {
        receipt: String,
        target: Option<SubmissionTarget>,
    },
    /// The user explicitly cancelled this submission.
    Cancelled,
}

pub use forge_provider_api::submission::SubmissionTarget;

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Full input retained independently of exchange completion and diagnostic logging.
pub struct SubmissionRecord {
    #[serde(default)]
    pub input_message_id: Option<String>,
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

#[derive(Clone, Copy)]
enum PreparationIdentity {
    Unidentified,
    Task,
    EditableInput,
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
            record: Mutex::new(record),
            acceptance: tokio::sync::watch::channel(accepted).0,
            steering_queued: tokio::sync::watch::channel(false).0,
            steering_active: AtomicBool::new(false),
            accepted_action: Mutex::new(None),
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
        provider: super::ProviderKind,
    ) -> Result<Arc<Submission>> {
        self.prepare_identified(request, provider, None)
    }

    fn prepare_identified(
        self: &Arc<Self>,
        request: &ProviderRequest,
        provider: super::ProviderKind,
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
        provider: super::ProviderKind,
        input_message_id: Option<String>,
        identity: PreparationIdentity,
    ) -> Result<Arc<Submission>> {
        let provider = serde_json::to_string(&provider)?;
        let record = SubmissionRecord {
            input_message_id,
            id: uuid::Uuid::new_v4().to_string(),
            request: request.clone(),
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
                    "input identity changed provider"
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
                        "input identity was reused for different provider content"
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
            record: Mutex::new(record),
            acceptance: tokio::sync::watch::channel(false).0,
            steering_queued: tokio::sync::watch::channel(false).0,
            steering_active: AtomicBool::new(false),
            accepted_action: Mutex::new(None),
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
        provider: super::ProviderKind,
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

    /// Saves steering before it enters an active turn's queue.
    pub(crate) fn prepare_steering(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderKind,
    ) -> Result<Arc<Submission>> {
        self.prepare_input(catalog, input, provider, None)
    }

    pub(crate) fn prepare_input(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderKind,
        input_message_id: Option<String>,
    ) -> Result<Arc<Submission>> {
        self.prepare_record(
            &Self::input_request(catalog, input),
            provider,
            input_message_id,
            PreparationIdentity::EditableInput,
        )
    }

    pub(crate) fn prepare_task_input(
        self: &Arc<Self>,
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
        provider: super::ProviderKind,
        id: &crate::task::TaskInputId,
    ) -> Result<Arc<Submission>> {
        self.prepare_record(
            &Self::input_request(catalog, input),
            provider,
            Some(id.as_str().into()),
            PreparationIdentity::Task,
        )
    }

    fn input_request(
        catalog: &super::ProviderCatalogRequest,
        input: super::ProviderInput,
    ) -> ProviderRequest {
        ProviderRequest {
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
                "delivery": record.delivery, "included": record.included, "error": record.error,
            }),
        );
    }

    fn receipts(
        &self,
        request: &ProviderRequest,
        provider: super::ProviderKind,
    ) -> Result<std::collections::HashSet<String>> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let mut statement = connection.prepare("SELECT json_extract(payload,'$.delivery.receipt') FROM submission WHERE runtime=?1 AND provider=?2 AND json_extract(payload,'$.delivery.state')='accepted'")?;
        Ok(statement
            .query_map(
                params![request.runtime_key()?, serde_json::to_string(&provider)?],
                |row| row.get::<_, String>(0),
            )?
            .collect::<std::result::Result<_, _>>()?)
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
    record: Mutex<SubmissionRecord>,
    acceptance: tokio::sync::watch::Sender<bool>,
    steering_queued: tokio::sync::watch::Sender<bool>,
    steering_active: AtomicBool,
    accepted_action: Mutex<Option<AcceptedAction>>,
}

/// Keeps live steering distinct from a lost acknowledgment until its worker settles.
pub(crate) struct SteeringFlight(Arc<Submission>);

impl Drop for SteeringFlight {
    fn drop(&mut self) {
        self.0.steering_active.store(false, Ordering::Release);
    }
}

struct AcceptedAction(Arc<dyn Fn() -> Result<()> + Send + Sync>);

impl std::fmt::Debug for AcceptedAction {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("AcceptedAction")
    }
}

impl Submission {
    /// Own this live steering attempt without authorizing replay of uncertain input.
    pub(crate) fn begin_steering(self: &Arc<Self>) -> Result<SteeringFlight> {
        ensure!(
            self.record()?.delivery == SubmissionState::Prepared,
            "steering input was already dispatched"
        );
        ensure!(
            self.steering_active
                .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                .is_ok(),
            "steering input already has a worker"
        );
        self.steering_queued.send_replace(false);
        Ok(SteeringFlight(self.clone()))
    }

    /// Observe FIFO handoff independently from the provider's acknowledgment.
    pub(crate) fn steering_queued(&self) -> tokio::sync::watch::Receiver<bool> {
        self.steering_queued.subscribe()
    }

    /// Publish handoff only after the active turn's FIFO owns this input.
    pub(crate) fn mark_steering_queued(&self) {
        self.steering_queued.send_replace(true);
    }

    /// Runs committed-state activation before acceptance becomes visible to consumers.
    pub(crate) fn on_accepted(
        &self,
        action: impl Fn() -> Result<()> + Send + Sync + 'static,
    ) -> Result<()> {
        *self
            .accepted_action
            .lock()
            .map_err(|_| anyhow::anyhow!("submission acceptance action lock poisoned"))? =
            Some(AcceptedAction(Arc::new(action)));
        if matches!(self.record()?.delivery, SubmissionState::Accepted { .. }) {
            self.notify_accepted()?;
        }
        Ok(())
    }

    fn notify_accepted(&self) -> Result<()> {
        let action = self
            .accepted_action
            .lock()
            .map_err(|_| anyhow::anyhow!("submission acceptance action lock poisoned"))?
            .as_ref()
            .map(|action| action.0.clone());
        if let Some(action) = action {
            action()?;
        }
        self.acceptance.send_replace(true);
        Ok(())
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
            self.persist(|submission| submission.notify_accepted())
                .await?;
            return Ok(record);
        }
        if matches!(record.delivery, SubmissionState::Sending { .. }) {
            let request = record.request.clone();
            let provider_kind = provider.descriptor().kind;
            let known = self
                .store
                .persist(move |store| store.receipts(&request, provider_kind))
                .await?;
            if let Some(receipt) = provider.reconcile_submission(&record, &known).await? {
                self.persist(move |submission| submission.accepted(receipt))
                    .await?;
                return self.record();
            }
        }
        Ok(record)
    }
    /// Returns the stable host identity used for correlation, never a deduplication promise.
    pub fn id(&self) -> &str {
        &self.id
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
    pub fn sending(&self, target: SubmissionTarget) -> Result<()> {
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
    pub fn accepted(&self, receipt: String) -> Result<()> {
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
            self.notify_accepted()?;
            return Ok(());
        }
        ensure!(
            !matches!(record.delivery, SubmissionState::Cancelled),
            "cancelled submission cannot be acknowledged"
        );
        let target = match &record.delivery {
            SubmissionState::Sending { target } => Some(target.clone()),
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
                submission.notify_accepted()?;
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
                matches!(record.delivery, SubmissionState::Sending { .. }),
                "submission was not dispatched"
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
        self.update(|record| {
            if record.delivery == SubmissionState::Prepared {
                record.delivery = SubmissionState::Cancelled;
            }
            Ok(())
        })
    }

    fn update(&self, change: impl FnOnce(&mut SubmissionRecord) -> Result<()>) -> Result<()> {
        let mut connection = self
            .store
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("submission journal lock poisoned"))?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let mut record = self.record()?;
        let was_prepared = record.delivery == SubmissionState::Prepared;
        change(&mut record)?;
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
            let unresolved = sending.iter().any(|id| {
                !self.steering_active.load(Ordering::Acquire)
                    || !live
                        .get(id)
                        .and_then(Weak::upgrade)
                        .is_some_and(|other| other.steering_active.load(Ordering::Acquire))
            });
            ensure!(
                !unresolved,
                "an earlier submission has uncertain delivery; input remains saved"
            );
        }
        transaction.execute(
            "UPDATE submission SET payload=?2 WHERE id=?1",
            params![self.id, serde_json::to_string(&record)?],
        )?;
        transaction.commit()?;
        self.store.publish(&record)?;
        drop(connection);
        self.store.log(&record);
        Ok(())
    }

    /// Reconciles prior uncertain input, then attaches only definitely undelivered context.
    pub async fn recover(&self, provider: &Provider, request: &mut ProviderRequest) -> Result<()> {
        let provider_kind = provider.descriptor().kind;
        for pending in self.store.pending(request, provider_kind, Some(&self.id))? {
            if pending.id == self.id {
                continue;
            }
            if matches!(pending.delivery, SubmissionState::Sending { .. }) {
                let handle = self.store.submission(&pending.id)?;
                let known_receipts = self.store.receipts(request, provider_kind)?;
                if let Some(receipt) = provider
                    .reconcile_submission(&pending, &known_receipts)
                    .await?
                {
                    handle.accepted(receipt)?;
                    if request.provider_session_id.is_none() {
                        request.provider_session_id = match &pending.delivery {
                            SubmissionState::Sending { target } => {
                                target.conversation_id().map(str::to_owned)
                            }
                            _ => None,
                        };
                    }
                } else {
                    anyhow::bail!(
                        "Submission {} has uncertain delivery. Both prompts remain saved. Reconciliation could not confirm provider acceptance. Last error: {}",
                        pending.id,
                        pending
                            .error
                            .as_deref()
                            .unwrap_or("acknowledgment was not received")
                    );
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{
        PromptMode, ProviderCompletion, ProviderDescriptor, ProviderError, ProviderEventSink,
        ProviderInput, ProviderKind,
    };
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

    #[test]
    fn live_steering_can_overlap_but_lost_acknowledgments_block_new_dispatch() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let first = store.prepare(&request("first"), ProviderKind::Codex)?;
        let second = store.prepare(&request("second"), ProviderKind::Codex)?;
        let third = store.prepare(&request("third"), ProviderKind::Codex)?;
        let first_flight = first.begin_steering()?;
        assert!(first.begin_steering().is_err());
        let second_flight = second.begin_steering()?;
        let third_flight = third.begin_steering()?;
        let target = SubmissionTarget::native("codex", "thread", serde_json::json!({}));
        first.sending(target.clone())?;
        second.sending(target.clone())?;
        second.accepted("second accepted first".into())?;
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
        assert!(error.to_string().contains("uncertain delivery"));
        assert_eq!(third.record()?.delivery, SubmissionState::Prepared);
        first.accepted("late first acknowledgment".into())?;
        third.sending(target)?;
        drop(third_flight);
        Ok(())
    }

    #[test]
    fn pending_uses_unfinished_index_and_does_not_decode_completed_history() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = SubmissionStore::open(root.path(), Arc::new(TraceStore::open(root.path())?))?;
        let request = request("pending");
        let pending = store.prepare(&request, ProviderKind::Codex)?;
        let connection = store.connection.lock().unwrap();
        let runtime = request.runtime_key()?;
        let provider = serde_json::to_string(&ProviderKind::Codex)?;
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
        let result = store.pending(&request, ProviderKind::Codex, None)?;
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
        let submission = store.prepare(&request("message"), super::super::ProviderKind::Codex)?;
        let alias = second.submission(submission.id())?;
        assert!(Arc::ptr_eq(&submission, &alias));
        let mut acceptance = alias.acceptance();
        submission
            .persist(|submission| {
                submission.sending(SubmissionTarget::native(
                    "codex",
                    "thread",
                    serde_json::json!({}),
                ))
            })
            .await?;
        store.connection.lock().unwrap().execute_batch("CREATE TRIGGER fail_accept BEFORE UPDATE ON submission WHEN json_extract(NEW.payload,'$.delivery.state')='accepted' BEGIN SELECT RAISE(ABORT,'accept failed'); END;")?;
        assert!(
            submission
                .persist(|submission| submission.accepted("receipt".into()))
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
            .persist(|submission| submission.accepted("receipt".into()))
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

    struct ReceiptProvider {
        fail_before_send: AtomicBool,
        lose_ack: AtomicBool,
        history_accepts: AtomicBool,
        send_count: AtomicUsize,
        input: Mutex<Vec<ProviderRequest>>,
    }

    impl ReceiptProvider {
        fn new() -> Self {
            Self {
                fail_before_send: AtomicBool::new(false),
                lose_ack: AtomicBool::new(false),
                history_accepts: AtomicBool::new(false),
                send_count: AtomicUsize::new(0),
                input: Mutex::new(Vec::new()),
            }
        }
    }

    #[async_trait::async_trait]
    impl crate::provider::ProviderAdapter for ReceiptProvider {
        type Host = crate::provider::HarnessProviderHost;
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        fn descriptor(&self) -> ProviderDescriptor {
            ProviderDescriptor {
                kind: ProviderKind::Codex,
                label: "submission fixture".into(),
                capability: crate::provider::ProviderCapability {
                    integration: crate::provider::ProviderIntegration {
                        tracked_submission: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            }
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            Some("native-thread".into())
        }
        async fn reconcile_submission(
            &self,
            _: &SubmissionRecord,
            _: &std::collections::HashSet<String>,
        ) -> Result<Option<String>> {
            Ok(self
                .history_accepts
                .load(Ordering::SeqCst)
                .then(|| "history-receipt".into()))
        }
        async fn prompt_stream(
            &self,
            request: ProviderRequest,
            _: ProviderEventSink,
        ) -> Result<ProviderCompletion, ProviderError> {
            let output: anyhow::Result<ProviderCompletion> = async {
                if self.fail_before_send.swap(false, Ordering::SeqCst) {
                    anyhow::bail!("provider startup failed");
                }
                let submission = request.submission.as_ref().unwrap();
                submission.sending(SubmissionTarget::native(
                    "codex",
                    "native-thread",
                    serde_json::json!({}),
                ))?;
                self.send_count.fetch_add(1, Ordering::SeqCst);
                self.input.lock().unwrap().push(request.clone());
                if self.lose_ack.swap(false, Ordering::SeqCst) {
                    anyhow::bail!("connection lost after write");
                }
                submission.accepted("native-ack".into())?;
                Ok(ProviderCompletion::default())
            }
            .await;
            output.map_err(ProviderError::from)
        }
    }

    fn request(text: &str) -> ProviderRequest {
        ProviderRequest {
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

    #[tokio::test]
    async fn submission_restart_recovers_only_input_that_never_reached_dispatch() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let provider = ReceiptProvider::new();
        provider.fail_before_send.store(true, Ordering::SeqCst);
        let earlier = request("earlier instruction\nwith exact whitespace\n");
        assert!(
            super::super::retry::prompt(&provider, earlier.clone(), None, &trace, &store)
                .await
                .is_err()
        );
        drop(store);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        super::super::retry::prompt(
            &provider,
            request("new instruction replaces earlier scope"),
            None,
            &trace,
            &store,
        )
        .await?;
        let sent = provider.input.lock().unwrap();
        assert_eq!(sent.len(), 1);
        assert_eq!(sent[0].recovered_input, vec![earlier.input.clone()]);
        assert_eq!(
            sent[0].input.text(),
            "new instruction replaces earlier scope"
        );
        assert!(
            store
                .pending(&earlier, super::super::ProviderKind::Codex, None)?
                .is_empty()
        );
        Ok(())
    }

    #[tokio::test]
    async fn recovery_does_not_replay_dispatcher_owned_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let provider = ReceiptProvider::new();
        let owned = store.prepare_identified(
            &request("owned input"),
            super::super::ProviderKind::Codex,
            Some("message-id".into()),
        )?;
        let mut next = request("next input");
        let submission = store.prepare(&next, super::super::ProviderKind::Codex)?;
        submission.recover(&provider, &mut next).await?;
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
        let prepared = store.prepare(&request("not sent"), super::super::ProviderKind::Codex)?;
        prepared.cancel()?;
        assert_eq!(prepared.record()?.delivery, SubmissionState::Cancelled);
        let dispatched =
            store.prepare(&request("dispatched"), super::super::ProviderKind::Codex)?;
        dispatched.sending(SubmissionTarget::native(
            "codex",
            "thread",
            serde_json::json!({}),
        ))?;
        dispatched.cancel()?;
        assert!(matches!(
            dispatched.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        dispatched.accepted("receipt".into())?;
        dispatched.cancel()?;
        assert!(matches!(
            dispatched.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        Ok(())
    }

    #[test]
    fn task_entry_identity_reuses_durable_delivery_and_activates_before_notification() -> Result<()>
    {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let first = store.prepare_record(
            &request("entry"),
            super::super::ProviderKind::Codex,
            Some("entry-id".into()),
            PreparationIdentity::Task,
        )?;
        let id = first.id().to_owned();
        let accepted = first.acceptance();
        let activated = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let activation = activated.clone();
        first.on_accepted(move || {
            activation.store(true, Ordering::SeqCst);
            Ok(())
        })?;
        first.sending(SubmissionTarget::native(
            "codex",
            "thread",
            serde_json::json!({}),
        ))?;
        first.accepted("receipt".into())?;
        assert!(*accepted.borrow() && activated.load(Ordering::SeqCst));
        let duplicate = store.prepare_record(
            &request("entry"),
            super::super::ProviderKind::Codex,
            Some("entry-id".into()),
            PreparationIdentity::Task,
        )?;
        assert_eq!(duplicate.id(), id);
        assert!(
            store
                .prepare_record(
                    &request("changed"),
                    super::super::ProviderKind::Codex,
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
            super::super::ProviderKind::Codex,
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
    async fn task_entry_activation_failure_keeps_receipt_and_retries_before_notifying() -> Result<()>
    {
        let directory = tempfile::tempdir()?;
        let store = SubmissionStore::open(
            directory.path(),
            Arc::new(TraceStore::open(directory.path())?),
        )?;
        let submission = store.prepare(&request("entry"), super::super::ProviderKind::Codex)?;
        let accepted = submission.acceptance();
        let attempts = Arc::new(AtomicUsize::new(0));
        let activation = attempts.clone();
        submission.on_accepted(move || {
            if activation.fetch_add(1, Ordering::SeqCst) == 0 {
                anyhow::bail!("activation unavailable");
            }
            Ok(())
        })?;
        assert!(submission.accepted("receipt".into()).is_err());
        assert!(matches!(
            submission.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        assert!(!*accepted.borrow());
        submission.reconcile(&ReceiptProvider::new()).await?;
        assert!(*accepted.borrow());
        assert_eq!(attempts.load(Ordering::SeqCst), 2);
        Ok(())
    }

    #[tokio::test]
    async fn submission_lost_ack_blocks_replay_and_retains_every_new_prompt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let provider = ReceiptProvider::new();
        provider.lose_ack.store(true, Ordering::SeqCst);
        assert!(
            super::super::retry::prompt(&provider, request("first"), None, &trace, &store)
                .await
                .is_err()
        );
        for text in ["second", "third"] {
            let error = super::super::retry::prompt(&provider, request(text), None, &trace, &store)
                .await
                .unwrap_err();
            assert!(error.to_string().contains("uncertain delivery"));
            assert!(error.to_string().contains("connection lost after write"));
        }
        assert_eq!(provider.send_count.load(Ordering::SeqCst), 1);
        assert_eq!(
            store
                .pending(&request(""), super::super::ProviderKind::Codex, None)?
                .len(),
            3
        );
        provider.history_accepts.store(true, Ordering::SeqCst);
        super::super::retry::prompt(&provider, request("fourth"), None, &trace, &store).await?;
        let sent = provider.input.lock().unwrap();
        assert_eq!(sent.len(), 2);
        assert_eq!(
            sent[1].provider_session_id.as_deref(),
            Some("native-thread")
        );
        assert_eq!(
            sent[1].recovered_input,
            vec![
                ProviderInput::from_text("second"),
                ProviderInput::from_text("third")
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn submission_ack_commit_is_atomic_with_recovered_inputs() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let original = store.prepare(&request("first"), super::super::ProviderKind::Codex)?;
        let next = store.prepare(&request("second"), super::super::ProviderKind::Codex)?;
        let mut combined = request("second");
        next.recover(&ReceiptProvider::new(), &mut combined).await?;
        next.sending(SubmissionTarget::native(
            "codex",
            "thread",
            serde_json::json!({}),
        ))?;
        store.connection.lock().unwrap().execute_batch(&format!(
            "CREATE TRIGGER fail_ack BEFORE UPDATE ON submission WHEN NEW.id='{}' BEGIN SELECT RAISE(ABORT,'disk commit failure'); END;", original.id()))?;
        assert!(next.accepted("ack".into()).is_err());
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
        next.accepted("ack".into())?;
        next.accepted("duplicate ack".into())?;
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
        let earlier = store.prepare(&request("earlier"), ProviderKind::Codex)?;
        let current = store.prepare(&request("current"), ProviderKind::Codex)?;
        let later = store.prepare(&request("later"), ProviderKind::Codex)?;
        let mut input = request("current");
        current.recover(&ReceiptProvider::new(), &mut input).await?;
        assert_eq!(
            input.recovered_input,
            vec![ProviderInput::from_text("earlier")]
        );
        current.sending(SubmissionTarget::native(
            "codex",
            "thread",
            serde_json::json!({}),
        ))?;
        current.accepted("ack".into())?;
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
        let earlier = store.prepare(&original, ProviderKind::Codex)?;
        drop(earlier);
        drop(store);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let next = store.prepare(&request("latest"), ProviderKind::Codex)?;
        let mut input = request("latest");
        next.recover(&ReceiptProvider::new(), &mut input).await?;
        assert_eq!(input.recovered_input, vec![original.input]);
        let native =
            crate::provider::copilot::attachments::input("context", &input.input_attachments())?;
        let files = native.attachments;
        assert_eq!(std::fs::read(&files[0].path)?, bytes);
        Ok(())
    }

    #[tokio::test]
    async fn submission_acknowledged_stream_failure_is_not_missing_input() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace)?;
        let original = store.prepare(&request("accepted"), ProviderKind::Codex)?;
        original.sending(SubmissionTarget::native(
            "codex",
            "thread",
            serde_json::json!({}),
        ))?;
        original.accepted("native ack".into())?;
        original.failed("stream timed out".into())?;
        let next = store.prepare(&request("latest"), ProviderKind::Codex)?;
        let mut input = request("latest");
        next.recover(&ReceiptProvider::new(), &mut input).await?;
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
        let original = store.prepare(&request("cancel me"), super::super::ProviderKind::Codex)?;
        original.cancel()?;
        let next = store.prepare(&request("new"), super::super::ProviderKind::Codex)?;
        let mut new = request("new");
        next.recover(&ReceiptProvider::new(), &mut new).await?;
        assert!(new.recovered_input.is_empty());
        next.sending(SubmissionTarget::Untracked)?;
        next.cancel()?;
        assert!(matches!(
            next.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        let following = store.prepare(&request("following"), super::super::ProviderKind::Codex)?;
        assert!(
            following
                .recover(&ReceiptProvider::new(), &mut request("following"))
                .await
                .is_err()
        );
        Ok(())
    }
}

#[async_trait::async_trait]
impl forge_provider_api::submission::InputReceipt for Submission {
    fn id(&self) -> &str {
        self.id()
    }

    async fn record_sending(
        self: Arc<Self>,
        target: SubmissionTarget,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.sending(target))
            .await
            .map_err(Into::into)
    }

    async fn record_accepted(
        self: Arc<Self>,
        receipt: String,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.accepted(receipt))
            .await
            .map_err(Into::into)
    }

    async fn record_rejected(
        self: Arc<Self>,
        message: String,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.rejected(message))
            .await
            .map_err(Into::into)
    }

    async fn record_failure(
        self: Arc<Self>,
        message: String,
    ) -> Result<(), forge_provider_api::ProviderError> {
        self.persist(move |submission| submission.failed(message))
            .await
            .map_err(Into::into)
    }
}
