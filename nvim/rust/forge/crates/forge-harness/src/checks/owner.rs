//! Deliver scoped check requests to the exchange owner without using the caller-command FIFO.

use super::{CheckSupervisor, PreparedCheckRun, RunRequest};
use crate::{broker::publication::SessionUpdateSink, provider::InvocationScope};
use forge_provider_api::control::ToolCallId;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, sync::Arc};
use tokio::sync::{mpsc, oneshot};

const CAPACITY: usize = forge_protocol::MAX_PENDING_FRAMES;

/// Report normalized admission, ownership, or worker failures to the requesting tool.
#[derive(Debug)]
pub(crate) enum CheckOwnerFailure {
    Unavailable,
    StaleScope,
    ConflictingRequest,
    Worker(anyhow::Error),
}

impl std::fmt::Display for CheckOwnerFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unavailable => formatter.write_str("check owner is unavailable"),
            Self::StaleScope => formatter.write_str("check request no longer owns its invocation"),
            Self::ConflictingRequest => {
                formatter.write_str("tool-call identity was reused with different check input")
            }
            Self::Worker(error) => write!(formatter, "check operation failed: {error:#}"),
        }
    }
}
impl std::error::Error for CheckOwnerFailure {}

/// Carry only host-assigned invocation authority and normalized prepared check work.
pub(crate) struct ToolCheckRequest {
    pub(crate) scope: InvocationScope,
    pub(crate) authority: Option<crate::tools::TaskAuthority>,
    pub(crate) call_id: ToolCallId,
    pub(crate) request: RunRequest,
    pub(crate) run: PreparedCheckRun,
    pub(crate) sink: Option<SessionUpdateSink>,
    pub(crate) cancelled: tokio::sync::watch::Receiver<bool>,
    pub(crate) launch_hold: crate::agent::AgentLaunchHold,
    pub(crate) reply: oneshot::Sender<Result<String, CheckOwnerFailure>>,
}

/// Carry authenticated discovery work without creating a check-history run.
pub(crate) struct ToolDiscoveryRequest {
    pub(crate) scope: InvocationScope,
    pub(crate) authority: Option<crate::tools::TaskAuthority>,
    pub(crate) request: super::adapter::CheckDiscoverRequest,
    pub(crate) inputs: super::discovery::DiscoveryInputs,
    pub(crate) deadline: tokio::time::Instant,
    pub(crate) quiescence: Option<crate::agent::AgentQuiescence>,
    pub(crate) cancelled: tokio::sync::watch::Receiver<bool>,
    pub(crate) reply: oneshot::Sender<Result<super::adapter::Discovery, CheckOwnerFailure>>,
}

/// Queue check execution and discovery through the same invocation admission boundary.
pub(crate) enum PendingCheckTool {
    Run(Box<ToolCheckRequest>),
    Discover(Box<ToolDiscoveryRequest>),
}

impl PendingCheckTool {
    /// Get the host-assigned invocation that must still own this request.
    pub(crate) fn scope(&self) -> &InvocationScope {
        match self {
            Self::Run(request) => &request.scope,
            Self::Discover(request) => &request.scope,
        }
    }

    /// Reject pending work without leaving its requesting tool waiting for a response.
    pub(crate) fn reject(self, error: CheckOwnerFailure) {
        match self {
            Self::Run(request) => {
                let _ = request.reply.send(Err(error));
            }
            Self::Discover(request) => {
                let _ = request.reply.send(Err(error));
            }
        }
    }
}

/// Retain an automatic completion response independently of the transport request's lifetime.
pub(crate) struct AutomaticCheckRequest {
    pub(crate) reply: oneshot::Sender<Result<CheckCompletionOutcome, CheckOwnerFailure>>,
}

/// Preserve the existing completion projection while the owner retains its workflow transaction.
pub(crate) struct CheckCompletionOutcome {
    pub(crate) value: serde_json::Value,
    pub(crate) events: Vec<forge_protocol::message::SessionEvent>,
}

/// Distinguish scoped tool work from owner-controlled automatic workflow requests.
pub(crate) enum CheckOwnerEvent {
    StartTool(Box<ToolCheckRequest>),
    Discover(Box<ToolDiscoveryRequest>),
    CancelTool {
        scope: InvocationScope,
        run_id: String,
        reply: oneshot::Sender<Result<(), CheckOwnerFailure>>,
    },
    CompleteAutomatic(AutomaticCheckRequest),
}

/// Submit internal work with bounded backpressure without retaining the receiving owner.
#[derive(Clone)]
pub(crate) struct CheckOwnerSender {
    sender: mpsc::Sender<CheckOwnerEvent>,
}

impl CheckOwnerSender {
    /// Create one session's bounded internal check event route.
    pub(crate) fn channel() -> (Self, mpsc::Receiver<CheckOwnerEvent>) {
        let (sender, receiver) = mpsc::channel(CAPACITY);
        (Self { sender }, receiver)
    }

    /// Await run admission only; execution and collection remain with the owner after this returns.
    #[expect(
        clippy::too_many_arguments,
        reason = "The owner message keeps invocation authority and prepared work explicit."
    )]
    pub(crate) async fn start(
        &self,
        scope: InvocationScope,
        authority: Option<crate::tools::TaskAuthority>,
        call_id: ToolCallId,
        request: RunRequest,
        run: PreparedCheckRun,
        sink: Option<SessionUpdateSink>,
        cancelled: tokio::sync::watch::Receiver<bool>,
        launch_hold: crate::agent::AgentLaunchHold,
    ) -> Result<String, CheckOwnerFailure> {
        let (reply, receiver) = oneshot::channel();
        self.send(CheckOwnerEvent::StartTool(Box::new(ToolCheckRequest {
            scope,
            authority,
            call_id,
            request,
            run,
            sink,
            cancelled,
            launch_hold,
            reply,
        })))
        .await?;
        receiver.await.map_err(|_| CheckOwnerFailure::Unavailable)?
    }

    /// Request cancellation only for work owned by the authenticated invocation.
    pub(crate) async fn cancel(
        &self,
        scope: InvocationScope,
        run_id: String,
    ) -> Result<(), CheckOwnerFailure> {
        let (reply, receiver) = oneshot::channel();
        self.send(CheckOwnerEvent::CancelTool {
            scope,
            run_id,
            reply,
        })
        .await?;
        receiver.await.map_err(|_| CheckOwnerFailure::Unavailable)?
    }

    /// Await discovery while the session owner retains its worker and process lifetime.
    #[allow(clippy::too_many_arguments)]
    pub(crate) async fn discover(
        &self,
        scope: InvocationScope,
        authority: Option<crate::tools::TaskAuthority>,
        request: super::adapter::CheckDiscoverRequest,
        inputs: super::discovery::DiscoveryInputs,
        deadline: tokio::time::Instant,
        cancelled: tokio::sync::watch::Receiver<bool>,
        quiescence: Option<crate::agent::AgentQuiescence>,
    ) -> Result<super::adapter::Discovery, CheckOwnerFailure> {
        let (reply, receiver) = oneshot::channel();
        self.send(CheckOwnerEvent::Discover(Box::new(ToolDiscoveryRequest {
            scope,
            authority,
            request,
            inputs,
            deadline,
            quiescence,
            cancelled,
            reply,
        })))
        .await?;
        receiver.await.map_err(|_| CheckOwnerFailure::Unavailable)?
    }

    /// Ask the owner to drive automatic completion checks and any resulting repair work.
    pub(crate) async fn complete(&self) -> Result<CheckCompletionOutcome, CheckOwnerFailure> {
        let (reply, receiver) = oneshot::channel();
        self.send(CheckOwnerEvent::CompleteAutomatic(AutomaticCheckRequest {
            reply,
        }))
        .await?;
        receiver.await.map_err(|_| CheckOwnerFailure::Unavailable)?
    }

    async fn send(&self, event: CheckOwnerEvent) -> Result<(), CheckOwnerFailure> {
        self.sender
            .send(event)
            .await
            .map_err(|_| CheckOwnerFailure::Unavailable)
    }
}

/// Preserve native admission evidence across a host restart without replaying a reserved operation.
#[derive(Deserialize, Serialize)]
struct CheckToolReceipt {
    scope: InvocationScope,
    call_id: ToolCallId,
    request: RunRequest,
    run_id: String,
    state: CheckToolReceiptState,
}

impl CheckToolReceipt {
    /// Replay admitted identity only when original authenticated scope and normalized intent match.
    fn replay(
        &self,
        scope: &InvocationScope,
        call_id: &ToolCallId,
        request: &RunRequest,
    ) -> Result<String, CheckOwnerFailure> {
        if &self.scope != scope
            || &self.call_id != call_id
            || crate::codec::to_vec(&self.request)
                .map_err(|error| CheckOwnerFailure::Worker(error.into()))?
                != crate::codec::to_vec(request)
                    .map_err(|error| CheckOwnerFailure::Worker(error.into()))?
        {
            return Err(CheckOwnerFailure::ConflictingRequest);
        }
        match self.state {
            CheckToolReceiptState::Admitted => Ok(self.run_id.clone()),
            CheckToolReceiptState::Reserved | CheckToolReceiptState::Rejected => {
                Err(CheckOwnerFailure::Unavailable)
            }
        }
    }
}

/// A reserved start can be uncertain; only admitted starts expose a retained run identity.
#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum CheckToolReceiptState {
    Reserved,
    Admitted,
    Rejected,
}

/// Retain process collection independently of a dropped tool response receiver.
pub(crate) struct CheckOwnerRuns {
    runner: Arc<CheckSupervisor>,
    scopes: HashMap<String, InvocationScope>,
    admitted: HashMap<(InvocationScope, ToolCallId), (Vec<u8>, String)>,
    workers: tokio::task::JoinSet<CheckOwnerCompletion>,
    discoveries: HashMap<String, tokio::sync::watch::Sender<bool>>,
}

/// Report actual process collection with its original invocation identity.
pub(crate) struct CheckOwnerCompletion {
    pub(crate) scope: InvocationScope,
    pub(crate) run_id: String,
    pub(crate) outcome: Result<(), CheckOwnerFailure>,
}

impl CheckOwnerRuns {
    /// Recover admitted tool runs; automatic and plan checks have no tool receipt.
    pub(crate) fn result_sources(&self) -> anyhow::Result<Vec<(String, String)>> {
        let directory = self.runner.root.join("owner-receipts");
        if !directory.try_exists()? {
            return Ok(Vec::new());
        }
        let mut sources = Vec::new();
        for entry in std::fs::read_dir(directory)? {
            let path = entry?.path();
            if path.extension().is_none_or(|extension| extension != "json") {
                continue;
            }
            let receipt: CheckToolReceipt = crate::codec::from_slice(&std::fs::read(path)?)?;
            if matches!(receipt.state, CheckToolReceiptState::Admitted) {
                sources.push((
                    receipt.run_id,
                    receipt.scope.exchange_id.as_str().to_owned(),
                ));
            }
        }
        Ok(sources)
    }

    /// Require observed worker collection before publishing terminal run evidence.
    pub(crate) fn is_collected(&self, run_id: &str) -> bool {
        !self.scopes.contains_key(run_id)
    }

    /// Create process ownership for one session's check supervisor.
    pub(crate) fn new(runner: Arc<CheckSupervisor>) -> Self {
        Self {
            runner,
            scopes: HashMap::new(),
            admitted: HashMap::new(),
            workers: tokio::task::JoinSet::new(),
            discoveries: HashMap::new(),
        }
    }

    /// Launch only after the owner has authenticated scope and revalidated its current resource gate.
    pub(crate) fn start(&mut self, request: PendingCheckTool) -> Result<(), CheckOwnerFailure> {
        match request {
            PendingCheckTool::Run(request) => self.run_start(*request),
            PendingCheckTool::Discover(request) => self.discovery_start(*request),
        }
    }

    /// Admit discovery synchronously, then retain its worker until process collection completes.
    fn discovery_start(&mut self, request: ToolDiscoveryRequest) -> Result<(), CheckOwnerFailure> {
        let ToolDiscoveryRequest {
            scope,
            authority,
            request,
            inputs,
            deadline,
            quiescence,
            mut cancelled,
            mut reply,
        } = request;
        if *cancelled.borrow() || reply.is_closed() {
            let _ = reply.send(Err(CheckOwnerFailure::StaleScope));
            return Ok(());
        }
        if request.mode == super::adapter::DiscoverMode::Exact
            && let Some(authority) = authority
            && let Err(error) = authority
                .validate_scope()
                .and_then(|()| authority.admit_mutation())
        {
            let _ = reply.send(Err(CheckOwnerFailure::Worker(error)));
            return Ok(());
        }
        let id = uuid::Uuid::new_v4().to_string();
        let reservation = match self.runner.discovery_reserve(&id, request.mode) {
            Ok(reservation) => reservation,
            Err(error) => {
                let _ = reply.send(Err(CheckOwnerFailure::Worker(error)));
                return Ok(());
            }
        };
        let (stop, stopped) = tokio::sync::watch::channel(false);
        self.discoveries.insert(id.clone(), stop.clone());
        self.scopes.insert(id.clone(), scope.clone());
        let runner = Arc::clone(&self.runner);
        self.workers.spawn(async move {
            let operation = super::discovery::discover(
                &runner,
                inputs,
                request,
                deadline,
                stopped,
                reservation.process_owner(),
            );
            tokio::pin!(operation);
            let result = tokio::select! {
                biased;
                _ = reply.closed() => {
                    let _ = stop.send(true);
                    operation.await
                }
                _ = async { let _ = cancelled.wait_for(|value| *value).await; } => {
                    let _ = stop.send(true);
                    operation.await
                }
                result = &mut operation => result,
            };
            let succeeded = result.is_ok();
            let _ = reply.send(result.map_err(CheckOwnerFailure::Worker));
            drop(reservation);
            drop(quiescence);
            CheckOwnerCompletion {
                scope,
                run_id: id,
                outcome: if succeeded {
                    Ok(())
                } else {
                    Err(CheckOwnerFailure::Unavailable)
                },
            }
        });
        Ok(())
    }

    /// Launch an authenticated run and retain replay evidence until its scope retires.
    fn run_start(&mut self, request: ToolCheckRequest) -> Result<(), CheckOwnerFailure> {
        let ToolCheckRequest {
            scope,
            authority,
            call_id,
            request,
            run,
            sink,
            cancelled,
            reply,
            launch_hold,
        } = request;
        let key = (scope.clone(), call_id.clone());
        let fingerprint = crate::codec::to_vec(&request)
            .map_err(|error| CheckOwnerFailure::Worker(error.into()))?;
        if let Some((previous, run_id)) = self.admitted.get(&key) {
            let result = if previous == &fingerprint {
                Ok(run_id.clone())
            } else {
                Err(CheckOwnerFailure::ConflictingRequest)
            };
            let _ = reply.send(result);
            return Ok(());
        }
        let receipt_path = self.receipt_path(&scope, &call_id)?;
        if receipt_path.exists() {
            let receipt: CheckToolReceipt = crate::codec::from_slice(
                &std::fs::read(&receipt_path)
                    .map_err(|error| CheckOwnerFailure::Worker(error.into()))?,
            )
            .map_err(|error| CheckOwnerFailure::Worker(error.into()))?;
            let result = receipt.replay(&scope, &call_id, &request);
            let _ = reply.send(result);
            return Ok(());
        }
        if let Some(authority) = authority
            && let Err(error) = authority
                .validate_scope()
                .and_then(|()| authority.admit_mutation())
        {
            let _ = reply.send(Err(CheckOwnerFailure::Worker(error)));
            return Ok(());
        }
        if self.admitted.len() >= CAPACITY {
            let _ = reply.send(Err(CheckOwnerFailure::Unavailable));
            return Ok(());
        }
        if *cancelled.borrow() {
            let _ = reply.send(Err(CheckOwnerFailure::StaleScope));
            return Ok(());
        }
        let mut receipt = CheckToolReceipt {
            scope: scope.clone(),
            call_id,
            request,
            run_id: run.id.clone(),
            state: CheckToolReceiptState::Reserved,
        };
        write_receipt(&receipt_path, &receipt).map_err(CheckOwnerFailure::Worker)?;
        let (run_id, receiver) = match self.runner.launch_owned(run, sink, Some(cancelled)) {
            Ok(owned) => owned,
            Err(error) => {
                receipt.state = CheckToolReceiptState::Rejected;
                write_receipt(&receipt_path, &receipt).map_err(CheckOwnerFailure::Worker)?;
                let _ = reply.send(Err(CheckOwnerFailure::Worker(error)));
                return Ok(());
            }
        };
        receipt.state = CheckToolReceiptState::Admitted;
        let receipt_commit = write_receipt(&receipt_path, &receipt);
        self.admitted.insert(key, (fingerprint, run_id.clone()));
        self.scopes.insert(run_id.clone(), scope.clone());
        let owned_id = run_id.clone();
        self.workers.spawn(async move {
            let outcome = receiver
                .await
                .map_err(|_| CheckOwnerFailure::Unavailable)
                .and_then(|result| {
                    result
                        .map(|_| ())
                        .map_err(|error| CheckOwnerFailure::Worker(anyhow::anyhow!(error)))
                });
            drop(launch_hold);
            CheckOwnerCompletion {
                scope,
                run_id: owned_id,
                outcome,
            }
        });
        let _ = reply.send(
            receipt_commit
                .map(|()| run_id)
                .map_err(CheckOwnerFailure::Worker),
        );
        Ok(())
    }

    /// Resolve a receipt path from authenticated scope and tool identity, never caller-selected paths.
    fn receipt_path(
        &self,
        scope: &InvocationScope,
        call_id: &ToolCallId,
    ) -> Result<std::path::PathBuf, CheckOwnerFailure> {
        let identity = crate::codec::to_vec(&(scope, call_id))
            .map_err(|error| CheckOwnerFailure::Worker(error.into()))?;
        Ok(self
            .runner
            .root
            .join("owner-receipts")
            .join(format!("{}.json", crate::plan::digest(&identity))))
    }

    /// Cancel a run only when its invocation matches the retained owner identity.
    pub(crate) fn cancel(
        &self,
        scope: &InvocationScope,
        run_id: &str,
    ) -> Result<(), CheckOwnerFailure> {
        if self.scopes.get(run_id) != Some(scope) {
            return Err(CheckOwnerFailure::StaleScope);
        }
        self.runner
            .cancel(run_id)
            .map_err(CheckOwnerFailure::Worker)
    }

    /// Interrupt every retained run during owner shutdown without discarding collection futures.
    pub(crate) fn cancel_all(&self) -> Result<(), CheckOwnerFailure> {
        for run_id in self.scopes.keys() {
            if let Some(stop) = self.discoveries.get(run_id) {
                let _ = stop.send(true);
                continue;
            }
            self.runner
                .cancel_for_collection(run_id)
                .map_err(CheckOwnerFailure::Worker)?;
        }
        Ok(())
    }

    /// Release replay receipts only after the owner retires the invocation and its workers are collected.
    pub(crate) fn retire_scope(
        &mut self,
        scope: &InvocationScope,
    ) -> Result<(), CheckOwnerFailure> {
        if self.scopes.values().any(|active| active == scope) {
            for (id, active) in &self.scopes {
                if active == scope
                    && let Some(stop) = self.discoveries.get(id)
                {
                    let _ = stop.send(true);
                }
            }
            return Err(CheckOwnerFailure::Unavailable);
        }
        self.admitted.retain(|(owner, _), _| owner != scope);
        Ok(())
    }

    /// Report whether actual worker collection is still required before replacement work.
    pub(crate) fn is_empty(&self) -> bool {
        self.workers.is_empty()
    }

    /// Await one worker completion without retaining a broker lock or a caller-command slot.
    pub(crate) async fn next(
        &mut self,
    ) -> Option<Result<CheckOwnerCompletion, tokio::task::JoinError>> {
        self.workers.join_next().await
    }

    /// Release invocation ownership only after the owner processes actual collection.
    pub(crate) fn settle(&mut self, completion: &CheckOwnerCompletion) {
        self.discoveries.remove(&completion.run_id);
        if self.scopes.get(&completion.run_id) == Some(&completion.scope) {
            self.scopes.remove(&completion.run_id);
        }
    }
}

/// Commit admission evidence before acknowledging a start or allowing it to be replayed.
fn write_receipt(path: &std::path::Path, receipt: &CheckToolReceipt) -> anyhow::Result<()> {
    use std::io::Write;
    let directory = path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("check receipt has no parent"))?;
    std::fs::create_dir_all(directory)?;
    let temporary = directory.join(format!(".receipt-{}.tmp", uuid::Uuid::new_v4()));
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)?;
    file.write_all(&crate::codec::to_vec(receipt)?)?;
    file.sync_all()?;
    drop(file);
    let result = std::fs::rename(&temporary, path);
    if result.is_err() {
        let _ = std::fs::remove_file(&temporary);
    }
    result?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A saved terminal result cannot become a notification before its worker is collected.
    #[test]
    fn result_sources_wait_for_owner_collection() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let broker = crate::broker::HarnessBroker::initialize(serde_json::from_value(
            serde_json::json!({
                "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"collection",
                "provider":{"kind":"mock","command":[]}
            }),
        )?)?;
        let mut owner = CheckOwnerRuns::new(broker.checks());
        let scope = InvocationScope::new(crate::provider::ThreadOwner::root("session"), "exchange");
        let call = ToolCallId::new("call")?;
        write_receipt(
            &owner.receipt_path(&scope, &call)?,
            &CheckToolReceipt {
                scope: scope.clone(),
                call_id: call,
                request: RunRequest::default(),
                run_id: "run".into(),
                state: CheckToolReceiptState::Admitted,
            },
        )?;
        owner.scopes.insert("run".into(), scope.clone());
        assert_eq!(
            owner.result_sources()?,
            vec![("run".into(), "exchange".into())]
        );
        assert!(!owner.is_collected("run"));
        owner.settle(&CheckOwnerCompletion {
            scope,
            run_id: "run".into(),
            outcome: Ok(()),
        });
        assert!(owner.is_collected("run"));
        assert_eq!(
            owner.result_sources()?,
            vec![("run".into(), "exchange".into())]
        );
        Ok(())
    }

    /// Reopening a receipt replays admitted work, rejects conflicting intent, and never replays uncertainty.
    #[test]
    fn durable_tool_receipt_retains_admission_and_uncertain_start() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let path = root.path().join("receipt.json");
        let scope = InvocationScope::new(
            crate::provider::ThreadOwner::root("checks-test"),
            "exchange",
        );
        let call_id = ToolCallId::new("call-1")?;
        let request = RunRequest::default();
        let mut receipt = CheckToolReceipt {
            scope: scope.clone(),
            call_id: call_id.clone(),
            request: request.clone(),
            run_id: "original-run".into(),
            state: CheckToolReceiptState::Reserved,
        };
        write_receipt(&path, &receipt)?;
        let reopened: CheckToolReceipt = crate::codec::from_slice(&std::fs::read(&path)?)?;
        assert!(matches!(
            reopened.replay(&scope, &call_id, &request),
            Err(CheckOwnerFailure::Unavailable)
        ));
        receipt.state = CheckToolReceiptState::Admitted;
        write_receipt(&path, &receipt)?;
        let reopened: CheckToolReceipt = crate::codec::from_slice(&std::fs::read(&path)?)?;
        assert_eq!(reopened.replay(&scope, &call_id, &request)?, "original-run");
        let changed = RunRequest::Selected(super::super::SelectedChecks {
            ids: vec!["different-check".into()],
            ..Default::default()
        });
        assert!(matches!(
            reopened.replay(&scope, &call_id, &changed),
            Err(CheckOwnerFailure::ConflictingRequest)
        ));
        Ok(())
    }
}
