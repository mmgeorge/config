use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Result, bail, ensure};
use tokio::sync::oneshot;
use tokio::sync::{OwnedSemaphorePermit, Semaphore};

use crate::model::GithubRepositoryId;

pub(crate) const MAX_REMOTE_OPERATIONS: usize = 64;
const MAX_RESOURCE: usize = 64;
const MAX_INPUT_BYTES: usize = 16 * 1024 * 1024;

/// Remote issue or PR identity. Both use GitHub's shared repository number namespace.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RemoteResource {
    repository: GithubRepositoryId,
    number: u64,
}

impl RemoteResource {
    pub fn notification(repository: GithubRepositoryId, number: u64) -> Result<Self> {
        ensure!(number > 0, "invalid notification thread identity");
        Ok(Self { repository, number })
    }

    pub fn repository(repository: GithubRepositoryId) -> Self {
        Self {
            repository,
            number: 0,
        }
    }
    /// Rejects numbers outside GitHub's positive GraphQL integer range.
    pub fn new(repository: GithubRepositoryId, number: u64) -> Result<Self> {
        ensure!(
            number > 0 && number <= i32::MAX as u64,
            "invalid GitHub resource number"
        );
        Ok(Self { repository, number })
    }
}

/// Evidence scope within one FIFO resource. Creation keys identify one submitted operation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RemoteScope {
    PullRequest,
    Comment(Box<str>),
    Creation(Box<str>),
}

impl RemoteScope {
    fn retained_bytes(&self) -> Result<usize> {
        match self {
            Self::PullRequest => Ok(0),
            Self::Comment(identity) | Self::Creation(identity) => {
                ensure!(
                    !identity.is_empty()
                        && identity.len() <= 256
                        && identity.bytes().all(|byte| byte.is_ascii_graphic()),
                    "invalid GitHub reconciliation scope"
                );
                Ok(identity.len())
            }
        }
    }
}

/// Admission intent. Reconciliation reads remote truth and never resubmits a write.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RemoteIntent {
    Mutation,
    Reconcile,
}

/// Completion evidence supplied while retaining exclusive resource ownership.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RemoteCompletion {
    Confirmed,
    Unchanged,
    Uncertain,
    Reconciled,
}

/// Process-local FIFO ownership for at most 64 operations, 64 remote resources, and 16 MiB of input.
///
/// Clones share admission and uncertainty state. Uncertain resources retain a slot until a
/// successful reconciliation. This queue does not exclude other processes or external writers.
#[derive(Clone)]
pub struct RemoteQueue {
    state: Arc<Mutex<QueueState>>,
    input: Arc<Semaphore>,
}

impl Default for RemoteQueue {
    fn default() -> Self {
        Self {
            state: Arc::default(),
            input: Arc::new(Semaphore::new(MAX_INPUT_BYTES)),
        }
    }
}

#[derive(Default)]
struct QueueState {
    resource: HashMap<RemoteResource, ResourceState>,
    admitted: usize,
    next: u64,
    closed: bool,
}

#[derive(Default)]
struct ResourceState {
    active: Option<u64>,
    pending: VecDeque<PendingOperation>,
    uncertain: Option<RemoteScope>,
}

struct PendingOperation {
    id: u64,
    intent: RemoteIntent,
    sender: oneshot::Sender<Result<()>>,
}

/// Retains admission until completion or drop, including while waiting for its FIFO turn.
///
/// Dropping before a write removes admission. Dropping after begin_mutation marks the resource
/// uncertain. The service must retain this owner independently of a request receiver.
pub struct RemoteOperation {
    pub(crate) mutation_started: Arc<AtomicBool>,
    _input: OwnedSemaphorePermit,
    queue: RemoteQueue,
    resource: RemoteResource,
    scope: RemoteScope,
    id: u64,
    intent: RemoteIntent,
    receiver: Option<oneshot::Receiver<Result<()>>>,
    ready: bool,
    completion: RemoteCompletion,
}

impl RemoteQueue {
    /// Admits in call order or rejects closed, full, or unresolved mutation admission.
    ///
    /// No remote work begins here. The returned owner must await ready before accessing the
    /// resource. Reconciliation remains admissible for an uncertain resource at capacity.
    /// retained_bytes charges owned request allocation through completion or cancellation.
    pub fn enqueue(
        &self,
        resource: RemoteResource,
        intent: RemoteIntent,
        retained_bytes: usize,
    ) -> Result<RemoteOperation> {
        self.enqueue_scoped(resource, RemoteScope::PullRequest, intent, retained_bytes)
    }

    /// Serializes all scopes on one resource while restricting which observation clears uncertainty.
    /// Scope text is bounded to 256 ASCII bytes and charged alongside retained request input.
    pub fn enqueue_scoped(
        &self,
        resource: RemoteResource,
        scope: RemoteScope,
        intent: RemoteIntent,
        retained_bytes: usize,
    ) -> Result<RemoteOperation> {
        let retained_bytes = retained_bytes
            .checked_add(scope.retained_bytes()?)
            .ok_or_else(|| anyhow::anyhow!("GitHub mutation input size overflow"))?;
        ensure!(
            retained_bytes <= MAX_INPUT_BYTES,
            "GitHub mutation input exceeds its byte limit"
        );
        let input = Arc::clone(&self.input)
            .try_acquire_many_owned(retained_bytes as u32)
            .map_err(|_| anyhow::anyhow!("GitHub mutation input admission is full"))?;
        let mut state = self.state.lock().expect("GitHub remote queue poisoned");
        ensure!(!state.closed, "GitHub mutation admission is closed");
        ensure!(
            state.admitted < MAX_REMOTE_OPERATIONS,
            "GitHub mutation admission is full"
        );
        if let Some(entry) = state.resource.get(&resource) {
            ensure!(
                entry.uncertain.is_none() || intent == RemoteIntent::Reconcile,
                "OutcomeUnknown: reconcile the remote resource before another mutation"
            );
        } else {
            ensure!(
                state.resource.len() < MAX_RESOURCE,
                "GitHub remote resource admission is full"
            );
        }
        let id = state
            .next
            .checked_add(1)
            .ok_or_else(|| anyhow::anyhow!("GitHub operation identity exhausted"))?;
        state.next = id;
        state.admitted += 1;
        let entry = state.resource.entry(resource.clone()).or_default();
        let (sender, receiver) = oneshot::channel();
        if entry.active.is_none() {
            entry.active = Some(id);
            let _ = sender.send(Ok(()));
        } else {
            entry
                .pending
                .push_back(PendingOperation { id, intent, sender });
        }
        Ok(RemoteOperation {
            mutation_started: Arc::new(AtomicBool::new(false)),
            _input: input,
            queue: self.clone(),
            resource,
            scope,
            id,
            intent,
            receiver: Some(receiver),
            ready: false,
            completion: RemoteCompletion::Unchanged,
        })
    }

    /// Rejects new operations and queued work without releasing an active resource owner.
    pub fn close(&self) {
        let mut state = self.state.lock().expect("GitHub remote queue poisoned");
        state.closed = true;
        let mut removed = 0;
        for entry in state.resource.values_mut() {
            for pending in entry.pending.drain(..) {
                removed += 1;
                let _ = pending
                    .sender
                    .send(Err(anyhow::anyhow!("GitHub mutation admission is closed")));
            }
        }
        state.admitted -= removed;
    }

    fn complete(
        &self,
        resource: &RemoteResource,
        scope: &RemoteScope,
        id: u64,
        completion: RemoteCompletion,
    ) {
        let mut state = self.state.lock().expect("GitHub remote queue poisoned");
        let Some(entry) = state.resource.get_mut(resource) else {
            return;
        };
        if entry.active != Some(id) {
            if let Some(position) = entry.pending.iter().position(|pending| pending.id == id) {
                entry.pending.remove(position);
                state.admitted -= 1;
            }
            return;
        }
        entry.active = None;
        match completion {
            RemoteCompletion::Uncertain => entry.uncertain = Some(scope.clone()),
            RemoteCompletion::Reconciled if entry.uncertain.as_ref() == Some(scope) => {
                entry.uncertain = None
            }
            RemoteCompletion::Reconciled => {}
            RemoteCompletion::Confirmed | RemoteCompletion::Unchanged => {}
        }
        let mut removed = 1;
        while let Some(pending) = entry.pending.pop_front() {
            if entry.uncertain.is_some() && pending.intent == RemoteIntent::Mutation {
                removed += 1;
                let _ = pending.sender.send(Err(anyhow::anyhow!(
                    "OutcomeUnknown: preceding mutation requires reconciliation"
                )));
            } else if pending.sender.send(Ok(())).is_ok() {
                entry.active = Some(pending.id);
                break;
            } else {
                removed += 1;
            }
        }
        if entry.active.is_none() && entry.uncertain.is_none() {
            state.resource.remove(resource);
        }
        state.admitted -= removed;
    }
}

impl RemoteOperation {
    /// Waits for exclusive resource ownership in admission order.
    pub async fn ready(mut self) -> Result<Self> {
        self.receiver
            .take()
            .expect("GitHub queue receiver missing")
            .await
            .map_err(|_| anyhow::anyhow!("GitHub mutation queue ended without admission"))??;
        self.ready = true;
        Ok(self)
    }

    /// Records possible remote effects before invoking a mutation implementation.
    pub fn begin_mutation(&mut self) -> Result<()> {
        ensure!(
            self.ready && self.intent == RemoteIntent::Mutation,
            "GitHub mutation has no active write admission"
        );
        self.mutation_started.store(true, Ordering::Release);
        self.completion = RemoteCompletion::Uncertain;
        Ok(())
    }

    /// Releases ownership using observed evidence, rejecting reconciliation from a write owner.
    pub fn complete(mut self, completion: RemoteCompletion) -> Result<()> {
        ensure!(
            self.ready,
            "GitHub operation has not acquired resource ownership"
        );
        if completion == RemoteCompletion::Reconciled && self.intent != RemoteIntent::Reconcile {
            bail!("GitHub mutation cannot clear an uncertain resource");
        }
        ensure!(
            completion != RemoteCompletion::Uncertain || self.intent == RemoteIntent::Mutation,
            "read-only reconciliation cannot create mutation uncertainty"
        );
        self.completion = completion;
        Ok(())
    }
}

impl Drop for RemoteOperation {
    fn drop(&mut self) {
        self.queue
            .complete(&self.resource, &self.scope, self.id, self.completion);
    }
}
