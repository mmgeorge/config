//! One history writer thread per state root.
//!
//! A commit with `synchronous=FULL` waits on the disk, so a caller that commits inline paces
//! itself, and every task queued behind it, by the disk. This module moves those commits to
//! one named thread. Callers queue commands; the thread drains the queue, runs every queued
//! command inside one transaction, and commits once, so a burst of writes shares one sync.
//!
//! Two kinds of command exist. An awaited command returns only after its batch commits, and
//! reports its own failure to its caller. A write-behind command returns a [`WriteTicket`] at
//! once; its caller has already shown the content and accepts that a crash loses the queued
//! tail. A failed write-behind command fences its [`WriteScope`], which skips every later
//! write-behind command in that scope, so the saved state always stays a prefix of what was
//! shown.
//!
//! The API blocks on standard-library primitives. Async callers wrap the blocking calls in
//! `tokio::task::spawn_blocking`.

use std::{
    collections::{HashMap, VecDeque},
    path::{Path, PathBuf},
    sync::{Arc, Condvar, Mutex, MutexGuard, OnceLock, Weak, mpsc},
    thread::{JoinHandle, ThreadId},
};

use anyhow::{Context, Result};
use rusqlite::Connection;

use super::SqliteStore;
use crate::exchange::Exchange;

/// How many write-behind commands may queue before `enqueue` waits for the writer.
const WRITE_BEHIND_HIGH_WATER: usize = 256;

/// Name the state whose saved prefix a failed write-behind command ends.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum WriteScope {
    Session(String),
}

impl std::fmt::Display for WriteScope {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Session(session) => write!(formatter, "session {session}"),
        }
    }
}

/// Report that a scope's saved state stopped at an earlier write.
#[derive(Clone, Debug, thiserror::Error)]
#[error("history write-behind failed for {scope}: {reason}")]
pub(crate) struct WriteBehindFailure {
    scope: WriteScope,
    reason: String,
}

/// Identify one queued write-behind command to a caller that later waits for it.
#[derive(Clone, Debug)]
pub(crate) struct WriteTicket {
    sequence: u64,
    scope: WriteScope,
}

/// Lend the batch transaction to one command. The command runs inside its own savepoint.
pub(crate) struct HistoryTransaction<'batch> {
    connection: &'batch Connection,
}

impl std::ops::Deref for HistoryTransaction<'_> {
    type Target = Connection;
    fn deref(&self) -> &Connection {
        self.connection
    }
}

type TransactionCommand = Box<dyn FnOnce(&HistoryTransaction<'_>) -> Result<()> + Send>;

/// Describe the content of one write-behind command.
enum WriteBehindWork {
    Command(TransactionCommand),
    /// A whole-exchange save, kept as data so a newer save can replace it while it waits.
    Exchange(Box<Exchange>),
}

enum QueuedCommand {
    WriteBehind {
        ticket: WriteTicket,
        work: WriteBehindWork,
    },
    Awaited {
        run: TransactionCommand,
        settle: Box<dyn FnOnce(Result<()>) + Send>,
    },
    /// A caller waiting to borrow the store between batches.
    Lend(LendOffer),
}

/// Pass the writer's store to one caller thread and back.
struct LendSlot {
    state: Mutex<LendState>,
    changed: Condvar,
}

enum LendState {
    /// The caller waits for the writer to reach its place in the queue.
    Waiting,
    Lent(Box<SqliteStore>),
    /// The caller holds the store.
    Held,
    Returned(Box<SqliteStore>),
    /// The writer stopped before it reached this offer.
    Abandoned,
}

impl LendSlot {
    fn lock(&self) -> MutexGuard<'_, LendState> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }
}

/// The queued half of a lease. Dropping it unserved releases the waiting caller.
struct LendOffer(Arc<LendSlot>);

impl LendOffer {
    /// Lend the store and wait on this thread until the caller returns it.
    fn lend(self, store: SqliteStore) -> SqliteStore {
        let mut state = self.0.lock();
        *state = LendState::Lent(Box::new(store));
        self.0.changed.notify_all();
        loop {
            if let LendState::Returned(_) = &*state {
                let LendState::Returned(store) = std::mem::replace(&mut *state, LendState::Held)
                else {
                    unreachable!("the state was matched as returned");
                };
                return *store;
            }
            state = self
                .0
                .changed
                .wait(state)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }
}

impl Drop for LendOffer {
    fn drop(&mut self) {
        let mut state = self.0.lock();
        if matches!(*state, LendState::Waiting) {
            *state = LendState::Abandoned;
            self.0.changed.notify_all();
        }
    }
}

thread_local! {
    /// Whether this thread holds a lease. A second lease would wait on the first forever.
    static LEASE_HELD: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Borrow the writer's store on the calling thread, in queue order.
///
/// The writer thread waits while the lease lives, so every statement the holder runs is
/// ordered after the commands queued before the lease and before those queued after it. The
/// lease stays on the thread that took it.
pub(crate) struct WriterLease {
    slot: Arc<LendSlot>,
    store: Option<Box<SqliteStore>>,
    /// Keeps the writer thread alive until the store is back, so a lease taken from a
    /// temporary handle never makes that handle's drop wait for the lease itself.
    _writer: HistoryWriter,
    _thread_bound: std::marker::PhantomData<*const ()>,
}

impl std::ops::Deref for WriterLease {
    type Target = SqliteStore;
    fn deref(&self) -> &SqliteStore {
        self.store.as_ref().expect("a live lease holds the store")
    }
}

impl std::ops::DerefMut for WriterLease {
    fn deref_mut(&mut self) -> &mut SqliteStore {
        self.store.as_mut().expect("a live lease holds the store")
    }
}

/// Borrow only the writer's SQLite connection, for an owner that keeps its own tables.
pub(crate) struct ConnectionLease(WriterLease);

impl std::ops::Deref for ConnectionLease {
    type Target = Connection;
    fn deref(&self) -> &Connection {
        self.0.connection()
    }
}

impl std::ops::DerefMut for ConnectionLease {
    fn deref_mut(&mut self) -> &mut Connection {
        self.0.connection_mut()
    }
}

impl Drop for WriterLease {
    fn drop(&mut self) {
        LEASE_HELD.with(|held| held.set(false));
        if let Some(store) = self.store.take() {
            *self.slot.lock() = LendState::Returned(store);
            self.slot.changed.notify_all();
        }
    }
}

#[derive(Default)]
struct WriterQueue {
    commands: VecDeque<QueuedCommand>,
    queued_write_behind: usize,
    issued: u64,
    /// Every ticket at or below this sequence committed, failed, or was skipped.
    settled: u64,
    failure: HashMap<WriteScope, WriteBehindFailure>,
    closing: bool,
    stopped: bool,
}

struct WriterShared {
    queue: Mutex<WriterQueue>,
    /// Wakes the writer thread for new work or closing.
    work: Condvar,
    /// Wakes callers waiting for the watermark, queue capacity, or thread exit.
    progress: Condvar,
    /// Counts failed scopes, so a watcher that subscribes late still observes a failure.
    failed: tokio::sync::watch::Sender<u64>,
}

impl WriterShared {
    fn lock(&self) -> MutexGuard<'_, WriterQueue> {
        self.queue
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }
}

/// Stop and join the writer thread when the last handle drops, so the state lease and the
/// database file are released before the owner continues.
struct WriterThread {
    shared: Arc<WriterShared>,
    thread: Mutex<Option<JoinHandle<()>>>,
    thread_id: ThreadId,
    /// Idle read-only handles for this root. They live and die with the writer, so the state
    /// lease they hold is released when the last writer handle drops.
    readers: Arc<Mutex<Vec<SqliteStore>>>,
}

/// Map each canonical state root to its live writer.
fn registry() -> &'static Mutex<HashMap<PathBuf, Weak<WriterThread>>> {
    static WRITER: OnceLock<Mutex<HashMap<PathBuf, Weak<WriterThread>>>> = OnceLock::new();
    WRITER.get_or_init(Default::default)
}

impl WriterThread {
    fn stop(&self) {
        self.shared.lock().closing = true;
        self.shared.work.notify_all();
        if std::thread::current().id() == self.thread_id {
            return;
        }
        let thread = self
            .thread
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        if let Some(thread) = thread {
            let _ = thread.join();
        }
    }
}

impl Drop for WriterThread {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Queue history writes for the one thread that commits them.
#[derive(Clone)]
pub(crate) struct HistoryWriter {
    thread: Arc<WriterThread>,
}

impl HistoryWriter {
    /// Get the writer for a state root, starting its thread on first use.
    pub(crate) fn for_root(root: &Path) -> Result<Self> {
        std::fs::create_dir_all(root)
            .with_context(|| format!("create Harness data directory {}", root.display()))?;
        let key = std::fs::canonicalize(root)?;
        let mut registry = registry()
            .lock()
            .map_err(|_| anyhow::anyhow!("history writer registry lock poisoned"))?;
        registry.retain(|_, entry| entry.strong_count() > 0);
        if let Some(thread) = registry.get(&key).and_then(Weak::upgrade)
            && !thread.shared.lock().closing
        {
            return Ok(Self { thread });
        }
        let shared = Arc::new(WriterShared {
            queue: Mutex::new(WriterQueue::default()),
            work: Condvar::new(),
            progress: Condvar::new(),
            failed: tokio::sync::watch::Sender::new(0),
        });
        let (opened, open_result) = mpsc::sync_channel(1);
        let thread_shared = shared.clone();
        let thread_root = root.to_owned();
        let thread = std::thread::Builder::new()
            .name("forge-history-writer".into())
            .spawn(move || {
                let _stopped = StoppedOnExit(thread_shared.clone());
                #[expect(
                    clippy::disallowed_methods,
                    reason = "the writer thread opens the store that every lease borrows"
                )]
                let store = match SqliteStore::open(&thread_root) {
                    Ok(store) => {
                        let _ = opened.send(Ok(()));
                        store
                    }
                    Err(error) => {
                        let _ = opened.send(Err(error));
                        return;
                    }
                };
                run(store, &thread_shared);
            })
            .context("start history writer thread")?;
        let thread_id = thread.thread().id();
        let thread = Arc::new(WriterThread {
            shared,
            thread: Mutex::new(Some(thread)),
            thread_id,
            readers: Arc::default(),
        });
        open_result
            .recv()
            .context("history writer thread ended before opening storage")??;
        registry.insert(key, Arc::downgrade(&thread));
        Ok(Self { thread })
    }

    /// Get the writer for a root only if some owner has already started it.
    pub(super) fn started(root: &Path) -> Option<Self> {
        let key = std::fs::canonicalize(root).ok()?;
        let thread = registry().lock().ok()?.get(&key).and_then(Weak::upgrade)?;
        let open = !thread.shared.lock().closing;
        open.then_some(Self { thread })
    }

    /// Get this root's pool of idle read-only handles.
    pub(super) fn readers(&self) -> Arc<Mutex<Vec<SqliteStore>>> {
        self.thread.readers.clone()
    }

    fn shared(&self) -> &WriterShared {
        &self.thread.shared
    }

    /// Queue one write-behind command and return without waiting for its commit.
    pub(crate) fn enqueue(
        &self,
        scope: WriteScope,
        command: impl FnOnce(&HistoryTransaction<'_>) -> Result<()> + Send + 'static,
    ) -> Result<WriteTicket, WriteBehindFailure> {
        self.enqueue_work(scope, WriteBehindWork::Command(Box::new(command)))
    }

    /// Queue a whole-exchange save. It replaces a save of the same exchange that still waits
    /// at the tail of the queue, because the newer snapshot contains the older one. A save
    /// that is not at the tail stays: a tool-output delta queued after it depends on the byte
    /// offset that save leaves behind.
    pub(crate) fn enqueue_exchange(
        &self,
        exchange: Exchange,
    ) -> Result<WriteTicket, WriteBehindFailure> {
        let scope = WriteScope::Session(exchange.session_id.clone());
        self.enqueue_work(scope, WriteBehindWork::Exchange(Box::new(exchange)))
    }

    fn enqueue_work(
        &self,
        scope: WriteScope,
        work: WriteBehindWork,
    ) -> Result<WriteTicket, WriteBehindFailure> {
        let shared = self.shared();
        let mut queue = shared.lock();
        // A lease holder cannot wait for capacity: the writer drains nothing until the lease
        // returns.
        let may_wait = !LEASE_HELD.with(std::cell::Cell::get);
        while may_wait && queue.queued_write_behind >= WRITE_BEHIND_HIGH_WATER && !queue.stopped {
            queue = shared
                .progress
                .wait(queue)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
        if let Some(failure) = queue.failure.get(&scope) {
            return Err(failure.clone());
        }
        if queue.closing || queue.stopped {
            return Err(WriteBehindFailure {
                scope,
                reason: "the history writer is closed".into(),
            });
        }
        if let WriteBehindWork::Exchange(newer) = &work
            && let Some(QueuedCommand::WriteBehind {
                ticket,
                work: WriteBehindWork::Exchange(waiting),
            }) = queue.commands.back_mut()
            && waiting.id == newer.id
            && ticket.scope == scope
        {
            let ticket = ticket.clone();
            let WriteBehindWork::Exchange(newer) = work else {
                unreachable!("the work was matched as an exchange save");
            };
            *waiting = newer;
            return Ok(ticket);
        }
        queue.issued += 1;
        let ticket = WriteTicket {
            sequence: queue.issued,
            scope,
        };
        queue.commands.push_back(QueuedCommand::WriteBehind {
            ticket: ticket.clone(),
            work,
        });
        queue.queued_write_behind += 1;
        drop(queue);
        shared.work.notify_one();
        Ok(ticket)
    }

    /// Run one command in the next batch and return its output after that batch commits.
    pub(crate) fn commit<Output: Send + 'static>(
        &self,
        command: impl FnOnce(&HistoryTransaction<'_>) -> Result<Output> + Send + 'static,
    ) -> Result<Output> {
        let output = Arc::new(Mutex::new(None));
        let written = output.clone();
        let (settled, settlement) = mpsc::sync_channel(1);
        self.push(QueuedCommand::Awaited {
            run: Box::new(move |transaction| {
                let value = command(transaction)?;
                *written
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(value);
                Ok(())
            }),
            settle: Box::new(move |result| {
                let _ = settled.send(result);
            }),
        })?;
        off_worker(|| settlement.recv())
            .context("history writer stopped before the command settled")??;
        output
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
            .context("history writer settled a command without its output")
    }

    /// Borrow the store after every command queued before this call has settled.
    pub(crate) fn lease(&self) -> Result<WriterLease> {
        anyhow::ensure!(
            std::thread::current().id() != self.thread.thread_id,
            "the history writer thread cannot lease its own store"
        );
        anyhow::ensure!(
            !LEASE_HELD.with(std::cell::Cell::get),
            "this thread already holds a history writer lease"
        );
        let slot = Arc::new(LendSlot {
            state: Mutex::new(LendState::Waiting),
            changed: Condvar::new(),
        });
        self.push(QueuedCommand::Lend(LendOffer(slot.clone())))?;
        let mut state = slot.lock();
        loop {
            match &*state {
                LendState::Lent(_) => break,
                LendState::Abandoned => {
                    anyhow::bail!("the history writer stopped before lending its store")
                }
                LendState::Waiting | LendState::Held | LendState::Returned(_) => {
                    state = slot
                        .changed
                        .wait(state)
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                }
            }
        }
        let LendState::Lent(store) = std::mem::replace(&mut *state, LendState::Held) else {
            unreachable!("the state was matched as lent");
        };
        drop(state);
        LEASE_HELD.with(|held| held.set(true));
        Ok(WriterLease {
            slot,
            store: Some(store),
            _writer: self.clone(),
            _thread_bound: std::marker::PhantomData,
        })
    }

    /// Borrow the store of a root's writer for one operation, starting the writer when no
    /// owner has. The wait for the queue ahead moves off a runtime worker thread.
    pub(crate) fn lease_for(root: &Path) -> Result<WriterLease> {
        let writer = Self::for_root(root)?;
        off_worker(|| writer.lease())
    }

    /// Borrow the writer's connection after every command queued before this call has settled.
    pub(crate) fn connection(&self) -> Result<ConnectionLease> {
        self.lease().map(ConnectionLease)
    }

    /// Run work that needs the whole store, such as a method that manages its own
    /// transactions, after every command queued before it has settled. The outer error
    /// reports a writer that could not lend its store; the output is the command's own.
    ///
    /// The caller waits for the queue ahead of it and for its own commits. On a runtime worker
    /// thread that wait moves off the worker first, so no other task queues behind it.
    pub(crate) fn exclusive<Output>(
        &self,
        command: impl FnOnce(&mut SqliteStore) -> Output,
    ) -> Result<Output> {
        off_worker(|| Ok(command(&mut *self.lease()?)))
    }

    fn push(&self, command: QueuedCommand) -> Result<()> {
        let shared = self.shared();
        let mut queue = shared.lock();
        anyhow::ensure!(
            !queue.closing && !queue.stopped,
            "the history writer is closed"
        );
        queue.commands.push_back(command);
        drop(queue);
        shared.work.notify_one();
        Ok(())
    }

    /// Wait until the ticket's command settled, and report a failure of its scope.
    pub(crate) fn wait_committed(&self, ticket: &WriteTicket) -> Result<(), WriteBehindFailure> {
        let queue = self.wait_settled(ticket.sequence);
        match queue.failure.get(&ticket.scope) {
            Some(failure) => Err(failure.clone()),
            None if queue.settled < ticket.sequence => Err(WriteBehindFailure {
                scope: ticket.scope.clone(),
                reason: "the history writer stopped before this write committed".into(),
            }),
            None => Ok(()),
        }
    }

    /// Wait until every write-behind command queued so far has settled. A reader on another
    /// connection calls this before a read that must include content already shown.
    pub(crate) fn barrier(&self) {
        // A lease holder is already ordered after every earlier command, and the writer
        // cannot settle later ones until the lease returns.
        if LEASE_HELD.with(std::cell::Cell::get) {
            return;
        }
        let issued = self.shared().lock().issued;
        drop(self.wait_settled(issued));
    }

    fn wait_settled(&self, sequence: u64) -> MutexGuard<'_, WriterQueue> {
        let shared = self.shared();
        let mut queue = shared.lock();
        while queue.settled < sequence && !queue.stopped {
            queue = shared
                .progress
                .wait(queue)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
        queue
    }

    /// Get the failure that ended a scope's saved prefix, if any.
    pub(crate) fn failure(&self, scope: &WriteScope) -> Option<WriteBehindFailure> {
        self.shared().lock().failure.get(scope).cloned()
    }

    /// Watch the count of failed scopes. A watcher then asks `failure` about its own scope.
    pub(crate) fn failed(&self) -> tokio::sync::watch::Receiver<u64> {
        self.shared().failed.subscribe()
    }
}

/// Run blocking work without holding a runtime worker that other tasks queue behind.
///
/// A multi-thread runtime hands the worker's queued tasks to another thread for the duration.
/// A current-thread runtime has no second worker, and a thread outside any runtime has no
/// queue, so both run the work in place.
pub(crate) fn off_worker<Output>(work: impl FnOnce() -> Output) -> Output {
    let multi_thread = tokio::runtime::Handle::try_current().is_ok_and(|runtime| {
        runtime.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread
    });
    if multi_thread {
        tokio::task::block_in_place(work)
    } else {
        work()
    }
}

/// Release every waiter when the writer thread ends, including by panic.
struct StoppedOnExit(Arc<WriterShared>);

impl Drop for StoppedOnExit {
    fn drop(&mut self) {
        let mut queue = self.0.lock();
        queue.stopped = true;
        queue.commands.clear();
        queue.queued_write_behind = 0;
        drop(queue);
        self.0.progress.notify_all();
    }
}

/// Drain the queue in batches until the last handle closes the writer.
fn run(mut store: SqliteStore, shared: &WriterShared) {
    loop {
        let mut queue = shared.lock();
        while queue.commands.is_empty() && !queue.closing {
            queue = shared
                .work
                .wait(queue)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
        if queue.commands.is_empty() {
            return;
        }
        if matches!(queue.commands.front(), Some(QueuedCommand::Lend(_))) {
            let Some(QueuedCommand::Lend(offer)) = queue.commands.pop_front() else {
                unreachable!("the front command was matched as a lease");
            };
            drop(queue);
            store = offer.lend(store);
            continue;
        }
        let end = queue
            .commands
            .iter()
            .position(|command| matches!(command, QueuedCommand::Lend(_)))
            .unwrap_or(queue.commands.len());
        let batch: Vec<_> = queue.commands.drain(..end).collect();
        let fenced: std::collections::HashSet<WriteScope> = queue.failure.keys().cloned().collect();
        drop(queue);
        run_batch(&store, shared, batch, fenced);
    }
}

/// Recognize a lock timeout, which leaves the database unchanged and is worth retrying.
fn is_busy(error: &rusqlite::Error) -> bool {
    matches!(
        error.sqlite_error_code(),
        Some(rusqlite::ErrorCode::DatabaseBusy | rusqlite::ErrorCode::DatabaseLocked)
    )
}

/// Run one batch in one transaction, then publish, settle, and advance the watermark.
fn run_batch(
    store: &SqliteStore,
    shared: &WriterShared,
    batch: Vec<QueuedCommand>,
    mut fenced: std::collections::HashSet<WriteScope>,
) {
    let connection: &Connection = store.connection();
    if let Err(error) = connection.execute_batch("BEGIN IMMEDIATE") {
        let busy = is_busy(&error);
        let reason = format!(
            "{:#}",
            anyhow::Error::from(error).context("begin history batch")
        );
        let mut retained = Vec::new();
        let mut failures = Vec::new();
        let mut settled = 0;
        for command in batch {
            match command {
                QueuedCommand::Awaited { settle, .. } => settle(Err(anyhow::anyhow!("{reason}"))),
                // A lock timeout changed nothing, so the shown content stays queued.
                QueuedCommand::WriteBehind { .. } if busy => retained.push(command),
                QueuedCommand::WriteBehind { ticket, .. } => {
                    settled = settled.max(ticket.sequence);
                    failures.push(ticket.scope);
                }
                QueuedCommand::Lend(_) => unreachable!("a batch holds no lease"),
            }
        }
        let mut queue = shared.lock();
        for command in retained.into_iter().rev() {
            queue.commands.push_front(command);
        }
        finish_batch(shared, queue, settled, 0, failures, &reason);
        return;
    }

    let transaction = HistoryTransaction { connection };
    let mut settled = 0;
    let mut write_behind = 0;
    let mut failures: Vec<(WriteScope, String)> = Vec::new();
    let mut committed_scopes = Vec::new();
    let mut awaiting = Vec::new();
    let mut saved_exchanges = Vec::new();
    for command in batch {
        match command {
            QueuedCommand::WriteBehind { ticket, work } => {
                settled = settled.max(ticket.sequence);
                write_behind += 1;
                if fenced.contains(&ticket.scope) {
                    continue;
                }
                let result = in_savepoint(connection, || match work {
                    WriteBehindWork::Command(command) => command(&transaction),
                    WriteBehindWork::Exchange(exchange) => {
                        SqliteStore::write_exchange(connection, &exchange)
                            .context("write exchange")?;
                        saved_exchanges.push((
                            exchange.session_id.clone(),
                            exchange.id.clone(),
                            exchange.awaiting_input() && exchange.elicitation().is_some(),
                        ));
                        Ok(())
                    }
                });
                match result {
                    Ok(()) => committed_scopes.push(ticket.scope),
                    Err(error) => {
                        fenced.insert(ticket.scope.clone());
                        failures.push((ticket.scope, format!("{error:#}")));
                    }
                }
            }
            QueuedCommand::Awaited { run, settle } => {
                match in_savepoint(connection, || run(&transaction)) {
                    Ok(()) => awaiting.push(settle),
                    Err(error) => settle(Err(error)),
                }
            }
            QueuedCommand::Lend(_) => unreachable!("a batch holds no lease"),
        }
    }

    if let Err(error) = connection.execute_batch("COMMIT") {
        let _ = connection.execute_batch("ROLLBACK");
        let reason = format!(
            "{:#}",
            anyhow::Error::from(error).context("commit history batch")
        );
        for settle in awaiting {
            settle(Err(anyhow::anyhow!("{reason}")));
        }
        // Nothing in this batch was saved, so every scope that wrote in it ends here.
        failures.extend(
            committed_scopes
                .into_iter()
                .map(|scope| (scope, reason.clone())),
        );
    } else {
        for (session, exchange, waiting) in saved_exchanges {
            if fenced.contains(&WriteScope::Session(session.clone())) {
                continue;
            }
            if let Err(error) = store.publish_admission_exchange(&session, &exchange, Some(waiting))
            {
                let scope = WriteScope::Session(session);
                fenced.insert(scope.clone());
                failures.push((scope, format!("{error:#}")));
            }
        }
        for settle in awaiting {
            settle(Ok(()));
        }
    }
    let queue = shared.lock();
    let mut scopes = Vec::new();
    let mut reasons = HashMap::new();
    for (scope, reason) in failures {
        reasons.entry(scope.clone()).or_insert(reason);
        scopes.push(scope);
    }
    finish_batch_with(shared, queue, settled, write_behind, scopes, |scope| {
        reasons.get(scope).cloned().unwrap_or_default()
    });
}

/// Run one command inside a savepoint so its failure leaves the rest of the batch intact.
fn in_savepoint(connection: &Connection, command: impl FnOnce() -> Result<()>) -> Result<()> {
    connection.execute_batch("SAVEPOINT history_command")?;
    match command() {
        Ok(()) => {
            connection.execute_batch("RELEASE history_command")?;
            Ok(())
        }
        Err(error) => {
            connection
                .execute_batch("ROLLBACK TO history_command; RELEASE history_command")
                .context("roll back failed history command")?;
            Err(error)
        }
    }
}

fn finish_batch(
    shared: &WriterShared,
    queue: MutexGuard<'_, WriterQueue>,
    settled: u64,
    write_behind: usize,
    failures: Vec<WriteScope>,
    reason: &str,
) {
    let settled_write_behind = write_behind + failures.len();
    finish_batch_with(
        shared,
        queue,
        settled,
        settled_write_behind,
        failures,
        |_| reason.to_owned(),
    );
}

/// Record failures, advance the watermark, and wake every waiter.
fn finish_batch_with(
    shared: &WriterShared,
    mut queue: MutexGuard<'_, WriterQueue>,
    settled: u64,
    write_behind: usize,
    failures: Vec<WriteScope>,
    reason: impl Fn(&WriteScope) -> String,
) {
    let failed = !failures.is_empty();
    for scope in failures {
        let reason = reason(&scope);
        queue
            .failure
            .entry(scope.clone())
            .or_insert(WriteBehindFailure { scope, reason });
    }
    queue.settled = queue.settled.max(settled);
    queue.queued_write_behind = queue.queued_write_behind.saturating_sub(write_behind);
    drop(queue);
    shared.progress.notify_all();
    if failed {
        shared.failed.send_modify(|count| *count += 1);
    }
}

/// Keep one store handle from reading or writing ahead of the write-behind work it queued.
///
/// Dereferencing waits for that work first. The wait keeps every direct statement on this
/// handle ordered after the queued writes, which run on the writer's own connection.
pub(super) struct OrderedConnection {
    connection: super::database::HistoryConnection,
    writer: std::cell::OnceCell<HistoryWriter>,
    outstanding: std::cell::RefCell<Option<WriteTicket>>,
}

impl OrderedConnection {
    pub(super) fn new(connection: super::database::HistoryConnection) -> Self {
        Self {
            connection,
            writer: std::cell::OnceCell::new(),
            outstanding: std::cell::RefCell::new(None),
        }
    }

    /// Get this root's writer, starting it on the first write-behind save.
    pub(super) fn writer(&self, root: &Path) -> Result<&HistoryWriter> {
        if let Some(writer) = self.writer.get() {
            return Ok(writer);
        }
        let writer = HistoryWriter::for_root(root)?;
        Ok(self.writer.get_or_init(|| writer))
    }

    /// Get the writer only if this handle has already queued work on it.
    pub(super) fn started_writer(&self) -> Option<&HistoryWriter> {
        self.writer.get()
    }

    /// Remember the newest queued write; the next direct statement waits for it.
    pub(super) fn retain(&self, ticket: WriteTicket) {
        *self.outstanding.borrow_mut() = Some(ticket);
    }

    /// Borrow SQLite without waiting, for the writer thread's own handle.
    pub(super) fn unordered(&self) -> &Connection {
        &self.connection
    }

    /// Borrow SQLite mutably without waiting, for the writer thread's own handle.
    pub(super) fn unordered_mut(&mut self) -> &mut Connection {
        &mut self.connection
    }

    pub(super) fn settle(&self) {
        // A lease holder is ordered by its lease; the writer settles nothing until it returns.
        if LEASE_HELD.with(std::cell::Cell::get) {
            return;
        }
        let ticket = self.outstanding.borrow_mut().take();
        if let (Some(ticket), Some(writer)) = (ticket, self.writer.get()) {
            // A failure is reported through the scope's fence, not through this statement.
            let _ = writer.wait_committed(&ticket);
        }
    }
}

impl std::ops::Deref for OrderedConnection {
    type Target = Connection;
    fn deref(&self) -> &Connection {
        self.settle();
        &self.connection
    }
}

impl std::ops::DerefMut for OrderedConnection {
    fn deref_mut(&mut self) -> &mut Connection {
        self.settle();
        &mut self.connection
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;
    use crate::storage::test::{running_exchange, session};
    use rusqlite::params;

    /// Open a root with one session, scratch tables, and a second connection for observation.
    fn fixture() -> Result<(tempfile::TempDir, HistoryWriter, Connection)> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&session("session", "D:/work"))?;
        store.save_agent_run(&crate::agent::Agent::primary("session", 0))?;
        drop(store);
        let observer = crate::storage::test::raw_connection(root.path())?;
        observer.execute_batch(
            "CREATE TABLE note(scope TEXT NOT NULL, body TEXT NOT NULL);
             CREATE TABLE topic(id INTEGER PRIMARY KEY);
             CREATE TABLE reply(topic INTEGER NOT NULL REFERENCES topic(id) DEFERRABLE INITIALLY DEFERRED);",
        )?;
        let writer = HistoryWriter::for_root(root.path())?;
        Ok((root, writer, observer))
    }

    fn scope(session: &str) -> WriteScope {
        WriteScope::Session(session.into())
    }

    fn note(
        writer: &HistoryWriter,
        session: &'static str,
        body: &'static str,
    ) -> Result<WriteTicket, WriteBehindFailure> {
        writer.enqueue(scope(session), move |transaction| {
            transaction.execute("INSERT INTO note VALUES(?1,?2)", params![session, body])?;
            Ok(())
        })
    }

    fn notes(observer: &Connection) -> Vec<String> {
        let mut statement = observer
            .prepare("SELECT body FROM note ORDER BY rowid")
            .unwrap();
        statement
            .query_map([], |row| row.get(0))
            .unwrap()
            .collect::<rusqlite::Result<_>>()
            .unwrap()
    }

    /// Hold the writer inside an exclusive command so later commands queue deterministically.
    fn park(writer: &HistoryWriter) -> (mpsc::Sender<()>, std::thread::JoinHandle<Result<()>>) {
        let (release, released) = mpsc::channel::<()>();
        let (parked, is_parked) = mpsc::channel::<()>();
        let parking = writer.clone();
        let holder = std::thread::spawn(move || {
            parking.exclusive(move |_| {
                let _ = parked.send(());
                let _ = released.recv();
            })
        });
        is_parked.recv().unwrap();
        (release, holder)
    }

    /// Commands queued while the writer is busy share one commit.
    #[test]
    fn queued_commands_commit_as_one_batch() -> Result<()> {
        let (_root, writer, observer) = fixture()?;
        let version = |observer: &Connection| -> i64 {
            observer
                .pragma_query_value(None, "data_version", |row| row.get(0))
                .unwrap()
        };
        let (release, holder) = park(&writer);
        note(&writer, "session", "one")?;
        note(&writer, "session", "two")?;
        let last = note(&writer, "other", "three")?;
        let before = version(&observer);
        release.send(()).unwrap();
        holder.join().unwrap()?;
        writer.wait_committed(&last)?;
        assert_eq!(notes(&observer), ["one", "two", "three"]);
        assert_eq!(
            version(&observer),
            before + 1,
            "the batch used several commits"
        );
        Ok(())
    }

    /// A failed awaited command reports its own error and leaves its neighbours committed.
    #[test]
    fn failed_command_rolls_back_alone() -> Result<()> {
        let (_root, writer, observer) = fixture()?;
        let (release, holder) = park(&writer);
        let kept = note(&writer, "session", "kept")?;
        let failing = writer.clone();
        let awaited = std::thread::spawn(move || {
            failing.commit(|transaction| {
                transaction.execute("INSERT INTO note VALUES('session','discarded')", [])?;
                anyhow::ensure!(transaction.is_autocommit(), "refused");
                Ok(())
            })
        });
        // The awaited command is queued once two commands wait behind the parked one.
        while writer.shared().lock().commands.len() < 2 {
            std::thread::yield_now();
        }
        release.send(()).unwrap();
        holder.join().unwrap()?;
        assert!(awaited.join().unwrap().is_err());
        writer.wait_committed(&kept)?;
        assert_eq!(notes(&observer), ["kept"]);
        assert_eq!(writer.commit(|_| Ok(7))?, 7);
        Ok(())
    }

    /// A failed write-behind command fences its scope and skips that scope's later commands.
    #[test]
    fn failed_write_behind_fences_only_its_scope() -> Result<()> {
        let (_root, writer, observer) = fixture()?;
        let failures = writer.failed();
        observer.execute_batch(
            "CREATE TRIGGER refuse_note BEFORE INSERT ON note WHEN NEW.body='refused'
             BEGIN SELECT RAISE(ABORT,'note refused'); END;",
        )?;
        let (release, holder) = park(&writer);
        note(&writer, "session", "before")?;
        let refused = note(&writer, "session", "refused")?;
        let skipped = note(&writer, "session", "after")?;
        let other = note(&writer, "other", "unrelated")?;
        release.send(()).unwrap();
        holder.join().unwrap()?;

        writer.wait_committed(&other)?;
        assert!(writer.wait_committed(&refused).is_err());
        assert!(writer.wait_committed(&skipped).is_err());
        assert_eq!(notes(&observer), ["before", "unrelated"]);
        assert!(
            writer
                .failure(&scope("session"))
                .is_some_and(|failure| failure.to_string().contains("note refused"))
        );
        assert!(writer.failure(&scope("other")).is_none());
        assert!(failures.has_changed().unwrap());
        assert!(note(&writer, "session", "later").is_err());
        Ok(())
    }

    /// A commit that fails saves nothing, so every scope that wrote in the batch is fenced.
    #[test]
    fn failed_commit_fences_every_scope_in_the_batch() -> Result<()> {
        let (_root, writer, observer) = fixture()?;
        let (release, holder) = park(&writer);
        let first = note(&writer, "session", "lost")?;
        let orphan = writer.enqueue(scope("other"), |transaction| {
            // The deferred foreign key is checked at COMMIT, not at this statement.
            transaction.execute("INSERT INTO reply VALUES(99)", [])?;
            Ok(())
        })?;
        release.send(()).unwrap();
        holder.join().unwrap()?;

        assert!(writer.wait_committed(&orphan).is_err());
        assert!(writer.wait_committed(&first).is_err());
        assert!(notes(&observer).is_empty());
        writer.barrier();
        assert_eq!(
            writer.commit(|_| Ok(1))?,
            1,
            "the writer stopped after a failed commit"
        );
        Ok(())
    }

    /// A newer exchange save replaces one waiting at the tail, never one behind another command.
    #[test]
    fn exchange_saves_collapse_only_at_the_queue_tail() -> Result<()> {
        let (_root, writer, observer) = fixture()?;
        let (release, holder) = park(&writer);
        let mut exchange = running_exchange("active");
        let first = writer.enqueue_exchange(exchange.clone())?;
        exchange.prompt = "second".into();
        let second = writer.enqueue_exchange(exchange.clone())?;
        assert_eq!(first.sequence, second.sequence);
        note(&writer, "session", "between")?;
        exchange.prompt = "third".into();
        let third = writer.enqueue_exchange(exchange)?;
        assert!(third.sequence > second.sequence);
        release.send(()).unwrap();
        holder.join().unwrap()?;

        writer.wait_committed(&third)?;
        let prompt: String = observer.query_row(
            "SELECT json_extract(payload,'$.prompt') FROM exchange_record WHERE id='active'",
            [],
            |row| row.get(0),
        )?;
        assert_eq!(prompt, "third");
        Ok(())
    }
}
