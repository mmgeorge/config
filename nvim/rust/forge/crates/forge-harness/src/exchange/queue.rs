//! Retain FIFO admission and completion waiters inside the exchange owner.

use std::collections::{HashMap, VecDeque};

use tokio::sync::oneshot;

use super::command::{CommandId, CommandRequest, CommandResult, ExecuteCommandError};
use super::core_log::{CoreOutcome, CoreRecord};
use super::journal::{CommandJournal, CommandJournalError, CommandRecord};

/// Resolve one observer independently of the command's lifetime and retained result.
pub(super) type CommandReply = oneshot::Sender<Result<CommandResult, ExecuteCommandError>>;

/// Transfer one identified command and its admitted completion/capacity ownership together.
pub(super) struct CommandAdmission {
    pub(super) request: CommandRequest,
    pub(super) reply: CommandReply,
    pub(super) task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
}

/// Own durable FIFO admission without launching I/O or changing exchange state.
pub(super) struct CommandQueue {
    journal: CommandJournal,
    order: VecDeque<CommandId>,
    pending: HashMap<CommandId, QueuedCommand>,
    availability: QueueAvailability,
    publications: Option<std::sync::Arc<super::publication::SessionPublications>>,
}

impl CommandQueue {
    /// Append safe core evidence through this queue's existing durable journal owner.
    pub(super) fn append_core(
        &mut self,
        record: &super::core_log::CoreRecord,
    ) -> Result<u64, CommandJournalError> {
        self.journal.append_core(record)
    }

    /// End live observation through the sole journal owner while retaining active cleanup.
    pub(super) fn lose_observation(
        &mut self,
        command: &mut QueuedCommand,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        if command
            .lose_observation(&mut self.journal, trace, session, step)
            .is_err()
        {
            self.retention_failed(trace, session, step);
        }
    }

    /// Persist response-attempt evidence before publishing a result to one observer.
    fn respond(
        &mut self,
        log: CommandResponseLog<'_>,
        reply: CommandReply,
        result: Result<CommandResult, ExecuteCommandError>,
    ) {
        if log.send(&mut self.journal, reply, result).is_err() {
            self.retention_failed(log.trace, log.session, log.step);
        }
    }

    /// Couple command certification to the session's required publication storage.
    pub(super) fn bind_publications(
        &mut self,
        publications: std::sync::Arc<super::publication::SessionPublications>,
    ) {
        self.publications = Some(publications);
    }
    /// Fence admission and terminate pending observers when committed output cannot be retained.
    pub(super) fn retention_failed(
        &mut self,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        if matches!(self.availability, QueueAvailability::StorageUnavailable) {
            return;
        }
        self.availability = QueueAvailability::StorageUnavailable;
        trace.record(
            session,
            "exchange.storage.outcome_uncommitted",
            serde_json::json!({"update_step":step}),
        );
        self.fail_pending(
            ExecuteCommandError::OutcomeUncommitted,
            trace,
            session,
            step,
        );
    }
    /// Recover the journal before admitting any commands in the new owner epoch.
    pub(super) fn open(root: &std::path::Path, session: &str) -> Result<Self, CommandJournalError> {
        let mut journal = CommandJournal::open(root, session)?;
        journal.recover()?;
        Ok(Self {
            journal,
            order: VecDeque::new(),
            pending: HashMap::new(),
            availability: QueueAvailability::Open,
            publications: None,
        })
    }

    /// Prevent interrupted task startup from becoming an automatic readiness resend after recovery.
    pub(super) fn restore_task_delivery(
        &self,
        services: &super::services::OwnerServices,
    ) -> Result<Option<crate::input::InputUpdate>, CommandJournalError> {
        let task = services
            .tasks
            .current()
            .map_err(|_| CommandJournalError::DomainCommit)?;
        let Some(entry) = task
            .as_ref()
            .and_then(|task| task.entry.as_ref())
            .filter(|entry| entry.delivery == crate::task::TaskInputDelivery::Pending)
        else {
            return Ok(None);
        };
        let Some(failed) = self.journal.failed_task_input(&entry.id)? else {
            return Ok(None);
        };
        let input = services
            .input
            .snapshot()
            .map_err(|_| CommandJournalError::DomainCommit)?;
        if input.suspended {
            return Ok(None);
        }
        // Only an explicit, later delivery-resume commit can lift the failed-startup pause across epochs.
        if let Some(id) = &input.control_operation_id {
            let id = CommandId::new(id.clone()).map_err(|_| CommandJournalError::DomainCommit)?;
            if let Some(resumed) = self.journal.lookup(&id)?
                && resumed.sequence > failed.sequence
                && matches!(
                    &resumed.request.command,
                    super::command::Command::Input(super::command::InputCommand::ResumeDelivery)
                )
                && resumed.completion().is_some_and(|result| result.is_ok())
            {
                return Ok(None);
            }
        }
        services
            .input
            .suspend(true, Some(failed.request.id.as_str()))
            .map(Some)
            .map_err(|_| CommandJournalError::DomainCommit)
    }

    /// Attach/replay an existing identity or durably append one FIFO admission.
    pub(super) fn admit(
        &mut self,
        admission: CommandAdmission,
        active: Option<&mut QueuedCommand>,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        let CommandAdmission {
            request,
            reply,
            task_admission,
        } = admission;
        let response_log = CommandResponseLog {
            id: &request.id,
            trace,
            session,
            step,
        };
        if let Err(error) = self.journal.append_core(&CoreRecord::CommandReceived {
            command: request.id.clone(),
            kind: super::core_log::CoreCommandKind::from(&request.command),
        }) {
            self.storage_failed(error, reply, response_log);
            return;
        }
        if self.storage_unavailable() {
            self.retention_failed(trace, session, step);
            self.respond(
                response_log,
                reply,
                Err(ExecuteCommandError::OutcomeUncommitted),
            );
            return;
        }
        match self.availability {
            QueueAvailability::Open => {}
            QueueAvailability::Closing => {
                self.respond(response_log, reply, Err(ExecuteCommandError::OwnerClosed));
                return;
            }
            QueueAvailability::StorageUnavailable => {
                self.respond(
                    response_log,
                    reply,
                    Err(ExecuteCommandError::OutcomeUncommitted),
                );
                return;
            }
        }
        let previous = match self.journal.lookup(&request.id) {
            Ok(previous) => previous,
            Err(error) => {
                self.storage_failed(error, reply, response_log);
                return;
            }
        };
        if let Some(previous) = previous {
            let equal = serde_json::to_vec(&previous.request)
                .and_then(|expected| Ok(expected == serde_json::to_vec(&request)?));
            match equal {
                Ok(false) => {
                    self.respond(
                        response_log,
                        reply,
                        Err(ExecuteCommandError::IdentityConflict),
                    );
                    return;
                }
                Err(error) => {
                    self.storage_failed(error.into(), reply, response_log);
                    return;
                }
                Ok(true) => {}
            }
            if let Some(outcome) = previous.completion() {
                self.respond(
                    response_log,
                    reply,
                    outcome.map_err(ExecuteCommandError::Failed),
                );
                return;
            }
            let destination = match active {
                Some(active) if active.record.request.id == request.id => Some(active),
                Some(_) | None => self.pending.get_mut(&request.id),
            };
            if let Some(command) = destination {
                command.waiters.push(reply);
                return;
            }
            self.storage_failed(CommandJournalError::NotActive, reply, response_log);
            return;
        }
        match self.journal.admit(&request) {
            Ok(record) => {
                trace.record(session, "exchange.command.queued", serde_json::json!({
                    "command_id":request.id.as_str(),"sequence":record.sequence,"revision":record.state.revision(),"update_step":step,
                }));
                self.order.push_back(request.id.clone());
                self.pending.insert(
                    request.id,
                    QueuedCommand {
                        record,
                        waiters: vec![reply],
                        admitted_at: tokio::time::Instant::now(),
                        task_admission,
                    },
                );
            }
            Err(CommandJournalError::IdentityConflict) => {
                self.respond(
                    response_log,
                    reply,
                    Err(ExecuteCommandError::IdentityConflict),
                );
            }
            Err(error) => self.storage_failed(error, reply, response_log),
        }
    }

    /// Get the command that `start` would take next, without starting it.
    pub(super) fn next_command(&self) -> Option<&super::command::Command> {
        let id = self.order.front()?;
        self.pending
            .get(id)
            .map(|command| &command.record.request.command)
    }

    /// Start at most one queued command when the driver's update-step contract permits it.
    pub(super) fn start(
        &mut self,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) -> Option<QueuedCommand> {
        if self.storage_unavailable() || !matches!(self.availability, QueueAvailability::Open) {
            return None;
        }
        let id = self.order.pop_front()?;
        let mut command = self
            .pending
            .remove(&id)
            .expect("FIFO entries retain their command and resolvers");
        match self.journal.start(&command.record) {
            Ok(record) => {
                command.record = record;
                command.admitted_at = tokio::time::Instant::now();
                trace.record(session, "exchange.command.started", serde_json::json!({
                    "command_id":id.as_str(),"sequence":command.record.sequence,"revision":command.record.state.revision(),"command":command.record.request.command.name(),"update_step":step,
                }));
                Some(command)
            }
            Err(error) => {
                self.uncommitted(command, error, trace, session, step);
                None
            }
        }
    }

    /// Read completion committed atomically by the input domain transaction.
    pub(super) fn committed(
        &self,
        id: &CommandId,
    ) -> Result<Option<CommandRecord>, CommandJournalError> {
        self.journal.lookup(id)
    }

    /// Commit a prerequisite failure or a previously durable idempotent domain result.
    pub(super) fn finish(
        &mut self,
        command: QueuedCommand,
        outcome: Result<super::command::CommandOutcome, super::command::CommandFailure>,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        if self.storage_unavailable() {
            let mut command = command;
            self.lose_observation(&mut command, trace, session, step);
            return;
        }
        match self.journal.finish(&command.record.request.id, outcome) {
            Ok(record) => {
                self.publish_completion(&record, trace, session, step);
                if command
                    .notify(&mut self.journal, record, trace, session, step)
                    .is_err()
                {
                    self.retention_failed(trace, session, step);
                }
            }
            Err(error) => self.uncommitted(command, error, trace, session, step),
        }
    }

    /// Notify a result already committed by a domain transaction without another write.
    pub(super) fn complete(
        &mut self,
        mut command: QueuedCommand,
        record: CommandRecord,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        if self.storage_unavailable() {
            self.lose_observation(&mut command, trace, session, step);
            return;
        }
        self.publish_completion(&record, trace, session, step);
        if command
            .notify(&mut self.journal, record, trace, session, step)
            .is_err()
        {
            self.retention_failed(trace, session, step);
        };
    }

    /// Notify disconnected response observers that this exact durable result can be read once.
    fn publish_completion(
        &self,
        record: &CommandRecord,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        let Some(publications) = &self.publications else {
            return;
        };
        if let Err(error) = publications.publish_batch(vec![forge_protocol::message::SessionEvent {
            session_id: session.into(), event: "command_completed".into(),
            payload: serde_json::json!({"id":record.request.id,"revision":record.state.revision()}),
        }]) {
            trace.record(session, "exchange.command.notification_failed", serde_json::json!({"command_id":record.request.id,"update_step":step,"reason":error.to_string()}));
        }
    }

    /// Terminate live observation after a failed write; never require a second failing write.
    pub(super) fn uncommitted(
        &mut self,
        command: QueuedCommand,
        error: CommandJournalError,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        trace.record(session, "exchange.command.uncommitted", serde_json::json!({
            "command_id":command.record.request.id.as_str(),"sequence":command.record.sequence,"update_step":step,"reason":error.to_string(),
        }));
        self.availability = QueueAvailability::StorageUnavailable;
        command.fail_observers(
            ExecuteCommandError::OutcomeUncommitted,
            trace,
            session,
            step,
        );
        self.fail_pending(
            ExecuteCommandError::OutcomeUncommitted,
            trace,
            session,
            step,
        );
    }

    /// Preserve durable queued intent and release waiters while the host shuts down.
    pub(super) fn close(&mut self, trace: &crate::trace::TraceStore, session: &str, step: u64) {
        if matches!(self.availability, QueueAvailability::StorageUnavailable) {
            return;
        }
        self.availability = QueueAvailability::Closing;
        while let Some(id) = self.order.pop_front() {
            let command = self
                .pending
                .remove(&id)
                .expect("queued identities retain their command");
            self.finish(
                command,
                Err(super::command::CommandFailure::OwnerStopping),
                trace,
                session,
                step,
            );
        }
    }

    /// Report runnable work without exposing mutable queue contents.
    pub(super) fn has_pending(&self) -> bool {
        matches!(self.availability, QueueAvailability::Open) && !self.order.is_empty()
    }

    /// Keep active resource collection running while live waiters learn storage failed.
    pub(super) fn storage_unavailable(&self) -> bool {
        matches!(self.availability, QueueAvailability::StorageUnavailable)
            || self
                .publications
                .as_ref()
                .is_some_and(|publications| publications.storage_failed())
    }

    /// Close admission after a storage failure without pretending the rejected wait committed.
    fn storage_failed(
        &mut self,
        error: CommandJournalError,
        reply: CommandReply,
        response_log: CommandResponseLog<'_>,
    ) {
        response_log.trace.record(response_log.session, "exchange.command.storage_failed", serde_json::json!({
            "command_id":response_log.id.as_str(),"update_step":response_log.step,"reason":error.to_string(),
        }));
        self.availability = QueueAvailability::StorageUnavailable;
        self.respond(
            response_log,
            reply,
            Err(ExecuteCommandError::OutcomeUncommitted),
        );
        self.fail_pending(
            ExecuteCommandError::OutcomeUncommitted,
            response_log.trace,
            response_log.session,
            response_log.step,
        );
    }

    /// Consume every queued resolver while retaining authoritative journal rows for recovery.
    fn fail_pending(
        &mut self,
        error: ExecuteCommandError,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        self.order.clear();
        for (_, command) in self.pending.drain() {
            command.fail_observers(error.clone(), trace, session, step);
        }
    }
}

/// Retain resolvers with the exact record they observe until terminal notification.
pub(super) struct QueuedCommand {
    /// Immutable request and its committed active revision.
    pub(super) record: CommandRecord,
    /// Monotonic active-command admission; FIFO waiting does not consume its response budget.
    pub(super) admitted_at: tokio::time::Instant,
    /// Reserved child-task capacity acquired before this command entered its parent's FIFO.
    pub(super) task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
    waiters: Vec<CommandReply>,
}

impl QueuedCommand {
    /// End live observation while retaining the command and its in-flight resource ownership.
    fn lose_observation(
        &mut self,
        journal: &mut CommandJournal,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) -> Result<(), CommandJournalError> {
        let log = CommandResponseLog {
            id: &self.record.request.id,
            trace,
            session,
            step,
        };
        let mut failure = None;
        for waiter in self.waiters.drain(..) {
            if failure.is_some() {
                log.send_unretained(waiter, Err(ExecuteCommandError::OutcomeUncommitted));
                continue;
            }
            if let Err(error) = log.send(
                journal,
                waiter,
                Err(ExecuteCommandError::OutcomeUncommitted),
            ) {
                failure.get_or_insert(error);
            }
        }
        failure.map_or(Ok(()), Err)
    }
    /// Notify each attached observer after the journal proves the terminal outcome.
    fn notify(
        self,
        journal: &mut CommandJournal,
        record: CommandRecord,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) -> Result<(), CommandJournalError> {
        let Some(completion) = record.completion() else {
            self.fail_observers(ExecuteCommandError::ObservationLost, trace, session, step);
            return Err(CommandJournalError::NotActive);
        };
        trace.record(session, "exchange.command.completed", serde_json::json!({
            "command_id":record.request.id.as_str(),"sequence":record.sequence,"revision":record.state.revision(),"update_step":step,"succeeded":completion.is_ok(),
        }));
        let log = CommandResponseLog {
            id: &record.request.id,
            trace,
            session,
            step,
        };
        let mut failure = None;
        for reply in self.waiters {
            if failure.is_some() {
                log.send_unretained(reply, Err(ExecuteCommandError::OutcomeUncommitted));
                continue;
            }
            if let Err(error) = log.send(
                journal,
                reply,
                completion.clone().map_err(ExecuteCommandError::Failed),
            ) {
                failure.get_or_insert(error);
            }
        }
        failure.map_or(Ok(()), Err)
    }

    /// Release observers after storage failure without retrying the unavailable journal per waiter.
    fn fail_observers(
        self,
        error: ExecuteCommandError,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) {
        let log = CommandResponseLog {
            id: &self.record.request.id,
            trace,
            session,
            step,
        };
        for reply in self.waiters {
            log.send_unretained(reply, Err(error.clone()));
        }
    }
}

enum QueueAvailability {
    Open,
    Closing,
    StorageUnavailable,
}

/// Correlate every response attempt, including replay, admission rejection, and lost observation.
#[derive(Clone, Copy)]
struct CommandResponseLog<'scope> {
    /// Immutable identity received from the caller.
    id: &'scope CommandId,
    /// Session trace retaining structured completion/response observations.
    trace: &'scope crate::trace::TraceStore,
    /// Session containing the command's identity and FIFO sequence.
    session: &'scope str,
    /// Owner update step consuming or resolving this observer.
    step: u64,
}

impl CommandResponseLog<'_> {
    /// Notify one observer and record its exact identity even if the receiver was dropped.
    fn send(
        self,
        journal: &mut CommandJournal,
        reply: CommandReply,
        result: Result<CommandResult, ExecuteCommandError>,
    ) -> Result<(), CommandJournalError> {
        let persisted = journal.append_core(&CoreRecord::CommandResponded {
            command: self.id.clone(),
            outcome: CoreOutcome::response(&result),
        });
        let result = if persisted.is_ok() {
            result
        } else {
            Err(ExecuteCommandError::OutcomeUncommitted)
        };
        self.send_unretained(reply, result);
        persisted.map(|_| ())
    }

    /// Publish an explicitly uncertified response without blocking required control progress on failed storage.
    fn send_unretained(
        self,
        reply: CommandReply,
        result: Result<CommandResult, ExecuteCommandError>,
    ) {
        let (sequence, revision) = match &result {
            Ok(result) => (Some(result.sequence), Some(result.revision)),
            Err(_) => (None, None),
        };
        let code = result.as_ref().err().map(ExecuteCommandError::code);
        let observed = reply.send(result).is_ok();
        self.trace.record(
            self.session,
            "exchange.command.response",
            serde_json::json!({
                "command_id":self.id.as_str(),"sequence":sequence,"revision":revision,
                "update_step":self.step,"error_code":code,"observed":observed,
            }),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::super::command::{CancelExchangeRequest, Command, CommandFailure, InputControl};
    use super::*;

    /// Create captured cancellation intent without any caller payload or native work.
    fn request(id: &str) -> CommandRequest {
        CommandRequest {
            id: CommandId::new(id).unwrap(),
            command: Command::CancelExchange(CancelExchangeRequest::default()),
            input_control: InputControl::default(),
        }
    }

    /// A failed required response record fences admission and resolves all waiters without a trace queue dependency.
    #[tokio::test]
    async fn core_response_failure_downgrades_reply_and_releases_pending_observers()
    -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        super::super::core_log::persist_test_session(root.path(), "session")?;
        let trace = crate::trace::TraceStore::open(root.path())?;
        trace.configure(crate::trace::LogLevel::None)?;
        let mut queue = CommandQueue::open(root.path(), "session")?;
        let (active_reply, active_response) = oneshot::channel();
        let active_request = request("active");
        queue.admit(
            CommandAdmission {
                request: active_request.clone(),
                reply: active_reply,
                task_admission: None,
            },
            None,
            &trace,
            "session",
            1,
        );
        let active = queue.start(&trace, "session", 2).unwrap();
        let (pending_reply, pending_response) = oneshot::channel();
        queue.admit(
            CommandAdmission {
                request: request("pending"),
                reply: pending_reply,
                task_admission: None,
            },
            None,
            &trace,
            "session",
            3,
        );
        let database = crate::storage::test::raw_connection(root.path())?;
        database.execute_batch("CREATE TRIGGER reject_core_response BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='command_responded' BEGIN SELECT RAISE(ABORT,'fixture core response failure'); END;")?;
        queue.finish(
            active,
            Err(CommandFailure::Interrupted),
            &trace,
            "session",
            4,
        );
        assert!(matches!(
            active_response.await?,
            Err(ExecuteCommandError::OutcomeUncommitted)
        ));
        assert!(matches!(
            pending_response.await?,
            Err(ExecuteCommandError::OutcomeUncommitted)
        ));
        assert!(queue.storage_unavailable());
        assert!(!queue.has_pending());
        assert!(
            queue
                .committed(&active_request.id)?
                .unwrap()
                .completion()
                .is_some()
        );
        let responses: i64 = database.query_row(
            "SELECT COUNT(*) FROM owner_core_log WHERE session_id='session' AND json_extract(record,'$.record')='command_responded'",
            [], |row| row.get(0),
        )?;
        assert_eq!(responses, 0);
        Ok(())
    }
}
