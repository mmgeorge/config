//! Commit owner admission and outcomes beside the domain records they authorize.

use std::path::Path;

use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::{Deserialize, Serialize};

use super::command::{
    Command, CommandFailure, CommandId, CommandOutcome, CommandRequest, CommandResult, InputChange,
    InputCommand, InputId, InputRevision,
};
use super::core_log::{CoreCommandKind, CoreOutcome, CoreRecord, CoreTransitionKind};

/// Retain one command's durable progress without process-local resolver handles.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub(crate) enum CommandState {
    /// Admission is durable; no effect has started.
    Queued { revision: u64 },
    /// The owner started the command and retains its asynchronous stage.
    Active {
        revision: u64,
        /// Task state already committed while journaled input preparation remains outstanding.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        task_commit: Option<super::command::TaskCommandCheckpoint>,
    },
    /// The result or typed failure committed before notifying observers.
    Finished {
        revision: u64,
        outcome: Result<CommandOutcome, CommandFailure>,
    },
}

impl CommandState {
    /// Get the persisted record revision used to correlate repeated observations.
    pub(crate) fn revision(&self) -> u64 {
        match self {
            Self::Queued { revision }
            | Self::Active { revision, .. }
            | Self::Finished { revision, .. } => *revision,
        }
    }
}

/// Preserve immutable intent and FIFO identity separately from command progress.
#[derive(Clone, Debug)]
pub(crate) struct CommandRecord {
    /// Request whose normalized payload defines idempotency.
    pub(crate) request: CommandRequest,
    /// Session-local admission order retained across host restarts.
    pub(crate) sequence: u64,
    /// Durable progress and result.
    pub(crate) state: CommandState,
}

impl CommandRecord {
    /// Read terminal progress without manufacturing a completion from admission.
    pub(crate) fn completion(&self) -> Option<Result<CommandResult, CommandFailure>> {
        let CommandState::Finished { revision, outcome } = &self.state else {
            return None;
        };
        Some(outcome.clone().map(|outcome| CommandResult {
            id: self.request.id.clone(),
            sequence: self.sequence,
            revision: *revision,
            outcome,
        }))
    }
}

/// Distinguish durable identity conflicts from storage and schema failures.
#[derive(Debug, thiserror::Error)]
pub(crate) enum CommandJournalError {
    #[error("owner command database could not be opened")]
    Database(#[from] crate::storage::database::DatabaseError),
    /// Exchange state could not commit the active command's postcondition.
    #[error("owner domain state could not be committed")]
    DomainCommit,
    /// SQLite could not establish the requested transaction.
    #[error("owner command storage failed: {0}")]
    Storage(#[from] rusqlite::Error),
    /// A stored or outgoing record violates the typed command representation.
    #[error("owner command record could not be decoded: {0}")]
    Codec(#[from] serde_json::Error),
    /// The same identity was reused for a different immutable request.
    #[error("command identity was reused for a different intent")]
    IdentityConflict,
    /// A command or record revision exceeded its storage representation.
    #[error("owner command sequence or revision is exhausted")]
    RevisionExhausted,
    /// Input effects attempted to complete a command that was not active.
    #[error("input result has no active owner command")]
    NotActive,
    /// Session routing requires a nonempty identity.
    #[error("owner command session identity is empty")]
    InvalidSession,
    /// The input transaction failed before a durable owner result was established.
    #[error("input state and command completion could not be committed")]
    InputCommit,
    /// The metadata-only connection owner lost its synchronization invariant.
    #[error("owner core evidence storage is unavailable")]
    CoreUnavailable,
}

/// Own the session's admission journal; domain commits can use the same SQL tables.
pub(crate) struct CommandJournal {
    connection: crate::storage::database::HistoryConnection,
    session: String,
}

impl CommandJournal {
    /// Open an already-bootstrapped shared database without starting provider work.
    pub(crate) fn open(root: &Path, session: &str) -> Result<Self, CommandJournalError> {
        if session.trim().is_empty() {
            return Err(CommandJournalError::InvalidSession);
        }
        let connection = crate::storage::database::HistoryConnection::open(root)?;
        Ok(Self {
            connection,
            session: session.into(),
        })
    }

    /// Open metadata-only ownership with foreign-key enforcement against concurrent session deletion.
    pub(crate) fn open_metadata(root: &Path, session: &str) -> Result<Self, CommandJournalError> {
        let journal = Self::open(root, session)?;
        journal
            .connection
            .pragma_update(None, "foreign_keys", "ON")?;
        Ok(journal)
    }

    /// Append required lifecycle evidence through this owner's existing durable connection.
    /// Failure is observable by the owner; no optional trace queue or filesystem writer is involved.
    pub(crate) fn append_core(&mut self, record: &CoreRecord) -> Result<u64, CommandJournalError> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let sequence = append_core_record(&transaction, &self.session, record)?;
        transaction.commit()?;
        Ok(sequence)
    }

    /// Check whether this owner has durable host history without allocating a hidden session.
    pub(crate) fn has_durable_session(&self) -> Result<bool, CommandJournalError> {
        Ok(self.connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM session_record WHERE id=?1)",
            [&self.session],
            |row| row.get(0),
        )?)
    }

    /// Recover unfinished intent as an explicit failure, never as implicit native replay.
    pub(crate) fn recover(&mut self) -> Result<(), CommandJournalError> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let records = {
            let mut query = transaction.prepare(
                "SELECT id,state,request FROM owner_command WHERE session_id=?1 AND json_extract(state,'$.state') IN ('queued','active') ORDER BY sequence",
            )?;
            query
                .query_map([&self.session], |row| {
                    Ok((
                        row.get::<_, String>(0)?,
                        row.get::<_, String>(1)?,
                        row.get::<_, String>(2)?,
                    ))
                })?
                .collect::<Result<Vec<_>, _>>()?
        };
        for (id, state, request) in records {
            let previous: CommandState = serde_json::from_str(&state)?;
            let was_active = matches!(&previous, CommandState::Active { .. });
            let next = CommandState::Finished {
                revision: previous
                    .revision()
                    .checked_add(1)
                    .ok_or(CommandJournalError::RevisionExhausted)?,
                outcome: Err(match previous {
                    CommandState::Active {
                        task_commit: Some(committed),
                        ..
                    } => CommandFailure::TaskSynchronization {
                        committed,
                        failure: super::command::TaskSynchronizationFailure::Interrupted,
                    },
                    CommandState::Active {
                        task_commit: None, ..
                    }
                    | CommandState::Queued { .. }
                    | CommandState::Finished { .. } => CommandFailure::Interrupted,
                }),
            };
            transaction.execute(
                "UPDATE owner_command SET state=?3 WHERE session_id=?1 AND id=?2",
                params![self.session, id, serde_json::to_string(&next)?],
            )?;
            let CommandState::Finished { revision, outcome } = &next else {
                unreachable!("restart recovery always records a finished command")
            };
            append_core_record(
                &transaction,
                &self.session,
                &CoreRecord::CommandFinished {
                    command: CommandId::new(id.clone())
                        .map_err(|_| CommandJournalError::IdentityConflict)?,
                    revision: *revision,
                    outcome: CoreOutcome::command(outcome),
                },
            )?;
            if was_active {
                let request: CommandRequest = serde_json::from_str(&request)?;
                append_core_record(
                    &transaction,
                    &self.session,
                    &CoreRecord::OperationCompleted {
                        operation: super::core_log::CoreOperation::Command {
                            command: request.id,
                            kind: CoreCommandKind::from(&request.command),
                        },
                        outcome: CoreOutcome::Interrupted,
                    },
                )?;
            }
        }
        transaction.commit()?;
        Ok(())
    }

    /// Read the authoritative result after a dropped waiter or transport reconnect.
    pub(crate) fn lookup(
        &self,
        id: &CommandId,
    ) -> Result<Option<CommandRecord>, CommandJournalError> {
        read_record(&self.connection, &self.session, id.as_str())
    }

    /// Find a failed creation checkpoint for this exact task input, including interruption on host restart.
    pub(super) fn failed_task_input(
        &self,
        input: &crate::task::TaskInputId,
    ) -> Result<Option<CommandRecord>, CommandJournalError> {
        let id: Option<String> = self.connection.query_row(
            "SELECT id FROM owner_command WHERE session_id=?1 AND json_extract(state,'$.state')='finished' AND json_extract(state,'$.outcome.Err.failure')='task_synchronization' AND json_extract(state,'$.outcome.Err.reason.committed.stage')='input_pending' AND json_extract(state,'$.outcome.Err.reason.committed.input')=?2 ORDER BY sequence DESC LIMIT 1",
            params![self.session, input.as_str()], |row| row.get(0),
        ).optional()?;
        id.map(|id| read_record(&self.connection, &self.session, &id))
            .transpose()
            .map(Option::flatten)
    }

    /// Commit a FIFO admission or retrieve the identical already-admitted request.
    pub(crate) fn admit(
        &mut self,
        request: &CommandRequest,
    ) -> Result<CommandRecord, CommandJournalError> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        if let Some(previous) = read_record(&transaction, &self.session, request.id.as_str())? {
            if serde_json::to_vec(&previous.request)? != serde_json::to_vec(request)? {
                return Err(CommandJournalError::IdentityConflict);
            }
            return Ok(previous);
        }
        let previous: i64 = transaction.query_row(
            "SELECT COALESCE(MAX(sequence),0) FROM owner_command WHERE session_id=?1",
            [&self.session],
            |row| row.get(0),
        )?;
        let sequence = previous
            .checked_add(1)
            .ok_or(CommandJournalError::RevisionExhausted)?;
        let state = CommandState::Queued { revision: 1 };
        transaction.execute("INSERT INTO owner_command(session_id,id,sequence,request,state) VALUES(?1,?2,?3,?4,?5)",
            params![self.session,request.id.as_str(),sequence,serde_json::to_string(request)?,serde_json::to_string(&state)?])?;
        append_core_record(
            &transaction,
            &self.session,
            &CoreRecord::CommandAdmitted {
                command: request.id.clone(),
                kind: CoreCommandKind::from(&request.command),
            },
        )?;
        transaction.commit()?;
        Ok(CommandRecord {
            request: request.clone(),
            sequence: u64::try_from(sequence)
                .map_err(|_| CommandJournalError::RevisionExhausted)?,
            state,
        })
    }

    /// Commit active status before launching the command's first effect.
    pub(crate) fn start(
        &mut self,
        record: &CommandRecord,
    ) -> Result<CommandRecord, CommandJournalError> {
        if !matches!(record.state, CommandState::Queued { .. }) {
            return Err(CommandJournalError::NotActive);
        }
        let revision = record
            .state
            .revision()
            .checked_add(1)
            .ok_or(CommandJournalError::RevisionExhausted)?;
        let mut next = record.clone();
        next.state = CommandState::Active {
            revision,
            task_commit: None,
        };
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        transaction.execute(
            "UPDATE owner_command SET state=?3 WHERE session_id=?1 AND id=?2",
            params![
                self.session,
                record.request.id.as_str(),
                serde_json::to_string(&next.state)?
            ],
        )?;
        append_core_record(
            &transaction,
            &self.session,
            &CoreRecord::CommandStarted {
                command: record.request.id.clone(),
                revision,
            },
        )?;
        transaction.commit()?;
        Ok(next)
    }

    /// Commit a result for work whose domain effect was already durable or never started.
    pub(crate) fn finish(
        &mut self,
        id: &CommandId,
        outcome: Result<CommandOutcome, CommandFailure>,
    ) -> Result<CommandRecord, CommandJournalError> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut record = read_record(&transaction, &self.session, id.as_str())?
            .ok_or(CommandJournalError::NotActive)?;
        if record.completion().is_some() {
            return Ok(record);
        }
        finish_record(&transaction, &self.session, &mut record, outcome)?;
        transaction.commit()?;
        Ok(record)
    }
}

/// Preserve committed task evidence before resolving its command or scheduling ordinary input.
pub(crate) enum TaskCommandCommit {
    /// Reserve one workflow input and its command checkpoint in the domain transaction.
    WorkflowInput {
        command: CommandId,
        task: Option<super::command::TaskSelection>,
        message: Box<crate::input::InputMessage>,
    },
    /// Bind attachment to its captured source; a current task can complete without a lifecycle transition.
    Attachment {
        command: CommandId,
        task: super::command::TaskSelection,
        source: super::command::PlanSelection,
    },
    /// Local policy mutation and command completion are one atomic commit.
    Complete {
        command: CommandId,
        outcome: super::command::TaskCommandOutcome,
    },
    /// Task installation committed before its original input could be dispatched.
    AwaitInput {
        command: CommandId,
        task: super::command::TaskSelection,
        input: crate::task::TaskInputId,
        kind: crate::task::TaskKind,
        source: Option<super::command::PlanSourceGuard>,
    },
}

/// Couple a task mutation to its result or input checkpoint in the same SQL transaction.
pub(crate) fn commit_task_command(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    commit: TaskCommandCommit,
) -> Result<(), CommandJournalError> {
    if let TaskCommandCommit::WorkflowInput {
        command,
        task,
        message,
    } = &commit
    {
        let record = read_record(transaction, session, command.as_str())?
            .ok_or(CommandJournalError::NotActive)?;
        if !matches!(
            record.state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ) {
            return Err(CommandJournalError::NotActive);
        }
        if !matches!(&record.request.command, Command::ContinueWorkflow(_)) {
            return Err(CommandJournalError::IdentityConflict);
        }
        crate::input::reserve_workflow_input(transaction, session, message)
            .map_err(|_| CommandJournalError::InputCommit)?;
        return checkpoint_task_command(
            transaction,
            session,
            &record,
            super::command::TaskCommandCheckpoint::WorkflowInputPending {
                task: task.clone(),
                input: super::command::InputId::new(message.id.clone())
                    .map_err(|_| CommandJournalError::IdentityConflict)?,
            },
        );
    }
    if let TaskCommandCommit::Attachment {
        command,
        task,
        source,
    } = &commit
    {
        let mut record = read_record(transaction, session, command.as_str())?
            .ok_or(CommandJournalError::NotActive)?;
        if !matches!(
            record.state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ) {
            return Err(CommandJournalError::NotActive);
        }
        let Command::Task(super::command::TaskCommand::AttachPlan(expected)) =
            &record.request.command
        else {
            return Err(CommandJournalError::IdentityConflict);
        };
        if expected != source {
            return Err(CommandJournalError::IdentityConflict);
        }
        let outcome = super::command::TaskCommandOutcome::Attached {
            task: task.clone(),
            plan: source.id().clone(),
        };
        return finish_record(
            transaction,
            session,
            &mut record,
            Ok(CommandOutcome::Task(outcome)),
        );
    }
    if let TaskCommandCommit::AwaitInput {
        command,
        task,
        input,
        kind,
        source,
    } = &commit
    {
        let record = read_record(transaction, session, command.as_str())?
            .ok_or(CommandJournalError::NotActive)?;
        if !matches!(
            record.state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ) {
            return Err(CommandJournalError::NotActive);
        }
        let accepted = match &record.request.command {
            Command::Task(intent) => intent.accepts_installation(*kind, source.as_ref()),
            Command::ContinueWorkflow(request) => request.execution().is_some_and(|selection| {
                matches!(
                    kind,
                    crate::task::TaskKind::Plan | crate::task::TaskKind::Execute
                ) && source.as_ref()
                    == Some(&super::command::PlanSourceGuard::Working(
                        selection.source().clone(),
                    ))
            }),
            _ => false,
        };
        if !accepted {
            return Err(CommandJournalError::IdentityConflict);
        }
        return checkpoint_task_command(
            transaction,
            session,
            &record,
            super::command::TaskCommandCheckpoint::InputPending {
                task: task.clone(),
                input: input.clone(),
            },
        );
    }
    let (id, outcome) = match commit {
        TaskCommandCommit::Complete { command, outcome } => (command, outcome),
        TaskCommandCommit::AwaitInput { .. } => {
            unreachable!("input checkpoints were committed above")
        }
        TaskCommandCommit::Attachment { .. } => {
            unreachable!("attachment authority was committed above")
        }
        TaskCommandCommit::WorkflowInput { .. } => {
            unreachable!("workflow reservation was committed above")
        }
    };
    let mut record =
        read_record(transaction, session, id.as_str())?.ok_or(CommandJournalError::NotActive)?;
    if !matches!(
        record.state,
        CommandState::Active {
            task_commit: None,
            ..
        }
    ) {
        return Err(CommandJournalError::NotActive);
    }
    let accepted = match &record.request.command {
        Command::Task(intent) => intent.accepts_outcome(&outcome),
        Command::ContinueWorkflow(request) => match &outcome {
            super::command::TaskCommandOutcome::AwaitingInput { source, .. }
            | super::command::TaskCommandOutcome::ExecutionUpdated { source, .. } => request
                .execution()
                .is_some_and(|selection| selection.source().id() == source.id()),
            super::command::TaskCommandOutcome::Continued { .. }
            | super::command::TaskCommandOutcome::ContinuationReserved { .. }
            | super::command::TaskCommandOutcome::WorkflowAwaitingInput { .. } => true,
            _ => false,
        },
        _ => false,
    };
    if !accepted {
        return Err(CommandJournalError::IdentityConflict);
    }
    finish_record(
        transaction,
        session,
        &mut record,
        Ok(CommandOutcome::Task(outcome)),
    )
}

/// Persist an active task checkpoint without changing its immutable request or completing native work.
fn checkpoint_task_command(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    record: &CommandRecord,
    checkpoint: super::command::TaskCommandCheckpoint,
) -> Result<(), CommandJournalError> {
    let revision = record
        .state
        .revision()
        .checked_add(1)
        .ok_or(CommandJournalError::RevisionExhausted)?;
    let state = CommandState::Active {
        revision,
        task_commit: Some(checkpoint),
    };
    transaction.execute(
        "UPDATE owner_command SET state=?3 WHERE session_id=?1 AND id=?2",
        params![
            session,
            record.request.id.as_str(),
            serde_json::to_string(&state)?
        ],
    )?;
    append_core_record(
        transaction,
        session,
        &CoreRecord::TransitionCommitted {
            command: Some(record.request.id.clone()),
            invocation: None,
            kind: CoreTransitionKind::TaskCommitted,
            outcome: CoreOutcome::Pending,
        },
    )?;
    Ok(())
}

/// Couple input-state/receipt persistence to its owner-command result in one transaction.
/// Host-generated input without a caller command keeps its existing domain receipt.
pub(crate) fn commit_input_command(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    id: &str,
    update: &crate::input::InputUpdate,
) -> Result<(), CommandJournalError> {
    let Some(mut record) = read_record(transaction, session, id)? else {
        return Ok(());
    };
    if !matches!(record.state, CommandState::Active { .. }) {
        return Err(CommandJournalError::NotActive);
    }
    if !matches!(record.request.command, Command::Input(_)) {
        return Err(CommandJournalError::IdentityConflict);
    }
    let outcome = input_outcome(&record.request, update);
    finish_record(transaction, session, &mut record, outcome)
}

/// Normalize legacy input receipts without parsing diagnostic strings.
pub(crate) fn input_outcome(
    request: &CommandRequest,
    update: &crate::input::InputUpdate,
) -> Result<CommandOutcome, CommandFailure> {
    if let Some(error) = update.error_code {
        return Err(CommandFailure::Input(error));
    }
    if update.error.is_some() {
        return Err(CommandFailure::LegacyInputRejected);
    }
    let command = match &request.command {
        Command::Input(command) => command,
        Command::Agent(_)
        | Command::ContinueWorkflow(_)
        | Command::Rollback(_)
        | Command::Task(_)
        | Command::CancelExchange(_)
        | Command::CompactContext(_)
        | Command::ConfigureSession(_) => {
            return Err(CommandFailure::PrerequisiteFailed);
        }
    };
    let changed = |id: &InputId| {
        let message = update
            .upsert
            .iter()
            .find(|message| message.id == id.as_str())
            .expect("a committed input mutation retains the selected input's result revision");
        InputRevision {
            id: id.clone(),
            revision: message.revision,
        }
    };
    let change = match command {
        InputCommand::Submit(input) => InputChange::Submitted {
            input: changed(&input.id),
        },
        InputCommand::Retry(input) if update.removed.iter().any(|id| id == input.id.as_str()) => {
            InputChange::DeliveryReconciled {
                input: input.id.clone(),
            }
        }
        InputCommand::Retry(input) => InputChange::RetryEligible {
            input: changed(&input.id),
        },
        InputCommand::ResumeDelivery => InputChange::DeliveryResumed,
        InputCommand::BeginEdit(request) => InputChange::EditBegun {
            input: changed(&request.input.id),
            edit: request.edit.clone(),
        },
        InputCommand::CommitEdit(request) => InputChange::Edited {
            inputs: request
                .edits
                .iter()
                .map(|edit| {
                    if update.removed.iter().any(|id| id == edit.input.id.as_str()) {
                        super::command::InputEditResult::Removed(edit.input.id.clone())
                    } else {
                        super::command::InputEditResult::Updated(changed(&edit.input.id))
                    }
                })
                .collect(),
        },
        InputCommand::CancelEdit(edit) => InputChange::EditCancelled { edit: edit.clone() },
        InputCommand::Remove(input) => InputChange::Removed {
            input: input.id.clone(),
        },
    };
    Ok(CommandOutcome::Input {
        queue_revision: update.revision,
        change,
    })
}

/// Read one typed record from a connection or its active transaction.
fn read_record(
    connection: &Connection,
    session: &str,
    id: &str,
) -> Result<Option<CommandRecord>, CommandJournalError> {
    let row: Option<(i64, String, String)> = connection
        .query_row(
            "SELECT sequence,request,state FROM owner_command WHERE session_id=?1 AND id=?2",
            params![session, id],
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        )
        .optional()?;
    row.map(|(sequence, request, state)| {
        Ok(CommandRecord {
            sequence: u64::try_from(sequence)
                .map_err(|_| CommandJournalError::RevisionExhausted)?,
            request: serde_json::from_str(&request)?,
            state: serde_json::from_str(&state)?,
        })
    })
    .transpose()
}

/// Persist terminal progress before publishing any result or consuming its resolver.
fn finish_record(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    record: &mut CommandRecord,
    outcome: Result<CommandOutcome, CommandFailure>,
) -> Result<(), CommandJournalError> {
    let revision = record
        .state
        .revision()
        .checked_add(1)
        .ok_or(CommandJournalError::RevisionExhausted)?;
    let state = CommandState::Finished { revision, outcome };
    transaction.execute(
        "UPDATE owner_command SET state=?3 WHERE session_id=?1 AND id=?2",
        params![
            session,
            record.request.id.as_str(),
            serde_json::to_string(&state)?
        ],
    )?;
    let CommandState::Finished { revision, outcome } = &state else {
        unreachable!("command completion always constructs a finished state")
    };
    append_core_record(
        transaction,
        session,
        &CoreRecord::CommandFinished {
            command: record.request.id.clone(),
            revision: *revision,
            outcome: CoreOutcome::command(outcome),
        },
    )?;
    if matches!(record.state, CommandState::Active { .. }) {
        append_core_record(
            transaction,
            session,
            &CoreRecord::OperationCompleted {
                operation: super::core_log::CoreOperation::Command {
                    command: record.request.id.clone(),
                    kind: CoreCommandKind::from(&record.request.command),
                },
                outcome: CoreOutcome::command(outcome),
            },
        )?;
    }
    record.state = state;
    Ok(())
}

/// Append one safe core fact atomically with any domain mutation on this transaction.
fn append_core_record(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    record: &CoreRecord,
) -> Result<u64, CommandJournalError> {
    let encoded = serde_json::to_string(record)?;
    let previous: i64 = transaction.query_row(
        "SELECT COALESCE(MAX(sequence),0) FROM owner_core_log WHERE session_id=?1",
        [session],
        |row| row.get(0),
    )?;
    let sequence = previous
        .checked_add(1)
        .ok_or(CommandJournalError::RevisionExhausted)?;
    transaction.execute(
        "INSERT INTO owner_core_log(session_id,sequence,record) VALUES(?1,?2,?3)",
        params![session, sequence, encoded],
    )?;
    u64::try_from(sequence).map_err(|_| CommandJournalError::RevisionExhausted)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::{
        InputAction, InputIntent, InputMessage, InputOperation, InputPurpose, SessionInputQueue,
    };

    /// Read only the metadata journal so assertions cannot accidentally inspect the command payload table.
    fn core_records(journal: &CommandJournal) -> anyhow::Result<Vec<(i64, String)>> {
        let mut statement = journal.connection.prepare(
            "SELECT sequence,record FROM owner_core_log WHERE session_id=?1 ORDER BY sequence",
        )?;
        Ok(statement
            .query_map([&journal.session], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<Result<Vec<_>, _>>()?)
    }

    /// Required evidence survives reopen and disabled/cleared optional traces without retaining user content.
    #[test]
    fn core_evidence_survives_disabled_trace_and_reopen_without_payloads() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let _input = input_queue(root.path())?;
        let trace = crate::trace::TraceStore::open(root.path())?;
        trace.configure(false)?;
        let request = CommandRequest::from_input(operation())?;
        {
            let mut journal = CommandJournal::open(root.path(), "session")?;
            let admitted = journal.admit(&request)?;
            journal.start(&admitted)?;
            journal.finish(&request.id, Err(CommandFailure::Interrupted))?;
            let event = forge_provider_api::BoundProviderEvent {
                binding_id: forge_provider_api::BindingId::new("binding")?,
                sequence: forge_provider_api::EventSequence::new(41),
                event: forge_provider_api::ProviderEvent::Provider {
                    event: forge_provider_api::ProviderConnectionEvent::Warning(
                        forge_provider_api::ProviderWarning {
                            scope: forge_provider_api::WarningScope::Thread {
                                thread_id: crate::provider::ThreadOwner::root("owner").thread_id(),
                            },
                            kind: forge_provider_api::ProviderWarningKind::NativeNotice {
                                category: forge_provider_api::ProviderFailureKind::Transport,
                            },
                            message: forge_provider_api::SanitizedMessage::new(
                                "private diagnostic content",
                            )?,
                        },
                    ),
                },
            };
            assert_eq!(
                journal.append_core(&CoreRecord::provider_event(
                    forge_provider_api::EventScope::Binding {
                        thread_id: forge_provider_api::ThreadId::from(
                            crate::provider::ThreadOwner::root("owner")
                        )
                    },
                    &event
                ))?,
                5
            );
            assert_eq!(
                journal.append_core(&CoreRecord::CommandResponded {
                    command: request.id.clone(),
                    outcome: CoreOutcome::Interrupted,
                })?,
                6
            );
        }
        trace.clear_session("session")?;
        let journal = CommandJournal::open(root.path(), "session")?;
        let records = core_records(&journal)?;
        assert_eq!(
            records
                .iter()
                .map(|(sequence, _)| *sequence)
                .collect::<Vec<_>>(),
            [1, 2, 3, 4, 5, 6]
        );
        for (_, encoded) in &records {
            let _: CoreRecord = serde_json::from_str(encoded)?;
            assert!(!encoded.contains("retained input"));
            assert!(!encoded.contains("private diagnostic content"));
        }
        assert!(matches!(serde_json::from_str::<CoreRecord>(&records[4].1)?,
            CoreRecord::ProviderEventConsumed { sequence, kind: super::super::core_log::CoreProviderEventKind::Provider, .. }
                if sequence.get() == 41));
        Ok(())
    }

    /// Required core persistence shares command transactions and cannot leave admitted or finished state ahead of evidence.
    #[test]
    fn core_write_failure_rolls_back_command_admission_start_and_completion() -> anyhow::Result<()>
    {
        let root = tempfile::tempdir()?;
        let _input = input_queue(root.path())?;
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let request = CommandRequest::from_input(operation())?;
        let reject = "CREATE TRIGGER reject_core BEFORE INSERT ON owner_core_log BEGIN SELECT RAISE(ABORT,'fixture core storage failure'); END;";
        journal.connection.execute_batch(reject)?;
        assert!(journal.admit(&request).is_err());
        assert!(journal.lookup(&request.id)?.is_none());
        assert!(core_records(&journal)?.is_empty());
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_core")?;
        let queued = journal.admit(&request)?;
        journal.connection.execute_batch(reject)?;
        assert!(journal.start(&queued).is_err());
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Queued { revision: 1 }
        ));
        assert_eq!(core_records(&journal)?.len(), 1);
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_core")?;
        journal.start(&queued)?;
        journal.connection.execute_batch(reject)?;
        assert!(
            journal
                .finish(&request.id, Err(CommandFailure::Interrupted))
                .is_err()
        );
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active { revision: 2, .. }
        ));
        assert_eq!(core_records(&journal)?.len(), 2);
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_core")?;
        journal.finish(&request.id, Err(CommandFailure::Interrupted))?;
        assert_eq!(core_records(&journal)?.len(), 4);
        Ok(())
    }

    /// Roll back task policy if its owner result cannot commit, then publish both together.
    #[test]
    fn task_policy_and_command_completion_share_one_transaction() -> anyhow::Result<()> {
        use super::super::command::{
            InputControl, PlanAcceptancePolicy, TaskCommand, TaskCommandOutcome, TaskSelection,
        };
        let (root, mut store, tasks) = crate::task::SessionTaskController::test_goal();
        let previous = tasks.current()?.unwrap();
        let plan: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"owner-policy-plan","session_id":"session","request":"Plan a change",
            "state":"configuring_checks","working_path":"working.json","model_revision":0,
            "user_revision":0,"created_at_ms":1,"updated_at_ms":1,
        }))?;
        let task = tasks
            .change(
                &mut store,
                Some(&previous.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan, 1)?)),
                1,
            )?
            .unwrap();
        let intent = TaskCommand::SetAutoAccept(PlanAcceptancePolicy {
            target: TaskSelection::capture(&task),
            enabled: true,
        });
        let request = CommandRequest {
            id: CommandId::new("atomic-task-policy")?,
            command: Command::Task(intent.clone()),
            input_control: InputControl::AtExecution,
        };
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        journal.connection.execute_batch("CREATE TRIGGER reject_task_owner_result BEFORE UPDATE ON owner_command WHEN json_extract(NEW.state,'$.state')='finished' BEGIN SELECT RAISE(ABORT,'fixture result failure'); END;")?;
        assert!(matches!(
            tasks.apply_owner_command(&mut store, &request.id, &intent, &task.permit(), 2),
            Err(super::super::command::TaskCommandFailure::StateUnavailable)
        ));
        assert!(!tasks.current()?.unwrap().auto_accept_plan);
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ));
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_task_owner_result")?;
        tasks.apply_owner_command(&mut store, &request.id, &intent, &task.permit(), 2)?;
        assert!(tasks.current()?.unwrap().auto_accept_plan);
        assert!(matches!(
            journal
                .lookup(&request.id)?
                .unwrap()
                .completion()
                .unwrap()?
                .outcome,
            CommandOutcome::Task(TaskCommandOutcome::AutoAcceptChanged { enabled: true, .. })
        ));
        Ok(())
    }

    /// Recover a committed pause receipt without replaying its task mutation.
    #[test]
    fn task_recovery_preserves_committed_pause_receipt() -> anyhow::Result<()> {
        use super::super::command::{InputControl, TaskCommand, TaskCommandOutcome, TaskSelection};
        let (root, mut store, tasks) = crate::task::SessionTaskController::test_goal();
        let task = tasks.current()?.unwrap();
        let intent = TaskCommand::Pause(TaskSelection::capture(&task));
        let request = CommandRequest {
            id: CommandId::new("interrupted-task-pause")?,
            command: Command::Task(intent.clone()),
            input_control: InputControl::AtExecution,
        };
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        tasks.apply_owner_command(&mut store, &request.id, &intent, &task.permit(), 2)?;
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Finished {
                outcome: Ok(CommandOutcome::Task(TaskCommandOutcome::Paused { .. })),
                ..
            }
        ));
        let paused = tasks.current()?.unwrap();
        assert_eq!(paused.status(), crate::task::TaskStatus::Paused);
        drop(journal);
        let mut recovered = CommandJournal::open(root.path(), "session")?;
        recovered.recover()?;
        let record = recovered.admit(&request)?;
        assert!(matches!(
            record.completion().unwrap(),
            Ok(CommandResult {
                outcome: CommandOutcome::Task(TaskCommandOutcome::Paused { .. }),
                ..
            })
        ));
        assert_eq!(tasks.current()?.unwrap().revision, paused.revision);
        Ok(())
    }

    /// Checkpoint creation atomically, then recover its original input without granting dispatch or replay.
    #[test]
    fn task_creation_recovery_preserves_input_and_rolls_back_failed_checkpoint()
    -> anyhow::Result<()> {
        use super::super::command::{
            InputControl, NewGoal, TaskCommand, TaskCommandCheckpoint, TaskSynchronizationFailure,
        };
        let (root, mut store, tasks) = crate::task::SessionTaskController::test_goal();
        let original = tasks.current()?.unwrap();
        let mut replacement = crate::task::Task::goal("Replace once".into(), Default::default(), 2);
        replacement.request_entry("Begin original task input".into(), 2)?;
        let input = replacement.entry.as_ref().unwrap().id.clone();
        let request = CommandRequest {
            id: CommandId::new("atomic-goal-creation")?,
            command: Command::Task(TaskCommand::StartGoal(NewGoal::new("Replace once".into())?)),
            input_control: InputControl::AtExecution,
        };
        let prepared =
            || crate::task::TaskInstallation::new(replacement.clone(), Default::default());
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        journal.connection.execute_batch("CREATE TRIGGER reject_task_checkpoint BEFORE UPDATE ON owner_command WHEN json_extract(NEW.state,'$.task_commit') IS NOT NULL BEGIN SELECT RAISE(ABORT,'fixture checkpoint failure'); END;")?;
        assert!(
            tasks
                .apply_owner_creation(
                    &mut store,
                    &request.id,
                    Some(&original.permit()),
                    prepared()?,
                    2
                )
                .is_err()
        );
        assert_eq!(tasks.current()?.unwrap().revision, original.revision);
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ));
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_task_checkpoint")?;
        tasks.apply_owner_creation(
            &mut store,
            &request.id,
            Some(&original.permit()),
            prepared()?,
            2,
        )?;
        let committed = tasks.current()?.unwrap();
        assert_eq!(committed.entry.as_ref().unwrap().id, input);
        assert!(matches!(journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active { task_commit: Some(TaskCommandCheckpoint::InputPending { input: selected, .. }), .. } if selected == input));
        drop(journal);
        let mut recovered = CommandJournal::open(root.path(), "session")?;
        recovered.recover()?;
        let record = recovered.admit(&request)?;
        assert!(matches!(record.completion().unwrap(),
            Err(CommandFailure::TaskSynchronization {
                committed: TaskCommandCheckpoint::InputPending { input: selected, .. },
                failure: TaskSynchronizationFailure::Interrupted,
            }) if selected == input));
        assert_eq!(tasks.current()?.unwrap().revision, committed.revision);
        assert_eq!(
            recovered.failed_task_input(&input)?.unwrap().request.id,
            request.id
        );
        assert_eq!(
            tasks.current()?.unwrap().entry.as_ref().unwrap().delivery,
            crate::task::TaskInputDelivery::Pending
        );
        Ok(())
    }

    /// A source edit or substituted guard cannot commit a branch under the original queued request.
    #[test]
    fn plan_source_guard_and_command_authority_share_the_installation_transaction()
    -> anyhow::Result<()> {
        use super::super::command::{
            InputControl, PlanSelection, PlanSourceGuard, TaskCommand, TaskCommandFailure,
        };
        let (root, mut store, tasks) = crate::task::SessionTaskController::test_goal();
        let original = tasks.current()?.unwrap();
        let mut source: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"source-plan","session_id":"session","request":"Source","state":"generating",
            "working_path":"source.json","document_version":1,"model_revision":0,"user_revision":0,
            "created_at_ms":1,"updated_at_ms":1,
        }))?;
        store.save_plan(&source)?;
        let selected = PlanSelection::capture(&source)?;
        let mut branch = source.clone();
        branch.id = "branch-plan".into();
        let mut task = crate::task::Task::plan(branch, 2)?;
        task.request_entry("Fork the selected design".into(), 2)?;
        let prepare = |selection: PlanSelection| {
            let effects = crate::storage::TaskEffects {
                source_plan: vec![PlanSourceGuard::Working(selection)],
                ..Default::default()
            };
            crate::task::TaskInstallation::new(task.clone(), effects)
        };
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let request = CommandRequest {
            id: CommandId::new("fork-captured-source")?,
            command: Command::Task(TaskCommand::ForkPlan(selected.clone())),
            input_control: InputControl::AtExecution,
        };
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        source.document_version = 2;
        store.save_plan(&source)?;
        assert!(matches!(
            tasks.apply_owner_creation(
                &mut store,
                &request.id,
                Some(&original.permit()),
                prepare(selected)?,
                3
            ),
            Err(TaskCommandFailure::PlanChanged)
        ));
        assert_eq!(tasks.current()?.unwrap().revision, original.revision);
        assert!(store.load_plan("branch-plan")?.is_none());
        let fresh = PlanSelection::capture(&source)?;
        assert!(matches!(
            tasks.apply_owner_creation(
                &mut store,
                &request.id,
                Some(&original.permit()),
                prepare(fresh.clone())?,
                3
            ),
            Err(TaskCommandFailure::IdentityConflict)
        ));
        assert_eq!(tasks.current()?.unwrap().revision, original.revision);
        assert!(store.load_plan("branch-plan")?.is_none());
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active {
                task_commit: None,
                ..
            }
        ));
        let fresh_request = CommandRequest {
            id: CommandId::new("fork-fresh-source")?,
            command: Command::Task(TaskCommand::ForkPlan(fresh.clone())),
            input_control: InputControl::AtExecution,
        };
        let admitted = journal.admit(&fresh_request)?;
        journal.start(&admitted)?;
        tasks.apply_owner_creation(
            &mut store,
            &fresh_request.id,
            Some(&original.permit()),
            prepare(fresh)?,
            4,
        )?;
        assert!(store.load_plan("branch-plan")?.is_some());
        assert!(matches!(
            journal.lookup(&fresh_request.id)?.unwrap().state,
            CommandState::Active {
                task_commit: Some(
                    super::super::command::TaskCommandCheckpoint::InputPending { .. }
                ),
                ..
            }
        ));
        Ok(())
    }

    /// Create a real accepted-input journal sharing the command database.
    fn input_queue(root: &Path) -> anyhow::Result<SessionInputQueue> {
        super::super::core_log::persist_test_session(root, "session")?;
        let submissions = crate::provider::submission::SubmissionStore::open(
            root,
            std::sync::Arc::new(crate::trace::TraceStore::open(root)?),
        )?;
        SessionInputQueue::open(root, "session", &submissions)
    }

    /// Create caller input without native delivery evidence.
    fn operation() -> InputOperation {
        InputOperation {
            id: "submit-command".into(),
            control_revision: 0,
            after_control: None,
            action: InputAction::Submit {
                message: Box::new(InputMessage::queued(crate::input::InputDraft {
                    id: "input".into(),
                    text: "retained input".into(),
                    purpose: InputPurpose::User,
                    attachments: Vec::new(),
                    config: None,
                    intent: InputIntent::Queue,
                })),
            },
        }
    }

    /// Commit acceptance and completion together; duplicate identity cannot change the request.
    #[test]
    fn input_acceptance_and_command_completion_share_one_commit() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let input = input_queue(root.path())?;
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let operation = operation();
        let request = CommandRequest::from_input(operation.clone())?;
        let admitted = journal.admit(&request)?;
        assert_eq!(admitted.sequence, 1);
        assert!(admitted.completion().is_none());
        let mut conflicting = request.clone();
        let Command::Input(super::super::command::InputCommand::Submit(submit)) =
            &mut conflicting.command
        else {
            panic!("submission fixture changed")
        };
        submit.text.push_str(" changed");
        assert!(matches!(
            journal.admit(&conflicting),
            Err(CommandJournalError::IdentityConflict)
        ));
        journal.start(&admitted)?;
        input.apply(operation)?;
        let record = journal.lookup(&request.id)?.unwrap();
        let completed = record.completion().unwrap().unwrap();
        assert!(matches!(
            completed.outcome,
            CommandOutcome::Input {
                change: InputChange::Submitted {
                    input: InputRevision { revision: 1, .. }
                },
                ..
            }
        ));
        assert_eq!(completed.revision, 3);
        assert_eq!(journal.admit(&request)?.sequence, admitted.sequence);
        Ok(())
    }

    /// Roll back input content and its receipt when command completion cannot persist.
    #[test]
    fn failed_completion_write_preserves_input_and_command_state() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let input = input_queue(root.path())?;
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let operation = operation();
        let request = CommandRequest::from_input(operation.clone())?;
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        journal.connection.execute_batch("CREATE TRIGGER reject_owner_completion BEFORE UPDATE ON owner_command WHEN json_extract(NEW.state,'$.state')='finished' BEGIN SELECT RAISE(ABORT,'fixture completion write failure'); END;")?;
        assert!(input.apply(operation.clone()).is_err());
        assert!(input.snapshot()?.messages.is_empty());
        assert!(input.operation(request.id.as_str())?.is_none());
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active { .. }
        ));
        journal
            .connection
            .execute_batch("DROP TRIGGER reject_owner_completion")?;
        input.apply(operation)?;
        assert!(
            journal
                .lookup(&request.id)?
                .unwrap()
                .completion()
                .unwrap()
                .is_ok()
        );
        Ok(())
    }

    /// Prevent a host input receipt from completing a different command with the same identity.
    #[test]
    fn input_receipt_cannot_complete_a_cancellation_command() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let input = input_queue(root.path())?;
        let mut journal = CommandJournal::open(root.path(), "session")?;
        let operation = operation();
        let request = CommandRequest {
            id: CommandId::new(operation.id.clone())?,
            command: Command::CancelExchange(super::super::command::CancelExchangeRequest {
                input: super::super::command::CancelledInputPolicy::Retain,
            }),
            input_control: super::super::command::InputControl::default(),
        };
        let admitted = journal.admit(&request)?;
        journal.start(&admitted)?;
        assert!(input.apply(operation).is_err());
        assert!(input.snapshot()?.messages.is_empty());
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().state,
            CommandState::Active { .. }
        ));
        Ok(())
    }

    /// Fail interrupted durable intent explicitly instead of replaying effects on restart.
    #[test]
    fn restart_recovery_never_replays_an_active_command() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let _input = input_queue(root.path())?;
        let request = CommandRequest::from_input(operation())?;
        {
            let mut journal = CommandJournal::open(root.path(), "session")?;
            let admitted = journal.admit(&request)?;
            journal.start(&admitted)?;
        }
        let mut journal = CommandJournal::open(root.path(), "session")?;
        journal.recover()?;
        assert!(matches!(
            journal.lookup(&request.id)?.unwrap().completion(),
            Some(Err(CommandFailure::Interrupted))
        ));
        assert!(journal.admit(&request)?.completion().is_some());
        Ok(())
    }
}
