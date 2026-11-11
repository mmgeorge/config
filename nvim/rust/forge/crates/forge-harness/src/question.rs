//! Durable user decisions independent of provider exchanges and workflow phases.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::{Deserialize, Serialize};

const FORMAT_VERSION: u32 = 4;

/// Identify one question without confusing it with an exchange or task.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub(crate) struct QuestionId(String);

impl TryFrom<String> for QuestionId {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self> {
        ensure!(!value.trim().is_empty(), "question identity is empty");
        Ok(Self(value))
    }
}

impl From<QuestionId> for String {
    fn from(value: QuestionId) -> Self {
        value.0
    }
}

impl QuestionId {
    /// Get the transport representation of this identity.
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }

    /// Derive a question identity from its host-owned operation and position.
    fn for_operation(operation_id: &str, index: usize) -> Self {
        Self(format!(
            "{}:{index}",
            crate::plan::digest(operation_id.as_bytes())
        ))
    }
}

/// Specify whether unresolved input blocks execution.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum QuestionRequirement {
    #[default]
    Optional,
    Required,
}

/// Select whether an answer resumes a waiting tool or enters the input journal.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum QuestionDelivery {
    #[default]
    Message,
    ToolResult,
}

/// Describe a choice independently of its user-visible label.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionOption {
    pub(crate) id: String,
    pub(crate) label: String,
    pub(crate) description: String,
    #[serde(default)]
    pub(crate) recommended: bool,
}

/// Describe model input before Harness assigns canonical identities.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionDraft {
    pub(crate) header: String,
    pub(crate) question: String,
    #[serde(default)]
    pub(crate) requirement: QuestionRequirement,
    #[serde(default)]
    pub(crate) options: Vec<crate::plan::PlanQuestionOption>,
}

/// Preserve one immutable version of a question's wording and choices.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionContent {
    pub(crate) revision: u64,
    pub(crate) header: String,
    pub(crate) question: String,
    pub(crate) options: Vec<QuestionOption>,
}

/// Preserve an explicit user answer without treating a recommendation as consent.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum QuestionAnswer {
    Selected {
        option_id: String,
        feedback: Option<String>,
    },
    Other {
        text: String,
    },
}

/// Record why a question no longer needs an answer.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum QuestionResolution {
    Pending,
    Answered {
        answer: QuestionAnswer,
        message_id: String,
    },
    Dismissed,
    Withdrawn {
        reason: String,
    },
    Retired {
        reason: String,
    },
}

/// Distinguish clarification from decisions and later corrections.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum QuestionMessageKind {
    Clarification,
    Reply,
    Answer,
    Correction,
}

/// Preserve an attributed discussion item and its delivery identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionMessage {
    pub(crate) id: String,
    pub(crate) kind: QuestionMessageKind,
    pub(crate) text: String,
    pub(crate) reply_to: Option<String>,
}

/// Retain a question across exchanges while keeping its original task attribution.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionRecord {
    pub(crate) id: QuestionId,
    pub(crate) exchange_id: String,
    pub(crate) task_id: Option<String>,
    pub(crate) phase: Option<crate::task::TaskPhase>,
    pub(crate) requirement: QuestionRequirement,
    /// Answer destination chosen when the question was created.
    #[serde(default)]
    pub(crate) delivery: QuestionDelivery,
    pub(crate) content: QuestionContent,
    pub(crate) previous: Vec<QuestionContent>,
    pub(crate) resolution: QuestionResolution,
    pub(crate) discussion: Vec<QuestionMessage>,
}

impl QuestionRecord {
    /// Check whether this question still needs a decision.
    pub(crate) fn pending(&self) -> bool {
        matches!(self.resolution, QuestionResolution::Pending)
    }
}

/// Publish the authoritative question list independently of transient execution state.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct QuestionSnapshot {
    pub(crate) revision: u64,
    pub(crate) questions: Vec<QuestionRecord>,
}

impl QuestionSnapshot {
    /// Keep unfinished required tool questions attached to their originating exchange.
    pub(crate) fn required_pending_in_exchange(&self, exchange_id: &str) -> bool {
        self.questions.iter().any(|question| {
            question.exchange_id == exchange_id
                && question.requirement == QuestionRequirement::Required
                && question.pending()
        })
    }

    /// Preserve the turn's required-input stop even after a fast answer resolves the gate.
    pub(crate) fn required_in_exchange(&self, exchange_id: &str) -> bool {
        self.questions.iter().any(|question| {
            question.exchange_id == exchange_id
                && question.requirement == QuestionRequirement::Required
                && question.delivery == QuestionDelivery::Message
        })
    }

    /// Check the required-input gate without interpreting optional questions as waits.
    pub(crate) fn required_pending(&self) -> bool {
        self.questions.iter().any(|question| {
            question.pending() && question.requirement == QuestionRequirement::Required
        })
    }
}

/// Address one revision and make duplicate mutations safely repeatable.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionCommand {
    pub(crate) operation_id: String,
    pub(crate) question_id: QuestionId,
    pub(crate) revision: u64,
    pub(crate) action: QuestionAction,
}

/// Limit each mutation to its meaningful payload.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum QuestionAction {
    Answer { answer: QuestionAnswer },
    Clarify { text: String },
    Correct { text: String },
    Dismiss,
    Reply { message_id: String, text: String },
    Revise { draft: QuestionDraft },
    Withdraw { reason: String },
}

/// Retain user input until the independent input journal acknowledges admission.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionOutbox {
    pub(crate) id: String,
    pub(crate) question_id: QuestionId,
    pub(crate) task_id: Option<String>,
    pub(crate) kind: QuestionMessageKind,
    pub(crate) text: String,
    pub(crate) reply: forge_provider_api::QuestionReply,
}

#[derive(Default, Deserialize, Serialize)]
struct QuestionState {
    snapshot: QuestionSnapshot,
    outbox: Vec<QuestionOutbox>,
    operations: BTreeMap<String, String>,
    /// Exact committed mutation responses; old records explicitly have no recoverable result.
    #[serde(default)]
    mutation_results: BTreeMap<String, QuestionSnapshot>,
    /// Exact completed tool-wait answers, keyed by their immutable ordered question identities.
    #[serde(default)]
    wait_results: BTreeMap<String, Vec<QuestionRecord>>,
    chat_continuation: BTreeMap<String, ChatContinuation>,
    cancelled_exchanges: BTreeSet<String>,
    /// Canonical answer/correction messages already returned to a live tool observer.
    #[serde(default)]
    observed_tool_messages: BTreeSet<String>,
}

/// Retain one automatic continuation for an interrupted chat request.
#[derive(Deserialize, Serialize)]
enum ChatContinuation {
    Waiting,
    Admitted,
    Completed,
    Cancelled,
}

/// Distinguish work that can resume from a read-only question discussion.
pub(crate) enum QuestionOrigin<'task> {
    Chat,
    Task(&'task crate::task::Task),
    Discussion(Option<&'task crate::task::Task>),
    Tool(Option<&'task crate::task::Task>),
}

impl QuestionOrigin<'_> {
    /// Get task attribution without granting discussion a continuation.
    fn task(&self) -> Option<&crate::task::Task> {
        match self {
            Self::Chat => None,
            Self::Task(task) => Some(task),
            Self::Discussion(task) | Self::Tool(task) => *task,
        }
    }
}

/// Distinguish immutable creation conflicts from unavailable or incomplete durable receipts.
#[derive(Debug, thiserror::Error)]
pub(crate) enum QuestionCreationReplayError {
    /// The authenticated call identity already carries different creation input.
    #[error("question call identity was reused for different input")]
    IdentityConflict,
    /// Older mutation receipts did not retain their original response snapshot.
    #[error("the legacy question mutation has no retained response snapshot")]
    LegacyResultUnavailable,
    /// The stored creation record no longer has every promised question identity.
    #[error("question creation receipt is incomplete")]
    Incomplete,
    /// The question journal could not be read or encoded.
    #[error("question creation receipt is unavailable")]
    State(#[source] anyhow::Error),
}

/// Serialize question changes in their own transaction, without an execution lock.
pub(crate) struct QuestionController {
    connection: Mutex<crate::storage::database::HistoryConnection>,
    /// Each guard registers once per ID; repeated IDs represent independently owned waits.
    waiting: Mutex<Vec<QuestionId>>,
    session_id: String,
    pub(crate) changed: tokio::sync::watch::Sender<u64>,
}

impl QuestionController {
    /// Create a controller using the session database's durable SQLite policy.
    pub(crate) fn open(root: &Path, session_id: &str) -> Result<Self> {
        let connection = crate::storage::database::HistoryConnection::open(root)?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        connection.execute_batch("CREATE TABLE IF NOT EXISTS session_question_state(session_id TEXT PRIMARY KEY REFERENCES session_record(id) ON DELETE CASCADE,format_version INTEGER NOT NULL,payload TEXT NOT NULL);")?;
        Ok(Self {
            connection: Mutex::new(connection),
            waiting: Mutex::new(Vec::new()),
            session_id: session_id.into(),
            changed: tokio::sync::watch::channel(0).0,
        })
    }

    /// Get current durable questions, including answers and discussion history.
    pub(crate) fn snapshot(&self) -> Result<QuestionSnapshot> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        Ok(self.load(&connection)?.snapshot)
    }

    /// Get user messages that have not yet entered the input journal.
    pub(crate) fn outbox(&self) -> Result<Vec<QuestionOutbox>> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        Ok(self.load(&connection)?.outbox)
    }

    /// Reject queued answer delivery after its originating execution was cancelled.
    pub(crate) fn delivery_is_current(&self, id: &QuestionId) -> Result<bool> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        let state = self.load(&connection)?;
        let Some(question) = state
            .snapshot
            .questions
            .iter()
            .find(|question| question.id == *id)
        else {
            return Ok(false);
        };
        if state.cancelled_exchanges.contains(&question.exchange_id)
            || matches!(question.resolution, QuestionResolution::Retired { .. })
        {
            return Ok(false);
        }
        Ok(task_attribution_is_current(
            &connection,
            &self.session_id,
            question.task_id.as_deref(),
        )?)
    }

    /// Acknowledge durable input admission; provider delivery remains independently tracked.
    pub(crate) fn admitted(&self, id: &str) -> Result<()> {
        self.update(|state| {
            state.outbox.retain(|message| message.id != id);
            Ok(())
        })
    }

    /// Get interrupted chat requests whose required decisions are all resolved.
    pub(crate) fn ready_chat_continuations(&self) -> Result<Vec<String>> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        let state = self.load(&connection)?;
        if state.snapshot.required_pending() || !state.outbox.is_empty() {
            return Ok(Vec::new());
        }
        Ok(state
            .chat_continuation
            .iter()
            .filter(|(_, continuation)| matches!(continuation, ChatContinuation::Waiting))
            .map(|(exchange_id, _)| exchange_id.clone())
            .collect())
    }

    /// Acknowledge durable admission without replaying the chat continuation after restart.
    pub(crate) fn chat_continuation_admitted(&self, exchange_id: &str) -> Result<()> {
        self.update(|state| {
            let continuation = state
                .chat_continuation
                .get_mut(exchange_id)
                .context("chat continuation is missing")?;
            *continuation = ChatContinuation::Admitted;
            Ok(())
        })
    }

    /// Keep completed work distinct from a queued continuation that cancellation can revoke.
    pub(crate) fn chat_continuation_completed(&self, exchange_id: &str) -> Result<()> {
        self.update(|state| {
            let continuation = state
                .chat_continuation
                .get_mut(exchange_id)
                .context("chat continuation is missing")?;
            *continuation = ChatContinuation::Completed;
            Ok(())
        })
    }

    /// Reject already queued work after its owning chat request was cancelled.
    pub(crate) fn chat_continuation_is_current(&self, exchange_id: &str) -> Result<bool> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        Ok(matches!(
            self.load(&connection)?.chat_continuation.get(exchange_id),
            Some(ChatContinuation::Waiting | ChatContinuation::Admitted)
        ))
    }

    /// Revalidate immutable taskless continuation authority before any queued input is delivered.
    pub(crate) fn workflow_continuation_is_current(
        &self,
        selection: &crate::exchange::command::ExchangeContinuationSelection,
    ) -> Result<bool> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        let latest: Option<String> = connection.query_row("SELECT id FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1", params![self.session_id,crate::session::HarnessSession::primary_agent_id(&self.session_id)], |row| row.get(0)).optional()?;
        if latest.as_deref() != Some(selection.id().as_str()) {
            return Ok(false);
        }
        let payload: Option<String> = connection
            .query_row(
                "SELECT payload FROM exchange_record WHERE session_id=?1 AND id=?2",
                params![self.session_id, selection.id().as_str()],
                |row| row.get(0),
            )
            .optional()?;
        let Some(payload) = payload else {
            return Ok(false);
        };
        let exchange: crate::exchange::Exchange = serde_json::from_str(&payload)?;
        let task: Option<String> = connection.query_row(
            "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1",
            [&self.session_id],
            |row| row.get(0),
        )?;
        let task: Option<crate::task::Task> =
            task.map(|task| serde_json::from_str(&task)).transpose()?;
        Ok(selection.validate(&exchange, task.as_ref()).is_ok())
    }

    /// Revoke cancelled chat continuations while keeping optional questions answerable.
    pub(crate) fn cancel_chat(&self, active_exchange: Option<&str>) -> Result<()> {
        self.update(|state| {
            let mut cancelled = std::collections::BTreeSet::new();
            for (exchange_id, continuation) in &mut state.chat_continuation {
                if matches!(
                    continuation,
                    ChatContinuation::Waiting | ChatContinuation::Admitted
                ) {
                    cancelled.insert(exchange_id.clone());
                    *continuation = ChatContinuation::Cancelled;
                }
            }
            if let Some(exchange_id) = active_exchange {
                cancelled.insert(exchange_id.to_owned());
            }
            state.cancelled_exchanges.extend(cancelled.iter().cloned());
            let mut retired = std::collections::BTreeSet::new();
            for question in &mut state.snapshot.questions {
                if cancelled.contains(&question.exchange_id) {
                    retired.insert(question.id.as_str().to_owned());
                    if question.requirement == QuestionRequirement::Required && question.pending() {
                        question.resolution = QuestionResolution::Retired {
                            reason: "The owning chat request was cancelled".into(),
                        };
                    }
                }
            }
            state
                .outbox
                .retain(|message| !retired.contains(message.question_id.as_str()));
            Ok(())
        })
    }

    /// Replay a committed question mutation without requiring the originating invocation to remain live.
    pub(crate) fn replay_mutation(
        &self,
        command: &QuestionCommand,
    ) -> std::result::Result<Option<QuestionSnapshot>, QuestionCreationReplayError> {
        let connection = self.connection.lock().map_err(|_| {
            QuestionCreationReplayError::State(anyhow::anyhow!("question store poisoned"))
        })?;
        let state = self
            .load(&connection)
            .map_err(QuestionCreationReplayError::State)?;
        let Some(previous) = state.operations.get(&command.operation_id) else {
            return Ok(None);
        };
        let encoded = serde_json::to_string(command)
            .map_err(|error| QuestionCreationReplayError::State(error.into()))?;
        if previous != &encoded {
            return Err(QuestionCreationReplayError::IdentityConflict);
        }
        state
            .mutation_results
            .get(&command.operation_id)
            .cloned()
            .map(Some)
            .ok_or(QuestionCreationReplayError::LegacyResultUnavailable)
    }

    /// Replay an immutable creation receipt without validating live execution or publishing again.
    pub(crate) fn replay_creation(
        &self,
        operation_id: &str,
        exchange_id: &str,
        origin: QuestionOrigin<'_>,
        drafts: &[QuestionDraft],
    ) -> std::result::Result<Option<Vec<QuestionId>>, QuestionCreationReplayError> {
        let connection = self.connection.lock().map_err(|_| {
            QuestionCreationReplayError::State(anyhow::anyhow!("question store poisoned"))
        })?;
        let state = self
            .load(&connection)
            .map_err(QuestionCreationReplayError::State)?;
        let Some(previous) = state.operations.get(operation_id) else {
            return Ok(None);
        };
        let request = creation_request(exchange_id, &origin, drafts)
            .map_err(QuestionCreationReplayError::State)?;
        let legacy =
            serde_json::to_string(&(exchange_id, matches!(origin, QuestionOrigin::Chat), drafts))
                .map_err(|error| QuestionCreationReplayError::State(error.into()))?;
        if previous != &request && previous != &legacy {
            return Err(QuestionCreationReplayError::IdentityConflict);
        }
        let prefix = format!("{}:", crate::plan::digest(operation_id.as_bytes()));
        let ids: Vec<_> = state
            .snapshot
            .questions
            .iter()
            .filter(|question| question.id.0.starts_with(&prefix))
            .map(|question| question.id.clone())
            .collect();
        if ids.len() != drafts.len() {
            return Err(QuestionCreationReplayError::Incomplete);
        }
        Ok(Some(ids))
    }

    /// Attach an answer observer to an existing creation without mutating its durable questions.
    pub(crate) fn replay_tool_wait(
        self: &Arc<Self>,
        ids: Vec<QuestionId>,
    ) -> Result<LocalQuestionWait> {
        let changed = self.changed.subscribe();
        let result = self.wait_result(&ids)?;
        self.waiting
            .lock()
            .map_err(|_| anyhow::anyhow!("question wait registry poisoned"))?
            .extend(ids.iter().cloned());
        Ok(LocalQuestionWait {
            observation: Some(QuestionWaitObservation {
                controller: self.clone(),
                ids,
                changed,
                result,
            }),
        })
    }

    /// Read a completed wait result without replacing it with later answers or corrections.
    fn wait_result(
        &self,
        ids: &[QuestionId],
    ) -> std::result::Result<Option<Vec<QuestionRecord>>, QuestionWaitError> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| QuestionWaitError::StateUnavailable)?;
        let state = self.load(&connection).map_err(QuestionWaitError::State)?;
        let key = wait_result_key(ids)?;
        if let Some(result) = state.wait_results.get(&key) {
            return Ok(Some(result.clone()));
        }
        if state
            .snapshot
            .questions
            .iter()
            .filter(|question| ids.contains(&question.id))
            .any(|question| {
                question
                    .discussion
                    .iter()
                    .any(|message| state.observed_tool_messages.contains(&message.id))
            })
        {
            return Err(QuestionWaitError::LegacyResultUnavailable);
        }
        Ok(None)
    }

    /// Append questions atomically before acknowledging the model tool call.
    pub(crate) fn ask(
        &self,
        operation_id: &str,
        exchange_id: &str,
        origin: QuestionOrigin<'_>,
        drafts: Vec<QuestionDraft>,
    ) -> Result<Vec<QuestionId>> {
        ensure!(
            !operation_id.trim().is_empty() && !exchange_id.trim().is_empty(),
            "question operation and exchange identities are required"
        );
        ensure!(!drafts.is_empty(), "at least one question is required");
        let continues_chat = matches!(origin, QuestionOrigin::Chat);
        let delivery = if matches!(origin, QuestionOrigin::Tool(_)) {
            QuestionDelivery::ToolResult
        } else {
            QuestionDelivery::Message
        };
        let task = origin.task();
        let request = creation_request(exchange_id, &origin, &drafts)?;
        let legacy_request = serde_json::to_string(&(exchange_id, continues_chat, &drafts))?;
        self.update_guarded(task, |state| {
            ensure!(
                !state.cancelled_exchanges.contains(exchange_id),
                "question execution was cancelled"
            );
            if let Some(previous) = state.operations.get(operation_id) {
                ensure!(
                    previous == &request || previous == &legacy_request,
                    "question operation identity was reused"
                );
                return Ok(state
                    .snapshot
                    .questions
                    .iter()
                    .filter(|question| {
                        question.id.0.starts_with(&format!(
                            "{}:",
                            crate::plan::digest(operation_id.as_bytes())
                        ))
                    })
                    .map(|question| question.id.clone())
                    .collect());
            }
            let mut ids = Vec::new();
            for (index, draft) in drafts.into_iter().enumerate() {
                let id = QuestionId::for_operation(operation_id, index);
                let requirement = draft.requirement;
                let content = content(draft, 1)?;
                if continues_chat && requirement == QuestionRequirement::Required {
                    state
                        .chat_continuation
                        .entry(exchange_id.into())
                        .or_insert(ChatContinuation::Waiting);
                }
                ids.push(id.clone());
                state.snapshot.questions.push(QuestionRecord {
                    id,
                    exchange_id: exchange_id.into(),
                    task_id: task.map(|task| task.id.as_str().into()),
                    phase: task.map(crate::task::Task::phase),
                    requirement,
                    delivery,
                    content,
                    previous: Vec::new(),
                    resolution: QuestionResolution::Pending,
                    discussion: Vec::new(),
                });
            }
            state.operations.insert(operation_id.into(), request);
            Ok(ids)
        })
    }

    /// Keep the calling tool pending until every question is answered or withdrawn.
    /// Dropping the future releases its answer destinations so later answers use normal delivery.
    #[cfg(test)]
    #[cfg(test)]
    pub(crate) async fn ask_wait(
        &self,
        operation_id: &str,
        exchange_id: &str,
        task: Option<&crate::task::Task>,
        drafts: Vec<QuestionDraft>,
    ) -> Result<Vec<QuestionRecord>> {
        self.ask_wait_authorized(operation_id, exchange_id, task, drafts, None)
            .await
    }

    #[cfg(test)]
    pub(crate) async fn ask_wait_authorized(
        &self,
        operation_id: &str,
        exchange_id: &str,
        task: Option<&crate::task::Task>,
        drafts: Vec<QuestionDraft>,
        authority: Option<&crate::tools::TaskAuthority>,
    ) -> Result<Vec<QuestionRecord>> {
        let ids = (0..drafts.len())
            .map(|index| QuestionId::for_operation(operation_id, index))
            .collect();
        let mut changed = self.changed.subscribe();
        let waiting = QuestionWait::new(self, ids)?;
        let publish = || {
            self.ask(
                operation_id,
                exchange_id,
                QuestionOrigin::Tool(task),
                drafts,
            )
        };
        match authority {
            Some(authority) => authority.commit(publish)?,
            None => publish()?,
        };
        loop {
            let questions = self
                .snapshot()?
                .questions
                .into_iter()
                .filter(|question| waiting.ids.contains(&question.id))
                .collect::<Vec<_>>();
            if questions.iter().all(|question| !question.pending()) {
                return Ok(questions);
            }
            changed
                .changed()
                .await
                .context("question controller closed while waiting for answers")?;
        }
    }

    /// Publish on the mutation owner and retain rollback custody until its wait resource is installed.
    pub(crate) fn prepare_tool_wait(
        self: &std::sync::Arc<Self>,
        operation_id: &str,
        exchange_id: &str,
        task: Option<&crate::task::Task>,
        drafts: Vec<QuestionDraft>,
        authority: Option<&crate::tools::TaskAuthority>,
    ) -> Result<LocalQuestionWait> {
        let ids = (0..drafts.len())
            .map(|index| QuestionId::for_operation(operation_id, index))
            .collect::<Vec<_>>();
        let changed = self.changed.subscribe();
        let result = self.wait_result(&ids)?;
        self.waiting
            .lock()
            .map_err(|_| anyhow::anyhow!("question wait registry poisoned"))?
            .extend(ids.iter().cloned());
        let wait = LocalQuestionWait {
            observation: Some(QuestionWaitObservation {
                controller: self.clone(),
                result,
                ids,
                changed,
            }),
        };
        let publish = || {
            self.ask(
                operation_id,
                exchange_id,
                QuestionOrigin::Tool(task),
                drafts,
            )
        };
        match authority {
            Some(authority) => authority.commit(publish)?,
            None => publish()?,
        };
        Ok(wait)
    }

    /// Commit observation/fallback evidence before retiring one registered answer destination.
    ///
    /// A closing owner cannot wait for storage to recover, so when its commit fails it releases
    /// the destination without moving the answers; they stay in the canonical discussion.
    pub(crate) fn retire_wait(
        &self,
        ids: &[QuestionId],
        end: &QuestionWaitEnd,
        policy: QuestionWaitPolicy,
    ) -> std::result::Result<Option<Vec<QuestionRecord>>, QuestionWaitError> {
        match (self.retire_wait_durably(ids, end), policy) {
            (Err(_), QuestionWaitPolicy::Closing) => {
                release_waiting(self, ids)?;
                Ok(None)
            }
            (result, _) => result,
        }
    }

    /// Record observed answers and move unobserved ones to the outbox in one transaction.
    fn retire_wait_durably(
        &self,
        ids: &[QuestionId],
        end: &QuestionWaitEnd,
    ) -> std::result::Result<Option<Vec<QuestionRecord>>, QuestionWaitError> {
        // Keep question-store-before-wait-registry order, matching answer admission.
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| QuestionWaitError::StateUnavailable)?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut state = self.load(&transaction).map_err(QuestionWaitError::State)?;
        let mut waiting = self
            .waiting
            .lock()
            .map_err(|_| QuestionWaitError::RegistryUnavailable)?;
        if ids.iter().any(|id| !waiting.contains(id)) {
            return Err(QuestionWaitError::RegistryMismatch);
        }
        let previous_observed = state.observed_tool_messages.len();
        let mut result = None;
        let mut result_inserted = false;
        if let QuestionWaitEnd::Observed(report) = end {
            if report.questions.len() != ids.len()
                || report
                    .questions
                    .iter()
                    .zip(ids)
                    .any(|(question, id)| &question.id != id)
            {
                return Err(QuestionWaitError::ObservationMismatch);
            }
            let key = wait_result_key(ids)?;
            if let Some(saved) = state.wait_results.get(&key) {
                result = Some(saved.clone());
            } else {
                result = Some(report.questions.clone());
                state.wait_results.insert(key, report.questions.clone());
                result_inserted = true;
            }
            let retained_report = QuestionWaitReport::observed(
                result
                    .as_ref()
                    .expect("observed wait selects a retained result"),
            );
            for observed in &retained_report.messages {
                if !ids.contains(&observed.question)
                    || !state.snapshot.questions.iter().any(|question| {
                        question.id == observed.question
                            && question.discussion.iter().any(|message| {
                                message.id == observed.message
                                    && matches!(
                                        message.kind,
                                        QuestionMessageKind::Answer
                                            | QuestionMessageKind::Correction
                                    )
                            })
                    })
                {
                    return Err(QuestionWaitError::ObservationMismatch);
                }
                state
                    .observed_tool_messages
                    .insert(observed.message.clone());
            }
        }

        // Recover only messages with no remaining destination and no prior tool-observation receipt.
        // A closing owner recovers them too: answers given while the wait was registered went to
        // that wait rather than the outbox, and the outbox is the only record that survives into
        // the next owner, which admits it at startup. The closing owner itself admits nothing.
        let mut recovered = Vec::new();
        for id in ids.iter().filter(|id| {
            waiting
                .iter()
                .filter(|registered| *registered == *id)
                .count()
                == 1
        }) {
            let question = state
                .snapshot
                .questions
                .iter()
                .find(|question| question.id == *id)
                .ok_or(QuestionWaitError::TargetMissing)?;
            if !task_attribution_is_current(
                &transaction,
                &self.session_id,
                question.task_id.as_deref(),
            )? || state.cancelled_exchanges.contains(&question.exchange_id)
                || matches!(question.resolution, QuestionResolution::Retired { .. })
            {
                continue;
            }
            for message in question.discussion.iter().filter(|message| {
                matches!(
                    message.kind,
                    QuestionMessageKind::Answer | QuestionMessageKind::Correction
                )
            }) {
                if state.observed_tool_messages.contains(&message.id)
                    || state.outbox.iter().any(|pending| pending.id == message.id)
                {
                    continue;
                }
                recovered.push(QuestionOutbox {
                    id: message.id.clone(),
                    question_id: question.id.clone(),
                    task_id: question.task_id.clone(),
                    kind: message.kind,
                    text: message.text.clone(),
                    reply: saved_reply(&state, &question.id, &message.id, message.kind)
                        .map_err(QuestionWaitError::State)?,
                });
            }
        }
        let changed = result_inserted
            || previous_observed != state.observed_tool_messages.len()
            || !recovered.is_empty();
        state.outbox.extend(recovered);
        if changed {
            state.snapshot.revision = state
                .snapshot
                .revision
                .checked_add(1)
                .ok_or(QuestionWaitError::RevisionExhausted)?;
            transaction.execute("INSERT INTO session_question_state(session_id,format_version,payload) VALUES(?1,?2,?3) ON CONFLICT(session_id) DO UPDATE SET format_version=excluded.format_version,payload=excluded.payload", params![self.session_id, FORMAT_VERSION, serde_json::to_string(&state)?])?;
        }
        transaction.commit()?;
        release_registered(&mut waiting, ids);
        drop(waiting);
        drop(connection);
        if changed {
            self.changed.send_replace(state.snapshot.revision);
        }
        Ok(result)
    }

    /// Commit an explicit decision or discussion item together with its delivery intent.
    pub(crate) fn apply(&self, command: QuestionCommand) -> Result<QuestionSnapshot> {
        ensure!(
            !command.operation_id.trim().is_empty(),
            "question operation identity is required"
        );
        let encoded = serde_json::to_string(&command)?;
        let operation_id = command.operation_id.clone();
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut state = self.load(&transaction)?;
        if let Some(previous) = state.operations.get(&operation_id) {
            ensure!(
                previous == &encoded,
                "question operation identity was reused"
            );
            return state
                .mutation_results
                .get(&operation_id)
                .cloned()
                .ok_or_else(|| QuestionCreationReplayError::LegacyResultUnavailable.into());
        }
        // Apply the mutation before retaining the exact response in the same durable transaction.
        let mutate = |state: &mut QuestionState| -> Result<()> {
            let other_required_pending = state.snapshot.questions.iter().any(|question| {
                question.id != command.question_id
                    && question.requirement == QuestionRequirement::Required
                    && question.pending()
            });
            let question = state
                .snapshot
                .questions
                .iter_mut()
                .find(|question| question.id == command.question_id)
                .context("question does not belong to this session")?;
            let cancelled = state.cancelled_exchanges.contains(&question.exchange_id);
            ensure!(
                question.content.revision == command.revision,
                "question changed; review its current choices before submitting"
            );
            ensure!(
                !matches!(question.resolution, QuestionResolution::Retired { .. }),
                "question owner is no longer active"
            );
            let message_id = command.operation_id.clone();
            let mut outbound = None;
            match command.action {
                QuestionAction::Answer { answer } => {
                    ensure!(
                        question.pending(),
                        "question already resolved; send a correction instead"
                    );
                    let text = answer_text(&question.content, &answer)?;
                    question.resolution = QuestionResolution::Answered {
                        answer,
                        message_id: message_id.clone(),
                    };
                    outbound = Some((QuestionMessageKind::Answer, text));
                }
                QuestionAction::Clarify { text } => {
                    ensure!(!cancelled, "question execution was cancelled");
                    ensure!(question.pending(), "question is no longer pending");
                    ensure!(!text.trim().is_empty(), "clarification is empty");
                    outbound = Some((QuestionMessageKind::Clarification, text));
                }
                QuestionAction::Correct { text } => {
                    ensure!(
                        matches!(question.resolution, QuestionResolution::Answered { .. }),
                        "only an answered question accepts a correction"
                    );
                    ensure!(!text.trim().is_empty(), "correction is empty");
                    outbound = Some((QuestionMessageKind::Correction, text));
                }
                QuestionAction::Dismiss => {
                    ensure!(question.pending(), "question is no longer pending");
                    ensure!(
                        question.requirement == QuestionRequirement::Optional,
                        "required questions cannot be dismissed"
                    );
                    question.resolution = QuestionResolution::Dismissed;
                }
                QuestionAction::Reply {
                    message_id: parent,
                    text,
                } => {
                    ensure!(!text.trim().is_empty(), "clarification reply is empty");
                    ensure!(
                        question
                            .discussion
                            .iter()
                            .any(|message| message.id == parent
                                && message.kind == QuestionMessageKind::Clarification),
                        "clarification message not found"
                    );
                    question.discussion.push(QuestionMessage {
                        id: message_id.clone(),
                        kind: QuestionMessageKind::Reply,
                        text,
                        reply_to: Some(parent),
                    });
                }
                QuestionAction::Revise { draft } => {
                    ensure!(question.pending(), "resolved questions cannot be revised");
                    ensure!(
                        draft.requirement == question.requirement,
                        "question requirement cannot change during revision"
                    );
                    let next = content(
                        draft,
                        question
                            .content
                            .revision
                            .checked_add(1)
                            .context("question revision exhausted")?,
                    )?;
                    question
                        .previous
                        .push(std::mem::replace(&mut question.content, next));
                }
                QuestionAction::Withdraw { reason } => {
                    ensure!(question.pending(), "question is no longer pending");
                    ensure!(!reason.trim().is_empty(), "withdrawal reason is required");
                    question.resolution = QuestionResolution::Withdrawn { reason };
                }
            }
            if let Some((kind, text)) = outbound {
                question.discussion.push(QuestionMessage {
                    id: message_id.clone(),
                    kind,
                    text: text.clone(),
                    reply_to: None,
                });
                // Optional answers remain durable history without authorizing another native turn.
                if cancelled {
                    return Ok(());
                }
                let waiting = question.delivery == QuestionDelivery::ToolResult
                    && self
                        .waiting
                        .lock()
                        .map_err(|_| anyhow::anyhow!("question wait registry poisoned"))?
                        .contains(&question.id);
                if waiting
                    && matches!(
                        kind,
                        QuestionMessageKind::Answer | QuestionMessageKind::Correction
                    )
                {
                    return Ok(());
                }
                let response = match kind {
                    QuestionMessageKind::Clarification => {
                        forge_provider_api::QuestionReplyResponse::Clarification {
                            options: question
                                .content
                                .options
                                .iter()
                                .map(|option| forge_provider_api::QuestionReplyOption {
                                    id: option.id.clone(),
                                    label: option.label.clone(),
                                    description: option.description.clone(),
                                    recommended: option.recommended,
                                })
                                .collect(),
                            text: text.clone(),
                            reply_to: None,
                        }
                    }
                    QuestionMessageKind::Answer => {
                        forge_provider_api::QuestionReplyResponse::Answer {
                            answer: text.clone(),
                            required_questions_remain: other_required_pending,
                        }
                    }
                    QuestionMessageKind::Correction => {
                        forge_provider_api::QuestionReplyResponse::Correction {
                            answer: text.clone(),
                            required_questions_remain: other_required_pending,
                        }
                    }
                    QuestionMessageKind::Reply => anyhow::bail!(
                        "a discussion reply cannot be submitted as a question response"
                    ),
                };
                let reply = forge_provider_api::QuestionReply {
                    question_id: question.id.as_str().to_owned(),
                    revision: question.content.revision,
                    message_id: message_id.clone(),
                    question: question.content.question.clone(),
                    response,
                };
                state.outbox.push(QuestionOutbox {
                    id: message_id,
                    question_id: question.id.clone(),
                    task_id: question.task_id.clone(),
                    kind,
                    text,
                    reply,
                });
            }
            Ok(())
        };
        mutate(&mut state)?;
        state.snapshot.revision = state
            .snapshot
            .revision
            .checked_add(1)
            .context("question snapshot revision exhausted")?;
        let snapshot = state.snapshot.clone();
        state.operations.insert(operation_id.clone(), encoded);
        state
            .mutation_results
            .insert(operation_id, snapshot.clone());
        transaction.execute("INSERT INTO session_question_state(session_id,format_version,payload) VALUES(?1,?2,?3) ON CONFLICT(session_id) DO UPDATE SET format_version=excluded.format_version,payload=excluded.payload", params![self.session_id, FORMAT_VERSION, serde_json::to_string(&state)?])?;
        transaction.commit()?;
        self.changed.send_replace(snapshot.revision);
        Ok(snapshot)
    }

    fn load(&self, connection: &Connection) -> Result<QuestionState> {
        let encoded: Option<(u32, String)> = connection
            .query_row(
                "SELECT format_version,payload FROM session_question_state WHERE session_id=?1",
                params![self.session_id],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .optional()?;
        let state: QuestionState = encoded
            .map(|(version, encoded)| decode_question_state(version, &encoded))
            .transpose()?
            .unwrap_or_default();
        let mut identities = std::collections::BTreeSet::new();
        for question in &state.snapshot.questions {
            ensure!(
                identities.insert(question.id.as_str()),
                "duplicate persisted question identity"
            );
            ensure!(
                !question.exchange_id.is_empty()
                    && question.content.revision > 0
                    && !question.content.header.trim().is_empty()
                    && !question.content.question.trim().is_empty(),
                "invalid persisted question content"
            );
            let mut options = std::collections::BTreeSet::new();
            for option in &question.content.options {
                ensure!(
                    !option.id.is_empty()
                        && !option.label.trim().is_empty()
                        && options.insert(&option.id),
                    "invalid persisted question choice"
                );
            }
            if let QuestionResolution::Answered { answer, .. } = &question.resolution {
                answer_text(&question.content, answer)?;
            }
            ensure!(
                !matches!(question.resolution, QuestionResolution::Dismissed)
                    || question.requirement == QuestionRequirement::Optional,
                "required question was dismissed"
            );
        }
        Ok(state)
    }

    fn update<Output>(
        &self,
        change: impl FnOnce(&mut QuestionState) -> Result<Output>,
    ) -> Result<Output> {
        self.update_guarded(None, change)
    }

    fn update_guarded<Output>(
        &self,
        task: Option<&crate::task::Task>,
        change: impl FnOnce(&mut QuestionState) -> Result<Output>,
    ) -> Result<Output> {
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| anyhow::anyhow!("question store poisoned"))?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        if let Some(task) = task {
            let current: Option<(String, String)> = transaction.query_row(
                "SELECT json_extract(payload,'$.session.active_task.id'), json_extract(payload,'$.session.active_task.revision') FROM session_record WHERE id=?1 AND json_type(payload,'$.session.active_task')='object'",
                [&self.session_id], |row| Ok((row.get(0)?, row.get(1)?))).optional()?;
            ensure!(
                current
                    .as_ref()
                    .is_some_and(|(id, revision)| id == task.id.as_str()
                        && revision == task.revision.as_str()),
                "question task changed before persistence"
            );
        }
        let mut state = self.load(&transaction)?;
        let result = change(&mut state)?;
        state.snapshot.revision = state
            .snapshot
            .revision
            .checked_add(1)
            .context("question snapshot revision exhausted")?;
        transaction.execute("INSERT INTO session_question_state(session_id,format_version,payload) VALUES(?1,?2,?3) ON CONFLICT(session_id) DO UPDATE SET format_version=excluded.format_version,payload=excluded.payload", params![self.session_id, FORMAT_VERSION, serde_json::to_string(&state)?])?;
        transaction.commit()?;
        self.changed.send_replace(state.snapshot.revision);
        Ok(result)
    }
}

/// Derive a stable wait receipt key from the exact ordered question identities.
fn wait_result_key(ids: &[QuestionId]) -> std::result::Result<String, QuestionWaitError> {
    Ok(crate::plan::digest(&serde_json::to_vec(ids)?))
}

/// Encode the complete durable creation intent, including answer destination and task attribution.
fn creation_request(
    exchange_id: &str,
    origin: &QuestionOrigin<'_>,
    drafts: &[QuestionDraft],
) -> Result<String> {
    let source = match origin {
        QuestionOrigin::Chat => "chat",
        QuestionOrigin::Task(_) => "task",
        QuestionOrigin::Discussion(_) => "discussion",
        QuestionOrigin::Tool(_) => "tool",
    };
    serde_json::to_string(&(
        2_u8,
        exchange_id,
        source,
        origin.task().map(|task| task.id.as_str()),
        drafts,
    ))
    .map_err(Into::into)
}

/// Reconstruct one admitted reply without interpreting its old provider-rendered text.
fn saved_reply(
    state: &QuestionState,
    question_id: &QuestionId,
    message_id: &str,
    kind: QuestionMessageKind,
) -> Result<forge_provider_api::QuestionReply> {
    let question = state
        .snapshot
        .questions
        .iter()
        .find(|question| question.id == *question_id)
        .context("saved question response has no question")?;
    let message = question
        .discussion
        .iter()
        .find(|message| message.id == message_id && message.kind == kind)
        .context("saved question response has no canonical discussion entry")?;
    let revision = state
        .operations
        .get(message_id)
        .map(|command| serde_json::from_str::<QuestionCommand>(command))
        .transpose()?
        .map_or(question.content.revision, |command| command.revision);
    let content = std::iter::once(&question.content)
        .chain(question.previous.iter())
        .find(|content| content.revision == revision)
        .context("saved question revision is unavailable")?;
    let required_questions_remain = state.snapshot.questions.iter().any(|other| {
        other.id != *question_id
            && other.requirement == QuestionRequirement::Required
            && other.pending()
    });
    let response = match kind {
        QuestionMessageKind::Answer => forge_provider_api::QuestionReplyResponse::Answer {
            answer: message.text.clone(),
            required_questions_remain,
        },
        QuestionMessageKind::Correction => forge_provider_api::QuestionReplyResponse::Correction {
            answer: message.text.clone(),
            required_questions_remain,
        },
        QuestionMessageKind::Clarification => {
            forge_provider_api::QuestionReplyResponse::Clarification {
                options: content
                    .options
                    .iter()
                    .map(|option| forge_provider_api::QuestionReplyOption {
                        id: option.id.clone(),
                        label: option.label.clone(),
                        description: option.description.clone(),
                        recommended: option.recommended,
                    })
                    .collect(),
                text: message.text.clone(),
                reply_to: message.reply_to.clone(),
            }
        }
        QuestionMessageKind::Reply => {
            anyhow::bail!("saved discussion reply cannot authorize provider input")
        }
    };
    let reply = forge_provider_api::QuestionReply {
        question_id: question_id.as_str().to_owned(),
        revision,
        message_id: message_id.to_owned(),
        question: content.question.clone(),
        response,
    };
    Ok(reply)
}

/// Rebuild older outbox entries from canonical discussion, never from their native text envelope.
fn decode_question_state(version: u32, encoded: &str) -> Result<QuestionState> {
    if version == FORMAT_VERSION {
        return Ok(serde_json::from_str(encoded)?);
    }
    ensure!(version == 3, "unsupported question state format");
    #[derive(Deserialize)]
    struct LegacyOutbox {
        id: String,
        question_id: QuestionId,
        task_id: Option<String>,
        kind: QuestionMessageKind,
    }
    #[derive(Deserialize)]
    struct LegacyState {
        snapshot: QuestionSnapshot,
        outbox: Vec<LegacyOutbox>,
        operations: BTreeMap<String, String>,
        chat_continuation: BTreeMap<String, ChatContinuation>,
        cancelled_exchanges: BTreeSet<String>,
    }
    let legacy: LegacyState = serde_json::from_str(encoded)?;
    let state = QuestionState {
        snapshot: legacy.snapshot,
        operations: legacy.operations,
        chat_continuation: legacy.chat_continuation,
        cancelled_exchanges: legacy.cancelled_exchanges,
        ..QuestionState::default()
    };
    let outbox = legacy
        .outbox
        .into_iter()
        .map(|entry| {
            let reply = saved_reply(&state, &entry.question_id, &entry.id, entry.kind)?;
            let text = match &reply.response {
                forge_provider_api::QuestionReplyResponse::Answer { answer, .. }
                | forge_provider_api::QuestionReplyResponse::Correction { answer, .. } => {
                    answer.clone()
                }
                forge_provider_api::QuestionReplyResponse::Clarification { text, .. } => {
                    text.clone()
                }
            };
            Ok(QuestionOutbox {
                id: entry.id,
                question_id: entry.question_id,
                task_id: entry.task_id,
                kind: entry.kind,
                text,
                reply,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(QuestionState { outbox, ..state })
}

/// Release live answer destinations when a waiting tool completes or is cancelled.
#[cfg(test)]
struct QuestionWait<'controller> {
    controller: &'controller QuestionController,
    ids: Vec<QuestionId>,
}

/// Keep rollback custody on the owner until a published wait is installed as an owned resource.
pub(crate) struct LocalQuestionWait {
    observation: Option<QuestionWaitObservation>,
}

impl LocalQuestionWait {
    /// Get the question identities registered by this one guard.
    pub(crate) fn question_ids(&self) -> &[QuestionId] {
        &self
            .observation
            .as_ref()
            .expect("local question wait retains observation until ownership transfer")
            .ids
    }

    /// Transfer the registration exactly once into a retirement capability owned by the driver.
    pub(crate) fn into_owned(
        mut self,
        retirement: crate::exchange::QuestionWaitRetirement,
    ) -> OwnedQuestionWait {
        OwnedQuestionWait {
            observation: self
                .observation
                .take()
                .expect("local question wait transfers its observation once"),
            retirement: WaitRetirement::Owner(retirement),
        }
    }

    /// Preserve direct-controller custody only for isolated unit fixtures.
    #[cfg(test)]
    pub(crate) fn into_test_owned(mut self) -> OwnedQuestionWait {
        let observation = self
            .observation
            .take()
            .expect("local question wait transfers its observation once");
        let retirement = LocalWaitRetirement {
            controller: observation.controller.clone(),
            ids: observation.ids.clone(),
        };
        OwnedQuestionWait {
            observation,
            retirement: WaitRetirement::Local(retirement),
        }
    }
}

impl Drop for LocalQuestionWait {
    /// Roll back a registration only while it remains under local mutation-owner custody.
    fn drop(&mut self) {
        if let Some(observation) = &self.observation {
            let _ = release_waiting(&observation.controller, &observation.ids);
        }
    }
}

/// Retain readonly answer observation separately from the authority that retires its destination.
struct QuestionWaitObservation {
    controller: std::sync::Arc<QuestionController>,
    ids: Vec<QuestionId>,
    changed: tokio::sync::watch::Receiver<u64>,
    result: Option<Vec<QuestionRecord>>,
}

impl QuestionWaitObservation {
    /// Observe the exact registered questions without mutating their lifecycle or delivery state.
    async fn answers(&mut self) -> std::result::Result<Vec<QuestionRecord>, QuestionWaitError> {
        if let Some(result) = &self.result {
            return Ok(result.clone());
        }
        loop {
            let questions = self
                .controller
                .snapshot()
                .map_err(QuestionWaitError::State)?
                .questions
                .into_iter()
                .filter(|question| self.ids.contains(&question.id))
                .collect::<Vec<_>>();
            if questions.len() != self.ids.len() {
                return Err(QuestionWaitError::TargetMissing);
            }
            if questions.iter().all(|question| !question.pending()) {
                return Ok(questions);
            }
            self.changed
                .changed()
                .await
                .map_err(|_| QuestionWaitError::ObservationLost)?;
        }
    }
}

/// Keep production retirement owner-reported; fixture-only local retirement cannot enter production.
enum WaitRetirement {
    Owner(crate::exchange::QuestionWaitRetirement),
    #[cfg(test)]
    Local(LocalWaitRetirement),
}

/// Observe a published question wait while its retirement remains exclusively owned by the driver.
pub(crate) struct OwnedQuestionWait {
    observation: QuestionWaitObservation,
    retirement: WaitRetirement,
}

impl OwnedQuestionWait {
    /// Get the committed question identities owned by this answer observation.
    #[cfg(test)]
    pub(crate) fn question_ids(&self) -> &[QuestionId] {
        &self.observation.ids
    }

    /// Report exactly the returned answers before releasing the wait, or terminate on owner loss.
    pub(crate) async fn answers(
        mut self,
    ) -> std::result::Result<Vec<QuestionRecord>, QuestionWaitError> {
        let result = match &mut self.retirement {
            WaitRetirement::Owner(retirement) => tokio::select! {
                biased;
                _ = retirement.closed() => Err(QuestionWaitError::OwnerClosed),
                answers = self.observation.answers() => answers,
            },
            #[cfg(test)]
            WaitRetirement::Local(_) => self.observation.answers().await,
        };
        if let Ok(questions) = &result {
            match &mut self.retirement {
                WaitRetirement::Owner(retirement) => return retirement.observed(questions).await,
                #[cfg(test)]
                WaitRetirement::Local(retirement) => {
                    let stored = retirement.controller.retire_wait(
                        &retirement.ids,
                        &QuestionWaitEnd::Observed(QuestionWaitReport::observed(questions)),
                        QuestionWaitPolicy::Live,
                    )?;
                    retirement.ids.clear();
                    return stored.ok_or(QuestionWaitError::ObservationMismatch);
                }
            }
        }
        result
    }
}

/// Release direct-controller waits only in unit fixtures without an exchange driver.
#[cfg(test)]
struct LocalWaitRetirement {
    controller: std::sync::Arc<QuestionController>,
    ids: Vec<QuestionId>,
}

#[cfg(test)]
impl Drop for LocalWaitRetirement {
    /// Preserve another fixture guard's registration for the same question IDs.
    fn drop(&mut self) {
        let _ = release_waiting(&self.controller, &self.ids);
    }
}

/// Retain the exact canonical messages returned by an answer observation, never later corrections.
pub(crate) struct QuestionWaitReport {
    questions: Vec<QuestionRecord>,
    messages: Vec<ObservedQuestionMessage>,
}

struct ObservedQuestionMessage {
    question: QuestionId,
    message: String,
}

impl QuestionWaitReport {
    /// Capture the answer/correction messages present in the tool's returned question snapshot.
    pub(crate) fn observed(questions: &[QuestionRecord]) -> Self {
        Self {
            questions: questions.to_vec(),
            messages: questions
                .iter()
                .flat_map(|question| {
                    question
                        .discussion
                        .iter()
                        .filter(|message| {
                            matches!(
                                message.kind,
                                QuestionMessageKind::Answer | QuestionMessageKind::Correction
                            )
                        })
                        .map(|message| ObservedQuestionMessage {
                            question: question.id.clone(),
                            message: message.id.clone(),
                        })
                })
                .collect(),
        }
    }
}

/// Distinguish a completed tool observation from abandonment and explicit owner shutdown.
pub(crate) enum QuestionWaitEnd {
    Observed(QuestionWaitReport),
    Abandoned,
    OwnerClosed,
}

/// Select how a failed retirement commit ends: a live owner retries it, a closing owner releases
/// the destination so shutdown cannot wait on storage recovery.
#[derive(Clone, Copy)]
pub(crate) enum QuestionWaitPolicy {
    Live,
    Closing,
}

impl QuestionWaitEnd {
    /// Get the local resource outcome for correlated logs without claiming native SDK receipt.
    pub(crate) fn name(&self) -> &'static str {
        match self {
            Self::Observed(_) => "observed",
            Self::Abandoned => "abandoned",
            Self::OwnerClosed => "owner_closed",
        }
    }
}

/// Preserve wait-resource observation and persistence failures as typed owner facts.
#[derive(Debug, thiserror::Error)]
pub(crate) enum QuestionWaitError {
    #[error("the legacy question wait has no retained response")]
    LegacyResultUnavailable,
    #[error("question wait owner is closed")]
    OwnerClosed,
    #[error("question wait observation ended")]
    ObservationLost,
    #[error("question wait target is missing")]
    TargetMissing,
    #[error("question wait registry is unavailable")]
    RegistryUnavailable,
    #[error("question wait registry does not contain the retained registration")]
    RegistryMismatch,
    #[error("question wait observation does not match its canonical messages")]
    ObservationMismatch,
    #[error("question wait state is unavailable")]
    StateUnavailable,
    #[error("question wait state revision is exhausted")]
    RevisionExhausted,
    #[error("question wait state could not be loaded")]
    State(#[source] anyhow::Error),
    #[error("question wait retirement could not be committed")]
    Storage(#[from] rusqlite::Error),
    #[error("question wait state could not be encoded")]
    Codec(#[from] serde_json::Error),
}

impl QuestionWaitError {
    /// Identify failures that leave an accepted logical response without durable custody.
    pub(crate) fn requires_storage_fence(&self) -> bool {
        match self {
            Self::StateUnavailable
            | Self::State(_)
            | Self::Storage(_)
            | Self::Codec(_)
            | Self::RevisionExhausted
            | Self::RegistryUnavailable => true,
            Self::LegacyResultUnavailable
            | Self::OwnerClosed
            | Self::ObservationLost
            | Self::TargetMissing
            | Self::RegistryMismatch
            | Self::ObservationMismatch => false,
        }
    }
    /// Get a stable diagnostic code without parsing callback or storage text.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::LegacyResultUnavailable => "question_wait_legacy_result_unavailable",
            Self::OwnerClosed => "question_wait_owner_closed",
            Self::ObservationLost => "question_wait_observation_lost",
            Self::TargetMissing => "question_wait_target_missing",
            Self::RegistryUnavailable | Self::RegistryMismatch => {
                "question_wait_registry_unavailable"
            }
            Self::ObservationMismatch => "question_wait_observation_mismatch",
            Self::StateUnavailable | Self::State(_) => "question_wait_state_unavailable",
            Self::RevisionExhausted => "question_wait_revision_exhausted",
            Self::Storage(_) => "question_wait_retirement_failed",
            Self::Codec(_) => "question_wait_encoding_failed",
        }
    }
}

#[cfg(test)]
impl<'controller> QuestionWait<'controller> {
    /// Register all destinations before publishing questions so fast answers cannot bypass the tool.
    fn new(controller: &'controller QuestionController, ids: Vec<QuestionId>) -> Result<Self> {
        controller
            .waiting
            .lock()
            .map_err(|_| anyhow::anyhow!("question wait registry poisoned"))?
            .extend(ids.iter().cloned());
        Ok(Self { controller, ids })
    }
}

#[cfg(test)]
impl Drop for QuestionWait<'_> {
    /// Restore normal message delivery when no live tool owns these answers.
    fn drop(&mut self) {
        let _ = release_waiting(self.controller, &self.ids);
    }
}

/// Release only this guard's registrations, preserving another wait for the same question identities.
fn release_waiting(
    controller: &QuestionController,
    ids: &[QuestionId],
) -> std::result::Result<(), QuestionWaitError> {
    let mut waiting = controller
        .waiting
        .lock()
        .map_err(|_| QuestionWaitError::RegistryUnavailable)?;
    release_registered(&mut waiting, ids);
    Ok(())
}

/// Remove exactly one registration per selected question, preserving independent wait guards.
fn release_registered(waiting: &mut Vec<QuestionId>, ids: &[QuestionId]) {
    for id in ids {
        if let Some(index) = waiting.iter().position(|registered| registered == id) {
            waiting.swap_remove(index);
        }
    }
}

/// Validate task attribution in the same SQLite snapshot as its dependent question operation.
fn task_attribution_is_current(
    connection: &Connection,
    session: &str,
    task: Option<&str>,
) -> rusqlite::Result<bool> {
    let Some(task) = task else {
        return Ok(true);
    };
    let current: Option<Option<String>> = connection.query_row(
        "SELECT json_extract(payload,'$.session.active_task.id') FROM session_record WHERE id=?1",
        [session], |row| row.get(0),
    ).optional()?;
    Ok(current.flatten().as_deref() == Some(task))
}

fn content(draft: QuestionDraft, revision: u64) -> Result<QuestionContent> {
    ensure!(
        !draft.header.trim().is_empty() && !draft.question.trim().is_empty(),
        "question heading and text are required"
    );
    let mut options = Vec::new();
    for (index, option) in draft.options.into_iter().enumerate() {
        ensure!(!option.label.trim().is_empty(), "choice label is empty");
        options.push(QuestionOption {
            id: format!("{revision}:{index}"),
            label: option.label,
            description: option.description,
            recommended: option.recommended,
        });
    }
    Ok(QuestionContent {
        revision,
        header: draft.header,
        question: draft.question,
        options,
    })
}

fn answer_text(content: &QuestionContent, answer: &QuestionAnswer) -> Result<String> {
    match answer {
        QuestionAnswer::Selected {
            option_id,
            feedback,
        } => {
            let option = content
                .options
                .iter()
                .find(|option| &option.id == option_id)
                .context("selected choice does not belong to this question revision")?;
            Ok(format!(
                "{}{}",
                option.label,
                feedback
                    .as_ref()
                    .map(|text| format!("\n{text}"))
                    .unwrap_or_default()
            ))
        }
        QuestionAnswer::Other { text } => {
            ensure!(!text.trim().is_empty(), "answer is empty");
            Ok(text.clone())
        }
    }
}

/// Retire a replaced task's questions in the same transaction as its lifecycle transition.
pub(crate) fn retire_task(
    transaction: &rusqlite::Transaction<'_>,
    session_id: &str,
    task_id: &str,
) -> Result<()> {
    let exists: bool = transaction.query_row("SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_question_state')", [], |row| row.get(0))?;
    if !exists {
        return Ok(());
    }
    let payload: Option<(u32, String)> = transaction
        .query_row(
            "SELECT format_version,payload FROM session_question_state WHERE session_id=?1",
            params![session_id],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .optional()?;
    let Some((version, payload)) = payload else {
        return Ok(());
    };
    let mut state = decode_question_state(version, &payload)?;
    for question in &mut state.snapshot.questions {
        if question.task_id.as_deref() == Some(task_id) && question.pending() {
            question.resolution = QuestionResolution::Retired {
                reason: "The owning task was cancelled, cleared, or replaced".into(),
            };
        }
    }
    state
        .outbox
        .retain(|message| message.task_id.as_deref() != Some(task_id));
    state.snapshot.revision = state
        .snapshot
        .revision
        .checked_add(1)
        .context("question snapshot revision exhausted")?;
    transaction.execute(
        "UPDATE session_question_state SET payload=?2,format_version=?3 WHERE session_id=?1",
        params![session_id, serde_json::to_string(&state)?, FORMAT_VERSION],
    )?;
    Ok(())
}

/// Project retained questions into their originating exchange without changing stored exchanges.
pub(crate) fn project(
    exchanges: &mut [crate::exchange::Exchange],
    snapshot: &QuestionSnapshot,
) -> Result<()> {
    for question in &snapshot.questions {
        let Some(exchange) = exchanges
            .iter_mut()
            .find(|exchange| exchange.id == question.exchange_id)
        else {
            continue;
        };
        let mut text = question.content.question.clone();
        for message in &question.discussion {
            let author = match message.kind {
                QuestionMessageKind::Clarification => "You asked",
                QuestionMessageKind::Reply => "Explanation",
                QuestionMessageKind::Answer => "You answered",
                QuestionMessageKind::Correction => "You corrected",
            };
            text.push_str(&format!("\n\n{author}: {}", message.text));
        }
        let answer = match &question.resolution {
            QuestionResolution::Pending => None,
            QuestionResolution::Answered { answer, .. } => {
                Some(answer_text(&question.content, answer)?)
            }
            QuestionResolution::Dismissed => Some("Dismissed by user".into()),
            QuestionResolution::Withdrawn { reason } | QuestionResolution::Retired { reason } => {
                Some(reason.clone())
            }
        };
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::QuestionPresented {
                id: format!("question:{}", question.id.as_str()),
                question: crate::plan::PlanQuestionSet {
                    id: question.id.as_str().into(),
                    questions: vec![crate::plan::PlanQuestion {
                        id: question.id.as_str().into(),
                        header: format!(
                            "{}{}",
                            question.content.header,
                            if question.requirement == QuestionRequirement::Required {
                                " · Required"
                            } else {
                                ""
                            }
                        ),
                        question: text,
                        options: question
                            .content
                            .options
                            .iter()
                            .map(|option| crate::plan::PlanQuestionOption {
                                recommended: option.recommended,
                                label: option.label.clone(),
                                description: option.description.clone(),
                            })
                            .collect(),
                        allow_freeform: true,
                    }],
                },
                answer,
            });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep partial answers inside a waiting call and fall back to messages after cancellation.
    #[tokio::test]
    async fn waiting_tools_collect_answers_and_release_cancelled_destinations() -> Result<()> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let controller = QuestionController::open(root.path(), "session")?;
        let mut pending = Box::pin(controller.ask_wait(
            "wait",
            "exchange",
            None,
            vec![
                draft(QuestionRequirement::Required),
                draft(QuestionRequirement::Required),
            ],
        ));
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        let questions = controller.snapshot()?.questions;
        controller.apply(command(
            &questions[0].id,
            "first-answer",
            QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "Tabs".into(),
                },
            },
        ))?;
        assert!(controller.outbox()?.is_empty());
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        controller.apply(command(
            &questions[1].id,
            "withdraw",
            QuestionAction::Withdraw {
                reason: "The first answer settles this too".into(),
            },
        ))?;
        let answers = pending.await?;
        assert_eq!(answers.len(), 2);
        assert!(answers.iter().all(|question| !question.pending()));
        assert!(controller.ready_chat_continuations()?.is_empty());
        let mut cancelled = Box::pin(controller.ask_wait(
            "cancelled",
            "exchange",
            None,
            vec![draft(QuestionRequirement::Required)],
        ));
        assert!(futures_util::poll!(cancelled.as_mut()).is_pending());
        let id = controller.snapshot()?.questions[2].id.clone();
        drop(cancelled);
        controller.apply(command(
            &id,
            "late-answer",
            QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "Sidebar".into(),
                },
            },
        ))?;
        assert_eq!(controller.outbox()?.len(), 1);
        Ok(())
    }

    /// A partial answer given to a live wait survives the owner closing before the wait ends.
    #[test]
    fn closing_owner_moves_partial_wait_answers_to_the_outbox() -> Result<()> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let controller = std::sync::Arc::new(QuestionController::open(root.path(), "session")?);
        let wait = controller.prepare_tool_wait(
            "closing",
            "exchange",
            None,
            vec![
                draft(QuestionRequirement::Required),
                draft(QuestionRequirement::Required),
            ],
            None,
        )?;
        let ids = wait.question_ids().to_vec();
        controller.apply(command(
            &ids[0],
            "partial-answer",
            QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "Tabs".into(),
                },
            },
        ))?;
        assert!(
            controller.outbox()?.is_empty(),
            "a live wait owns the answer"
        );
        controller.retire_wait(
            &ids,
            &QuestionWaitEnd::OwnerClosed,
            QuestionWaitPolicy::Closing,
        )?;
        let outbox = controller.outbox()?;
        assert_eq!(outbox.len(), 1, "the partial answer was dropped on close");
        assert_eq!(outbox[0].id, "partial-answer");
        drop(wait);
        let restored = QuestionController::open(root.path(), "session")?;
        assert_eq!(restored.outbox()?.len(), 1);
        Ok(())
    }

    fn draft(requirement: QuestionRequirement) -> QuestionDraft {
        QuestionDraft {
            header: "Layout".into(),
            question: "Which layout?".into(),
            requirement,
            options: vec![crate::plan::PlanQuestionOption {
                recommended: true,
                label: "Tabs".into(),
                description: "Compact navigation".into(),
            }],
        }
    }

    fn command(id: &QuestionId, operation: &str, action: QuestionAction) -> QuestionCommand {
        QuestionCommand {
            operation_id: operation.into(),
            question_id: id.clone(),
            revision: 1,
            action,
        }
    }

    #[test]
    fn chat_continuation_waits_for_required_answers_and_is_admitted_once() -> Result<()> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let controller = QuestionController::open(root.path(), "session")?;
        let ids = controller.ask(
            "mixed",
            "original-request",
            QuestionOrigin::Chat,
            vec![
                draft(QuestionRequirement::Required),
                draft(QuestionRequirement::Optional),
            ],
        )?;
        assert!(controller.ready_chat_continuations()?.is_empty());
        controller.apply(command(
            &ids[0],
            "required-answer",
            QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "Tabs".into(),
                },
            },
        ))?;
        assert!(
            controller.ready_chat_continuations()?.is_empty(),
            "answer must enter the delivery journal first"
        );
        controller.admitted("required-answer")?;
        assert_eq!(
            controller.ready_chat_continuations()?,
            vec!["original-request"]
        );
        controller.chat_continuation_admitted("original-request")?;
        let restored = QuestionController::open(root.path(), "session")?;
        assert!(restored.ready_chat_continuations()?.is_empty());
        restored.chat_continuation_completed("original-request")?;
        restored.cancel_chat(None)?;
        assert!(
            restored.snapshot()?.questions[1].pending(),
            "cancellation retired a completed request's optional question"
        );
        restored.apply(command(
            &ids[1],
            "late-optional",
            QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "Compact".into(),
                },
            },
        ))?;
        restored.admitted("late-optional")?;
        assert!(
            restored.ready_chat_continuations()?.is_empty(),
            "late optional answer must not resume completed work"
        );
        Ok(())
    }

    #[test]
    fn clarification_does_not_answer_and_revision_rejects_stale_choices_atomically() -> Result<()> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let controller = QuestionController::open(root.path(), "session")?;
        let id = controller
            .ask(
                "ask",
                "exchange",
                QuestionOrigin::Chat,
                vec![draft(QuestionRequirement::Optional)],
            )?
            .remove(0);
        controller.apply(command(
            &id,
            "clarify",
            QuestionAction::Clarify {
                text: "Does this change existing sessions?".into(),
            },
        ))?;
        controller.apply(command(
            &id,
            "reply",
            QuestionAction::Reply {
                message_id: "clarify".into(),
                text: "Only new sessions change.".into(),
            },
        ))?;
        assert!(controller.snapshot()?.questions[0].pending());
        let mut revised = draft(QuestionRequirement::Optional);
        revised.options[0].label = "Sidebar".into();
        controller.apply(command(
            &id,
            "revise",
            QuestionAction::Revise { draft: revised },
        ))?;
        let before = serde_json::to_value(controller.snapshot()?)?;
        assert!(
            controller
                .apply(command(
                    &id,
                    "stale",
                    QuestionAction::Answer {
                        answer: QuestionAnswer::Selected {
                            option_id: "1:0".into(),
                            feedback: None
                        }
                    }
                ))
                .is_err()
        );
        assert_eq!(before, serde_json::to_value(controller.snapshot()?)?);
        assert_eq!(controller.snapshot()?.questions[0].discussion.len(), 2);
        assert_eq!(controller.outbox()?.len(), 1);
        Ok(())
    }
}
