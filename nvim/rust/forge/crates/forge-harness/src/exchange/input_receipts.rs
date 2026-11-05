//! Apply durable native input receipts to host state only in the session owner.

use std::{collections::HashMap, sync::Arc};

use forge_provider_api::SubmissionId;
use futures_util::StreamExt;

use super::{
    command::{InputId, InputRevision},
    services::OwnerServices,
};
use crate::{
    input::{InputDelivery, InputDeliveryGuard, InputPurpose, InputRoute, InputUpdate},
    provider::submission::{Submission, SubmissionRetirement, SubmissionState},
};

/// Describe an observed native execution result without transporting native diagnostic text.
#[derive(Clone, Copy)]
pub(crate) enum InputExecutionResult {
    /// Execution returned without an error; acceptance still requires receipt evidence.
    Completed,
    /// Execution was intentionally cancelled; delivery evidence still determines consumption.
    Cancelled,
    /// Execution failed; the receipt determines whether delivery is uncertain or accepted.
    Failed,
}

/// Describe the observed dispatch lane after steering returns.
#[derive(Clone, Copy)]
pub(crate) enum InputDispatchActivity {
    /// Existing native or parent work can still own this steering input.
    Active,
    /// No active turn remains; a prepared receipt proves safe queue eligibility.
    Idle,
}

/// Explain why a prepared input no longer needs its selected dispatch.
#[derive(Clone, Copy)]
pub(crate) enum InputAbandonment {
    /// Intentional cancellation stopped this selected input before dispatch.
    Cancelled,
    /// The question/task revision no longer owns the input.
    Superseded,
    /// Explicit delivery control prevented dispatch.
    Paused,
    /// Host admission retracted the input before native dispatch.
    Retracted,
    /// Configuration collection stopped admission before native dispatch.
    Reconfigured,
}

/// Carry native facts to the owner without allowing the producer to select queue state.
pub(crate) enum InputReceiptFact {
    /// Report a failed preparation/dispatch worker at its original claimed revision.
    WorkerFailed { input: InputRevision },
    /// Bind a prepared receipt to the exact claimed input revision before dispatch.
    Prepared {
        input: InputRevision,
        submission: Arc<Submission>,
    },
    /// Read fresh durable evidence before making acceptance visible to input consumers.
    Acceptance { submission: SubmissionId },
    /// Settle delivery after native reconciliation and execution collection.
    Settled {
        submission: SubmissionId,
        result: InputExecutionResult,
        activity: InputDispatchActivity,
    },
    /// Collect a no-longer-needed prepared receipt without fabricating non-delivery.
    Abandoned {
        submission: SubmissionId,
        reason: InputAbandonment,
    },
}

impl InputReceiptFact {
    /// Get a stable log name without logging input content or provider tokens.
    #[cfg(test)]
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::WorkerFailed { .. } => "input_worker_failed",
            Self::Prepared { .. } => "input_receipt_prepared",
            Self::Acceptance { .. } => "input_receipt_acceptance",
            Self::Settled { .. } => "input_receipt_settled",
            Self::Abandoned { .. } => "input_receipt_abandoned",
        }
    }

    /// Get the host-created receipt identity shared by native and host observations.
    #[cfg(test)]
    pub(super) fn submission(&self) -> Option<&SubmissionId> {
        match self {
            Self::WorkerFailed { .. } => None,
            Self::Prepared { submission, .. } => Some(submission.receipt_id()),
            Self::Acceptance { submission }
            | Self::Settled { submission, .. }
            | Self::Abandoned { submission, .. } => Some(submission),
        }
    }

    /// Get the original claimed input when evidence precedes receipt binding.
    #[cfg(test)]
    pub(super) fn input(&self) -> Option<&InputRevision> {
        match self {
            Self::WorkerFailed { input } | Self::Prepared { input, .. } => Some(input),
            Self::Acceptance { .. } | Self::Settled { .. } | Self::Abandoned { .. } => None,
        }
    }
}

/// Retain receipt-to-input authority until host settlement commits successfully.
#[derive(Default)]
pub(super) struct InputReceipts {
    pending: HashMap<SubmissionId, TrackedInput>,
}

/// Identify independently observed receipt evidence without retaining a caller's wait.
pub(super) enum InputReceiptWake {
    /// The receipt watch changed; read its durable journal before applying it.
    Evidence(SubmissionId),
    /// The receipt watch ended without proving any new delivery state.
    Unavailable(SubmissionId),
}

impl InputReceipts {
    /// Revalidate the exact input and receipt before authorizing a new foreground invocation.
    pub(super) fn capture_dispatch(
        &self,
        input: &InputRevision,
        submission: &SubmissionId,
        services: &OwnerServices,
    ) -> Result<crate::input::InputMessage, InputReceiptError> {
        let tracked = self
            .pending
            .get(submission)
            .ok_or(InputReceiptError::Unknown)?;
        if tracked.input.id != input.id || tracked.input.revision != input.revision {
            return Err(InputReceiptError::StaleInput);
        }
        if !matches!(
            tracked
                .submission
                .record()
                .map_err(InputReceiptError::Receipt)?
                .delivery,
            SubmissionState::Prepared
        ) {
            return Err(InputReceiptError::DispatchUnavailable);
        }
        let message = services
            .input
            .snapshot()
            .map_err(InputReceiptError::Queue)?
            .messages
            .into_iter()
            .find(|message| {
                message.id == input.id.as_str()
                    && message.submission_id() == Some(submission.as_str())
                    && message.delivery() == InputDelivery::Delivering
            })
            .ok_or(InputReceiptError::StaleInput)?;
        if !crate::input::input_purpose_is_current(
            &services.tasks,
            &services.questions,
            &message.purpose,
            &services.root,
            services.observer.session_id(),
        )
        .map_err(InputReceiptError::Authority)?
        {
            return Err(InputReceiptError::StaleInput);
        }
        Ok(message)
    }

    /// Restore only durable receipt evidence; reopening a session never dispatches input.
    pub(super) fn restore(
        &mut self,
        services: &OwnerServices,
    ) -> Result<Vec<InputUpdate>, InputReceiptError> {
        let mut updates = Vec::new();
        let snapshot = services
            .input
            .snapshot()
            .map_err(InputReceiptError::Queue)?;
        for message in snapshot.messages {
            let Some(id) = &message.submission_id() else {
                continue;
            };
            let submission = services
                .submissions
                .submission(id)
                .map_err(InputReceiptError::Receipt)?;
            let record = submission.record().map_err(InputReceiptError::Receipt)?;
            if !matches!(
                record.delivery,
                SubmissionState::Accepted { .. } | SubmissionState::Sending { .. }
            ) {
                continue;
            }
            if record.request.harness_session_id != services.observer.session_id()
                || record.input_message_id.as_deref() != Some(message.id.as_str())
            {
                return Err(InputReceiptError::IdentityMismatch);
            }
            let id = submission.receipt_id().clone();
            let acceptance = submission.acceptance();
            let task_permit = record.task_receipt.clone();
            let route = message.route().ok_or(InputReceiptError::IdentityMismatch)?;
            self.pending.insert(
                id.clone(),
                TrackedInput {
                    input: InputRevision {
                        id: InputId::new(message.id)
                            .map_err(|_| InputReceiptError::IdentityMismatch)?,
                        revision: message.revision,
                    },
                    purpose: message.purpose,
                    task_permit,
                    route,
                    submission,
                    binding: ReceiptBinding::Bound,
                    acceptance,
                    observation: ReceiptObservation::Watching,
                    work: ReceiptWork::Unconfirmed,
                    accepted: false,
                },
            );
            if matches!(record.delivery, SubmissionState::Accepted { .. })
                && let Some(update) =
                    self.apply(InputReceiptFact::Acceptance { submission: id }, services)?
            {
                updates.push(update);
            }
        }
        Ok(updates)
    }

    /// Await independent receipt evidence even after the original input worker returns.
    pub(super) async fn next(&self) -> InputReceiptWake {
        let mut waiting = futures_util::stream::FuturesUnordered::new();
        for (id, tracked) in &self.pending {
            if !matches!(tracked.observation, ReceiptObservation::Watching) {
                continue;
            }
            let id = id.clone();
            // A cancelled select or a ready sibling must not consume the owner's watch version.
            let mut acceptance = tracked.acceptance.clone();
            waiting.push(async move {
                match acceptance.changed().await {
                    Ok(()) => InputReceiptWake::Evidence(id),
                    Err(_) => InputReceiptWake::Unavailable(id),
                }
            });
        }
        match waiting.next().await {
            Some(wake) => wake,
            None => std::future::pending().await,
        }
    }

    /// Retain failed evidence without repeatedly polling a closed or uncommittable observation.
    pub(super) fn suspend_observation(&mut self, id: &SubmissionId) {
        if let Some(tracked) = self.pending.get_mut(id) {
            tracked.observation = ReceiptObservation::Suspended;
        }
    }

    /// Apply one correlated receipt fact and retain authority if its commit fails.
    pub(super) fn apply(
        &mut self,
        fact: InputReceiptFact,
        services: &OwnerServices,
    ) -> Result<Option<InputUpdate>, InputReceiptError> {
        match fact {
            InputReceiptFact::WorkerFailed { input } => {
                let registered = self
                    .pending
                    .iter()
                    .find(|(_, tracked)| {
                        tracked.input.id == input.id && tracked.input.revision == input.revision
                    })
                    .map(|(id, _)| id.clone());
                if let Some(submission) = registered {
                    return self.apply(
                        InputReceiptFact::Settled {
                            submission,
                            result: InputExecutionResult::Failed,
                            activity: InputDispatchActivity::Active,
                        },
                        services,
                    );
                }
                services
                    .input
                    .delivery_if_current(
                        input.id.as_str(),
                        InputDeliveryGuard::Claimed(input.revision),
                        Some(InputDelivery::Failed),
                        None,
                        Some("Input preparation failed before provider dispatch".into()),
                    )
                    .map_err(InputReceiptError::Queue)
            }
            InputReceiptFact::Prepared { input, submission } => {
                self.register(input, submission, services).map(Some)
            }
            InputReceiptFact::Acceptance { submission } => {
                self.observe_one(&submission, services)?;
                let tracked = self
                    .pending
                    .get_mut(&submission)
                    .ok_or(InputReceiptError::Unknown)?;
                if !tracked.accepted {
                    return Err(InputReceiptError::AcceptanceUnconfirmed);
                }
                let unconfirmed = matches!(tracked.work, ReceiptWork::Unconfirmed);
                let delivery = if unconfirmed {
                    None
                } else {
                    Some(InputDelivery::Accepted)
                };
                let update = tracked.commit_delivery(services, delivery, None);
                if unconfirmed
                    && (update.is_ok() || matches!(&update, Err(InputReceiptError::StaleInput)))
                {
                    self.pending.remove(&submission);
                }
                let update = match update {
                    Ok(update) => {
                        if let Some(tracked) = self.pending.get_mut(&submission) {
                            tracked.acceptance.borrow_and_update();
                            tracked.observation = ReceiptObservation::Watching;
                        }
                        Some(update)
                    }
                    Err(InputReceiptError::StaleInput) => {
                        self.pending.remove(&submission);
                        None
                    }
                    Err(error) => return Err(error),
                };
                Ok(update)
            }
            InputReceiptFact::Settled {
                submission,
                result,
                activity,
            } => {
                self.observe_one(&submission, services)?;
                let tracked = self
                    .pending
                    .get_mut(&submission)
                    .ok_or(InputReceiptError::Unknown)?;
                let retirement = tracked
                    .submission
                    .retire_prepared()
                    .map_err(|error| InputReceiptError::Receipt(error.into()))?;
                if retirement == SubmissionRetirement::Accepted {
                    tracked.observe_acceptance(services)?;
                }
                let delivery = match retirement {
                    SubmissionRetirement::Accepted => None,
                    SubmissionRetirement::Cancelled
                        if matches!(result, InputExecutionResult::Cancelled) =>
                    {
                        None
                    }
                    SubmissionRetirement::Cancelled
                        if tracked.route == InputRoute::Steering
                            && matches!(activity, InputDispatchActivity::Idle) =>
                    {
                        Some(InputDelivery::Queued)
                    }
                    SubmissionRetirement::Cancelled => Some(InputDelivery::Failed),
                    SubmissionRetirement::Sending => Some(InputDelivery::Unknown),
                };
                if delivery == Some(InputDelivery::Failed) {
                    tracked.pause_task(services)?;
                }
                let error = match (&delivery, result) {
                    (Some(InputDelivery::Queued), _)
                    | (None, InputExecutionResult::Completed | InputExecutionResult::Cancelled) => {
                        None
                    }
                    (Some(InputDelivery::Unknown), _) => {
                        Some("Delivery unconfirmed. Reconcile before retrying.".into())
                    }
                    (Some(InputDelivery::Failed), _) => {
                        Some("Provider did not confirm message acceptance".into())
                    }
                    (None, InputExecutionResult::Failed) => {
                        Some("Input execution failed after message acceptance".into())
                    }
                    (Some(InputDelivery::Delivering | InputDelivery::Accepted), _) => {
                        unreachable!("settlement selects only terminal or safely queued delivery")
                    }
                };
                let mut update = tracked.commit_delivery(services, delivery, error)?;
                if matches!(result, InputExecutionResult::Cancelled)
                    && retirement == SubmissionRetirement::Cancelled
                {
                    update.retracted = Some(tracked.input.id.as_str().into());
                }
                if retirement == SubmissionRetirement::Sending {
                    tracked.work = ReceiptWork::Unconfirmed;
                } else {
                    self.pending.remove(&submission);
                }
                Ok(Some(update))
            }
            InputReceiptFact::Abandoned { submission, reason } => {
                self.observe_one(&submission, services)?;
                let tracked = self
                    .pending
                    .get_mut(&submission)
                    .ok_or(InputReceiptError::Unknown)?;
                let retirement = tracked
                    .submission
                    .retire_prepared()
                    .map_err(|error| InputReceiptError::Receipt(error.into()))?;
                if retirement == SubmissionRetirement::Accepted {
                    tracked.observe_acceptance(services)?;
                }
                let delivery = match retirement {
                    SubmissionRetirement::Accepted => None,
                    SubmissionRetirement::Sending => Some(InputDelivery::Unknown),
                    SubmissionRetirement::Cancelled => match reason {
                        InputAbandonment::Cancelled
                        | InputAbandonment::Superseded
                        | InputAbandonment::Retracted => None,
                        InputAbandonment::Paused | InputAbandonment::Reconfigured => {
                            Some(InputDelivery::Queued)
                        }
                    },
                };
                let retracted = matches!(
                    reason,
                    InputAbandonment::Cancelled | InputAbandonment::Retracted
                ) && retirement == SubmissionRetirement::Cancelled;
                let mut update = tracked.commit_delivery(
                    services,
                    delivery,
                    (retirement == SubmissionRetirement::Sending)
                        .then(|| "Delivery unconfirmed. Reconcile before retrying.".into()),
                )?;
                if retracted {
                    update.retracted = Some(tracked.input.id.as_str().into());
                }
                if retirement == SubmissionRetirement::Sending {
                    tracked.work = ReceiptWork::Unconfirmed;
                } else {
                    self.pending.remove(&submission);
                }
                Ok(Some(update))
            }
        }
    }

    /// Apply receipt evidence before terminal foreground reducers can inspect task progress.
    pub(super) fn observe_acceptance(
        &mut self,
        services: &OwnerServices,
    ) -> Result<(), InputReceiptError> {
        for tracked in self.pending.values_mut() {
            tracked.observe_acceptance(services)?;
        }
        Ok(())
    }

    /// Register only a receipt for this session and the exact claimed input content.
    fn register(
        &mut self,
        input: InputRevision,
        submission: Arc<Submission>,
        services: &OwnerServices,
    ) -> Result<InputUpdate, InputReceiptError> {
        let record = submission.record().map_err(InputReceiptError::Receipt)?;
        if record.request.harness_session_id != services.observer.session_id()
            || record.input_message_id.as_deref() != Some(input.id.as_str())
        {
            return Err(InputReceiptError::IdentityMismatch);
        }
        self.release_reconciled(services)?;
        if self.pending.contains_key(submission.receipt_id()) {
            return Err(InputReceiptError::AlreadyRegistered);
        }
        let message = services
            .input
            .snapshot()
            .map_err(InputReceiptError::Queue)?
            .messages
            .into_iter()
            .find(|message| {
                message.id == input.id.as_str()
                    && message.revision == input.revision
                    && message.delivery() == InputDelivery::Delivering
            })
            .ok_or(InputReceiptError::StaleInput)?;
        let route = message.route().ok_or(InputReceiptError::StaleInput)?;
        let id = submission.receipt_id().clone();
        let acceptance = submission.acceptance();
        let task_permit = match record.task_receipt.clone() {
            Some(permit) => Some(permit),
            None => captured_task_receipt_authority(&message.purpose, services)?,
        };
        if let Some(permit) = &task_permit {
            submission
                .bind_task_receipt(permit.clone())
                .map_err(InputReceiptError::Receipt)?;
        }
        self.pending.insert(
            id.clone(),
            TrackedInput {
                input,
                purpose: message.purpose,
                task_permit,
                route,
                submission,
                binding: ReceiptBinding::Claimed,
                acceptance,
                observation: ReceiptObservation::Watching,
                work: ReceiptWork::Running,
                accepted: false,
            },
        );
        // Keep the receipt even when task activation or queue binding cannot commit.
        let tracked = self
            .pending
            .get_mut(&id)
            .expect("inserted receipt is retained by its owner");
        tracked.observe_acceptance(services)?;
        let update = tracked.commit_delivery(services, Some(InputDelivery::Delivering), None)?;
        Ok(update)
    }

    /// Retire uncertain-worker watches only after receipt proof and an explicit queue transition.
    fn release_reconciled(&mut self, services: &OwnerServices) -> Result<(), InputReceiptError> {
        let snapshot = services
            .input
            .snapshot()
            .map_err(InputReceiptError::Queue)?;
        let mut retired = Vec::new();
        for (id, tracked) in &mut self.pending {
            if !matches!(tracked.work, ReceiptWork::Unconfirmed) {
                continue;
            }
            let record = tracked
                .submission
                .record()
                .map_err(InputReceiptError::Receipt)?;
            if matches!(record.delivery, SubmissionState::Sending { .. }) {
                continue;
            }
            let still_bound = snapshot.messages.iter().any(|message| {
                message.id == tracked.input.id.as_str()
                    && message.submission_id() == Some(id.as_str())
            });
            if !still_bound {
                tracked.observe_acceptance(services)?;
                retired.push(id.clone());
            }
        }
        for id in retired {
            self.pending.remove(&id);
        }
        Ok(())
    }

    /// Revalidate durable acceptance independently of notification ordering.
    fn observe_one(
        &mut self,
        id: &SubmissionId,
        services: &OwnerServices,
    ) -> Result<(), InputReceiptError> {
        self.pending
            .get_mut(id)
            .ok_or(InputReceiptError::Unknown)?
            .observe_acceptance(services)
    }
}

/// Capture the task's observation fence before native callbacks can advance its control revision.
fn captured_task_receipt_authority(
    purpose: &InputPurpose,
    services: &OwnerServices,
) -> Result<Option<crate::task::TaskReceiptPermit>, InputReceiptError> {
    let revision = match purpose {
        InputPurpose::TaskEntry { revision, .. }
        | InputPurpose::TaskContinuation { revision, .. } => revision,
        _ => return Ok(None),
    };
    Ok(services
        .tasks
        .current()
        .map_err(InputReceiptError::Task)?
        .filter(|task| &task.revision == revision)
        .map(|task| task.permit().receipt()))
}

/// Retain the exact task purpose selected before native dispatch.
struct TrackedInput {
    input: InputRevision,
    purpose: InputPurpose,
    task_permit: Option<crate::task::TaskReceiptPermit>,
    route: InputRoute,
    submission: Arc<Submission>,
    binding: ReceiptBinding,
    acceptance: tokio::sync::watch::Receiver<bool>,
    observation: ReceiptObservation,
    work: ReceiptWork,
    accepted: bool,
}

/// Keep uncertain delivery observed after its dispatch worker has finished.
enum ReceiptWork {
    Running,
    Unconfirmed,
}

/// Keep failed observation owned without a permanently-ready retry loop.
enum ReceiptObservation {
    Watching,
    Suspended,
}

/// Distinguish authority before and after the queue binds its native receipt.
enum ReceiptBinding {
    Claimed,
    Bound,
}

impl TrackedInput {
    /// Atomically reject a stale worker before it can overwrite a newer input attempt.
    fn commit_delivery(
        &mut self,
        services: &OwnerServices,
        delivery: Option<InputDelivery>,
        error: Option<String>,
    ) -> Result<InputUpdate, InputReceiptError> {
        let guard = match self.binding {
            ReceiptBinding::Claimed => InputDeliveryGuard::Claimed(self.input.revision),
            ReceiptBinding::Bound => InputDeliveryGuard::Receipt(self.submission.receipt_id()),
        };
        let update = services
            .input
            .delivery_if_current(
                self.input.id.as_str(),
                guard,
                delivery,
                Some(self.submission.id().into()),
                error,
            )
            .map_err(InputReceiptError::Queue)?
            .ok_or(InputReceiptError::StaleInput)?;
        self.binding = ReceiptBinding::Bound;
        Ok(update)
    }

    /// Commit task activation before exposing native acceptance to host reducers.
    fn observe_acceptance(&mut self, services: &OwnerServices) -> Result<(), InputReceiptError> {
        if self.accepted
            || !matches!(
                self.submission
                    .record()
                    .map_err(InputReceiptError::Receipt)?
                    .delivery,
                SubmissionState::Accepted { .. }
            )
        {
            return Ok(());
        }
        match &self.purpose {
            InputPurpose::TaskEntry { revision, id, .. } => {
                let mut store = crate::storage::SqliteStore::open(&services.root)
                    .map_err(InputReceiptError::Task)?;
                match &self.task_permit {
                    Some(permit) => services.tasks.acknowledge_dispatched_input(
                        &mut store,
                        permit,
                        id,
                        crate::task::TaskReceiptInput::Entry,
                        now_ms()?,
                    ),
                    None => services
                        .tasks
                        .acknowledge_entry(&mut store, revision, id, now_ms()?),
                }
                .map_err(InputReceiptError::Task)?;
            }
            InputPurpose::TaskContinuation { revision, id, .. } => {
                let mut store = crate::storage::SqliteStore::open(&services.root)
                    .map_err(InputReceiptError::Task)?;
                match &self.task_permit {
                    Some(permit) => services.tasks.acknowledge_dispatched_input(
                        &mut store,
                        permit,
                        id,
                        crate::task::TaskReceiptInput::Continuation,
                        now_ms()?,
                    ),
                    None => {
                        services
                            .tasks
                            .acknowledge_continuation(&mut store, revision, id, now_ms()?)
                    }
                }
                .map_err(InputReceiptError::Task)?;
            }
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::Question { .. }
            | InputPurpose::QuestionContinuation { .. }
            | InputPurpose::WorkflowContinuation { .. } => {}
        }
        self.accepted = true;
        Ok(())
    }

    /// Pause only the still-current task whose input could not be delivered.
    fn pause_task(&self, services: &OwnerServices) -> Result<(), InputReceiptError> {
        let revision = match &self.purpose {
            InputPurpose::TaskEntry { revision, .. }
            | InputPurpose::TaskContinuation { revision, .. } => revision,
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::Question { .. }
            | InputPurpose::QuestionContinuation { .. }
            | InputPurpose::WorkflowContinuation { .. } => return Ok(()),
        };
        let mut store =
            crate::storage::SqliteStore::open(&services.root).map_err(InputReceiptError::Task)?;
        match services.tasks.change(
            &mut store,
            Some(revision),
            crate::task::TaskChange::Pause,
            now_ms()?,
        ) {
            Ok(_) => Ok(()),
            Err(error) if error.is::<crate::task::StaleTask>() => Ok(()),
            Err(error) => Err(InputReceiptError::Task(error)),
        }
    }
}

/// Distinguish invalid receipt authority, unavailable observation, and failed durable host commits.
#[derive(Debug, thiserror::Error)]
pub(crate) enum InputReceiptError {
    /// Committed workflow authority could not be read before dispatch or settlement.
    #[error("input workflow authority is unavailable")]
    Authority(#[source] crate::input::InputAuthorityError),
    /// Receipt evidence already forbids starting another invocation for this input.
    #[error("input receipt does not authorize a new foreground dispatch")]
    DispatchUnavailable,
    /// Intake ended before this fact was admitted.
    #[error("exchange owner closed before input receipt intake")]
    #[cfg(test)]
    OwnerClosed,
    /// The owner retains admitted work even when its acknowledgment cannot be observed.
    #[error("input receipt application was not observed")]
    #[cfg(test)]
    ObservationLost,
    /// This owner has no registered dispatch for the given receipt.
    #[error("input receipt is not registered with this exchange owner")]
    Unknown,
    /// A producer tried to replace an already-bound receipt.
    #[error("input receipt is already registered")]
    AlreadyRegistered,
    /// Native receipt authority does not match this session and input.
    #[error("input receipt identity does not match the selected session input")]
    IdentityMismatch,
    /// Input changed before preparation could bind its receipt.
    #[error("input changed before receipt registration")]
    StaleInput,
    /// Notification alone does not certify acceptance.
    #[error("input acceptance is not confirmed by its durable receipt")]
    AcceptanceUnconfirmed,
    /// The existing receipt journal could not supply durable evidence.
    #[error("input receipt evidence is unavailable")]
    Receipt(#[source] anyhow::Error),
    /// A queue commit failed; the owner retains the registered receipt for reconciliation.
    #[error("input delivery state could not be committed")]
    Queue(#[source] anyhow::Error),
    /// A revision-scoped task acknowledgment or failure settlement did not commit.
    #[error("input task state could not be committed")]
    Task(#[source] anyhow::Error),
    /// The host clock cannot produce the persisted event timestamp.
    #[error("input receipt timestamp is unavailable")]
    Clock,
}

impl InputReceiptError {
    /// Fence further execution when durable receipt or host state cannot be established.
    pub(super) fn fences_execution(&self) -> bool {
        match self {
            Self::Receipt(_)
            | Self::Queue(_)
            | Self::Task(_)
            | Self::Authority(_)
            | Self::Clock => true,
            #[cfg(test)]
            Self::OwnerClosed | Self::ObservationLost => false,
            Self::Unknown
            | Self::AlreadyRegistered
            | Self::IdentityMismatch
            | Self::StaleInput
            | Self::AcceptanceUnconfirmed => false,
            Self::DispatchUnavailable => false,
        }
    }
}

/// Create the checked host timestamp used by existing task journal reducers.
fn now_ms() -> Result<i64, InputReceiptError> {
    let elapsed = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|_| InputReceiptError::Clock)?;
    i64::try_from(elapsed.as_millis()).map_err(|_| InputReceiptError::Clock)
}
