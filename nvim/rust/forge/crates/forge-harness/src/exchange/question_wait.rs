//! Retain question-wait observation and retirement independently of caller intake capacity.

use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
};

use futures_util::{FutureExt, StreamExt, future::BoxFuture, stream::FuturesUnordered};
use tokio::{
    sync::{oneshot, watch},
    time::Instant,
};

use crate::question::{
    LocalQuestionWait, OwnedQuestionWait, QuestionId, QuestionWaitEnd, QuestionWaitError,
    QuestionWaitPolicy, QuestionWaitReport,
};
use crate::trace::TraceStore;

/// Production limit on concurrently registered waits for one session owner.
const MAX_WAITS: usize = 1024;
const RETIREMENT_BUDGET: usize = 32;

/// Identify one process-local wait registration, independently of its persistent question identities.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct WaitId(uuid::Uuid);

/// Distinguish a live owner from the finite retirement of all its remaining wait registrations.
enum WaitOwnerState {
    Open,
    Closing,
}

/// Seal observation before shutdown can synthesize retirement of an otherwise live guard.
enum ReportAdmission {
    Open,
    Reported,
    Sealed,
}

/// Report answer observation or abandonment through a reserved, one-use resource channel.
/// Dropping this capability cannot be rejected by a full caller command/event queue.
#[must_use = "the question wait must retain this retirement capability until it ends"]
pub(crate) struct QuestionWaitRetirement {
    report: Option<oneshot::Sender<WaitObservation>>,
    owner_closed: watch::Receiver<bool>,
    admission: Arc<Mutex<ReportAdmission>>,
}

impl QuestionWaitRetirement {
    /// Record exactly the answers returned to the tool observer; this does not claim native SDK delivery.
    pub(crate) async fn observed(
        &mut self,
        questions: &[crate::question::QuestionRecord],
    ) -> Result<Vec<crate::question::QuestionRecord>, QuestionWaitError> {
        let (reply, receipt) = oneshot::channel();
        {
            let mut admission = self
                .admission
                .lock()
                .map_err(|_| QuestionWaitError::RegistryUnavailable)?;
            match *admission {
                ReportAdmission::Open => {}
                ReportAdmission::Sealed => return Err(QuestionWaitError::OwnerClosed),
                ReportAdmission::Reported => return Err(QuestionWaitError::ObservationMismatch),
            }
            let report = self
                .report
                .take()
                .ok_or(QuestionWaitError::ObservationMismatch)?;
            if report
                .send(WaitObservation {
                    report: QuestionWaitReport::observed(questions),
                    reply,
                })
                .is_err()
            {
                *admission = ReportAdmission::Sealed;
                return Err(QuestionWaitError::OwnerClosed);
            }
            *admission = ReportAdmission::Reported;
        }
        tokio::select! {
            biased;
            result = receipt => result.map_err(|_| QuestionWaitError::ObservationLost)?,
            _ = self.closed() => Err(QuestionWaitError::OwnerClosed),
        }
    }

    /// End a user-answer wait when its owning driver closes or disappears.
    pub(crate) async fn closed(&mut self) {
        let _ = self.owner_closed.wait_for(|closed| *closed).await;
    }
}

impl Drop for QuestionWaitRetirement {
    /// Publish abandonment before releasing its report-admission gate; a closing owner cannot discard it.
    fn drop(&mut self) {
        if let Ok(mut admission) = self.admission.lock() {
            if matches!(*admission, ReportAdmission::Open) {
                *admission = ReportAdmission::Reported;
            }
            drop(self.report.take());
        }
    }
}

/// Await durable logical answer retention before a tool may report successful completion.
struct WaitObservation {
    report: QuestionWaitReport,
    reply: oneshot::Sender<Result<Vec<crate::question::QuestionRecord>, QuestionWaitError>>,
}

/// Keep the registered IDs until the owner commits the associated retirement fact.
struct Registration {
    questions: Vec<QuestionId>,
    admission: Arc<Mutex<ReportAdmission>>,
}

/// Keep a completed observation available across failed storage writes without re-observing its channel.
struct Retirement {
    id: WaitId,
    end: QuestionWaitEnd,
    reply: Option<oneshot::Sender<Result<Vec<crate::question::QuestionRecord>, QuestionWaitError>>>,
}

impl Retirement {
    /// End a live response wait without dropping the report that still requires durable retirement.
    fn release_uncommitted_observer(&mut self) {
        if let Some(reply) = self.reply.take() {
            let _ = reply.send(Err(QuestionWaitError::StateUnavailable));
        }
    }
}

/// Keep the exact wait identity with a failed owner retirement, including failures before persistence.
#[derive(Debug, thiserror::Error)]
#[error("question wait {wait_id} retirement failed: {error}")]
pub(super) struct QuestionWaitFailure {
    wait_id: uuid::Uuid,
    #[source]
    error: QuestionWaitError,
}

impl QuestionWaitFailure {
    /// Get the process-local resource identity retained through retries.
    pub(super) fn wait_id(&self) -> uuid::Uuid {
        self.wait_id
    }

    /// Get the normalized category and original typed storage/observation cause.
    pub(super) fn error(&self) -> &QuestionWaitError {
        &self.error
    }
}

/// Own all production answer-destination registration and retirement transitions for one session.
pub(super) struct QuestionWaits {
    capacity: usize,
    state: WaitOwnerState,
    active: HashMap<WaitId, Registration>,
    observations: FuturesUnordered<BoxFuture<'static, Retirement>>,
    retained: VecDeque<Retirement>,
    owner_closed: watch::Sender<bool>,
    retry_at: Option<Instant>,
}

impl Default for QuestionWaits {
    /// Create an empty, bounded resource owner before admitting any waiting question tools.
    fn default() -> Self {
        Self::with_capacity(MAX_WAITS)
    }
}

impl QuestionWaits {
    /// Create an empty owner that admits at most `capacity` concurrent waits.
    pub(super) fn with_capacity(capacity: usize) -> Self {
        Self {
            capacity,
            state: WaitOwnerState::Open,
            active: HashMap::new(),
            observations: FuturesUnordered::new(),
            retained: VecDeque::new(),
            owner_closed: watch::channel(false).0,
            retry_at: None,
        }
    }

    /// Get whether another waiting destination can be installed at this owner update.
    /// Keep excess callbacks in the owner's staged queue until retirement frees capacity.
    pub(super) fn has_capacity(&self) -> bool {
        self.active.len() < self.capacity
    }

    /// Publish and install one wait only after proving capacity and validating its captured scope.
    pub(super) fn begin(
        &mut self,
        creation: super::QuestionCreation,
        services: &super::OwnerServices,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) -> Result<OwnedQuestionWait, super::QuestionEventError> {
        match self.state {
            WaitOwnerState::Closing => return Err(super::QuestionEventError::OwnerClosed),
            WaitOwnerState::Open => {}
        }
        if !self.has_capacity() {
            return Err(super::QuestionEventError::WaitCapacity);
        }
        let local = creation.prepare_wait(&services.questions, services)?;
        Ok(self.register(local, trace, session, step))
    }

    /// Apply ready retirement facts before another event can interpret answer-destination ownership.
    pub(super) fn update(
        &mut self,
        services: &super::OwnerServices,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) -> Result<(), QuestionWaitFailure> {
        if self
            .retry_at
            .is_some_and(|deadline| Instant::now() < deadline)
        {
            return Ok(());
        }
        self.retry_at = None;
        let result = self.advance(services, trace, session, step);
        if result.is_err() {
            self.retry_at = Some(Instant::now() + std::time::Duration::from_millis(250));
        }
        result
    }

    /// Commit a bounded ready prefix without discarding a fact whose retirement fails.
    fn advance(
        &mut self,
        services: &super::OwnerServices,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) -> Result<(), QuestionWaitFailure> {
        for _ in 0..RETIREMENT_BUDGET {
            if self.retained.is_empty() {
                match self.observations.next().now_or_never() {
                    Some(Some(retired)) => self.retained.push_back(retired),
                    Some(None) | None => {
                        if matches!(self.state, WaitOwnerState::Closing) {
                            let mut closing = None;
                            for (id, registration) in &self.active {
                                let mut admission =
                                    registration.admission.lock().map_err(|_| {
                                        QuestionWaitFailure {
                                            wait_id: id.0,
                                            error: QuestionWaitError::RegistryUnavailable,
                                        }
                                    })?;
                                match *admission {
                                    ReportAdmission::Open => {
                                        *admission = ReportAdmission::Sealed;
                                        closing = Some(*id);
                                        break;
                                    }
                                    ReportAdmission::Reported | ReportAdmission::Sealed => {}
                                }
                            }
                            if let Some(id) = closing {
                                self.retained.push_back(Retirement {
                                    id,
                                    end: QuestionWaitEnd::OwnerClosed,
                                    reply: None,
                                });
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
            let retired = self
                .retained
                .front()
                .expect("a ready or closing retirement was retained before applying it");
            let Some(registration) = self.active.get(&retired.id) else {
                self.retained.pop_front();
                continue;
            };
            let policy = match self.state {
                WaitOwnerState::Open => QuestionWaitPolicy::Live,
                WaitOwnerState::Closing => QuestionWaitPolicy::Closing,
            };
            let result =
                match services
                    .questions
                    .retire_wait(&registration.questions, &retired.end, policy)
                {
                    Ok(result) => result,
                    Err(error) => {
                        let wait_id = retired.id.0;
                        // Keep the resource fact for reconciliation, but do not make a caller wait for storage recovery.
                        self.retained
                            .front_mut()
                            .expect("failed retirement remains queued")
                            .release_uncommitted_observer();
                        return Err(QuestionWaitFailure { wait_id, error });
                    }
                };
            trace.record(
                session,
                "exchange.question_wait.retired",
                serde_json::json!({
                    "wait_id":retired.id.0.to_string(),"question_ids":registration.questions,"question_count":registration.questions.len(),
                    "outcome":retired.end.name(),"update_step":step,
                }),
            );
            self.active.remove(&retired.id);
            if let Some(retired) = self.retained.pop_front()
                && let Some(reply) = retired.reply
            {
                let _ = reply.send(result.ok_or(QuestionWaitError::ObservationMismatch));
            }
        }
        Ok(())
    }

    /// Observe resource facts without waiting for caller intake space or mutating question state.
    pub(super) async fn next(&mut self) {
        if let Some(deadline) = self.retry_at {
            tokio::time::sleep_until(deadline).await;
            return;
        }
        if !self.retained.is_empty()
            || matches!(self.state, WaitOwnerState::Closing) && !self.active.is_empty()
        {
            tokio::task::yield_now().await;
            return;
        }
        if self.observations.is_empty() {
            std::future::pending::<()>().await;
            return;
        }
        if let Some(retired) = self.observations.next().await {
            self.retained.push_back(retired);
        }
    }

    /// Close answer observation immediately; the owner still retires each registration at an update boundary.
    pub(super) fn close(&mut self) {
        self.state = WaitOwnerState::Closing;
        for (id, registration) in &self.active {
            if let Ok(mut admission) = registration.admission.lock() {
                match *admission {
                    ReportAdmission::Open => {
                        *admission = ReportAdmission::Sealed;
                        self.retained.push_back(Retirement {
                            id: *id,
                            end: QuestionWaitEnd::OwnerClosed,
                            reply: None,
                        });
                    }
                    ReportAdmission::Reported | ReportAdmission::Sealed => {}
                }
            }
        }
        self.owner_closed.send_replace(true);
    }

    /// Get whether all accepted wait registrations have committed their retirement outcome.
    pub(super) fn is_idle(&self) -> bool {
        self.active.is_empty()
    }

    /// Transfer local rollback custody into one owner registration before its guard reaches an external waiter.
    fn register(
        &mut self,
        local: LocalQuestionWait,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) -> OwnedQuestionWait {
        let id = WaitId(uuid::Uuid::new_v4());
        let questions = local.question_ids().to_vec();
        let (report, observation) = oneshot::channel::<WaitObservation>();
        let admission = Arc::new(Mutex::new(ReportAdmission::Open));
        self.active.insert(
            id,
            Registration {
                questions: questions.clone(),
                admission: admission.clone(),
            },
        );
        self.observations.push(
            async move {
                match observation.await {
                    Ok(observation) => Retirement {
                        id,
                        end: QuestionWaitEnd::Observed(observation.report),
                        reply: Some(observation.reply),
                    },
                    Err(_) => Retirement {
                        id,
                        end: QuestionWaitEnd::Abandoned,
                        reply: None,
                    },
                }
            }
            .boxed(),
        );
        trace.record(
            session,
            "exchange.question_wait.registered",
            serde_json::json!({
                "wait_id":id.0.to_string(),"question_ids":questions,"question_count":questions.len(),"update_step":step,
            }),
        );
        local.into_owned(QuestionWaitRetirement {
            report: Some(report),
            owner_closed: self.owner_closed.subscribe(),
            admission,
        })
    }
}

impl Drop for QuestionWaits {
    /// Release blocked observation on abrupt owner loss without mutating state outside the update owner.
    fn drop(&mut self) {
        self.owner_closed.send_replace(true);
    }
}

#[cfg(test)]
mod tests {}
