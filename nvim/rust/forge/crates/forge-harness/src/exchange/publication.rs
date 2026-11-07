//! Retain committed session notifications independently of their transient outbound channel.

use std::{
    num::{NonZeroU16, NonZeroU64},
    path::Path,
    sync::{
        Arc, Mutex, Weak,
        atomic::{AtomicBool, Ordering},
    },
};

use forge_protocol::{message::SessionEvent, outbound::MessageSender};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::{Deserialize, Serialize};

/// Identify one immutable producer fact whose publication may be recovered after a failed acknowledgment.
#[derive(Clone, Serialize)]
#[serde(tag = "kind", content = "identity", rename_all = "snake_case")]
pub(super) enum PublicationOrigin {
    /// The canonical admission receipt for this question reply or chat-continuation message.
    QuestionInput(super::command::InputId),
    /// The committed question snapshot at this session-local revision.
    Questions(u64),
    /// The immutable result of this revision-scoped answer, independently of caller observation.
    PlanQuestionAnswer(String),
    /// One owner-held update retains this identity across failed append observations.
    OwnerUpdate(String),
}

/// Identify one committed publication within its owning session, across host epochs.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "u64", into = "u64")]
pub(crate) struct PublicationSequence(NonZeroU64);

impl PublicationSequence {
    /// Get the positive session-local sequence used in logs and delivery cursors.
    pub(crate) fn get(self) -> u64 {
        self.0.get()
    }

    /// Get the SQLite representation guaranteed by boundary validation.
    fn sql(self) -> i64 {
        i64::try_from(self.get()).expect("publication sequences fit positive SQLite integers")
    }
}

impl TryFrom<u64> for PublicationSequence {
    type Error = PublicationError;
    /// Reject zero and out-of-range persisted or caller-provided delivery cursors.
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        if i64::try_from(value).is_err() {
            return Err(PublicationError::InvalidSequence);
        }
        NonZeroU64::new(value)
            .map(Self)
            .ok_or(PublicationError::InvalidSequence)
    }
}

impl From<PublicationSequence> for u64 {
    /// Preserve a committed publication identity without creating a new sequence.
    fn from(sequence: PublicationSequence) -> Self {
        sequence.get()
    }
}

/// Distinguish local persistence, outbound admission, and an interrupted delivery observation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub(crate) enum PublicationState {
    /// The complete event is retained and no transport admission has been established.
    Pending,
    /// Delivery was authorized; interruption cannot prove whether its frame entered transport.
    Offering,
    /// The complete frame entered ordered outbound delivery; this is not a UI acknowledgment.
    Admitted,
    /// Host interruption ended an offer without durable admission evidence; never blindly resend it.
    Uncertain,
}

/// Keep the committed event and its delivery evidence together for later delivery or recovery.
pub(crate) struct RetainedPublication {
    sequence: PublicationSequence,
    event: SessionEvent,
    state: PublicationState,
}

/// Identify the durable owner publication shared by live delivery and reconnect replay.
#[derive(Serialize)]
struct PublicationStamp {
    sequence: PublicationSequence,
}

/// Capture a bounded publication prefix and its stable reconciliation boundary.
pub(crate) struct PublicationSnapshot {
    records: Vec<RetainedPublication>,
    high_water: Option<PublicationSequence>,
    receipt: PublicationReconciliation,
}

impl PublicationSnapshot {
    /// Get the final committed sequence visible when this page was captured.
    pub(crate) fn high_water(&self) -> Option<PublicationSequence> {
        self.high_water
    }

    /// Separate retained content from the evidence required after its complete outbound admission.
    pub(crate) fn into_parts(self) -> (Vec<RetainedPublication>, PublicationReconciliation) {
        (self.records, self.receipt)
    }
}

/// Authorize reconciliation only for the exact prefix read from this session's store.
pub(crate) struct PublicationReconciliation {
    session: String,
    after: Option<PublicationSequence>,
    through: Option<PublicationSequence>,
}

impl RetainedPublication {
    /// Get this event's durable session-local identity.
    pub(crate) fn sequence(&self) -> PublicationSequence {
        self.sequence
    }
    /// Get the complete event without exposing mutation of committed content.
    pub(crate) fn event(&self) -> &SessionEvent {
        &self.event
    }
    /// Copy the retained event with its delivery identity without changing stored producer content.
    pub(crate) fn delivery_event(&self) -> SessionEvent {
        stamp_event(self.event.clone(), self.sequence)
    }
    /// Get delivery evidence without inferring remote receipt from channel admission.
    pub(crate) fn state(&self) -> PublicationState {
        self.state
    }
}

/// Serialize publication identity and delivery evidence independently of exchange mutation ownership.
pub(crate) struct SessionPublications {
    session: String,
    connection: Mutex<crate::storage::database::HistoryConnection>,
    changed: tokio::sync::Notify,
    reconciliation: Mutex<Option<PublicationBarrier>>,
    failed: AtomicBool,
    recovery: Mutex<std::collections::VecDeque<PublicationRecovery>>,
}

/// Retain the exact identities and bytes whose commit or delivery evidence was not observed.
struct PublicationRecovery {
    key: String,
    events: Vec<(PublicationOrigin, SessionEvent)>,
}

/// Hold output behind one bounded owner snapshot until its response is admitted or abandoned.
pub(super) struct PublicationLease {
    publications: Weak<SessionPublications>,
    id: uuid::Uuid,
}

impl Drop for PublicationLease {
    /// Release an abandoned response without claiming any publication reached the client.
    fn drop(&mut self) {
        if let Some(publications) = self.publications.upgrade() {
            publications.release_reconciliation(self.id);
        }
    }
}

struct PublicationBarrier {
    id: uuid::Uuid,
    deadline: tokio::time::Instant,
}

impl SessionPublications {
    /// Open session publication storage without changing a live owner's delivery evidence.
    pub(crate) fn open(root: &Path, session: &str) -> Result<Self, PublicationError> {
        if session.trim().is_empty() {
            return Err(PublicationError::SessionMismatch);
        }
        let connection = crate::storage::database::HistoryConnection::open(root)?;
        Ok(Self {
            session: session.into(),
            connection: Mutex::new(connection),
            changed: tokio::sync::Notify::new(),
            reconciliation: Mutex::new(None),
            failed: AtomicBool::new(false),
            recovery: Mutex::new(std::collections::VecDeque::new()),
        })
    }

    /// Get whether any required publication lost storage certification in this owner epoch.
    pub(super) fn storage_failed(&self) -> bool {
        self.failed.load(Ordering::Acquire)
    }

    /// Serialize a domain transaction's outbox insert against failed and concurrent publication prefixes.
    pub(super) fn ordered_mutation<T, E>(
        &self,
        unavailable: E,
        mutation: impl FnOnce() -> Result<T, E>,
    ) -> Result<T, E> {
        let pending = match self.recovery.lock() {
            Ok(pending) => pending,
            Err(_) => return Err(unavailable),
        };
        if self.storage_failed() || !pending.is_empty() {
            return Err(unavailable);
        }
        let result = mutation();
        drop(pending);
        result
    }

    /// Get whether exact failed publication payloads still require a retention retry.
    pub(super) fn has_recovery(&self) -> bool {
        self.recovery
            .lock()
            .map_or(true, |pending| !pending.is_empty())
    }

    /// Retry a bounded prefix without resending output or allocating replacement publication identities.
    pub(super) fn retry_recovery(&self) -> Result<(), PublicationError> {
        let mut pending = self
            .recovery
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        for _ in 0..32 {
            let Some(batch) = pending.front() else {
                break;
            };
            self.append_batch(&batch.events)?;
            pending.pop_front();
        }
        self.changed.notify_one();
        Ok(())
    }

    /// Queue a logical publication behind the failed prefix without changing a retained identity.
    fn retain_failure(
        &self,
        pending: &mut std::collections::VecDeque<PublicationRecovery>,
        key: String,
        events: Vec<(PublicationOrigin, SessionEvent)>,
    ) -> Result<(), PublicationError> {
        self.failed.store(true, Ordering::Release);
        if let Some(previous) = pending.iter().find(|batch| batch.key == key) {
            if serde_json::to_vec(&previous.events)? != serde_json::to_vec(&events)? {
                return Err(PublicationError::IdentityConflict);
            }
        } else {
            pending.push_back(PublicationRecovery { key, events });
        }
        self.changed.notify_one();
        Ok(())
    }

    /// Pause new output only when no earlier offer remains in flight.
    pub(super) fn begin_reconciliation(
        self: &Arc<Self>,
    ) -> Result<PublicationLease, PublicationError> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let mut barrier = self
            .reconciliation
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        if barrier.is_some() {
            return Err(PublicationError::DeliveryInProgress);
        }
        let offering: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM owner_publication WHERE session_id=?1 AND json_extract(state,'$.state')='offering')",
            [&self.session], |row| row.get(0),
        )?;
        if offering {
            return Err(PublicationError::DeliveryInProgress);
        }
        let id = uuid::Uuid::new_v4();
        *barrier = Some(PublicationBarrier {
            id,
            deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(30),
        });
        self.changed.notify_one();
        Ok(PublicationLease {
            publications: Arc::downgrade(self),
            id,
        })
    }

    /// Confirm only a still-owned response lease; expiration never silently acknowledges its page.
    pub(super) fn confirm_reconciliation(
        &self,
        lease: &PublicationLease,
        receipt: &PublicationReconciliation,
    ) -> Result<(), PublicationError> {
        let owner = lease
            .publications
            .upgrade()
            .ok_or(PublicationError::ReconciliationExpired)?;
        if !std::ptr::eq(self, owner.as_ref()) {
            return Err(PublicationError::SessionMismatch);
        }
        let live = self
            .reconciliation
            .lock()
            .map_err(|_| PublicationError::Unavailable)?
            .as_ref()
            .is_some_and(|barrier| {
                barrier.id == lease.id && tokio::time::Instant::now() < barrier.deadline
            });
        if !live {
            return Err(PublicationError::ReconciliationExpired);
        }
        self.confirm_reconciled(receipt)
    }

    /// Wake the owner at the current lease deadline, even when no client or publication remains active.
    pub(super) async fn reconciliation_expiry(&self) {
        let scheduled = self.reconciliation.lock().ok().and_then(|barrier| {
            barrier
                .as_ref()
                .map(|barrier| (barrier.id, barrier.deadline))
        });
        let Some((id, deadline)) = scheduled else {
            return std::future::pending().await;
        };
        tokio::time::sleep_until(deadline).await;
        self.release_reconciliation(id);
    }

    /// Release only the matching lease so a late drop cannot clear a newer snapshot's barrier.
    fn release_reconciliation(&self, id: uuid::Uuid) {
        if let Ok(mut barrier) = self.reconciliation.lock()
            && barrier.as_ref().is_some_and(|barrier| barrier.id == id)
        {
            *barrier = None;
            self.changed.notify_one();
        }
    }

    /// Get whether a coherent owner response currently reserves the output boundary.
    fn reconciliation_pending(&self) -> Result<bool, PublicationError> {
        self.reconciliation
            .lock()
            .map(|barrier| barrier.is_some())
            .map_err(|_| PublicationError::Unavailable)
    }

    /// Classify interrupted offers once at owner startup, before any output worker is launched.
    pub(super) fn recover_interrupted_offers(&self) -> Result<(), PublicationError> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        connection.execute(
            "UPDATE owner_publication SET state=?2 WHERE session_id=?1 AND json_extract(state,'$.state')='offering'",
            params![self.session, serde_json::to_string(&PublicationState::Uncertain)?],
        )?;
        Ok(())
    }

    /// Commit a complete event before attempting bounded, nonblocking outbound admission.
    /// Pending or uncertain earlier publications prevent later events from bypassing their order.
    pub(crate) fn publish(
        &self,
        event: SessionEvent,
        transport: Option<&MessageSender>,
    ) -> Result<RetainedPublication, PublicationError> {
        self.publish_once(
            PublicationOrigin::OwnerUpdate(uuid::Uuid::new_v4().to_string()),
            event,
            transport,
        )
    }

    /// Retain one logical notification group atomically before the owner offers any of its frames.
    pub(super) fn publish_batch(&self, events: Vec<SessionEvent>) -> Result<(), PublicationError> {
        if events.is_empty() {
            return Ok(());
        }
        let key = uuid::Uuid::new_v4().to_string();
        let events: Vec<_> = events
            .into_iter()
            .enumerate()
            .map(|(index, event)| {
                (
                    PublicationOrigin::OwnerUpdate(format!("{key}:{index}")),
                    event,
                )
            })
            .collect();
        let mut pending = self
            .recovery
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        if !pending.is_empty() {
            self.retain_failure(&mut pending, key, events)?;
            return Err(PublicationError::Unavailable);
        }
        let result = self.append_batch(&events);
        if result
            .as_ref()
            .err()
            .is_some_and(PublicationError::requires_recovery)
        {
            self.retain_failure(&mut pending, key, events)?;
        }
        result
    }

    /// Commit one recovered or newly assigned atomic event group with its original identities.
    fn append_batch(
        &self,
        events: &[(PublicationOrigin, SessionEvent)],
    ) -> Result<(), PublicationError> {
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        for (origin, event) in events {
            Self::append_transaction(
                &transaction,
                &self.session,
                event.clone(),
                Some(origin.clone()),
            )?;
        }
        transaction.commit()?;
        self.changed.notify_one();
        Ok(())
    }

    /// Retain a producer fact exactly once; reject identity reuse with different content.
    /// Recovery observes its original retention evidence and never repeats an uncertain send.
    pub(super) fn publish_once(
        &self,
        origin: PublicationOrigin,
        event: SessionEvent,
        transport: Option<&MessageSender>,
    ) -> Result<RetainedPublication, PublicationError> {
        let key = serde_json::to_string(&origin)?;
        let mut pending = self
            .recovery
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        if !pending.is_empty() {
            self.retain_failure(&mut pending, key, vec![(origin, event)])?;
            return Err(PublicationError::Unavailable);
        }
        let result = self.append(event.clone(), Some(origin.clone()), transport);
        if result
            .as_ref()
            .err()
            .is_some_and(PublicationError::requires_recovery)
        {
            self.retain_failure(&mut pending, key, vec![(origin, event)])?;
        }
        result
    }

    /// Atomically retain the event and optional producer identity before authorizing any output.
    fn append(
        &self,
        event: SessionEvent,
        origin: Option<PublicationOrigin>,
        transport: Option<&MessageSender>,
    ) -> Result<RetainedPublication, PublicationError> {
        self.validate(&event)?;
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let (mut retained, insertion) =
            Self::append_transaction(&transaction, &self.session, event, origin)?;
        transaction.commit()?;
        self.changed.notify_one();
        if matches!(insertion, PublicationInsertion::Existing) {
            return Ok(retained);
        }
        let sequence = retained.sequence;
        let Some(transport) = transport else {
            return Ok(retained);
        };
        if self.reconciliation_pending()? {
            return Ok(retained);
        }
        let blocked: bool = connection.query_row("SELECT EXISTS(SELECT 1 FROM owner_publication WHERE session_id=?1 AND sequence<?2 AND json_extract(state,'$.state')!='admitted')",
            params![self.session, sequence.sql()], |row| row.get(0))?;
        if blocked {
            return Ok(retained);
        }

        // Keep local publication-before-output lock order; try_send_event performs no callbacks or waits.
        self.write_state(&connection, sequence, PublicationState::Offering)?;
        let state = match transport.try_send_event(&retained.delivery_event()) {
            Ok(()) => PublicationState::Admitted,
            Err(_) => PublicationState::Pending,
        };
        self.write_state(&connection, sequence, state)?;
        retained.state = state;
        Ok(retained)
    }

    /// Retain a plan decision's notification in the same transaction as its answer and receipt.
    pub(crate) fn retain_plan_answer(
        transaction: &rusqlite::Transaction<'_>,
        session: &str,
        request: &crate::plan::PlanQuestionAnswerRequest,
        result: &crate::plan::PlanQuestionAnswerCommit,
    ) -> Result<(), PublicationError> {
        let event = SessionEvent {
            session_id: session.into(),
            event: "question_updated".into(),
            payload: result.notification_payload()?,
        };
        Self::append_transaction(
            transaction,
            session,
            event,
            Some(PublicationOrigin::PlanQuestionAnswer(
                request.operation_id().into(),
            )),
        )?;
        Ok(())
    }

    /// Wake output delivery after another domain transaction commits retained notifications.
    pub(super) fn notify_ready(&self) {
        self.changed.notify_one();
    }

    /// Allocate or recover one producer fact within the caller's existing SQLite transaction.
    fn append_transaction(
        transaction: &rusqlite::Transaction<'_>,
        session: &str,
        event: SessionEvent,
        origin: Option<PublicationOrigin>,
    ) -> Result<(RetainedPublication, PublicationInsertion), PublicationError> {
        if event.session_id != session {
            return Err(PublicationError::SessionMismatch);
        }
        forge_protocol::contract::validate("session", &event.event, &event.payload)
            .map_err(|_| PublicationError::InvalidEvent)?;
        let encoded = serde_json::to_string(&event)?;
        let origin = origin.as_ref().map(serde_json::to_string).transpose()?;
        if let Some(origin) = &origin {
            let previous: Option<(i64, String, String)> = transaction.query_row(
                "SELECT p.sequence,p.event,p.state FROM owner_publication_origin o JOIN owner_publication p ON p.session_id=o.session_id AND p.sequence=o.sequence WHERE o.session_id=?1 AND o.origin=?2",
                params![session, origin], |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            ).optional()?;
            if let Some((sequence, retained, state)) = previous {
                if retained != encoded {
                    return Err(PublicationError::IdentityConflict);
                }
                let sequence = PublicationSequence::try_from(
                    u64::try_from(sequence).map_err(|_| PublicationError::InvalidSequence)?,
                )?;
                return Ok((
                    RetainedPublication {
                        sequence,
                        event,
                        state: serde_json::from_str(&state)?,
                    },
                    PublicationInsertion::Existing,
                ));
            }
        }
        let previous: i64 = transaction.query_row(
            "SELECT COALESCE(MAX(sequence),0) FROM owner_publication WHERE session_id=?1",
            [session],
            |row| row.get(0),
        )?;
        let number = previous
            .checked_add(1)
            .ok_or(PublicationError::SequenceExhausted)?;
        let sequence = PublicationSequence::try_from(
            u64::try_from(number).map_err(|_| PublicationError::InvalidSequence)?,
        )?;
        transaction.execute(
            "INSERT INTO owner_publication(session_id,sequence,event,state) VALUES(?1,?2,?3,?4)",
            params![
                session,
                sequence.sql(),
                encoded,
                serde_json::to_string(&PublicationState::Pending)?
            ],
        )?;
        if let Some(origin) = origin {
            transaction.execute(
                "INSERT INTO owner_publication_origin(session_id,origin,sequence) VALUES(?1,?2,?3)",
                params![session, origin, sequence.sql()],
            )?;
        }
        Ok((
            RetainedPublication {
                sequence,
                event,
                state: PublicationState::Pending,
            },
            PublicationInsertion::New,
        ))
    }

    /// Wait for retained content without making a producer wait for outbound transport capacity.
    pub(super) async fn changed(&self) {
        self.changed.notified().await;
    }

    /// Authorize only the oldest undelivered event, retaining an offer boundary before external work.
    fn begin_offer(&self) -> Result<Option<RetainedPublication>, PublicationError> {
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        if self.reconciliation_pending()? {
            return Ok(None);
        }
        let first: Option<(i64, String, String)> = transaction.query_row(
            "SELECT sequence,event,state FROM owner_publication WHERE session_id=?1 AND json_extract(state,'$.state')!='admitted' ORDER BY sequence LIMIT 1",
            [&self.session], |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        ).optional()?;
        let Some((sequence, encoded, state)) = first else {
            return Ok(None);
        };
        match serde_json::from_str::<PublicationState>(&state)? {
            PublicationState::Pending => {}
            PublicationState::Offering | PublicationState::Uncertain => return Ok(None),
            PublicationState::Admitted => return Err(PublicationError::Unavailable),
        }
        let sequence = PublicationSequence::try_from(
            u64::try_from(sequence).map_err(|_| PublicationError::InvalidSequence)?,
        )?;
        let event = serde_json::from_str(&encoded)?;
        self.validate(&event)?;
        self.write_state(&transaction, sequence, PublicationState::Offering)?;
        transaction.commit()?;
        Ok(Some(RetainedPublication {
            sequence,
            event,
            state: PublicationState::Offering,
        }))
    }

    /// Commit the retained worker's observation; retrying this write never repeats its external send.
    fn complete_offer(
        &self,
        sequence: PublicationSequence,
        outcome: DeliveryOutcome,
    ) -> Result<(), PublicationError> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        self.write_state(
            &connection,
            sequence,
            match outcome {
                DeliveryOutcome::Admitted => PublicationState::Admitted,
                DeliveryOutcome::Uncertain => PublicationState::Uncertain,
            },
        )
    }

    /// Read a bounded prefix without treating retrieval as admission or advancing delivery evidence.
    #[cfg(test)]
    pub(crate) fn after(
        &self,
        cursor: Option<PublicationSequence>,
        limit: NonZeroU16,
    ) -> Result<Vec<RetainedPublication>, PublicationError> {
        let connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let mut query = connection.prepare("SELECT sequence,event,state FROM owner_publication WHERE session_id=?1 AND sequence>?2 ORDER BY sequence LIMIT ?3")?;
        let rows = query
            .query_map(
                params![
                    self.session,
                    cursor.map_or(0, PublicationSequence::sql),
                    i64::from(limit.get())
                ],
                |row| {
                    Ok((
                        row.get::<_, i64>(0)?,
                        row.get::<_, String>(1)?,
                        row.get::<_, String>(2)?,
                    ))
                },
            )?
            .collect::<Result<Vec<_>, _>>()?;
        rows.into_iter()
            .map(|(sequence, encoded, state)| {
                let sequence = PublicationSequence::try_from(
                    u64::try_from(sequence).map_err(|_| PublicationError::InvalidSequence)?,
                )?;
                let event = serde_json::from_str(&encoded)?;
                self.validate(&event)?;
                Ok(RetainedPublication {
                    sequence,
                    event,
                    state: serde_json::from_str(&state)?,
                })
            })
            .collect()
    }

    /// Read a stable bounded reconnect page without acknowledging retrieval as outbound admission.
    pub(crate) fn snapshot(
        &self,
        cursor: Option<PublicationSequence>,
        limit: NonZeroU16,
    ) -> Result<PublicationSnapshot, PublicationError> {
        // Capture the high-water mark and page under one read transaction.
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let transaction = connection.transaction()?;
        let maximum: Option<i64> = transaction.query_row(
            "SELECT MAX(sequence) FROM owner_publication WHERE session_id=?1",
            [&self.session],
            |row| row.get(0),
        )?;
        let high_water = maximum
            .map(|value| {
                PublicationSequence::try_from(
                    u64::try_from(value).map_err(|_| PublicationError::InvalidSequence)?,
                )
            })
            .transpose()?;
        if cursor
            .is_some_and(|cursor| high_water.is_none_or(|maximum| cursor.get() > maximum.get()))
        {
            return Err(PublicationError::InvalidCursor);
        }
        if let Some(cursor) = cursor {
            let skips_unconfirmed: bool = transaction.query_row(
                "SELECT EXISTS(SELECT 1 FROM owner_publication WHERE session_id=?1 AND sequence<=?2 AND json_extract(state,'$.state')!='admitted')",
                params![self.session, cursor.sql()], |row| row.get(0),
            )?;
            if skips_unconfirmed {
                return Err(PublicationError::InvalidCursor);
            }
        }
        // Validate the complete retained page before exposing its reconciliation receipt.
        let records = {
            let mut query = transaction.prepare(
                "SELECT sequence,event,state FROM owner_publication WHERE session_id=?1 AND sequence>?2 ORDER BY sequence LIMIT ?3",
            )?;
            let rows = query
                .query_map(
                    params![
                        self.session,
                        cursor.map_or(0, PublicationSequence::sql),
                        i64::from(limit.get())
                    ],
                    |row| {
                        Ok((
                            row.get::<_, i64>(0)?,
                            row.get::<_, String>(1)?,
                            row.get::<_, String>(2)?,
                        ))
                    },
                )?
                .collect::<Result<Vec<_>, _>>()?;
            rows.into_iter()
                .map(|(sequence, event, state)| {
                    let sequence = PublicationSequence::try_from(
                        u64::try_from(sequence).map_err(|_| PublicationError::InvalidSequence)?,
                    )?;
                    let event = serde_json::from_str(&event)?;
                    self.validate(&event)?;
                    Ok(RetainedPublication {
                        sequence,
                        event,
                        state: serde_json::from_str(&state)?,
                    })
                })
                .collect::<Result<Vec<_>, PublicationError>>()?
        };
        transaction.commit()?;
        let through = records.last().map(RetainedPublication::sequence);
        Ok(PublicationSnapshot {
            records,
            high_water,
            receipt: PublicationReconciliation {
                session: self.session.clone(),
                after: cursor,
                through,
            },
        })
    }

    /// Record complete reconnect-page admission without resending uncertain events or skipping later content.
    /// The caller must retain the receipt until the entire page enters ordered outbound delivery.
    pub(crate) fn confirm_reconciled(
        &self,
        receipt: &PublicationReconciliation,
    ) -> Result<(), PublicationError> {
        if receipt.session != self.session {
            return Err(PublicationError::SessionMismatch);
        }
        let Some(through) = receipt.through else {
            return Ok(());
        };
        let mut connection = self
            .connection
            .lock()
            .map_err(|_| PublicationError::Unavailable)?;
        let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let offering: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM owner_publication WHERE session_id=?1 AND sequence>?2 AND sequence<=?3 AND json_extract(state,'$.state')='offering')",
            params![self.session, receipt.after.map_or(0, PublicationSequence::sql), through.sql()], |row| row.get(0),
        )?;
        if offering {
            return Err(PublicationError::DeliveryInProgress);
        }
        transaction.execute(
            "UPDATE owner_publication SET state=?4 WHERE session_id=?1 AND sequence>?2 AND sequence<=?3",
            params![self.session, receipt.after.map_or(0, PublicationSequence::sql), through.sql(), serde_json::to_string(&PublicationState::Admitted)?],
        )?;
        transaction.commit()?;
        self.changed.notify_one();
        Ok(())
    }

    /// Reject cross-session or unrecognized notifications before they enter retained publication storage.
    fn validate(&self, event: &SessionEvent) -> Result<(), PublicationError> {
        if event.session_id != self.session {
            return Err(PublicationError::SessionMismatch);
        }
        forge_protocol::contract::validate("session", &event.event, &event.payload)
            .map_err(|_| PublicationError::InvalidEvent)
    }

    /// Record an offer boundary or its evidence without conflating a failed write with nondelivery.
    fn write_state(
        &self,
        connection: &Connection,
        sequence: PublicationSequence,
        state: PublicationState,
    ) -> Result<(), PublicationError> {
        let encoded = serde_json::to_string(&state)?;
        let written = connection
            .execute(
                "UPDATE owner_publication SET state=?3 WHERE session_id=?1 AND sequence=?2",
                params![self.session, sequence.sql(), encoded],
            )
            .map_err(|source| PublicationError::DeliveryState {
                publication: sequence,
                source,
            })?;
        if written != 1 {
            return Err(PublicationError::Unavailable);
        }
        Ok(())
    }
}

/// Distinguish a new fact from an immutable replay without repeating its output offer.
enum PublicationInsertion {
    New,
    Existing,
}

/// Distinguish completed frame admission from any interrupted or failed multipart send.
#[derive(Clone, Copy)]
enum DeliveryOutcome {
    Admitted,
    Uncertain,
}

/// Retain the exact output job or its observation until storage records its delivery evidence.
enum ActiveDelivery {
    Sending {
        sequence: PublicationSequence,
        worker: tokio::task::JoinHandle<DeliveryOutcome>,
    },
    Observed {
        sequence: PublicationSequence,
        outcome: DeliveryOutcome,
    },
}

/// Describe one delivery boundary for correlated owner logs without claiming receipt by the UI.
pub(super) enum PublicationProgress {
    Offering(PublicationSequence),
    Admitted(PublicationSequence),
    Uncertain(PublicationSequence),
}

/// Own one ordered publication worker independently of native exchange completion and collection.
#[derive(Default)]
pub(super) struct PublicationDelivery {
    active: Option<ActiveDelivery>,
    retry_at: Option<tokio::time::Instant>,
}

impl PublicationDelivery {
    /// Advance one delivery boundary; preserve completed observations across failed evidence writes.
    pub(super) fn update(
        &mut self,
        publications: &SessionPublications,
        transport: Option<&MessageSender>,
    ) -> Result<Option<PublicationProgress>, PublicationError> {
        if self
            .retry_at
            .is_some_and(|deadline| tokio::time::Instant::now() < deadline)
        {
            return Ok(None);
        }
        self.retry_at = None;
        let progress = self.advance(publications, transport);
        if progress.is_err() {
            self.retry_at =
                Some(tokio::time::Instant::now() + std::time::Duration::from_millis(250));
        }
        progress
    }

    /// Observe a retained job without exposing its handle to caller cancellation or re-awaiting it.
    pub(super) async fn next(&mut self) {
        if let Some(deadline) = self.retry_at {
            tokio::time::sleep_until(deadline).await;
            return;
        }
        match self.active.as_mut() {
            Some(ActiveDelivery::Sending { sequence, worker }) => {
                let sequence = *sequence;
                let outcome = worker.await.unwrap_or(DeliveryOutcome::Uncertain);
                self.active = Some(ActiveDelivery::Observed { sequence, outcome });
            }
            Some(ActiveDelivery::Observed { .. }) => {}
            None => std::future::pending().await,
        }
    }

    /// Commit returned evidence before authorizing any later event in the retained order.
    fn advance(
        &mut self,
        publications: &SessionPublications,
        transport: Option<&MessageSender>,
    ) -> Result<Option<PublicationProgress>, PublicationError> {
        match &self.active {
            Some(ActiveDelivery::Sending { .. }) => return Ok(None),
            Some(ActiveDelivery::Observed { sequence, outcome }) => {
                let sequence = *sequence;
                let outcome = *outcome;
                publications.complete_offer(sequence, outcome)?;
                self.active = None;
                publications.changed.notify_one();
                return Ok(Some(match outcome {
                    DeliveryOutcome::Admitted => PublicationProgress::Admitted(sequence),
                    DeliveryOutcome::Uncertain => PublicationProgress::Uncertain(sequence),
                }));
            }
            None => {}
        }
        let Some(transport) = transport else {
            return Ok(None);
        };
        if transport.is_closed() {
            return Ok(None);
        }
        let Some(publication) = publications.begin_offer()? else {
            return Ok(None);
        };
        let transport = transport.clone();
        let sequence = publication.sequence;
        self.active = Some(ActiveDelivery::Sending {
            sequence,
            worker: tokio::spawn(async move {
                match transport
                    .send_event(stamp_event(publication.event, sequence))
                    .await
                {
                    Ok(()) => DeliveryOutcome::Admitted,
                    Err(_) => DeliveryOutcome::Uncertain,
                }
            }),
        });
        Ok(Some(PublicationProgress::Offering(sequence)))
    }
}

impl Drop for PublicationDelivery {
    /// Stop only output forwarding at owner shutdown; its durable offer remains uncertain on recovery.
    fn drop(&mut self) {
        if let Some(ActiveDelivery::Sending { worker, .. }) = &self.active {
            worker.abort();
        }
    }
}

/// Preserve publication-storage failures separately from transient UI-channel availability.
#[derive(Debug, thiserror::Error)]
pub(crate) enum PublicationError {
    #[error("publication database could not be opened")]
    Database(#[from] crate::storage::database::DatabaseError),
    #[error("reconnect response lease expired before confirmation")]
    ReconciliationExpired,
    #[error("publication cursor is beyond the committed session prefix")]
    InvalidCursor,
    #[error("publication reconciliation overlaps an active output offer")]
    DeliveryInProgress,
    #[error("publication producer identity refers to different content")]
    IdentityConflict,
    #[error("publication session does not match its owner")]
    SessionMismatch,
    #[error("publication event violates the session event contract")]
    InvalidEvent,
    #[error("publication sequence must be a positive SQLite integer")]
    InvalidSequence,
    #[error("publication sequence is exhausted")]
    SequenceExhausted,
    #[error("publication storage is unavailable")]
    Unavailable,
    #[error("publication storage failed")]
    Storage(#[from] rusqlite::Error),
    #[error("publication record could not be encoded or decoded")]
    Codec(#[from] serde_json::Error),
    #[error("publication delivery evidence could not be committed")]
    DeliveryState {
        publication: PublicationSequence,
        #[source]
        source: rusqlite::Error,
    },
}

impl PublicationError {
    /// Retain valid producer work after storage uncertainty without queuing rejected identities or content.
    fn requires_recovery(&self) -> bool {
        match self {
            Self::Database(_)
            | Self::Storage(_)
            | Self::Codec(_)
            | Self::DeliveryState { .. }
            | Self::Unavailable
            | Self::SequenceExhausted => true,
            Self::ReconciliationExpired
            | Self::InvalidCursor
            | Self::DeliveryInProgress
            | Self::IdentityConflict
            | Self::SessionMismatch
            | Self::InvalidEvent
            | Self::InvalidSequence => false,
        }
    }
}

/// Stamp only outbound copies so producer identity comparisons retain their original bytes.
fn stamp_event(mut event: SessionEvent, sequence: PublicationSequence) -> SessionEvent {
    event.payload["_publication"] = serde_json::json!(PublicationStamp { sequence });
    event
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::FutureExt;

    /// A failed input batch retains its suffix and remains ahead of later producer events.
    #[test]
    fn failed_batch_retains_suffix_and_orders_new_publishers() -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        publications.connection.lock().unwrap().execute_batch(
            "CREATE TRIGGER reject_input_prefix BEFORE INSERT ON owner_publication BEGIN SELECT RAISE(ABORT,'injected publication failure'); END;",
        )?;
        assert!(
            publications
                .publish_batch(vec![event("first"), event("second")])
                .is_err()
        );
        publications
            .connection
            .lock()
            .unwrap()
            .execute_batch("DROP TRIGGER reject_input_prefix;")?;
        assert!(publications.publish(event("third"), None).is_err());
        assert!(records(&publications).is_empty());
        publications.retry_recovery()?;
        let retained = records(&publications);
        assert_eq!(retained.len(), 3);
        for (index, expected) in ["first", "second", "third"].into_iter().enumerate() {
            assert_eq!(retained[index].event().payload["text"], expected);
        }
        assert!(!publications.has_recovery());
        Ok(())
    }

    /// A snapshot lease pauses both direct offers and the ordered delivery worker until abandoned.
    #[test]
    fn reconnect_lease_blocks_output_and_drop_releases_without_acknowledgment()
    -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = Arc::new(SessionPublications::open(root.path(), "session")?);
        let lease = publications.begin_reconciliation()?;
        let (sender, mut receiver) = forge_protocol::outbound::channel();
        publications.publish(event("during snapshot"), Some(&sender))?;
        assert!(publications.begin_offer()?.is_none());
        assert!(receiver.try_recv().is_err());
        assert_eq!(records(&publications)[0].state(), PublicationState::Pending);
        drop(lease);
        assert_eq!(records(&publications)[0].state(), PublicationState::Pending);
        assert!(publications.begin_offer()?.is_some());
        Ok(())
    }

    /// A lost caller cannot pin output forever, and its late token cannot release a newer lease.
    #[tokio::test(start_paused = true)]
    async fn reconnect_lease_expiration_rejects_late_confirmation() -> Result<(), PublicationError>
    {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = Arc::new(SessionPublications::open(root.path(), "session")?);
        publications.publish(event("retained"), None)?;
        let lease = publications.begin_reconciliation()?;
        let (_, receipt) = publications
            .snapshot(None, NonZeroU16::new(1).unwrap())?
            .into_parts();
        let expiry = publications.reconciliation_expiry();
        tokio::pin!(expiry);
        assert!(futures_util::poll!(expiry.as_mut()).is_pending());
        tokio::time::advance(std::time::Duration::from_secs(31)).await;
        expiry.await;
        assert!(matches!(
            publications.confirm_reconciliation(&lease, &receipt),
            Err(PublicationError::ReconciliationExpired)
        ));
        let current = publications.begin_reconciliation()?;
        drop(lease);
        assert!(matches!(
            publications.begin_reconciliation(),
            Err(PublicationError::DeliveryInProgress)
        ));
        drop(current);
        assert!(publications.begin_offer()?.is_some());
        Ok(())
    }

    /// A failed event in a logical group cannot leave a publishable prefix behind.
    #[test]
    fn publication_batch_rolls_back_every_event_on_failure() -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        let mut invalid = event("wrong session");
        invalid.session_id = "another session".into();
        assert!(matches!(
            publications.publish_batch(vec![event("first"), invalid]),
            Err(PublicationError::SessionMismatch)
        ));
        assert!(records(&publications).is_empty());
        publications.publish_batch(vec![event("one"), event("two")])?;
        let retained = records(&publications);
        assert_eq!(retained.len(), 2);
        assert_eq!(retained[0].sequence().get(), 1);
        assert_eq!(retained[1].sequence().get(), 2);
        assert!(
            retained
                .iter()
                .all(|publication| publication.state() == PublicationState::Pending)
        );
        Ok(())
    }

    /// Reconnect admission releases only the captured uncertain prefix, preserving later output order.
    #[tokio::test]
    async fn reconnect_page_reconciles_uncertainty_without_skipping_later_appends()
    -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        let first = publications.publish(event("uncertain"), None)?;
        publications.begin_offer()?.unwrap();
        drop(publications);
        let publications = SessionPublications::open(root.path(), "session")?;
        publications.recover_interrupted_offers()?;
        let page = publications.snapshot(None, NonZeroU16::new(1).unwrap())?;
        assert_eq!(page.high_water(), Some(first.sequence()));
        let (retained, receipt) = page.into_parts();
        assert_eq!(retained[0].state(), PublicationState::Uncertain);
        assert!(
            matches!(
                publications.snapshot(Some(first.sequence()), NonZeroU16::new(1).unwrap()),
                Err(PublicationError::InvalidCursor)
            ),
            "an unconfirmed client cursor skipped the uncertain head"
        );
        let second = publications.publish(event("after snapshot"), None)?;
        assert_eq!(
            records(&publications)[0].state(),
            PublicationState::Uncertain
        );
        publications.confirm_reconciled(&receipt)?;
        publications.confirm_reconciled(&receipt)?;
        assert_eq!(
            records(&publications)[0].state(),
            PublicationState::Admitted
        );
        assert_eq!(records(&publications)[1].state(), PublicationState::Pending);
        let (sender, mut receiver) = forge_protocol::outbound::channel();
        let mut delivery = PublicationDelivery::default();
        assert!(matches!(delivery.update(&publications, Some(&sender))?,
            Some(PublicationProgress::Offering(found)) if found == second.sequence()));
        delivery.next().await;
        delivery.update(&publications, Some(&sender))?;
        let frame = receiver.recv().await.unwrap().unwrap();
        let sent: SessionEvent = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(sent.payload["text"], "after snapshot");
        assert!(receiver.try_recv().is_err());
        Ok(())
    }

    /// Create valid transport content whose exact order and body survive retention.
    fn event(text: &str) -> SessionEvent {
        SessionEvent {
            session_id: "session".into(),
            event: "exchange_updated".into(),
            payload: serde_json::json!({"text":text}),
        }
    }

    /// Read the retained prefix without changing its output evidence.
    fn records(publications: &SessionPublications) -> Vec<RetainedPublication> {
        publications
            .after(None, NonZeroU16::new(20).unwrap())
            .unwrap()
    }

    /// A failed delivery-evidence commit must retry storage while retaining the successful send fact.
    #[tokio::test]
    async fn evidence_write_retry_never_repeats_an_admitted_frame() -> Result<(), PublicationError>
    {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        let publication = publications.publish(event("once"), None)?;
        let (sender, mut receiver) = forge_protocol::outbound::channel();
        let mut delivery = PublicationDelivery::default();
        delivery.update(&publications, Some(&sender))?;
        delivery.next().await;
        drop(receiver.recv().await.unwrap().unwrap());
        {
            let connection = publications.connection.lock().unwrap();
            connection.execute_batch("CREATE TRIGGER reject_publication_evidence BEFORE UPDATE ON owner_publication WHEN json_extract(NEW.state,'$.state')='admitted' BEGIN SELECT RAISE(ABORT,'injected evidence failure'); END;")?;
        }
        assert!(matches!(
            delivery.update(&publications, Some(&sender)),
            Err(PublicationError::DeliveryState { .. })
        ));
        assert!(
            matches!(delivery.active, Some(ActiveDelivery::Observed { sequence, outcome: DeliveryOutcome::Admitted }) if sequence == publication.sequence())
        );
        {
            let connection = publications.connection.lock().unwrap();
            connection.execute_batch("DROP TRIGGER reject_publication_evidence;")?;
        }
        assert!(
            matches!(delivery.advance(&publications, Some(&sender))?, Some(PublicationProgress::Admitted(found)) if found == publication.sequence())
        );
        assert_eq!(
            records(&publications)[0].state(),
            PublicationState::Admitted
        );
        assert!(receiver.try_recv().is_err());
        Ok(())
    }

    /// Interrupted offers must remain an uncertainty barrier rather than silently replaying or skipping.
    #[tokio::test]
    async fn recovered_offer_preserves_uncertainty_and_blocks_later_events()
    -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        publications.publish(event("possibly delivered"), None)?;
        publications.begin_offer()?.unwrap();
        drop(publications);
        let publications = SessionPublications::open(root.path(), "session")?;
        publications.recover_interrupted_offers()?;
        let (sender, mut receiver) = forge_protocol::outbound::channel();
        publications.publish(event("later"), Some(&sender))?;
        let mut delivery = PublicationDelivery::default();
        assert!(delivery.update(&publications, Some(&sender))?.is_none());
        assert!(delivery.next().now_or_never().is_none());
        let retained = records(&publications);
        assert_eq!(retained[0].state(), PublicationState::Uncertain);
        assert_eq!(retained[1].state(), PublicationState::Pending);
        assert!(receiver.try_recv().is_err());
        Ok(())
    }

    /// Invalid and cross-session notifications cannot consume identities or enter the delivery prefix.
    #[test]
    fn invalid_publications_leave_the_retained_prefix_unchanged() -> Result<(), PublicationError> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let publications = SessionPublications::open(root.path(), "session")?;
        let mut foreign = event("foreign");
        foreign.session_id = "other".into();
        assert!(matches!(
            publications.publish(foreign, None),
            Err(PublicationError::SessionMismatch)
        ));
        let mut invalid = event("invalid");
        invalid.event = "unrecognized_notification".into();
        assert!(matches!(
            publications.publish(invalid, None),
            Err(PublicationError::InvalidEvent)
        ));
        for payload in [
            serde_json::Value::Null,
            serde_json::json!([]),
            serde_json::json!(7),
        ] {
            let mut nonobject = event("invalid payload");
            nonobject.payload = payload;
            assert!(matches!(
                publications.publish(nonobject, None),
                Err(PublicationError::InvalidEvent)
            ));
        }
        assert!(records(&publications).is_empty());
        assert_eq!(
            publications.publish(event("valid"), None)?.sequence().get(),
            1
        );
        Ok(())
    }
}
