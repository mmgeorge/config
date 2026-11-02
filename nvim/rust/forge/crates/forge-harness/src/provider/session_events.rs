//! Retain one ordered observation channel and collect execution tasks per session owner.

use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::sync::{Arc, Mutex};

use forge_provider_api::events::{EventDeliveryError, EventObserver, ProviderEventStream};
use forge_provider_api::lifecycle::{
    ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate, ObservationFailure,
    SubmissionId,
};

use super::{
    ExecutionOwner, InvocationId, InvocationScope, ProviderCompletion, ProviderError,
    ProviderEvent, ProviderEventSink, TurnAddress,
};

#[derive(Default)]
pub(crate) struct SessionEvents {
    owners: Mutex<HashMap<ExecutionOwner, OwnerEvents>>,
    requests: Mutex<HashMap<InvocationId, super::ProviderRequest>>,
    tasks: tokio::sync::Mutex<HashMap<InvocationId, InvocationTask>>,
}

/// Retain host workflow results until the engine settles their native lifecycle.
#[derive(Default, Debug)]
pub(crate) struct CompletionSlot {
    result: Mutex<CompletionState>,
    certificate: Mutex<Option<ExecutionCertificate>>,
}

#[derive(Default, Debug)]
struct CompletionState {
    completion: Option<ProviderCompletion>,
    host_outcome: Option<ExecutionResult>,
    batches: Vec<HostEffectBatch>,
    revision: u64,
}

/// Preserve one provider-published host-effect batch and its stable invocation revision.
#[derive(Clone, Debug)]
pub(crate) struct HostEffectBatch {
    pub(crate) revision: u64,
    pub(crate) completion: ProviderCompletion,
}

/// Native evidence for one separately completed foreground execution.
#[derive(Debug)]
pub(crate) struct NativeExecutionCertificate {
    pub(crate) anchor: TurnAddress,
    pub(crate) covered: Vec<SubmissionId>,
    pub(crate) result: ExecutionResult,
    pub(crate) completed_at_ms: i64,
}

/// Native evidence for the initial foreground execution and later continuations.
#[derive(Debug)]
pub(crate) enum ExecutionCertificate {
    Foreground {
        anchor: TurnAddress,
        covered: Vec<SubmissionId>,
        initial_result: Option<Box<ExecutionResult>>,
        continuations: Vec<NativeExecutionCertificate>,
    },
    WithoutExecution {
        covered: Vec<SubmissionId>,
    },
}

pub(crate) struct CompletionPreview {
    pub revision: u64,
    pub completion: Option<ProviderCompletion>,
    pub host_outcome: Option<ExecutionResult>,
    pub batches: Vec<HostEffectBatch>,
}

impl CompletionSlot {
    pub(crate) fn publish(&self, completion: ProviderCompletion) -> Result<(), ProviderError> {
        let mut result = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        result.revision = result
            .revision
            .checked_add(1)
            .ok_or_else(|| protocol("completion revision exhausted"))?;
        let revision = result.revision;
        result.batches.push(HostEffectBatch {
            revision,
            completion: completion.clone(),
        });
        match result.completion.as_mut() {
            Some(previous) => previous.merge_execution(completion),
            None => result.completion = Some(completion),
        }
        Ok(())
    }
    /// Retain a host outcome independently of immutable native terminal evidence.
    pub(crate) fn publish_host_outcome(
        &self,
        outcome: ExecutionResult,
    ) -> Result<(), ProviderError> {
        let mut state = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(|| protocol("completion revision exhausted"))?;
        state.host_outcome = Some(outcome);
        state.revision = revision;
        Ok(())
    }
    /// Start another native attempt without discarding accumulated host effects.
    pub(crate) fn begin_attempt(&self) -> Result<(), ProviderError> {
        let mut state = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(|| protocol("completion revision exhausted"))?;
        state.host_outcome = None;
        state.revision = revision;
        Ok(())
    }
    /// Retain adapter-certified terminal evidence independently from host effects.
    pub(crate) fn certify(&self, certificate: ExecutionCertificate) -> Result<(), ProviderError> {
        let mut slot = self
            .certificate
            .lock()
            .map_err(|_| protocol("execution certificate poisoned"))?;
        *slot = Some(certificate);
        Ok(())
    }

    /// Consume the certificate established by the adapter for this execution.
    pub(crate) fn take_certificate(&self) -> Result<Option<ExecutionCertificate>, ProviderError> {
        Ok(self
            .certificate
            .lock()
            .map_err(|_| protocol("execution certificate poisoned"))?
            .take())
    }

    #[cfg(test)]
    pub(crate) fn take(&self) -> Result<Option<ProviderCompletion>, ProviderError> {
        Ok(self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?
            .completion
            .take())
    }

    /// Capture host effects with their revision without consuming them.
    pub(crate) fn preview(&self) -> Result<CompletionPreview, ProviderError> {
        let state = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        Ok(CompletionPreview {
            revision: state.revision,
            completion: state.completion.clone(),
            host_outcome: state.host_outcome.clone(),
            batches: state.batches.clone(),
        })
    }

    /// Consume host effects and any separate host outcome together.
    pub(crate) fn take_with_outcome(
        &self,
    ) -> Result<(Option<ProviderCompletion>, Option<ExecutionResult>), ProviderError> {
        let mut state = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        state.batches.clear();
        Ok((state.completion.take(), state.host_outcome.take()))
    }

    /// Consume the exact effect revision reviewed by the host.
    pub(crate) fn take_revision(&self, revision: u64) -> Result<bool, ProviderError> {
        let mut state = self
            .result
            .lock()
            .map_err(|_| protocol("completion slot poisoned"))?;
        if state.revision != revision {
            return Ok(false);
        }
        state.completion.take();
        state.host_outcome.take();
        state.batches.clear();
        Ok(true)
    }
}

/// Define which adapter proves that its returned terminal boundary covers admitted input.
pub(crate) enum TerminalBoundary {
    AdapterCertificate,
    AcpPrompt,
    LocalFixture,
}

pub(crate) enum Acknowledgement {
    Local(Option<Arc<super::submission::Submission>>),
    Native(Option<tokio::sync::watch::Receiver<bool>>),
}

struct OwnerEvents {
    sink: ProviderEventSink,
    stream: Option<ProviderEventStream>,
}

struct InvocationTask {
    delivery: Arc<DeliverySet>,
    owner: ExecutionOwner,
    stop: tokio::sync::watch::Sender<bool>,
    task: tokio::task::JoinHandle<()>,
}

/// Fence the terminal boundary against controls admitted before it closed.
struct DeliverySet {
    state: Mutex<DeliverySetState>,
    changed: tokio::sync::Notify,
}

struct DeliverySetState {
    accepting: bool,
    pending: usize,
    accepted: HashSet<SubmissionId>,
    uncertain: bool,
}

struct SteeringDelivery {
    delivery: Arc<DeliverySet>,
    submission: Option<SubmissionId>,
    settled: bool,
}

impl DeliverySet {
    fn new(accepted: Vec<SubmissionId>) -> Arc<Self> {
        Arc::new(Self {
            state: Mutex::new(DeliverySetState {
                accepting: true,
                pending: 0,
                accepted: accepted.into_iter().collect(),
                uncertain: false,
            }),
            changed: tokio::sync::Notify::new(),
        })
    }

    fn accepting(&self) -> Result<bool, ProviderError> {
        Ok(self
            .state
            .lock()
            .map_err(|_| protocol("submission coverage lock poisoned"))?
            .accepting)
    }

    fn register(
        self: &Arc<Self>,
        submission: Option<SubmissionId>,
    ) -> Result<SteeringDelivery, ProviderError> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| protocol("submission coverage lock poisoned"))?;
        if !state.accepting {
            return Err(ProviderError::NotConsumed {
                message: "native execution closed before steering dispatch".into(),
            });
        }
        state.pending += 1;
        Ok(SteeringDelivery {
            delivery: self.clone(),
            submission,
            settled: false,
        })
    }

    /// Wait until the native result has closed control admission before a safe continuation.
    async fn closed(&self) -> Result<(), ProviderError> {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if !self.accepting()? {
                return Ok(());
            }
            changed.await;
        }
    }

    async fn finish(&self) -> Result<Vec<SubmissionId>, ProviderError> {
        loop {
            let changed = self.changed.notified();
            {
                let mut state = self
                    .state
                    .lock()
                    .map_err(|_| protocol("submission coverage lock poisoned"))?;
                let was_accepting = state.accepting;
                state.accepting = false;
                if was_accepting {
                    self.changed.notify_waiters();
                }
                if state.pending == 0 {
                    if state.uncertain {
                        return Err(ProviderError::Observation(ObservationFailure::Disconnected));
                    }
                    let mut accepted = state.accepted.iter().cloned().collect::<Vec<_>>();
                    accepted.sort_by(|left, right| left.as_str().cmp(right.as_str()));
                    return Ok(accepted);
                }
            }
            changed.await;
        }
    }
}

impl SteeringDelivery {
    fn complete(mut self, result: &Result<(), ProviderError>) -> Result<(), ProviderError> {
        let mut state = self
            .delivery
            .state
            .lock()
            .map_err(|_| protocol("submission coverage lock poisoned"))?;
        state.pending -= 1;
        match result {
            Ok(()) => {
                if let Some(id) = self.submission.take() {
                    state.accepted.insert(id);
                }
            }
            Err(ProviderError::NotConsumed { .. }) => {}
            Err(_) => state.uncertain = true,
        }
        self.settled = true;
        drop(state);
        self.delivery.changed.notify_one();
        Ok(())
    }
}

impl Drop for SteeringDelivery {
    fn drop(&mut self) {
        if !self.settled {
            let mut state = self
                .delivery
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.pending -= 1;
            state.uncertain = true;
            drop(state);
            self.delivery.changed.notify_one();
        }
    }
}

struct ExecutionPublication {
    scope: InvocationScope,
    anchor: Mutex<Option<TurnAddress>>,
}

impl SessionEvents {
    /// Retain the original owner and host context for same-invocation continuation.
    pub(crate) fn retain_request(
        &self,
        scope: &InvocationScope,
        request: &super::ProviderRequest,
    ) -> Result<(), ProviderError> {
        if request.owner()? != scope.owner {
            return Err(protocol(
                "submission scope belongs to a different execution owner",
            ));
        }
        self.requests
            .lock()
            .map_err(|_| protocol("retained invocation registry poisoned"))?
            .insert(scope.invocation_id.clone(), request.clone());
        Ok(())
    }

    /// Dispatch a native provider while the retained session publishes its execution result.
    pub(crate) async fn submit<F, Fut>(
        self: &Arc<Self>,
        request: super::ProviderRequest,
        scope: InvocationScope,
        terminal_boundary: TerminalBoundary,
        run: F,
    ) -> Result<(), ProviderError>
    where
        F: FnOnce(super::ProviderRequest, ProviderEventSink) -> Fut + Send + 'static,
        Fut: Future<Output = Result<ProviderCompletion, ProviderError>> + Send + 'static,
    {
        self.retain_request(&scope, &request)?;
        let covered = request
            .submission
            .as_ref()
            .map(|receipt| {
                SubmissionId::try_from(receipt.id().to_owned())
                    .map_err(|error| protocol(&error.to_string()))
            })
            .transpose()?
            .into_iter()
            .collect();
        let acceptance = Acknowledgement::Native(
            request
                .submission
                .as_ref()
                .map(|receipt| receipt.acceptance()),
        );
        let completion = request
            .control_context
            .as_ref()
            .map(|context| context.provider_completion.clone());
        self.start(
            scope,
            covered,
            acceptance,
            completion,
            terminal_boundary,
            move |sink| run(request, sink),
        )
        .await
    }

    /// Determine whether a native invocation still accepts controls within its current execution.
    pub(crate) async fn running(&self, scope: &InvocationScope) -> Result<bool, ProviderError> {
        match self.tasks.lock().await.get(&scope.invocation_id) {
            Some(task) if task.owner == scope.owner => task.delivery.accepting(),
            Some(_) => Err(protocol("steering crosses execution ownership")),
            None => Ok(false),
        }
    }

    /// Collect the previous native boundary before replaying explicitly unconsumed input.
    pub(crate) async fn wait_closed(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
        let delivery = self
            .tasks
            .lock()
            .await
            .get(&scope.invocation_id)
            .filter(|task| task.owner == scope.owner)
            .map(|task| task.delivery.clone());
        if let Some(delivery) = delivery {
            tokio::time::timeout(std::time::Duration::from_secs(30), delivery.closed())
                .await
                .map_err(|_| {
                    ProviderError::Observation(ObservationFailure::LifecycleUnavailable)
                })??;
        }
        Ok(())
    }

    /// Reuse the original invocation context for accepted input after its previous native execution.
    pub(crate) fn continuation(
        &self,
        scope: &InvocationScope,
        input: super::SteeringInput,
    ) -> Result<super::ProviderRequest, ProviderError> {
        let mut request = self
            .requests
            .lock()
            .map_err(|_| protocol("retained invocation registry poisoned"))?
            .get(&scope.invocation_id)
            .cloned()
            .ok_or_else(|| protocol("steering has no retained invocation"))?;
        if request.owner()? != scope.owner {
            return Err(protocol("retained invocation belongs to another owner"));
        }
        if request
            .control_context
            .as_ref()
            .and_then(|context| context.submission_cancelled.as_ref())
            .is_some_and(|cancelled| cancelled.load(std::sync::atomic::Ordering::Acquire))
        {
            return Err(ProviderError::Cancelled);
        }
        request.input = super::ProviderInput::Text {
            text: input.text().to_owned(),
            attachments: input.attachments().to_vec(),
        };
        request.submission = input.submission();
        request.recovered_input.clear();
        Ok(request)
    }

    /// Retain steering coverage before dispatch and fence native acknowledgment independently.
    pub(crate) async fn steer<F, Fut>(
        &self,
        scope: &InvocationScope,
        input: super::SteeringInput,
        send: F,
    ) -> Result<(), ProviderError>
    where
        F: FnOnce(super::SteeringInput) -> Fut,
        Fut: Future<Output = Result<(), ProviderError>>,
    {
        let delivery = self
            .tasks
            .lock()
            .await
            .get(&scope.invocation_id)
            .filter(|task| task.owner == scope.owner)
            .map(|task| task.delivery.clone())
            .ok_or_else(|| ProviderError::NotConsumed {
                message: "steering has no retained native execution".into(),
            })?;
        let submission = input
            .submission()
            .map(|receipt| SubmissionId::try_from(receipt.id().to_owned()))
            .transpose()
            .map_err(|error| protocol(&error.to_string()))?;
        let registration = delivery.register(submission)?;
        let result =
            match tokio::time::timeout(std::time::Duration::from_secs(30), send(input)).await {
                Ok(result) => result,
                Err(_) => Err(ProviderError::DeliveryUncertain {
                    message: "provider steering acknowledgment exceeded 30 seconds".into(),
                }),
            };
        registration.complete(&result)?;
        result
    }

    /// Install the only consumer before initialization or any native input dispatch.
    pub(crate) fn subscribe(
        &self,
        owner: ExecutionOwner,
    ) -> Result<ProviderEventStream, ProviderError> {
        let mut owners = self
            .owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?;
        let events = owners.entry(owner.clone()).or_insert_with(|| {
            let (sink, stream) = super::events::channel(super::EventScope::SessionOperation {
                owner,
                operation_id: uuid::Uuid::new_v4().to_string(),
            });
            OwnerEvents {
                sink,
                stream: Some(stream),
            }
        });
        events
            .stream
            .take()
            .ok_or_else(|| protocol("session already has an event consumer"))
    }

    /// Get the owner-wide publisher installed before native initialization.
    pub(crate) fn session_sink(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<ProviderEventSink, ProviderError> {
        self.owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?
            .get(owner)
            .map(|events| events.sink.clone())
            .ok_or_else(|| protocol("session observation must be installed before initialization"))
    }

    /// Bind a scoped publication to the retained session channel.
    pub(crate) fn sink(&self, scope: &InvocationScope) -> Result<ProviderEventSink, ProviderError> {
        self.owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?
            .get(&scope.owner)
            .ok_or_else(|| protocol("session observation must be installed before submission"))?
            .sink
            .invocation_sink(scope)
            .map_err(ProviderError::from)
    }

    /// Run admitted work independently of its acceptance waiter and keep observing through completion.
    pub(crate) async fn start<F, Fut>(
        self: &Arc<Self>,
        scope: InvocationScope,
        covered: Vec<SubmissionId>,
        acknowledgement: Acknowledgement,
        completion: Option<Arc<CompletionSlot>>,
        terminal_boundary: TerminalBoundary,
        run: F,
    ) -> Result<(), ProviderError>
    where
        F: FnOnce(ProviderEventSink) -> Fut + Send + 'static,
        Fut: Future<Output = Result<ProviderCompletion, ProviderError>> + Send + 'static,
    {
        let publication = Arc::new(ExecutionPublication {
            scope: scope.clone(),
            anchor: Mutex::new(None),
        });
        let sink = self.sink(&scope)?.with_observer(publication.clone());
        let (stop, mut stopped) = tokio::sync::watch::channel(false);
        let (respond, response) = tokio::sync::oneshot::channel();
        let previous = {
            let mut tasks = self.tasks.lock().await;
            tasks.retain(|_, invocation| !invocation.task.is_finished());
            if let Some(previous) = tasks.get(&scope.invocation_id)
                && previous.delivery.accepting()?
            {
                return Err(protocol("invocation is already dispatched"));
            }
            tasks.remove(&scope.invocation_id)
        };
        if let Some(previous) = previous {
            previous.task.await.map_err(|error| {
                protocol(&format!("previous invocation did not drain: {error}"))
            })?;
        }
        let mut tasks = self.tasks.lock().await;
        if tasks.contains_key(&scope.invocation_id) {
            return Err(protocol("invocation was concurrently dispatched"));
        }
        let (mut acceptance, local) = match acknowledgement {
            Acknowledgement::Local(submission) => (None, Some(submission)),
            Acknowledgement::Native(acceptance) => (acceptance, None),
        };
        let delivery = DeliverySet::new(covered);
        let completion_delivery = delivery.clone();
        let task = tokio::spawn(async move {
            let mut respond = Some(respond);
            let locally_accepted = local.is_some();
            if let Some(Some(submission)) = local {
                let admitted = submission
                    .sending(super::submission::SubmissionTarget::Untracked)
                    .and_then(|_| submission.accepted("local mock admission".into()));
                if let Err(error) = admitted {
                    if let Some(respond) = respond.take() {
                        let _ = respond.send(Err(error.into()));
                    }
                    return;
                }
            }
            if (locally_accepted
                || acceptance
                    .as_ref()
                    .is_some_and(|accepted| *accepted.borrow()))
                && let Some(respond) = respond.take()
            {
                let _ = respond.send(Ok(()));
            }
            // The session task retains adapter futures without carrying their native state inline.
            let mut execution = Box::pin(run(sink.clone()));
            let mut output = loop {
                tokio::select! {
                    biased;
                    _ = stopped.changed() => break Err(ProviderError::Cancelled),
                    changed = async { match acceptance.as_mut() {
                        Some(acceptance) => acceptance.changed().await,
                        None => std::future::pending().await,
                    } }, if respond.is_some() => {
                        if changed.is_err() { acceptance = None; }
                        else if acceptance.as_ref().is_some_and(|accepted| *accepted.borrow())
                            && let Some(respond) = respond.take() { let _ = respond.send(Ok(())); }
                    }
                    result = &mut execution => break result,
                }
            };
            if let (Some(slot), Ok(result)) = (&completion, &output)
                && let Err(error) = slot.publish(result.clone())
            {
                output = Err(error);
            }
            let coverage = completion_delivery.finish().await;
            let covered = match coverage {
                Ok(covered) => covered,
                Err(error) => {
                    output = Err(error);
                    Vec::new()
                }
            };
            if output.is_err()
                && let Some(slot) = &completion
            {
                match publication.observation(Vec::new(), &output) {
                    Ok(observation) => {
                        if let ExecutionUpdate::Finished { result, .. } = observation.update()
                            && let Err(error) = slot.publish_host_outcome(result.clone())
                        {
                            output = Err(error);
                        }
                    }
                    Err(error) => output = Err(error),
                }
            }
            let certificate = completion
                .as_ref()
                .map(|slot| slot.take_certificate())
                .transpose();
            let observation = match certificate {
                Ok(Some(Some(certificate))) => {
                    let certified = match &certificate {
                        ExecutionCertificate::Foreground {
                            covered,
                            continuations,
                            ..
                        } => covered
                            .iter()
                            .chain(continuations.iter().flat_map(|entry| entry.covered.iter()))
                            .cloned()
                            .collect::<HashSet<_>>(),
                        ExecutionCertificate::WithoutExecution { covered } => {
                            covered.iter().cloned().collect()
                        }
                    };
                    if covered.iter().any(|id| !certified.contains(id)) {
                        ExecutionObservation::new(publication.scope.clone(), ExecutionUpdate::ObservationLost {
                            failure: ObservationFailure::Protocol { message: "native terminal boundary did not cover every accepted input".into() }, completion: None,
                        }).map(|observation| vec![observation]).map_err(|error| protocol(&error.to_string()))
                    } else {
                        publication.certified_observations(certificate, &output)
                    }
                }
                Ok(Some(None)) | Ok(None) => match terminal_boundary {
                    TerminalBoundary::AcpPrompt | TerminalBoundary::LocalFixture => publication
                        .observation(covered, &output)
                        .map(|observation| vec![observation]),
                    TerminalBoundary::AdapterCertificate => {
                        if matches!(
                            &output,
                            Err(ProviderError::PreparationFailed(_)
                                | ProviderError::PreparationCleanup { .. })
                        ) {
                            if let Some(respond) = respond.take() {
                                let _ = respond.send(output.map(|_| ()));
                            }
                            return;
                        }
                        let failure = match &output {
                            Err(ProviderError::Observation(failure)) => failure.clone(),
                            Err(ProviderError::DeliveryUncertain { message }) => {
                                ObservationFailure::DeliveryUncertain {
                                    message: message.clone(),
                                }
                            }
                            Err(error) => ObservationFailure::Protocol {
                                message: error.to_string(),
                            },
                            Ok(_) => ObservationFailure::Protocol {
                                message: "adapter did not certify its terminal coverage".into(),
                            },
                        };
                        ExecutionObservation::new(
                            publication.scope.clone(),
                            ExecutionUpdate::ObservationLost {
                                failure,
                                completion: None,
                            },
                        )
                        .map(|observation| vec![observation])
                        .map_err(|error| protocol(&error.to_string()))
                    }
                },
                Err(error) => Err(error),
            };
            if let Some(respond) = respond.take() {
                let _ = respond.send(output.map(|_| ()));
            }
            match observation {
                Ok(observations) => {
                    for observation in observations {
                        if let Err(error) = sink
                            .send_wait(ProviderEvent::Execution { observation })
                            .await
                        {
                            sink.fail(error);
                            break;
                        }
                    }
                }
                Err(error) => sink.fail(EventDeliveryError::transport(error)),
            }
        });
        tasks.insert(
            scope.invocation_id,
            InvocationTask {
                delivery,
                owner: scope.owner,
                stop,
                task,
            },
        );
        drop(tasks);
        response
            .await
            .map_err(|_| protocol("submission task ended without delivery outcome"))?
    }

    /// Cancel pending startup for a runtime whose native session has not been submitted.
    pub(crate) async fn cancel_runtime(&self, key: &str) -> Result<(), ProviderError> {
        let owners: HashSet<_> = self
            .tasks
            .lock()
            .await
            .values()
            .filter(|task| task.owner.runtime_key() == key)
            .map(|task| task.owner.clone())
            .collect();
        for owner in owners {
            self.cancel(&owner).await?;
        }
        Ok(())
    }

    /// Cancel and collect a selected owner's local execution tasks while its stream remains open.
    pub(crate) async fn cancel(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let tasks = {
            let mut pending = self.tasks.lock().await;
            let ids: Vec<_> = pending
                .iter()
                .filter(|(_, task)| &task.owner == owner)
                .map(|(id, _)| id.clone())
                .collect();
            ids.into_iter()
                .filter_map(|id| pending.remove(&id))
                .collect::<Vec<_>>()
        };
        for task in &tasks {
            task.stop.send_replace(true);
        }
        let mut failure = None;
        for task in tasks {
            if let Err(error) = task.task.await {
                failure.get_or_insert_with(|| {
                    protocol(&format!("session execution task failed: {error}"))
                });
            }
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    /// Retire one collected owner's channel so late publishers cannot reach its replacement.
    pub(crate) async fn reset_owner(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        self.cancel(owner).await?;
        let mut requests = self
            .requests
            .lock()
            .map_err(|_| protocol("retained invocation registry poisoned"))?;
        let ids = requests
            .iter()
            .filter_map(|(id, request)| match request.owner() {
                Ok(request_owner) if &request_owner == owner => Some(Ok(id.clone())),
                Ok(_) => None,
                Err(error) => Some(Err(error)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        for id in ids {
            requests.remove(&id);
        }
        self.owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?
            .remove(owner);
        Ok(())
    }

    /// Collect all tasks before closing session publication during provider disposal.
    pub(crate) async fn shutdown(&self) -> Result<(), ProviderError> {
        let owners = self
            .owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        let mut failure = None;
        for owner in owners {
            if let Err(error) = self.cancel(&owner).await {
                failure.get_or_insert(error);
            }
        }
        self.owners
            .lock()
            .map_err(|_| protocol("session event registry poisoned"))?
            .clear();
        self.requests
            .lock()
            .map_err(|_| protocol("retained invocation registry poisoned"))?
            .clear();
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }
}

impl ExecutionPublication {
    /// Publish all certified continuations before settling the initial execution.
    fn certified_observations(
        &self,
        certificate: ExecutionCertificate,
        output: &Result<ProviderCompletion, ProviderError>,
    ) -> Result<Vec<ExecutionObservation>, ProviderError> {
        match certificate {
            ExecutionCertificate::Foreground {
                anchor,
                covered,
                initial_result,
                continuations,
            } => {
                let initial_result = initial_result.map(|result| *result);
                let published = self
                    .anchor
                    .lock()
                    .map_err(|_| protocol("execution publication poisoned"))?
                    .clone();
                if published.as_ref() != Some(&anchor) {
                    return Ok(vec![ExecutionObservation::new(self.scope.clone(), ExecutionUpdate::ObservationLost {
                        failure: ObservationFailure::Protocol { message: "certified foreground anchor conflicts with published execution start".into() },
                        completion: None,
                    }).map_err(|error| protocol(&error.to_string()))?]);
                }
                let mut anchors = HashSet::new();
                anchors.insert((anchor.thread_id.clone(), anchor.turn_id.clone()));
                let mut submissions = HashSet::new();
                for id in &covered {
                    if !submissions.insert(id.clone()) {
                        return self.invalid_certificate("native certificate repeats a submission");
                    }
                }
                for entry in &continuations {
                    if !anchors
                        .insert((entry.anchor.thread_id.clone(), entry.anchor.turn_id.clone()))
                    {
                        return self
                            .invalid_certificate("native certificate repeats an execution anchor");
                    }
                    for id in &entry.covered {
                        if !submissions.insert(id.clone()) {
                            return self
                                .invalid_certificate("native certificate repeats a submission");
                        }
                    }
                }
                if output.is_ok()
                    && continuations.last().is_some_and(|entry| {
                        !matches!(entry.result, ExecutionResult::Completed { .. })
                    })
                {
                    return self.invalid_certificate(
                        "native terminal result conflicts with successful host completion",
                    );
                }
                let mut observations = Vec::new();
                for entry in &continuations {
                    observations.push(
                        ExecutionObservation::new(
                            self.scope.clone(),
                            ExecutionUpdate::Started {
                                address: entry.anchor.clone(),
                            },
                        )
                        .map_err(|error| protocol(&error.to_string()))?,
                    );
                }
                if continuations.is_empty() {
                    observations.push(match initial_result {
                        Some(result) => ExecutionObservation::new(
                            self.scope.clone(),
                            ExecutionUpdate::Finished {
                                address: anchor,
                                covered_submissions: covered,
                                completed_at_ms: super::retry::now_ms(),
                                result: match (result, output) {
                                    (ExecutionResult::Completed { .. }, Ok(completion)) => {
                                        ExecutionResult::Completed {
                                            completion: completion.native.clone().into(),
                                        }
                                    }
                                    (result, _) => result,
                                },
                            },
                        )
                        .map_err(|error| protocol(&error.to_string()))?,
                        None => self.observation(covered, output)?,
                    });
                    return Ok(observations);
                }
                let initial_result = initial_result
                    .ok_or_else(|| protocol("initial execution lacks its terminal result"))?;
                observations.push(
                    ExecutionObservation::new(
                        self.scope.clone(),
                        ExecutionUpdate::Finished {
                            address: anchor,
                            covered_submissions: covered,
                            completed_at_ms: super::retry::now_ms(),
                            result: initial_result,
                        },
                    )
                    .map_err(|error| protocol(&error.to_string()))?,
                );
                let last = continuations.len() - 1;
                for (index, entry) in continuations.into_iter().enumerate() {
                    {
                        let result = if index == last {
                            match (entry.result, output) {
                                (ExecutionResult::Completed { .. }, Ok(completion)) => {
                                    ExecutionResult::Completed {
                                        completion: completion.native.clone().into(),
                                    }
                                }
                                (result, _) => result,
                            }
                        } else {
                            entry.result
                        };
                        observations.push(
                            ExecutionObservation::new(
                                self.scope.clone(),
                                ExecutionUpdate::Finished {
                                    address: entry.anchor,
                                    covered_submissions: entry.covered,
                                    completed_at_ms: entry.completed_at_ms,
                                    result,
                                },
                            )
                            .map_err(|error| protocol(&error.to_string()))?,
                        );
                    }
                }
                Ok(observations)
            }
            ExecutionCertificate::WithoutExecution { covered } => {
                let update = match output {
                    Ok(completion) => ExecutionUpdate::ResolvedWithoutExecution {
                        covered_submissions: covered,
                        completed_at_ms: super::retry::now_ms(),
                        result: ExecutionResult::Completed {
                            completion: completion.native.clone().into(),
                        },
                    },
                    Err(ProviderError::DeliveryUncertain { message }) => {
                        ExecutionUpdate::ObservationLost {
                            failure: ObservationFailure::DeliveryUncertain {
                                message: message.clone(),
                            },
                            completion: None,
                        }
                    }
                    Err(error) => ExecutionUpdate::ObservationLost {
                        failure: ObservationFailure::Protocol {
                            message: error.to_string(),
                        },
                        completion: None,
                    },
                };
                Ok(vec![
                    ExecutionObservation::new(self.scope.clone(), update)
                        .map_err(|error| protocol(&error.to_string()))?,
                ])
            }
        }
    }

    /// Expose malformed native evidence without publishing any partial lifecycle.
    fn invalid_certificate(
        &self,
        message: &str,
    ) -> Result<Vec<ExecutionObservation>, ProviderError> {
        Ok(vec![
            ExecutionObservation::new(
                self.scope.clone(),
                ExecutionUpdate::ObservationLost {
                    failure: ObservationFailure::Protocol {
                        message: message.into(),
                    },
                    completion: None,
                },
            )
            .map_err(|error| protocol(&error.to_string()))?,
        ])
    }

    /// Build native completion independently of the acceptance waiter's lifetime.
    fn observation(
        &self,
        covered_submissions: Vec<SubmissionId>,
        output: &Result<ProviderCompletion, ProviderError>,
    ) -> Result<ExecutionObservation, ProviderError> {
        let anchor = self
            .anchor
            .lock()
            .map_err(|_| protocol("execution publication poisoned"))?
            .clone();
        self.observation_at(anchor, covered_submissions, output)
    }

    /// Build a terminal observation for the exact native execution it describes.
    fn observation_at(
        &self,
        anchor: Option<TurnAddress>,
        covered_submissions: Vec<SubmissionId>,
        output: &Result<ProviderCompletion, ProviderError>,
    ) -> Result<ExecutionObservation, ProviderError> {
        let empty = || forge_provider_api::completion::ProviderCompletion::default().into();
        let update = match (anchor, output) {
            (_, Err(ProviderError::Observation(failure))) => ExecutionUpdate::ObservationLost {
                failure: failure.clone(),
                completion: None,
            },
            (_, Err(ProviderError::DeliveryUncertain { message })) => {
                ExecutionUpdate::ObservationLost {
                    failure: ObservationFailure::DeliveryUncertain {
                        message: message.clone(),
                    },
                    completion: None,
                }
            }
            (_, Err(ProviderError::Transport { .. } | ProviderError::Delivery(_))) => {
                ExecutionUpdate::ObservationLost {
                    failure: ObservationFailure::Disconnected,
                    completion: None,
                }
            }
            (Some(address), result) => {
                let result = match result {
                    Ok(completion) => ExecutionResult::Completed {
                        completion: completion.native.clone().into(),
                    },
                    Err(ProviderError::Cancelled) => ExecutionResult::Cancelled {
                        completion: empty(),
                    },
                    Err(ProviderError::Rejected { message }) => ExecutionResult::Failed {
                        completion: empty(),
                        failure: ExecutionFailure::Rejected {
                            message: message.clone(),
                        },
                    },
                    Err(ProviderError::Retryable(failure)) => ExecutionResult::Failed {
                        completion: empty(),
                        failure: ExecutionFailure::Retryable {
                            address: failure.address.clone(),
                            code: failure.code.clone(),
                            message: failure.message.clone(),
                        },
                    },
                    Err(error) => ExecutionResult::Failed {
                        completion: empty(),
                        failure: ExecutionFailure::Terminal {
                            code: None,
                            message: error.to_string(),
                        },
                    },
                };
                ExecutionUpdate::Finished {
                    address,
                    covered_submissions,
                    completed_at_ms: super::retry::now_ms(),
                    result,
                }
            }
            (None, result) => ExecutionUpdate::ObservationLost {
                failure: ObservationFailure::Protocol {
                    message: match result {
                        Ok(_) => "provider completed without an execution start".into(),
                        Err(error) => error.to_string(),
                    },
                },
                completion: None,
            },
        };
        ExecutionObservation::new(self.scope.clone(), update)
            .map_err(|error| protocol(&error.to_string()))
    }
}

impl EventObserver for ExecutionPublication {
    /// Retain only adapter-certified live foreground starts.
    fn additional_events(
        &self,
        event: &ProviderEvent,
    ) -> Result<Vec<ProviderEvent>, EventDeliveryError> {
        if let ProviderEvent::Execution { observation } = event {
            if observation.scope() != &self.scope {
                return Err(EventDeliveryError::transport(
                    "execution start crosses invocation ownership",
                ));
            }
            if let ExecutionUpdate::Started { address } = observation.update() {
                let mut anchor = self
                    .anchor
                    .lock()
                    .map_err(|_| EventDeliveryError::transport("execution publication poisoned"))?;
                if anchor.is_none() {
                    *anchor = Some(address.clone());
                }
            }
        }
        Ok(Vec::new())
    }
}

/// Preserve a protocol violation as a typed provider failure.
fn protocol(message: &str) -> ProviderError {
    ProviderError::Protocol {
        message: message.to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A changed host-effect snapshot cannot be consumed by an earlier settlement.
    #[test]
    fn host_effect_commit_rejects_stale_preview() {
        let slot = CompletionSlot::default();
        let first = ProviderCompletion {
            control_error: Some("first".into()),
            ..Default::default()
        };
        slot.publish(first).unwrap();
        let CompletionPreview {
            revision,
            completion: preview,
            ..
        } = slot.preview().unwrap();
        assert_eq!(preview.unwrap().control_error.as_deref(), Some("first"));
        let later = ProviderCompletion {
            plan_read: Some("later".into()),
            ..Default::default()
        };
        slot.publish(later).unwrap();
        assert!(!slot.take_revision(revision).unwrap());
        let CompletionPreview {
            revision: current,
            completion: effects,
            ..
        } = slot.preview().unwrap();
        let effects = effects.unwrap();
        assert_eq!(effects.control_error.as_deref(), Some("first"));
        assert_eq!(effects.plan_read.as_deref(), Some("later"));
        assert!(slot.take_revision(current).unwrap());
        assert!(slot.preview().unwrap().completion.is_none());
    }

    /// A new attempt clears only its previous host outcome, retaining earlier workflow effects.
    #[test]
    fn continuation_keeps_host_effects_and_clears_previous_outcome() {
        let slot = CompletionSlot::default();
        let first = ProviderCompletion {
            plan_read: Some("retained".into()),
            ..Default::default()
        };
        slot.publish(first).unwrap();
        slot.publish_host_outcome(ExecutionResult::Cancelled {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        })
        .unwrap();
        let old = slot.preview().unwrap().revision;
        slot.begin_attempt().unwrap();
        let CompletionPreview {
            revision,
            completion: effects,
            host_outcome: outcome,
            ..
        } = slot.preview().unwrap();
        assert!(revision > old);
        assert_eq!(effects.unwrap().plan_read.as_deref(), Some("retained"));
        assert!(outcome.is_none());
    }

    /// Generic native starts, including child and replay events, cannot claim the foreground anchor.
    #[test]
    fn foreground_anchor_ignores_uncertified_starts() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let publication = ExecutionPublication {
            scope: scope.clone(),
            anchor: Mutex::new(None),
        };
        for thread in ["child", "replay"] {
            let address = TurnAddress {
                scope: scope.clone(),
                thread_id: thread.into(),
                turn_id: "turn".into(),
            };
            publication.additional_events(&ProviderEvent::Turn {
                address,
                update: super::super::TurnUpdate::Started { started_at_ms: 1 },
            })?;
        }
        assert!(publication.anchor.lock().unwrap().is_none());
        let address = TurnAddress {
            scope: scope.clone(),
            thread_id: "root".into(),
            turn_id: "turn".into(),
        };
        let observation = ExecutionObservation::new(
            scope,
            ExecutionUpdate::Started {
                address: address.clone(),
            },
        )?;
        publication.additional_events(&ProviderEvent::Execution { observation })?;
        assert_eq!(publication.anchor.lock().unwrap().as_ref(), Some(&address));
        Ok(())
    }

    /// Replacing an owner never delivers old-generation publications to its new subscriber.
    #[tokio::test]
    async fn reset_owner_isolates_old_publisher_from_replacement() -> anyhow::Result<()> {
        let events = SessionEvents::default();
        let owner = ExecutionOwner::root("owner");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut old = events.subscribe(owner.clone())?;
        let old_sink = events.sink(&scope)?;
        events.reset_owner(&owner).await?;
        let mut replacement = events.subscribe(owner.clone())?;
        let new_sink = events.sink(&scope)?;
        let event = |text: &str| ProviderEvent::Diagnostic {
            scope: scope.clone().into(),
            diagnostic: crate::provider::ProviderDiagnostic {
                code: None,
                message: text.into(),
                fatal: false,
            },
        };
        old_sink.send_wait(event("old")).await?;
        assert!(old.try_next()?.is_some());
        assert!(replacement.try_next()?.is_none());
        new_sink.send_wait(event("new")).await?;
        let received = replacement
            .recv()
            .await?
            .expect("new publication must arrive");
        assert!(
            matches!(received, ProviderEvent::Diagnostic { diagnostic, .. } if diagnostic.message == "new")
        );
        Ok(())
    }

    /// Keep two separately certified native continuations open before settling the root.
    #[test]
    fn certified_continuations_publish_distinct_turn_boundaries() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let address = |turn: &str| TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: turn.to_owned().into(),
        };
        let root = address("root");
        let publication = ExecutionPublication {
            scope: scope.clone(),
            anchor: Mutex::new(Some(root.clone())),
        };
        let entry = |turn: &str, input: &str| -> anyhow::Result<NativeExecutionCertificate> {
            Ok(NativeExecutionCertificate {
                anchor: address(turn),
                covered: vec![SubmissionId::try_from(input.to_owned())?],
                result: ExecutionResult::Completed {
                    completion: forge_provider_api::completion::ProviderCompletion::default()
                        .into(),
                },
                completed_at_ms: 1,
            })
        };
        let events = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor: root.clone(),
                covered: vec![SubmissionId::try_from("initial".to_owned())?],
                initial_result: Some(Box::new(ExecutionResult::Completed {
                    completion: forge_provider_api::completion::ProviderCompletion::default()
                        .into(),
                })),
                continuations: vec![entry("first", "steer-1")?, entry("second", "steer-2")?],
            },
            &Ok(ProviderCompletion::default()),
        )?;
        assert_eq!(events.len(), 5);
        assert!(
            matches!(events[0].update(), ExecutionUpdate::Started { address } if address.turn_id.as_str() == "first")
        );
        assert!(
            matches!(events[1].update(), ExecutionUpdate::Started { address } if address.turn_id.as_str() == "second")
        );
        assert!(
            matches!(events[2].update(), ExecutionUpdate::Finished { address, .. } if address == &root)
        );
        assert!(
            matches!(events[3].update(), ExecutionUpdate::Finished { address, covered_submissions, .. } if address.turn_id.as_str() == "first" && covered_submissions[0].as_str() == "steer-1")
        );
        assert!(
            matches!(events[4].update(), ExecutionUpdate::Finished { address, covered_submissions, .. } if address.turn_id.as_str() == "second" && covered_submissions[0].as_str() == "steer-2")
        );
        Ok(())
    }

    /// Preserve a failed earlier native turn when a later execution succeeds.
    #[test]
    fn continuation_preserves_earlier_native_failure() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let anchor = TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "root".into(),
        };
        let next = TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "next".into(),
        };
        let publication = ExecutionPublication {
            scope,
            anchor: Mutex::new(Some(anchor.clone())),
        };
        let initial_result = ExecutionResult::Failed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
            failure: ExecutionFailure::Rejected {
                message: "first failed".into(),
            },
        };
        let observations = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor,
                covered: vec![SubmissionId::try_from("first".to_owned())?],
                initial_result: Some(Box::new(initial_result)),
                continuations: vec![NativeExecutionCertificate {
                    anchor: next,
                    covered: vec![SubmissionId::try_from("second".to_owned())?],
                    result: ExecutionResult::Completed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    },
                    completed_at_ms: 1,
                }],
            },
            &Ok(ProviderCompletion::default()),
        )?;
        assert!(
            matches!(observations[1].update(), ExecutionUpdate::Finished { result: ExecutionResult::Failed { failure: ExecutionFailure::Rejected { message }, .. }, .. } if message == "first failed")
        );
        assert!(matches!(
            observations[2].update(),
            ExecutionUpdate::Finished {
                result: ExecutionResult::Completed { .. },
                ..
            }
        ));
        Ok(())
    }

    /// Keep final provider metadata when the last native execution is a continuation.
    #[test]
    fn continuation_settlement_preserves_final_provider_metadata() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let address = |turn: &str| TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: turn.to_owned().into(),
        };
        let root = address("root");
        let publication = ExecutionPublication {
            scope: scope.clone(),
            anchor: Mutex::new(Some(root.clone())),
        };
        let native_result = || ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        };
        let mut completion = ProviderCompletion::default();
        completion.native.runtime.model = Some("final-model".into());
        let observations = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor: root,
                covered: vec![SubmissionId::try_from("first".to_owned())?],
                initial_result: Some(Box::new(native_result())),
                continuations: vec![NativeExecutionCertificate {
                    anchor: address("next"),
                    covered: vec![SubmissionId::try_from("second".to_owned())?],
                    result: native_result(),
                    completed_at_ms: 1,
                }],
            },
            &Ok(completion),
        )?;
        let ExecutionUpdate::Finished {
            result: ExecutionResult::Completed { completion },
            ..
        } = observations.last().unwrap().update()
        else {
            panic!("missing final completion");
        };
        let native: forge_provider_api::completion::ProviderCompletion = completion.clone().into();
        assert_eq!(native.runtime.model.as_deref(), Some("final-model"));
        Ok(())
    }

    /// Reject duplicate execution anchors before publishing any lifecycle event.
    #[test]
    fn certificate_rejects_duplicate_native_anchors() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let anchor = TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let publication = ExecutionPublication {
            scope,
            anchor: Mutex::new(Some(anchor.clone())),
        };
        let result = ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        };
        let observations = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor: anchor.clone(),
                covered: vec![SubmissionId::try_from("first".to_owned())?],
                initial_result: Some(Box::new(result.clone())),
                continuations: vec![NativeExecutionCertificate {
                    anchor,
                    covered: vec![SubmissionId::try_from("second".to_owned())?],
                    result,
                    completed_at_ms: 1,
                }],
            },
            &Ok(ProviderCompletion::default()),
        )?;
        assert_eq!(observations.len(), 1);
        assert!(matches!(
            observations[0].update(),
            ExecutionUpdate::ObservationLost { .. }
        ));
        Ok(())
    }

    /// Reject one submission claimed by two execution boundaries.
    #[test]
    fn certificate_rejects_overlapping_submission_coverage() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let anchor = TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "root".into(),
        };
        let continuation = TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "next".into(),
        };
        let publication = ExecutionPublication {
            scope,
            anchor: Mutex::new(Some(anchor.clone())),
        };
        let input = SubmissionId::try_from("input".to_owned())?;
        let result = ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        };
        let observations = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor,
                covered: vec![input.clone()],
                initial_result: Some(Box::new(result.clone())),
                continuations: vec![NativeExecutionCertificate {
                    anchor: continuation,
                    covered: vec![input],
                    result,
                    completed_at_ms: 1,
                }],
            },
            &Ok(ProviderCompletion::default()),
        )?;
        assert_eq!(observations.len(), 1);
        assert!(matches!(
            observations[0].update(),
            ExecutionUpdate::ObservationLost { .. }
        ));
        Ok(())
    }

    /// Keep completed native work distinct from a host-side rejection.
    #[test]
    fn host_rejection_preserves_native_terminal_outcome() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let anchor = TurnAddress {
            scope: scope.clone(),
            thread_id: "native".into(),
            turn_id: "turn".into(),
        };
        let publication = ExecutionPublication {
            scope,
            anchor: Mutex::new(Some(anchor.clone())),
        };
        let native = ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        };
        let observations = publication.certified_observations(
            ExecutionCertificate::Foreground {
                anchor,
                covered: vec![input("initial")],
                initial_result: Some(Box::new(native)),
                continuations: Vec::new(),
            },
            &Err(ProviderError::Rejected {
                message: "host rejected the completed response".into(),
            }),
        )?;
        assert!(matches!(
            observations.last().unwrap().update(),
            ExecutionUpdate::Finished {
                result: ExecutionResult::Completed { .. },
                ..
            }
        ));
        Ok(())
    }

    /// Goal-only completion requires explicit evidence and never fabricates a turn.
    #[test]
    fn goal_only_completion_requires_native_certificate() -> anyhow::Result<()> {
        let scope = InvocationScope::new(ExecutionOwner::root("owner"), "exchange");
        let publication = ExecutionPublication {
            scope: scope.clone(),
            anchor: Mutex::new(None),
        };
        let input = SubmissionId::try_from("resume".to_owned())?;
        let success = Ok(ProviderCompletion::default());
        let without = publication.observation(vec![input.clone()], &success)?;
        assert!(matches!(
            without.update(),
            ExecutionUpdate::ObservationLost { .. }
        ));
        let certified = publication.certified_observations(
            ExecutionCertificate::WithoutExecution {
                covered: vec![input.clone()],
            },
            &success,
        )?;
        assert!(
            matches!(certified[0].update(), ExecutionUpdate::ResolvedWithoutExecution { covered_submissions, .. } if covered_submissions == &vec![input])
        );
        Ok(())
    }

    /// An earlier native boundary cannot discard host effects when pending input continues it.
    #[test]
    fn continuation_preserves_unsettled_host_effects() {
        let slot = CompletionSlot::default();
        let mut first = ProviderCompletion {
            control_error: Some("retained control result".into()),
            ..Default::default()
        };
        first.native.evidence.workspace_changed = true;
        slot.publish(first).unwrap();
        let second = ProviderCompletion {
            plan_read: Some("current plan".into()),
            ..Default::default()
        };
        slot.publish(second).unwrap();
        let settled = slot.take().unwrap().unwrap();
        assert_eq!(
            settled.control_error.as_deref(),
            Some("retained control result")
        );
        assert_eq!(settled.plan_read.as_deref(), Some("current plan"));
        assert!(settled.native.evidence.workspace_changed);
        assert!(slot.take().unwrap().is_none());
    }

    fn input(id: &str) -> SubmissionId {
        SubmissionId::try_from(id.to_owned()).unwrap()
    }

    /// The terminal publication includes controls admitted before native completion.
    #[tokio::test]
    async fn terminal_waits_for_control_acknowledgment_and_covers_its_input() {
        let deliveries = DeliverySet::new(vec![input("initial")]);
        let steering = deliveries.register(Some(input("steer"))).unwrap();
        let completion = deliveries.finish();
        tokio::pin!(completion);
        assert!(futures_util::poll!(completion.as_mut()).is_pending());
        assert!(matches!(
            deliveries.register(Some(input("late"))),
            Err(ProviderError::NotConsumed { .. })
        ));
        steering.complete(&Ok(())).unwrap();
        assert_eq!(
            completion.await.unwrap(),
            vec![input("initial"), input("steer")]
        );
    }

    /// Explicit non-consumption is excluded from terminal coverage and remains safe to continue.
    #[tokio::test]
    async fn rejected_control_does_not_claim_terminal_coverage() {
        let deliveries = DeliverySet::new(vec![input("initial")]);
        deliveries
            .register(Some(input("steer")))
            .unwrap()
            .complete(&Err(ProviderError::NotConsumed {
                message: "native idle rejection".into(),
            }))
            .unwrap();
        assert_eq!(deliveries.finish().await.unwrap(), vec![input("initial")]);
    }

    /// Losing the acknowledgment waiter cannot become successful execution evidence.
    #[tokio::test]
    async fn abandoned_control_marks_execution_observation_uncertain() {
        let deliveries = DeliverySet::new(vec![input("initial")]);
        drop(deliveries.register(Some(input("steer"))).unwrap());
        assert!(matches!(
            deliveries.finish().await,
            Err(ProviderError::Observation(ObservationFailure::Disconnected))
        ));
    }
}
