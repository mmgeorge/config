use super::event_stream;
use anyhow::Context;
use std::sync::Arc;

/// Identify the independently owned native inventories contributing to one binding.
#[derive(Clone, Copy)]
pub(super) enum TerminalSource {
    Main,
    Helper,
}

/// Serialize complete binding inventories instead of letting independent readers replace each other.
pub(super) struct TerminalPublisher {
    owner: forge_provider_api::ThreadId,
    sink: forge_provider_api::ProviderEventSink,
    state: std::sync::Mutex<PublishedInventories>,
    publication: tokio::sync::Mutex<()>,
}

impl TerminalPublisher {
    /// Create an empty binding inventory before either native reader is attached.
    pub(super) fn new(
        owner: forge_provider_api::ThreadId,
        sink: forge_provider_api::ProviderEventSink,
    ) -> Arc<Self> {
        Arc::new(Self {
            owner,
            sink,
            state: std::sync::Mutex::new(PublishedInventories::default()),
            publication: tokio::sync::Mutex::new(()),
        })
    }

    /// Replace one reader's authority while retaining the other reader's inventory.
    pub(super) fn source(self: &Arc<Self>, source: TerminalSource) -> TerminalSourceHandle {
        let generation = uuid::Uuid::new_v4();
        let mut state = self
            .state
            .lock()
            .expect("terminal publication state is not poisoned");
        let slot = match source {
            TerminalSource::Main => &mut state.main,
            TerminalSource::Helper => &mut state.helper,
        };
        // The main reader attaches with the native session, so its inventory starts unreported.
        let snapshot = match source {
            TerminalSource::Main => forge_provider_api::terminal::TerminalInventory::opening(),
            TerminalSource::Helper => {
                forge_provider_api::terminal::TerminalInventory::observed(Vec::new())
            }
        };
        let lease = match slot {
            Some(previous) => {
                previous.lease.send_replace(generation);
                previous.generation = generation;
                previous.lease.subscribe()
            }
            None => {
                let (lease, receiver) = tokio::sync::watch::channel(generation);
                *slot = Some(PublishedInventory {
                    generation,
                    lease,
                    snapshot,
                });
                receiver
            }
        };
        TerminalSourceHandle {
            publisher: self.clone(),
            source,
            generation,
            lease,
        }
    }

    /// Get the same combined inventory published to the binding event stream.
    pub(super) fn snapshot(&self) -> forge_provider_api::terminal::TerminalInventory {
        self.state
            .lock()
            .expect("terminal publication state is not poisoned")
            .snapshot()
    }
}

/// Retain only one source generation's authority to replace its inventory component.
#[derive(Clone)]
pub(super) struct TerminalSourceHandle {
    publisher: Arc<TerminalPublisher>,
    source: TerminalSource,
    generation: uuid::Uuid,
    lease: tokio::sync::watch::Receiver<uuid::Uuid>,
}

impl TerminalSourceHandle {
    /// Publish the union atomically; retiring a source also interrupts an unadmitted send.
    pub(super) async fn publish(
        &self,
        snapshot: forge_provider_api::terminal::TerminalInventory,
    ) -> Result<(), forge_provider_api::ProviderError> {
        let mut lease = self.lease.clone();
        if *lease.borrow_and_update() != self.generation {
            return Ok(());
        }
        let _publication = tokio::select! {
            biased;
            _ = lease.changed() => return Ok(()),
            publication = self.publisher.publication.lock() => publication,
        };
        let merged = {
            let mut state = self
                .publisher
                .state
                .lock()
                .expect("terminal publication state is not poisoned");
            let slot = match self.source {
                TerminalSource::Main => &mut state.main,
                TerminalSource::Helper => &mut state.helper,
            };
            let Some(current) = slot
                .as_mut()
                .filter(|current| current.generation == self.generation)
            else {
                return Ok(());
            };
            current.snapshot = snapshot;
            state.snapshot()
        };
        let writer = self.publisher.sink.activity_writer();
        tokio::select! {
            biased;
            _ = lease.changed() => Ok(()),
            result = writer.send_wait(forge_provider_api::AdapterActivity::Thread {
                thread_id: self.publisher.owner.clone(), event: forge_provider_api::ThreadEvent::TerminalsUpdated(Ok(merged)),
            }) => result.map_err(Into::into),
        }
    }
}

#[derive(Default)]
struct PublishedInventories {
    main: Option<PublishedInventory>,
    helper: Option<PublishedInventory>,
}

impl PublishedInventories {
    /// Combine both source components in stable main-then-helper order.
    fn snapshot(&self) -> forge_provider_api::terminal::TerminalInventory {
        let mut snapshot = forge_provider_api::terminal::TerminalInventory::observed(Vec::new());
        for inventory in self.main.iter().chain(self.helper.iter()) {
            snapshot.complete &= inventory.snapshot.complete;
            snapshot
                .terminal
                .extend(inventory.snapshot.terminal.iter().cloned());
        }
        snapshot
    }
}

struct PublishedInventory {
    generation: uuid::Uuid,
    lease: tokio::sync::watch::Sender<uuid::Uuid>,
    snapshot: forge_provider_api::terminal::TerminalInventory,
}

/// Issue opaque terminal handles within one exact live binding.
#[derive(Default)]
pub(super) struct TerminalRegistry {
    native: std::collections::HashMap<forge_provider_api::terminal::TerminalId, String>,
}

impl TerminalRegistry {
    /// Replace native identities with issued handles and retire handles missing from this snapshot.
    pub(super) fn issue(
        &mut self,
        snapshot: &mut forge_provider_api::terminal::TerminalInventory,
    ) -> Result<(), forge_provider_api::ProviderError> {
        let present = snapshot
            .terminal
            .iter()
            .map(|terminal| terminal.id.as_str().to_owned())
            .collect::<std::collections::HashSet<_>>();
        self.native.retain(|_, native| present.contains(native));
        for terminal in &mut snapshot.terminal {
            let native = terminal.id.as_str().to_owned();
            let issued = self
                .native
                .iter()
                .find_map(|(issued, target)| (target == &native).then(|| issued.clone()));
            terminal.id = if let Some(issued) = issued {
                issued
            } else {
                let issued = forge_provider_api::terminal::TerminalId::new(
                    uuid::Uuid::new_v4().to_string(),
                )?;
                self.native.insert(issued.clone(), native);
                issued
            };
        }
        Ok(())
    }

    /// Resolve only a handle previously issued by this binding, without native-ID fallback.
    pub(super) fn resolve(
        &self,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Option<String> {
        self.native.get(terminal).cloned()
    }

    /// Retire a successfully stopped occurrence before a native task ID can be reused.
    pub(super) fn retire(&mut self, terminal: &forge_provider_api::terminal::TerminalId) {
        self.native.remove(terminal);
    }
}

/// Publish inventory through the binding's normalized stream and opaque handle registry.
#[derive(Clone)]
pub(super) struct TerminalPublication {
    pub(super) owner: forge_provider_api::ThreadId,
    pub(super) sink: forge_provider_api::ProviderEventSink,
    pub(super) registry: Arc<tokio::sync::Mutex<TerminalRegistry>>,
    pub(super) publisher: Option<Arc<TerminalPublisher>>,
}

/// Retains terminal observations after the prompt event subscription ends.
pub struct CopilotTerminalMonitor {
    session: std::sync::Weak<github_copilot_sdk::session::Session>,
    snapshot: tokio::sync::watch::Receiver<
        Option<
            std::result::Result<
                forge_provider_api::terminal::TerminalInventory,
                forge_provider_api::ProviderError,
            >,
        >,
    >,
    task: Option<tokio::task::JoinHandle<Result<(), forge_provider_api::ProviderError>>>,
    shutdown: Option<tokio::sync::oneshot::Sender<()>>,
}

impl Drop for CopilotTerminalMonitor {
    fn drop(&mut self) {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
    }
}

impl CopilotTerminalMonitor {
    /// Subscribe before the first query so terminal transitions cannot be lost at attachment.
    pub fn new(
        session: &std::sync::Arc<super::handle::CopilotSession>,
        trace: Arc<dyn forge_provider_api::trace::ProviderTrace>,
        publication: TerminalPublication,
    ) -> Self {
        let event = event_stream::SessionEventStream::new(
            session,
            |event| event.event_type == "session.background_tasks_changed",
            trace,
            publication.owner.to_string(),
        );
        Self::from_stream(event, Arc::downgrade(&session.inner), publication)
    }

    /// Retain the reader inside the cooperative monitor until explicit collection joins both.
    fn from_stream(
        mut event: event_stream::SessionEventStream,
        session: std::sync::Weak<github_copilot_sdk::session::Session>,
        publication: TerminalPublication,
    ) -> Self {
        let monitored_session = session.clone();
        let publisher = publication.publisher.clone().unwrap_or_else(|| {
            TerminalPublisher::new(publication.owner.clone(), publication.sink.clone())
        });
        let source = publisher.source(TerminalSource::Main);
        let (send, snapshot) = tokio::sync::watch::channel(None);
        let (shutdown, closing) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let observe = async {
                let mut first = true;
                let mut previous = None;
                loop {
                    let Some(session) = session.upgrade() else {
                        return;
                    };
                    let result = async {
                        // Refresh once at attachment; refreshing on each notification would
                        // manufacture another notification and create a feedback loop.
                        if first {
                            session
                                .rpc()
                                .tasks()
                                .refresh()
                                .await
                                .context("refresh Copilot background terminals")?;
                        }
                        let result = session
                            .rpc()
                            .tasks()
                            .list()
                            .await
                            .context("list Copilot background terminals")?;
                        crate::terminal::parse_snapshot(&serde_json::to_value(result)?, true)
                    }
                    .await
                    .map_err(|_: anyhow::Error| {
                        native_failure(
                            forge_provider_api::ProviderOperation::TerminalList,
                            forge_provider_api::ProviderFailureKind::Runtime,
                        )
                    });
                    first = false;
                    let result = match result {
                        Ok(mut snapshot) => {
                            match publication.registry.lock().await.issue(&mut snapshot) {
                                Ok(()) => Ok(snapshot),
                                Err(error) => Err(error),
                            }
                        }
                        Err(error) => Err(error),
                    };
                    if let Ok(snapshot) = &result
                        && previous.as_ref() != Some(snapshot)
                    {
                        if source.publish(snapshot.clone()).await.is_err() {
                            return;
                        }
                        previous = Some(snapshot.clone());
                    }
                    if result.is_err() {
                        previous = None;
                        let failure = forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TerminalList,
                            forge_provider_api::ProviderFailureKind::Runtime,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Unchanged,
                        );
                        if publication
                            .sink
                            .activity_writer()
                            .send_wait(forge_provider_api::AdapterActivity::Thread {
                                thread_id: publication.owner.clone(),
                                event: forge_provider_api::ThreadEvent::TerminalsUpdated(Err(
                                    failure,
                                )),
                            })
                            .await
                            .is_err()
                        {
                            return;
                        }
                    }
                    if send.send(Some(result)).is_err() {
                        return;
                    }
                    drop(session);
                    match event.recv().await {
                        Ok(_) => {}
                        Err(_) => {
                            let _ = send.send(Some(Err(native_failure(
                                forge_provider_api::ProviderOperation::TerminalList,
                                forge_provider_api::ProviderFailureKind::Transport,
                            ))));
                            return;
                        }
                    }
                }
            };
            tokio::select! { _ = closing => {}, _ = observe => {} }
            event.close().await.map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::ThreadClose,
                    forge_provider_api::ProviderFailureKind::Runtime,
                )
            })
        });
        Self {
            session: monitored_session,
            snapshot,
            task: Some(task),
            shutdown: Some(shutdown),
        }
    }
}

impl CopilotTerminalMonitor {
    /// Check whether this monitor still observes the requested native session.
    pub fn observes(&self, session: &Arc<super::handle::CopilotSession>) -> bool {
        let session = &session.inner;
        self.task.as_ref().is_some_and(|task| !task.is_finished())
            && self.session.ptr_eq(&Arc::downgrade(session))
    }

    /// Get the latest completed native inventory without waiting for another refresh.
    pub fn snapshot(
        &self,
    ) -> Option<
        std::result::Result<
            forge_provider_api::terminal::TerminalInventory,
            forge_provider_api::ProviderError,
        >,
    > {
        self.snapshot.borrow().clone()
    }

    /// Cancel and collect the observation task before releasing the session.
    pub async fn shutdown(&mut self) -> Result<(), forge_provider_api::ProviderError> {
        if let Some(shutdown) = self.shutdown.take() {
            let _ = shutdown.send(());
        }
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        let result = task.await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::ThreadClose,
                forge_provider_api::ProviderFailureKind::Runtime,
            )
        });
        self.task.take();
        result?
    }
}

fn native_failure(
    operation: forge_provider_api::ProviderOperation,
    kind: forge_provider_api::ProviderFailureKind,
) -> forge_provider_api::ProviderError {
    forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        operation, kind,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A reused native task ID cannot turn an old selection into authority over another binding.
    #[test]
    fn issued_terminal_handles_reject_other_bindings_and_reused_native_tasks() {
        let snapshot = || {
            forge_provider_api::terminal::TerminalInventory::observed(vec![
                forge_provider_api::terminal::BackgroundTerminal {
                    id: forge_provider_api::terminal::TerminalId::new("native-reused").unwrap(),
                    command: "fixture".into(),
                },
            ])
        };
        let mut first = TerminalRegistry::default();
        let mut original = snapshot();
        first.issue(&mut original).unwrap();
        let old = original.terminal[0].id.clone();
        let mut replacement = TerminalRegistry::default();
        let mut current = snapshot();
        replacement.issue(&mut current).unwrap();
        assert!(replacement.resolve(&old).is_none());
        assert_ne!(old, current.terminal[0].id);
        first
            .issue(&mut forge_provider_api::terminal::TerminalInventory::observed(Vec::new()))
            .unwrap();
        original = snapshot();
        first.issue(&mut original).unwrap();
        assert!(
            first.resolve(&old).is_none(),
            "a retired terminal handle resolved to a newer occurrence"
        );
    }
}
