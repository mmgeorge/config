//! Stage settings I/O as facts while the exchange owner retains every commit decision.

use super::command::{CommandId, SessionConfigurationPatch};
use super::failure::OperationFailure;
use super::transition::ChildCollectionPolicy;
use crate::broker::publication::SessionUpdate;
use crate::broker::{
    ConfigurationCandidate, ConfigurationCatalogResult, ConfigurationEffect,
    ConfigurationPreparation, ConfigurationReconcileResult, HarnessBroker,
};
use crate::provider::ProviderModel;

/// Retain validation, provider work, and host commit for one immutable settings patch.
pub(super) struct SettingsRun {
    patch: SessionConfigurationPatch,
    owner: SettingsOwner,
    stage: SettingsStage,
    resource_notices: ResourceNoticePolicy,
}

impl SettingsRun {
    /// Create a preflight phase that validates intent before interrupting any work.
    pub(super) fn new(command: CommandId, patch: SessionConfigurationPatch) -> Self {
        Self {
            patch,
            owner: SettingsOwner::Command(command),
            stage: SettingsStage::Preparing(PreparationPurpose::Inspect),
            resource_notices: ResourceNoticePolicy::Preserve,
        }
    }

    /// Prepare restricted per-input settings as internal work without a synthetic caller command.
    pub(super) fn for_input(
        settings: super::command::QueuedModelSettings,
    ) -> Result<Self, serde_json::Error> {
        Ok(Self {
            patch: SessionConfigurationPatch::queued_model(settings)?,
            owner: SettingsOwner::Input,
            stage: SettingsStage::Preparing(PreparationPurpose::Inspect),
            resource_notices: ResourceNoticePolicy::Preserve,
        })
    }

    /// Rebuild authority after collection instead of committing the preflight snapshot.
    pub(super) fn collected(&mut self) {
        assert!(matches!(self.stage, SettingsStage::AwaitingCollection));
        self.stage = SettingsStage::Preparing(PreparationPurpose::Apply);
    }

    /// Await one correlated settings fact without holding the broker or choosing a transition.
    pub(super) async fn next(&mut self) -> SettingsEvent {
        match &mut self.stage {
            SettingsStage::Catalog { task, deadline, .. } => tokio::select! {
                result = task => SettingsEvent::Catalog(result.unwrap_or_else(|error| Err(OperationFailure::JobObservationLost(error)))),
                _ = tokio::time::sleep_until(*deadline) => SettingsEvent::Deadline,
            },
            SettingsStage::Reconcile { task, deadline, .. } => tokio::select! {
                result = task => SettingsEvent::Reconciled(result.unwrap_or_else(|error| Err(OperationFailure::JobObservationLost(error)))),
                _ = tokio::time::sleep_until(*deadline) => SettingsEvent::Deadline,
            },
            SettingsStage::Cleanup { task, deadline } => tokio::select! {
                result = task => SettingsEvent::Cleaned(result.unwrap_or_else(|error| Err(OperationFailure::JobObservationLost(error)))),
                _ = tokio::time::sleep_until(*deadline) => SettingsEvent::Deadline,
            },
            SettingsStage::Preparing(_)
            | SettingsStage::Committing(_)
            | SettingsStage::Finalizing => SettingsEvent::Ready,
            SettingsStage::AwaitingCollection | SettingsStage::Consumed => {
                std::future::pending().await
            }
        }
    }

    /// Revalidate completed facts and authorize only the next owned settings stage.
    pub(super) fn update(
        &mut self,
        event: SettingsEvent,
        broker: &mut HarnessBroker,
        can_commit: bool,
    ) -> Result<SettingsProgress, OperationFailure> {
        match self.advance(event, broker, can_commit) {
            Err(OperationFailure::Domain(source))
                if source
                    .downcast_ref::<super::command::ConfigurationRejection>()
                    .is_some() =>
            {
                Err(OperationFailure::Validation(source))
            }
            result => result,
        }
    }

    /// Consume one identified stage only after its asynchronous evidence has been read.
    fn advance(
        &mut self,
        event: SettingsEvent,
        broker: &mut HarnessBroker,
        can_commit: bool,
    ) -> Result<SettingsProgress, OperationFailure> {
        // A deadline never consumes its task; recovery must join it before collecting execution.
        if matches!(event, SettingsEvent::Deadline) {
            return Err(OperationFailure::JobDeadline);
        }
        let stage = std::mem::replace(&mut self.stage, SettingsStage::Consumed);
        if !can_commit {
            // Job-result events have already joined their task. Dropping a reconciliation
            // result retains its uncertainty fence instead of certifying a provider write.
            return Err(OperationFailure::StorageUncommitted);
        }
        match event {
            SettingsEvent::Ready => match stage {
                SettingsStage::Preparing(purpose) => {
                    let preparation = broker
                        .prepare_configuration(
                            self.patch
                                .encode_legacy()
                                .map_err(|error| OperationFailure::Domain(error.into()))?,
                        )
                        .map_err(OperationFailure::Domain)?;
                    if preparation.needs_catalog(broker) {
                        let job = broker
                            .prepare_configuration_catalog()
                            .map_err(OperationFailure::Domain)?;
                        self.stage = SettingsStage::Catalog {
                            purpose,
                            preparation,
                            task: tokio::spawn(async move {
                                job.run().await.map_err(OperationFailure::Domain)
                            }),
                            deadline: deadline(30),
                        };
                        return Ok(SettingsProgress::Waiting);
                    }
                    self.validate(broker, purpose, preparation, &[])
                }
                SettingsStage::Committing(candidate) => {
                    let committed = broker
                        .commit_configuration_candidate(candidate)
                        .map_err(OperationFailure::Domain)?;
                    // The common final configuration update publishes the committed snapshot once.
                    self.stage = match committed.cleanup {
                        Some(job) => SettingsStage::Cleanup {
                            task: tokio::spawn(async move {
                                job.run().await.map_err(OperationFailure::Domain)
                            }),
                            deadline: deadline(60),
                        },
                        None => SettingsStage::Finalizing,
                    };
                    Ok(SettingsProgress::Waiting)
                }
                SettingsStage::Finalizing => {
                    if let SettingsOwner::Command(command) = &self.owner {
                        let action = serde_json::json!({"operation_id":command.as_str(),"config":self.patch.encode_legacy().map_err(|error| OperationFailure::Domain(error.into()))?});
                        broker
                            .confirm_configuration_application(&action)
                            .map_err(OperationFailure::Domain)?;
                    }
                    let updates = match self.resource_notices {
                        ResourceNoticePolicy::Preserve => Vec::new(),
                        ResourceNoticePolicy::Refresh => {
                            let mut notices = broker
                                .prepare_configuration_mcp_notices()
                                .map_err(OperationFailure::Domain)?;
                            let updates = std::mem::take(&mut notices.deliveries)
                                .into_iter()
                                .flat_map(|delivery| delivery.into_updates())
                                .collect();
                            broker
                                .commit_configuration_mcp_notices(notices)
                                .map_err(OperationFailure::Domain)?;
                            updates
                        }
                    };
                    Ok(SettingsProgress::Committed { updates })
                }
                SettingsStage::Catalog { .. }
                | SettingsStage::AwaitingCollection
                | SettingsStage::Reconcile { .. }
                | SettingsStage::Cleanup { .. }
                | SettingsStage::Consumed => {
                    unreachable!("only synchronous settings stages produce ready facts")
                }
            },
            SettingsEvent::Catalog(result) => {
                let SettingsStage::Catalog {
                    purpose,
                    preparation,
                    task: _,
                    deadline: _,
                } = stage
                else {
                    unreachable!("catalog facts belong to the retained catalog task")
                };
                let models = broker
                    .commit_configuration_catalog(result?)
                    .map_err(OperationFailure::Domain)?;
                self.validate(broker, purpose, preparation, &models)
            }
            SettingsEvent::Reconciled(result) => {
                let SettingsStage::Reconcile {
                    mut candidate,
                    task: _,
                    deadline: _,
                } = stage
                else {
                    unreachable!("reconciliation facts belong to the retained candidate task")
                };
                broker
                    .certify_candidate_reconcile(&mut candidate, result?)
                    .map_err(OperationFailure::Domain)?;
                self.stage = SettingsStage::Committing(candidate);
                Ok(SettingsProgress::Waiting)
            }
            SettingsEvent::Cleaned(result) => {
                let SettingsStage::Cleanup {
                    task: _,
                    deadline: _,
                } = stage
                else {
                    unreachable!("cleanup facts belong to the retained cleanup task")
                };
                broker
                    .commit_configuration_reconcile(result?)
                    .map_err(OperationFailure::Domain)?;
                self.stage = SettingsStage::Finalizing;
                Ok(SettingsProgress::Waiting)
            }
            SettingsEvent::Deadline => {
                unreachable!("deadline handling preserves the still-owned task above")
            }
        }
    }

    /// Transfer outstanding I/O into the retained collection ledger without aborting it.
    pub(super) fn into_collection_job(
        self,
    ) -> Option<tokio::task::JoinHandle<Result<(), crate::provider::ProviderError>>> {
        match self.stage {
            SettingsStage::Catalog {
                task, preparation, ..
            } => Some(tokio::spawn(async move {
                let _preparation = preparation;
                match task.await {
                    Ok(Ok(_)) => Ok(()),
                    Ok(Err(_)) | Err(_) => Err(crate::provider::ProviderError::DeliveryUncertain),
                }
            })),
            SettingsStage::Reconcile {
                task, candidate, ..
            } => Some(tokio::spawn(async move {
                let _candidate = candidate;
                match task.await {
                    Ok(Ok(_)) => Ok(()),
                    Ok(Err(_)) | Err(_) => Err(crate::provider::ProviderError::DeliveryUncertain),
                }
            })),
            SettingsStage::Cleanup { task, .. } => Some(tokio::spawn(async move {
                match task.await {
                    Ok(Ok(_)) => Ok(()),
                    Ok(Err(_)) | Err(_) => Err(crate::provider::ProviderError::DeliveryUncertain),
                }
            })),
            SettingsStage::Preparing(_)
            | SettingsStage::AwaitingCollection
            | SettingsStage::Committing(_)
            | SettingsStage::Finalizing
            | SettingsStage::Consumed => None,
        }
    }

    /// Validate the effective selection before deciding whether collection is necessary.
    fn validate(
        &mut self,
        broker: &HarnessBroker,
        purpose: PreparationPurpose,
        preparation: Box<ConfigurationPreparation>,
        models: &[ProviderModel],
    ) -> Result<SettingsProgress, OperationFailure> {
        let candidate = broker
            .build_configuration_candidate(preparation, models)
            .map_err(OperationFailure::Domain)?;
        if matches!(purpose, PreparationPurpose::Inspect) {
            let children = match candidate.effect() {
                ConfigurationEffect::Local => None,
                ConfigurationEffect::ReplaceRoot => Some(ChildCollectionPolicy::Preserve),
                ConfigurationEffect::ReplaceTree => Some(ChildCollectionPolicy::Collect),
            };
            if let Some(children) = children {
                self.stage = SettingsStage::AwaitingCollection;
                return Ok(SettingsProgress::Collect(children));
            }
        }
        if let Some(revision) = self.patch.application_revision() {
            let SettingsOwner::Command(command) = &self.owner else {
                unreachable!(
                    "restricted queued settings cannot apply an external settings revision"
                )
            };
            broker
                .owner_begin_configuration_application(command, revision)
                .map_err(OperationFailure::Domain)?;
        }
        self.resource_notices = if candidate.has_resource_selection() {
            ResourceNoticePolicy::Refresh
        } else {
            ResourceNoticePolicy::Preserve
        };
        self.stage = match broker
            .prepare_candidate_reconcile(&candidate)
            .map_err(OperationFailure::Domain)?
        {
            Some(job) => SettingsStage::Reconcile {
                candidate,
                task: tokio::spawn(async move {
                    job.run().await.map_err(|error| {
                        OperationFailure::Reconciliation(
                            crate::provider::lifecycle_error::classify(
                                forge_provider_api::ProviderOperation::ReconcileSettings,
                                error,
                            ),
                        )
                    })
                }),
                deadline: deadline(60),
            },
            None => SettingsStage::Committing(candidate),
        };
        Ok(SettingsProgress::Waiting)
    }
}

/// Distinguish journaled caller configuration from restricted internal input preparation.
enum SettingsOwner {
    Command(CommandId),
    Input,
}

/// Carry one result from the exact task retained in the active settings stage.
pub(super) enum SettingsEvent {
    Ready,
    Catalog(Result<ConfigurationCatalogResult, OperationFailure>),
    Reconciled(Result<ConfigurationReconcileResult, OperationFailure>),
    Cleaned(Result<ConfigurationReconcileResult, OperationFailure>),
    Deadline,
}

impl SettingsEvent {
    /// Get a bounded stage label without configuration values or provider content.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Ready => "settings_ready",
            Self::Catalog(_) => "settings_catalog_completed",
            Self::Reconciled(_) => "settings_reconcile_completed",
            Self::Cleaned(_) => "settings_cleanup_completed",
            Self::Deadline => "settings_deadline",
        }
    }
}

/// Distinguish continued I/O, an owner-selected collection scope, and host commitment.
pub(super) enum SettingsProgress {
    Waiting,
    Collect(ChildCollectionPolicy),
    Committed { updates: Vec<SessionUpdate> },
}

#[derive(Clone, Copy)]
enum PreparationPurpose {
    Inspect,
    Apply,
}

#[derive(Clone, Copy)]
enum ResourceNoticePolicy {
    Preserve,
    Refresh,
}

enum SettingsStage {
    Preparing(PreparationPurpose),
    Catalog {
        purpose: PreparationPurpose,
        preparation: Box<ConfigurationPreparation>,
        task: tokio::task::JoinHandle<Result<ConfigurationCatalogResult, OperationFailure>>,
        deadline: tokio::time::Instant,
    },
    AwaitingCollection,
    Reconcile {
        candidate: Box<ConfigurationCandidate>,
        task: tokio::task::JoinHandle<Result<ConfigurationReconcileResult, OperationFailure>>,
        deadline: tokio::time::Instant,
    },
    Committing(Box<ConfigurationCandidate>),
    Cleanup {
        task: tokio::task::JoinHandle<Result<ConfigurationReconcileResult, OperationFailure>>,
        deadline: tokio::time::Instant,
    },
    Finalizing,
    Consumed,
}

/// Create an observation deadline; expiry does not abort the owned I/O task.
fn deadline(seconds: u64) -> tokio::time::Instant {
    tokio::time::Instant::now() + std::time::Duration::from_secs(seconds)
}
