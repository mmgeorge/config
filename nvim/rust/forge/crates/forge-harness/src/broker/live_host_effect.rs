//! Commit callback effects while their native invocation is still running.
use super::*;

#[derive(Debug, thiserror::Error)]
enum LiveHostRejection {
    #[error("live host effect already owns the invocation")]
    Busy,
    #[error("live effect has no current task")]
    TaskMissing,
    #[error("live effect has no dispatched task authority")]
    AuthorityMissing,
    #[error("live effect has no active plan")]
    PlanMissing,
    #[error("unsupported live host effect")]
    Unsupported,
    #[error("live callback must retain one ordered plan mutation")]
    MultipleMutations,
}

fn live_host_failure(error: &anyhow::Error) -> ProviderError {
    use forge_provider_api::error::{ProviderFailure, ProviderFailureKind, ProviderOperation};
    if error.is::<LiveHostRejection>() {
        ProviderError::Failure(ProviderFailure::new(
            ProviderOperation::HostEffectPublication,
            ProviderFailureKind::Rejected,
        ))
    } else if error.is::<rusqlite::Error>() {
        ProviderError::Failure(ProviderFailure::new(
            ProviderOperation::HostEffectPublication,
            ProviderFailureKind::Runtime,
        ))
    } else {
        crate::provider::lifecycle_error::classify_source(
            ProviderOperation::HostEffectPublication,
            error.as_ref(),
        )
    }
}

pub(crate) struct LiveHostState {
    scope: crate::provider::InvocationScope,
    call_id: forge_provider_api::control::ToolCallId,
    batch_revision: u64,
    reply: Option<
        tokio::sync::oneshot::Sender<Result<crate::provider::LiveHostCommit, ProviderError>>,
    >,
    committed: bool,
    plan: Option<PlanRecord>,
    permit: Option<crate::task::TaskPermit>,
    entry: Option<crate::task::TaskInputId>,
    id: crate::provider::session_execution::HostEffectId,
}

impl HarnessBroker {
    fn trace_live_host(&self, live: &LiveHostState, stage: &str, outcome: &str) {
        self.trace.record(
            &self.session.id,
            "host_effect.lifecycle",
            json!({
                "scope":live.scope,"call_id":live.call_id,"batch_revision":live.batch_revision,
                "stage":stage,"outcome":outcome,
            }),
        );
    }

    pub(super) fn begin_live_host_effect(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        request: crate::provider::LiveHostEffect,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Running { transaction, .. } = dispatch else {
            request.reject(ProviderError::Cancelled);
            return Ok(ForegroundAction::Wait);
        };
        if transaction.state.scope != request.scope || transaction.collection.is_some() {
            self.trace.record(&self.session.id, "host_effect.lifecycle", json!({"scope":request.scope,"call_id":request.call_id,"batch_revision":request.batch.revision,"stage":"rejected","outcome":"retired"}));
            request.reject(ProviderError::Cancelled);
            return Ok(ForegroundAction::Wait);
        }
        if request.batch.completion.goal_control.is_some() {
            self.trace.record(&self.session.id, "host_effect.lifecycle", json!({"scope":request.scope,"call_id":request.call_id,"batch_revision":request.batch.revision,"stage":"admitted","outcome":"goal"}));
            let (id, events) = match self.commit_live_goal_effect(transaction, &request) {
                Ok(committed) => committed,
                Err(error) => {
                    self.trace.record(&self.session.id, "host_effect.lifecycle", json!({"scope":request.scope,"call_id":request.call_id,"batch_revision":request.batch.revision,"stage":"rejected","outcome":"goal_commit_failed"}));
                    let _ = request.reply.send(Err(live_host_failure(&error)));
                    return Err(error);
                }
            };
            transaction.live_host = Some(LiveHostState {
                scope: request.scope,
                call_id: request.call_id,
                batch_revision: request.batch.revision,
                reply: Some(request.reply),
                committed: true,
                plan: None,
                permit: None,
                entry: None,
                id,
            });
            let permit = self.task_controller.current()?.map(|task| task.permit());
            transaction.live_host.as_mut().unwrap().permit = permit;
            self.trace_live_host(transaction.live_host.as_ref().unwrap(), "committed", "goal");
            let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
            delivery.events = events;
            return Ok(ForegroundAction::Effect(
                ForegroundJob::PlanTerminalPublication {
                    delivery,
                    cell: transaction.settlement.clone(),
                    id,
                },
            ));
        }
        let prepared = (|| -> Result<_> {
            anyhow::ensure!(transaction.live_host.is_none(), LiveHostRejection::Busy);
            let task = self
                .task_controller
                .current()?
                .ok_or(LiveHostRejection::TaskMissing)?;
            transaction
                .dispatched_task_permit
                .as_ref()
                .ok_or(LiveHostRejection::AuthorityMissing)?
                .validate_observation(Some(&task))?;
            let completion = &request.batch.completion;
            let plan_id = self
                .current_plan_id()?
                .ok_or(LiveHostRejection::PlanMissing)?;
            let plan = self
                .store
                .load_plan(&plan_id)?
                .ok_or(LiveHostRejection::PlanMissing)?;
            let kind = if completion.plan_submit.is_some() {
                crate::provider::session_execution::HostEffectKind::PlanSubmit
            } else if !completion.plan_edit.is_empty() {
                crate::provider::session_execution::HostEffectKind::PlanEdit
            } else if !completion.design_patch.is_empty() {
                crate::provider::session_execution::HostEffectKind::DesignPatch
            } else {
                return Err(LiveHostRejection::Unsupported.into());
            };
            let id = crate::provider::session_execution::HostEffectId {
                batch_revision: request.batch.revision,
                kind,
                ordinal: 0,
            };
            let permit = task.permit();
            let job = if let Some(submit) = completion.plan_submit.clone() {
                ForegroundJob::LivePlanSubmission(Box::new(
                    foreground_plan_effect::PlanSubmissionJob {
                        store: self.plan_file.clone(),
                        session_id: self.session.id.clone(),
                        workspace: PathBuf::from(&self.session.workspace),
                        plan: plan.clone(),
                        request: submit,
                        id,
                        cell: transaction.settlement.clone(),
                        task_controller: self.task_controller.clone(),
                        permit: permit.clone(),
                    },
                ))
            } else {
                anyhow::ensure!(
                    completion.plan_edit.len() + completion.design_patch.len() == 1,
                    LiveHostRejection::MultipleMutations
                );
                let mutation = if let Some(edit) = completion.plan_edit.first() {
                    foreground_plan_effect::PlanMutation::Edit(edit.clone())
                } else {
                    foreground_plan_effect::PlanMutation::Design(completion.design_patch[0].clone())
                };
                ForegroundJob::LivePlanMutation(Box::new(foreground_plan_effect::PlanMutationJob {
                    store: self.plan_file.clone(),
                    session_id: self.session.id.clone(),
                    workspace: PathBuf::from(&self.session.workspace),
                    plan: plan.clone(),
                    mutation,
                    id,
                    cell: transaction.settlement.clone(),
                    task_controller: self.task_controller.clone(),
                    permit: permit.clone(),
                    now_ms: self.clock.now_ms(),
                }))
            };
            Ok((job, plan, permit, id))
        })();
        match prepared {
            Ok((job, plan, permit, id)) => {
                self.trace.record(&self.session.id, "host_effect.lifecycle", json!({"scope":request.scope,"call_id":request.call_id,"batch_revision":request.batch.revision,"stage":"admitted","outcome":"plan"}));
                transaction.live_host = Some(LiveHostState {
                    scope: request.scope,
                    call_id: request.call_id,
                    batch_revision: request.batch.revision,
                    reply: Some(request.reply),
                    committed: false,
                    plan: Some(plan),
                    permit: Some(permit),
                    entry: transaction.dispatched_entry_id.clone(),
                    id,
                });
                Ok(ForegroundAction::Effect(job))
            }
            Err(error) => {
                self.trace.record(&self.session.id, "host_effect.lifecycle", json!({"scope":request.scope,"call_id":request.call_id,"batch_revision":request.batch.revision,"stage":"rejected","outcome":"plan_preparation_failed"}));
                request.reject(live_host_failure(&error));
                Err(error)
            }
        }
    }

    pub(super) fn complete_live_plan_submission(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<foreground_plan_effect::PlanSubmissionResult>,
    ) -> Result<ForegroundAction> {
        let (ForegroundDispatch::Running { transaction, .. }
        | ForegroundDispatch::Finalizing { transaction, .. }) = dispatch
        else {
            anyhow::bail!("live host effect lost its foreground");
        };
        let live = transaction
            .live_host
            .as_mut()
            .context("live plan effect has no owner")?;
        let permit = live
            .permit
            .clone()
            .context("live plan submission has no task authority")?;
        let events = self.commit_plan_submission(
            &mut transaction.state.interaction,
            live.plan
                .as_mut()
                .context("live plan submission has no plan")?,
            &permit,
            live.entry.as_ref(),
            result?,
        )?;
        live.committed = true;
        live.permit = None;
        let task = self
            .task_controller
            .current()?
            .context("live plan task disappeared")?;
        permit.validate_observation(Some(&task))?;
        transaction.dispatched_task_permit = Some(task.permit());
        live.permit = Some(task.permit());
        self.store.save_exchange(&transaction.state.interaction)?;
        self.trace_live_host(live, "committed", "plan_submission");
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
        delivery.events = events;
        Ok(ForegroundAction::Effect(
            ForegroundJob::PlanTerminalPublication {
                delivery,
                cell: transaction.settlement.clone(),
                id: live.id,
            },
        ))
    }

    pub(super) fn complete_live_plan_mutation(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<foreground_plan_effect::PlanMutationResult>,
    ) -> Result<ForegroundAction> {
        let (ForegroundDispatch::Running { transaction, .. }
        | ForegroundDispatch::Finalizing { transaction, .. }) = dispatch
        else {
            anyhow::bail!("live mutation lost its foreground");
        };
        let live = transaction
            .live_host
            .as_mut()
            .context("live mutation has no owner")?;
        let permit = live
            .permit
            .clone()
            .context("live plan mutation has no task authority")?;
        self.commit_plan_mutation(
            live.plan
                .as_mut()
                .context("live plan mutation has no plan")?,
            &permit,
            live.entry.as_ref(),
            result?,
        )?;
        live.committed = true;
        live.permit = None;
        let task = self
            .task_controller
            .current()?
            .context("live planning task disappeared")?;
        permit.validate_observation(Some(&task))?;
        transaction.dispatched_task_permit = Some(task.permit());
        live.permit = Some(task.permit());
        self.trace_live_host(live, "committed", "plan_mutation");
        let mut live = transaction
            .live_host
            .take()
            .expect("committed live mutation retains its reply");
        self.trace_live_host(&live, "replied", "committed");
        if let Some(reply) = live.reply.take() {
            let _ = reply.send(Ok(crate::provider::LiveHostCommit {
                task_permit: live.permit.clone(),
            }));
        }
        Ok(ForegroundAction::Wait)
    }

    pub(super) fn complete_live_host_publication(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<Vec<SessionEvent>>,
    ) -> Result<ForegroundAction> {
        let (ForegroundDispatch::Running { transaction, .. }
        | ForegroundDispatch::Finalizing { transaction, .. }) = dispatch
        else {
            anyhow::bail!("live host publication lost its foreground");
        };
        let mut live = transaction
            .live_host
            .take()
            .context("live host publication has no owner")?;
        match result {
            Ok(_) => {
                self.trace_live_host(&live, "replied", "committed");
                if let Some(reply) = live.reply.take() {
                    let _ = reply.send(Ok(crate::provider::LiveHostCommit {
                        task_permit: live.permit.clone(),
                    }));
                }
                Ok(ForegroundAction::Wait)
            }
            Err(error) => {
                self.trace_live_host(
                    &live,
                    "replied",
                    if live.committed {
                        "committed_publication_pending"
                    } else {
                        "uncommitted"
                    },
                );
                if let Some(reply) = live.reply.take() {
                    let _ = reply.send(if live.committed {
                        Ok(crate::provider::LiveHostCommit {
                            task_permit: live.permit.clone(),
                        })
                    } else {
                        Err(ProviderError::DeliveryUncertain)
                    });
                }
                Err(error)
            }
        }
    }
}

impl ForegroundDispatch {
    /// Release a callback observer on failure while retaining its mutation evidence for recovery.
    pub(crate) fn fail_live_host_observer(
        &mut self,
        trace: &crate::trace::TraceStore,
        session: &str,
    ) {
        let transaction = match self {
            Self::Running { transaction, .. } | Self::Finalizing { transaction, .. } => transaction,
            Self::Admitting(_) | Self::Done(_) | Self::Transitioning => return,
        };
        transaction.fail_live_host_observer(trace, session);
    }
}

impl ForegroundTransaction {
    /// Resolve callback waiters before collecting their native invocation, retaining durable effects.
    pub(crate) fn fail_live_host_observer(
        &mut self,
        trace: &crate::trace::TraceStore,
        session: &str,
    ) {
        if let Some(live) = self.live_host.as_mut()
            && let Some(reply) = live.reply.take()
        {
            trace.record(session, "host_effect.lifecycle", json!({"scope":live.scope,"call_id":live.call_id,
                "batch_revision":live.batch_revision,"stage":"replied",
                "outcome":if live.committed { "committed_publication_pending" } else { "recovery_required" }}));
            let _ = reply.send(if live.committed {
                Ok(crate::provider::LiveHostCommit {
                    task_permit: live.permit.clone(),
                })
            } else {
                Err(ProviderError::DeliveryUncertain)
            });
        }
        self.live_host_receiver.close();
        while let Ok(request) = self.live_host_receiver.try_recv() {
            trace.record(
                session,
                "host_effect.lifecycle",
                json!({"scope":request.scope,
                "call_id":request.call_id,"batch_revision":request.batch.revision,
                "stage":"rejected","outcome":"retired"}),
            );
            request.reject(ProviderError::Cancelled);
        }
    }
}
