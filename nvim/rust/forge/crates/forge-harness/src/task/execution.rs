use anyhow::{Context, Result, ensure};

use crate::exchange::Exchange;
use crate::plan::{PlanExecutionRecord, PlanExecutionState, PlanRecord, PlanState};
use crate::storage::SqliteStore;

use super::{SessionTaskController, TaskPermit, WorkflowSnapshot};

/// Distinguish phase evidence from a contract revision requiring acceptance.
pub(crate) enum ExecutionSettlementKind {
    PhaseFinished,
    RevisionSubmitted { manual_verification: Vec<String> },
}

/// Carry prepared artifacts into one controller-owned acceptance decision.
pub(crate) struct ExecutionSettlement {
    pub record: PlanExecutionRecord,
    pub plan: PlanRecord,
    pub exchange: Exchange,
    pub kind: ExecutionSettlementKind,
    pub title: String,
}

/// Publish only the execution and exchange that successfully committed together.
pub(crate) struct SettledExecution {
    pub record: PlanExecutionRecord,
    pub exchange: Exchange,
}

impl SessionTaskController {
    /// Settle provider work under the same transaction as current acceptance policy.
    pub(crate) fn settle_execution(
        &self,
        store: &mut SqliteStore,
        permit: &TaskPermit,
        settlement: ExecutionSettlement,
        now_ms: i64,
    ) -> Result<SettledExecution> {
        let ExecutionSettlement {
            mut record,
            mut plan,
            exchange,
            kind,
            title,
        } = settlement;
        self.update_with_effects(store, permit, now_ms, |task| {
            let (phase_completion, revised_manual) = match kind {
                ExecutionSettlementKind::PhaseFinished => (true, None),
                ExecutionSettlementKind::RevisionSubmitted {
                    manual_verification,
                } => (false, Some(manual_verification)),
            };
            let WorkflowSnapshot::Execute {
                execution: previous,
            } = &task.workflow
            else {
                anyhow::bail!("execution settlement requires the current execution task");
            };
            ensure!(
                phase_completion || previous.record.phase == crate::plan::PlanPhase::Resolve,
                "contract revisions require Resolve"
            );

            // Acceptance policy and its artifact decision must share the revision-fenced commit.
            let title = if !phase_completion && task.auto_accept_plan {
                plan.accepted_digest = plan.review_digest.clone();
                plan.accepted_revision = Some(plan.model_revision);
                plan.state = PlanState::Accepted;
                record.revision = plan.model_revision;
                record.manual_verification =
                    revised_manual.context("revision manual contract is missing")?;
                record
                    .revision_history
                    .push(crate::plan::execution::ExecutionRevision {
                        revision: record.revision,
                        reason: record
                            .pending_revision_reason
                            .take()
                            .context("submitted revision reason is missing")?,
                        approval: "automatic".into(),
                        recorded_at_ms: now_ms,
                    });
                record.progress = Default::default();
                record.resume_after_review = false;
                record.state = PlanExecutionState::Active;
                "Plan revision accepted (auto)"
            } else {
                title.as_str()
            };
            if !phase_completion || record.state == PlanExecutionState::Complete {
                record.append_lifecycle(
                    crate::plan::ExchangeAnchor::capture(&exchange),
                    now_ms,
                    if phase_completion {
                        crate::plan::PlanExecutionLifecycleEvent::Completed {
                            title: plan.title.clone(),
                        }
                    } else {
                        crate::plan::PlanExecutionLifecycleEvent::Phase {
                            title: title.into(),
                            phase: record.phase,
                            revision: plan.model_revision,
                            state: record.state,
                        }
                    },
                );
            }
            super::controller::validate_execution_update(&previous.record, &record)?;
            let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                unreachable!()
            };
            execution.record = record.clone();
            Ok((
                SettledExecution {
                    record,
                    exchange: exchange.clone(),
                },
                crate::storage::TaskEffects {
                    plan: vec![plan],
                    exchange: vec![exchange],
                    ..Default::default()
                },
            ))
        })
    }
}
