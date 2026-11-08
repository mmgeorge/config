use std::collections::HashMap;

use crate::exchange::{Exchange, ExchangeNode};
use crate::plan::{
    ExchangePlanEvent, PlanAudit, PlanDeviation, PlanEventContent, PlanExecutionRecord,
    PlanLifecycleKind, PlanLifecycleRecord, PlanRecord, PlanResolutionRecord,
};

/// Attach plan audit content to exchange positions before constructing the timeline.
pub(super) fn attach(
    exchanges: &mut [Exchange],
    plans: &[PlanRecord],
    lifecycle: Vec<PlanLifecycleRecord>,
    executions: Vec<PlanExecutionRecord>,
    resolutions: Vec<PlanResolutionRecord>,
    deviations: &[PlanDeviation],
    audits: &[PlanAudit],
) {
    let mut pending: HashMap<String, Vec<(i64, ExchangePlanEvent)>> = HashMap::new();
    for record in lifecycle {
        let Some(plan) = plans.iter().find(|plan| plan.id == record.plan_id) else {
            continue;
        };
        let anchor = record.anchor.clone();
        if let Some(anchor) = anchor {
            pending.entry(anchor.exchange_id).or_default().push((
                record.created_at_ms,
                ExchangePlanEvent {
                    id: record.id.clone(),
                    node_count: anchor.node_count,
                    content: PlanEventContent::Lifecycle {
                        title: if record.title.is_empty() {
                            plan.title.clone()
                        } else {
                            record.title.clone()
                        },
                        lifecycle: record,
                    },
                },
            ));
        }
    }
    for execution in executions {
        for record in execution.lifecycle.clone() {
            let anchor = record.anchor;
            if let Some(anchor) = anchor {
                pending.entry(anchor.exchange_id).or_default().push((
                    record.occurred_at_ms,
                    ExchangePlanEvent {
                        id: format!("{}:lifecycle:{}", execution.id, record.sequence),
                        node_count: anchor.node_count,
                        content: PlanEventContent::Execution {
                            event: record.event,
                        },
                    },
                ));
            }
        }
    }
    for resolution in resolutions {
        let anchor = resolution.anchor.clone();
        if let Some(anchor) = anchor {
            let deviation = deviations
                .iter()
                .filter(|record| resolution.deviation_ids.contains(&record.id))
                .cloned()
                .collect();
            let audit = audits
                .iter()
                .find(|record| record.id == resolution.audit_id)
                .cloned();
            pending.entry(anchor.exchange_id).or_default().push((
                resolution.resolved_at_ms,
                ExchangePlanEvent {
                    id: resolution.id.clone(),
                    node_count: anchor.node_count,
                    content: PlanEventContent::Resolution {
                        resolution,
                        deviation,
                        audit,
                    },
                },
            ));
        }
    }
    for exchange in exchanges {
        if let Some(mut events) = pending.remove(&exchange.id) {
            events.sort_by_key(|(time, _)| *time);
            crate::plan::event::insert_events(exchange, pair_questions(events));
        }
        // The timeline snapshot carries current selections without changing the durable plan owner.
        if let Some(elicitation) = plans.iter()
            .filter_map(|plan| plan.elicitation.as_ref())
            .find(|elicitation| exchange.node_list.iter().any(|node| matches!(node,
                ExchangeNode::PlanEvent { event } if matches!(&event.content,
                    PlanEventContent::Lifecycle { lifecycle, .. }
                    if lifecycle.kind == PlanLifecycleKind::QuestionAsked
                        && lifecycle.question.as_ref().is_some_and(|question| question.id == elicitation.question_set.id)))))
        {
            exchange.set_elicitation(Some(elicitation.clone()));
        }
    }
}

/// Retain question position and attach recorded answers without consuming the answer event.
fn pair_questions(events: Vec<(i64, ExchangePlanEvent)>) -> Vec<ExchangePlanEvent> {
    let mut paired: Vec<ExchangePlanEvent> = Vec::new();
    for (_, event) in events {
        if let PlanEventContent::Lifecycle { lifecycle, .. } = &event.content
            && matches!(
                lifecycle.kind,
                PlanLifecycleKind::QuestionAnswered | PlanLifecycleKind::QuestionWithdrawn
            )
                && let Some(question) = &lifecycle.question
                    && let Some(prior) = paired.iter_mut().rev().find(|prior| matches!(&prior.content,
                        PlanEventContent::Lifecycle { lifecycle: asked, .. }
                        if asked.kind == PlanLifecycleKind::QuestionAsked && asked.question.as_ref().is_some_and(|asked|
                            asked.questions.iter().map(|question| &question.id).eq(question.questions.iter().map(|question| &question.id)))))
                        && let PlanEventContent::Lifecycle { lifecycle: asked, .. } = &mut prior.content {
                            asked.answer = lifecycle.answer.clone();
                        }
        paired.push(event);
    }
    paired
}
