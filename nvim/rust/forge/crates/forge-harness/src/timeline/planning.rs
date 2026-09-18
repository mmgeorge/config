use std::collections::HashMap;

use crate::exchange::{Exchange, ExchangeNode, InputIntent};
use crate::plan::{
    ExchangeAnchor, ExchangePlanEvent, PlanAudit, PlanDeviation, PlanEventContent,
    PlanExecutionLifecycleEvent, PlanExecutionRecord, PlanLifecycleKind, PlanLifecycleRecord,
    PlanRecord, PlanResolutionRecord,
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
        let anchor = record.anchor.clone().or_else(|| {
            legacy_anchor(
                exchanges,
                &record.plan_id,
                None,
                record.created_at_ms,
                matches!(
                    record.kind,
                    PlanLifecycleKind::Accepted | PlanLifecycleKind::ChangesRequested
                ),
            )
        });
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
        for mut record in execution.lifecycle.clone() {
            if let PlanExecutionLifecycleEvent::TaskCompleted {
                task_path,
                elapsed_ms,
                ..
            } = &mut record.event
            {
                *elapsed_ms =
                    execution.task_duration_ms(task_path, exchanges.iter(), record.occurred_at_ms);
            }
            let anchor = record
                .anchor
                .or_else(|| {
                    record
                        .after_exchange_id
                        .as_ref()
                        .and_then(|id| exchanges.iter().find(|exchange| &exchange.id == id))
                        .map(|exchange| legacy_position(exchange, record.occurred_at_ms))
                })
                .or_else(|| {
                    legacy_anchor(
                        exchanges,
                        &execution.plan_id,
                        Some(&execution.id),
                        record.occurred_at_ms,
                        true,
                    )
                });
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
        let anchor = resolution.anchor.clone().or_else(|| {
            legacy_anchor(
                exchanges,
                &resolution.plan_id,
                Some(&resolution.execution_id),
                resolution.resolved_at_ms,
                false,
            )
        });
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
    }
}

/// Retain question identity while placing its resolved details beside the response.
fn pair_questions(events: Vec<(i64, ExchangePlanEvent)>) -> Vec<ExchangePlanEvent> {
    let mut paired: Vec<ExchangePlanEvent> = Vec::new();
    for (_, event) in events {
        if let PlanEventContent::Lifecycle { lifecycle, .. } = &event.content {
            if matches!(
                lifecycle.kind,
                PlanLifecycleKind::QuestionAnswered | PlanLifecycleKind::QuestionWithdrawn
            ) {
                if let Some(question) = &lifecycle.question {
                    if let Some(prior) = paired.iter_mut().rev().find(|prior| matches!(&prior.content,
                        PlanEventContent::Lifecycle { lifecycle: asked, .. }
                        if asked.kind == PlanLifecycleKind::QuestionAsked && asked.question.as_ref().is_some_and(|asked|
                            asked.questions.iter().map(|question| &question.id).eq(question.questions.iter().map(|question| &question.id))))) {
                        prior.node_count = event.node_count;
                        prior.content = event.content;
                        continue;
                    }
                }
            }
        }
        paired.push(event);
    }
    paired
}

/// Recover ownership only for records written before explicit exchange anchors existed.
fn legacy_anchor(
    exchanges: &[Exchange],
    plan_id: &str,
    execution_id: Option<&str>,
    time: i64,
    admission: bool,
) -> Option<ExchangeAnchor> {
    let matching = || {
        exchanges.iter().filter(|exchange| {
            exchange.plan_id.as_deref() == Some(plan_id)
                && execution_id.is_none_or(|id| exchange.execution_id.as_deref() == Some(id))
        })
    };
    let exchange = if admission {
        matching()
            .find(|exchange| exchange.created_at_ms >= time)
            .or_else(|| {
                matching()
                    .filter(|exchange| exchange.created_at_ms <= time)
                    .last()
            })
    } else {
        matching()
            .filter(|exchange| exchange.created_at_ms <= time)
            .last()
            .or_else(|| matching().next())
    }?;
    Some(legacy_position(exchange, time))
}

/// Place old questions before their answer when historical provider items lack timestamps.
fn legacy_position(exchange: &Exchange, time: i64) -> ExchangeAnchor {
    let node_count = if time <= exchange.created_at_ms {
        0
    } else {
        exchange
            .node_list
            .iter()
            .position(|node| match node {
                ExchangeNode::ExchangeInput { prompt } => {
                    prompt.created_at_ms >= time && prompt.intent == InputIntent::Clarification
                }
                ExchangeNode::ArtifactChange { change } => change.created_at_ms > time,
                ExchangeNode::PlanCommentResolution { resolution } => {
                    resolution.created_at_ms > time
                }
                _ => false,
            })
            .unwrap_or(exchange.node_list.len())
    };
    ExchangeAnchor {
        exchange_id: exchange.id.clone(),
        node_count,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn exchange() -> Exchange {
        serde_json::from_value(json!({
            "id":"exchange", "agent_id":"primary", "session_id":"session", "ordinal":1,
            "prompt":"Plan the migration", "kind":"plan_draft", "plan_id":"plan",
            "execution_id":"execution", "state":"running", "attributed_matches_checkpoint":false,
            "created_at_ms":1, "node_list":[], "duration_ms":0, "comment":[]
        }))
        .unwrap()
    }

    fn plan() -> PlanRecord {
        serde_json::from_value(json!({
            "id":"plan", "session_id":"session", "request":"Plan the migration",
            "title":"Migration", "state":"generating", "working_path":"",
            "model_revision":0, "user_revision":0, "created_at_ms":1, "updated_at_ms":1
        }))
        .unwrap()
    }

    fn lifecycle(kind: PlanLifecycleKind, anchor: Option<ExchangeAnchor>) -> PlanLifecycleRecord {
        PlanLifecycleRecord {
            title: "Migration".into(),
            anchor,
            id: format!("{kind:?}"),
            session_id: "session".into(),
            plan_id: "plan".into(),
            kind,
            model_revision: 0,
            user_revision: 0,
            overall_comment: None,
            annotation: vec![],
            question: None,
            answer: None,
            created_at_ms: 10,
        }
    }

    fn ids(exchange: &Exchange) -> Vec<String> {
        exchange
            .node_list
            .iter()
            .map(|node| node.id().to_owned())
            .collect()
    }

    #[test]
    fn all_lifecycle_events_remain_inside_the_exchange_across_live_updates_and_reload() {
        let mut source = exchange();
        source
            .append_input(InputIntent::Steering, "Before question".into(), 2)
            .unwrap();
        let before = source.node_list[0].id().to_owned();
        let anchor = ExchangeAnchor::capture(&source);
        let kinds = [
            PlanLifecycleKind::QuestionAsked,
            PlanLifecycleKind::QuestionAnswered,
            PlanLifecycleKind::QuestionWithdrawn,
            PlanLifecycleKind::Created,
            PlanLifecycleKind::ChangesRequested,
            PlanLifecycleKind::RevisionCreated,
            PlanLifecycleKind::Accepted,
            PlanLifecycleKind::Cancelled,
        ];
        let records: Vec<_> = kinds
            .into_iter()
            .map(|kind| lifecycle(kind, Some(anchor.clone())))
            .collect();
        source
            .append_input(InputIntent::Clarification, "Rust learning demo".into(), 10)
            .unwrap();
        let answer = source.node_list[1].id().to_owned();
        let mut projected = vec![source.clone()];
        attach(
            &mut projected,
            &[plan()],
            records.clone(),
            vec![],
            vec![],
            &[],
            &[],
        );
        let mut expected = vec![before];
        expected.extend(kinds.into_iter().map(|kind| format!("{kind:?}")));
        expected.push(answer);
        assert_eq!(ids(&projected[0]), expected);

        source
            .append_input(InputIntent::Steering, "Continue after answer".into(), 11)
            .unwrap();
        expected.push(source.node_list[2].id().to_owned());
        crate::plan::event::replace_exchange(&mut projected[0], &source);
        assert_eq!(ids(&projected[0]), expected);
        let source: Exchange =
            serde_json::from_value(serde_json::to_value(source).unwrap()).unwrap();
        let records = serde_json::from_value(serde_json::to_value(records).unwrap()).unwrap();
        let mut restored = vec![source];
        attach(&mut restored, &[plan()], records, vec![], vec![], &[], &[]);
        assert_eq!(ids(&restored[0]), expected);
    }

    #[test]
    fn legacy_question_precedes_answer_without_repeating_at_the_timeline_tail() {
        let mut source = exchange();
        source
            .append_input(InputIntent::Steering, "Before question".into(), 2)
            .unwrap();
        source
            .append_input(InputIntent::Clarification, "Rust learning demo".into(), 20)
            .unwrap();
        source
            .append_input(InputIntent::Steering, "Later message".into(), 30)
            .unwrap();
        let mut projected = vec![source.clone()];
        attach(
            &mut projected,
            &[plan()],
            vec![lifecycle(PlanLifecycleKind::QuestionAsked, None)],
            vec![],
            vec![],
            &[],
            &[],
        );
        assert_eq!(
            ids(&projected[0]),
            vec![
                source.node_list[0].id(),
                "QuestionAsked",
                source.node_list[1].id(),
                source.node_list[2].id()
            ]
        );
    }

    #[test]
    fn answered_and_withdrawn_questions_render_once_at_the_response_position() {
        for outcome in [
            PlanLifecycleKind::QuestionAnswered,
            PlanLifecycleKind::QuestionWithdrawn,
        ] {
            let mut source = exchange();
            let question = crate::plan::PlanQuestionSet {
                id: "question-set".into(),
                questions: vec![crate::plan::PlanQuestion {
                    id: "goal".into(),
                    header: "Goal".into(),
                    question: "Which migration goal?".into(),
                    options: vec![crate::plan::PlanQuestionOption {
                        label: "Rust learning".into(),
                        description: "Learn Rust ownership".into(),
                    }],
                    allow_freeform: true,
                }],
            };
            let mut asked = lifecycle(
                PlanLifecycleKind::QuestionAsked,
                Some(ExchangeAnchor::capture(&source)),
            );
            asked.question = Some(question.clone());
            source.append_input(InputIntent::Steering, "Clarify the options first".into(), 10).unwrap();
            let answer = "Planning feedback:\n- Goal: Rust learning";
            source
                .append_input(InputIntent::Clarification, answer.into(), 11)
                .unwrap();
            let mut resolved = lifecycle(outcome, Some(ExchangeAnchor::capture(&source)));
            resolved.created_at_ms = 11;
            resolved.question = Some(question);
            resolved.answer = Some(answer.into());
            source
                .append_input(
                    InputIntent::Steering,
                    "Later commentary boundary".into(),
                    12,
                )
                .unwrap();
            let mut projected = vec![source];
            attach(
                &mut projected,
                &[plan()],
                vec![asked, resolved],
                vec![],
                vec![],
                &[],
                &[],
            );
            let entry = super::super::TimelineEntry::Exchange {
                id: "exchange".into(),
                created_at_ms: 1,
                exchange: projected.remove(0),
                agent_by_id: HashMap::new(),
            };
            let rendered =
                crate::buffer::projection::project(&entry, &Default::default(), false).unwrap();
            let text = rendered
                .entry
                .block
                .iter()
                .flat_map(|block| {
                    (0..block.text.row_count()).map(|row| block.text.row(row).unwrap())
                })
                .collect::<Vec<_>>()
                .join("\n");
            assert_eq!(text.matches("Which migration goal?").count(), 1);
            assert!(
                text.find("Which migration goal?").unwrap()
                    < text.find("Later commentary boundary").unwrap()
            );
            assert!(!text.contains("QuestionAsked"));
            assert!(
                rendered
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| &block.metadata.fold)
                    .any(|fold| fold.id.0 == "QuestionAsked" && fold.closed)
            );
            if outcome == PlanLifecycleKind::QuestionAnswered {
                assert!(rendered.prompt.iter().any(|id| id.0 == "QuestionAsked"));
                assert_eq!(text.matches("Clarification:").count(), 1);
                assert!(text.contains("Clarification: Goal: Rust learning"));
                assert!(text.find("Clarify the options first").unwrap() < text.find("Clarification: Goal").unwrap());
            } else {
                assert!(text.contains("Clarification withdrawn"));
            }
        }
    }

    #[test]
    fn task_transitions_and_resolution_keep_their_position_before_later_input() {
        let mut source = exchange();
        source
            .append_input(InputIntent::Steering, "Task one".into(), 2)
            .unwrap();
        let anchor = ExchangeAnchor::capture(&source);
        source
            .append_input(InputIntent::Steering, "Task two".into(), 10)
            .unwrap();
        let execution: PlanExecutionRecord = serde_json::from_value(json!({
            "id":"execution", "session_id":"session", "plan_id":"plan", "goal_id":"goal",
            "state":"active", "created_at_ms":1, "lifecycle":[
                {"anchor":anchor, "sequence":1,"after_exchange_id":"exchange","occurred_at_ms":10,
                 "kind":"task_completed","task_path":"/tasks/0","ordinal":1,"total":2,"title":"First","elapsed_ms":0},
                {"anchor":anchor, "sequence":2,"after_exchange_id":"exchange","occurred_at_ms":10,
                 "kind":"task_started","task_path":"/tasks/1","ordinal":2,"total":2,"title":"Second"},
                {"anchor":anchor, "sequence":3,"after_exchange_id":"exchange","occurred_at_ms":10,
                 "kind":"deviation_recorded","deviation_id":"deviation","summary":"Scope decision"}
            ]
        })).unwrap();
        let resolution: PlanResolutionRecord = serde_json::from_value(json!({
            "anchor":anchor,"id":"resolved","session_id":"session","plan_id":"plan",
            "execution_id":"execution","accepted_revision":1,"kind":"blocked",
            "task_summary":{"completed":1,"blocked":1,"total":2},
            "test_summary":{"passed":1,"failed":0,"skipped":0,"not_run":0},
            "deviation_ids":[],"audit_id":"audit","resolved_at_ms":10
        }))
        .unwrap();
        let mut projected = vec![source.clone()];
        attach(
            &mut projected,
            &[plan()],
            vec![],
            vec![execution],
            vec![resolution],
            &[],
            &[],
        );
        assert_eq!(
            ids(&projected[0]),
            vec![
                source.node_list[0].id(),
                "execution:lifecycle:1",
                "execution:lifecycle:2",
                "execution:lifecycle:3",
                "resolved",
                source.node_list[1].id()
            ]
        );
    }
}
