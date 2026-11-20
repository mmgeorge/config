use anyhow::{Context, Result, ensure};
use forge_buffer::block::{BufferBlock, TextPosition, TextRange};
use forge_buffer::text::BufferText;

use crate::exchange::{Exchange, ExchangeNode};
use crate::turn::{MessageDelivery, TurnItem};

use super::question::QuestionHistory;

pub(super) struct ExchangeLayout<'exchange> {
    pub questions: QuestionHistory<'exchange>,
    pub opening: Vec<&'exchange ExchangeNode>,
    pub conclusion: Vec<&'exchange ExchangeNode>,
    pub results: Vec<&'exchange ExchangeNode>,
    pub activity: Vec<&'exchange ExchangeNode>,
    /// User steering that a settled exchange draws below its folded heading.
    pub steering: Vec<&'exchange ExchangeNode>,
    pub continuation: Vec<&'exchange ExchangeNode>,
}

impl<'exchange> ExchangeLayout<'exchange> {
    /// Places review admission first, generated results before the outer response, and an
    /// execution's completion or failure after it. An execution transition recorded after the
    /// response's turn ended, such as a revision the user accepted between two turns, is not a
    /// result of that turn and keeps its place after the response.
    /// Question clarifications retain their branch ownership. Missing turn or message references fail.
    pub fn new(exchange: &'exchange Exchange) -> Result<Self> {
        let questions = QuestionHistory::new(exchange);
        let mut opening = Vec::new();
        let mut results = Vec::new();
        let mut conclusion = Vec::new();
        // Steering awaiting consumption and consumption anchors have no transcript position yet,
        // so they cannot mark where a response begins.
        let mut activity = exchange
            .node_list
            .iter()
            .filter(|node| !questions.nested.contains(node.id()) && exchange.displays_node(node))
            .collect::<Vec<_>>();
        // Results move ahead of the response of the turn that produced them, including
        // a plan submission recorded after that response. Once a later turn starts in
        // the same exchange (a continued plan revision, or a restart after interruption),
        // its results stay in source order after the earlier response instead.
        let mut first_response_turn = None;
        let mut later_turn_start = activity.len();
        for (position, node) in activity.iter().enumerate() {
            let ExchangeNode::TurnContent { turn_id, .. } = node else {
                continue;
            };
            match first_response_turn {
                None if is_outer_message(exchange, node)? => {
                    first_response_turn = Some(turn_id.as_str());
                }
                Some(response_turn) if response_turn != turn_id.as_str() => {
                    later_turn_start = position;
                    break;
                }
                _ => {}
            }
        }
        let response_completed_at_ms = first_response_turn.and_then(|response_turn| {
            exchange
                .turn
                .iter()
                .find(|turn| turn.id() == response_turn)
                .and_then(crate::turn::Turn::completed_at_ms)
        });
        let recorded_after_response = |event: &crate::plan::ExchangePlanEvent| {
            event
                .occurred_at_ms
                .zip(response_completed_at_ms)
                .is_some_and(|(occurred_at_ms, completed_at_ms)| occurred_at_ms > completed_at_ms)
        };
        let mut position = 0;
        activity.retain(|node| {
            let before_response = position < later_turn_start;
            position += 1;
            match node {
                ExchangeNode::PlanEvent { event } => match &event.content {
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if event.node_count == 0
                            && matches!(
                                lifecycle.kind,
                                crate::plan::PlanLifecycleKind::Accepted
                                    | crate::plan::PlanLifecycleKind::ChangesRequested
                            ) =>
                    {
                        opening.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if before_response
                            && matches!(
                                lifecycle.kind,
                                crate::plan::PlanLifecycleKind::Created
                                    | crate::plan::PlanLifecycleKind::RevisionCreated
                            ) =>
                    {
                        results.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Execution {
                        event:
                            crate::plan::PlanExecutionLifecycleEvent::Completed { .. }
                            | crate::plan::PlanExecutionLifecycleEvent::Failed { .. },
                    } => {
                        conclusion.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Execution { .. }
                    | crate::plan::PlanEventContent::Resolution { .. }
                        if before_response && !recorded_after_response(event) =>
                    {
                        results.push(*node);
                        false
                    }
                    _ => true,
                },
                ExchangeNode::ArtifactChange { .. }
                | ExchangeNode::ImplementationReport { .. }
                | ExchangeNode::VerificationResult { .. }
                    if before_response =>
                {
                    results.push(*node);
                    false
                }
                _ => true,
            }
        });
        let mut response_position = activity.len();
        let mut response_turn = None;
        for (position, node) in activity.iter().enumerate() {
            if is_outer_message(exchange, node)? {
                response_position = position;
                if let ExchangeNode::TurnContent { turn_id, .. } = node {
                    response_turn = Some(turn_id.as_str());
                }
                break;
            }
        }
        let mut continuation = activity.split_off(response_position);
        continuation.retain(|node| {
            let owned_activity = match node {
                ExchangeNode::AgentReference { .. } | ExchangeNode::TimelineEvent { .. } => true,
                ExchangeNode::SkillRead { turn_id, .. }
                | ExchangeNode::InstructionRead { turn_id, .. }
                | ExchangeNode::TurnContent {
                    turn_id,
                    item: TurnItem::Tool { .. },
                    ..
                } => Some(turn_id.as_str()) == response_turn,
                _ => false,
            };
            if owned_activity {
                activity.push(*node);
                false
            } else {
                true
            }
        });
        // A settled exchange folds its activity under its heading, and a stop or an abandoned
        // draft can leave no response to end that activity, so a steer drawn inside the fold
        // would vanish from the transcript. The user's own message is never folded away: once the
        // exchange settles, steering that preceded the response is drawn below the heading.
        let steering = if exchange.is_finished() {
            let (steering, remaining) = activity.into_iter().partition(|node| {
                matches!(node, ExchangeNode::ExchangeInput { prompt } if matches!(
                    &prompt.intent,
                    crate::exchange::InputIntent::User {
                        content: crate::input::UserInputContent::UserText { .. }
                    }
                ))
            });
            activity = remaining;
            steering
        } else {
            Vec::new()
        };
        Ok(Self {
            questions,
            opening,
            results,
            conclusion,
            activity,
            steering,
            continuation,
        })
    }
}

/// Keeps unclassified streaming text visible without granting commentary ownership.
pub(super) fn is_outer_message(exchange: &Exchange, node: &ExchangeNode) -> Result<bool> {
    let ExchangeNode::TurnContent {
        turn_id,
        item: TurnItem::Message { id },
        ..
    } = node
    else {
        return Ok(false);
    };
    let turn = exchange
        .turn
        .iter()
        .find(|turn| turn.id() == turn_id)
        .context("timeline content references a missing provider turn")?;
    let message = turn
        .messages()
        .iter()
        .find(|message| message.id() == id)
        .context("timeline content references a missing message")?;
    Ok(matches!(
        message.delivery(),
        MessageDelivery::Final | MessageDelivery::Unknown
    ) && !response_continues_with_checks(exchange, node))
}

/// Keep a response followed by internal check evidence inside the continuing activity.
pub(super) fn response_continues_with_checks(exchange: &Exchange, node: &ExchangeNode) -> bool {
    exchange
        .node_list
        .iter()
        .skip_while(|candidate| candidate.id() != node.id())
        .skip(1)
        .find_map(|candidate| match candidate {
            ExchangeNode::ExchangeInput { prompt } => Some(matches!(
                &prompt.intent,
                crate::exchange::InputIntent::User {
                    content: crate::input::UserInputContent::CheckResult { .. }
                }
            )),
            _ => None,
        })
        .unwrap_or(false)
}

/// Materializes Markdown padding once so native wrapping shares the parser's content origin.
pub(super) fn materialize(block: &mut BufferBlock) -> Result<()> {
    if !block.metadata.markdown {
        return Ok(());
    }
    let Some(layout) = block.metadata.layout.as_mut() else {
        return Ok(());
    };
    let padding = layout.indent - 2;
    if padding == layout.source_indent {
        return Ok(());
    }
    ensure!(
        layout.source_indent == 0,
        "content layout was materialized before resolution completed"
    );
    let prefix = " ".repeat(padding);
    let count = block.text.row_count();
    block.text = BufferText::from_rows(
        block
            .text
            .wire_rows()
            .into_iter()
            .map(|line| format!("{prefix}{line}"))
            .collect::<Vec<_>>(),
    )?;
    layout.source_indent = padding;
    let position = |position: &mut TextPosition| {
        if position.row < count {
            position.column += padding;
        }
    };
    let range = |range: &mut TextRange| {
        position(&mut range.start);
        position(&mut range.end);
    };
    for target in &mut block.metadata.target {
        range(&mut target.range);
    }
    for decoration in block
        .metadata
        .decoration
        .iter_mut()
        .chain(&mut block.metadata.visible_decoration)
        .chain(&mut block.metadata.source_highlight)
    {
        range(&mut decoration.range);
    }
    for conceal in &mut block.metadata.conceal {
        range(&mut conceal.range);
    }
    for overlay in &mut block.metadata.source_overlay {
        range(&mut overlay.range);
    }
    for gutter in &mut block.metadata.gutter {
        position(&mut gutter.position);
    }
    block.validate()?;
    Ok(())
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use forge_provider_api::{AgentMessageRole, ItemEvent, ItemStart, SubmissionId};
    use serde_json::json;

    use crate::exchange::{Exchange, ExchangeOutcome, InputIntent};
    use crate::turn::test_support::{address, completed_event, item_event, message};

    fn running_exchange() -> anyhow::Result<Exchange> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"steered", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Write about ring buffers", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        Ok(exchange)
    }

    fn acknowledge_steer(
        exchange: &mut Exchange,
        address: &crate::provider::TurnAddress,
        text: &str,
    ) -> anyhow::Result<SubmissionId> {
        let submission = SubmissionId::try_from("steer".to_owned())?;
        exchange.append_acknowledged_input(
            &address.scope,
            Some("steer-message".into()),
            Some(submission.clone()),
            InputIntent::user_text(text.into()),
            text.into(),
            2,
        )?;
        Ok(submission)
    }

    /// Render the rows a reader sees, with every closed section folded to its heading.
    fn visible_rows(exchange: &Exchange) -> anyhow::Result<Vec<String>> {
        let projected = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(exchange),
            &forge_buffer::width::WidthProfile::default(),
            10,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        Ok(crate::buffer::sections::preview(projected.entry)?
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .map(str::to_owned)
            .collect())
    }

    /// A steer the provider took while its draft was cut off by a stop has no response after it,
    /// so the settled exchange still shows it below the folded heading, labelled unconfirmed.
    #[test]
    fn a_stopped_exchange_shows_its_unconfirmed_steer_below_the_folded_heading()
    -> anyhow::Result<()> {
        let mut exchange = running_exchange()?;
        let turn = address(&exchange, "turn");
        exchange.start_turn(turn.clone(), 0)?;
        exchange.observe_event(
            &message(
                &turn,
                "draft",
                Some(AgentMessageRole::Response),
                "A ring buffer",
            ),
            1,
        )?;
        acknowledge_steer(&mut exchange, &turn, "Also mention the word anchor once.")?;
        exchange.observe_event(
            &completed_event(&turn, crate::turn::TurnOutcome::Interrupted, 3),
            3,
        )?;
        exchange.finish(ExchangeOutcome::Cancelled, 4)?;
        let rows = visible_rows(&exchange)?;
        let steer = rows
            .iter()
            .filter(|row| {
                row.contains("Steering (not confirmed read): Also mention the word anchor once.")
            })
            .count();
        assert_eq!(steer, 1, "{rows:#?}");
        Ok(())
    }

    /// A steer consumed before the response stays visible once its exchange settles and the
    /// heading folds the activity that preceded the response.
    #[test]
    fn a_steer_consumed_before_the_response_stays_visible_after_settlement() -> anyhow::Result<()> {
        let mut exchange = running_exchange()?;
        let turn = address(&exchange, "turn");
        exchange.start_turn(turn.clone(), 0)?;
        let text = "Also end your final answer with the word anchor.";
        let submission = acknowledge_steer(&mut exchange, &turn, text)?;
        exchange.observe_event(
            &item_event(
                &turn,
                "echo",
                ItemEvent::Started(ItemStart::UserMessage(
                    forge_provider_api::UserMessageStart {
                        text: text.into(),
                        submission_id: Some(submission),
                    },
                )),
            ),
            3,
        )?;
        exchange.observe_event(
            &message(
                &turn,
                "plan",
                Some(AgentMessageRole::Commentary),
                "Spawning both.",
            ),
            4,
        )?;
        exchange.observe_event(
            &crate::turn::test_support::message_completed(&turn, "plan", "Spawning both."),
            4,
        )?;
        exchange.observe_event(
            &message(
                &turn,
                "answer",
                Some(AgentMessageRole::Response),
                "Done. anchor",
            ),
            5,
        )?;
        exchange.observe_event(
            &crate::turn::test_support::message_completed(&turn, "answer", "Done. anchor"),
            5,
        )?;
        exchange.observe_event(
            &completed_event(
                &turn,
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                6,
            ),
            6,
        )?;
        exchange.finish(ExchangeOutcome::Complete, 7)?;
        let rows = visible_rows(&exchange)?;
        let steer = rows.iter().position(|row| row.contains(text));
        let answer = rows.iter().position(|row| row.contains("Done. anchor"));
        assert_eq!(
            rows.iter().filter(|row| row.contains(text)).count(),
            1,
            "{rows:#?}"
        );
        assert!(steer < answer, "{rows:#?}");
        Ok(())
    }
}
