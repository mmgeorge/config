use crate::exchange::{Exchange, ExchangeInput, ExchangeNode, InputIntent};
use crate::plan::{
    PlanEventContent, PlanLifecycleKind, PlanQuestion, PlanQuestionResponse, PlanQuestionSet,
};
use std::collections::{HashMap, HashSet};

/// Groups question feedback without changing exchange execution or checkpoint ownership.
pub(super) struct QuestionHistory<'exchange> {
    pub group: HashMap<&'exchange str, QuestionGroup<'exchange>>,
    pub nested: HashSet<&'exchange str>,
}

/// Owns the presentation branches of one question-set occurrence.
pub(super) struct QuestionGroup<'exchange> {
    pub id: &'exchange str,
    pub set: std::borrow::Cow<'exchange, PlanQuestionSet>,
    pub branch: Vec<QuestionBranch<'exchange>>,
    pub clarification: Vec<Clarification<'exchange>>,
    pub resolved: bool,
    pub withdrawal: Option<&'exchange str>,
}

/// Keeps a question's decisions and explanations together across picker navigation.
pub(super) struct QuestionBranch<'exchange> {
    pub question_set_id: String,
    pub question: std::borrow::Cow<'exchange, PlanQuestion>,
    pub clarification: Vec<Clarification<'exchange>>,
    pub answer: Option<String>,
    pub source: crate::session::ResolutionSource,
}

/// Retains a clarification and provider content until the next interaction boundary.
pub(super) struct Clarification<'exchange> {
    pub prompt: &'exchange ExchangeInput,
    pub content: Vec<&'exchange ExchangeNode>,
}

impl<'exchange> QuestionHistory<'exchange> {
    pub fn new(exchange: &'exchange Exchange) -> Self {
        let mut history = Self {
            group: HashMap::new(),
            nested: HashSet::new(),
        };
        let mut occurrence = Vec::new();
        for (position, node) in exchange.node_list.iter().enumerate() {
            if let ExchangeNode::ExchangeInput { prompt } = node
                && let InputIntent::User { content } = &prompt.intent
            {
                if let Some(group) = QuestionGroup::from_content(&prompt.id, content) {
                    occurrence.push((position, group));
                }
                continue;
            }
            let source = match node {
                ExchangeNode::QuestionPresented {
                    id,
                    question,
                    answer,
                } => Some((id.as_str(), question, answer.as_deref())),
                ExchangeNode::QuestionWithdrawn { id, question, .. }
                    if !exchange.node_list[..position].iter().any(|node| match node {
                        ExchangeNode::QuestionPresented { question: prior, .. } => prior.id == question.id,
                        ExchangeNode::PlanEvent { event } => matches!(&event.content,
                            PlanEventContent::Lifecycle { lifecycle, .. }
                            if lifecycle.kind == PlanLifecycleKind::QuestionAsked
                                && lifecycle.question.as_ref().is_some_and(|prior| prior.id == question.id)),
                        _ => false,
                    }) => Some((id.as_str(), question, None)),
                ExchangeNode::PlanEvent { event } => match &event.content {
                    PlanEventContent::Lifecycle { lifecycle, .. }
                        if lifecycle.kind == PlanLifecycleKind::QuestionAsked =>
                    {
                        lifecycle.question.as_ref().map(|question| {
                            (event.id.as_str(), question, lifecycle.answer.as_deref())
                        })
                    }
                    _ => None,
                },
                _ => None,
            };
            if let Some((id, set, answer)) = source {
                let withdrawal = exchange.node_list.iter().find_map(|node| match node {
                    ExchangeNode::QuestionWithdrawn {
                        question, reason, ..
                    } if question.id == set.id => Some(reason.as_str()),
                    ExchangeNode::PlanEvent { event } => match &event.content {
                        PlanEventContent::Lifecycle { lifecycle, .. }
                            if lifecycle.kind == PlanLifecycleKind::QuestionWithdrawn
                                && lifecycle
                                    .question
                                    .as_ref()
                                    .is_some_and(|question| question.id == set.id) =>
                        {
                            Some(lifecycle.answer.as_deref().unwrap_or("No decision remains"))
                        }
                        _ => None,
                    },
                    _ => None,
                });
                let mut group = QuestionGroup {
                    id,
                    set: std::borrow::Cow::Borrowed(set),
                    branch: set
                        .questions
                        .iter()
                        .map(|question| QuestionBranch {
                            question_set_id: set.id.clone(),
                            question: std::borrow::Cow::Borrowed(question),
                            clarification: Vec::new(),
                            answer: None,
                            source: crate::session::ResolutionSource::User,
                        })
                        .collect(),
                    clarification: Vec::new(),
                    resolved: answer.is_some() || withdrawal.is_some(),
                    withdrawal,
                };
                if let Some(answer) = answer.filter(|_| {
                    withdrawal.is_none() || matches!(node, ExchangeNode::QuestionPresented { .. })
                }) {
                    group.legacy_answer(answer);
                }
                for node in &exchange.node_list {
                    if let ExchangeNode::QuestionWithdrawn {
                        question, answer, ..
                    } = node
                        && question.id == set.id
                    {
                        for answer in answer {
                            if let Some(branch) = group.branch.iter_mut().find(|branch| {
                                branch.question.id == answer.question_id && branch.answer.is_none()
                            }) {
                                branch.answer = Some(response_text(&answer.response));
                                branch.source = answer.source;
                            }
                        }
                    }
                }
                occurrence.push((position, group));
            }
        }
        for node in &exchange.node_list {
            if let ExchangeNode::QuestionWithdrawn { id, question, .. } = node
                && occurrence
                    .iter()
                    .any(|(_, group)| group.set.id == question.id && group.id != id)
            {
                history.nested.insert(id);
            }
        }
        for (position, node) in exchange.node_list.iter().enumerate() {
            let ExchangeNode::ExchangeInput { prompt } = node else {
                continue;
            };
            if matches!(prompt.intent, InputIntent::User { .. }) {
                continue;
            }
            let Some((_, group)) = occurrence.iter_mut().rev().find(|(start, group)| {
                *start < position
                    && prompt
                        .question
                        .as_ref()
                        .is_none_or(|target| target.set_id == group.set.id)
            }) else {
                continue;
            };
            history.nested.insert(node.id());
            if prompt.intent == InputIntent::Answer {
                group.resolved = true;
                group.legacy_answer(&prompt.text);
                if let Some(target) = &prompt.question {
                    for answer in &target.answer {
                        if let Some(branch) = group
                            .branch
                            .iter_mut()
                            .find(|branch| branch.question.id == answer.question_id)
                        {
                            branch.answer = Some(response_text(&answer.response));
                            branch.source = answer.source;
                        }
                    }
                }
                continue;
            }
            let content = exchange.node_list[position + 1..]
                .iter()
                .take_while(|node| {
                    matches!(
                        node,
                        ExchangeNode::TurnContent { .. }
                            | ExchangeNode::TimelineEvent { .. }
                            | ExchangeNode::AgentReference { .. }
                            | ExchangeNode::ArtifactChange { .. }
                            | ExchangeNode::SkillRead { .. }
                            | ExchangeNode::InstructionRead { .. }
                            | ExchangeNode::Compaction { .. }
                    )
                })
                .collect::<Vec<_>>();
            history.nested.extend(content.iter().map(|node| node.id()));
            let clarification = Clarification { prompt, content };
            let target = prompt
                .question
                .as_ref()
                .and_then(|target| target.question_id.as_deref());
            let branch = if let Some(target) = target {
                group
                    .branch
                    .iter_mut()
                    .find(|branch| branch.question.id == target)
            } else if group.branch.len() == 1 {
                group.branch.first_mut()
            } else {
                None
            };
            if let Some(branch) = branch {
                branch.clarification.push(clarification);
            } else {
                // Records without question identity remain at set scope.
                group.clarification.push(clarification);
            }
        }
        for node in &exchange.node_list {
            if let ExchangeNode::PlanEvent { event } = node
                && let PlanEventContent::Lifecycle { lifecycle, .. } = &event.content
                && matches!(
                    lifecycle.kind,
                    PlanLifecycleKind::QuestionAnswered | PlanLifecycleKind::QuestionWithdrawn
                )
                && let Some((_, group)) = occurrence.iter_mut().rev().find(|(_, group)| {
                    lifecycle
                        .question
                        .as_ref()
                        .is_some_and(|question| question.id == group.set.id)
                })
            {
                group.resolved = true;
                if lifecycle.kind == PlanLifecycleKind::QuestionAnswered
                    && let Some(answer) = &lifecycle.answer
                {
                    group.legacy_answer(answer);
                }
                history.nested.insert(node.id());
            }
        }
        if let Some(elicitation) = exchange.elicitation()
            && let Some((_, group)) = occurrence
                .iter_mut()
                .rev()
                .find(|(_, group)| group.set.id == elicitation.question_set.id)
        {
            for branch in group.branch.iter_mut().filter(|_| !group.resolved) {
                branch.source = elicitation
                    .answer
                    .iter()
                    .find(|answer| answer.question_id == branch.question.id)
                    .map_or(crate::session::ResolutionSource::User, |answer| {
                        answer.source
                    });
                branch.answer = elicitation
                    .answer
                    .iter()
                    .find(|answer| answer.question_id == branch.question.id)
                    .map(|answer| response_text(&answer.response));
            }
        }
        // Merge adjacent answer categories without moving them across visible activity.
        let mut grouped: Vec<(usize, QuestionGroup<'exchange>)> = Vec::new();
        for (position, mut group) in occurrence {
            if group.answered()
                && let Some((previous_position, previous)) = grouped.last_mut()
                && previous.answered()
                && exchange.node_list[*previous_position + 1..position]
                    .iter()
                    .all(|node| history.nested.contains(node.id()) || !exchange.displays_node(node))
            {
                history.nested.insert(group.id);
                previous.branch.append(&mut group.branch);
                previous.clarification.append(&mut group.clarification);
                *previous_position = position;
            } else {
                grouped.push((position, group));
            }
        }
        history.group = grouped
            .into_iter()
            .map(|(_, group)| (group.id, group))
            .collect();
        history
    }
}

impl<'content> QuestionGroup<'content> {
    pub(super) fn from_content(
        id: &'content str,
        content: &'content crate::input::UserInputContent,
    ) -> Option<Self> {
        let crate::input::UserInputContent::QuestionResponse { responses } = content else {
            return None;
        };
        if !responses.iter().all(|response| {
            matches!(
                response.reply.response,
                forge_provider_api::QuestionReplyResponse::Answer { .. }
            )
        }) {
            return None;
        }
        let branch = responses
            .iter()
            .map(|response| QuestionBranch {
                question_set_id: response.reply.question_id.clone(),
                question: std::borrow::Cow::Borrowed(&response.question),
                clarification: Vec::new(),
                answer: Some(response_text(&response.value)),
                source: crate::session::ResolutionSource::User,
            })
            .collect::<Vec<_>>();
        let first = branch.first()?;
        Some(Self {
            id,
            set: std::borrow::Cow::Owned(PlanQuestionSet {
                id: first.question_set_id.clone(),
                questions: branch
                    .iter()
                    .map(|branch| branch.question.as_ref().clone())
                    .collect(),
            }),
            branch,
            clarification: Vec::new(),
            resolved: true,
            withdrawal: None,
        })
    }

    /// Identify completed answer groups independently of their submission mechanism.
    fn answered(&self) -> bool {
        self.resolved
            && self.withdrawal.is_none()
            && self.branch.iter().any(|branch| branch.answer.is_some())
    }

    fn legacy_answer(&mut self, answer: &str) {
        for branch in &mut self.branch {
            let prefix = format!("{}:", branch.question.header);
            if let Some(answer) = answer
                .lines()
                .map(|line| line.trim().trim_start_matches("- "))
                .find_map(|line| line.strip_prefix(&prefix))
            {
                branch.answer = Some(answer.trim().to_owned());
            }
        }
        if self.branch.len() == 1 && self.branch[0].answer.is_none() {
            self.branch[0].answer = Some(
                answer
                    .trim()
                    .strip_prefix("Planning feedback:")
                    .unwrap_or(answer)
                    .trim()
                    .trim_start_matches("- ")
                    .to_owned(),
            );
        }
    }
}

pub(super) fn response_text(response: &PlanQuestionResponse) -> String {
    match response {
        PlanQuestionResponse::Selected { option, feedback } => feedback.as_ref().map_or_else(
            || option.clone(),
            |feedback| format!("{option} — {feedback}"),
        ),
        PlanQuestionResponse::Other { text } => text.clone(),
        PlanQuestionResponse::Skipped => "Skipped".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Merge adjacent answers while preserving pending boundaries and original question actions.
    #[test]
    fn answered_groups_preserve_order_boundaries_and_question_identity() {
        for pending_middle in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
                "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Choose preferences", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            for (index, identity) in ["layout", "color", "scope"].into_iter().enumerate() {
                exchange.node_list.push(ExchangeNode::QuestionPresented {
                    id: identity.into(),
                    question: serde_json::from_value(serde_json::json!({
                        "id":identity, "questions":[{
                            "id":identity, "header":identity, "question":identity,
                            "options":[], "allow_freeform":true
                        }]
                    }))
                    .unwrap(),
                    answer: (!(pending_middle && index == 1)).then(|| "Selected".into()),
                });
            }
            let history = QuestionHistory::new(&exchange);
            if pending_middle {
                assert_eq!(history.group.len(), 3);
                assert!(history.nested.is_empty());
            } else {
                assert_eq!(history.group.len(), 1);
                let group = &history.group["layout"];
                assert_eq!(
                    group
                        .branch
                        .iter()
                        .map(|branch| branch.question_set_id.as_str())
                        .collect::<Vec<_>>(),
                    ["layout", "color", "scope"]
                );
                assert!(history.nested.contains("color"));
                assert!(history.nested.contains("scope"));
            }
        }
    }
    #[test]
    fn visible_user_input_ends_an_answer_group() {
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"preferences", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        for identity in ["layout", "color"] {
            if identity == "color" {
                exchange.node_list.push(ExchangeNode::ExchangeInput {
                    prompt: ExchangeInput {
                        id: "intervening-input".into(),
                        question: None,
                        intent: InputIntent::user_text("Keep researching".into()),
                        text: "Keep researching".into(),
                        created_at_ms: 1,
                        submission_id: None,
                        consumption: crate::exchange::InputConsumption::Consumed,
                    },
                });
            }
            exchange.node_list.push(ExchangeNode::QuestionPresented {
                id: identity.into(),
                question: PlanQuestionSet {
                    id: identity.into(),
                    questions: vec![PlanQuestion {
                        id: identity.into(),
                        header: identity.into(),
                        question: identity.into(),
                        options: Vec::new(),
                        allow_freeform: true,
                    }],
                },
                answer: Some("Selected".into()),
            });
        }
        assert_eq!(QuestionHistory::new(&exchange).group.len(), 2);
    }
}
