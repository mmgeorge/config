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
    pub set: &'exchange PlanQuestionSet,
    pub branch: Vec<QuestionBranch<'exchange>>,
    pub clarification: Vec<Clarification<'exchange>>,
    pub resolved: bool,
    pub withdrawal: Option<&'exchange str>,
}

/// Keeps a question's decisions and explanations together across picker navigation.
pub(super) struct QuestionBranch<'exchange> {
    pub question: &'exchange PlanQuestion,
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
            let source = match node {
                ExchangeNode::QuestionPresented {
                    id,
                    question,
                    answer,
                } => Some((id.as_str(), question, answer.as_deref())),
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
                    set,
                    branch: set
                        .questions
                        .iter()
                        .map(|question| QuestionBranch {
                            question,
                            clarification: Vec::new(),
                            answer: None,
                            source: crate::session::ResolutionSource::User,
                        })
                        .collect(),
                    clarification: Vec::new(),
                    resolved: answer.is_some() || withdrawal.is_some(),
                    withdrawal,
                };
                if let Some(answer) = answer.filter(|_| withdrawal.is_none()) {
                    group.legacy_answer(answer);
                }
                occurrence.push((position, group));
            }
        }
        for (position, node) in exchange.node_list.iter().enumerate() {
            let ExchangeNode::ExchangeInput { prompt } = node else {
                continue;
            };
            if prompt.intent == InputIntent::Steering {
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
        history.group = occurrence
            .into_iter()
            .map(|(_, group)| (group.id, group))
            .collect();
        history
    }
}

impl QuestionGroup<'_> {
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

fn response_text(response: &PlanQuestionResponse) -> String {
    match response {
        PlanQuestionResponse::Selected { option, feedback } => feedback.as_ref().map_or_else(
            || option.clone(),
            |feedback| format!("{option} — {feedback}"),
        ),
        PlanQuestionResponse::Other { text } => text.clone(),
        PlanQuestionResponse::Skipped => "Skipped".into(),
    }
}
