//! Track which standing sections one native conversation already sees.
//!
//! Each provider binding owns one record. A turn delivers only sections whose value differs from
//! what the record says the conversation last accepted, and the record advances only when the
//! provider accepts the submission that carried them. Any event that may remove earlier context
//! from the model's view, such as compaction, rewind, or retraction, clears the record so the next
//! turn delivers every section again.

use forge_provider_api::forge::{Aside, ChildAgent, Permissions, Task, TurnContext};

/// Delivery state for one native binding's standing sections.
#[derive(Debug)]
pub(super) struct ContextDelivery {
    /// Random identity that lets the record ignore context another record selected.
    record: String,
    /// Permissions the conversation last accepted.
    permissions: Option<Permissions>,
    /// Aside origin the conversation last accepted.
    aside: Option<Aside>,
    /// Child agent identity the conversation last accepted.
    agent: Option<ChildAgent>,
    /// Task the conversation last accepted.
    task: Option<Task>,
}

/// Current standing sections for one turn, before delivery filtering.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct StandingSections {
    pub(crate) permissions: Option<Permissions>,
    /// Present only in an aside session.
    pub(crate) aside: Option<Aside>,
    /// Present only in a child agent's conversation.
    pub(crate) agent: Option<ChildAgent>,
    pub(crate) task: Option<Task>,
}

impl ContextDelivery {
    /// Start an empty record with a fresh identity.
    pub(super) fn new() -> Self {
        let mut record = uuid::Uuid::new_v4().simple().to_string();
        record.truncate(12);
        Self {
            record,
            permissions: None,
            aside: None,
            agent: None,
            task: None,
        }
    }

    /// Select the sections whose value the conversation has not accepted yet.
    pub(super) fn pending(&self, sections: StandingSections) -> TurnContext {
        TurnContext {
            record: self.record.clone(),
            permissions: sections
                .permissions
                .filter(|permissions| self.permissions.as_ref() != Some(permissions)),
            aside: sections
                .aside
                .filter(|aside| self.aside.as_ref() != Some(aside)),
            agent: sections
                .agent
                .filter(|agent| self.agent.as_ref() != Some(agent)),
            task: sections
                .task
                .filter(|task| self.task.as_ref() != Some(task)),
        }
    }

    /// Record the sections of a submission the provider accepted.
    ///
    /// Context another record selected, such as one that existed before a rebind, is ignored.
    pub(super) fn commit(&mut self, context: &TurnContext) {
        if context.record != self.record {
            return;
        }
        if let Some(permissions) = &context.permissions {
            self.permissions = Some(permissions.clone());
        }
        if let Some(aside) = &context.aside {
            self.aside = Some(aside.clone());
        }
        if let Some(agent) = &context.agent {
            self.agent = Some(agent.clone());
        }
        if let Some(task) = &context.task {
            self.task = Some(task.clone());
        }
    }

    /// Forget every delivered section after an event that may have removed it from history.
    pub(super) fn clear(&mut self) {
        self.permissions = None;
        self.aside = None;
        self.agent = None;
        self.task = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::approval::ApprovalMode;
    use forge_provider_api::forge::{GoalTask, ReadAccess, WriteAccess};

    fn sections(objective: &str, approval: ApprovalMode) -> StandingSections {
        StandingSections {
            permissions: Some(Permissions {
                approval,
                read: ReadAccess::Full,
                write: WriteAccess::Off,
                toolsets: Vec::new(),
            }),
            aside: None,
            agent: None,
            task: Some(Task::Goal(GoalTask {
                objective: objective.into(),
            })),
        }
    }

    fn names(context: &TurnContext) -> (bool, bool) {
        (context.permissions.is_some(), context.task.is_some())
    }

    /// Deliver each section once, then only the sections whose value changed.
    #[test]
    fn accepted_sections_are_not_repeated_until_they_change() {
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(sections("ship", ApprovalMode::Ask));
        assert_eq!(first.record, delivery.record);
        assert_eq!(names(&first), (true, true));
        delivery.commit(&first);
        assert!(
            delivery
                .pending(sections("ship", ApprovalMode::Ask))
                .is_empty()
        );
        assert_eq!(
            names(&delivery.pending(sections("ship", ApprovalMode::Allow))),
            (true, false)
        );
        assert_eq!(
            names(&delivery.pending(sections("test", ApprovalMode::Ask))),
            (false, true)
        );
    }

    /// Keep unaccepted sections pending so a rejected submission's retry carries them again.
    #[test]
    fn unaccepted_sections_remain_pending() {
        let delivery = ContextDelivery::new();
        let rejected = delivery.pending(sections("ship", ApprovalMode::Ask));
        assert_eq!(
            delivery.pending(sections("ship", ApprovalMode::Ask)),
            rejected
        );
    }

    /// Deliver every section again after history may have lost it.
    #[test]
    fn clearing_redelivers_every_section() {
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(sections("ship", ApprovalMode::Ask));
        delivery.commit(&first);
        delivery.clear();
        assert_eq!(
            names(&delivery.pending(sections("ship", ApprovalMode::Ask))),
            (true, true)
        );
    }

    /// Deliver an aside's origin once, and again after history may have lost it.
    #[test]
    fn aside_origin_is_delivered_once_until_cleared() {
        let aside_sections = || StandingSections {
            aside: Some(Aside {
                origin_session_id: "origin".into(),
                origin_session_name: "main".into(),
            }),
            ..sections("ship", ApprovalMode::Ask)
        };
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(aside_sections());
        assert!(first.aside.is_some());
        delivery.commit(&first);
        assert!(delivery.pending(aside_sections()).is_empty());
        delivery.clear();
        assert!(delivery.pending(aside_sections()).aside.is_some());
    }

    /// Deliver a child's identity and profile once, and again after compaction may have lost them.
    #[test]
    fn child_agent_is_delivered_once_until_cleared() {
        let agent_sections = |profile: &str| StandingSections {
            agent: Some(ChildAgent {
                path: "/root/explore-1".into(),
                parent: "/root".into(),
                profile_instructions: profile.into(),
            }),
            ..sections("ship", ApprovalMode::Ask)
        };
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(agent_sections("Read only."));
        assert!(first.agent.is_some());
        delivery.commit(&first);
        assert!(delivery.pending(agent_sections("Read only.")).is_empty());
        assert!(
            delivery
                .pending(agent_sections("Edit freely."))
                .agent
                .is_some()
        );
        delivery.clear();
        assert!(
            delivery
                .pending(agent_sections("Read only."))
                .agent
                .is_some()
        );
    }

    /// Ignore context selected by another record.
    #[test]
    fn commit_ignores_context_from_another_record() {
        let mut delivery = ContextDelivery::new();
        let mut foreign = delivery.pending(sections("ship", ApprovalMode::Ask));
        foreign.record = "another".into();
        delivery.commit(&foreign);
        assert_eq!(
            names(&delivery.pending(sections("ship", ApprovalMode::Ask))),
            (true, true)
        );
        assert_ne!(ContextDelivery::new().record, delivery.record);
    }
}
