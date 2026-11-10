//! Track which standing sections one native conversation already sees.
//!
//! Each provider binding owns one record. A turn delivers only sections whose value differs from
//! what the record says the conversation last accepted, and the record advances only when the
//! provider accepts the submission that carried them. Any event that may remove earlier context
//! from the model's view, such as compaction, rewind, or retraction, clears the record so the next
//! turn delivers every section again.

use forge_provider_api::forge::{Permissions, Task, TurnContext};

/// Delivery state for one native binding's standing sections.
#[derive(Debug)]
pub(super) struct ContextDelivery {
    /// Random identity that lets the record ignore context another record selected.
    record: String,
    /// Permissions the conversation last accepted.
    permissions: Option<Permissions>,
    /// Task the conversation last accepted.
    task: Option<Task>,
}

/// Current standing sections for one turn, before delivery filtering.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct StandingSections {
    pub(crate) permissions: Option<Permissions>,
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
        if let Some(task) = &context.task {
            self.task = Some(task.clone());
        }
    }

    /// Forget every delivered section after an event that may have removed it from history.
    pub(super) fn clear(&mut self) {
        self.permissions = None;
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
