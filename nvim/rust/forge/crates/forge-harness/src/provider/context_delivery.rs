//! Track which Harness context sections one native conversation already sees.
//!
//! Each provider binding owns one record. A turn delivers only sections whose text differs from
//! what the record says the conversation last accepted, and the record advances only when the
//! provider accepts the submission that carried them. Any event that may remove earlier context
//! from the model's view, such as compaction, rewind, or retraction, clears the record so the next
//! turn delivers every section again.

use forge_provider_api::turn_context::{ContextPart, ContextSection, TurnContext};
use std::collections::BTreeMap;

/// Delivery state for one native binding's tracked context sections.
#[derive(Debug)]
pub(super) struct ContextDelivery {
    /// Random identity that lets the record ignore context another record selected.
    record: String,
    /// Section content, attributes and text, the conversation last accepted, by section.
    delivered: BTreeMap<ContextSection, ContextPart>,
}

impl ContextDelivery {
    /// Start an empty record with a fresh identity.
    pub(super) fn new() -> Self {
        let mut record = uuid::Uuid::new_v4().simple().to_string();
        record.truncate(12);
        Self {
            record,
            delivered: BTreeMap::new(),
        }
    }

    /// Select the sections whose text the conversation has not accepted yet.
    pub(super) fn pending(&self, sections: Vec<ContextPart>) -> TurnContext {
        TurnContext {
            record: self.record.clone(),
            parts: sections
                .into_iter()
                .filter(|part| self.delivered.get(&part.section) != Some(part))
                .collect(),
        }
    }

    /// Record the sections of a submission the provider accepted.
    ///
    /// Context another record selected, such as one that existed before a rebind, is ignored.
    pub(super) fn commit(&mut self, context: &TurnContext) {
        if context.record != self.record {
            return;
        }
        for part in &context.parts {
            self.delivered.insert(part.section, part.clone());
        }
    }

    /// Forget every delivered section after an event that may have removed it from history.
    pub(super) fn clear(&mut self) {
        self.delivered.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sections(task: &str, permissions: &str) -> Vec<ContextPart> {
        vec![
            ContextPart::new(ContextSection::Permissions, permissions),
            ContextPart::new(ContextSection::Task, "Task instructions.")
                .with_attribute("kind", task),
        ]
    }

    fn names(context: &TurnContext) -> Vec<ContextSection> {
        context.parts.iter().map(|part| part.section).collect()
    }

    /// Deliver each section once, then only the sections whose text changed.
    #[test]
    fn accepted_sections_are_not_repeated_until_they_change() {
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(sections("chat", "ask"));
        assert_eq!(first.record, delivery.record);
        assert_eq!(names(&first), ContextSection::ALL);
        delivery.commit(&first);
        assert!(delivery.pending(sections("chat", "ask")).is_empty());
        let changed = delivery.pending(sections("chat", "allow"));
        assert_eq!(names(&changed), [ContextSection::Permissions]);
        assert_eq!(changed.parts[0].text, "allow");
        // An attribute change, such as a new task kind, re-sends the section even with equal text.
        assert_eq!(
            names(&delivery.pending(sections("goal", "ask"))),
            [ContextSection::Task]
        );
    }

    /// Keep unaccepted sections pending so a rejected submission's retry carries them again.
    #[test]
    fn unaccepted_sections_remain_pending() {
        let delivery = ContextDelivery::new();
        let rejected = delivery.pending(sections("plan", "ask"));
        assert_eq!(delivery.pending(sections("plan", "ask")), rejected);
    }

    /// Deliver every section again after history may have lost it.
    #[test]
    fn clearing_redelivers_every_section() {
        let mut delivery = ContextDelivery::new();
        let first = delivery.pending(sections("chat", "ask"));
        delivery.commit(&first);
        delivery.clear();
        assert_eq!(
            names(&delivery.pending(sections("chat", "ask"))),
            ContextSection::ALL
        );
    }

    /// Ignore context selected by another record.
    #[test]
    fn commit_ignores_context_from_another_record() {
        let mut delivery = ContextDelivery::new();
        let mut foreign = delivery.pending(sections("chat", "ask"));
        foreign.record = "another".into();
        delivery.commit(&foreign);
        assert_eq!(delivery.pending(sections("chat", "ask")).parts.len(), 2);
        assert_ne!(ContextDelivery::new().record, delivery.record);
    }
}
