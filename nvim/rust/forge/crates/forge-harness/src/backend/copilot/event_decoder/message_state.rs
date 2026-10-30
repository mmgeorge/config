use std::collections::HashMap;

use anyhow::{Result, ensure};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum CopilotPhase {
    Commentary,
    FinalAnswer,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(super) enum CopilotPhaseMetadata {
    #[default]
    Missing,
    Known(CopilotPhase),
    Unknown(String),
}

impl CopilotPhaseMetadata {
    pub(super) fn parse(value: Option<&str>) -> Self {
        match value {
            None => Self::Missing,
            Some("commentary") => Self::Known(CopilotPhase::Commentary),
            Some("final_answer") => Self::Known(CopilotPhase::FinalAnswer),
            Some(value) => Self::Unknown(value.into()),
        }
    }

    fn reconcile(&mut self, incoming: Self) -> Result<()> {
        if incoming == Self::Missing {
            return Ok(());
        }
        ensure!(
            !matches!(self, Self::Known(_)) || *self == incoming,
            "Copilot message changed phase from {self:?} to {incoming:?}"
        );
        *self = incoming;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub(super) struct CopilotMessageState {
    pub(super) phase: CopilotPhaseMetadata,
    pub(super) text: String,
    pub(super) published: bool,
    pub(super) completed: bool,
    order: usize,
}

#[derive(Default)]
pub(super) struct CopilotMessageStore {
    message: HashMap<(String, String), CopilotMessageState>,
    explicit_final_seen: bool,
    fallback_id: Option<String>,
}

impl CopilotMessageStore {
    pub(super) fn has_text(&self) -> bool {
        self.message
            .values()
            .any(|message| !message.text.is_empty())
    }

    pub(super) fn has_unfinished_text(&self) -> bool {
        self.message
            .values()
            .any(|message| message.published && !message.completed)
    }

    pub(super) fn interrupt_open(&mut self) -> Vec<(String, String)> {
        self.message
            .iter_mut()
            .filter_map(|(key, message)| {
                if !message.published || message.completed {
                    return None;
                }
                message.completed = true;
                Some(key.clone())
            })
            .collect()
    }

    pub(super) fn observe(
        &mut self,
        thread: &str,
        id: &str,
        phase: CopilotPhaseMetadata,
        root: bool,
    ) -> Result<&mut CopilotMessageState> {
        ensure!(!id.is_empty(), "Copilot message identity is empty");
        let order = self.message.len();
        let message = self
            .message
            .entry((thread.into(), id.into()))
            .or_insert_with(|| CopilotMessageState {
                phase: CopilotPhaseMetadata::Missing,
                text: String::new(),
                published: false,
                completed: false,
                order,
            });
        message.phase.reconcile(phase)?;
        self.explicit_final_seen |=
            root && message.phase == CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer);
        Ok(message)
    }

    pub(super) fn fallback(&mut self, root_thread: &str) -> Option<(String, String)> {
        if self.explicit_final_seen || self.fallback_id.is_some() {
            return None;
        }
        let ((_, id), message) = self
            .message
            .iter()
            .filter(|((thread, _), message)| {
                thread == root_thread && !message.text.trim().is_empty()
            })
            .max_by_key(|(_, message)| message.order)?;
        if message.phase != CopilotPhaseMetadata::Known(CopilotPhase::Commentary) {
            return None;
        }
        let id = format!("forge:copilot:response:{}", id);
        let text = message.text.clone();
        self.fallback_id = Some(id.clone());
        Some((id, text))
    }

    pub(super) fn append(message: &mut CopilotMessageState, text: &str) -> Result<()> {
        ensure!(
            !message.completed,
            "completed Copilot message rejects a text delta"
        );
        message.text.push_str(text);
        Ok(())
    }

    pub(super) fn complete(message: &mut CopilotMessageState, text: &str) -> Result<()> {
        ensure!(
            !message.completed || message.text == text,
            "completed Copilot message rejects a conflicting snapshot"
        );
        message.text = text.into();
        message.completed = true;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn commentary(store: &mut CopilotMessageStore, id: &str, text: &str) {
        store
            .observe(
                "root",
                id,
                CopilotPhaseMetadata::Known(CopilotPhase::Commentary),
                true,
            )
            .unwrap()
            .text = text.into();
    }

    #[test]
    fn late_older_snapshot_does_not_replace_latest_commentary_candidate() {
        let mut store = CopilotMessageStore::default();
        commentary(&mut store, "first", "First");
        commentary(&mut store, "second", "Second");
        commentary(&mut store, "first", "First authoritative");
        let (id, text) = store.fallback("root").unwrap();
        assert_ne!(id, "second");
        assert_eq!(text, "Second");
        assert!(store.fallback("root").is_none());
    }

    #[test]
    fn explicit_empty_final_suppresses_commentary_fallback() {
        let mut store = CopilotMessageStore::default();
        commentary(&mut store, "commentary", "Progress");
        store
            .observe(
                "root",
                "empty-final",
                CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer),
                true,
            )
            .unwrap();
        assert!(store.fallback("root").is_none());
    }

    #[test]
    fn unknown_later_text_blocks_stale_commentary_fallback() {
        let mut store = CopilotMessageStore::default();
        commentary(&mut store, "commentary", "Progress");
        store
            .observe(
                "root",
                "unknown",
                CopilotPhaseMetadata::Unknown("future".into()),
                true,
            )
            .unwrap()
            .text = "Unclassified newer text".into();
        assert!(store.fallback("root").is_none());
    }

    #[test]
    fn child_final_and_empty_tool_envelopes_do_not_replace_root_candidate() {
        let mut store = CopilotMessageStore::default();
        commentary(&mut store, "commentary", "Root progress");
        store
            .observe(
                "child",
                "final",
                CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer),
                false,
            )
            .unwrap()
            .text = "Child answer".into();
        store
            .observe("root", "tool-envelope", CopilotPhaseMetadata::Missing, true)
            .unwrap();
        commentary(&mut store, "whitespace", " \n\t");
        assert_eq!(store.fallback("root").unwrap().1, "Root progress");
    }

    #[test]
    fn phase_metadata_distinguishes_missing_unknown_and_conflicting_values() {
        assert_eq!(
            CopilotPhaseMetadata::parse(None),
            CopilotPhaseMetadata::Missing
        );
        assert_eq!(
            CopilotPhaseMetadata::parse(Some("thinking")),
            CopilotPhaseMetadata::Unknown("thinking".into())
        );
        let mut store = CopilotMessageStore::default();
        commentary(&mut store, "message", "Progress");
        let error = store
            .observe(
                "root",
                "message",
                CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer),
                true,
            )
            .unwrap_err();
        assert!(error.to_string().contains("changed phase"));
    }

    #[test]
    fn snapshot_replaces_deltas_and_terminal_replays_do_not_append() {
        let mut store = CopilotMessageStore::default();
        let message = store
            .observe(
                "root",
                "commentary",
                CopilotPhaseMetadata::Known(CopilotPhase::Commentary),
                true,
            )
            .unwrap();
        CopilotMessageStore::append(message, "draft").unwrap();
        CopilotMessageStore::complete(message, "authoritative").unwrap();
        CopilotMessageStore::complete(message, "authoritative").unwrap();
        assert!(CopilotMessageStore::complete(message, "different").is_err());
        assert!(CopilotMessageStore::append(message, "late").is_err());
        assert_eq!(store.fallback("root").unwrap().1, "authoritative");
    }
}
