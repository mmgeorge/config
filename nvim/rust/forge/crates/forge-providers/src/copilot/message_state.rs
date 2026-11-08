use std::collections::HashMap;

use anyhow::{Result, ensure};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CopilotPhase {
    Commentary,
    FinalAnswer,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum CopilotPhaseMetadata {
    #[default]
    Missing,
    Known(CopilotPhase),
    Unknown(String),
}

impl CopilotPhaseMetadata {
    pub fn parse(value: Option<&str>) -> Self {
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
pub struct CopilotMessageState {
    pub phase: CopilotPhaseMetadata,
    pub text: String,
    pub published: bool,
    pub completed: bool,
    order: usize,
}

#[derive(Default)]
pub struct CopilotMessageStore {
    message: HashMap<(String, String), CopilotMessageState>,
    explicit_final_seen: bool,
    fallback_id: Option<String>,
}

impl CopilotMessageStore {
    pub fn has_unfinished_text(&self, thread: Option<&str>) -> bool {
        self.message.iter().any(|((owner, _), message)| {
            thread == Some(owner.as_str()) && message.published && !message.completed
        })
    }

    pub fn interrupt_open(&mut self, thread: Option<&str>) -> Vec<((String, String), String)> {
        self.message
            .iter_mut()
            .filter_map(|(key, message)| {
                if thread != Some(key.0.as_str()) || !message.published || message.completed {
                    return None;
                }
                message.completed = true;
                Some((key.clone(), message.text.clone()))
            })
            .collect()
    }

    pub fn observe(
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

    pub fn fallback(&mut self, root_thread: &str) -> Option<(String, String)> {
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

    pub fn append(message: &mut CopilotMessageState, text: &str) -> Result<()> {
        ensure!(
            !message.completed,
            "completed Copilot message rejects a text delta"
        );
        message.text.push_str(text);
        Ok(())
    }

    pub fn complete(message: &mut CopilotMessageState, text: &str) -> Result<()> {
        ensure!(
            !message.completed || message.text == text,
            "completed Copilot message rejects a conflicting snapshot"
        );
        message.text = text.into();
        message.completed = true;
        Ok(())
    }
}
