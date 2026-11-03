use std::collections::VecDeque;
use std::sync::Mutex;

use forge_provider_api::{ProviderChangeSet, ProviderEvent, ToolAddress, ToolUpdate};

/// Attach host rename effects to the native tool's ordered lifecycle events.
#[derive(Default)]
pub(super) struct ChangePublication {
    state: Mutex<State>,
}

#[derive(Default)]
struct State {
    calls: VecDeque<(String, ToolAddress)>,
    completed: VecDeque<(String, ProviderChangeSet)>,
}

impl ChangePublication {
    /// Retain committed edits until the native tool's next output or completion is observed.
    pub(super) fn publish(
        &self,
        name: &str,
        arguments: &serde_json::Value,
        changes: ProviderChangeSet,
    ) {
        let identity = format!("{name}:{}", arguments);
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .completed
            .push_back((identity, changes));
    }

    /// Place changes after an observed native event, never ahead of its tool declaration.
    pub(crate) fn observe(&self, event: &ProviderEvent) -> Vec<ProviderEvent> {
        let ProviderEvent::Tool { address, update } = event else {
            return Vec::new();
        };
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let ToolUpdate::Requested { invocation } = update {
            let Some(name) = [super::rename::FILE, super::rename::SYMBOL]
                .into_iter()
                .find(|name| invocation.name.ends_with(name))
            else {
                return Vec::new();
            };
            if !state.calls.iter().any(|(_, previous)| previous == address) {
                state
                    .calls
                    .push_back((format!("{name}:{}", invocation.arguments), address.clone()));
            }
        }
        let Some(index) = state.calls.iter().position(|(_, call)| call == address) else {
            return Vec::new();
        };
        let identity = &state.calls[index].0;
        if let Some(completed) = state
            .completed
            .iter()
            .position(|(call, _)| call == identity)
        {
            let (_, changes) = state
                .completed
                .remove(completed)
                .expect("located completion exists");
            state.calls.remove(index);
            return vec![ProviderEvent::Tool {
                address: address.clone(),
                update: ToolUpdate::Changes { changes },
            }];
        }
        if matches!(update, ToolUpdate::Finished { .. }) {
            state.calls.remove(index);
        }
        Vec::new()
    }
}
