use crate::provider::{ProviderEventSink, ProviderOutput};
use crate::tools::{ControlToolInvocation, ControlToolRegistry, apply_invocation};
use anyhow::Result;
use serde_json::Value;

#[derive(Default)]
pub(super) struct CodexNormalization {
    native: forge_providers::codex::normalization::CodexNormalization,
}

impl std::ops::Deref for CodexNormalization {
    type Target = forge_providers::codex::normalization::CodexNormalization;
    fn deref(&self) -> &Self::Target {
        &self.native
    }
}
impl std::ops::DerefMut for CodexNormalization {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.native
    }
}

impl CodexNormalization {
    pub(super) async fn normalize(
        &mut self,
        message: &Value,
        output: &mut ProviderOutput,
        sink: Option<&ProviderEventSink>,
        workspace: &str,
        apply_control: bool,
    ) -> Result<()> {
        self.native.set_control_tools(
            ControlToolRegistry
                .definition_list()
                .into_iter()
                .map(|tool| tool.name.to_owned())
                .chain(std::iter::once("harness_plan_edit".to_owned())),
        );
        let prior_events = output.event.len();
        let mut native = forge_provider_api::completion::ProviderOutput {
            event: std::mem::take(&mut output.event),
            completion: std::mem::take(&mut output.completion.native),
        };
        let result = self
            .native
            .normalize(message, &mut native, None, workspace, apply_control)
            .await;
        output.event = native.event;
        output.completion.merge_native(native.completion);
        result?;
        for call in self.native.take_control_calls() {
            apply_invocation(
                &ControlToolInvocation {
                    task_permit: None,
                    name: call.name,
                    arguments: call.arguments,
                },
                &mut output.completion,
            )?;
        }
        if let Some(sink) = sink {
            for event in output.event.drain(prior_events..) {
                sink.send_wait(event).await?;
            }
        }
        Ok(())
    }
}
