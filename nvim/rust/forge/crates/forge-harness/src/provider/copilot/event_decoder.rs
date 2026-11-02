pub use forge_providers::copilot::event_decoder::CopilotEventDecoder;

#[cfg(test)]
use github_copilot_sdk::SessionEvent;
#[cfg(test)]
use serde_json::Value;

#[cfg(test)]
mod tests;
