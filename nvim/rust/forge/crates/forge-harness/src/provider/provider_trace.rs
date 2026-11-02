use crate::trace::TraceStore;
use forge_provider_api::trace::ProviderTrace;
use serde_json::Value;

impl ProviderTrace for TraceStore {
    fn record(&self, session_id: &str, event: &str, payload: Value) {
        TraceStore::record(self, session_id, event, payload);
    }
}
