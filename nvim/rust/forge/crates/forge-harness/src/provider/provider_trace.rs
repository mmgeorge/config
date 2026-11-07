use crate::trace::TraceStore;
use forge_provider_api::trace::{ProviderTrace, ProviderTraceRecord};

impl ProviderTrace for TraceStore {
    fn record(&self, record: ProviderTraceRecord) {
        let owner = record
            .scope
            .as_ref()
            .map(|scope| scope.owner.session_id().as_str())
            .unwrap_or_else(|| record.binding_id.as_str());
        if let Ok(payload) = serde_json::to_value(&record) {
            TraceStore::record(self, owner, "provider.lifecycle", payload);
        }
    }
}
