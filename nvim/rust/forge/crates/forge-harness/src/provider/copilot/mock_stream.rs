use crate::provider::{ProviderCompletion, ProviderError, ProviderEventSink, ProviderRequest};

pub(crate) async fn replay(
    request: &ProviderRequest,
    sink: &ProviderEventSink,
) -> Result<ProviderCompletion, ProviderError> {
    Ok(forge_providers::copilot::mock_stream::replay(
        request
            .provider_session_id
            .clone()
            .unwrap_or_else(|| "mock-copilot-session".into()),
        request.workspace.clone(),
        request.model.clone(),
        sink,
    )
    .await?
    .into())
}
