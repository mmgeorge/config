use forge_provider_api::{ProviderContextWindow, ProviderModel};
use github_copilot_sdk::Model;

pub fn model_descriptor(model: Model) -> ProviderModel {
    let context_limit = model
        .capabilities
        .limits
        .as_ref()
        .and_then(|limit| limit.max_context_window_tokens)
        .and_then(|limit| u64::try_from(limit).ok())
        .filter(|limit| *limit > 0);
    let token_price = model
        .billing
        .as_ref()
        .and_then(|billing| billing.token_prices.as_ref());
    let mut context_window = vec![ProviderContextWindow {
        id: "default".into(),
        token_limit: context_limit,
    }];
    if token_price
        .and_then(|price| price.long_context.as_ref())
        .is_some()
    {
        context_window.push(ProviderContextWindow {
            id: "long_context".into(),
            token_limit: None,
        });
    }
    let vision = model
        .capabilities
        .supports
        .as_ref()
        .and_then(|supports| supports.vision)
        .unwrap_or_else(|| {
            model
                .capabilities
                .limits
                .as_ref()
                .is_some_and(|limit| limit.vision.is_some())
        });
    ProviderModel {
        service_tiers: vec![forge_provider_api::ServiceTier::Standard],
        is_default: model.id == "auto",
        id: model.id,
        reasoning: model.supported_reasoning_efforts.unwrap_or_default(),
        default_reasoning: model.default_reasoning_effort,
        selected_reasoning: None,
        context_window,
        default_context_window: Some("default".into()),
        selected_context_window: None,
        vision,
        description: None,
    }
}
