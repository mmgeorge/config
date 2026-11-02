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
        reasoning: ordered_efforts(model.supported_reasoning_efforts.unwrap_or_default()),
        default_reasoning: model.default_reasoning_effort,
        selected_reasoning: None,
        context_window,
        default_context_window: Some("default".into()),
        selected_context_window: None,
        vision,
        description: None,
    }
}

/// Order this provider's native effort vocabulary before publishing its model catalog.
fn ordered_efforts(mut choices: Vec<String>) -> Vec<String> {
    let order = [
        "none", "minimal", "low", "medium", "high", "xhigh", "max", "ultra",
    ];
    choices.sort_by_key(|value| {
        order
            .iter()
            .position(|item| *item == value)
            .unwrap_or(order.len())
    });
    choices
}

/// Order native catalog families without requiring the picker to parse provider identifiers.
pub fn order_models(models: &mut [ProviderModel]) {
    fn rank(id: &str) -> (u8, std::cmp::Reverse<Vec<u64>>, u8) {
        if id == "auto" {
            return (0, std::cmp::Reverse(vec![]), 0);
        }
        let normalized = id.to_ascii_lowercase();
        let id = normalized.as_str();
        let (family, version, variant) = if let Some(rest) = id.strip_prefix("claude-") {
            match rest.split_once('-') {
                Some((kind, version)) if ["opus", "sonnet", "haiku"].contains(&kind) => (
                    1,
                    version,
                    match kind {
                        "opus" => 0,
                        "sonnet" => 1,
                        "haiku" => 2,
                        _ => unreachable!(),
                    },
                ),
                _ => return (4, std::cmp::Reverse(vec![]), 0),
            }
        } else if let Some(version) = id.strip_prefix("gpt-") {
            (2, version, 0)
        } else if let Some(version) = id.strip_prefix("gemini-") {
            (3, version, 0)
        } else {
            return (4, std::cmp::Reverse(vec![]), 0);
        };
        if !version.starts_with(|c: char| c.is_ascii_digit()) {
            return (4, std::cmp::Reverse(vec![]), 0);
        }
        let version = version
            .split(|c: char| !c.is_ascii_digit())
            .take_while(|s| !s.is_empty())
            .filter_map(|s| s.parse().ok())
            .collect();
        (family, std::cmp::Reverse(version), variant)
    }
    models.sort_by(|a, b| {
        let left = rank(&a.id);
        let right = rank(&b.id);
        left.cmp(&right).then_with(|| {
            if left.0 == 4 {
                std::cmp::Ordering::Equal
            } else {
                a.id.cmp(&b.id)
            }
        })
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn native_model_order_is_published_without_picker_name_rules() {
        let mut models: Vec<ProviderModel> = [
            "custom-b",
            "gpt-5.1",
            "gemini-2.5",
            "claude-sonnet-4.5",
            "auto",
            "gpt-5.2",
            "claude-opus-4.5",
            "custom-a",
        ]
        .into_iter()
        .map(|id| {
            serde_json::from_value(
                serde_json::json!({"id":id,"reasoning":[],"context_window":[],"vision":false}),
            )
            .unwrap()
        })
        .collect();
        order_models(&mut models);
        assert_eq!(
            models
                .iter()
                .map(|model| model.id.as_str())
                .collect::<Vec<_>>(),
            [
                "auto",
                "claude-opus-4.5",
                "claude-sonnet-4.5",
                "gpt-5.2",
                "gpt-5.1",
                "gemini-2.5",
                "custom-b",
                "custom-a"
            ]
        );
    }
}
