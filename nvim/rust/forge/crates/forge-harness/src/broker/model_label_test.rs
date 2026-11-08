use super::preference_for_session;
use crate::provider::{ProviderModel, ServiceTier};
use crate::session::catalog_model_label;

fn catalog_model(id: &str, label: &str, is_default: bool) -> ProviderModel {
    ProviderModel {
        service_tiers: vec![ServiceTier::Standard],
        id: id.into(),
        label: Some(label.into()),
        reasoning: Vec::new(),
        default_reasoning: None,
        selected_reasoning: None,
        context_window: Vec::new(),
        default_context_window: None,
        selected_context_window: None,
        vision: false,
        description: None,
        is_default,
    }
}

/// A selected alias keeps its catalog label in the provider preference, and a later session
/// that never loaded the catalog does not erase it.
#[test]
fn preference_retains_selected_model_label() {
    let catalog = [
        catalog_model("default", "Default (recommended)", false),
        catalog_model("opus", "Opus 5.5", true),
    ];
    let mut session = crate::storage::test::session("labeled", "workspace");
    session.select_model("opus".into(), catalog_model_label(&catalog, "opus"));
    let preference = preference_for_session(&session, None);
    assert_eq!(
        preference.model_setting["opus"].label.as_deref(),
        Some("Opus 5.5")
    );

    let mut unlabeled = crate::storage::test::session("unlabeled", "workspace");
    unlabeled.select_model("opus".into(), None);
    let retained = preference_for_session(&unlabeled, Some(preference));
    assert_eq!(
        retained.model_setting["opus"].label.as_deref(),
        Some("Opus 5.5")
    );

    unlabeled.refresh_model_label(&[catalog_model("opus", "Opus 6", true)]);
    assert_eq!(unlabeled.model_label.as_deref(), Some("Opus 6"));
    assert_eq!(
        catalog_model_label(&catalog, "default").as_deref(),
        Some("Default (recommended)")
    );
}
