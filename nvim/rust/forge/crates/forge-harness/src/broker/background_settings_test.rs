use super::*;

/// Reject explicit Background tuning before writing it or opening a provider conversation.
#[tokio::test]
async fn invalid_background_model_preserves_session_before_dispatch() -> Result<()> {
    let root = tempfile::tempdir()?;
    let identity = crate::background::BackgroundIdentity {
        branch: None,
        head: None,
        reason: String::new(),
    };
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace": root.path(), "data_root": root.path().join("data"),
        "client_id": "background-selection", "provider": {"kind":"mock", "command":[]},
        "background": identity,
    }))?)?;
    let before = broker.session.clone();
    let config = crate::service::TextGenerationConfig {
        provider: broker.provider_launch.clone(),
        model: "removed-model".into(),
        reasoning_effort: Some("removed-effort".into()),
        service_tier: crate::provider::ServiceTier::Ultrafast,
        auto_compaction: Default::default(),
        data_root: root.path().join("data"),
    };
    let result = broker.configure_background_model(&config).await;
    let stored = broker.store.load_session(&before.id)?.unwrap();
    assert_eq!(stored.model, before.model);
    assert_eq!(stored.effort, before.effort);
    assert_eq!(stored.service_tier, before.service_tier);
    assert!(stored.provider_session_id.is_none());
    assert!(broker.store.list_exchange(&before.id)?.is_empty());
    assert!(
        result.is_err(),
        "invalid tuning must fail before provider work"
    );
    broker.provider.shutdown().await?;
    Ok(())
}
