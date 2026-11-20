//! Validate a session's inherited model tuning against a catalog that may answer late.

use super::*;

impl SessionController {
    /// Check the session's inherited model, thinking, context, and speed against the provider's
    /// catalog in the background, once per controller, and publish any correction.
    ///
    /// A provider's model list can take tens of seconds on a loaded machine, and opening a
    /// session, which every provider switch waits on, must not depend on it. The catalog query
    /// therefore runs after the open without holding the broker, and a configuration committed
    /// meanwhile wins over the late catalog. Until the catalog answers, the provider binding
    /// validates the inherited values when it opens. A catalog that cannot be read leaves the
    /// values as saved; a saved model the catalog does not offer is reported in the session.
    pub(super) fn resolve_inherited_model_settings(self: &Arc<Self>, sink: &MessageSender) {
        let mut slot = self
            .inherited_model_resolution
            .lock()
            .expect("inherited model resolution slot poisoned");
        if slot.is_some() {
            return;
        }
        let controller = Arc::downgrade(self);
        let sink = sink.clone();
        *slot = Some(tokio::spawn(async move {
            let Some(started_controller) = controller.upgrade() else {
                return;
            };
            let session_id = started_controller.observer.session_id().to_owned();
            let started = Instant::now();
            let catalog = started_controller
                .broker
                .lock()
                .await
                .inherited_model_catalog();
            // Discovery must not keep a closed session's controller alive.
            drop(started_controller);
            let result = match catalog {
                Ok(catalog) => {
                    tracing::Instrument::instrument(
                        catalog.run(),
                        tracing::info_span!("session_model_validation", session_id),
                    )
                    .await
                }
                Err(error) => Err(error),
            };
            tracing::info!(target: "forge::harness", event_name = "session.model_validation.finished",
                session_id, success = result.is_ok(),
                elapsed_ms = started.elapsed().as_secs_f64() * 1000.0);
            let Some(controller) = controller.upgrade() else {
                return;
            };
            let (inherited, models) = match result {
                Ok(catalog) => catalog,
                Err(error) => {
                    controller.trace.record(
                        &session_id,
                        "session.inherited_model_settings_unchecked",
                        json!({"reason": format!("{error:#}")}),
                    );
                    return;
                }
            };
            let applied = controller
                .broker
                .lock()
                .await
                .apply_inherited_model_settings(&inherited, &models);
            let event = match applied {
                Ok(Some(event)) => event,
                Ok(None) => return,
                Err(error) => {
                    controller.trace.record(
                        &session_id,
                        "session.inherited_model_settings_rejected",
                        json!({"reason": format!("{error:#}")}),
                    );
                    SessionEvent {
                        session_id: session_id.clone(),
                        event: "provider_event".into(),
                        payload: json!({"kind": "error", "text": format!(
                            "The saved model settings do not match the provider's model list: \
                             {error:#}. Choose a model with /model.")}),
                    }
                }
            };
            if let Err(publication) = controller.publications.publish(event, Some(&sink)) {
                controller.trace.record(
                    &session_id,
                    "session.inherited_model_settings_publication_failed",
                    json!({"reason": publication.to_string()}),
                );
            }
        }));
    }

    /// Stop a catalog check that has not answered; it only reads the catalog, so stopping it
    /// changes nothing.
    pub(super) fn stop_inherited_model_resolution(&self) {
        if let Some(task) = self
            .inherited_model_resolution
            .lock()
            .expect("inherited model resolution slot poisoned")
            .take()
        {
            task.abort();
        }
    }
}
