//! Apply provider restart wakes, and own the transitions into and out of a restart.

use super::super::*;

impl ExchangeDriver {
    /// Apply one provider restart fact.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn observation_recovery_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: crate::exchange::observation_recovery::RecoveryEvent,
    ) -> WakeEnd {
        let DriverEnv {
            broker, services, ..
        } = env;
        if self.commands.storage_unavailable() {
            self.observation_recovery.fail_storage();
            self.execution_fail(services);
        }
        let result = {
            let mut broker = broker.lock().await;
            self.observation_recovery.apply(wake, &mut broker, services)
        };
        match result {
            Ok(true) => {
                let catalog = { broker.lock().await.provider_catalog_request() };
                match catalog {
                    Ok(catalog) => {
                        services.catalog_publish(catalog).await;
                        self.observation_recovery.finish(Ok(()));
                        self.restart_succeeded(env, state, step);
                    }
                    Err(_) => self.restart_failed(
                        env,
                        step,
                        crate::exchange::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable,
                    ),
                }
            }
            Ok(false) => {}
            Err(error) => self.restart_failed(env, step, error),
        }
        WakeEnd::Finished
    }

    /// Hold execution and restart the provider after work failed with unknown provider state.
    ///
    /// A failed turn or stop can leave native state the owner never observed, so the owner
    /// replaces the provider generation before any later command or turn runs. The failed work
    /// stays retained until that replacement succeeds. Queued turns fail now, because the
    /// restart starts only once the owner is idle and they cannot start while it waits.
    /// `cause` names the failure in the trace.
    pub(in crate::exchange::driver) fn restart_after_failure(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        failed: FailedWork,
        cause: &'static str,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        state.failed_work = Some(failed);
        queued_turns_reject(
            &mut state.pending,
            "the provider is restarting after a failure",
            env.context(step),
        );
        if !self
            .observation_recovery
            .request_after_failure(services.observer.session_events())
        {
            self.restart_failed(
                env,
                step,
                crate::exchange::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable,
            );
            return;
        }
        self.execution_restart_begin(services);
        trace.record(
            session_id,
            "exchange.restart.requested",
            serde_json::json!({"update_step":step,"kind":cause}),
        );
    }

    /// Reopen execution after the provider restarted, and release the failed turn it replaced.
    pub(in crate::exchange::driver) fn restart_succeeded(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        state.failed_work = None;
        self.execution_reopen(services);
        trace.record(
            session_id,
            "exchange.restart.finished",
            serde_json::json!({"update_step":step}),
        );
    }

    /// Stop execution for the session's lifetime after a provider restart failed, and report it.
    pub(in crate::exchange::driver) fn restart_failed(
        &mut self,
        env: &DriverEnv,
        step: u64,
        error: crate::exchange::provider_lifecycle::ProviderLifecycleFailure,
    ) {
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = env;
        trace.record(
            session_id,
            "exchange.restart.failed",
            serde_json::json!({"reason":error.to_string(),"update_step":step}),
        );
        services.tasks.mark_configuration_uncertain();
        self.observation_recovery.finish(Err(error.clone()));
        self.execution_fail(services);
        let event = forge_protocol::message::SessionEvent {
            session_id: session_id.clone(),
            event: "provider_event".into(),
            payload: serde_json::json!({
                "kind":"error",
                "code":"provider_restart_failed",
                "text":format!(
                    "The provider stopped and could not be restarted ({error}). Reopen the session."
                ),
            }),
        };
        if let Err(publish_error) = services.publications.publish(event, None) {
            trace.record(
                session_id,
                "exchange.restart.failure_unpublished",
                serde_json::json!({"reason":publish_error.to_string(),"update_step":step}),
            );
        }
    }
}
