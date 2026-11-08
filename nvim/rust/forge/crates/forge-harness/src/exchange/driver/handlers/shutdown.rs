//! Apply shutdown wakes for the exchange driver loop.

use super::super::*;

impl ExchangeDriver {
    /// Apply one shutdown fact.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn shutdown_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        step: u64,
        foreground_status: &TurnStatus,
        _closed: &mut bool,
        wake: crate::exchange::shutdown::ShutdownEvent,
    ) -> WakeEnd {
        let DriverEnv {
            broker,
            trace,
            session_id,
            services,
            ..
        } = env;
        let DriverState { shutdown, .. } = state;
        let event = wake;
        trace.record(
            session_id,
            "exchange.shutdown.event",
            serde_json::json!({"event":event.name(),"update_step":step}),
        );
        let progress = shutdown
            .as_mut()
            .expect("shutdown evidence retains its collection owner")
            .update(
                event,
                foreground_status.clone(),
                &mut *broker.lock().await,
                services,
            );
        if let Some(failure) = progress.failure {
            trace.record(
                session_id,
                "exchange.shutdown.stop_failed",
                serde_json::json!({"update_step":step,"reason":failure.to_string()}),
            );
        }
        if let Some(input) = progress.input {
            publish_input_observation(input, None, services, trace, session_id, step);
        }
        for event in progress.events {
            if let Err(error) = retain_publication(event, None, services, trace, session_id, step) {
                trace.record(
                    session_id,
                    "exchange.shutdown.publication_failed",
                    serde_json::json!({"update_step":step,"reason":error.to_string()}),
                );
            }
        }
        WakeEnd::Finished
    }

    /// Retry shutdown collection after it reported progress.
    #[allow(clippy::too_many_arguments)]
    pub(in crate::exchange::driver) async fn shutdown_retry_wake(
        &mut self,
        env: &DriverEnv,
        state: &mut DriverState,
        _update_step: u64,
        _foreground_status: &TurnStatus,
        _closed: &mut bool,
    ) -> WakeEnd {
        let DriverEnv { services, .. } = env;
        let DriverState { shutdown, .. } = state;
        {
            {
                if let Some(shutdown) = shutdown.as_mut() {
                    shutdown.retry(services);
                }
            }
        }
        WakeEnd::Finished
    }
}
