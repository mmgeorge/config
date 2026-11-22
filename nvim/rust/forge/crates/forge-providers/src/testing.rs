//! Exercise native provider fixtures through the production retained turn owner.

pub use crate::turn_runner::{NativeTurnDriver, RetainedTurn};

#[cfg(test)]
pub(crate) mod diagnostics {
    use std::sync::{Arc, Mutex};

    /// Record every field of every diagnostic event the adapter emits.
    pub(crate) struct CapturedFields(pub(crate) Arc<Mutex<Vec<String>>>);

    impl tracing::field::Visit for CapturedFields {
        fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
            self.0
                .lock()
                .unwrap()
                .push(format!("{}={value:?}", field.name()));
        }
    }

    impl tracing_subscriber::Layer<tracing_subscriber::Registry> for CapturedFields {
        fn on_event(
            &self,
            event: &tracing::Event<'_>,
            context: tracing_subscriber::layer::Context<'_, tracing_subscriber::Registry>,
        ) {
            if let Some(span) = context.event_span(event) {
                self.0.lock().unwrap().push(format!("span={}", span.name()));
            }
            event.record(&mut CapturedFields(self.0.clone()));
        }
    }
}
