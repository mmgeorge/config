use std::sync::{Arc, Weak};

use serde_json::{Map, Value};
use tracing::{
    Event, Metadata, Subscriber,
    field::{Field, Visit},
};
use tracing_subscriber::{
    Layer,
    layer::{Context, SubscriberExt},
    registry::{LookupSpan, Registry},
};

use super::{
    LogLevel,
    policy::diagnostic_field,
    writer::{Admission, TraceWriter},
};

enum Destination {
    Host,
    Store {
        writer: Weak<TraceWriter>,
        admission: Arc<Admission>,
    },
}

/// Connect tracing instrumentation to the existing owned, rotating JSONL writer.
pub(super) struct LoggingLayer {
    destination: Destination,
}

impl LoggingLayer {
    /// Create a scoped subscriber whose weak destination cannot keep a writer alive.
    pub(super) fn dispatch(
        writer: Weak<TraceWriter>,
        admission: Arc<Admission>,
    ) -> tracing::Dispatch {
        tracing::Dispatch::new(Registry::default().with(Self {
            destination: Destination::Store { writer, admission },
        }))
    }

    /// Resolve the host destination or the exact store selected by a scoped dispatcher.
    fn writer(&self) -> Option<Arc<TraceWriter>> {
        match &self.destination {
            Destination::Host => super::store::TraceStore::active_writer(),
            Destination::Store { writer, admission } => {
                if admission.is_closed() {
                    return None;
                }
                writer.upgrade()
            }
        }
    }
}

impl<RegistryType> Layer<RegistryType> for LoggingLayer
where
    RegistryType: Subscriber + for<'lookup> LookupSpan<'lookup>,
{
    /// Re-evaluate the cached mode after a picker change or late host initialization.
    fn register_callsite(
        &self,
        _metadata: &'static Metadata<'static>,
    ) -> tracing::subscriber::Interest {
        tracing::subscriber::Interest::sometimes()
    }

    /// Filter before formatting fields; retain correlation spans even at warning/error thresholds.
    fn enabled(&self, metadata: &Metadata<'_>, _context: Context<'_, RegistryType>) -> bool {
        if metadata.is_span() {
            return true;
        }
        let Some(writer) = self.writer() else {
            return false;
        };
        let level = writer.level();
        if level == LogLevel::None {
            return false;
        }
        if !metadata.target().starts_with("forge")
            && level != LogLevel::Trace
            && *metadata.level() > tracing::Level::WARN
        {
            return false;
        }
        level.allows(metadata.level())
    }

    /// Retain explicit correlation fields, never automatic argument dumps.
    fn on_new_span(
        &self,
        attributes: &tracing::span::Attributes<'_>,
        id: &tracing::Id,
        context: Context<'_, RegistryType>,
    ) {
        let mut fields = DiagnosticFields::default();
        attributes.record(&mut fields);
        if let Some(span) = context.span(id) {
            span.extensions_mut().insert(fields);
        }
    }

    /// Keep late-recorded span identities available to subsequent diagnostics.
    fn on_record(
        &self,
        id: &tracing::Id,
        values: &tracing::span::Record<'_>,
        context: Context<'_, RegistryType>,
    ) {
        if let Some(span) = context.span(id)
            && let Some(fields) = span.extensions_mut().get_mut::<DiagnosticFields>()
        {
            values.record(fields);
        }
    }

    /// Publish redacted diagnostic fields and the logical async span path without raw payloads.
    fn on_event(&self, event: &Event<'_>, context: Context<'_, RegistryType>) {
        let Some(writer) = self.writer() else {
            return;
        };
        let mut fields = DiagnosticFields::default();
        if event.metadata().target() == "forge::record" {
            fields.source = FieldSource::Metadata;
        }
        event.record(&mut fields);
        let mut span_path = Vec::new();
        let mut inherited = Map::new();
        if let Some(scope) = context.event_scope(event) {
            for span in scope.from_root() {
                let mut retained = span
                    .extensions()
                    .get::<DiagnosticFields>()
                    .map(|fields| fields.values.clone())
                    .unwrap_or_default();
                inherited.extend(retained.clone());
                retained.insert("name".into(), span.name().into());
                span_path.push(Value::Object(retained));
            }
        }
        if event.metadata().target() == "forge::record"
            && let Some(encoded) = fields.payload.take()
        {
            match serde_json::from_str::<Value>(&encoded) {
                Ok(Value::Object(payload)) => fields.values.extend(payload),
                Ok(_) | Err(_) => {
                    eprintln!("Forge tracing metadata could not be decoded");
                    return;
                }
            }
        }
        inherited.extend(fields.values);
        let session_id = inherited
            .remove("session_id")
            .and_then(|value| value.as_str().map(str::to_owned))
            .unwrap_or_else(|| "global".into());
        let event_name = inherited
            .remove("event_name")
            .and_then(|value| value.as_str().map(str::to_owned))
            .unwrap_or_else(|| event.metadata().name().into());
        inherited.insert(
            "level".into(),
            event
                .metadata()
                .level()
                .as_str()
                .to_ascii_lowercase()
                .into(),
        );
        inherited.insert("target".into(), event.metadata().target().into());
        if let Some(file) = event.metadata().file() {
            inherited.insert("file".into(), file.into());
        }
        if let Some(line) = event.metadata().line() {
            inherited.insert("line".into(), line.into());
        }
        if !span_path.is_empty() {
            inherited.insert("span_path".into(), span_path.into());
        }
        let admission = match &self.destination {
            Destination::Host => &writer.admission,
            Destination::Store { admission, .. } => admission,
        };
        writer.record(
            admission,
            &session_id,
            &event_name,
            &Value::Object(inherited),
            *event.metadata().level(),
        );
    }
}

#[derive(Default)]
enum FieldSource {
    #[default]
    Native,
    Metadata,
}

#[derive(Default)]
struct DiagnosticFields {
    values: Map<String, Value>,
    payload: Option<String>,
    source: FieldSource,
}

impl Visit for DiagnosticFields {
    /// Preserve ordinary text while excluding unrecognized native fields.
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "payload" && matches!(self.source, FieldSource::Metadata) {
            self.payload = Some(value.into());
        } else if diagnostic_field(field.name())
            || matches!(field.name(), "message" | "error" | "event_name")
        {
            self.values.insert(field.name().into(), value.into());
        }
    }

    /// Preserve typed scalar metadata without stringifying it.
    fn record_u64(&mut self, field: &Field, value: u64) {
        if diagnostic_field(field.name()) {
            self.values.insert(field.name().into(), value.into());
        }
    }

    /// Preserve typed scalar metadata without stringifying it.
    fn record_i64(&mut self, field: &Field, value: i64) {
        if diagnostic_field(field.name()) {
            self.values.insert(field.name().into(), value.into());
        }
    }

    /// Keep duration measurements numeric for external JSONL consumers.
    fn record_f64(&mut self, field: &Field, value: f64) {
        if diagnostic_field(field.name())
            && let Some(number) = serde_json::Number::from_f64(value)
        {
            self.values
                .insert(field.name().into(), Value::Number(number));
        }
    }

    /// Preserve typed scalar metadata without stringifying it.
    fn record_bool(&mut self, field: &Field, value: bool) {
        if diagnostic_field(field.name()) {
            self.values.insert(field.name().into(), value.into());
        }
    }

    /// Format only allowlisted display fields, not arbitrary provider arguments.
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if diagnostic_field(field.name())
            || matches!(field.name(), "message" | "error" | "event_name")
            || field.name() == "payload" && matches!(self.source, FieldSource::Metadata)
        {
            self.record_str(field, &format!("{value:?}"));
        }
    }
}

/// Install the host subscriber once; library callers can use a store-scoped dispatcher instead.
pub fn install() -> Result<(), tracing::subscriber::SetGlobalDefaultError> {
    tracing::subscriber::set_global_default(Registry::default().with(LoggingLayer {
        destination: Destination::Host,
    }))
}
