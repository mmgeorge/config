use crate::tools::schema::string_schema;
use serde_json::Value;
pub(super) fn session() -> (&'static str, Value) {
    ("session_id", string_schema())
}
pub(super) fn cursor() -> (&'static str, Value) {
    ("cursor", string_schema())
}
pub(super) fn filters() -> Vec<(&'static str, Value)> {
    vec![
        ("repository_id", string_schema()),
        ("workspace", string_schema()),
    ]
}
