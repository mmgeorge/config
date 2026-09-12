use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer, de, ser::SerializeStruct};
use serde_json::Value;

/// Carries a method and payload without importing the owning feature.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Request {
    pub id: u64,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

/// Carries exactly one successful result or structured failure for a request.
#[derive(Clone, Debug)]
pub struct Response {
    pub id: u64,
    outcome: Result<Value, ProtocolError>,
}

/// Preserves a stable failure code and optional recovery metadata.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ProtocolError {
    pub code: String,
    pub message: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// Associates an asynchronous feature event with its owning session.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SessionEvent {
    pub session_id: String,
    pub event: String,
    pub payload: Value,
}

/// Correlates host feature progress with its active request without requiring a Harness session.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RequestEvent {
    pub request_id: u64,
    pub event: String,
    pub payload: Value,
}

/// Routes document updates independently of request completion and Harness sessions.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DocumentEvent {
    pub document: String,
    pub event: String,
    pub payload: Value,
}

/// Distinguishes correlated responses from unsolicited session events.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    Response(Response),
    Event(SessionEvent),
    RequestEvent(RequestEvent),
    DocumentEvent(DocumentEvent),
}

impl Response {
    /// Encode a successful payload, including an explicit JSON null result.
    pub fn success<Payload: Serialize>(id: u64, result: Payload) -> serde_json::Result<Self> {
        Ok(Self {
            id,
            outcome: Ok(serde_json::to_value(result)?),
        })
    }

    /// Create a terminal failure without recovery metadata.
    pub fn failure(id: u64, code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            id,
            outcome: Err(ProtocolError {
                code: code.into(),
                message: message.into(),
                data: None,
            }),
        }
    }

    /// Create a terminal failure with structured recovery metadata.
    pub fn failure_with_data(
        id: u64,
        code: impl Into<String>,
        message: impl Into<String>,
        data: Value,
    ) -> Self {
        Self {
            id,
            outcome: Err(ProtocolError {
                code: code.into(),
                message: message.into(),
                data: Some(data),
            }),
        }
    }

    /// Borrow the successful payload without consuming the response.
    pub fn result(&self) -> Option<&Value> {
        self.outcome.as_ref().ok()
    }

    /// Borrow the structured failure without consuming the response.
    pub fn error(&self) -> Option<&ProtocolError> {
        self.outcome.as_ref().err()
    }
}

impl Serialize for Response {
    fn serialize<Serialization: Serializer>(
        &self,
        serializer: Serialization,
    ) -> Result<Serialization::Ok, Serialization::Error> {
        let mut record = serializer.serialize_struct("Response", 2)?;
        record.serialize_field("id", &self.id)?;
        match &self.outcome {
            Ok(result) => record.serialize_field("result", result)?,
            Err(error) => record.serialize_field("error", error)?,
        }
        record.end()
    }
}

impl<'input> Deserialize<'input> for Response {
    fn deserialize<Deserialization: Deserializer<'input>>(
        deserializer: Deserialization,
    ) -> Result<Self, Deserialization::Error> {
        deserializer.deserialize_struct("Response", &["id", "result", "error"], ResponseVisitor)
    }
}

struct ResponseVisitor;

impl<'input> de::Visitor<'input> for ResponseVisitor {
    type Value = Response;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a request id and exactly one result or structured error")
    }

    fn visit_map<Mapping: de::MapAccess<'input>>(
        self,
        mut mapping: Mapping,
    ) -> Result<Response, Mapping::Error> {
        let mut id = None;
        let mut outcome = None;
        while let Some(field) = mapping.next_key::<String>()? {
            match field.as_str() {
                "id" => {
                    if id.is_some() {
                        return Err(de::Error::duplicate_field("id"));
                    }
                    id = Some(mapping.next_value()?);
                }
                "result" | "error" => {
                    if outcome.is_some() {
                        return Err(de::Error::custom("response contains multiple outcomes"));
                    }
                    outcome = Some(if field == "result" {
                        Ok(mapping.next_value()?)
                    } else {
                        Err(mapping.next_value()?)
                    });
                }
                _ => return Err(de::Error::unknown_field(&field, &["id", "result", "error"])),
            }
        }
        Ok(Response {
            id: id.ok_or_else(|| de::Error::missing_field("id"))?,
            outcome: outcome.ok_or_else(|| de::Error::custom("response has no outcome"))?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Message, Response};
    use serde_json::{Value, json};

    #[test]
    fn null_results_and_structured_failures_remain_distinct() {
        let success = Response::success(7, Value::Null).unwrap();
        let encoded = serde_json::to_string(&success).unwrap();
        assert_eq!(encoded, r#"{"id":7,"result":null}"#);
        let decoded: Response = serde_json::from_str(&encoded).unwrap();
        assert_eq!(decoded.result(), Some(&Value::Null));
        assert!(decoded.error().is_none());

        let failure =
            Response::failure_with_data(8, "conflict", "lease held", json!({"owner":"a"}));
        let encoded = serde_json::to_string(&failure).unwrap();
        let decoded: Response = serde_json::from_str(&encoded).unwrap();
        assert!(decoded.result().is_none());
        let error = decoded.error().unwrap();
        assert_eq!(error.code, "conflict");
        assert_eq!(error.data.as_ref().unwrap()["owner"], "a");
        assert!(matches!(
            serde_json::from_str::<Message>(&encoded).unwrap(),
            Message::Response(_)
        ));
    }

    #[test]
    fn malformed_responses_cannot_be_adopted_as_valid_messages() {
        for source in [
            r#"{"id":1}"#,
            r#"{"id":1,"result":null,"error":{"code":"x","message":"x"}}"#,
            r#"{"id":1,"result":null,"error":null}"#,
            r#"{"id":1,"error":null}"#,
            r#"{"id":1,"id":2,"result":true}"#,
            r#"{"id":1,"result":null,"result":true}"#,
            r#"{"id":1,"error":{"code":"x"}}"#,
            r#"{"id":1,"result":true,"unexpected":false}"#,
            r#"{"id":1,"result":true,"session_id":"a","event":"b","payload":{}}"#,
            r#"{"result":true}"#,
        ] {
            assert!(
                serde_json::from_str::<Response>(source).is_err(),
                "{source}"
            );
            assert!(serde_json::from_str::<Message>(source).is_err(), "{source}");
        }
    }

    #[test]
    fn request_progress_is_distinct_from_responses_and_session_events() {
        let source =
            r#"{"request_id":7,"event":"github.sync.progress","payload":{"phase":"reading"}}"#;
        assert!(matches!(
            serde_json::from_str::<Message>(source).unwrap(),
            Message::RequestEvent(_)
        ));
        assert!(serde_json::from_str::<Response>(source).is_err());
        for source in [
            r#"{"request_id":7,"event":"github.sync.progress","payload":{},"id":7}"#,
            r#"{"request_id":7,"event":"github.sync.progress","payload":{},"session_id":"session"}"#,
            r#"{"request_id":7,"event":"github.sync.progress"}"#,
        ] {
            assert!(serde_json::from_str::<Message>(source).is_err());
        }
    }
}
