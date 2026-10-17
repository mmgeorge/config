use serde::de::DeserializeOwned;
use serde::Serialize;

/// Decode complete plan JSON with stack growth instead of a total nesting limit.
pub(super) fn from_str<Value: DeserializeOwned>(source: &str) -> Result<Value, serde_json::Error> {
    let mut reader = serde_json::Deserializer::from_str(source);
    reader.disable_recursion_limit();
    let value = Value::deserialize(serde_stacker::Deserializer::new(&mut reader))?;
    reader.end()?;
    Ok(value)
}

/// Decode persisted plan bytes while preserving the JSON parser's UTF-8 diagnostics.
pub(super) fn from_slice<Value: DeserializeOwned>(source: &[u8]) -> Result<Value, serde_json::Error> {
    let mut reader = serde_json::Deserializer::from_slice(source);
    reader.disable_recursion_limit();
    let value = Value::deserialize(serde_stacker::Deserializer::new(&mut reader))?;
    reader.end()?;
    Ok(value)
}

/// Serialize a complete plan without consuming call-stack space per nested branch.
pub(super) fn to_vec<Value: Serialize + ?Sized>(value: &Value) -> Result<Vec<u8>, serde_json::Error> {
    let mut bytes = Vec::new();
    value.serialize(serde_stacker::Serializer::new(&mut serde_json::Serializer::new(&mut bytes)))?;
    Ok(bytes)
}

/// Serialize a persisted plan with readable indentation and stack growth.
pub(super) fn to_vec_pretty<Value: Serialize + ?Sized>(value: &Value) -> Result<Vec<u8>, serde_json::Error> {
    let mut bytes = Vec::new();
    value.serialize(serde_stacker::Serializer::new(&mut serde_json::Serializer::pretty(&mut bytes)))?;
    Ok(bytes)
}

/// Produce editable UTF-8 JSON using the persisted plan's complete representation.
pub(super) fn to_string_pretty<Value: Serialize + ?Sized>(value: &Value) -> Result<String, serde_json::Error> {
    Ok(String::from_utf8(to_vec_pretty(value)?).expect("JSON serialization produces UTF-8"))
}

/// Build the model-facing plan value without a recursive serialization limit.
pub(super) fn to_value<Value: Serialize>(value: Value) -> Result<serde_json::Value, serde_json::Error> {
    value.serialize(serde_stacker::Serializer::new(serde_json::value::Serializer))
}
