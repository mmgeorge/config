use serde::Serialize;
use serde::de::DeserializeOwned;

struct StackSafe<'value, Value: ?Sized>(&'value Value);

impl<Value: Serialize + ?Sized> Serialize for StackSafe<'_, Value> {
    fn serialize<Serializer: serde::Serializer>(
        &self,
        serializer: Serializer,
    ) -> Result<Serializer::Ok, Serializer::Error> {
        self.0.serialize(serde_stacker::Serializer::new(serializer))
    }
}

/// Decode complete JSON with stack growth instead of a total nesting limit.
pub(crate) fn from_str<Value: DeserializeOwned>(source: &str) -> Result<Value, serde_json::Error> {
    let mut reader = serde_json::Deserializer::from_str(source);
    reader.disable_recursion_limit();
    let value = Value::deserialize(serde_stacker::Deserializer::new(&mut reader))?;
    reader.end()?;
    Ok(value)
}

/// Decode persisted bytes while preserving the JSON parser's UTF-8 diagnostics.
pub(crate) fn from_slice<Value: DeserializeOwned>(
    source: &[u8],
) -> Result<Value, serde_json::Error> {
    let mut reader = serde_json::Deserializer::from_slice(source);
    reader.disable_recursion_limit();
    let value = Value::deserialize(serde_stacker::Deserializer::new(&mut reader))?;
    reader.end()?;
    Ok(value)
}

/// Serialize complete JSON without consuming call-stack space per nested branch.
pub(crate) fn to_vec<Value: Serialize + ?Sized>(
    value: &Value,
) -> Result<Vec<u8>, serde_json::Error> {
    let mut bytes = Vec::new();
    value.serialize(serde_stacker::Serializer::new(
        &mut serde_json::Serializer::new(&mut bytes),
    ))?;
    Ok(bytes)
}

/// Serialize persisted JSON with readable indentation and stack growth.
pub(crate) fn to_vec_pretty<Value: Serialize + ?Sized>(
    value: &Value,
) -> Result<Vec<u8>, serde_json::Error> {
    let mut bytes = Vec::new();
    value.serialize(serde_stacker::Serializer::new(
        &mut serde_json::Serializer::pretty(&mut bytes),
    ))?;
    Ok(bytes)
}

/// Produce editable UTF-8 JSON using the complete persisted representation.
pub(crate) fn to_string_pretty<Value: Serialize + ?Sized>(
    value: &Value,
) -> Result<String, serde_json::Error> {
    serde_json::to_string_pretty(&StackSafe(value))
}

pub(crate) fn to_string<Value: Serialize + ?Sized>(
    value: &Value,
) -> Result<String, serde_json::Error> {
    serde_json::to_string(&StackSafe(value))
}

/// Build a JSON value without a recursive serialization limit.
pub(crate) fn to_value<Value: Serialize>(
    value: Value,
) -> Result<serde_json::Value, serde_json::Error> {
    value.serialize(serde_stacker::Serializer::new(
        serde_json::value::Serializer,
    ))
}

/// Decode a retained JSON value with the same stack behavior as persisted bytes.
pub(crate) fn from_value<Value: DeserializeOwned>(
    source: serde_json::Value,
) -> Result<Value, serde_json::Error> {
    Value::deserialize(serde_stacker::Deserializer::new(source))
}
