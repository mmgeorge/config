mod method;

pub use method::RuntimeMethod;

pub(crate) fn validate_root_control(value: Option<&serde_json::Value>) -> anyhow::Result<()> {
    anyhow::ensure!(
        value.is_none_or(serde_json::Value::is_null),
        "Root turn controls cannot address provider child threads. Use agent.message or agent.interrupt with a Harness child identity"
    );
    Ok(())
}
