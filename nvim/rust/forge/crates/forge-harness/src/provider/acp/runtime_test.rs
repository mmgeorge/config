use anyhow::Result;
use forge_providers::acp::runtime::RuntimeStatus;
use std::collections::BTreeMap;
#[tokio::test]
async fn launch_preserves_selected_claude_executable() -> Result<()> {
    use super::transport::AcpTransport;
    use crate::permissions::acp::AcpPermissionContext;
    use crate::provider::approval::PermissionCoordinator;
    use forge_providers::acp::session::AcpSessionState;
    use serde_json::json;
    use std::sync::Arc;

    let directory = tempfile::tempdir()?;
    let peer = directory.path().join("peer.cjs");
    std::fs::write(
        &peer,
        r#"
const readline = require('node:readline');
readline.createInterface({input: process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  process.stdout.write(JSON.stringify({jsonrpc: '2.0', id: request.id, result: {
    protocolVersion: 1, executable: process.env.CLAUDE_CODE_EXECUTABLE,
    agentInfo: {version: 'adapter-fixture'}
  }}) + '\n');
});
"#,
    )?;
    let executable = directory
        .path()
        .join("selected native CLI.exe")
        .to_string_lossy()
        .into_owned();
    let runtime = RuntimeStatus {
        available: true,
        managed: false,
        adapter_version: None,
        command: vec!["node".into(), peer.to_string_lossy().into_owned()],
        environment: BTreeMap::from([("CLAUDE_CODE_EXECUTABLE".into(), executable.clone())]),
    };
    let handler = Arc::new(AcpSessionState::new(
        crate::provider::ProviderKind::Claude,
        Arc::new(AcpPermissionContext::new(PermissionCoordinator::transient(
            directory.path(),
        )?)),
    ));
    let transport =
        AcpTransport::launch(&runtime, &directory.path().to_string_lossy(), handler, None).await?;
    let initialized = transport
        .request("initialize", json!({"protocolVersion": 1}))
        .await?;
    transport.shutdown().await?;
    assert_eq!(initialized["executable"], executable);
    assert_eq!(initialized["agentInfo"]["version"], "adapter-fixture");
    assert!(
        runtime.provider_version().await.is_err(),
        "missing native executable must report a version failure"
    );
    Ok(())
}
