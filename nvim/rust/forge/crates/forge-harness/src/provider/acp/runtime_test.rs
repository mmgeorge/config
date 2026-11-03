use anyhow::Result;
use forge_providers::acp::runtime::RuntimeStatus;
use std::collections::BTreeMap;

/// Exercise public setup through owned ACP peers without creating sessions or submitting input.
#[tokio::test]
async fn setup_enforces_shared_runtime_requirement_and_preserves_failed_probe() -> Result<()> {
    use super::AcpProvider;
    use crate::provider::{ProviderAdapter, ProviderCatalogRequest, ProviderError, ProviderKind};
    use forge_provider_api::version::{RuntimeProbeFailure, RuntimeVersionError};
    use forge_providers::acp::AcpConnectionError;
    use serde_json::Value;

    let directory = tempfile::tempdir()?;
    let peer = directory.path().join("setup-version-peer.cjs");
    std::fs::write(
        &peer,
        r#"
const fs = require('node:fs');
const readline = require('node:readline');
readline.createInterface({input: process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  fs.appendFileSync(process.argv[3], JSON.stringify(request) + '\n');
  if (request.id === undefined) return;
  if (request.method !== 'initialize') throw new Error('setup dispatched session work');
  const response = process.argv[2] === 'failed'
    ? {error:{code:-32603,message:'fixture initialize probe failure'}}
    : {result:{protocolVersion:1,agentInfo:{version:process.argv[2]},authMethods:[]}};
  process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:request.id,...response}) + '\n');
});
"#,
    )?;
    let node =
        forge_shell::process::executable("node").expect("ACP setup fixture requires Node.js");
    for (kind, minimum, below) in [
        (ProviderKind::Claude, "0.99.1", "0.99.0"),
        (ProviderKind::Antigravity, "1.4.5", "1.4.4"),
    ] {
        for version in [below, minimum, "failed"] {
            let wire = directory.path().join(format!("{kind:?}-{version}.jsonl"));
            let provider = AcpProvider::new(
                kind,
                vec![
                    node.to_string_lossy().into_owned(),
                    peer.to_string_lossy().into_owned(),
                    version.into(),
                    wire.to_string_lossy().into_owned(),
                ],
                crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
                directory.path().to_owned(),
            )?;
            let result = provider
                .setup(ProviderCatalogRequest {
                    execution_owner: None,
                    harness_session_id: "setup-version-gate".into(),
                    workspace: directory.path().to_string_lossy().into_owned(),
                    access: crate::provider::registered_capability(kind)
                        .resolve_access(Default::default()),
                    approval_mode: crate::session::ApprovalMode::Ask,
                    provider_session_id: None,
                })
                .await;
            if version == minimum {
                assert_eq!(result?["adapter_version"], minimum);
            } else {
                let error = result.expect_err("unsupported setup was reported as available");
                let Some(ProviderError::RuntimeVersion(error)) =
                    error.downcast_ref::<ProviderError>()
                else {
                    anyhow::bail!("setup lost its typed version error: {error:#}");
                };
                let requirement = match error {
                    RuntimeVersionError::Unsupported {
                        requirement,
                        detected,
                    } if version == below => {
                        assert_eq!(*detected, semver::Version::parse(below)?);
                        requirement
                    }
                    RuntimeVersionError::Probe {
                        requirement,
                        failure: RuntimeProbeFailure::Request { source },
                    } if version == "failed" => {
                        let native = source
                            .downcast_ref::<AcpConnectionError>()
                            .expect("ACP initialize probe erased its native cause");
                        assert!(
                            matches!(native, AcpConnectionError::Remote { method, .. } if method == "initialize")
                        );
                        assert!(
                            native
                                .to_string()
                                .contains("fixture initialize probe failure")
                        );
                        requirement
                    }
                    error => anyhow::bail!("unexpected setup version classification: {error}"),
                };
                assert_eq!(requirement.provider(), kind);
                assert_eq!(requirement.runtime(), node.as_path());
                assert_eq!(*requirement.minimum(), semver::Version::parse(minimum)?);
            }
            provider.shutdown().await?;
            let frames = std::fs::read_to_string(wire)?
                .lines()
                .map(serde_json::from_str::<Value>)
                .collect::<serde_json::Result<Vec<_>>>()?;
            assert_eq!(
                frames.len(),
                1,
                "setup emitted native work beyond its version handshake"
            );
            assert_eq!(frames[0]["method"], "initialize");
        }
    }
    Ok(())
}

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
