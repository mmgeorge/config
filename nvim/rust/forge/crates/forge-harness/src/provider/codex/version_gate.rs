//! Probe the real provider initialization gate through an owned fake app-server process.

use super::*;
use forge_provider_api::version::RuntimeVersionError;
use futures_util::{SinkExt, StreamExt};
use std::io::Write;
use std::path::PathBuf;
use tokio_tungstenite::tungstenite::Message;

const LAUNCHER: &str = r#"def main [test_bin: string, config: string, --listen: string] {
    with-env { FORGE_CODEX_VERSION_ENDPOINT: $listen, FORGE_CODEX_VERSION_CONFIG: $config } {
        run-external $test_bin '--exact' 'provider::codex::version_gate::app_server_fixture' '--ignored' '--nocapture'
        exit $env.LAST_EXIT_CODE
    }
}
"#;

enum Reply {
    Agent(&'static str),
    Missing,
    Failed,
}

enum Expected {
    Supported,
    Unsupported,
    Missing,
    MalformedAgent,
    Failed,
}

/// Retain the same configured command while replacing its real subprocess and native reply.
struct GateFixture {
    root: tempfile::TempDir,
    command: Vec<String>,
    config: PathBuf,
    wire: PathBuf,
}

impl GateFixture {
    /// Create a cross-platform adapter which forwards the runtime's selected listen endpoint.
    fn new() -> Result<Self> {
        let root = tempfile::tempdir()?;
        let script = root.path().join("version-peer.nu");
        std::fs::write(&script, LAUNCHER)?;
        let status = std::process::Command::new("nu")
            .args(["--no-config-file", "-c", "if not (nu-check $env.FORGE_CODEX_VERSION_SCRIPT) { error make {msg: 'invalid version fixture launcher'} }"])
            .env("FORGE_CODEX_VERSION_SCRIPT", &script)
            .status()?;
        anyhow::ensure!(status.success(), "Nushell version fixture did not validate");
        let config = root.path().join("peer-config.json");
        let wire = root.path().join("wire.jsonl");
        let command = vec![
            "nu".into(),
            "--no-config-file".into(),
            script.to_string_lossy().into_owned(),
            std::env::current_exe()?.to_string_lossy().into_owned(),
            config.to_string_lossy().into_owned(),
        ];
        Ok(Self {
            root,
            command,
            config,
            wire,
        })
    }

    /// Select the actual process's initialization response and clear only this fixture's log.
    fn configure(&self, reply: Reply) -> Result<()> {
        let reply = match reply {
            Reply::Agent(agent) => json!({"result":{"userAgent":agent}}),
            Reply::Missing => json!({"result":{}}),
            Reply::Failed => json!({"error":{"code":-32000,"message":"fixture initialize failed"}}),
        };
        std::fs::write(
            &self.config,
            serde_json::to_vec(&json!({"initialize":reply,"wire":self.wire}))?,
        )?;
        std::fs::write(&self.wire, "")?;
        Ok(())
    }

    /// Run the public initialization path and retain typed version errors and native method order.
    async fn probe(&self, expected: Expected, resume: bool) -> Result<Vec<String>> {
        let workspace = self.root.path().to_string_lossy().into_owned();
        let provider = CodexProvider::new_with_permission_coordinator(
            self.command.clone(),
            PermissionCoordinator::transient(&workspace)?,
            Arc::new(TraceStore::open(self.root.path())?),
            None,
        )?;
        let mut request = CodexProvider::catalog_provider_request(&ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "version-gate".into(),
            workspace,
            provider_session_id: if resume {
                Some("retained-thread".to_owned().try_into()?)
            } else {
                None
            },
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
        });
        request.model = "fixture-model".into();
        request.service_tier = crate::provider::ServiceTier::Standard;
        let initialized = provider.initialize_session(request.clone()).await;
        match expected {
            Expected::Supported => {
                assert_eq!(initialized?, Some("retained-thread".to_owned().try_into()?));
                assert_eq!(
                    provider.initialize_session(request).await?,
                    Some("retained-thread".to_owned().try_into()?)
                );
            }
            Expected::Unsupported => {
                let error = initialized.expect_err("below-minimum runtime initialized");
                assert!(
                    matches!(&error, crate::provider::ProviderError::RuntimeVersion(RuntimeVersionError::Unsupported { detected, .. }) if detected == &semver::Version::new(0, 163, 9))
                );
                assert!(error.to_string().contains("0.164.0"));
            }
            Expected::Missing => {
                let error = initialized.expect_err("unusable native version initialized");
                assert!(matches!(
                    error,
                    crate::provider::ProviderError::RuntimeVersion(
                        RuntimeVersionError::Missing { .. }
                    )
                ));
            }
            Expected::MalformedAgent => {
                let error = initialized.expect_err("malformed native version initialized");
                assert!(
                    matches!(&error, crate::provider::ProviderError::RuntimeVersion(RuntimeVersionError::Malformed { reported, .. }) if reported == "not-a-version")
                );
            }
            Expected::Failed => {
                let error = initialized.expect_err("failed initialize response was accepted");
                let crate::provider::ProviderError::RuntimeVersion(RuntimeVersionError::Probe {
                    requirement,
                    failure,
                }) = &error
                else {
                    anyhow::bail!("failed initialize lost its runtime requirement: {error:#}");
                };
                assert_eq!(requirement.provider(), crate::provider::ProviderKind::Codex);
                assert_eq!(requirement.minimum(), &semver::Version::new(0, 164, 0));
                assert!(matches!(
                    failure,
                    forge_provider_api::version::RuntimeProbeFailure::Request { .. }
                ));
                use std::error::Error;
                assert!(
                    failure.source().is_some(),
                    "failed version probe lost its source"
                );
                anyhow::ensure!(
                    format!("{error:#}").contains("fixture initialize failed"),
                    "initialize failure lost its native cause: {error:#}"
                );
            }
        };
        provider.shutdown().await?;
        assert_eq!(
            provider.app_server_start_count(),
            1,
            "fixture unexpectedly restarted its runtime"
        );
        let methods = std::fs::read_to_string(&self.wire)?
            .lines()
            .map(|line| {
                serde_json::from_str::<Value>(line)
                    .map(|frame| frame["method"].as_str().unwrap().to_owned())
            })
            .collect::<serde_json::Result<Vec<_>>>()?;
        if matches!(expected, Expected::Supported) {
            assert_eq!(
                methods,
                [
                    "initialize",
                    "initialized",
                    if resume {
                        "thread/resume"
                    } else {
                        "thread/start"
                    }
                ]
            );
        } else {
            assert_eq!(
                methods,
                ["initialize"],
                "version rejection admitted native thread or turn work"
            );
        }
        Ok(methods)
    }
}

/// Act as an isolated app-server child launched only by the version-gate fixture.
#[tokio::test]
#[ignore = "owned subprocess entry point for the Codex version gate"]
async fn app_server_fixture() -> Result<()> {
    let endpoint = std::env::var("FORGE_CODEX_VERSION_ENDPOINT")?;
    let config: Value = serde_json::from_slice(&std::fs::read(
        std::env::var_os("FORGE_CODEX_VERSION_CONFIG").context("missing fixture config")?,
    )?)?;
    let listener = tokio::net::TcpListener::bind(
        endpoint
            .strip_prefix("ws://")
            .context("invalid fixture endpoint")?,
    )
    .await?;
    let (stream, _) = listener.accept().await?;
    let mut socket = tokio_tungstenite::accept_async(stream).await?;
    while let Some(message) = socket.next().await {
        let message = message?;
        if message.is_close() {
            break;
        }
        let request: Value = serde_json::from_str(message.to_text()?)?;
        let mut wire = std::fs::OpenOptions::new()
            .append(true)
            .open(config["wire"].as_str().context("wire path missing")?)?;
        writeln!(wire, "{}", serde_json::to_string(&request)?)?;
        wire.flush()?;
        let response = match request["method"].as_str() {
            Some("initialize") => config["initialize"].clone(),
            Some("initialized") => continue,
            Some("thread/start" | "thread/resume") => {
                json!({"result":{"thread":{"id":"retained-thread"}}})
            }
            method => anyhow::bail!("unexpected native work during version probe: {method:?}"),
        };
        let mut response = response;
        response["id"] = request["id"].clone();
        socket
            .send(Message::Text(response.to_string().into()))
            .await?;
    }
    Ok(())
}

/// Reject unsupported and unusable native versions before both new and resumed thread work.
#[tokio::test]
async fn initialization_gates_native_thread_start_and_resume() -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(60), async {
        let fixture = GateFixture::new()?;
        for resume in [false, true] {
            for (reply, expected) in [
                (
                    Reply::Agent("codex_cli_rs/0.163.9 (Windows 10.0.26300)"),
                    Expected::Unsupported,
                ),
                (Reply::Agent("codex/0.164.0 (fixture)"), Expected::Supported),
                (Reply::Agent("codex/0.165.0 (fixture)"), Expected::Supported),
                (Reply::Missing, Expected::Missing),
                (
                    Reply::Agent("codex/not-a-version (fixture)"),
                    Expected::MalformedAgent,
                ),
                (
                    Reply::Agent("codex_cli_rs/not-a-version (Windows 10.0.26300)"),
                    Expected::MalformedAgent,
                ),
                (Reply::Agent("0.164.0"), Expected::Supported),
                (Reply::Failed, Expected::Failed),
            ] {
                fixture.configure(reply)?;
                fixture.probe(expected, resume).await?;
            }
        }
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Re-probe a replacement process even when the configured executable and profile are unchanged.
#[tokio::test]
async fn replacement_process_cannot_reuse_previous_version_validation() -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        let fixture = GateFixture::new()?;
        fixture.configure(Reply::Agent("codex/0.164.0 (fixture)"))?;
        fixture.probe(Expected::Supported, false).await?;
        fixture.configure(Reply::Agent("codex/0.163.9 (fixture)"))?;
        fixture.probe(Expected::Unsupported, true).await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}
