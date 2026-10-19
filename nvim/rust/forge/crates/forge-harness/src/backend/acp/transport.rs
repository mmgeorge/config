use agent_client_protocol::{ByteStreams, ConnectionTo, UntypedMessage, UntypedRole};
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::{Mutex, oneshot};
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

#[async_trait]
/// Handles provider callbacks independently of pending outgoing requests.
pub(super) trait AcpHandler: Send + Sync {
    /// Normalize an incoming notification without echoing replayed history.
    async fn notification(&self, method: &str, parameter: Value) -> Result<()>;
    /// Resolve an incoming request through Forge-owned policy and controls.
    async fn request(&self, method: &str, parameter: Value) -> Result<Value>;
}

/// Owns one SDK connection and its entire provider process tree.
pub(super) struct AcpTransport {
    connection: ConnectionTo<UntypedRole>,
    process: Mutex<Box<dyn process_wrap::tokio::ChildWrapper>>,
    driver: tokio::task::JoinHandle<Result<(), agent_client_protocol::Error>>,
}

impl AcpTransport {
    /// Launch a provider with hidden, collected descendants and stable ACP v1 framing.
    pub(super) async fn launch(
        command: &[String],
        workspace: &str,
        handler: Arc<dyn AcpHandler>,
        profile: Option<&std::path::Path>,
    ) -> Result<Self> {
        let program = command.first().context("ACP provider command is empty")?;
        let mut launch = crate::process::command(program, &command[1..]);
        if let Some(profile) = profile {
            launch.env("GEMINI_HOME", profile);
        }
        launch
            .current_dir(workspace)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null());
        let mut process = crate::process::spawn_owned(launch).context("launch ACP provider")?;
        let outgoing = process.stdin().take().context("ACP stdin missing")?;
        let incoming = process.stdout().take().context("ACP stdout missing")?;
        let transport = ByteStreams::new(outgoing.compat_write(), incoming.compat());
        let (connected, ready) = oneshot::channel();
        let notification_handler = Arc::clone(&handler);
        let driver = tokio::spawn(async move {
            UntypedRole
                .builder()
                .name("forge-harness")
                .on_receive_notification(
                    async move |notification: UntypedMessage, _context| {
                        notification_handler
                            .notification(notification.method(), notification.params().clone())
                            .await
                            .map_err(|error| {
                                agent_client_protocol::util::internal_error(format!("{error:#}"))
                            })
                    },
                    agent_client_protocol::on_receive_notification!(),
                )
                .on_receive_request(
                    async move |request: UntypedMessage, responder, context| {
                        let handler = Arc::clone(&handler);
                        context.spawn(async move {
                            let response = handler
                                .request(request.method(), request.params().clone())
                                .await
                                .map_err(|error| {
                                    agent_client_protocol::util::internal_error(format!(
                                        "{error:#}"
                                    ))
                                });
                            match response {
                                Ok(value) => responder.respond(value),
                                Err(error) => responder.respond_with_error(error),
                            }
                        })
                    },
                    agent_client_protocol::on_receive_request!(),
                )
                .connect_with(transport, async move |context| {
                    let _ = connected.send(context.clone());
                    context.incoming_closed().await;
                    Ok(())
                })
                .await
        });
        let connection = tokio::time::timeout(std::time::Duration::from_secs(10), ready)
            .await
            .context("ACP connection startup exceeded 10 seconds")?
            .context("ACP driver exited before connection startup")?;
        Ok(Self {
            connection,
            process: Mutex::new(process),
            driver,
        })
    }

    /// Send a request through the SDK, preserving protocol errors as failures.
    pub(super) async fn request(&self, method: &str, parameter: Value) -> Result<Value> {
        let request = self
            .connection
            .send_request(UntypedMessage::new(method, parameter)?)
            .block_task();
        if method == "session/prompt" {
            Ok(request.await?)
        } else {
            let seconds = if method == "authenticate" { 120 } else { 30 };
            Ok(
                tokio::time::timeout(std::time::Duration::from_secs(seconds), request)
                    .await
                    .with_context(|| format!("ACP {method} exceeded {seconds} seconds"))??,
            )
        }
    }

    /// Kill and reap all provider descendants before acknowledging cleanup.
    pub(super) async fn shutdown(&self) -> Result<()> {
        let mut process = self.process.lock().await;
        Box::into_pin(process.kill())
            .await
            .context("collect ACP process tree")?;
        self.driver.abort();
        Ok(())
    }
}

impl Drop for AcpTransport {
    fn drop(&mut self) {
        self.driver.abort();
    }
}
