use agent_client_protocol::{ByteStreams, ConnectionTo, UntypedMessage, UntypedRole};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use tokio::sync::{Mutex, oneshot};
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

type SourceError = Box<dyn std::error::Error + Send + Sync>;

/// Describes an ACP connection failure without depending on host workflow errors.
#[derive(Debug, thiserror::Error)]
pub enum AcpConnectionError {
    /// The launch descriptor does not identify a program or required stream.
    #[error("{0}")]
    Configuration(&'static str),
    /// An owned provider process could not be launched or collected.
    #[error("{operation}: {source}")]
    Process {
        operation: &'static str,
        #[source]
        source: std::io::Error,
    },
    /// The native driver could not establish a connection.
    #[error("{message}: {source}")]
    Startup {
        message: &'static str,
        #[source]
        source: SourceError,
    },
    /// A native message could not be encoded.
    #[error("encode ACP {method}: {source}")]
    Protocol {
        method: String,
        #[source]
        source: SourceError,
    },
    /// The remote provider rejected or failed a request.
    #[error("ACP {method} failed: {source}")]
    Remote {
        method: String,
        #[source]
        source: SourceError,
    },
    /// A bounded non-prompt request did not finish.
    #[error("ACP {method} exceeded {seconds} seconds")]
    Timeout {
        method: String,
        seconds: u64,
        #[source]
        source: tokio::time::error::Elapsed,
    },
    /// The host rejected an incoming callback.
    #[error("{0}")]
    Callback(#[source] SourceError),
}

/// Handles native callbacks while the connection continues reading responses.
#[async_trait]
pub trait AcpHandler: Send + Sync {
    /// Deliver one provider notification to its host.
    async fn notification(&self, method: &str, parameter: Value) -> Result<(), AcpConnectionError>;
    /// Resolve one provider request through the host's policy and tools.
    async fn request(&self, method: &str, parameter: Value) -> Result<Value, AcpConnectionError>;
}

/// Supplies resolved launch settings without owning host configuration or permissions.
pub struct AcpLaunch<'a> {
    pub command: &'a [String],
    pub environment: &'a BTreeMap<String, String>,
    pub workspace: &'a str,
    pub profile: Option<&'a Path>,
}

/// Owns the ACP driver and the provider's process tree.
pub struct AcpConnection {
    connection: ConnectionTo<UntypedRole>,
    process: Mutex<Box<dyn process_wrap::tokio::ChildWrapper>>,
    driver: tokio::task::JoinHandle<Result<(), agent_client_protocol::Error>>,
}

impl AcpConnection {
    /// Launch the resolved provider and wait for its native connection.
    pub async fn launch(
        launch: AcpLaunch<'_>,
        handler: Arc<dyn AcpHandler>,
    ) -> Result<Self, AcpConnectionError> {
        let program = launch
            .command
            .first()
            .ok_or(AcpConnectionError::Configuration(
                "ACP provider command is empty",
            ))?;
        let mut command = forge_shell::process::command(program, &launch.command[1..]);
        command.envs(launch.environment);
        if let Some(profile) = launch.profile {
            command.env("GEMINI_HOME", profile);
        }
        command
            .current_dir(launch.workspace)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null());
        let mut process = forge_shell::process::spawn_owned(command).map_err(|source| {
            AcpConnectionError::Process {
                operation: "launch ACP provider",
                source,
            }
        })?;
        let outgoing = process
            .stdin()
            .take()
            .ok_or(AcpConnectionError::Configuration("ACP stdin missing"))?;
        let incoming = process
            .stdout()
            .take()
            .ok_or(AcpConnectionError::Configuration("ACP stdout missing"))?;
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
        let connection = match tokio::time::timeout(std::time::Duration::from_secs(10), ready).await
        {
            Ok(Ok(connection)) => connection,
            Ok(Err(source)) => {
                driver.abort();
                return Err(AcpConnectionError::Startup {
                    message: "ACP driver exited before connection startup",
                    source: Box::new(source),
                });
            }
            Err(source) => {
                driver.abort();
                return Err(AcpConnectionError::Startup {
                    message: "ACP connection startup exceeded 10 seconds",
                    source: Box::new(source),
                });
            }
        };
        Ok(Self {
            connection,
            process: Mutex::new(process),
            driver,
        })
    }

    /// Send a native request; interactive prompts remain unbounded.
    pub async fn request(
        &self,
        method: &str,
        parameter: Value,
    ) -> Result<Value, AcpConnectionError> {
        let message = UntypedMessage::new(method, parameter).map_err(|source| {
            AcpConnectionError::Protocol {
                method: method.into(),
                source: Box::new(source),
            }
        })?;
        let request = self.connection.send_request(message).block_task();
        let response = if method == "session/prompt" {
            request.await
        } else {
            let seconds = if method == "authenticate" { 120 } else { 30 };
            tokio::time::timeout(std::time::Duration::from_secs(seconds), request)
                .await
                .map_err(|source| AcpConnectionError::Timeout {
                    method: method.into(),
                    seconds,
                    source,
                })?
        };
        response.map_err(|source| AcpConnectionError::Remote {
            method: method.into(),
            source: Box::new(source),
        })
    }

    /// Collect the process tree and stop the native driver.
    pub async fn shutdown(&self) -> Result<(), AcpConnectionError> {
        let mut process = self.process.lock().await;
        Box::into_pin(process.kill())
            .await
            .map_err(|source| AcpConnectionError::Process {
                operation: "collect ACP process tree",
                source,
            })?;
        self.driver.abort();
        Ok(())
    }
}

impl Drop for AcpConnection {
    fn drop(&mut self) {
        self.driver.abort();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct UnexpectedCallback;

    #[async_trait]
    impl AcpHandler for UnexpectedCallback {
        async fn notification(&self, _: &str, _: Value) -> Result<(), AcpConnectionError> {
            panic!("empty command must not launch a provider")
        }

        async fn request(&self, _: &str, _: Value) -> Result<Value, AcpConnectionError> {
            panic!("empty command must not launch a provider")
        }
    }

    #[tokio::test]
    async fn missing_command_fails_before_launch() {
        let environment = BTreeMap::new();
        let launch = AcpLaunch {
            command: &[],
            environment: &environment,
            workspace: ".",
            profile: None,
        };
        let result = AcpConnection::launch(launch, Arc::new(UnexpectedCallback)).await;
        assert!(matches!(
            result,
            Err(AcpConnectionError::Configuration(
                "ACP provider command is empty"
            ))
        ));
    }
}
