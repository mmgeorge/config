use anyhow::{Context, Result};
use forge_provider_api::generation::{ReasoningSelection, TextGenerationRequest, TextValidator};
use github_copilot_sdk::{
    CliProgram, Client, ClientOptions, MessageOptions, SessionConfig, SystemMessageConfig,
};
use std::{sync::Arc, time::Duration};

/// Keep temporary runtime collection alive when its generation caller is cancelled.
struct IsolatedClient {
    client: Option<Client>,
    collection: Option<tokio::task::JoinHandle<Result<()>>>,
}

impl IsolatedClient {
    /// Get the live transport before ownership transfers into its collection task.
    fn client(&self) -> &Client {
        self.client
            .as_ref()
            .expect("isolated client has not begun collection")
    }

    /// Transfer native ownership into one bounded task before awaiting its result.
    fn begin_collection(&mut self) {
        if self.collection.is_none() {
            let client = self
                .client
                .take()
                .expect("isolated client collection starts once");
            self.collection = Some(tokio::spawn(async move {
                let result = tokio::time::timeout(Duration::from_secs(8), client.stop())
                    .await
                    .context("isolated Copilot SDK shutdown exceeded 8 seconds")
                    .and_then(|result| result.context("stop isolated Copilot SDK client"));
                if result.is_err() {
                    client.force_stop();
                }
                result
            }));
        }
    }

    /// Join native collection without aborting it when this waiter disappears.
    async fn collect(mut self) -> Result<()> {
        self.begin_collection();
        self.collection
            .as_mut()
            .expect("isolated collection task was installed")
            .await
            .context("isolated Copilot collection task failed")?
    }
}

impl Drop for IsolatedClient {
    /// Retain collection on cancellation rather than abandoning a live temporary runtime.
    fn drop(&mut self) {
        if self.client.is_some() {
            self.begin_collection();
        }
    }
}

/// Summarize captured history without attaching to or sending input to the main session.
pub async fn generate(
    command: &[String],
    workspace: &str,
    request: &TextGenerationRequest,
    validator: &dyn TextValidator,
) -> Result<String> {
    let mut options = ClientOptions::new().with_cwd(workspace);
    if let Some(program) = command.first() {
        options = options
            .with_program(CliProgram::Path(program.into()))
            .with_prefix_args(command.iter().skip(1));
    }
    let client = tokio::time::timeout(Duration::from_secs(20), Client::start(options))
        .await
        .context("isolated Copilot SDK startup exceeded 20 seconds")?
        .context("start isolated Copilot SDK client")?;
    complete_generation(client, command, workspace, request, validator).await
}

/// Apply the same native version gate to isolated generation before any session work.
async fn complete_generation(
    client: Client,
    command: &[String],
    workspace: &str,
    request: &TextGenerationRequest,
    validator: &dyn TextValidator,
) -> Result<String> {
    let client = IsolatedClient {
        client: Some(client),
        collection: None,
    };
    let result = async {
        super::runtime::validate_version(client.client(), command).await?;
        summarize(client.client(), workspace, request, validator).await
    }
    .await;
    let shutdown = client.collect().await;
    match (result, shutdown) {
        (Err(error), Err(shutdown)) => Err(forge_provider_api::ProviderError::Combined {
            primary: Box::new(forge_provider_api::ProviderError::from(error)),
            secondary: vec![forge_provider_api::ProviderError::from(shutdown)],
        }
        .into()),
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Ok(text), Ok(())) => Ok(text),
    }
}

/// Own the temporary session and remove its stored state before the isolated client stops.
async fn summarize(
    client: &Client,
    workspace: &str,
    request: &TextGenerationRequest,
    validator: &dyn TextValidator,
) -> Result<String> {
    let model = request.model.as_str();
    let mut config = SessionConfig::default();
    config.model = (model != "default").then(|| model.to_owned());
    if let ReasoningSelection::Level(effort) = &request.reasoning {
        config.reasoning_effort = Some(effort.to_owned());
    } else if matches!(request.reasoning, ReasoningSelection::Lowest) {
        let catalog = tokio::time::timeout(Duration::from_secs(20), client.list_models())
            .await
            .context("isolated Copilot model discovery exceeded 20 seconds")?
            .context("list isolated Copilot models")?;
        let descriptor = catalog
            .into_iter()
            .map(super::catalog::model_descriptor)
            .find(|entry| entry.id == model)
            .with_context(|| {
                format!("selected model {model} is absent from the Copilot model catalog")
            })?;
        config.reasoning_effort = forge_provider_api::generation::lowest_effort(&descriptor)?;
    }
    config.streaming = Some(true);
    config.working_directory = Some(workspace.into());
    config.available_tools = Some(Vec::new());
    config.tools = Some(Vec::new());
    config.enable_config_discovery = Some(false);
    config.enable_skills = Some(false);
    config.enable_file_hooks = Some(false);
    config.hooks = Some(false);
    config.enable_session_store = Some(false);
    config.skip_custom_instructions = Some(true);
    config.system_message = Some(
        SystemMessageConfig::new()
            .with_mode("replace")
            .with_content(request.instructions.clone()),
    );
    let session = tokio::time::timeout(
        Duration::from_secs(20),
        client.create_session(
            config.with_permission_handler(Arc::new(github_copilot_sdk::handler::DenyAllHandler)),
        ),
    )
    .await
    .context("isolated Copilot session creation exceeded 20 seconds")?
    .context("create isolated Copilot text-generation session")?;
    let result = tokio::time::timeout(
        Duration::from_secs(60),
        session.send_and_wait(MessageOptions::new(request.input.clone())),
    )
    .await
    .with_context(|| {
        format!(
            "Copilot session {} text generation exceeded 60 seconds",
            session.id()
        )
    })
    .and_then(|result| result.context("send isolated Copilot text-generation prompt"))
    .and_then(|event| {
        event.with_context(|| {
            format!(
                "Copilot session {} completed text generation without an assistant message",
                session.id()
            )
        })
    })
    .and_then(|event| {
        let content = event
            .data
            .get("content")
            .and_then(serde_json::Value::as_str)
            .with_context(|| {
                format!(
                    "Copilot session {} text-generation event {} omitted string content",
                    session.id(),
                    event.id
                )
            })?;
        validator.validate(content).with_context(|| {
            format!(
                "validate Copilot session {} text-generation event {}",
                session.id(),
                event.id
            )
        })
    });
    let cleanup = tokio::time::timeout(Duration::from_secs(8), async {
        let mut failures = Vec::new();
        if result.is_err()
            && let Err(error) = session.abort().await
        {
            failures.push(format!("abort: {error}"));
        }
        if let Err(error) = session.disconnect().await {
            failures.push(format!("disconnect: {error}"));
        }
        if let Err(error) = client.delete_session(session.id()).await {
            failures.push(format!("delete: {error}"));
        }
        anyhow::ensure!(
            failures.is_empty(),
            "Copilot session {} cleanup failed: {}",
            session.id(),
            failures.join(", ")
        );
        Ok(())
    })
    .await
    .with_context(|| {
        format!(
            "Copilot session {} cleanup exceeded 8 seconds",
            session.id()
        )
    })
    .and_then(|result| result);
    match (result, cleanup) {
        (Err(error), Err(cleanup)) => {
            Err(error.context(format!("Copilot session cleanup also failed: {cleanup:#}")))
        }
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Ok(text), Ok(())) => Ok(text),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{ProviderError, version::RuntimeVersionError};
    use serde_json::{Value, json};
    use std::sync::Mutex;
    use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

    #[derive(Clone, Copy)]
    enum Completion {
        Success,
        Failure,
        Cancel,
    }

    struct Validator;

    impl TextValidator for Validator {
        /// Keep text validation separate from SDK acknowledgment and completion.
        fn validate(&self, text: &str) -> Result<String, ProviderError> {
            Ok(text.to_owned())
        }
    }

    /// Connect the production isolated operation to a real SDK stream and private RPC recorder.
    fn fixture(
        version: &'static str,
        completion: Completion,
    ) -> (
        Client,
        Arc<Mutex<Vec<String>>>,
        tokio::sync::oneshot::Receiver<()>,
        tokio::task::JoinHandle<()>,
    ) {
        let (client_write, mut peer_read) = tokio::io::duplex(64 * 1024);
        let (mut peer_write, client_read) = tokio::io::duplex(64 * 1024);
        let client = Client::from_streams(client_read, client_write, std::env::temp_dir()).unwrap();
        let methods = Arc::new(Mutex::new(Vec::new()));
        let observed = methods.clone();
        let (sent, sending) = tokio::sync::oneshot::channel();
        let peer = tokio::spawn(async move {
            let mut sent = Some(sent);
            let mut session = String::new();
            let mut held_request = None;
            while let Some(request) = read_frame(&mut peer_read).await {
                let method = request["method"].as_str().unwrap();
                observed.lock().unwrap().push(method.to_owned());
                let result = match method {
                    "status.get" => json!({"version":version}),
                    "session.create" => {
                        assert_eq!(version, "1.0.103", "isolated session bypassed version gate");
                        session = request["params"]["sessionId"].as_str().unwrap().to_owned();
                        json!({"sessionId":session,"workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" => json!({"success":true}),
                    "session.sendAndWait" if matches!(completion, Completion::Cancel) => {
                        held_request = Some(request["id"].clone());
                        if let Some(sent) = sent.take() {
                            let _ = sent.send(());
                        }
                        continue;
                    }
                    "$/cancelRequest" => {
                        assert!(matches!(completion, Completion::Cancel));
                        assert_eq!(Some(&request["params"]["id"]), held_request.as_ref());
                        assert!(
                            request.get("id").is_none(),
                            "RPC cancellation must be a notification"
                        );
                        continue;
                    }
                    "session.sendAndWait" if matches!(completion, Completion::Failure) => {
                        write_frame(&mut peer_write, json!({"jsonrpc":"2.0","id":request["id"],"error":{"code":-32000,"message":"isolated send failed"}})).await;
                        continue;
                    }
                    "session.sendAndWait" => {
                        json!({"aborted":false,"messageId":"isolated-input","completionEventId":"isolated-idle","response":{
                            "id":"isolated-answer","type":"assistant.message","timestamp":"2026-11-02T00:00:00Z",
                            "data":{"messageId":"answer","originatingMessageId":"isolated-input","content":"Complete isolated text"}
                        }})
                    }
                    "session.abort" | "session.detach" => json!({"success":true}),
                    "session.delete" => json!({}),
                    other => panic!("unexpected isolated SDK request: {other}"),
                };
                write_frame(
                    &mut peer_write,
                    json!({"jsonrpc":"2.0","id":request["id"],"result":result}),
                )
                .await;
                if method == "session.sendAndWait" {
                    if let Some(sent) = sent.take() {
                        let _ = sent.send(());
                    }
                    if matches!(completion, Completion::Success) {
                        write_frame(&mut peer_write, json!({"jsonrpc":"2.0","method":"session.event","params":{"sessionId":session,"event":{
                            "id":"isolated-idle","type":"session.idle","timestamp":"2026-11-02T00:00:00Z","ephemeral":true,"data":{}
                        }}})).await;
                    }
                }
            }
        });
        (client, methods, sending, peer)
    }

    /// Read a bounded SDK protocol frame while preserving clean client collection as EOF.
    async fn read_frame(reader: &mut DuplexStream) -> Option<Value> {
        let mut header = Vec::new();
        loop {
            match reader.read_u8().await {
                Ok(byte) => header.push(byte),
                Err(error)
                    if header.is_empty() && error.kind() == std::io::ErrorKind::UnexpectedEof =>
                {
                    return None;
                }
                Err(error) => panic!("isolated peer header failed: {error}"),
            }
            if header.ends_with(b"\r\n\r\n") {
                break;
            }
            assert!(header.len() < 1024);
        }
        let size: usize = std::str::from_utf8(&header)
            .unwrap()
            .trim()
            .strip_prefix("Content-Length: ")
            .unwrap()
            .parse()
            .unwrap();
        assert!(size < 64 * 1024);
        let mut body = vec![0; size];
        reader.read_exact(&mut body).await.unwrap();
        Some(serde_json::from_slice(&body).unwrap())
    }

    /// Write native responses and notifications on the same ordered SDK connection.
    async fn write_frame(writer: &mut DuplexStream, frame: Value) {
        let body = serde_json::to_vec(&frame).unwrap();
        writer
            .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
            .await
            .unwrap();
        writer.write_all(&body).await.unwrap();
        writer.flush().await.unwrap();
    }

    /// Exercise generation with explicit model selection without unrelated catalog RPCs.
    fn request() -> TextGenerationRequest {
        TextGenerationRequest {
            model: "fixture-model".into(),
            reasoning: ReasoningSelection::Level("low".into()),
            instructions: "summarize".into(),
            input: "inspect".into(),
        }
    }

    /// Reject an actual isolated SDK client before create or send and still collect it.
    #[tokio::test]
    async fn isolated_generation_rejects_old_runtime_before_session_creation() {
        tokio::time::timeout(Duration::from_secs(5), async {
            let (client, methods, _, peer) = fixture("1.0.102", Completion::Success);
            let error = complete_generation(client, &[], ".", &request(), &Validator)
                .await
                .unwrap_err();
            assert!(matches!(
                error.downcast_ref::<ProviderError>(),
                Some(ProviderError::RuntimeVersion(
                    RuntimeVersionError::Unsupported { .. }
                ))
            ));
            peer.await.unwrap();
            assert_eq!(*methods.lock().unwrap(), ["status.get"]);
        })
        .await
        .expect("isolated rejection failed to collect its SDK transport");
    }

    /// Collect the temporary SDK client on completed, failed, and cancelled caller paths.
    #[tokio::test]
    async fn isolated_generation_collects_success_failure_and_cancelled_callers() {
        tokio::time::timeout(Duration::from_secs(10), async {
            for completion in [Completion::Success, Completion::Failure, Completion::Cancel] {
                let (client, methods, sending, peer) = fixture("1.0.103", completion);
                let generation = tokio::spawn(async move {
                    complete_generation(client, &[], ".", &request(), &Validator).await
                });
                match completion {
                    Completion::Success => {
                        assert_eq!(generation.await.unwrap().unwrap(), "Complete isolated text")
                    }
                    Completion::Failure => assert!(
                        format!("{:#}", generation.await.unwrap().unwrap_err())
                            .contains("isolated send failed")
                    ),
                    Completion::Cancel => {
                        sending.await.unwrap();
                        generation.abort();
                        assert!(generation.await.unwrap_err().is_cancelled());
                    }
                }
                peer.await.unwrap();
                let methods = methods.lock().unwrap();
                assert_eq!(methods.first().map(String::as_str), Some("status.get"));
                assert_eq!(
                    methods
                        .iter()
                        .filter(|method| method.as_str() == "session.create")
                        .count(),
                    1
                );
                assert_eq!(
                    methods
                        .iter()
                        .filter(|method| method.as_str() == "session.sendAndWait")
                        .count(),
                    1
                );
                if !matches!(completion, Completion::Cancel) {
                    assert!(methods.iter().any(|method| method == "session.detach"));
                    assert!(methods.iter().any(|method| method == "session.delete"));
                }
            }
        })
        .await
        .expect("isolated outcome failed to collect its SDK transport");
    }
}
