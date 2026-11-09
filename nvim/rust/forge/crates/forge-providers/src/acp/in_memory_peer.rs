//! Serve ACP provider commands from in-process peers so tests need no external executable.
//!
//! A test registers a peer under a unique executable file name. `AcpConnection::launch_retained`
//! consults this registry before spawning, so the production
//! resolver, cleanup graph and newline-delimited JSON-RPC framing all run unchanged. Only the
//! operating-system process is replaced: the connection receives duplex streams plus an inert
//! child handle whose collection always succeeds.

use serde_json::{Value, json};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, LazyLock, Mutex};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, DuplexStream};

/// Reply to one native message with zero or more messages for the client.
pub(super) type PeerHandler = Box<dyn FnMut(&Value) -> Vec<Value> + Send>;

/// Create the handler state for one connection, so each launch starts a fresh native session.
pub(super) type PeerHandlerFactory = Box<dyn Fn() -> PeerHandler + Send + Sync>;

struct PeerDefinition {
    requests: Arc<Mutex<Vec<Value>>>,
    handler_factory: PeerHandlerFactory,
}

static REGISTRY: LazyLock<Mutex<HashMap<String, Arc<PeerDefinition>>>> =
    LazyLock::new(Default::default);

/// Keep a peer registered, and its fake executable on disk, for the lifetime of a fixture.
pub(super) struct InMemoryPeer {
    program: String,
    registry_key: String,
    definition: Arc<PeerDefinition>,
}

impl InMemoryPeer {
    /// Register a peer behind a placeholder executable inside `directory`.
    ///
    /// The placeholder lets `forge_shell::process::executable` resolve the program exactly as it
    /// resolves a real adapter. It is never executed.
    pub(super) fn register(directory: &Path, handler_factory: PeerHandlerFactory) -> Self {
        let registry_key = format!("acp-in-memory-peer-{}.exe", uuid::Uuid::new_v4());
        let program = directory.join(&registry_key);
        std::fs::write(&program, b"").expect("write placeholder peer executable");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&program, std::fs::Permissions::from_mode(0o755))
                .expect("mark placeholder peer executable");
        }
        let definition = Arc::new(PeerDefinition {
            requests: Arc::default(),
            handler_factory,
        });
        REGISTRY
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(registry_key.clone(), definition.clone());
        Self {
            program: program.to_string_lossy().into_owned(),
            registry_key,
            definition,
        }
    }

    /// Get the command program that selects this peer.
    pub(super) fn program(&self) -> String {
        self.program.clone()
    }

    /// Get every native message the client has sent, across all connections, in arrival order.
    pub(super) fn requests(&self) -> Vec<Value> {
        self.definition
            .requests
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }
}

impl Drop for InMemoryPeer {
    fn drop(&mut self) {
        REGISTRY
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(&self.registry_key);
    }
}

fn lookup(program: &str) -> Option<Arc<PeerDefinition>> {
    let name = Path::new(program).file_name()?.to_str()?;
    REGISTRY
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(name)
        .cloned()
}

/// The client side of one in-memory connection.
pub(super) struct PeerStreams {
    pub(super) outgoing: DuplexStream,
    pub(super) incoming: DuplexStream,
}

/// A connection to a registered peer, standing in for a spawned process and its pipes.
pub(super) struct AttachedPeer {
    pub(super) process: Box<dyn process_wrap::tokio::ChildWrapper>,
    pub(super) streams: PeerStreams,
}

/// Connect to the peer selected by `program` and start serving it, or return `None`.
pub(super) fn attach(program: &str) -> Option<AttachedPeer> {
    let definition = lookup(program)?;
    let (client_writer, peer_reader) = tokio::io::duplex(64 * 1024);
    let (mut peer_writer, client_reader) = tokio::io::duplex(64 * 1024);
    let mut handler = (definition.handler_factory)();
    tokio::spawn(async move {
        let mut lines = BufReader::new(peer_reader).lines();
        while let Ok(Some(line)) = lines.next_line().await {
            let request: Value = serde_json::from_str(&line).expect("client sends JSON lines");
            definition
                .requests
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(request.clone());
            for mut message in handler(&request) {
                message["jsonrpc"] = json!("2.0");
                let mut encoded = serde_json::to_vec(&message).expect("peer message serializes");
                encoded.push(b'\n');
                if peer_writer.write_all(&encoded).await.is_err() {
                    return;
                }
            }
        }
    });
    Some(AttachedPeer {
        process: Box::new(DetachedProcess::default()),
        streams: PeerStreams {
            outgoing: client_writer,
            incoming: client_reader,
        },
    })
}

/// A child handle with no operating-system process, whose collection always succeeds.
#[derive(Debug, Default)]
struct DetachedProcess {
    stdin: Option<tokio::process::ChildStdin>,
    stdout: Option<tokio::process::ChildStdout>,
    stderr: Option<tokio::process::ChildStderr>,
}

fn clean_exit() -> std::process::ExitStatus {
    #[cfg(unix)]
    {
        std::os::unix::process::ExitStatusExt::from_raw(0)
    }
    #[cfg(windows)]
    {
        std::os::windows::process::ExitStatusExt::from_raw(0)
    }
}

impl process_wrap::tokio::ChildWrapper for DetachedProcess {
    fn inner(&self) -> &dyn process_wrap::tokio::ChildWrapper {
        self
    }
    fn inner_mut(&mut self) -> &mut dyn process_wrap::tokio::ChildWrapper {
        self
    }
    fn into_inner(self: Box<Self>) -> Box<dyn process_wrap::tokio::ChildWrapper> {
        self
    }
    fn stdin(&mut self) -> &mut Option<tokio::process::ChildStdin> {
        &mut self.stdin
    }
    fn stdout(&mut self) -> &mut Option<tokio::process::ChildStdout> {
        &mut self.stdout
    }
    fn stderr(&mut self) -> &mut Option<tokio::process::ChildStderr> {
        &mut self.stderr
    }
    fn id(&self) -> Option<u32> {
        None
    }
    fn start_kill(&mut self) -> std::io::Result<()> {
        Ok(())
    }
    fn try_wait(&mut self) -> std::io::Result<Option<std::process::ExitStatus>> {
        Ok(Some(clean_exit()))
    }
    fn wait(
        &mut self,
    ) -> std::pin::Pin<
        Box<
            dyn std::future::Future<Output = std::io::Result<std::process::ExitStatus>> + Send + '_,
        >,
    > {
        Box::pin(async { Ok(clean_exit()) })
    }
}
