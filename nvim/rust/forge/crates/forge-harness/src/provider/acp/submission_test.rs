use super::AcpProvider;
#[cfg(test)]
use crate::provider::ProviderAdapter as _;
use crate::provider::approval::PermissionCoordinator;
use crate::provider::submission::{Submission, SubmissionState, SubmissionStore, SubmissionTarget};
#[cfg(test)]
use crate::provider::test_support::ProviderTestExt as _;
use crate::provider::{PromptMode, ProviderInput, ProviderKind, ProviderRequest, SteeringInput};
use crate::session::ApprovalMode;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use std::sync::Arc;
use std::time::Duration;

struct Fixture {
    directory: tempfile::TempDir,
    provider: crate::provider::test_support::ProviderTestSession<AcpProvider>,
    store: Arc<SubmissionStore>,
    trace: Arc<TraceStore>,
    request: ProviderRequest,
}

impl Fixture {
    fn new(kind: ProviderKind) -> Result<Self> {
        let directory = tempfile::tempdir()?;
        let peer = directory.path().join("peer.cjs");
        let response = directory.path().join("response");
        let received = directory.path().join("received");
        std::fs::write(&response, "ok")?;
        std::fs::write(
            &peer,
            r#"
const fs = require('node:fs');
const readline = require('node:readline');
const write = value => process.stdout.write(JSON.stringify({jsonrpc:'2.0', ...value}) + '\n');
let pending;
setInterval(() => {
  if (pending && fs.readFileSync(process.argv[2], 'utf8') === 'release') {
    write({id:pending.id,result:{stopReason:'end_turn'}});
    pending = undefined;
  }
}, 5);
readline.createInterface({input:process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  fs.appendFileSync(process.argv[4], JSON.stringify(request) + '\n');
  if (request.method === 'session/cancel') {
    if (fs.readFileSync(process.argv[2], 'utf8') === 'cancelDisconnect') { process.exit(0); return; }
    if (pending) {
      write({id:pending.id,result:{stopReason:'cancelled'}});
      pending = undefined;
    }
    return;
  }
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = {protocolVersion:1, agentInfo:{version:'99.0.0'},
    agentCapabilities:{loadSession:true}, _meta:{steering:{supported:true}}};
  if (request.method === 'session/new' || request.method === 'session/load') result = {sessionId:'native-session'};
  if (request.method === 'session/prompt' || request.method === '_session/steering') {
    fs.writeFileSync(process.argv[3], request.method);
    const mode = fs.readFileSync(process.argv[2], 'utf8');
    if (mode === 'wait') { pending = request; return; }
    if (mode === 'disconnect') { process.exit(0); }
    if (mode === 'error') { write({id:request.id,error:{code:-32603,message:'native submission failure'}}); return; }
    if (mode === 'invalid') result = {};
    else if (request.method === 'session/prompt') result = {stopReason:'end_turn'};
    else if (mode === 'promptRequired') result = {outcome:mode,reason:'noRunningTurn'};
    else result = {outcome:mode};
  }
  write({id:request.id,result});
});
"#,
        )?;
        let node =
            forge_shell::process::executable("node").context("ACP fixture requires Node.js")?;
        let provider = AcpProvider::new(
            kind,
            vec![
                node.to_string_lossy().into_owned(),
                peer.to_string_lossy().into_owned(),
                response.to_string_lossy().into_owned(),
                received.to_string_lossy().into_owned(),
                directory
                    .path()
                    .join("wire.jsonl")
                    .to_string_lossy()
                    .into_owned(),
            ],
            PermissionCoordinator::transient(directory.path())?,
            directory.path().to_owned(),
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let request = ProviderRequest {
            execution_owner: None,
            harness_session_id: "acp-submission".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("original input"),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: String::new(),
            context_window: None,
            service_tier: Default::default(),
            access: crate::provider::registered_capability(kind).resolve_access(Default::default()),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        Ok(Self {
            directory,
            provider,
            store,
            trace,
            request,
        })
    }

    fn response(&self, response: &str) -> Result<()> {
        std::fs::write(self.directory.path().join("response"), response)?;
        let received = self.directory.path().join("received");
        if received.exists() {
            std::fs::remove_file(received)?;
        }
        Ok(())
    }

    fn prepare(&self) -> Result<(ProviderRequest, Arc<Submission>)> {
        let mut request = self.request.clone();
        let submission = self
            .store
            .prepare(&request, self.provider.descriptor().kind)?;
        request.submission = Some(submission.clone());
        Ok((request, submission))
    }

    async fn received(&self) -> Result<()> {
        tokio::time::timeout(Duration::from_secs(5), async {
            while !self.directory.path().join("received").exists() {
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        })
        .await
        .context("fixture did not receive submission")?;
        Ok(())
    }

    async fn close(&self) -> Result<()> {
        self.provider
            .cleanup_execution(&self.request.runtime_key()?)
            .await
    }

    /// Get the peer's native request and notification evidence without interpreting transcript text.
    fn wire(&self) -> Result<Vec<serde_json::Value>> {
        std::fs::read_to_string(self.directory.path().join("wire.jsonl"))?
            .lines()
            .map(|line| serde_json::from_str(line).map_err(Into::into))
            .collect()
    }
}

/// Permit steering during ACP's live terminal-response wait without acknowledging the prompt early.
#[tokio::test]
async fn acp_live_prompt_accepts_injected_steering_before_terminal_response() -> Result<()> {
    let fixture = Fixture::new(ProviderKind::Claude)?;
    fixture.response("wait")?;
    let (request, original) = fixture.prepare()?;
    let mut prompt = Box::pin(fixture.provider.session_prompt_and_collect(request));
    tokio::select! {
        result = &mut prompt => panic!("held ACP prompt completed: {result:?}"),
        received = fixture.received() => received?,
    }
    assert!(matches!(
        original.record()?.delivery,
        SubmissionState::Sending { .. }
    ));
    fixture.response("injected")?;
    let mut steer_request = fixture.request.clone();
    steer_request.input = ProviderInput::from_text("change the pending response");
    let steering = fixture
        .store
        .prepare(&steer_request, ProviderKind::Claude)?;
    let _flight = steering.begin_steering()?;
    tokio::time::timeout(
        Duration::from_secs(5),
        fixture.provider.steer_current(
            fixture.request.owner()?,
            SteeringInput::User {
                text: steer_request.input.text().into(),
                attachments: Vec::new(),
                submission: Some(steering.clone()),
            },
        ),
    )
    .await??;
    assert!(matches!(
        steering.record()?.delivery,
        SubmissionState::Accepted { .. }
    ));
    assert!(matches!(
        original.record()?.delivery,
        SubmissionState::Sending { .. }
    ));
    assert!(
        futures_util::poll!(prompt.as_mut()).is_pending(),
        "steering acceptance settled the ACP prompt"
    );
    fixture.response("release")?;
    tokio::time::timeout(Duration::from_secs(5), prompt).await??;
    assert!(matches!(
        original.record()?.delivery,
        SubmissionState::Accepted { .. }
    ));
    let wire = fixture.wire()?;
    assert_eq!(
        wire.iter()
            .filter(|request| request["method"] == "session/prompt")
            .count(),
        1
    );
    assert_eq!(
        wire.iter()
            .filter(|request| request["method"] == "_session/steering")
            .count(),
        1
    );
    fixture.close().await?;
    Ok(())
}

/// Collect native cancellation through the pending prompt response and reuse the retained reader.
#[tokio::test]
async fn acp_native_cancel_collects_terminal_response_and_reuses_reader() -> Result<()> {
    for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
        let fixture = Fixture::new(kind)?;
        fixture.response("wait")?;
        let (request, original) = fixture.prepare()?;
        let mut prompt = Box::pin(fixture.provider.session_prompt_and_collect(request));
        tokio::select! {
            result = &mut prompt => panic!("held ACP prompt completed: {result:?}"),
            received = fixture.received() => received?,
        }
        tokio::time::timeout(
            Duration::from_secs(5),
            fixture
                .provider
                .cancel_session(&fixture.request.runtime_key()?),
        )
        .await??;
        let cancelled = tokio::time::timeout(Duration::from_secs(5), prompt).await?;
        assert!(
            matches!(cancelled, Err(crate::provider::ProviderError::Cancelled)),
            "{cancelled:?}"
        );
        assert!(matches!(
            original.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        fixture.response("ok")?;
        let mut request = fixture.request.clone();
        request.input = ProviderInput::from_text("a distinct second prompt");
        request.submission = Some(fixture.store.prepare(&request, kind)?);
        tokio::time::timeout(
            Duration::from_secs(5),
            fixture.provider.session_prompt_and_collect(request),
        )
        .await??;
        let wire = fixture.wire()?;
        assert_eq!(
            wire.iter()
                .filter(|request| request["method"] == "initialize")
                .count(),
            1
        );
        assert_eq!(
            wire.iter()
                .filter(|request| request["method"] == "session/prompt")
                .count(),
            2
        );
        let cancellations: Vec<_> = wire
            .iter()
            .filter(|request| request["method"] == "session/cancel")
            .collect();
        assert_eq!(cancellations.len(), 1);
        assert!(
            cancellations[0].get("id").is_none(),
            "native cancellation must be a notification"
        );
        fixture.close().await?;
    }
    Ok(())
}

/// Preserve unknown delivery when the native reader disconnects instead of confirming cancellation.
#[tokio::test]
async fn acp_cancel_without_terminal_response_preserves_unknown_delivery() -> Result<()> {
    let fixture = Fixture::new(ProviderKind::Antigravity)?;
    fixture.response("wait")?;
    let (request, submission) = fixture.prepare()?;
    let mut prompt = Box::pin(fixture.provider.session_prompt_and_collect(request));
    tokio::select! {
        result = &mut prompt => panic!("held ACP prompt completed: {result:?}"),
        received = fixture.received() => received?,
    }
    fixture.response("cancelDisconnect")?;
    assert!(
        tokio::time::timeout(
            Duration::from_secs(5),
            fixture
                .provider
                .cancel_session(&fixture.request.runtime_key()?)
        )
        .await?
        .is_err()
    );
    let result = tokio::time::timeout(Duration::from_secs(5), prompt).await?;
    assert!(
        result.is_err() && !matches!(result, Err(crate::provider::ProviderError::Cancelled)),
        "disconnect supplied false terminal cancellation: {result:?}"
    );
    assert!(matches!(
        submission.record()?.delivery,
        SubmissionState::Sending { .. }
    ));
    let (mut next, receipt) = fixture.prepare()?;
    assert!(
        receipt
            .recover(fixture.provider.as_provider(), &mut next)
            .await
            .is_err()
    );
    assert_eq!(
        fixture
            .wire()?
            .iter()
            .filter(|request| request["method"] == "session/prompt")
            .count(),
        1
    );
    fixture.close().await?;
    Ok(())
}

#[tokio::test]
async fn acp_preparation_failure_retains_undelivered_input() -> Result<()> {
    for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
        let fixture = Fixture::new(kind)?;
        let provider = AcpProvider::new(
            kind,
            vec!["forge-missing-acp-peer".into()],
            PermissionCoordinator::transient(fixture.directory.path())?,
            fixture.directory.path().to_owned(),
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let (request, submission) = fixture.prepare()?;
        assert!(
            crate::provider::test_support::retry_prompt(
                &provider,
                request,
                None,
                &fixture.trace,
                &fixture.store
            )
            .await
            .is_err()
        );
        assert_eq!(submission.record()?.delivery, SubmissionState::Prepared);
        let (mut next, next_submission) = fixture.prepare()?;
        next_submission
            .recover(fixture.provider.as_provider(), &mut next)
            .await?;
        assert_eq!(next.recovered_input.len(), 1);
        assert_eq!(next.recovered_input[0].text(), "original input");
    }
    Ok(())
}

#[tokio::test]
async fn acp_prompt_response_controls_acceptance_for_both_providers() -> Result<()> {
    for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
        for response in ["ok", "error", "invalid", "disconnect"] {
            let fixture = Fixture::new(kind)?;
            fixture.response(response)?;
            let (request, submission) = fixture.prepare()?;
            let result = crate::provider::test_support::retry_prompt(
                &fixture.provider,
                request,
                None,
                &fixture.trace,
                &fixture.store,
            )
            .await;
            let record = submission.record()?;
            if response == "ok" {
                result?;
                assert!(matches!(
                    record.delivery,
                    SubmissionState::Accepted {
                        target: Some(SubmissionTarget::Native { .. }),
                        ..
                    }
                ));
            } else {
                assert!(result.is_err(), "{kind:?} accepted {response}");
                assert!(matches!(
                    record.delivery,
                    SubmissionState::Sending {
                        target: SubmissionTarget::Native { .. }
                    }
                ));
                assert!(record.error.is_some());
                let (mut next, next_submission) = fixture.prepare()?;
                assert!(
                    next_submission
                        .recover(fixture.provider.as_provider(), &mut next)
                        .await
                        .is_err()
                );
            }
            fixture
                .close()
                .await
                .with_context(|| format!("{kind:?} cleanup after {response}"))?;
        }
    }
    Ok(())
}

#[tokio::test]
async fn acp_cancelled_prompt_cannot_replay_after_dispatch() -> Result<()> {
    for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
        let fixture = Fixture::new(kind)?;
        fixture.response("wait")?;
        let (request, submission) = fixture.prepare()?;
        let (sink, _events) = crate::provider::events::channel(
            crate::provider::InvocationScope::new(request.owner()?, "cancelled-acp"),
        );
        let mut sending = Box::pin(fixture.provider.session_prompt_and_wait(request, sink));
        tokio::select! {
            result = &mut sending => panic!("pending submission returned: {result:?}"),
            received = fixture.received() => received?,
        }
        drop(sending);
        assert!(matches!(
            submission.record()?.delivery,
            SubmissionState::Sending { .. }
        ));
        let (mut next, next_submission) = fixture.prepare()?;
        assert!(
            next_submission
                .recover(fixture.provider.as_provider(), &mut next)
                .await
                .is_err()
        );
        fixture.close().await?;
    }
    Ok(())
}

#[tokio::test]
async fn acp_steering_only_requeues_a_proven_idle_rejection() -> Result<()> {
    for response in [
        "injected",
        "startedNewTurn",
        "promptRequired",
        "invalid",
        "error",
        "wait",
    ] {
        let fixture = Fixture::new(ProviderKind::Claude)?;
        fixture
            .provider
            .session_prompt_and_collect(fixture.request.clone())
            .await?;
        fixture.response(response)?;
        let mut steering_request = fixture.request.clone();
        steering_request.input = ProviderInput::from_text("steering input");
        let submission = fixture
            .store
            .prepare(&steering_request, ProviderKind::Claude)?;
        let input = SteeringInput::User {
            text: "steering input".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        };
        let result = if response == "wait" {
            let mut sending = Box::pin(
                fixture
                    .provider
                    .steer_current(fixture.request.owner()?, input),
            );
            tokio::select! {
                result = &mut sending => panic!("pending steering returned: {result:?}"),
                received = fixture.received() => received?,
            }
            drop(sending);
            Err(anyhow::anyhow!(
                "caller cancelled while awaiting acknowledgment"
            ))
        } else {
            fixture
                .provider
                .steer_current(fixture.request.owner()?, input)
                .await
                .map_err(anyhow::Error::from)
        };
        let delivery = submission.record()?.delivery;
        if matches!(response, "injected" | "startedNewTurn" | "promptRequired") {
            result?;
            assert!(matches!(delivery, SubmissionState::Accepted { .. }));
        } else {
            assert!(result.is_err());
            assert!(matches!(delivery, SubmissionState::Sending { .. }));
        }
        let (mut next, next_submission) = fixture.prepare()?;
        let recovery = next_submission
            .recover(fixture.provider.as_provider(), &mut next)
            .await;
        if matches!(response, "injected" | "startedNewTurn" | "promptRequired") {
            recovery?;
            assert!(next.recovered_input.is_empty());
        } else {
            assert!(recovery.is_err());
        }
        fixture.close().await?;
    }
    Ok(())
}

#[tokio::test]
async fn acp_journal_failure_never_authorizes_duplicate_dispatch() -> Result<()> {
    for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
        for failing_state in ["sending", "accepted"] {
            let fixture = Fixture::new(kind)?;
            let (request, submission) = fixture.prepare()?;
            let connection = rusqlite::Connection::open(
                fixture
                    .directory
                    .path()
                    .join("provider-submissions.sqlite3"),
            )?;
            connection.execute_batch(&format!(
                "CREATE TRIGGER fail_delivery BEFORE UPDATE ON submission
                 WHEN json_extract(NEW.payload,'$.delivery.state')='{failing_state}'
                 BEGIN SELECT RAISE(ABORT,'fixture journal failure'); END;"
            ))?;
            let result = crate::provider::test_support::retry_prompt(
                &fixture.provider,
                request,
                None,
                &fixture.trace,
                &fixture.store,
            )
            .await;
            assert!(format!("{:#}", result.unwrap_err()).contains("fixture journal failure"));
            let delivery = submission.record()?.delivery;
            if failing_state == "sending" {
                assert_eq!(delivery, SubmissionState::Prepared);
                assert!(!fixture.directory.path().join("received").exists());
            } else {
                assert!(matches!(delivery, SubmissionState::Sending { .. }));
                assert!(fixture.directory.path().join("received").exists());
                let (mut next, next_submission) = fixture.prepare()?;
                assert!(
                    next_submission
                        .recover(fixture.provider.as_provider(), &mut next)
                        .await
                        .is_err()
                );
            }
            fixture.close().await?;
        }
    }
    Ok(())
}
