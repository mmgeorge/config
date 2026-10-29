use super::AcpBackend;
use crate::backend::approval::PermissionCoordinator;
use crate::backend::submission::{Submission, SubmissionState, SubmissionStore, SubmissionTarget};
use crate::backend::{
    Backend, BackendInput, BackendKind, BackendRequest, PromptMode, SteeringInput,
};
use crate::session::ApprovalMode;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use std::sync::Arc;
use std::time::Duration;

struct Fixture {
    directory: tempfile::TempDir,
    backend: AcpBackend,
    store: Arc<SubmissionStore>,
    trace: Arc<TraceStore>,
    request: BackendRequest,
}

impl Fixture {
    fn new(kind: BackendKind) -> Result<Self> {
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
readline.createInterface({input:process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = {protocolVersion:1,
    agentCapabilities:{loadSession:true}, _meta:{steering:{supported:true}}};
  if (request.method === 'session/new' || request.method === 'session/load') result = {sessionId:'native-session'};
  if (request.method === 'session/prompt' || request.method === '_session/steering') {
    fs.writeFileSync(process.argv[3], request.method);
    const mode = fs.readFileSync(process.argv[2], 'utf8');
    if (mode === 'wait') return;
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
        let node = crate::process::executable("node").context("ACP fixture requires Node.js")?;
        let backend = AcpBackend::new(
            kind,
            vec![
                node.to_string_lossy().into_owned(),
                peer.to_string_lossy().into_owned(),
                response.to_string_lossy().into_owned(),
                received.to_string_lossy().into_owned(),
            ],
            PermissionCoordinator::transient(directory.path())?,
            directory.path().to_owned(),
        )?;
        let trace = Arc::new(TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "acp-submission".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: BackendInput::from_text("original input"),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: String::new(),
            context_window: None,
            service_tier: Default::default(),
            access: kind.resolve_access(Default::default()),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        Ok(Self {
            directory,
            backend,
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

    fn prepare(&self) -> Result<(BackendRequest, Arc<Submission>)> {
        let mut request = self.request.clone();
        let submission = self
            .store
            .prepare(&request, self.backend.descriptor().kind)?;
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
        self.backend
            .cleanup_execution(&self.request.runtime_key()?)
            .await
    }
}

#[tokio::test]
async fn acp_preparation_failure_retains_undelivered_input() -> Result<()> {
    for kind in [BackendKind::Claude, BackendKind::Antigravity] {
        let fixture = Fixture::new(kind)?;
        let backend = AcpBackend::new(
            kind,
            vec!["forge-missing-acp-peer".into()],
            PermissionCoordinator::transient(fixture.directory.path())?,
            fixture.directory.path().to_owned(),
        )?;
        let (request, submission) = fixture.prepare()?;
        assert!(
            crate::backend::retry::prompt(&backend, request, None, &fixture.trace, &fixture.store)
                .await
                .is_err()
        );
        assert_eq!(submission.record()?.delivery, SubmissionState::Prepared);
        let (mut next, next_submission) = fixture.prepare()?;
        next_submission.recover(&fixture.backend, &mut next).await?;
        assert_eq!(next.recovered_input.len(), 1);
        assert_eq!(next.recovered_input[0].text(), "original input");
    }
    Ok(())
}

#[tokio::test]
async fn acp_prompt_response_controls_acceptance_for_both_providers() -> Result<()> {
    for kind in [BackendKind::Claude, BackendKind::Antigravity] {
        for response in ["ok", "error", "invalid", "disconnect"] {
            let fixture = Fixture::new(kind)?;
            fixture.response(response)?;
            let (request, submission) = fixture.prepare()?;
            let result = crate::backend::retry::prompt(
                &fixture.backend,
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
                        target: Some(SubmissionTarget::Acp { .. }),
                        ..
                    }
                ));
            } else {
                assert!(result.is_err(), "{kind:?} accepted {response}");
                assert!(matches!(
                    record.delivery,
                    SubmissionState::Sending {
                        target: SubmissionTarget::Acp { .. }
                    }
                ));
                assert!(record.error.is_some());
                let (mut next, next_submission) = fixture.prepare()?;
                assert!(
                    next_submission
                        .recover(&fixture.backend, &mut next)
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
    for kind in [BackendKind::Claude, BackendKind::Antigravity] {
        let fixture = Fixture::new(kind)?;
        fixture.response("wait")?;
        let (request, submission) = fixture.prepare()?;
        let mut sending = Box::pin(fixture.backend.prompt(request));
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
                .recover(&fixture.backend, &mut next)
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
        let fixture = Fixture::new(BackendKind::Claude)?;
        fixture.backend.prompt(fixture.request.clone()).await?;
        fixture.response(response)?;
        let mut steering_request = fixture.request.clone();
        steering_request.input = BackendInput::from_text("steering input");
        let submission = fixture
            .store
            .prepare(&steering_request, BackendKind::Claude)?;
        let input = SteeringInput::User {
            text: "steering input".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        };
        let key = fixture.request.runtime_key()?;
        let result = if response == "wait" {
            let mut sending = Box::pin(fixture.backend.steer_session(&key, input));
            tokio::select! {
                result = &mut sending => panic!("pending steering returned: {result:?}"),
                received = fixture.received() => received?,
            }
            drop(sending);
            Err(anyhow::anyhow!(
                "caller cancelled while awaiting acknowledgment"
            ))
        } else {
            fixture.backend.steer_session(&key, input).await
        };
        let delivery = submission.record()?.delivery;
        if matches!(response, "injected" | "startedNewTurn") {
            result?;
            assert!(matches!(delivery, SubmissionState::Accepted { .. }));
        } else if response == "promptRequired" {
            assert!(result.is_err());
            assert_eq!(delivery, SubmissionState::Prepared);
        } else {
            assert!(result.is_err());
            assert!(matches!(delivery, SubmissionState::Sending { .. }));
        }
        let (mut next, next_submission) = fixture.prepare()?;
        let recovery = next_submission.recover(&fixture.backend, &mut next).await;
        if matches!(response, "injected" | "startedNewTurn" | "promptRequired") {
            recovery?;
            assert_eq!(
                next.recovered_input.len(),
                usize::from(response == "promptRequired")
            );
        } else {
            assert!(recovery.is_err());
        }
        fixture.close().await?;
    }
    Ok(())
}

#[tokio::test]
async fn acp_journal_failure_never_authorizes_duplicate_dispatch() -> Result<()> {
    for kind in [BackendKind::Claude, BackendKind::Antigravity] {
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
            let result = crate::backend::retry::prompt(
                &fixture.backend,
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
                        .recover(&fixture.backend, &mut next)
                        .await
                        .is_err()
                );
            }
            fixture.close().await?;
        }
    }
    Ok(())
}
