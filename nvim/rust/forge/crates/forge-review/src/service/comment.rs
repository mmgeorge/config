use super::*;
use crate::comments::CommentId;
use crate::review::{
    CommentRequest, CommentSettlement, PreparedComment, ReviewCommentCommand, ReviewCommentSnapshot,
};
use forge_github::comment::{CommentKind, CommentOperation, CommentResult, CommentState};
use forge_github::recovery::{RecoveryPhase, RecoveryResource, RecoveryResourceKind};
use forge_github::review_mutation::{RecoveryResolution, ReviewMutation, ReviewMutationRequest};
use forge_github::service::MutationNotStarted;

#[derive(Debug, Serialize)]
pub struct ReviewCommentResult {
    pub snapshot: ReviewCommentSnapshot,
    pub remote: Option<CommentResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<forge_buffer::patch::BufferPatch>,
}

struct CommentCapture {
    parent_node_id: String,
    comment: CommentId,
    sequence: u64,
    operation_id: String,
    mutation: ReviewMutation,
    kind: CommentKind,
}

impl ReviewService {
    pub async fn comment(
        &self,
        id: &DocumentId,
        command: ReviewCommentCommand,
    ) -> Result<ReviewCommentResult> {
        if let ReviewCommentCommand::Snapshot { comment } = command {
            let owner = self.owner(id)?;
            let snapshot = owner
                .document
                .lock()
                .expect("review document poisoned")
                .comment_snapshot(comment)?;
            return Ok(ReviewCommentResult {
                snapshot,
                remote: None,
                patch: None,
            });
        }
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review job admission is full")?;
        let guard = self.remote_guard(id)?;
        let (prepared, capture, resource) = {
            let mut document = guard
                .owner
                .document
                .lock()
                .expect("review document poisoned");
            let prepared = document.prepare_comment(command)?;
            let capture = capture_comment(&document, &prepared)?;
            let resource = RecoveryResource {
                repository: document.target.repository.clone(),
                kind: RecoveryResourceKind::PullRequest,
                number: document.target.number,
            };
            (prepared, capture, resource)
        };
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                if let PreparedComment::Recovery {
                    comment,
                    resolution,
                } = prepared
                {
                    let retained = service
                        .github
                        .recovery_inspect(resource.clone())
                        .await?
                        .context("comment has no retained recovery operation")?;
                    let captured: ReviewMutationRequest =
                        serde_json::from_value(retained.capture.submitted.clone())?;
                    ensure!(
                        captured.resource == resource
                            && captured.draft_target.as_deref()
                                == Some(format!("native-comment:{}", comment.0).as_str()),
                        "retained operation belongs to another comment draft"
                    );
                    let record = if let Some(resolution) = resolution {
                        service
                            .github
                            .recovery_resolve(
                                guard.owner.remote.clone(),
                                resource.clone(),
                                retained.capture.operation_id.clone(),
                                resolution,
                            )
                            .await?
                    } else {
                        retained
                    };
                    let kind = match captured.mutation {
                        ReviewMutation::ConversationCreate { .. }
                        | ReviewMutation::ConversationEdit { .. }
                        | ReviewMutation::ConversationDelete { .. } => CommentKind::IssueComment,
                        ReviewMutation::InlineCreate { .. }
                        | ReviewMutation::ReplyCreate { .. }
                        | ReviewMutation::InlineEdit { .. }
                        | ReviewMutation::InlineDelete { .. } => {
                            CommentKind::PullRequestReviewComment
                        }
                        _ => anyhow::bail!("retained operation is not a native comment mutation"),
                    };
                    let capture = CommentCapture {
                        parent_node_id: captured.parent_node_id.unwrap_or_default(),
                        comment,
                        sequence: captured
                            .edit_sequence
                            .context("retained comment has no edit sequence")?,
                        operation_id: captured.operation_id,
                        mutation: captured.mutation,
                        kind,
                    };
                    let remote = match &record.state {
                        RecoveryPhase::Confirmed { result }
                        | RecoveryPhase::UserLinked { result, .. } => {
                            decode_confirmation(result, &capture)?
                        }
                        RecoveryPhase::Rejected { diagnostic }
                        | RecoveryPhase::UserClosedUnknown { diagnostic } => {
                            CommentResult::Rejected {
                                message: diagnostic.clone(),
                            }
                        }
                        _ => anyhow::bail!("comment outcome remains unresolved"),
                    };
                    let _publication = guard.owner.publication.lock().await;
                    let draft = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned")
                        .comment_settlement_draft(comment, &remote)?;
                    service
                        .github
                        .recovery_settle_draft(resource, capture.operation_id, draft)
                        .await?;
                    let snapshot = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned")
                        .recover_comment(comment, &remote)?;
                    return Ok(ReviewCommentResult {
                        snapshot,
                        remote: Some(remote),
                        patch: None,
                    });
                }
                let PreparedComment::Remote {
                    request,
                    settlement,
                } = prepared
                else {
                    let PreparedComment::Snapshot(snapshot) = prepared else {
                        unreachable!()
                    };
                    let _publication = guard.owner.publication.lock().await;
                    let draft = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned")
                        .draft_payload()?;
                    service.github.review_draft_write(resource, draft).await?;
                    return Ok(ReviewCommentResult {
                        snapshot,
                        remote: None,
                        patch: None,
                    });
                };
                if let Some(capture) = capture {
                    let mut dispatch_attempted = false;
                    let outcome = async {
                        let actor = guard
                            .owner
                            .remote
                            .read_actor(resource.repository.clone())
                            .await?;
                        dispatch_attempted = true;
                        service
                            .github
                            .review_mutation(
                                guard.owner.remote.clone(),
                                ReviewMutationRequest {
                                    parent_node_id: Some(capture.parent_node_id.clone()),
                                    resource: resource.clone(),
                                    operation_id: capture.operation_id.clone(),
                                    actor_node_id: actor.node_id,
                                    edit_sequence: Some(capture.sequence),
                                    draft_target: Some(format!(
                                        "native-comment:{}",
                                        capture.comment.0
                                    )),
                                    mutation: capture.mutation.clone(),
                                },
                            )
                            .await
                    }
                    .await;
                    let translated = match &outcome {
                        Ok(record) => match &record.state {
                            RecoveryPhase::Confirmed { result } => {
                                decode_confirmation(result, &capture)
                            }
                            RecoveryPhase::Rejected { diagnostic } => Ok(CommentResult::Rejected {
                                message: diagnostic.clone(),
                            }),
                            RecoveryPhase::OutcomeUnknown { diagnostic } => {
                                Ok(CommentResult::OutcomeUnknown {
                                    message: diagnostic.clone(),
                                })
                            }
                            _ => Ok(CommentResult::OutcomeUnknown {
                                message: "comment operation has no terminal outcome".into(),
                            }),
                        },
                        Err(failure)
                            if !dispatch_attempted || failure.is::<MutationNotStarted>() =>
                        {
                            Ok(CommentResult::Rejected {
                                message: failure.to_string(),
                            })
                        }
                        Err(failure) => Ok(CommentResult::OutcomeUnknown {
                            message: failure.to_string(),
                        }),
                    }?;
                    let _publication = guard.owner.publication.lock().await;
                    if let Ok(record) = &outcome
                        && matches!(
                            record.state,
                            RecoveryPhase::Confirmed { .. } | RecoveryPhase::Rejected { .. }
                        )
                    {
                        let draft = guard
                            .owner
                            .document
                            .lock()
                            .expect("review document poisoned")
                            .comment_settlement_draft(capture.comment, &translated)?;
                        service
                            .github
                            .recovery_settle_draft(resource, capture.operation_id, draft)
                            .await?;
                    }
                    let snapshot = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned")
                        .settle_comment(settlement, Some(&translated))?;
                    outcome?;
                    return Ok(ReviewCommentResult {
                        snapshot,
                        remote: Some(translated),
                        patch: None,
                    });
                }
                let CommentRequest::Existing { target, operation } = request else {
                    unreachable!()
                };
                let mut recovery = None;
                if let CommentSettlement::Reconcile { comment, .. } = &settlement {
                    recovery = service.github.recovery_inspect(resource.clone()).await?;
                    if let Some(record) = &recovery {
                        ensure!(
                            record
                                .capture
                                .submitted
                                .get("draft_target")
                                .and_then(serde_json::Value::as_str)
                                == Some(format!("native-comment:{}", comment.0).as_str()),
                            "another captured comment requires recovery"
                        );
                        if matches!(
                            record.state,
                            RecoveryPhase::DispatchPossible | RecoveryPhase::OutcomeUnknown { .. }
                        ) {
                            recovery = Some(
                                service
                                    .github
                                    .recovery_resolve(
                                        guard.owner.remote.clone(),
                                        resource.clone(),
                                        record.capture.operation_id.clone(),
                                        RecoveryResolution::Link {
                                            remote_id: target.database_id,
                                        },
                                    )
                                    .await?,
                            );
                        }
                    }
                }
                let observed = service
                    .github
                    .comment(guard.owner.remote.clone(), target, operation)
                    .await?;
                let _publication = guard.owner.publication.lock().await;
                if let Some(record) = recovery
                    && let CommentSettlement::Reconcile { comment, .. } = &settlement
                {
                    let draft = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned")
                        .comment_settlement_draft(*comment, &observed)?;
                    service
                        .github
                        .recovery_settle_draft(resource.clone(), record.capture.operation_id, draft)
                        .await?;
                }
                let (snapshot, draft) = {
                    let mut document = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned");
                    let snapshot = document.settle_comment(settlement, Some(&observed))?;
                    (snapshot, document.draft_payload()?)
                };
                service.github.review_draft_write(resource, draft).await?;
                Ok(ReviewCommentResult {
                    snapshot,
                    remote: Some(observed),
                    patch: None,
                })
            }
            .await;
            let result = match result {
                Ok(result) => {
                    service
                        .publish_comment_result(guard.owner.clone(), result)
                        .await
                }
                Err(failure) => Err(failure),
            };
            drop(guard);
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review comment operation ended without a result")?
    }

    async fn publish_comment_result(
        &self,
        owner: Arc<DocumentOwner>,
        mut result: ReviewCommentResult,
    ) -> Result<ReviewCommentResult> {
        let _publication = owner.publication.lock().await;
        let materialized = owner
            .document
            .lock()
            .expect("review document poisoned")
            .projection
            .is_some();
        if !materialized {
            result.snapshot = owner
                .document
                .lock()
                .expect("review document poisoned")
                .comment_snapshot(result.snapshot.comment)?;
            return Ok(result);
        }
        let analysis = self
            .analysis
            .as_ref()
            .context("comment analysis service is unavailable")?;
        let permit = analysis
            .reserve(
                forge_diff::workers::WorkPriority::Foreground,
                forge_diff::workers::WorkBudget::new(2 * 1024 * 1024, None),
            )
            .map_err(|failure| {
                anyhow::anyhow!("comment projection admission failed: {failure:?}")
            })?;
        let ticket = permit.ticket();
        let projection_owner = owner.clone();
        let (sender, receiver) = oneshot::channel();
        permit.submit(move |_| {
            let projected = (|| -> Result<ReviewCommentResult> {
                let mut document = projection_owner
                    .document
                    .lock()
                    .expect("review document poisoned");
                result.snapshot = document.comment_snapshot(result.snapshot.comment)?;
                result.patch = document.project_comment(&result.snapshot)?;
                Ok(result)
            })();
            let _ = sender.send(projected);
        });
        let result = receiver
            .await
            .context("comment projection ended without a result");
        ticket.completed().await;
        result?
    }
}

fn capture_comment(
    document: &ReviewDocument,
    prepared: &PreparedComment,
) -> Result<Option<CommentCapture>> {
    let PreparedComment::Remote {
        request,
        settlement,
    } = prepared
    else {
        return Ok(None);
    };
    let comment = match settlement {
        CommentSettlement::Create { comment } | CommentSettlement::Save { comment } => *comment,
        _ => return Ok(None),
    };
    let (mutation, operation_id, kind) = match request {
        CommentRequest::Create {
            mutation,
            receipt,
            kind,
        } => (mutation.clone(), receipt.clone(), *kind),
        CommentRequest::Existing { target, operation } => {
            let (mutation, receipt) = match operation {
                CommentOperation::Edit { body, receipt } => (
                    match target.kind {
                        CommentKind::IssueComment => ReviewMutation::ConversationEdit {
                            comment_id: target.database_id,
                            body: body.clone(),
                        },
                        CommentKind::PullRequestReviewComment => ReviewMutation::InlineEdit {
                            comment_id: target.database_id,
                            body: body.clone(),
                        },
                    },
                    receipt,
                ),
                CommentOperation::Delete { receipt } => (
                    match target.kind {
                        CommentKind::IssueComment => ReviewMutation::ConversationDelete {
                            comment_id: target.database_id,
                        },
                        CommentKind::PullRequestReviewComment => ReviewMutation::InlineDelete {
                            comment_id: target.database_id,
                        },
                    },
                    receipt,
                ),
                CommentOperation::Reconcile => return Ok(None),
            };
            (mutation, receipt.clone(), target.kind)
        }
    };
    Ok(Some(CommentCapture {
        parent_node_id: document.target.node_id.clone(),
        comment,
        sequence: document.comment_snapshot(comment)?.sequence.0,
        operation_id,
        mutation,
        kind,
    }))
}

fn decode_confirmation(
    value: &serde_json::Value,
    capture: &CommentCapture,
) -> Result<CommentResult> {
    if matches!(
        capture.mutation,
        ReviewMutation::ConversationDelete { .. } | ReviewMutation::InlineDelete { .. }
    ) {
        return Ok(CommentResult::Confirmed { state: None });
    }
    let state = CommentState {
        kind: capture.kind,
        id: value
            .get("node_id")
            .and_then(serde_json::Value::as_str)
            .context("confirmed comment omitted node identity")?
            .into(),
        database_id: value
            .get("id")
            .and_then(serde_json::Value::as_u64)
            .context("confirmed comment omitted numeric identity")?,
        body: value
            .get("body")
            .and_then(serde_json::Value::as_str)
            .context("confirmed comment omitted body")?
            .into(),
        url: value
            .get("html_url")
            .and_then(serde_json::Value::as_str)
            .context("confirmed comment omitted URL")?
            .into(),
        viewer_did_author: true,
    };
    Ok(CommentResult::Confirmed { state: Some(state) })
}
