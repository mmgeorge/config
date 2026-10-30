use anyhow::{Context, Result, ensure};
use forge_buffer::identity::{DocumentId, InputSequence, ViewId};
use forge_buffer::input::DocumentInput;
use forge_buffer::width::WidthProfile;

use super::thread_projection::ReviewTarget;
use super::{ReviewMaterialization, ReviewService};
use crate::review::ReviewDocument;

#[derive(Debug, serde::Serialize)]
pub struct ReviewActionDelivery {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub patch: Vec<forge_buffer::patch::BufferPatch>,
    pub effect: Option<ReviewActionEffect>,
    pub diagnostic: Option<String>,
    pub refresh: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub comment: Option<crate::review::ReviewCommentSnapshot>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub choice: Option<Vec<ReviewActionChoice>>,
}

#[derive(Debug, serde::Serialize)]
pub struct ReviewActionChoice {
    pub key: &'static str,
    pub label: &'static str,
    pub value: &'static str,
}

#[derive(Debug, serde::Serialize)]
pub struct ReviewActionEffect {
    pub id: String,
    #[serde(flatten)]
    pub input: DocumentInput,
    pub kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub oid: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<u8>,
}

impl ReviewService {
    pub async fn act(
        &self,
        input: DocumentInput,
        directory: std::path::PathBuf,
    ) -> Result<ReviewActionDelivery> {
        ensure!(
            matches!(
                input.action.as_str(),
                "activate"
                    | "open"
                    | "browse"
                    | "expand"
                    | "toggle_visibility"
                    | "toggle_presentation"
                    | "source_expand"
                    | "source_collapse"
                    | "lifecycle:draft"
                    | "lifecycle:open"
                    | "lifecycle:closed"
            ),
            "unsupported review input action"
        );
        let owner = self.owner(&input.document)?;
        owner.validate_directory(&directory)?;
        let mut target = owner
            .document
            .lock()
            .expect("review document poisoned")
            .capture_input(&input)?;
        if input.action == "open"
            && let ReviewTarget::FileDiff { path } | ReviewTarget::SourceSection { path, .. } =
                &target
        {
            target = ReviewTarget::WorkspaceFile { path: path.clone() };
        }
        let mut delivery = ReviewActionDelivery {
            diff_view: None,
            patch: Vec::new(),
            effect: None,
            diagnostic: None,
            refresh: false,
            comment: None,
            choice: None,
        };
        ensure!(
            !input.action.starts_with("lifecycle:")
                || matches!(target, ReviewTarget::Lifecycle { .. }),
            "review lifecycle action requires its status target"
        );
        if input.action == "expand"
            && !matches!(
                target,
                ReviewTarget::FileDiff { .. }
                    | ReviewTarget::CommitDiff { .. }
                    | ReviewTarget::FileMore { .. }
                    | ReviewTarget::SourceSection { .. }
                    | ReviewTarget::CommitSource { .. }
            )
        {
            return Ok(delivery);
        }
        match target {
            ReviewTarget::DiffView { action } => {
                let changed = self.change_diff_view(&input.document, action).await?;
                delivery.patch = changed.patch;
                delivery.diff_view = changed.options;
            }
            ReviewTarget::SourceSection { path, identity } => {
                let result = self
                    .change_file_view(
                        &input.document,
                        path,
                        super::file::FileViewChange::Declaration {
                            identity,
                            expanded: input.action != "source_collapse",
                        },
                    )
                    .await?;
                if let Some(patch) = result.patch {
                    delivery.patch.push(patch);
                }
            }
            ReviewTarget::CommitSource {
                commit,
                path,
                declaration,
            } => {
                let change = match declaration {
                    Some(identity) => super::file::FileViewChange::Declaration {
                        identity,
                        expanded: input.action != "source_collapse",
                    },
                    None => super::file::FileViewChange::Continuation,
                };
                if let Some(patch) = self
                    .change_commit_source(&input.document, commit, path, change)
                    .await?
                {
                    delivery.patch.push(patch);
                }
            }
            ReviewTarget::Lifecycle { available } => {
                if matches!(input.action.as_str(), "activate" | "open") {
                    delivery.choice = Some(available.into_iter().map(lifecycle_choice).collect());
                    return Ok(delivery);
                }
                let desired = available
                    .into_iter()
                    .find(|desired| lifecycle_choice(*desired).value == input.action)
                    .context("review lifecycle choice is no longer available")?;
                let result = self.transition(&input.document, desired).await?;
                let (message, level) = if result.fresh_required {
                    ("Pull request lifecycle outcome is unknown. Resolve or reconcile it before another transition.".into(), 3)
                } else if let Some(lifecycle) = result.lifecycle {
                    (
                        format!("Pull request lifecycle is {}", lifecycle.label()),
                        2,
                    )
                } else {
                    ("Pull request lifecycle change was rejected.".into(), 3)
                };
                delivery.effect = Some(ReviewActionEffect {
                    id: format!("review-lifecycle-{}", input.sequence.0),
                    input,
                    kind: "notify",
                    url: None,
                    path: None,
                    workspace: None,
                    oid: None,
                    message: Some(message),
                    level: Some(level),
                });
                delivery.refresh = true;
            }
            ReviewTarget::FileMore { path } => {
                if let Some(patch) = self.read_file_more(&input.document, path).await?.patch {
                    delivery.patch.push(patch);
                }
            }
            ReviewTarget::FileDiff { path } => {
                let result = self
                    .read_file(&input.document, directory, owner.remote.clone(), path)
                    .await?;
                if let Some(patch) = result.patch {
                    delivery.patch.push(patch);
                }
            }
            ReviewTarget::WorkspaceFile { path } => {
                let workspace = owner
                    .workspace
                    .as_ref()
                    .context("review working-file target requires a captured workspace")?;
                let path = workspace_file_path(workspace, &path)?;
                delivery.effect = Some(ReviewActionEffect {
                    id: format!("review-file-{}", input.sequence.0),
                    input,
                    kind: "open_file",
                    url: None,
                    path: Some(path),
                    workspace: None,
                    oid: None,
                    message: None,
                    level: None,
                });
            }
            ReviewTarget::CommitMessage { oid } => {
                ensure!(
                    oid.len() == 40 && oid.bytes().all(|byte| byte.is_ascii_hexdigit()),
                    "review commit target has an invalid object ID"
                );
                delivery.effect = Some(ReviewActionEffect {
                    id: format!("review-commit-{}", input.sequence.0),
                    input,
                    kind: "open_commit",
                    url: None,
                    path: None,
                    workspace: Some(directory.to_string_lossy().into_owned()),
                    oid: Some(oid),
                    message: None,
                    level: None,
                });
            }
            ReviewTarget::CommitDiff { oid, anchor } => {
                let result = self
                    .read_commit_diff(
                        &input.document,
                        directory,
                        owner.remote.clone(),
                        oid,
                        anchor,
                    )
                    .await?;
                if let Some(patch) = result {
                    delivery.patch.push(patch);
                }
            }
            ReviewTarget::InlineComment { anchor } => {
                let drafted = self
                    .comment(
                        &input.document,
                        vec![],
                        vec![],
                        crate::review::ReviewCommentCommand::DraftInline { anchor },
                    )
                    .await?;
                if let Some(patch) = drafted.patch {
                    delivery.patch.push(patch);
                }
                delivery.comment = Some(drafted.snapshot);
            }
            ReviewTarget::Browser { url } => {
                validate_browser_url(&url)?;
                delivery.effect = Some(ReviewActionEffect {
                    id: format!("review-browser-{}", input.sequence.0),
                    input,
                    kind: "browser",
                    url: Some(url),
                    path: None,
                    workspace: None,
                    oid: None,
                    message: None,
                    level: None,
                });
            }
            ReviewTarget::ThreadComment {
                thread,
                comment,
                reply,
            } => {
                let imported = self
                    .comment(
                        &input.document,
                        vec![],
                        vec![],
                        crate::review::ReviewCommentCommand::LoadThreadComment {
                            thread_node_id: thread,
                            comment_node_id: comment,
                        },
                    )
                    .await?;
                if let Some(patch) = imported.patch {
                    delivery.patch.push(patch);
                }
                let mut selected = imported.snapshot;
                if reply {
                    match self
                        .comment(
                            &input.document,
                            vec![],
                            vec![],
                            crate::review::ReviewCommentCommand::DraftReply {
                                parent: selected.comment,
                            },
                        )
                        .await
                    {
                        Ok(result) => {
                            if let Some(patch) = result.patch {
                                delivery.patch.push(patch);
                            }
                            selected = result.snapshot;
                        }
                        Err(failure) => {
                            delivery.diagnostic = Some(failure.to_string());
                            return Ok(delivery);
                        }
                    }
                }
                delivery.comment = Some(selected.clone());
                let document = owner.document.lock().expect("review document poisoned");
                if document.input.get(&input.view) == Some(&input.sequence) {
                    let projection = document
                        .projection
                        .as_ref()
                        .context("review presentation disappeared")?;
                    let block =
                        forge_buffer::identity::BlockId(format!("region:{}", selected.region.0));
                    if projection.block(&block).is_some() {
                        let mut input = input;
                        input.revision = projection.revision();
                        input.block = block;
                        input.position = forge_buffer::block::TextPosition { row: 0, column: 0 };
                        input.target = None;
                        delivery.effect = Some(ReviewActionEffect {
                            id: format!("review-comment-{}", input.sequence.0),
                            input,
                            kind: "cursor",
                            url: None,
                            path: None,
                            workspace: None,
                            oid: None,
                            message: None,
                            level: None,
                        });
                    }
                }
            }
            ReviewTarget::ThreadMore { thread, cursor } => {
                let result = self
                    .read_thread(
                        &input.document,
                        directory,
                        owner.remote.clone(),
                        thread,
                        Some(cursor),
                    )
                    .await?;
                if let Some(patch) = result.patch {
                    delivery.patch.push(patch);
                }
            }
        }
        Ok(delivery)
    }

    pub async fn view(
        &self,
        identity: &DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<Option<ReviewMaterialization>> {
        let owner = self.owner(identity)?;
        let width = {
            let mut document = owner.document.lock().expect("review document poisoned");
            let changed = document.views.open(view.clone(), width)?;
            document.input.entry(view).or_insert(InputSequence(0));
            if !changed {
                return Ok(None);
            }
            document
                .views
                .profile()
                .cloned()
                .context("review width owner is missing")?
        };
        self.materialize(identity, width).await.map(Some)
    }

    pub async fn close_view(
        &self,
        identity: &DocumentId,
        view: &ViewId,
    ) -> Result<Option<ReviewMaterialization>> {
        let owner = self.owner(identity)?;
        let width = {
            let mut document = owner.document.lock().expect("review document poisoned");
            document.input.remove(view);
            if !document.views.close(view) {
                return Ok(None);
            }
            document.views.profile().cloned()
        };
        match width {
            Some(width) => self.materialize(identity, width).await.map(Some),
            None => Ok(None),
        }
    }
}

fn lifecycle_choice(
    desired: forge_github::pull_request::DesiredPullRequestState,
) -> ReviewActionChoice {
    use forge_github::pull_request::DesiredPullRequestState;
    match desired {
        DesiredPullRequestState::Draft => ReviewActionChoice {
            key: "d",
            label: "DRAFT",
            value: "lifecycle:draft",
        },
        DesiredPullRequestState::Open => ReviewActionChoice {
            key: "o",
            label: "OPEN",
            value: "lifecycle:open",
        },
        DesiredPullRequestState::Closed => ReviewActionChoice {
            key: "c",
            label: "CLOSED",
            value: "lifecycle:closed",
        },
    }
}

impl ReviewDocument {
    pub(super) fn capture_input(&mut self, input: &DocumentInput) -> Result<ReviewTarget> {
        input.validate()?;
        ensure!(input.document == self.id, "review input document changed");
        let projection = self
            .projection
            .as_ref()
            .context("review presentation is missing")?;
        ensure!(
            projection.revision() == input.revision,
            "review input revision is stale"
        );
        ensure!(
            self.input
                .get(&input.view)
                .is_some_and(|previous| *previous < input.sequence),
            "review input view or sequence is stale"
        );
        let block = projection
            .block(&input.block)
            .context("review input block disappeared")?;
        let row = block
            .text
            .row(input.position.row)
            .context("review input row disappeared")?;
        ensure!(
            row.is_char_boundary(input.position.column),
            "review input byte position is invalid"
        );
        if matches!(
            input.action.as_str(),
            "toggle_visibility" | "toggle_presentation"
        ) {
            self.input.insert(input.view.clone(), input.sequence);
            return Ok(ReviewTarget::DiffView {
                action: if input.action == "toggle_visibility" {
                    forge_diff::view::DiffViewAction::ToggleVisibility
                } else {
                    forge_diff::view::DiffViewAction::TogglePresentation
                },
            });
        }
        if matches!(input.action.as_str(), "source_expand" | "source_collapse") {
            let owner = block
                .metadata
                .content_node
                .as_ref()
                .context("review source section is missing")?;
            let target = self
                .projection_target
                .get(&forge_buffer::identity::TargetId(owner.0.clone()))
                .filter(|target| {
                    matches!(
                        target,
                        ReviewTarget::SourceSection { .. }
                            | ReviewTarget::CommitSource {
                                declaration: Some(_),
                                ..
                            }
                    )
                })
                .cloned()
                .context("review source section is no longer retained")?;
            self.input.insert(input.view.clone(), input.sequence);
            return Ok(target);
        }
        if input.action == "browse" {
            let selected = if let Some(target) = &input.target {
                ensure!(
                    block.target_at(input.position) == Some(target),
                    "review input target does not match captured position"
                );
                self.projection_target.get(target).cloned().or_else(|| {
                    self.projection_link
                        .get(&input.block)?
                        .iter()
                        .find(|link| &link.target == target)
                        .map(|link| ReviewTarget::Browser {
                            url: link.destination.clone(),
                        })
                })
            } else {
                None
            };
            let target = match selected {
                Some(target @ ReviewTarget::Browser { .. }) => target,
                _ => ReviewTarget::Browser {
                    url: format!(
                        "https://{}/{}/pull/{}",
                        self.target.repository.hostname(),
                        self.target.repository.repository_name(),
                        self.target.number
                    ),
                },
            };
            self.input.insert(input.view.clone(), input.sequence);
            return Ok(target);
        }
        let target = input
            .target
            .as_ref()
            .context("review input target is missing")?;
        ensure!(
            block.metadata.target.iter().any(|range| &range.id == target
                && range.range.start <= input.position
                && input.position < range.range.end),
            "review input target does not match captured position"
        );
        let selected = self
            .projection_target
            .get(target)
            .cloned()
            .or_else(|| {
                self.projection_link
                    .get(&input.block)?
                    .iter()
                    .find(|link| &link.target == target)
                    .map(|link| ReviewTarget::Browser {
                        url: link.destination.clone(),
                    })
            })
            .context("review input target is not retained")?;
        match &selected {
            ReviewTarget::ThreadComment {
                thread,
                comment,
                reply,
            } => {
                let thread = self.retained_thread(thread)?;
                let member = thread
                    .comment
                    .iter()
                    .find(|member| member.node_id == *comment)
                    .context("review input comment is no longer retained")?;
                ensure!(
                    if *reply {
                        thread.can_reply
                    } else {
                        member.viewer_did_author
                    },
                    "review input comment permission changed"
                );
            }
            ReviewTarget::ThreadMore { thread, cursor } => {
                ensure!(
                    self.retained_thread(thread)?.next_cursor.as_ref() == Some(cursor),
                    "review input continuation is stale"
                );
            }
            ReviewTarget::FileDiff { .. }
            | ReviewTarget::FileMore { .. }
            | ReviewTarget::WorkspaceFile { .. }
            | ReviewTarget::CommitMessage { .. }
            | ReviewTarget::CommitDiff { .. }
            | ReviewTarget::InlineComment { .. }
            | ReviewTarget::Lifecycle { .. }
            | ReviewTarget::DiffView { .. }
            | ReviewTarget::SourceSection { .. }
            | ReviewTarget::CommitSource { .. }
            | ReviewTarget::Browser { .. } => {}
        }
        self.input.insert(input.view.clone(), input.sequence);
        Ok(selected)
    }

    fn retained_thread(&self, identity: &str) -> Result<&super::thread::ReviewThread> {
        self.section
            .get(&super::ReviewSectionKind::Threads)
            .and_then(|section| {
                section
                    .item
                    .iter()
                    .filter_map(|item| item.thread.as_deref())
                    .find(|thread| thread.node_id == identity)
            })
            .context("review input thread is no longer retained")
    }
}

fn workspace_file_path(workspace: &std::path::Path, relative: &str) -> Result<String> {
    ensure!(
        !relative.is_empty() && !relative.contains('\0'),
        "review working-file target has an invalid path"
    );
    let relative = std::path::Path::new(relative);
    ensure!(
        relative
            .components()
            .all(|component| matches!(component, std::path::Component::Normal(_))),
        "review working-file target escapes its captured workspace"
    );
    let path = workspace.join(relative);
    let bytes = path.as_os_str().as_encoded_bytes();
    ensure!(
        path.is_absolute() && !bytes.contains(&0),
        "review working-file path is invalid"
    );
    Ok(path.to_string_lossy().into_owned())
}

fn validate_browser_url(url: &str) -> Result<()> {
    let authority = url
        .strip_prefix("https://")
        .or_else(|| url.strip_prefix("http://"))
        .and_then(|remainder| remainder.split('/').next());
    ensure!(
        !url.contains(char::is_control) && authority.is_some_and(|authority| !authority.is_empty()),
        "review browser target has an invalid URL"
    );
    Ok(())
}
