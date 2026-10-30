use std::{path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, Decoration, TextPosition, TextRange},
    identity::{BlockId, DocumentId},
    text::BufferText,
};
use forge_github::{
    review_api::GithubReviewRemote,
    review_source::{ReviewCommitDetailRequest, ReviewComparisonRequest},
};

use super::ReviewService;

#[derive(Clone)]
pub(crate) struct RetainedCommit {
    pub anchor: BlockId,
    pub file: Vec<CommitFile>,
    pub block: Vec<BufferBlock>,
    pub target: std::collections::BTreeMap<
        forge_buffer::identity::TargetId,
        super::thread_projection::ReviewTarget,
    >,
    pub projected_blocks: usize,
}

#[derive(Clone)]
pub(crate) enum CommitFile {
    Source {
        status: String,
        file: Box<super::file::RetainedFile>,
    },
    Raw(Vec<BufferBlock>),
}

impl ReviewService {
    pub async fn read_commit_diff(
        &self,
        id: &DocumentId,
        directory: PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        commit: String,
        anchor: BlockId,
    ) -> Result<Option<forge_buffer::patch::BufferPatch>> {
        let owner = self.owner(id)?;
        owner.validate_directory(&directory)?;
        let repository = {
            let document = owner.document.lock().expect("review document poisoned");
            if document.commit_view.contains_key(&commit) {
                return Ok(None);
            }
            document.target.repository.clone()
        };
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review commit admission is full")?;
        let service = self.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        self.spawn(async move {
            let _admission = admission;
            let result = async {
                let detail = service
                    .github
                    .review_commit_detail(
                        directory.clone(),
                        remote.clone(),
                        ReviewCommitDetailRequest {
                            repository: repository.clone(),
                            commit: commit.clone(),
                        },
                    )
                    .await?;
                ensure!(
                    detail.commit == commit,
                    "commit detail changed requested identity"
                );
                if let Some(parent) = &detail.parent {
                    let comparison = service
                        .github
                        .review_comparison(
                            directory.clone(),
                            remote.clone(),
                            ReviewComparisonRequest {
                                repository: repository.clone(),
                                base: parent.clone(),
                                head: commit.clone(),
                            },
                        )
                        .await?;
                    ensure!(
                        comparison.base == *parent
                            && comparison.head == commit
                            && comparison.merge_base == *parent,
                        "commit expansion is not a direct parent comparison"
                    );
                }
                let mut view = RetainedCommit {
                    anchor,
                    file: Vec::new(),
                    block: Vec::new(),
                    target: Default::default(),
                    projected_blocks: 0,
                };
                if detail.parent.is_none() {
                    view.file.push(CommitFile::Raw(vec![message_block(
                        &commit,
                        "Commit diff unavailable: root commit has no parent",
                    )?]));
                } else if detail.file.is_empty() {
                    view.file.push(CommitFile::Raw(vec![message_block(
                        &commit,
                        "No changed files",
                    )?]));
                } else {
                    for file in &detail.file {
                        let acquired = service
                            .acquire_commit_file(
                                &directory,
                                remote.clone(),
                                &repository,
                                &detail,
                                file,
                            )
                            .await;
                        match acquired {
                            Ok(source) => view.file.push(CommitFile::Source {
                                status: file.status.clone(),
                                file: Box::new(source),
                            }),
                            Err(failure) => {
                                let mut fallback =
                                    vec![file_block(&commit, &file.path, &file.status)?];
                                fallback.push(BufferBlock {
                                    id: BlockId(format!("commit:{commit}:fallback:{}", file.path)),
                                    text: BufferText::from_rows(
                                        format!("Unclassified source changes: {failure}").lines(),
                                    )?,
                                    metadata: BlockMetadata::default(),
                                });
                                fallback.extend(diff_blocks(
                                    &commit,
                                    &file.path,
                                    file.patch
                                        .as_deref()
                                        .unwrap_or("Diff unavailable for this file"),
                                )?);
                                view.file.push(CommitFile::Raw(fallback));
                            }
                        }
                    }
                }
                let _projection = owner.section_projection.lock().await;
                let options = {
                    let document = owner.document.lock().expect("review document poisoned");
                    if document.commit_view.contains_key(&commit) {
                        return Ok(None);
                    }
                    document.diff_view.committed()
                };
                let pool = service
                    .analysis
                    .clone()
                    .context("review analysis service is unavailable")?;
                view = prepare_commit_view(pool, view, options).await?;
                let mut document = owner.document.lock().expect("review document poisoned");
                if detail.parent.is_none() {
                    document.commit_diff.retain_root(&detail, "(root)".into())?;
                } else if detail.file.is_empty() {
                    document
                        .commit_diff
                        .retain_subject(&detail, "(empty)".into())?;
                } else {
                    document.commit_diff.retain_expanded_batch(
                        &detail,
                        detail
                            .file
                            .iter()
                            .map(|file| {
                                (
                                    file.path.clone(),
                                    file.patch
                                        .as_deref()
                                        .unwrap_or("Diff unavailable for this file")
                                        .as_bytes()
                                        .to_vec(),
                                )
                            })
                            .collect(),
                    )?;
                }
                publish_commit(&mut document, commit, view)
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver.await.context("review commit task ended")?
    }

    async fn acquire_commit_file(
        &self,
        directory: &std::path::Path,
        remote: Arc<dyn GithubReviewRemote>,
        repository: &forge_github::model::GithubRepositoryId,
        detail: &forge_github::review_source::ReviewCommitDetail,
        file: &forge_github::review_source::ReviewCommitFile,
    ) -> Result<super::file::RetainedFile> {
        use forge_github::review_source::ReviewSourceRequest;
        let parent = detail.parent.as_ref().context("commit has no parent")?;
        let diff = self
            .diff
            .as_ref()
            .context("review diff service is unavailable")?;
        let syntax = self
            .syntax
            .as_ref()
            .context("review syntax service is unavailable")?;
        let old = self
            .github
            .review_source(
                directory.to_path_buf(),
                remote.clone(),
                ReviewSourceRequest {
                    repository: repository.clone(),
                    commit: parent.clone(),
                    path: file
                        .previous_path
                        .clone()
                        .unwrap_or_else(|| file.path.clone()),
                },
            )
            .await?;
        let new = self
            .github
            .review_source(
                directory.to_path_buf(),
                remote,
                ReviewSourceRequest {
                    repository: repository.clone(),
                    commit: detail.commit.clone(),
                    path: file.path.clone(),
                },
            )
            .await?;
        ensure!(
            old.blob.is_none() == (file.status == "added"),
            "old commit file presence disagrees with the captured file status"
        );
        ensure!(
            new.blob.is_none() == (file.status == "removed"),
            "new commit file presence disagrees with the captured file status"
        );
        let identity = super::ReviewComparisonIdentity {
            base_commit: parent.clone(),
            head_commit: detail.commit.clone(),
            head_repository: repository.repository_name(),
        };
        let summary = super::file::ReviewFileAnalysis {
            path: file.path.clone(),
            base_commit: parent.clone(),
            head_commit: detail.commit.clone(),
            merge_base: parent.clone(),
            old_blob: old.blob,
            new_blob: new.blob,
            syntax_diagnostic: None,
            more: false,
        };
        super::file::analyze_file(
            diff,
            syntax,
            identity,
            summary,
            old.bytes,
            new.bytes,
            Some(detail.commit.clone()),
        )
        .await
    }

    pub(super) async fn change_commit_source(
        &self,
        id: &DocumentId,
        commit: String,
        path: String,
        change: super::file::FileViewChange,
    ) -> Result<Option<forge_buffer::patch::BufferPatch>> {
        let owner = self.owner(id)?;
        let pool = self
            .analysis
            .clone()
            .context("review analysis service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review commit admission is full")?;
        let (sender, receiver) = tokio::sync::oneshot::channel();
        self.spawn(async move {
            let _admission = admission;
            let _projection = owner.section_projection.lock().await;
            let result = async {
                let mut view = owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .commit_view
                    .get(&commit)
                    .cloned()
                    .context("commit diff is no longer retained")?;
                let selected = view
                    .file
                    .iter_mut()
                    .find_map(|entry| match entry {
                        CommitFile::Source { file, .. } if file.summary.path == path => Some(file),
                        _ => None,
                    })
                    .context("commit source is no longer retained")?;
                **selected =
                    super::file::prepare_file_view(pool, (**selected).clone(), change).await?;
                render_commit(&mut view)?;
                let mut document = owner.document.lock().expect("review document poisoned");
                publish_commit(&mut document, commit, view)
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver.await.context("review commit source task ended")?
    }
}

pub(super) async fn prepare_commit_view(
    pool: Arc<forge_diff::workers::AnalysisPool>,
    mut view: RetainedCommit,
    options: forge_diff::view::DiffViewOptions,
) -> Result<RetainedCommit> {
    for entry in &mut view.file {
        if let CommitFile::Source { file, .. } = entry
            && file
                .source_projection
                .as_ref()
                .is_some_and(|projection| projection.options() != options)
        {
            **file = super::file::prepare_file_view(
                pool.clone(),
                (**file).clone(),
                super::file::FileViewChange::Options(options),
            )
            .await?;
        }
    }
    render_commit(&mut view)?;
    Ok(view)
}

fn render_commit(view: &mut RetainedCommit) -> Result<()> {
    use forge_buffer::{
        block::{ContentLayout, TargetRange},
        identity::TargetId,
    };
    view.block.clear();
    view.target.clear();
    for entry in &view.file {
        match entry {
            CommitFile::Raw(block) => view.block.extend(block.iter().cloned()),
            CommitFile::Source { file, status } => {
                let commit = file
                    .commit
                    .as_ref()
                    .context("commit source scope is missing")?;
                view.block
                    .push(file_block(commit, &file.summary.path, status)?);
                view.block
                    .extend(file.block.iter().cloned().map(|mut block| {
                        block.metadata.layout = Some(ContentLayout {
                            indent: 6,
                            marker: None,
                            source_indent: 0,
                        });
                        block
                    }));
                view.target.extend(
                    file.target
                        .iter()
                        .map(|(id, target)| (id.clone(), target.clone())),
                );
                if file.summary.more {
                    let id = format!("commit:{commit}:more:{}", file.summary.path);
                    let target = TargetId(id.clone());
                    view.target.insert(
                        target.clone(),
                        super::thread_projection::ReviewTarget::CommitSource {
                            commit: commit.clone(),
                            path: file.summary.path.clone(),
                            declaration: None,
                        },
                    );
                    view.block.push(BufferBlock {
                        id: BlockId(id),
                        text: BufferText::from_rows(["Additional diff rows are available"])?,
                        metadata: BlockMetadata {
                            target: vec![TargetRange {
                                id: target,
                                range: TextRange {
                                    start: TextPosition { row: 0, column: 0 },
                                    end: TextPosition { row: 1, column: 0 },
                                },
                            }],
                            ..Default::default()
                        },
                    });
                }
            }
        }
    }
    Ok(())
}

pub(super) fn commit_patch(
    projection: &mut forge_buffer::document::BufferDocument,
    view: &mut RetainedCommit,
    previous: usize,
) -> Result<Option<forge_buffer::patch::BufferPatch>> {
    view.projected_blocks = 0;
    let Some(anchor) = projection.block_index(&view.anchor) else {
        return Ok(None);
    };
    let patch = projection.splice(anchor + 1..anchor + 1 + previous, view.block.clone())?;
    view.projected_blocks = view.block.len();
    Ok(patch)
}

fn publish_commit(
    document: &mut crate::review::ReviewDocument,
    commit: String,
    mut view: RetainedCommit,
) -> Result<Option<forge_buffer::patch::BufferPatch>> {
    let next = document
        .section_revision
        .checked_add(1)
        .context("review section revision exhausted")?;
    let previous = document
        .commit_view
        .get(&commit)
        .map_or(0, |view| view.projected_blocks);
    let mut projection = document
        .projection
        .clone()
        .context("review presentation disappeared")?;
    ensure!(
        projection.block_index(&view.anchor).is_some(),
        "commit presentation anchor disappeared"
    );
    let patch = commit_patch(&mut projection, &mut view, previous)?;
    if let Some(previous) = document.commit_view.get(&commit) {
        for target in previous.target.keys() {
            document.projection_target.remove(target);
        }
    }
    document.projection_target.extend(
        view.target
            .iter()
            .map(|(id, target)| (id.clone(), target.clone())),
    );
    document.projection = Some(projection);
    document.commit_view.insert(commit, view);
    document.section_revision = next;
    Ok(patch)
}

fn message_block(commit: &str, message: &str) -> Result<BufferBlock> {
    Ok(BufferBlock {
        id: BlockId(format!("commit:{commit}:message")),
        text: BufferText::from_rows([format!("  {message}")])?,
        metadata: BlockMetadata::default(),
    })
}

fn file_block(commit: &str, path: &str, status: &str) -> Result<BufferBlock> {
    let text = format!("  {status}: {path}");
    Ok(BufferBlock {
        id: BlockId(format!("commit:{commit}:file:{path}")),
        text: BufferText::from_rows([text.clone()])?,
        metadata: BlockMetadata {
            decoration: vec![Decoration {
                range: TextRange {
                    start: TextPosition { row: 0, column: 2 },
                    end: TextPosition {
                        row: 0,
                        column: text.len(),
                    },
                },
                capture: "ForgeStatusPath".into(),
                priority: 100,
            }],
            ..Default::default()
        },
    })
}

fn diff_blocks(commit: &str, path: &str, patch: &str) -> Result<Vec<BufferBlock>> {
    let normalized = patch.replace("\r\n", "\n").replace('\r', "\n");
    let prefix = format!("commit:{commit}:diff:{path}");
    let encoded_path = serde_json::to_string(path)?;
    let source = format!("--- {encoded_path}\n+++ {encoded_path}\n{normalized}\n");
    let parsed = normalized
        .starts_with("@@")
        .then(|| forge_diff::patch::UnifiedPatch::parse(&source));
    let patch = match parsed {
        Some(Ok(patch)) => patch,
        _ => {
            return Ok(vec![BufferBlock {
                id: BlockId(prefix),
                text: BufferText::from_rows(normalized.lines())?,
                metadata: BlockMetadata {
                    layout: Some(forge_buffer::block::ContentLayout {
                        indent: 6,
                        marker: None,
                        source_indent: 0,
                    }),
                    ..Default::default()
                },
            }]);
        }
    };
    let presenter = forge_diff::presentation::DiffPresenter::new(
        forge_diff::presentation::PresentationOptions {
            layout: Some(forge_buffer::block::ContentLayout {
                indent: 6,
                marker: None,
                source_indent: 0,
            }),
            ..Default::default()
        },
    );
    let mut blocks = Vec::new();
    for (index, hunk) in patch.file[0].hunk.iter().enumerate() {
        let (group, rows) = forge_diff::presentation::patch_rows(hunk);
        blocks.push(presenter.hunk(
            BlockId(format!("{prefix}:hunk:{index}")),
            &group,
            forge_diff::presentation::HunkHeading::Saved(&hunk.header),
        )?);
        for (batch, rows) in rows.chunks(128).enumerate() {
            blocks.push(presenter.rows(
                BlockId(format!("{prefix}:hunk:{index}:rows:{batch}")),
                rows,
                &group,
                None,
                None,
            )?);
        }
    }
    Ok(blocks)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_boundary_and_commit_patch_rows_have_stable_child_identities() {
        let commit = "a".repeat(40);
        let root = message_block(
            &commit,
            "Commit diff unavailable: root commit has no parent",
        )
        .unwrap();
        assert_eq!(root.id.0, format!("commit:{commit}:message"));
        assert_eq!(
            root.text.wire_rows(),
            ["  Commit diff unavailable: root commit has no parent"]
        );

        let malformed = diff_blocks(
            &commit,
            "src/lib.rs",
            "@@ malformed\n-complete raw text\n+retained",
        )
        .unwrap();
        assert_eq!(
            malformed[0].text.wire_rows(),
            ["@@ malformed", "-complete raw text", "+retained"]
        );
        let file = file_block(&commit, "src/lib.rs", "modified").unwrap();
        let diff = diff_blocks(&commit, "src/lib.rs", "@@ -1 +1 @@\r\n-old\r+new").unwrap();
        assert_eq!(file.text.wire_rows(), ["  modified: src/lib.rs"]);
        assert!(
            diff.iter()
                .all(|block| block.metadata.layout.as_ref().unwrap().indent == 6)
        );
        assert_eq!(diff[1].metadata.gutter.len(), 2);
        assert_eq!(
            diff.iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>(),
            ["@@ -1 +1 @@", "old", "new"]
        );
    }
}
