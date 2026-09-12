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
        let repository = owner
            .document
            .lock()
            .expect("review document poisoned")
            .target
            .repository
            .clone();
        let detail = self
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
            let comparison = self
                .github
                .review_comparison(
                    directory,
                    remote,
                    ReviewComparisonRequest {
                        repository: repository.clone(),
                        base: parent.clone(),
                        head: detail.commit.clone(),
                    },
                )
                .await?;
            ensure!(
                comparison.base == *parent
                    && comparison.head == detail.commit
                    && comparison.merge_base == *parent,
                "commit expansion is not a direct parent comparison"
            );
        }
        let mut blocks = Vec::new();
        let mut document = owner.document.lock().expect("review document poisoned");
        if detail.parent.is_none() {
            document.commit_diff.retain_root(&detail, "(root)".into())?;
            blocks.push(message_block(
                &commit,
                "Commit diff unavailable: root commit has no parent",
            )?);
        } else if detail.file.is_empty() {
            document
                .commit_diff
                .retain_subject(&detail, "(empty)".into())?;
            blocks.push(message_block(&commit, "No changed files")?);
        } else {
            let retained = detail
                .file
                .iter()
                .map(|file| {
                    let patch = file
                        .patch
                        .as_deref()
                        .unwrap_or("Diff unavailable for this file");
                    (file.path.clone(), patch.as_bytes().to_vec())
                })
                .collect();
            document
                .commit_diff
                .retain_expanded_batch(&detail, retained)?;
            for file in &detail.file {
                let patch = file
                    .patch
                    .as_deref()
                    .unwrap_or("Diff unavailable for this file");
                blocks.push(file_block(&commit, &file.path, file.status.as_str())?);
                blocks.push(diff_block(&commit, &file.path, patch)?);
            }
        }
        let previous = document.commit_projected.get(&commit).copied().unwrap_or(0);
        let projection = document
            .projection
            .as_mut()
            .context("review presentation disappeared")?;
        let anchor = projection
            .block_index(&anchor)
            .context("commit presentation anchor disappeared")?;
        let count = blocks.len();
        let patch = projection.edit(anchor + 1..anchor + 1 + previous, blocks)?;
        document.commit_projected.insert(commit, count);
        Ok(patch)
    }
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

fn diff_block(commit: &str, path: &str, patch: &str) -> Result<BufferBlock> {
    let rows = patch
        .replace("\r\n", "\n")
        .replace('\r', "\n")
        .lines()
        .map(|row| format!("    {row}"))
        .collect::<Vec<_>>();
    Ok(BufferBlock {
        id: BlockId(format!("commit:{commit}:diff:{path}")),
        text: BufferText::from_rows(rows)?,
        metadata: BlockMetadata::default(),
    })
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

        let file = file_block(&commit, "src/lib.rs", "modified").unwrap();
        let diff = diff_block(&commit, "src/lib.rs", "@@ -1 +1 @@\r\n-old\r+new").unwrap();
        assert_eq!(file.text.wire_rows(), ["  modified: src/lib.rs"]);
        assert_eq!(
            diff.text.wire_rows(),
            ["    @@ -1 +1 @@", "    -old", "    +new"]
        );
    }
}
