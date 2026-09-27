use std::sync::Arc;

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryPath,
    repository::{RepositoryRead, RepositoryState},
    resolve_argument,
    store::RepositoryStore,
};

pub mod comparison;

#[derive(Clone, Debug)]
pub enum RevisionOrigin {
    Commit(gix::ObjectId),
    Index,
    Object(gix::ObjectId),
}

#[derive(Clone, Debug)]
pub struct ResolvedFile {
    pub origin: RevisionOrigin,
    pub blob: gix::ObjectId,
}

pub async fn resolve_file(
    store: &RepositoryStore,
    repository: Arc<RepositoryState>,
    reference: String,
    path: RepositoryPath,
) -> Result<RepositoryRead<ResolvedFile>> {
    ensure!(
        !reference.is_empty()
            && reference.len() <= 4096
            && !reference.chars().any(char::is_control),
        "invalid revision reference"
    );
    let path_argument = resolve_argument(&path)?;
    let input_bytes = reference.capacity() + path.retained_bytes();
    store
        .read(repository, input_bytes, move |local, cancellation| {
            if let Some(object) = reference.strip_prefix("object:") {
                let blob = gix::ObjectId::from_hex(object.as_bytes())
                    .context("invalid captured blob identity")?;
                return Ok(ResolvedFile {
                    origin: RevisionOrigin::Object(blob),
                    blob,
                });
            }
            local.workdir().context("file revision requires worktree")?;
            cancellation.check()?;
            let commit = if reference == ":0" {
                None
            } else {
                let spec = format!("{reference}^{{commit}}");
                Some(
                    local
                        .rev_parse_single(spec.as_str())
                        .context("revision is unavailable")?
                        .detach(),
                )
            };
            cancellation.check()?;
            let blob = if let Some(commit) = commit {
                let tree = local
                    .find_object(commit)
                    .context("read revision commit")?
                    .peel_to_tree()
                    .context("read revision tree")?;
                tree.lookup_entry_by_path(&path_argument)
                    .context("look up revision file")?
                    .context("revision file is unavailable")?
                    .object_id()
            } else {
                let index = local.index_or_empty().context("read revision index")?;
                index
                    .entry_by_path_and_stage(
                        gix::bstr::BStr::new(path.raw()),
                        gix::index::entry::Stage::Unconflicted,
                    )
                    .context("revision file is unavailable")?
                    .id
            };
            cancellation.check()?;
            Ok(ResolvedFile {
                origin: commit.map_or(RevisionOrigin::Index, RevisionOrigin::Commit),
                blob,
            })
        })
        .await
}
