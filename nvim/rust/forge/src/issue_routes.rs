use std::{path::PathBuf, time::Duration};

use anyhow::Result;
use forge_buffer::{
    identity::{DocumentId, ViewId},
    input::DocumentInput,
    width::WidthProfile,
};
use forge_github::{issue_store::IssueStore, model::GithubRepositoryId};
use forge_review::edit::RegionEdit;
use serde::Deserialize;
use serde_json::{Value, json};

use crate::runtime::ForgeRuntime;

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum IssueRequest {
    Open {
        document: DocumentId,
        directory: PathBuf,
        database: PathBuf,
        repository: GithubRepositoryId,
        number: u64,
    },
    Snapshot {
        document: DocumentId,
    },
    Edit {
        edit: RegionEdit,
    },
    Save {
        document: DocumentId,
    },
    Refresh {
        document: DocumentId,
    },
    View {
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    },
    CloseView {
        document: DocumentId,
        view: ViewId,
    },
    Act {
        input: DocumentInput,
    },
    Resolve {
        document: DocumentId,
        operation_id: String,
        resolution: forge_github::review_mutation::RecoveryResolution,
    },
    Close {
        document: DocumentId,
    },
}

pub(super) async fn route(host: &ForgeRuntime, params: Value) -> Result<Value> {
    Ok(match serde_json::from_value(params)? {
        IssueRequest::Open {
            document,
            directory,
            database,
            repository,
            number,
        } => {
            let store = IssueStore::new(
                database,
                &repository.repository_name(),
                Duration::from_secs(1),
            )?;
            let remote = host.github_remote.for_directory(directory)?;
            serde_json::to_value(
                host.issues
                    .open(document, store, repository, number, remote)
                    .await?,
            )?
        }
        IssueRequest::Snapshot { document } => {
            serde_json::to_value(host.issues.snapshot(&document).await?)?
        }
        IssueRequest::Edit { edit } => serde_json::to_value(host.issues.edit(edit).await?)?,
        IssueRequest::Save { document } => {
            serde_json::to_value(host.issues.save(&document).await?)?
        }
        IssueRequest::Refresh { document } => {
            serde_json::to_value(host.issues.refresh(&document).await?)?
        }
        IssueRequest::View {
            document,
            view,
            width,
        } => serde_json::to_value(host.issues.view(&document, view, width).await?)?,
        IssueRequest::CloseView { document, view } => {
            serde_json::to_value(host.issues.close_view(&document, &view).await?)?
        }
        IssueRequest::Act { input } => serde_json::to_value(host.issues.act(input).await?)?,
        IssueRequest::Resolve {
            document,
            operation_id,
            resolution,
        } => serde_json::to_value(
            host.issues
                .resolve(&document, operation_id, resolution)
                .await?,
        )?,
        IssueRequest::Close { document } => {
            json!({"collected":host.issues.close_collected(&document).await})
        }
    })
}
