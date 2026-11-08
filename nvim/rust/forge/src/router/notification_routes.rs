use std::path::PathBuf;

use anyhow::Result;
use forge_buffer::{
    identity::{DocumentId, ViewId},
    input::DocumentInput,
    width::WidthProfile,
};
use serde::Deserialize;
use serde_json::{Value, json};

use crate::runtime::ForgeRuntime;

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum NotificationRequest {
    Open {
        document: DocumentId,
        workspace: PathBuf,
        hostname: String,
    },
    Snapshot {
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
    Close {
        document: DocumentId,
    },
}

pub(super) async fn route(host: &ForgeRuntime, params: Value) -> Result<Value> {
    Ok(match serde_json::from_value(params)? {
        NotificationRequest::Open {
            document,
            workspace,
            hostname,
        } => {
            let remote = host.github_remote.for_directory(workspace.clone())?;
            serde_json::to_value(
                host.notifications
                    .open(document, workspace, hostname, remote)
                    .await?,
            )?
        }
        NotificationRequest::Snapshot { document } => {
            serde_json::to_value(host.notifications.snapshot(&document)?)?
        }
        NotificationRequest::View {
            document,
            view,
            width,
        } => serde_json::to_value(host.notifications.view(&document, view, width)?)?,
        NotificationRequest::CloseView { document, view } => {
            serde_json::to_value(host.notifications.close_view(&document, &view)?)?
        }
        NotificationRequest::Act { input } => {
            serde_json::to_value(host.notifications.act(input).await?)?
        }
        NotificationRequest::Close { document } => {
            json!({"closed":host.notifications.close_collected(&document).await})
        }
    })
}
