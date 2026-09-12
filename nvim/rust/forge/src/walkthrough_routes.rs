use std::path::PathBuf;

use anyhow::Result;
use forge_buffer::{
    identity::{DocumentId, ViewId},
    input::DocumentInput,
    width::WidthProfile,
};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::runtime::ForgeRuntime;

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum WalkthroughRequest {
    Open {
        document: DocumentId,
        workspace: PathBuf,
        inventory: Option<bool>,
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
    Change {
        input: DocumentInput,
        document: DocumentId,
        annotation_document: DocumentId,
        review: Option<bool>,
    },
    AnnotationSource {
        document: DocumentId,
        input: DocumentInput,
    },
    AnnotationSnapshot {
        document: DocumentId,
        annotation: DocumentId,
    },
    AnnotationView {
        document: DocumentId,
        annotation: DocumentId,
        view: ViewId,
        width: WidthProfile,
    },
    AnnotationCloseView {
        document: DocumentId,
        annotation: DocumentId,
        view: ViewId,
    },
    AnnotationClose {
        document: DocumentId,
        annotation: DocumentId,
    },
    Close {
        document: DocumentId,
    },
}

#[derive(Serialize)]
struct WalkthroughSource {
    #[serde(flatten)]
    source: forge_status::source_document::SourceOpen,
    annotation: forge_buffer::patch::BufferSnapshot,
    source_row: usize,
    stale: bool,
    review: bool,
}

pub(super) async fn route(host: &ForgeRuntime, params: Value) -> Result<Value> {
    Ok(match serde_json::from_value(params)? {
        WalkthroughRequest::Open {
            document,
            workspace,
            inventory,
        } => serde_json::to_value(
            host.walkthrough
                .open(document, workspace, inventory.unwrap_or(true))
                .await?,
        )?,
        WalkthroughRequest::Snapshot { document } => {
            serde_json::to_value(host.walkthrough.snapshot(&document)?)?
        }
        WalkthroughRequest::View {
            document,
            view,
            width,
        } => serde_json::to_value(host.walkthrough.view(&document, view, width)?)?,
        WalkthroughRequest::CloseView { document, view } => {
            serde_json::to_value(host.walkthrough.close_view(&document, &view)?)?
        }
        WalkthroughRequest::Change {
            input,
            document,
            annotation_document,
            review,
        } => {
            let parent = input.document.clone();
            let mut change = host
                .walkthrough
                .resolve_change(input.clone(), annotation_document.clone())
                .await?;
            if review.unwrap_or(false) {
                if let Err(failure) = host
                    .walkthrough
                    .project_review(
                        &input,
                        &annotation_document,
                        &mut change,
                        &host.diff,
                        &host.syntax,
                    )
                    .await
                {
                    let _ = host
                        .walkthrough
                        .close_annotation(&parent, &annotation_document);
                    return Err(failure);
                }
            }
            let source = match host
                .sources
                .open_version(document, change.path, change.source)
                .await
            {
                Ok(source) => source,
                Err(failure) => {
                    host.walkthrough
                        .close_annotation(&parent, &annotation_document)?;
                    return Err(failure);
                }
            };
            serde_json::to_value(WalkthroughSource {
                source,
                annotation: change.annotation,
                source_row: change.row,
                stale: change.stale,
                review: review.unwrap_or(false),
            })?
        }
        WalkthroughRequest::AnnotationSource { document, input } => {
            json!({"source_row": host.walkthrough.annotation_source(&document, input)?})
        }
        WalkthroughRequest::AnnotationSnapshot {
            document,
            annotation,
        } => serde_json::to_value(
            host.walkthrough
                .annotation_snapshot(&document, &annotation)?,
        )?,
        WalkthroughRequest::AnnotationView {
            document,
            annotation,
            view,
            width,
        } => serde_json::to_value(host.walkthrough.annotation_view(
            &document,
            &annotation,
            view,
            Some(width),
        )?)?,
        WalkthroughRequest::AnnotationCloseView {
            document,
            annotation,
            view,
        } => serde_json::to_value(host.walkthrough.annotation_view(
            &document,
            &annotation,
            view,
            None,
        )?)?,
        WalkthroughRequest::AnnotationClose {
            document,
            annotation,
        } => {
            json!({"closed":host.walkthrough.close_annotation(&document, &annotation)?})
        }
        WalkthroughRequest::Close { document } => {
            json!({"closed":host.walkthrough.close_collected(&document).await})
        }
    })
}
