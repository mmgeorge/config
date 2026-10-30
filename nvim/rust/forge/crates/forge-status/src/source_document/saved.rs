use super::{BodyState, SourceDelivery, SourceDocument, SourceDocumentService, SourceOpen};
use anyhow::{Context, Result, ensure};
use forge_buffer::{
    document::BufferDocument,
    identity::{DocumentId, TargetId},
    input::DocumentInput,
    patch::BufferPatch,
};
use forge_diff::{
    display::ChunkLimits,
    engine::DiffEngine,
    saved_projection::{SavedDiffProjection, SavedDiffTarget},
    saved_source::SavedDiffSource,
    source::{Representation, SourceVersion},
    syntax::SyntaxEngine,
    view::{DiffCapabilities, DiffViewAction, DiffViewOptions, DiffViewState, DiffVisibility},
    workers::{AnalysisPool, WorkBudget, WorkPriority},
};
use std::{
    collections::BTreeMap,
    sync::{Arc, atomic::Ordering},
};

pub(super) struct SavedDocument {
    model: SavedDiffProjection,
    target: BTreeMap<TargetId, SavedDiffTarget>,
    operation: Arc<tokio::sync::Mutex<()>>,
}

enum SavedChange {
    Options(DiffViewOptions),
    Demand(Option<String>),
    Section(SavedDiffTarget, bool),
}

enum SavedOpening {
    Prepared {
        original: SourceVersion,
        object: String,
        prepared: Box<PreparedSaved>,
    },
    Raw {
        original: SourceVersion,
        diagnostic: String,
    },
}

struct PreparedSaved {
    model: SavedDiffProjection,
    target: BTreeMap<TargetId, SavedDiffTarget>,
    buffer: BufferDocument,
    patch: Option<BufferPatch>,
}

impl SourceDocumentService {
    pub async fn open_saved_diff(
        &self,
        id: DocumentId,
        title: String,
        source: SavedDiffSource,
    ) -> Result<SourceOpen> {
        id.validate()?;
        ensure!(!title.contains(['\0', '\n', '\r']), "invalid source title");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let diff = self.diff.clone();
        let syntax = self.syntax.clone();
        let document_id = id.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let prepared = async {
                let input_bytes = source.text.len();
                let (original, model) =
                    saved_worker(diff.analysis_pool(), input_bytes, move || {
                        let original =
                            SourceVersion::from_shared(source.text.clone(), Representation::Raw)?;
                        Ok((original, SavedDiffProjection::new(source)))
                    })
                    .await?;
                let model = match model {
                    Ok(model) => model,
                    Err(diagnostic) => {
                        return Ok(SavedOpening::Raw {
                            original,
                            diagnostic,
                        });
                    }
                };
                let object = saved_worker(diff.analysis_pool(), 0, {
                    let model = model.clone();
                    move || Ok(model.identity())
                })
                .await?;
                match prepare_saved(
                    model,
                    BufferDocument::new(document_id, Vec::new())?,
                    SavedChange::Demand(None),
                    diff,
                    syntax,
                )
                .await
                {
                    Ok(prepared) => Ok::<_, anyhow::Error>(SavedOpening::Prepared {
                        original,
                        object,
                        prepared: Box::new(prepared),
                    }),
                    Err(failure) => Ok(SavedOpening::Raw {
                        original,
                        diagnostic: format!("{failure:#}"),
                    }),
                }
            }
            .await;
            let _ = sender.send((admission, prepared));
        });
        let (admission, prepared) = receiver.await.context("saved source opening task ended")?;
        let (original, object, prepared) = match prepared? {
            SavedOpening::Prepared {
                original,
                object,
                prepared,
            } => (original, object, *prepared),
            SavedOpening::Raw {
                original,
                diagnostic,
            } => {
                let mut output = self.adopt_version(
                    id.clone(),
                    title,
                    super::PreparedSource {
                        source: original,
                        syntax: None,
                        syntax_diagnostic: Some(diagnostic.clone()),
                    },
                    "diff",
                    admission,
                )?;
                self.get(&id)?.lock().await.saved_diagnostic = Some(diagnostic);
                output.diff_view = Some(DiffViewOptions::default());
                return Ok(output);
            }
        };
        admission.check()?;
        let more = prepared.model.more();
        let document = SourceDocument {
            saved: Some(SavedDocument {
                model: prepared.model,
                target: prepared.target,
                operation: Arc::new(tokio::sync::Mutex::new(())),
            }),
            saved_diagnostic: None,
            diff_view: Some(DiffViewState::new(
                DiffCapabilities::Source,
                DiffVisibility::All,
            )),
            buffer: prepared.buffer,
            source: Some(original),
            syntax: None,
            offset: 0,
            row: 0,
            retained: title.len(),
            state: if more {
                BodyState::Partial
            } else {
                BodyState::Ready
            },
            input: Default::default(),
            _admission: admission,
        };
        let output = SourceOpen {
            diff_view: document.diff_view.as_ref().map(|view| view.committed()),
            snapshot: document.buffer.snapshot(),
            state: document.state.clone(),
            more,
            title,
            object,
            revision: "diff".into(),
            syntax_diagnostic: None,
        };
        let mut documents = self.document.lock().expect("source document lock");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service closed during open"
        );
        ensure!(
            !documents.contains_key(&id),
            "source document identity is already open"
        );
        documents.insert(id, Arc::new(tokio::sync::Mutex::new(document)));
        Ok(output)
    }

    pub(super) async fn saved_input(
        &self,
        owner: Arc<tokio::sync::Mutex<SourceDocument>>,
        input: DocumentInput,
    ) -> Result<SourceDelivery> {
        let (request, change, operation) = {
            let mut document = owner.lock().await;
            document._admission.check()?;
            ensure!(
                document.buffer.revision() == input.revision,
                "saved source input revision is stale"
            );
            ensure!(
                document
                    .input
                    .get(&input.view)
                    .is_none_or(|previous| input.sequence > *previous),
                "saved source input sequence is stale"
            );
            let block = document
                .buffer
                .block(&input.block)
                .context("saved source input block disappeared")?;
            let row = block
                .text
                .row(input.position.row)
                .context("saved source input row is outside its block")?;
            ensure!(
                input.position.column <= row.len() && row.is_char_boundary(input.position.column),
                "saved source input column is invalid"
            );
            if let Some(target) = &input.target {
                ensure!(
                    block.metadata.target.iter().any(|range| &range.id == target
                        && range.range.start <= input.position
                        && input.position < range.range.end),
                    "saved source target does not match the captured position"
                );
            }
            let saved = document
                .saved
                .as_ref()
                .context("saved source projection disappeared")?;
            let operation = saved.operation.clone();
            let target = block
                .metadata
                .content_node
                .as_ref()
                .and_then(|owner| saved.target.get(&TargetId(owner.0.clone())))
                .or_else(|| {
                    input
                        .target
                        .as_ref()
                        .and_then(|target| saved.target.get(target))
                })
                .cloned();
            let (request, change) = match input.action.as_str() {
                "toggle_visibility" | "toggle_presentation" => {
                    let request = document
                        .diff_view
                        .as_mut()
                        .context("saved source controls disappeared")?
                        .request(if input.action == "toggle_visibility" {
                            DiffViewAction::ToggleVisibility
                        } else {
                            DiffViewAction::TogglePresentation
                        })
                        .map_err(|failure| {
                            anyhow::anyhow!("saved source control failed: {failure:?}")
                        })?;
                    (Some(request), SavedChange::Options(request.options))
                }
                "source_expand" | "source_collapse" => (
                    None,
                    SavedChange::Section(
                        target.context("saved source section is missing")?,
                        input.action == "source_expand",
                    ),
                ),
                "demand" => match target {
                    Some(target) if target.declaration.is_some() => {
                        (None, SavedChange::Section(target, true))
                    }
                    Some(target) => (None, SavedChange::Demand(Some(target.file))),
                    None => (None, SavedChange::Demand(None)),
                },
                _ => anyhow::bail!("unsupported saved source input"),
            };
            document.input.insert(input.view.clone(), input.sequence);
            (request, change, operation)
        };
        let diff = self.diff.clone();
        let syntax = self.syntax.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let _operation = operation.lock().await;
            let result = async {
                let (model, buffer) = {
                    let document = owner.lock().await;
                    document._admission.check()?;
                    if request.is_some_and(|request| {
                        document
                            .diff_view
                            .as_ref()
                            .is_none_or(|view| view.requested() != request)
                    }) {
                        return Ok(saved_delivery(&document, None));
                    }
                    if request.is_none() {
                        ensure!(
                            document.buffer.revision() == input.revision,
                            "saved source expansion was superseded"
                        );
                    }
                    (
                        document
                            .saved
                            .as_ref()
                            .context("saved source projection disappeared")?
                            .model
                            .clone(),
                        document.buffer.clone(),
                    )
                };
                let base = buffer.revision();
                let prepared = prepare_saved(model, buffer, change, diff, syntax).await?;
                let mut document = owner.lock().await;
                document._admission.check()?;
                if request.is_some_and(|request| {
                    document
                        .diff_view
                        .as_ref()
                        .is_none_or(|view| view.requested() != request)
                }) {
                    return Ok(saved_delivery(&document, None));
                }
                ensure!(
                    document.buffer.revision() == base,
                    "saved source changed during projection preparation"
                );
                if let Some(request) = request {
                    ensure!(
                        document
                            .diff_view
                            .as_mut()
                            .context("saved source controls disappeared")?
                            .commit(request),
                        "saved source control was superseded"
                    );
                }
                let saved = document
                    .saved
                    .as_mut()
                    .context("saved source projection disappeared")?;
                saved.model = prepared.model;
                saved.target = prepared.target;
                let more = saved.model.more();
                document.buffer = prepared.buffer;
                document.state = if more {
                    BodyState::Partial
                } else {
                    BodyState::Ready
                };
                Ok(saved_delivery(&document, prepared.patch))
            }
            .await;
            if result.is_err()
                && let Some(request) = request
            {
                let mut document = owner.lock().await;
                if let Some(view) = &mut document.diff_view {
                    view.fail(request);
                }
            }
            let _ = sender.send(result);
        });
        receiver
            .await
            .context("saved source preparation task ended")?
    }
}

fn saved_delivery(document: &SourceDocument, patch: Option<BufferPatch>) -> SourceDelivery {
    SourceDelivery {
        diff_view: document.diff_view.as_ref().map(|view| view.committed()),
        patch,
        state: document.state.clone(),
        more: document.state == BodyState::Partial,
    }
}

async fn prepare_saved(
    mut model: SavedDiffProjection,
    mut buffer: BufferDocument,
    change: SavedChange,
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
) -> Result<PreparedSaved> {
    let key = match &change {
        SavedChange::Options(_) | SavedChange::Section(_, false) => None,
        SavedChange::Demand(key) => key.clone().or_else(|| model.next_file().map(str::to_owned)),
        SavedChange::Section(target, true) => Some(target.file.clone()),
    };
    let prepared_source = match &key {
        Some(key) => model
            .analyze_file(key, &diff, &syntax, WorkPriority::Foreground)
            .await
            .map_err(|failure| anyhow::anyhow!("saved source analysis failed: {failure:?}"))?,
        None => None,
    };
    saved_worker(diff.analysis_pool(), 512 * 1024, move || {
        if let Some(analysis) = prepared_source {
            model.install(&analysis.key, analysis.projection, analysis.diagnostic)?;
        }
        let limits = ChunkLimits {
            rows: 128,
            ..Default::default()
        };
        match change {
            SavedChange::Options(options) => model = model.reconfigure(options, limits)?,
            SavedChange::Demand(_) => {
                if let Some(key) = key {
                    model.demand(&key, limits)?;
                }
            }
            SavedChange::Section(target, expanded) => model.section(&target, expanded, limits)?,
        }
        let frame = model.render("saved")?;
        let patch = buffer.splice(0..buffer.block_count(), frame.block)?;
        Ok(PreparedSaved {
            model,
            target: frame.target,
            buffer,
            patch,
        })
    })
    .await
}

async fn saved_worker<Value: Send + 'static>(
    pool: Arc<AnalysisPool>,
    bytes: usize,
    operation: impl FnOnce() -> Result<Value> + Send + 'static,
) -> Result<Value> {
    let permit = pool
        .reserve(WorkPriority::Foreground, WorkBudget::new(bytes, None))
        .map_err(|failure| {
            anyhow::anyhow!("saved source projection admission failed: {failure:?}")
        })?;
    let ticket = permit.ticket();
    let (sender, receiver) = tokio::sync::oneshot::channel();
    permit.submit(move |_| {
        let _ = sender.send(operation());
    });
    let result = receiver
        .await
        .context("saved source projection worker ended");
    ticket.completed().await;
    result?
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::{
        block::TextPosition,
        identity::{InputSequence, ViewId},
        patch::BufferSnapshot,
    };
    use forge_diff::{
        cache::CacheLimits, saved_source::SavedFileSource, source::SourcePair,
        view::DiffPresentation,
    };
    use forge_git::store::RepositoryStore;

    #[tokio::test]
    async fn saved_semantic_document_keeps_native_targets_controls_and_lazy_file_ownership()
    -> Result<()> {
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let service = SourceDocumentService::new(
            Arc::new(RepositoryStore::default()),
            diff.clone(),
            syntax.clone(),
        );
        let before = "pub fn visible() {\n    previous();\n}\nfn hidden() { old_private(); }\n";
        let after = before
            .replace("previous", "updated")
            .replace("old_private", "new_private");
        let patch = format!(
            "--- a/source.rs\n+++ b/source.rs\n@@ -1,4 +1,4 @@\n{}{}diff --git a/unknown.bin b/unknown.bin\nGIT binary patch\nliteral 1\ncomplete-binary-payload\n",
            before
                .lines()
                .map(|row| format!("-{row}\n"))
                .collect::<String>(),
            after
                .lines()
                .map(|row| format!("+{row}\n"))
                .collect::<String>()
        );
        let id = DocumentId("saved-semantic".into());
        let opened = service
            .open_saved_diff(
                id.clone(),
                "Saved diff".into(),
                SavedDiffSource {
                    text: Arc::new(patch),
                    file: vec![SavedFileSource {
                        old_path: Some("source.rs".into()),
                        new_path: Some("source.rs".into()),
                        source: Ok(SourcePair {
                            old: SourceVersion::new(
                                before.as_bytes().to_vec(),
                                Representation::Raw,
                            )?,
                            new: SourceVersion::new(after.into_bytes(), Representation::Raw)?,
                        }),
                    }],
                    diagnostic: None,
                },
            )
            .await?;
        assert_eq!(opened.diff_view, Some(DiffViewOptions::default()));
        assert!(opened.more);
        let mut frame = opened.snapshot.clone();
        assert!(text(&frame).contains("updated();"));
        assert!(!text(&frame).contains("complete-binary-payload"));
        let mut transition = Vec::new();
        let mut sequence = 0;
        for action in [
            "demand",
            "toggle_presentation",
            "source_expand",
            "toggle_visibility",
            "toggle_presentation",
            "toggle_presentation",
            "source_collapse",
        ] {
            sequence += 1;
            let block = match action {
                "source_expand" | "source_collapse" => frame
                    .block
                    .iter()
                    .find(|block| block.text.row(0) == Some("pub fn visible() {"))
                    .unwrap(),
                "demand" => frame
                    .block
                    .iter()
                    .find(|block| block.id.0.ends_with(":more") && block.id.0.contains(":file:1:"))
                    .unwrap(),
                _ => &frame.block[0],
            };
            let input = DocumentInput {
                document: id.clone(),
                revision: frame.revision,
                view: ViewId("saved-native-view".into()),
                sequence: InputSequence(sequence),
                action: action.into(),
                block: block.id.clone(),
                position: TextPosition { row: 0, column: 0 },
                target: block
                    .metadata
                    .target
                    .first()
                    .map(|target| target.id.clone()),
            };
            let delivery = service.demand(input.clone()).await?;
            if let Some(patch) = &delivery.patch {
                patch.validate()?;
            }
            frame = service.snapshot(&id).await?;
            assert!(service.demand(input).await.is_err());
            let recovered = service.snapshot_state(&id).await?;
            assert_eq!(recovered.snapshot, frame);
            assert_eq!(recovered.diff_view, delivery.diff_view);
            assert_eq!(recovered.more, delivery.more);
            match sequence {
                2 => {
                    assert!(!text(&frame).contains("updated();"));
                    assert!(text(&frame).contains("fn hidden()"));
                    assert!(text(&frame).contains("Unclassified source changes"));
                }
                3 | 4 | 6 => assert!(text(&frame).contains("updated();")),
                5 => assert_eq!(
                    delivery.diff_view.unwrap().presentation,
                    DiffPresentation::Full
                ),
                7 => assert!(
                    !frame
                        .block
                        .iter()
                        .find(|block| block.text.row(0) == Some("pub fn visible() {"))
                        .unwrap()
                        .metadata
                        .section[0]
                        .open
                ),
                1 => {
                    assert!(text(&frame).contains("complete-binary-payload"));
                    assert!(!delivery.more);
                }
                _ => unreachable!(),
            }
            if sequence >= 4 {
                assert!(!text(&frame).contains("hidden"));
            }
            transition
                .push(serde_json::json!({"action":action,"delivery":delivery,"expected":frame}));
        }
        std::fs::write(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../target/saved-semantic-fixture.json"),
            serde_json::to_vec(&serde_json::json!({"initial":opened,"transition":transition}))?,
        )?;
        assert!(service.close(&id));
        assert!(service.snapshot(&id).await.is_err());
        assert_eq!(syntax.usage().active_jobs, 0);
        assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
        Ok(())
    }

    fn text(frame: &BufferSnapshot) -> String {
        frame
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n")
    }
}
