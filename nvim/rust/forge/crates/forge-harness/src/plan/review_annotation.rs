use super::{PlanAnnotationInput, digest, write_bytes_atomically};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::path::PathBuf;

const MAX_ANNOTATION_BYTES: usize = 1024 * 1024;

#[derive(Clone, Serialize, Deserialize)]
pub struct ReviewAnnotation {
    pub id: String,
    /// Links a follow-up to its earlier message and inherited source range.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,
    pub source: PlanAnnotationInput,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub anchor: Option<ReviewAnnotationAnchor>,
    #[serde(default, skip_serializing_if = "ReviewAnnotationKind::is_comment")]
    /// Identifies whether saving this annotation asks the model a question.
    pub kind: ReviewAnnotationKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Retains the answer independently of the user's editable text.
    pub reply: Option<ReviewQuestionReply>,
}

/// Distinguishes revision feedback from questions that leave the proposal unchanged.
#[derive(Clone, Copy, Default, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewAnnotationKind {
    #[default]
    /// Feedback applied when the user requests a revision.
    Comment,
    /// A request for an explanation that leaves the proposal unchanged.
    Question,
}

impl ReviewAnnotationKind {
    fn is_comment(&self) -> bool { *self == Self::Comment }
}

/// Binds a persisted answer to the exact question text it answered.
#[derive(Clone, Serialize, Deserialize)]
pub struct ReviewQuestionReply {
    /// Exact question body used to generate this answer.
    pub question_body: String,
    /// Concise explanation shown in the attached read-only box.
    pub body: String,
    /// Execution duration recorded by the question's main-conversation exchange.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<u64>,
}

/// Binds a review selection to immutable saved declaration positions.
#[derive(Clone, Serialize, Deserialize)]
pub struct ReviewAnnotationAnchor {
    pub start: super::PlanReviewTarget,
    pub end: super::PlanReviewTarget,
}

#[derive(Serialize, Deserialize)]
struct SavedAnnotation {
    source_digest: String,
    annotation: Vec<ReviewAnnotation>,
}

pub(crate) struct ReviewAnnotationStore {
    path: PathBuf,
    source_path: PathBuf,
    source_digest: String,
    saved: Option<Vec<u8>>,
    annotation: Vec<ReviewAnnotation>,
}

impl ReviewAnnotationStore {
    pub(crate) fn open(source_path: PathBuf, source_digest: String) -> Result<Self> {
        ensure!(
            source_digest.len() == 64 && source_digest.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "invalid saved plan digest"
        );
        let path = source_path
            .parent()
            .context("physical plan parent is missing")?
            .join(format!("review-annotations-{source_digest}.json"));
        let saved = read_optional(&path)?;
        let annotation = if let Some(saved) = &saved {
            let decoded: SavedAnnotation = serde_json::from_slice(saved)?;
            ensure!(
                decoded.source_digest == source_digest,
                "saved plan annotation identity changed"
            );
            validate(&decoded.annotation)?;
            decoded.annotation
        } else {
            Vec::new()
        };
        Ok(Self {
            path,
            source_path,
            source_digest,
            saved,
            annotation,
        })
    }

    pub(crate) fn annotation(&self) -> &[ReviewAnnotation] {
        &self.annotation
    }

    /// Rebuild display ranges from saved semantic targets.
    pub(crate) fn reanchor(
        &mut self,
        current: &super::PlanNavigationIndex,
    ) -> Result<()> {
        for annotation in &mut self.annotation {
            let anchor = annotation.anchor.as_ref().context("plan comment has no saved semantic anchor")?;
            let start = find_target(current, &anchor.start)
                .context("comment start is absent from the formatted design")?;
            let end = find_target(current, &anchor.end)
                .context("comment end is absent from the formatted design")?;
            annotation.source.start_line = start.min(end);
            annotation.source.end_line = start.max(end);
        }
        Ok(())
    }

    pub(crate) fn replace(&mut self, annotation: Vec<ReviewAnnotation>) -> Result<()> {
        validate(&annotation)?;
        let source = super::review_source::read_source(&self.source_path)?;
        ensure!(
            digest(&source) == self.source_digest,
            "physical plan source changed before saving annotations"
        );
        ensure!(
            read_optional(&self.path)? == self.saved,
            "plan annotations changed in another review"
        );
        let replacement = serde_json::to_vec(&SavedAnnotation {
            source_digest: self.source_digest.clone(),
            annotation: annotation.clone(),
        })?;
        ensure!(
            replacement.len() <= MAX_ANNOTATION_BYTES,
            "plan annotations exceed 1 MiB"
        );
        write_bytes_atomically(&self.path, &replacement)?;
        self.saved = Some(replacement);
        self.annotation = annotation;
        Ok(())
    }
}

fn find_target(
    index: &super::PlanNavigationIndex,
    target: &super::PlanReviewTarget,
) -> Option<u32> {
    index
        .anchor
        .iter()
        .find(|anchor| {
            if &anchor.target == target {
                return true;
            }
            match (target, &anchor.target) {
                (
                    super::PlanReviewTarget::Declaration {
                        path,
                        side,
                        line,
                        column: None,
                    },
                    super::PlanReviewTarget::Declaration {
                        path: current_path,
                        side: current_side,
                        line: current_line,
                        ..
                    },
                ) => path == current_path && side == current_side && line == current_line,
                _ => false,
            }
        })
        .map(|anchor| anchor.line)
}

fn read_optional(path: &std::path::Path) -> Result<Option<Vec<u8>>> {
    match std::fs::symlink_metadata(path) {
        Ok(metadata) => {
            ensure!(
                metadata.is_file() && !metadata.file_type().is_symlink(),
                "plan annotation path is not a regular file"
            );
            ensure!(
                metadata.len() <= MAX_ANNOTATION_BYTES as u64,
                "plan annotations exceed 1 MiB"
            );
            let mut bytes = Vec::new();
            std::fs::File::open(path)?
                .take(MAX_ANNOTATION_BYTES as u64 + 1)
                .read_to_end(&mut bytes)?;
            ensure!(
                bytes.len() <= MAX_ANNOTATION_BYTES,
                "plan annotations exceed 1 MiB"
            );
            Ok(Some(bytes))
        }
        Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(failure) => Err(failure.into()),
    }
}

fn validate(annotation: &[ReviewAnnotation]) -> Result<()> {
    ensure!(annotation.len() <= 64, "plan review exceeds 64 annotations");
    let mut identities = std::collections::HashSet::new();
    for annotation in annotation {
        ensure!(
            !annotation.id.is_empty()
                && annotation.id.len() <= 128
                && identities.insert(&annotation.id),
            "invalid or duplicate plan annotation identity"
        );
        ensure!(
            annotation.source.start_line > 0
                && annotation.source.end_line >= annotation.source.start_line,
            "invalid plan annotation source range"
        );
        ensure!(
            annotation.source.body.len() <= 65536 && !annotation.source.body.contains('\0'),
            "plan annotation exceeds 64 KiB or contains NUL"
        );
        if let Some(parent_id) = &annotation.parent_id {
            ensure!(identities.contains(parent_id) && parent_id != &annotation.id,
                "annotation parent must precede its follow-up");
        }
        if let Some(reply) = &annotation.reply {
            ensure!(annotation.kind == ReviewAnnotationKind::Question
                && reply.question_body == annotation.source.body
                && !reply.body.trim().is_empty() && reply.body.len() <= 8192
                && !reply.body.contains('\0'), "invalid plan question reply");
        }
    }
    for item in annotation {
        if let Some(parent_id) = &item.parent_id {
            let parent = annotation.iter().find(|parent| &parent.id == parent_id).expect("validated parent");
            ensure!(parent.source.start_line == item.source.start_line
                && parent.source.end_line == item.source.end_line,
                "follow-up must retain its parent's source range");
        }
    }
    Ok(())
}

/// Render only this conversation in saved order, including authoritative model replies.
pub(crate) fn thread_context(annotation: &[ReviewAnnotation], selected: &ReviewAnnotation) -> String {
    fn root<'annotation>(annotation: &'annotation [ReviewAnnotation], mut item: &'annotation ReviewAnnotation) -> &'annotation str {
        while let Some(parent_id) = &item.parent_id {
            item = annotation.iter().find(|parent| &parent.id == parent_id).expect("validated parent");
        }
        &item.id
    }
    let selected_root = root(annotation, selected);
    let mut context = String::new();
    for item in annotation.iter().filter(|item| root(annotation, item) == selected_root) {
        let heading = if item.kind == ReviewAnnotationKind::Question { "Plan question" } else { "Plan comment" };
        context.push_str(&format!("{heading}:\n{}\n\n", item.source.body));
        if let Some(reply) = &item.reply {
            context.push_str(&format!("Plan answer:\n{}\n\n", reply.body));
        }
    }
    context
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn persisted_declaration_targets_follow_display_spacing_changes() {
        let temporary = tempfile::tempdir().unwrap();
        let source_path = temporary.path().join("working.json");
        std::fs::write(&source_path, b"saved design").unwrap();
        let checksum = digest(b"saved design");
        let target = super::super::PlanReviewTarget::Declaration {
            path: "registry.rs".into(),
            side: "proposed".into(),
            line: 3,
            column: Some(20),
        };
        let mut index = super::super::PlanNavigationIndex {
            plan_id: "plan".into(),
            plan_version: 1,
            anchor: vec![super::super::PlanNavigationAnchor {
                line: 8,
                target: target.clone(),
                json_path: "saved position".into(),
                path: Some("registry.rs".into()),
                label: "second".into(),
            }],
        };
        let mut store = ReviewAnnotationStore::open(source_path.clone(), checksum.clone()).unwrap();
        store
            .replace(vec![ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
                id: "comment".into(),
                source: PlanAnnotationInput {
                    start_line: 8,
                    end_line: 8,
                    body: "Review second".into(),
                },
                anchor: Some(ReviewAnnotationAnchor {
                    start: target.clone(),
                    end: target.clone(),
                }),
            }])
            .unwrap();
        let mut reopened = ReviewAnnotationStore::open(source_path.clone(), checksum).unwrap();
        index.anchor[0].line = 12;
        reopened.reanchor(&index).unwrap();
        assert_eq!(reopened.annotation()[0].source.end_line, 12);
        assert_eq!(reopened.annotation()[0].source.body, "Review second");
        reopened.replace(reopened.annotation().to_vec()).unwrap();
        assert_eq!(std::fs::read(source_path).unwrap(), b"saved design");
    }

    #[test]
    fn annotation_restart_preserves_text_and_rejects_concurrent_or_source_changes() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("working.json");
        std::fs::write(&source, b"canonical").unwrap();
        let checksum = digest(b"canonical");
        let mut first = ReviewAnnotationStore::open(source.clone(), checksum.clone()).unwrap();
        let mut stale = ReviewAnnotationStore::open(source.clone(), checksum.clone()).unwrap();
        let annotation = vec![ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
            anchor: None,
            id: "comment".into(),
            source: PlanAnnotationInput {
                start_line: 1,
                end_line: 2,
                body: "literal ** text\n".into(),
            },
        }];
        first.replace(annotation.clone()).unwrap();
        first.replace(annotation.clone()).unwrap();
        assert!(stale.replace(annotation.clone()).is_err());
        let mut resumed = ReviewAnnotationStore::open(source.clone(), checksum).unwrap();
        assert_eq!(resumed.annotation()[0].source.body, "literal ** text\n");
        std::fs::write(source, b"changed").unwrap();
        assert!(resumed.replace(Vec::new()).is_err());
        assert_eq!(resumed.annotation().len(), 1);
    }
}

pub(crate) fn resolve_annotations(
    rendered: &super::RenderedPlan,
    annotation_input: Vec<PlanAnnotationInput>,
) -> Result<Vec<super::PlanAnnotation>> {
    let source_rows = rendered.markdown.lines().count();
    let source_anchor: std::collections::BTreeMap<_, _> = rendered
        .navigation
        .anchor
        .iter()
        .map(|anchor| (anchor.line, anchor))
        .collect();
    let mut subject_bytes = 0usize;
    let mut subject_count = 0usize;
    let annotation = annotation_input
        .into_iter()
        .enumerate()
        .map(|(index, input)| {
            anyhow::ensure!(
                index < 64 && input.body.len() <= 65536,
                "plan annotation admission exceeds 64 comments or 64 KiB per body"
            );
            anyhow::ensure!(
                input.start_line > 0 && input.end_line as usize <= source_rows,
                "plan annotation range is outside the reviewed source"
            );
            anyhow::ensure!(
                !input.body.trim().is_empty(),
                "plan annotation body cannot be empty"
            );
            anyhow::ensure!(
                input.start_line <= input.end_line,
                "plan annotation start line must not follow its end line"
            );
            let mut seen_path = std::collections::HashSet::new();
            let subject = source_anchor
                .range(input.start_line..=input.end_line)
                .map(|(_, anchor)| *anchor)
                .filter(|anchor| seen_path.insert(anchor.json_path.clone()))
                .map(|anchor| {
                    anyhow::ensure!(
                        subject_count < 4096,
                        "plan review exceeds 4096 annotation subjects"
                    );
                    let size = crate::limits::serialized_size(
                        anchor,
                        (1024 * 1024usize).saturating_sub(subject_bytes),
                    )?;
                    subject_count += 1;
                    subject_bytes += size;
                    Ok(crate::plan::PlanAnnotationSubject {
                        target: anchor.target.clone(),
                        json_path: anchor.json_path.clone(),
                        label: anchor.label.clone(),
                        path: anchor.path.clone(),
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            anyhow::ensure!(
                !subject.is_empty(),
                "plan annotation lines {}-{} have no semantic render anchors",
                input.start_line,
                input.end_line
            );
            let label = if subject.len() == 1 {
                subject[0].label.clone()
            } else {
                format!(
                    "{} through {}",
                    subject.first().expect("nonempty subjects").label,
                    subject.last().expect("nonempty subjects").label
                )
            };
            Ok(super::PlanAnnotation {
                subject,
                label,
                body: input.body,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(annotation)
}

#[cfg(test)]
mod subject_tests {
    use super::*;

    #[test]
    fn subject_resolution_bounds_semantic_expansion_and_rejects_unmapped_ranges() {
        let mut rendered = crate::plan::RenderedPlan {
            markdown: "x\n".repeat(4097),
            navigation: crate::plan::PlanNavigationIndex {
                plan_id: "plan".into(),
                plan_version: 1,
                anchor: (1..=4097)
                    .map(|line| crate::plan::PlanNavigationAnchor {
                        line,
                        target: crate::plan::PlanReviewTarget::Task {
                            title: "task".into(),
                        },
                        json_path: format!("/tasks/{line}"),
                        path: None,
                        label: "task".into(),
                    })
                    .collect(),
            },
        };
        let input = |end_line| {
            vec![PlanAnnotationInput {
                start_line: 1,
                end_line,
                body: "change".into(),
            }]
        };
        let failure = resolve_annotations(&rendered, input(4097)).unwrap_err();
        assert!(failure.to_string().contains("4096 annotation subjects"));
        rendered.navigation.anchor.truncate(1);
        rendered.navigation.anchor[0].label = "x".repeat(1024 * 1024);
        assert!(resolve_annotations(&rendered, input(1)).is_err());
        rendered.navigation.anchor.clear();
        assert!(
            resolve_annotations(&rendered, input(1))
                .unwrap_err()
                .to_string()
                .contains("no semantic render anchors")
        );
    }
}
