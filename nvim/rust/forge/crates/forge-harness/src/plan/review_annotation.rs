use super::{PlanAnnotationInput, digest, write_bytes_atomically};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

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
    fn is_comment(&self) -> bool {
        *self == Self::Comment
    }
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
    annotation: AnnotationSet,
}

struct AnnotationSet {
    values: Vec<ReviewAnnotation>,
    root: Vec<String>,
}

impl AnnotationSet {
    fn new(values: Vec<ReviewAnnotation>) -> Result<Self> {
        let root = validate(&values)?;
        Ok(Self { values, root })
    }
}

impl std::ops::Deref for AnnotationSet {
    type Target = [ReviewAnnotation];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
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
            decoded.annotation
        } else {
            Vec::new()
        };
        Ok(Self {
            path,
            source_path,
            source_digest,
            saved,
            annotation: AnnotationSet::new(annotation)?,
        })
    }

    pub(crate) fn annotation(&self) -> &[ReviewAnnotation] {
        &self.annotation
    }

    /// Rebuild display ranges without collapsing repeated semantic targets.
    pub(crate) fn reanchor(&mut self, current: &super::PlanNavigationIndex) -> Result<()> {
        let mut updated = self.annotation.to_vec();
        for annotation in &mut updated {
            let anchor = annotation
                .anchor
                .as_ref()
                .context("plan comment has no saved semantic anchor")?;
            let start = find_target(current, &anchor.start, annotation.source.start_line)
                .context("comment start is absent or ambiguous in the formatted design")?;
            let end = find_target(current, &anchor.end, annotation.source.end_line)
                .context("comment end is absent or ambiguous in the formatted design")?;
            annotation.source.start_line = start.min(end);
            annotation.source.end_line = start.max(end);
        }
        self.annotation = AnnotationSet::new(updated)?;
        Ok(())
    }

    pub(crate) fn replace(&mut self, annotation: Vec<ReviewAnnotation>) -> Result<()> {
        let annotation = AnnotationSet::new(annotation)?;
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
            annotation: annotation.to_vec(),
        })?;
        write_bytes_atomically(&self.path, &replacement)?;
        self.saved = Some(replacement);
        self.annotation = annotation;
        Ok(())
    }

    pub(crate) fn thread_context(&self, selected: &ReviewAnnotation) -> Result<String> {
        let selected_root = self
            .annotation
            .iter()
            .zip(&self.annotation.root)
            .find(|(item, _)| item.id == selected.id)
            .map(|(_, root)| root)
            .context("selected annotation is no longer present")?;
        let mut context = String::new();
        for (item, root) in self.annotation.iter().zip(&self.annotation.root) {
            if root != selected_root {
                continue;
            }
            let heading = if item.kind == ReviewAnnotationKind::Question {
                "Plan question"
            } else {
                "Plan comment"
            };
            context.push_str(&format!("{heading}:\n{}\n\n", item.source.body));
            if let Some(reply) = &item.reply {
                context.push_str(&format!("Plan answer:\n{}\n\n", reply.body));
            }
        }
        Ok(context)
    }
}

/// Resolve the saved row or a uniquely relocated semantic target.
fn find_target(
    index: &super::PlanNavigationIndex,
    target: &super::PlanReviewTarget,
    saved_line: u32,
) -> Option<u32> {
    let mut relocated = None;
    let mut ambiguous = false;
    for anchor in index.anchor.iter().filter(|anchor| {
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
    }) {
        if anchor.line == saved_line {
            return Some(saved_line);
        }
        if relocated.is_some_and(|line| line != anchor.line) {
            ambiguous = true;
        }
        relocated = Some(anchor.line);
    }
    if ambiguous { None } else { relocated }
}

fn read_optional(path: &std::path::Path) -> Result<Option<Vec<u8>>> {
    match std::fs::symlink_metadata(path) {
        Ok(metadata) => {
            ensure!(
                metadata.is_file() && !metadata.file_type().is_symlink(),
                "plan annotation path is not a regular file"
            );
            Ok(Some(std::fs::read(path)?))
        }
        Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(failure) => Err(failure.into()),
    }
}

fn validate(annotation: &[ReviewAnnotation]) -> Result<Vec<String>> {
    let mut identities: std::collections::HashMap<&str, (&ReviewAnnotation, String)> =
        std::collections::HashMap::new();
    let mut roots = Vec::with_capacity(annotation.len());
    for annotation in annotation {
        ensure!(
            !annotation.id.is_empty() && !identities.contains_key(annotation.id.as_str()),
            "invalid or duplicate plan annotation identity"
        );
        ensure!(
            annotation.source.start_line > 0
                && annotation.source.end_line >= annotation.source.start_line,
            "invalid plan annotation source range"
        );
        ensure!(
            !annotation.source.body.contains('\0'),
            "plan annotation contains NUL"
        );
        let root = if let Some(parent_id) = &annotation.parent_id {
            let (parent, root) = identities
                .get(parent_id.as_str())
                .context("annotation parent must precede its follow-up")?;
            ensure!(
                parent.source.start_line == annotation.source.start_line
                    && parent.source.end_line == annotation.source.end_line,
                "follow-up must retain its parent's source range"
            );
            root.clone()
        } else {
            annotation.id.clone()
        };
        if let Some(reply) = &annotation.reply {
            ensure!(
                annotation.kind == ReviewAnnotationKind::Question
                    && reply.question_body == annotation.source.body
                    && !reply.body.trim().is_empty()
                    && !reply.body.contains('\0'),
                "invalid plan question reply"
            );
        }
        identities.insert(&annotation.id, (annotation, root.clone()));
        roots.push(root);
    }
    Ok(roots)
}

#[cfg(test)]
mod tests {
    use super::*;

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
    let annotation = annotation_input
        .into_iter()
        .map(|input| {
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
                .map(|anchor| crate::plan::PlanAnnotationSubject {
                    target: anchor.target.clone(),
                    json_path: anchor.json_path.clone(),
                    label: anchor.label.clone(),
                    path: anchor.path.clone(),
                })
                .collect::<Vec<_>>();
            let (first, remaining) = subject.split_first().with_context(|| {
                format!(
                    "plan annotation lines {}-{} have no semantic render anchors",
                    input.start_line, input.end_line
                )
            })?;
            let label = if remaining.is_empty() {
                first.label.clone()
            } else {
                format!(
                    "{} through {}",
                    first.label,
                    remaining.last().unwrap_or(first).label
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
