use anyhow::{Context, Result, ensure};
use forge_git::RepositoryPath;
use serde::Deserialize;

pub const MAX_ARTIFACT_BYTES: usize = 2 * 1024 * 1024;
const MAX_ITEMS: usize = 10_000;

#[derive(Clone, Debug, Deserialize)]
pub struct Artifact {
    pub version: u32,
    pub flow: Vec<Flow>,
    pub overview: String,
    pub root: String,
    pub commit: String,
    pub tasks: Vec<Task>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Flow {
    pub text: String,
    #[serde(default)]
    pub children: Vec<Flow>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Task {
    pub title: String,
    pub justification: Option<String>,
    pub subtasks: Vec<Subtask>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Subtask {
    pub title: String,
    pub justification: Option<String>,
    pub changes: Vec<Change>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Change {
    pub action: Action,
    pub kind: Kind,
    pub target: String,
    pub role: Option<String>,
    pub note: String,
    pub file: Option<String>,
    pub line: Option<serde_json::Value>,
    pub annotation: Option<Annotation>,
    children: Option<serde_json::Value>,
    annotations: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Annotation {
    pub title: String,
    pub comment: String,
}

#[derive(Clone, Copy, Debug, Deserialize)]
pub enum Action {
    Add,
    Modify,
    Remove,
}

#[derive(Clone, Copy, Debug, Deserialize)]
pub enum Kind {
    Class,
    Struct,
    Enum,
    Trait,
    Interface,
    Field,
    Function,
    Method,
    Constant,
    Test,
    Config,
}

impl Artifact {
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        ensure!(
            bytes.len() <= MAX_ARTIFACT_BYTES,
            "walkthrough artifact exceeds 2 MiB"
        );
        let artifact: Self = serde_json::from_slice(bytes).context("invalid walkthrough schema")?;
        artifact.validate()?;
        Ok(artifact)
    }

    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.version == 12,
            "unsupported walkthrough version, expected 12"
        );
        text(&self.overview, "overview")?;
        text(&self.root, "root")?;
        ensure!(
            self.commit.len() == 40 && self.commit.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "walkthrough commit must be a full 40-digit HEAD SHA"
        );
        ensure!(
            !self.flow.is_empty() && !self.tasks.is_empty(),
            "walkthrough flow and tasks must not be empty"
        );
        let mut items = 0usize;
        for flow in &self.flow {
            flow.validate(0, &mut items)?;
        }
        for task in &self.tasks {
            count(&mut items)?;
            text(&task.title, "task title")?;
            optional(&task.justification, "task justification")?;
            ensure!(
                !task.subtasks.is_empty(),
                "walkthrough task has no subtasks"
            );
            let mut annotations = 0usize;
            for subtask in &task.subtasks {
                count(&mut items)?;
                text(&subtask.title, "subtask title")?;
                optional(&subtask.justification, "subtask justification")?;
                ensure!(
                    !subtask.changes.is_empty(),
                    "walkthrough subtask has no changes"
                );
                for change in &subtask.changes {
                    count(&mut items)?;
                    change.validate()?;
                    annotations += usize::from(change.annotation.is_some());
                }
            }
            ensure!(annotations > 0, "walkthrough task has no annotated steps");
        }
        Ok(())
    }
}

impl Flow {
    fn validate(&self, depth: usize, items: &mut usize) -> Result<()> {
        ensure!(depth < 64, "walkthrough flow nesting exceeds 64 levels");
        count(items)?;
        text(&self.text, "flow text")?;
        for child in &self.children {
            child.validate(depth + 1, items)?;
        }
        Ok(())
    }
}

impl Change {
    fn validate(&self) -> Result<()> {
        text(&self.target, "change target")?;
        text(&self.note, "change note")?;
        optional(&self.role, "change role")?;
        ensure!(
            self.children.is_none() && self.annotations.is_none(),
            "change children and plural annotations are unsupported"
        );
        if let Some(annotation) = &self.annotation {
            text(&annotation.title, "annotation title")?;
            text(&annotation.comment, "annotation comment")?;
            self.source()?;
        } else {
            ensure!(
                self.file.is_none() && self.line.is_none(),
                "change file and line require annotation"
            );
        }
        Ok(())
    }

    pub fn source(&self) -> Result<(RepositoryPath, usize)> {
        let file = self
            .file
            .as_ref()
            .context("annotation is missing file")?
            .replace('\\', "/");
        let path =
            RepositoryPath::new(file.strip_prefix("./").unwrap_or(&file).as_bytes().to_vec())?;
        let value = self.line.as_ref().context("annotation is missing line")?;
        let line = value
            .as_f64()
            .or_else(|| value.as_str().and_then(|value| value.parse::<f64>().ok()))
            .context("annotation line is not numeric")?;
        ensure!(
            line.is_finite() && line >= 1.0 && line <= u32::MAX as f64,
            "annotation line is outside supported source bounds"
        );
        Ok((path, line.floor() as usize - 1))
    }
}

fn text(value: &str, role: &str) -> Result<()> {
    ensure!(
        !value.trim().is_empty() && !value.contains('\0'),
        "walkthrough {role} is empty or contains NUL"
    );
    Ok(())
}

fn optional(value: &Option<String>, role: &str) -> Result<()> {
    if let Some(value) = value {
        text(value, role)?;
    }
    Ok(())
}

fn count(value: &mut usize) -> Result<()> {
    *value += 1;
    ensure!(
        *value <= MAX_ITEMS,
        "walkthrough exceeds 10000 flow/task/change records"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn artifact() -> serde_json::Value {
        serde_json::json!({"version":12,"flow":[{"text":"flow"}],"overview":"overview","root":"Project description","commit":"1234567890123456789012345678901234567890","tasks":[{"title":"Task","subtasks":[{"title":"Subtask","changes":[{"action":"Modify","kind":"Function","target":"function","note":"note","file":"src/main.rs","line":3,"annotation":{"title":"Annotation","comment":"Comment"}}]}]}]})
    }
    #[test]
    fn artifact_root_is_prose_and_annotation_coordinates_are_native() {
        let parsed = Artifact::parse(&serde_json::to_vec(&artifact()).unwrap()).unwrap();
        assert_eq!(parsed.root, "Project description");
        let (path, row) = parsed.tasks[0].subtasks[0].changes[0].source().unwrap();
        assert_eq!(path.raw(), b"src/main.rs");
        assert_eq!(row, 2);
    }
    #[test]
    fn artifact_rejects_escape_and_legacy_plural_annotation() {
        let mut value = artifact();
        value["tasks"][0]["subtasks"][0]["changes"][0]["file"] = "../outside".into();
        assert!(Artifact::parse(&serde_json::to_vec(&value).unwrap()).is_err());
        let mut value = artifact();
        value["tasks"][0]["subtasks"][0]["changes"][0]["annotations"] = serde_json::json!([]);
        assert!(Artifact::parse(&serde_json::to_vec(&value).unwrap()).is_err());
    }
}
