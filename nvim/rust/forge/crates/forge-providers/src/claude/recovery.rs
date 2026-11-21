//! Journal an interrupted Claude invocation and verify repair against its persisted branch.

use forge_provider_api::{
    InvocationScope, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};
use uuid::Uuid;

pub(super) struct RepairJournal {
    record: PathBuf,
    projects: PathBuf,
    session: Uuid,
}

#[derive(Serialize, Deserialize)]
pub(super) struct RepairIntent {
    pub(super) scope: InvocationScope,
    pub(super) inputs: Vec<Uuid>,
    pub(super) retained: Option<Uuid>,
    pub(super) unanswered: bool,
}

pub(super) enum RepairStatus {
    Clean,
    Cut { target: Uuid, last_input: Uuid },
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Entry {
    #[serde(rename = "type")]
    kind: String,
    uuid: Option<Uuid>,
    parent_uuid: Option<Uuid>,
    leaf_uuid: Option<Uuid>,
    cut_uuid: Option<Uuid>,
    #[serde(default)]
    rewound: bool,
    #[serde(default)]
    is_sidechain: bool,
    message: Option<serde_json::Value>,
}

impl RepairJournal {
    /// Locate the versioned journal and the CLI transcript for one native conversation.
    pub(super) fn new(root: &Path, projects: PathBuf, session: &str) -> Option<Self> {
        let session = Uuid::parse_str(session).ok()?;
        Some(Self {
            record: root.join("claude-repair").join(format!("{session}.json")),
            projects,
            session,
        })
    }

    /// Persist stop intent before the CLI receives its interrupt.
    pub(super) fn save(&self, intent: &RepairIntent) -> anyhow::Result<()> {
        let parent = self
            .record
            .parent()
            .expect("repair journal has a directory");
        std::fs::create_dir_all(parent)?;
        let mut temporary = tempfile::NamedTempFile::new_in(parent)?;
        serde_json::to_writer(temporary.as_file_mut(), intent)?;
        temporary.as_file_mut().flush()?;
        temporary.as_file().sync_all()?;
        temporary.persist(&self.record)?;
        Ok(())
    }

    /// Read pending work and retain malformed records for diagnosis.
    pub(super) fn load(&self) -> anyhow::Result<Option<RepairIntent>> {
        match std::fs::File::open(&self.record) {
            Ok(file) => Ok(Some(serde_json::from_reader(file)?)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(error.into()),
        }
    }

    /// Retire a repair after proving its saved effect.
    pub(super) fn clear(&self) -> anyhow::Result<()> {
        match std::fs::remove_file(&self.record) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(error) => Err(error.into()),
        }
    }

    /// Inspect the persisted branch without treating an absent live target as a successful cut.
    pub(super) fn status(&self, intent: &RepairIntent) -> anyhow::Result<RepairStatus> {
        let mut transcript = None;
        let filename = format!("{}.jsonl", self.session);
        for project in std::fs::read_dir(&self.projects)? {
            let project = project?;
            if !project.file_type()?.is_dir() {
                continue;
            }
            let path = project.path().join(&filename);
            match std::fs::File::open(path) {
                Ok(file) => {
                    anyhow::ensure!(transcript.is_none(), "duplicate Claude transcript");
                    transcript = Some(file);
                }
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error.into()),
            }
        }
        let transcript =
            transcript.ok_or_else(|| anyhow::anyhow!("Claude transcript is missing"))?;
        read_branch(std::io::BufReader::new(transcript), intent)
    }
}

/// Resolve the published main-branch leaf and admit only a cut of an unanswered owned suffix.
fn read_branch(reader: impl BufRead, intent: &RepairIntent) -> anyhow::Result<RepairStatus> {
    let mut entries = HashMap::new();
    let mut tip = None;
    let mut published_tip = None;
    let mut confirmed_cut = false;
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let entry: Entry = serde_json::from_str(&line)?;
        if entry.kind == "last-prompt" {
            published_tip = Some(entry.leaf_uuid);
            if entry.rewound {
                confirmed_cut = entry
                    .cut_uuid
                    .is_some_and(|cut| intent.inputs.contains(&cut));
            }
        } else if let Some(id) = entry.uuid
            && !entry.is_sidechain
            && matches!(
                entry.kind.as_str(),
                "user" | "assistant" | "attachment" | "system"
            )
        {
            tip = Some(id);
            published_tip = None;
            entries.insert(id, entry);
        }
    }
    let mut cursor = published_tip.unwrap_or(tip);
    let mut seen = HashSet::new();
    let mut chain = Vec::new();
    while let Some(id) = cursor {
        anyhow::ensure!(seen.insert(id), "cycle in Claude transcript");
        let entry = entries
            .get(&id)
            .ok_or_else(|| anyhow::anyhow!("missing Claude transcript parent"))?;
        chain.push((id, entry));
        cursor = entry.parent_uuid;
    }
    let mut candidate = None;
    let mut last_input = None;
    let mut retained_precedes_cut = intent.retained.is_none();
    for (id, entry) in chain.into_iter().rev() {
        if intent.retained == Some(id) {
            retained_precedes_cut = true;
        }
        if entry.kind == "assistant" {
            candidate = None;
            last_input = None;
        } else if entry.kind == "user" && intent.inputs.contains(&id) {
            candidate.get_or_insert((id, retained_precedes_cut && intent.retained != Some(id)));
            last_input = Some(id);
        } else if entry.kind == "user" && !interrupt_marker(entry) {
            anyhow::ensure!(candidate.is_none(), "later Claude input prevents repair");
        }
    }
    anyhow::ensure!(retained_precedes_cut, "retained Claude boundary is missing");
    if let Some((target, preserves_boundary)) = candidate {
        anyhow::ensure!(
            preserves_boundary,
            "Claude cut crosses the retained boundary"
        );
        return Ok(RepairStatus::Cut {
            target,
            last_input: last_input.expect("a target establishes the last input"),
        });
    }
    anyhow::ensure!(
        !intent.unanswered || confirmed_cut,
        "unanswered Claude input lacks a verified cut"
    );
    anyhow::ensure!(
        confirmed_cut
            || intent
                .inputs
                .iter()
                .all(|id| !entries.contains_key(id) || seen.contains(id)),
        "Claude branch changed without confirming repair"
    );
    Ok(RepairStatus::Clean)
}

/// Detect native interruption evidence without treating a foreign user prompt as part of the cut.
fn interrupt_marker(entry: &Entry) -> bool {
    let Some(message) = &entry.message else {
        return false;
    };
    match &message["content"] {
        serde_json::Value::String(text) => text == "[Request interrupted by user]",
        serde_json::Value::Array(blocks) => {
            blocks.len() == 1
                && blocks[0].get("text").and_then(serde_json::Value::as_str)
                    == Some("[Request interrupted by user]")
        }
        _ => false,
    }
}

/// Attach a safe local diagnostic to a recovery failure.
pub(super) fn failure(operation: ProviderOperation, error: anyhow::Error) -> ProviderError {
    let cause = ProviderError::from(ProviderFailure::new(
        operation,
        ProviderFailureKind::HistoryConflict,
    ));
    match forge_provider_api::SanitizedMessage::new(error.to_string()) {
        Ok(explanation) => ProviderError::Explained {
            cause: Box::new(cause),
            explanation,
        },
        Err(_) => cause,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Inspect a versioned transcript captured across interrupted recovery and reopen.
    #[test]
    fn stopped_repair_requires_persisted_branch_proof() -> anyhow::Result<()> {
        let fixture = tempfile::tempdir()?;
        let projects = fixture.path().join("projects");
        let project = projects.join("workspace");
        std::fs::create_dir_all(&project)?;
        let session = Uuid::new_v4();
        let retained = Uuid::new_v4();
        let stopped = Uuid::new_v4();
        let marker = Uuid::new_v4();
        let journal = RepairJournal::new(fixture.path(), projects, &session.to_string()).unwrap();
        let intent = RepairIntent {
            scope: InvocationScope::new(
                forge_provider_api::testing::root_thread("repair"),
                "exchange",
            ),
            retained: Some(retained),
            inputs: vec![stopped],
            unanswered: true,
        };
        journal.save(&intent)?;
        let path = project.join(format!("{session}.jsonl"));
        let entries = [
            json!({"type":"assistant","uuid":retained,"parentUuid":null}),
            json!({"type":"user","uuid":stopped,"parentUuid":retained}),
            json!({"type":"user","uuid":marker,"parentUuid":stopped,
                "message":{"content":"[Request interrupted by user]"}}),
            json!({"type":"last-prompt","leafUuid":marker}),
        ];
        let transcript = entries
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        std::fs::write(&path, &transcript)?;
        let reopened = journal.load()?.unwrap();
        assert!(
            matches!(journal.status(&reopened)?, RepairStatus::Cut { target, .. } if target == stopped)
        );
        let foreign = json!({"type":"user","uuid":Uuid::new_v4(),"parentUuid":marker});
        std::fs::write(&path, format!("{transcript}\n{foreign}\n"))?;
        assert!(journal.status(&reopened).is_err());
        let cut =
            json!({"type":"last-prompt","leafUuid":retained,"rewound":true,"cutUuid":stopped});
        std::fs::write(&path, format!("{transcript}\n{cut}\n"))?;
        assert!(matches!(journal.status(&reopened)?, RepairStatus::Clean));
        journal.clear()?;
        assert!(journal.load()?.is_none());
        Ok(())
    }
}
