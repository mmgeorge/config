//! Own the editable planning directory separately from trusted capture and review records.

use super::{DeclarationFile, PlanArtifact, PlanDocument, PlanFileStore};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

/// Name of the state directory that holds every session's plan authoring trees.
pub(crate) const AUTHORING_ROOT: &str = "plan-workspaces";

/// Get whether `path` lies in a plan authoring tree of this executable's state directory.
///
/// A planner writes its design and proposed files there rather than in the workspace, so a
/// count of the workspace files an exchange changed leaves these paths out.
pub(crate) fn is_authoring_path(path: &str) -> bool {
    let marker = format!(
        "/state/v{}/{AUTHORING_ROOT}/",
        crate::storage::state::VERSION
    );
    path.replace('\\', "/").contains(&marker)
}

/// Bind ordinary planning tools to one authenticated plan and its version lease.
#[derive(Clone)]
pub(crate) struct PlanAuthoringScope {
    files: Arc<PlanFileStore>,
    session_id: String,
    plan_id: String,
    write_scope: crate::session::WriteScope,
}

impl PlanAuthoringScope {
    /// Get the plan this scope authorizes.
    pub(crate) fn plan_id(&self) -> &str {
        &self.plan_id
    }

    /// Get read-only comparison resources without granting writes to host storage.
    pub(crate) fn readable_directory(&self) -> PathBuf {
        self.files.plan_dir(&self.session_id, &self.plan_id)
    }

    /// Create an invocation capability without granting access to another plan's files.
    pub(crate) fn new(
        files: Arc<PlanFileStore>,
        session_id: String,
        plan_id: String,
        write_scope: crate::session::WriteScope,
    ) -> Self {
        Self {
            files,
            session_id,
            plan_id,
            write_scope,
        }
    }

    /// Get the sole planning authoring root.
    pub(crate) fn directory(&self) -> PathBuf {
        self.files
            .authoring_directory(&self.session_id, &self.plan_id)
    }

    /// Capture an existing source at its corresponding proposal path.
    pub(crate) fn capture(&self, path: &str) -> Result<PathBuf> {
        ensure!(
            self.write_scope != crate::session::WriteScope::Off,
            "planning file capture requires writes to be enabled"
        );
        self.files
            .authoring_capture(&self.session_id, &self.plan_id, path)?;
        Ok(self.directory().join("files").join(path))
    }

    /// Get the captured original text for read-only comparison and restoration.
    pub(crate) fn baseline_path(&self, path: &str) -> PathBuf {
        self.files
            .plan_dir(&self.session_id, &self.plan_id)
            .join("baseline")
            .join(path)
    }

    /// Resolve a supported source for permission evaluation without reading its contents.
    pub(crate) fn source_path(&self, path: &str) -> Result<PathBuf> {
        super::design::validate_path(path)?;
        checked_destination(&dunce::canonicalize(&self.files.workspace)?, path)
    }

    /// Import the current editable tree without modifying either the draft or saved revision.
    pub(crate) fn snapshot(&self) -> Result<(PlanDocument, PlanDocument)> {
        let original = self
            .files
            .read_working_document(&self.session_id, &self.plan_id)?;
        let document = self.files.authoring_import(&self.session_id, &original)?;
        Ok((original, document))
    }

    /// Extract all changed captures from the workspace snapshot for a submission retry.
    pub(crate) fn baseline_changes(
        &self,
        design: &super::DeclarationDesign,
        paths: &[String],
    ) -> Result<BaselineChanges> {
        let mut changes = BaselineChanges::default();
        for path in paths {
            let previous = design
                .baseline
                .get(path)
                .context("missing captured baseline")?;
            super::resolution_deadline::ResolutionDeadline::check()?;
            self.source_path(path)?;
            let source = super::workspace_source(&self.files.workspace, path)?;
            if source
                .as_ref()
                .is_some_and(|source| super::digest(source.as_bytes()) == previous.source_digest)
            {
                continue;
            }
            let current = source
                .map(|source| design.capture_baseline(path, &source).map(|(file, _)| file))
                .transpose()?;
            let text = current
                .as_ref()
                .map_or("", |file| file.editable_text.as_str());
            let diff = similar::TextDiff::from_lines(&previous.editable_text, text)
                .unified_diff()
                .header("previous baseline", "current baseline")
                .to_string();
            changes.report.push_str(&format!("\n{path}\n"));
            if current.is_none() {
                changes
                    .report
                    .push_str("The captured file was removed from the workspace. Keeping its proposal will plan to create it again.\n");
            }
            if diff.is_empty() {
                changes.report.push_str("Source changed. No changes to captured declarations or configuration values.\n");
            } else {
                changes.report.push_str(&diff);
            }
            changes.files.insert(path.clone(), current);
        }
        Ok(changes)
    }

    /// Publish the refreshed capture ledger while preserving every editable proposal.
    pub(crate) fn baseline_refresh(
        &self,
        document: &PlanDocument,
        changes: &BaselineChanges,
    ) -> Result<()> {
        let gate = self
            .files
            .working_document_gate(&self.session_id, &self.plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.files
            .authoring_require_current(&self.session_id, document)?;
        let directory = self.readable_directory();
        let original = self
            .files
            .authoring_ledger(&self.session_id, &self.plan_id)?;
        let mut ledger = original.clone();
        for (path, file) in &changes.files {
            match file {
                Some(file) => {
                    ledger.baseline.insert(path.clone(), file.clone());
                }
                None => {
                    ledger.baseline.remove(path);
                }
            }
        }
        // The ledger is the commit point. A retry can rebuild the read-only copies from it.
        let publish = || -> Result<()> {
            for path in changes.files.keys() {
                ledger.write_baseline_copy(&directory, path)?;
            }
            super::resolution_deadline::ResolutionDeadline::check()?;
            super::write_json_atomically(&directory.join("capture-ledger.json"), &ledger)
        };
        if let Err(error) = publish() {
            for path in changes.files.keys() {
                original
                    .write_baseline_copy(&directory, path)
                    .with_context(|| format!("restore baseline copy after {error:#}"))?;
            }
            return Err(error);
        }
        Ok(())
    }

    /// Save a validated submission's exact authored inputs outside the editable directory.
    pub(crate) fn snapshot_save(&self, document: &PlanDocument) -> Result<String> {
        self.files
            .authoring_snapshot_save(&self.session_id, document)
    }

    /// Describe draft identity and inventory without exposing host-owned storage.
    pub(crate) fn status(&self) -> Result<String> {
        let (original, document) = self.snapshot()?;
        let design = document
            .design
            .as_ref()
            .context("planning requires a declaration design")?;
        let dirty = original
            .design
            .as_ref()
            .and_then(|design| design.authoring_fingerprint.as_ref())
            != design.authoring_fingerprint.as_ref();
        let revision = if crate::storage::state::database(&self.files.root).exists() {
            crate::storage::HistoryReader::current(&self.files.root)?
                .load_plan(&self.plan_id)?
                .filter(|plan| plan.session_id == self.session_id && plan.model_revision > 0)
                .map(|plan| plan.model_revision.to_string())
                .unwrap_or_else(|| "none".into())
        } else {
            "none".into()
        };
        let inventory = design
            .baseline
            .keys()
            .chain(design.proposed_source.keys())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .map(|path| {
                let state = match (design.baseline.get(path), design.proposed_source.get(path)) {
                    (_, None) => "deletion",
                    (None, Some(_)) => "addition",
                    (Some(baseline), Some(proposed)) if &baseline.editable_text == proposed => {
                        "unchanged"
                    }
                    (Some(_), Some(_)) => "modified",
                };
                format!("  files/{path} ({state})")
            })
            .collect::<Vec<_>>()
            .join("\n");
        Ok(format!(
            "Plan {} | planning\nDirectory: {}\nSubmitted revision: {revision}\nDraft: {}\nProposal files:\n{inventory}",
            self.plan_id,
            self.directory().display(),
            if dirty {
                "changed since the saved snapshot"
            } else {
                "matches the saved snapshot"
            },
        ))
    }
}

/// Retain trusted original source evidence independently of mutable proposal contents.
#[derive(Clone, Default, Deserialize, Serialize)]
struct CaptureLedger {
    baseline: BTreeMap<String, DeclarationFile>,
}

impl CaptureLedger {
    fn write_baseline_copy(&self, directory: &Path, path: &str) -> Result<()> {
        let destination = checked_destination(&directory.join("baseline"), path)?;
        if let Some(file) = self.baseline.get(path) {
            fs::create_dir_all(destination.parent().context("baseline has no parent")?)?;
            super::write_text_atomically(&destination, &file.editable_text)?;
        } else if destination.try_exists()? {
            fs::remove_file(destination)?;
        }
        Ok(())
    }
}

/// Hold replacement capture evidence and the corresponding old-to-new diff until publication.
#[derive(Default)]
pub(crate) struct BaselineChanges {
    files: BTreeMap<String, Option<DeclarationFile>>,
    pub(crate) report: String,
}

/// Recover a capture interrupted between copying its file and recording its baseline.
#[derive(Deserialize, Serialize)]
struct CaptureJournal {
    path: String,
    baseline: DeclarationFile,
}

/// Carry one verified authored-file replacement into the recoverable artifact transaction.
pub(super) struct AuthoringFileImage {
    path: String,
    prior: Vec<u8>,
    target: Vec<u8>,
}

impl AuthoringFileImage {
    /// Consume the image into its relative path and original/replacement bytes.
    pub(super) fn into_parts(self) -> (String, Vec<u8>, Vec<u8>) {
        (self.path, self.prior, self.target)
    }
}

impl PlanFileStore {
    /// Rebuild read-only baseline copies from the committed ledger after an interrupted refresh.
    pub(crate) fn restore_baseline_copies(&self, session_id: &str, plan_id: &str) -> Result<()> {
        if !self.authoring_directory(session_id, plan_id).try_exists()? {
            return Ok(());
        }
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        let ledger = self.authoring_ledger(session_id, plan_id)?;
        let directory = self.plan_dir(session_id, plan_id);
        for (path, file) in &ledger.baseline {
            let destination = checked_destination(&directory.join("baseline"), path)?;
            match fs::read(&destination) {
                Ok(bytes) if bytes == file.editable_text.as_bytes() => continue,
                Ok(_) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error).with_context(|| format!("read baseline {path}")),
            }
            ledger.write_baseline_copy(&directory, path)?;
        }
        Ok(())
    }

    /// Copy unfinished authoring inputs and capture evidence without linking writable files.
    pub(super) fn authoring_copy(
        &self,
        source_session_id: &str,
        source_plan_id: &str,
        target_session_id: &str,
        target: &PlanDocument,
    ) -> Result<()> {
        let source_root = self.authoring_directory(source_session_id, source_plan_id);
        if !source_root.try_exists()? {
            return Ok(());
        }
        let gate = self.working_document_gate(source_session_id, source_plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        ensure!(
            !self
                .plan_dir(source_session_id, source_plan_id)
                .join("capture-recovery.json")
                .exists(),
            "recover the interrupted capture before forking its planning directory"
        );
        let source = directory_contents(&source_root)?;
        let ledger = self.authoring_ledger(source_session_id, source_plan_id)?;
        self.authoring_create(target_session_id, target)?;
        let target_root = self.authoring_directory(target_session_id, &target.plan_id);
        for path in directory_contents(&target_root)?.keys() {
            if !source.contains_key(path) {
                fs::remove_file(checked_destination(&target_root, path)?)?;
            }
        }
        for (path, text) in &source {
            let destination = checked_destination(&target_root, path)?;
            fs::create_dir_all(
                destination
                    .parent()
                    .context("forked proposal has no parent")?,
            )?;
            super::write_text_atomically(&destination, text)?;
        }
        for (path, baseline) in &ledger.baseline {
            let destination = checked_destination(
                &self
                    .plan_dir(target_session_id, &target.plan_id)
                    .join("baseline"),
                path,
            )?;
            fs::create_dir_all(
                destination
                    .parent()
                    .context("forked baseline has no parent")?,
            )?;
            super::write_text_atomically(&destination, &baseline.editable_text)?;
        }
        ensure!(
            directory_contents(&source_root)? == source,
            "source planning directory changed during fork"
        );
        super::write_json_atomically(
            &self
                .plan_dir(target_session_id, &target.plan_id)
                .join("capture-ledger.json"),
            &ledger,
        )
    }

    /// Get the authoring root, which contains no host-owned metadata or snapshots.
    pub(crate) fn authoring_directory(&self, session_id: &str, plan_id: &str) -> PathBuf {
        crate::storage::state::directory(&self.root)
            .join(AUTHORING_ROOT)
            .join(session_id)
            .join(plan_id)
    }

    /// Remove the editable tree of a plan that no longer accepts edits.
    ///
    /// A cancelled plan's draft must not stay beside a later plan's directory, where its
    /// files and review notes could be read as current work. The submitted revisions under
    /// the plan directory remain, so transcript rows still open the cancelled plan.
    pub(crate) fn authoring_remove(&self, session_id: &str, plan_id: &str) -> Result<()> {
        let directory = self.authoring_directory(session_id, plan_id);
        match std::fs::remove_dir_all(&directory) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(error) => {
                Err(error).with_context(|| format!("remove plan workspace {}", directory.display()))
            }
        }
    }

    /// Create the initial editable tree without replacing an existing draft.
    pub(crate) fn authoring_create(&self, session_id: &str, document: &PlanDocument) -> Result<()> {
        let Some(design) = &document.design else {
            return Ok(());
        };
        let destination = self.authoring_directory(session_id, &document.plan_id);
        ensure!(
            !destination.exists(),
            "planning directory already exists: {}",
            destination.display()
        );
        let parent = destination
            .parent()
            .context("planning directory has no parent")?;
        fs::create_dir_all(parent)?;
        let temporary = tempfile::Builder::new()
            .prefix("creating-")
            .tempdir_in(parent)?;
        let directory = temporary.path();
        fs::create_dir_all(directory.join("docs"))?;
        fs::create_dir_all(directory.join("files"))?;
        for artifact in PlanArtifact::AUTHORED {
            super::write_text_atomically(
                &directory.join(artifact_file(artifact)?),
                &super::design_artifact_text(artifact, &document.title, design)?,
            )?;
        }
        for (path, text) in &design.proposed_source {
            let destination = checked_destination(&directory.join("files"), path)?;
            fs::create_dir_all(destination.parent().context("proposal has no parent")?)?;
            super::write_text_atomically(&destination, text)?;
        }
        for (path, baseline) in &design.baseline {
            let destination = checked_destination(
                &self
                    .plan_dir(session_id, &document.plan_id)
                    .join("baseline"),
                path,
            )?;
            fs::create_dir_all(destination.parent().context("baseline has no parent")?)?;
            super::write_text_atomically(&destination, &baseline.editable_text)?;
        }
        super::write_json_atomically(
            &self
                .plan_dir(session_id, &document.plan_id)
                .join("capture-ledger.json"),
            &CaptureLedger {
                baseline: design.baseline.clone(),
            },
        )?;
        fs::rename(directory, destination)?;
        self.authoring_progress(session_id, document)?;
        Ok(())
    }

    /// Observe ordinary file progress once and determine whether review needs resubmission.
    pub(crate) fn authoring_progress(
        &self,
        session_id: &str,
        document: &PlanDocument,
    ) -> Result<(bool, bool)> {
        let directory = self.authoring_directory(session_id, &document.plan_id);
        if !directory.try_exists()? {
            ensure!(
                document
                    .design
                    .as_ref()
                    .is_none_or(|design| design.authoring_fingerprint.is_none()),
                "submitted planning directory is missing"
            );
            return Ok((false, false));
        }
        let inventory = directory_contents(&directory)?;
        let ledger = self.authoring_ledger(session_id, &document.plan_id)?;
        let fingerprint = authoring_fingerprint(&inventory, &ledger.baseline)?;
        let path = self
            .plan_dir(session_id, &document.plan_id)
            .join("draft-observation.json");
        let previous: Option<String> = match fs::read(&path) {
            Ok(bytes) => Some(super::codec::from_slice(&bytes)?),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error).context("read planning progress"),
        };
        let changed = previous.as_ref() != Some(&fingerprint);
        if changed {
            super::write_json_atomically(&path, &fingerprint)?;
        }
        let dirty = document
            .design
            .as_ref()
            .and_then(|design| design.authoring_fingerprint.as_ref())
            != Some(&fingerprint);
        Ok((changed, dirty))
    }

    /// Capture baseline evidence and prepare a fixed-path copy with recoverable bookkeeping.
    pub(crate) fn authoring_capture(
        &self,
        session_id: &str,
        plan_id: &str,
        path: &str,
    ) -> Result<()> {
        super::design::validate_path(path)?;
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.authoring_capture_recover(session_id, plan_id)?;
        let ledger = self.authoring_ledger(session_id, plan_id)?;
        if let Some(baseline) = ledger.baseline.get(path) {
            let source = checked_destination(&dunce::canonicalize(&self.workspace)?, path)?;
            ensure!(
                super::digest(&fs::read(source)?) == baseline.source_digest,
                "{path}: repository source changed since capture; the original baseline was preserved"
            );
            return Ok(());
        }
        let root = self.authoring_directory(session_id, plan_id).join("files");
        let destination = checked_destination(&root, path)?;
        ensure!(
            !destination.try_exists()?,
            "{path}: an uncaptured proposal already exists; capture never overwrites it"
        );
        let source = checked_destination(&dunce::canonicalize(&self.workspace)?, path)?;
        ensure!(
            fs::symlink_metadata(&source)?.is_file(),
            "{path}: capture requires a regular source file"
        );
        let source =
            fs::read_to_string(source).with_context(|| format!("read capture source {path}"))?;
        let document = self.read_working_document(session_id, plan_id)?;
        let design = document
            .design
            .context("capture requires a declaration design")?;
        let (baseline, _) = design.capture_baseline(path, &source)?;
        super::write_json_atomically(
            &self
                .plan_dir(session_id, plan_id)
                .join("capture-recovery.json"),
            &CaptureJournal {
                path: path.into(),
                baseline,
            },
        )?;
        self.authoring_capture_recover(session_id, plan_id)
    }

    /// Finish only the identified capture, refusing to overwrite newer ordinary edits.
    fn authoring_capture_recover(&self, session_id: &str, plan_id: &str) -> Result<()> {
        let directory = self.plan_dir(session_id, plan_id);
        let journal_path = directory.join("capture-recovery.json");
        let journal: CaptureJournal = match fs::read(&journal_path) {
            Ok(bytes) => super::codec::from_slice(&bytes)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(error) => return Err(error).context("read capture recovery"),
        };
        let destination = checked_destination(
            &self.authoring_directory(session_id, plan_id).join("files"),
            &journal.path,
        )?;
        fs::create_dir_all(destination.parent().context("proposal has no parent")?)?;
        if destination.try_exists()? {
            ensure!(
                fs::read_to_string(&destination)? == journal.baseline.editable_text,
                "{}: interrupted capture conflicts with a newer proposal",
                journal.path
            );
        } else {
            use std::io::Write;
            let mut file = fs::OpenOptions::new()
                .create_new(true)
                .write(true)
                .open(&destination)?;
            file.write_all(journal.baseline.editable_text.as_bytes())?;
            file.sync_all()?;
        }
        let mut ledger = self.authoring_ledger(session_id, plan_id)?;
        ledger
            .baseline
            .insert(journal.path.clone(), journal.baseline.clone());
        let baseline = checked_destination(&directory.join("baseline"), &journal.path)?;
        fs::create_dir_all(baseline.parent().context("baseline has no parent")?)?;
        super::write_text_atomically(&baseline, &journal.baseline.editable_text)?;
        super::write_json_atomically(&directory.join("capture-ledger.json"), &ledger)?;
        fs::remove_file(journal_path)?;
        Ok(())
    }

    /// Load trusted capture evidence, never inferring origin from file creation events.
    fn authoring_ledger(&self, session_id: &str, plan_id: &str) -> Result<CaptureLedger> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("capture-ledger.json");
        Ok(super::codec::from_slice(
            &fs::read(&path).with_context(|| format!("read {}", path.display()))?,
        )?)
    }

    /// Read every editable input and bind its raw bytes to a deterministic fingerprint.
    pub(crate) fn authoring_import(
        &self,
        session_id: &str,
        original: &PlanDocument,
    ) -> Result<PlanDocument> {
        ensure!(
            !self
                .plan_dir(session_id, &original.plan_id)
                .join("capture-recovery.json")
                .exists(),
            "an interrupted capture must recover before the planning directory can be submitted"
        );
        let directory = self.authoring_directory(session_id, &original.plan_id);
        let inventory = directory_contents(&directory)?;
        let ledger = self.authoring_ledger(session_id, &original.plan_id)?;
        let mut document = original.clone();
        let design = document
            .design
            .as_mut()
            .context("planning requires a declaration design")?;
        design.baseline = ledger.baseline;
        design.baseline_calls.clear();
        design.moved.clear();
        design.proposed_source.clear();
        design.artifact_text.clear();
        design.parsed = None;
        for artifact in PlanArtifact::AUTHORED {
            let relative = artifact_file(artifact)?;
            let text = inventory.get(&relative).with_context(|| {
                format!(
                    "{}: required planning artifact is missing",
                    directory.join(&relative).display()
                )
            })?;
            design.artifact_text.insert(
                artifact.path().into(),
                super::design_artifact::lf_line_endings(text),
            );
        }
        for (path, text) in &inventory {
            super::resolution_deadline::ResolutionDeadline::check()?;
            if let Some(path) = path.strip_prefix("files/") {
                super::design::validate_path(path)?;
                design.proposed_source.insert(path.into(), text.clone());
            } else {
                ensure!(
                    PlanArtifact::AUTHORED
                        .into_iter()
                        .any(|artifact| artifact_file(artifact)
                            .is_ok_and(|relative| &relative == path)),
                    "{path}: only authored docs and proposed files belong in the planning directory"
                );
            }
        }
        if let Some(title) = design.artifact_text[PlanArtifact::Overview.path()]
            .lines()
            .find_map(|line| line.strip_prefix("# "))
            .filter(|title| !title.trim().is_empty())
        {
            document.title = title.trim().into();
        }
        let fingerprint = authoring_fingerprint(&inventory, &design.baseline)?;
        let unchanged = design.authoring_fingerprint.as_ref() == Some(&fingerprint);
        design.authoring_fingerprint = Some(fingerprint);
        if !unchanged {
            document.version = original
                .version
                .checked_add(1)
                .context("plan version exhausted")?;
        }
        Ok(document)
    }

    /// Preserve exact validated authored inputs in a host-owned immutable snapshot.
    pub(crate) fn authoring_snapshot_save(
        &self,
        session_id: &str,
        document: &PlanDocument,
    ) -> Result<String> {
        self.authoring_require_current(session_id, document)?;
        let fingerprint = super::digest(&super::codec::to_vec(document)?);
        let directory = self
            .plan_dir(session_id, &document.plan_id)
            .join("authoring-snapshots")
            .join(&fingerprint);
        if directory.try_exists()? {
            let saved: PlanDocument =
                super::codec::from_slice(&fs::read(directory.join("document.json"))?)?;
            ensure!(
                saved == *document,
                "snapshot identity was reused for different content"
            );
            return Ok(fingerprint);
        }
        let parent = directory.parent().context("snapshot has no parent")?;
        fs::create_dir_all(parent)?;
        let staging = tempfile::Builder::new()
            .prefix("pending-")
            .tempdir_in(parent)?;
        let temporary = staging.path();
        let design = document
            .design
            .as_ref()
            .context("snapshot requires semantic structure")?;
        for artifact in PlanArtifact::AUTHORED {
            let path = temporary.join(artifact_file(artifact)?);
            fs::create_dir_all(path.parent().context("artifact has no parent")?)?;
            super::write_text_atomically(&path, &design.artifact_text[artifact.path()])?;
        }
        fs::create_dir_all(temporary.join("files"))?;
        for (path, text) in &design.proposed_source {
            let path = checked_destination(&temporary.join("files"), path)?;
            fs::create_dir_all(path.parent().context("snapshot file has no parent")?)?;
            super::write_text_atomically(&path, text)?;
        }
        super::write_json_atomically(&temporary.join("document.json"), document)?;
        self.authoring_require_current(session_id, document)?;
        fs::rename(temporary, directory)?;
        Ok(fingerprint)
    }

    /// Load only a host-owned snapshot addressed by a verified content digest.
    pub(crate) fn authoring_snapshot_read(
        &self,
        session_id: &str,
        plan_id: &str,
        fingerprint: &str,
    ) -> Result<PlanDocument> {
        ensure!(
            fingerprint.len() == 64 && fingerprint.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "invalid authoring snapshot identity"
        );
        let path = self
            .plan_dir(session_id, plan_id)
            .join("authoring-snapshots")
            .join(fingerprint)
            .join("document.json");
        let document: PlanDocument = super::codec::from_slice(&fs::read(path)?)?;
        ensure!(
            document.plan_id == plan_id
                && super::digest(&super::codec::to_vec(&document)?) == fingerprint,
            "authoring snapshot belongs to another submission"
        );
        self.authoring_require_current(session_id, &document)?;
        Ok(document)
    }

    /// Reject acceptance or publication when editable bytes no longer match their snapshot.
    pub(crate) fn authoring_require_current(
        &self,
        session_id: &str,
        document: &PlanDocument,
    ) -> Result<()> {
        let Some(expected) = document
            .design
            .as_ref()
            .and_then(|design| design.authoring_fingerprint.as_ref())
        else {
            return Ok(());
        };
        let current = self.authoring_import(session_id, document)?;
        ensure!(
            current
                .design
                .as_ref()
                .and_then(|design| design.authoring_fingerprint.as_ref())
                == Some(expected),
            "planning directory changed; submit the current draft before review or acceptance"
        );
        Ok(())
    }

    /// Bind reviewer edits to raw authored files without replacing concurrent ordinary edits.
    pub(super) fn authoring_review_prepare(
        &self,
        session_id: &str,
        document: &mut PlanDocument,
    ) -> Result<()> {
        let Some(fingerprint) = document
            .design
            .as_ref()
            .and_then(|design| design.authoring_fingerprint.clone())
        else {
            return Ok(());
        };
        let original = self.read_working_document(session_id, &document.plan_id)?;
        if original
            .design
            .as_ref()
            .and_then(|design| design.authoring_fingerprint.as_ref())
            != Some(&fingerprint)
        {
            self.authoring_require_current(session_id, document)?;
            return Ok(());
        }
        self.authoring_require_current(session_id, &original)?;
        let inventory = authored_inputs(document)?;
        let design = document
            .design
            .as_mut()
            .context("review requires semantic structure")?;
        design.authoring_fingerprint = Some(authoring_fingerprint(&inventory, &design.baseline)?);
        Ok(())
    }

    /// Prepare file images for the existing recoverable review transaction.
    pub(super) fn authoring_review_images(
        &self,
        session_id: &str,
        document: &PlanDocument,
    ) -> Result<Vec<AuthoringFileImage>> {
        if document
            .design
            .as_ref()
            .is_none_or(|design| design.authoring_fingerprint.is_none())
        {
            return Ok(Vec::new());
        }
        let original = self.read_working_document(session_id, &document.plan_id)?;
        let root = self.authoring_directory(session_id, &document.plan_id);
        let current = directory_contents(&root)?;
        let desired = authored_inputs(document)?;
        let current_fingerprint = authoring_fingerprint(
            &current,
            &self
                .authoring_ledger(session_id, &document.plan_id)?
                .baseline,
        )?;
        ensure!(
            [&original, document].into_iter().any(|candidate| candidate
                .design
                .as_ref()
                .and_then(|design| design.authoring_fingerprint.as_ref())
                == Some(&current_fingerprint)),
            "planning directory changed before the reviewer edit"
        );
        ensure!(
            current.keys().eq(desired.keys()),
            "review edits cannot silently add or remove proposal paths"
        );
        Ok(desired
            .into_iter()
            .filter_map(|(path, target)| {
                let prior = &current[&path];
                (prior != &target).then(|| AuthoringFileImage {
                    path,
                    prior: prior.as_bytes().to_vec(),
                    target: target.into_bytes(),
                })
            })
            .collect())
    }
}

/// Render only authored inputs, preserving text overrides and keeping host data outside the draft.
/// Fingerprint what the planner authored: each file's text with LF line endings, and the
/// captured baselines.
///
/// Providers on Windows may write CRLF files, and Harness rewrites the directory in its own LF
/// form, so raw bytes would differ while the plan stays the same. Every side of the comparison,
/// submission, settlement, and review, uses this one form.
fn authoring_fingerprint(
    inventory: &BTreeMap<String, String>,
    baseline: &BTreeMap<String, DeclarationFile>,
) -> Result<String> {
    let authored = inventory
        .iter()
        .map(|(path, text)| (path, super::design_artifact::lf_line_endings(text)))
        .collect::<BTreeMap<_, _>>();
    Ok(super::digest(&super::codec::to_vec(&(
        &authored, baseline,
    ))?))
}

fn authored_inputs(document: &PlanDocument) -> Result<BTreeMap<String, String>> {
    let design = document
        .design
        .as_ref()
        .context("authoring requires semantic structure")?;
    let mut inputs = BTreeMap::new();
    for artifact in PlanArtifact::AUTHORED {
        inputs.insert(
            artifact_file(artifact)?,
            super::design_artifact_text(artifact, &document.title, design)?,
        );
    }
    inputs.extend(
        design
            .proposed_source
            .iter()
            .map(|(path, text)| (format!("files/{path}"), text.clone())),
    );
    Ok(inputs)
}

/// Map a planning artifact to the only supported editable Markdown path.
fn artifact_file(artifact: PlanArtifact) -> Result<String> {
    ensure!(
        artifact.authored(),
        "execution artifacts are not planning files"
    );
    Ok(format!(
        "docs/{}",
        artifact.path().trim_start_matches("plan:")
    ))
}

/// Resolve a portable relative file path without traversing links.
pub(super) fn checked_destination(root: &Path, relative: &str) -> Result<PathBuf> {
    ensure!(
        !relative.is_empty() && !relative.contains(['\\', ':']),
        "invalid relative planning path: {relative}"
    );
    let path = Path::new(relative);
    ensure!(
        path.components()
            .all(|component| matches!(component, Component::Normal(_))),
        "planning paths must be repository-relative without traversal: {relative}"
    );
    let mut resolved = root.to_path_buf();
    for ancestor in root.ancestors() {
        match fs::symlink_metadata(ancestor) {
            Ok(metadata) => ensure!(
                !metadata.file_type().is_symlink(),
                "planning root traverses a symbolic link: {}",
                ancestor.display()
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(error).context("inspect planning root"),
        }
    }
    for component in path.components() {
        resolved.push(component);
        match fs::symlink_metadata(&resolved) {
            Ok(metadata) => ensure!(
                !metadata.file_type().is_symlink(),
                "planning path traverses a symbolic link: {}",
                resolved.display()
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(error).with_context(|| format!("inspect {}", resolved.display()));
            }
        }
    }
    let root = crate::permissions::directory::resolve_path(root, root)?;
    let resolved = crate::permissions::directory::resolve_path(&resolved, &root)?;
    ensure!(
        crate::permissions::matcher::path_starts_with(&resolved, &root),
        "planning path escapes its owning directory: {relative}"
    );
    Ok(resolved)
}

/// Inventory all regular files, rejecting unsupported entries rather than silently skipping them.
fn directory_contents(root: &Path) -> Result<BTreeMap<String, String>> {
    ensure!(
        fs::symlink_metadata(root)?.is_dir(),
        "planning directory is missing or not a directory"
    );
    let mut pending = vec![root.to_path_buf()];
    let mut contents = BTreeMap::new();
    while let Some(directory) = pending.pop() {
        for entry in fs::read_dir(&directory)? {
            super::resolution_deadline::ResolutionDeadline::check()?;
            let entry = entry?;
            let path = entry.path();
            let relative = path
                .strip_prefix(root)?
                .components()
                .map(|component| {
                    component
                        .as_os_str()
                        .to_str()
                        .context("planning path is not UTF-8")
                })
                .collect::<Result<Vec<_>>>()?
                .join("/");
            ensure!(
                relative == "docs"
                    || relative == "files"
                    || relative.starts_with("docs/")
                    || relative.starts_with("files/"),
                "{relative}: planning directory contains only docs/ and files/"
            );
            let path = checked_destination(root, &relative)?;
            let kind = entry.file_type()?;
            if kind.is_dir() {
                ensure!(
                    !relative.starts_with("docs/"),
                    "{relative}: design artifacts cannot be nested"
                );
                pending.push(path);
            } else {
                ensure!(
                    kind.is_file(),
                    "{relative}: planning inputs must be regular files"
                );
                contents.insert(
                    relative,
                    fs::read_to_string(&path)
                        .with_context(|| format!("read {}", path.display()))?,
                );
            }
        }
    }
    ensure!(
        root.join("docs").is_dir() && root.join("files").is_dir(),
        "planning directory requires docs/ and files/"
    );
    Ok(contents)
}

impl PlanFileStore {
    /// Read the inline review comments saved on the reviewed working document.
    ///
    /// Every acceptance source reads them here: a decision from the review, automatic
    /// acceptance, and the continuation after acceptance questions. A saved comment turns the
    /// acceptance into a revision first. Questions and blank comments are not revision feedback,
    /// and a follow-up carries its conversation so the planner sees what it answers.
    pub(crate) fn saved_review_comments(
        &self,
        reviewed: &super::review_source::PlanReviewSource,
    ) -> Result<Vec<super::PlanAnnotationInput>> {
        let mut store = super::review_annotation::ReviewAnnotationStore::open(
            reviewed.path.with_extension("json"),
            reviewed.saved_digest.clone(),
        )?;
        if reviewed.document.design.is_some() {
            store.reanchor(&reviewed.rendered.navigation)?;
        }
        store
            .annotation()
            .iter()
            .filter(|annotation| {
                annotation.kind == super::ReviewAnnotationKind::Comment
                    && !annotation.source.body.trim().is_empty()
            })
            .map(|annotation| {
                let mut source = annotation.source.clone();
                if annotation.parent_id.is_some() {
                    let thread = store.thread_context(annotation)?;
                    source.body = format!(
                        "Conversation context:
{thread}Requested change:
{}",
                        source.body
                    );
                }
                Ok(source)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Submission accepts arbitrary overview Markdown and rejects a whitespace-only draft.
    #[tokio::test]
    async fn overview_needs_only_non_whitespace_text() -> Result<()> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let files = PlanFileStore::new(root.path().join("storage"), root.path())?;
        let plan = files
            .prepare_entry("session", "Write a plan.".into(), None, 1)?
            .record;
        let overview = files
            .authoring_directory("session", &plan.id)
            .join("docs/overview.md");
        let original = files.read_working_document("session", &plan.id)?;
        for text in [
            " \t\n",
            "Free-form text.",
            "## Anything\nNotes.\n## Anything\nMore notes.\n## Decisions\n- Unstructured choice.",
        ] {
            fs::write(&overview, text)?;
            let document = files.authoring_import("session", &original)?;
            let design = document.design.unwrap();
            let result = design.validated(root.path(), &document.title).await;
            if text.trim().is_empty() {
                assert!(
                    result
                        .err()
                        .context("blank overview passed")?
                        .to_string()
                        .contains("non-whitespace")
                );
            } else {
                let mut submitted = original.clone();
                submitted.design = Some(result?.into_design());
                submitted.validate_for_submission()?;
            }
        }
        Ok(())
    }

    /// Reassess source evidence for a new plan without discarding the seeded proposal.
    #[test]
    fn replanned_files_capture_current_sources_without_overwriting_proposals() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        fs::write(
            temporary.path().join("source.rs"),
            "pub fn source() { previous(); }\n",
        )?;
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let first = files.prepare_entry("session", "Revise source.".into(), None, 1)?;
        let overview = files
            .authoring_directory("session", &first.record.id)
            .join("docs/overview.md");
        assert_eq!(fs::read(&overview)?, b"");
        let authored = "# Source revision\n\n## Objective\nPreserve the caller contract.\n";
        fs::write(overview, authored)?;
        files.authoring_capture("session", &first.record.id, "source.rs")?;
        let proposed = "pub fn source();\nChange\n  Preserve the caller contract.\n";
        fs::write(
            files
                .authoring_directory("session", &first.record.id)
                .join("files/source.rs"),
            proposed,
        )?;
        let mut seed = files.authoring_import(
            "session",
            &files.read_working_document("session", &first.record.id)?,
        )?;
        seed.plan_id = "replanned".into();
        seed.version = 1;
        let current = "pub fn source() { current(); }\n";
        fs::write(temporary.path().join("source.rs"), current)?;
        files.prepare_entry("session", "Reassess source.".into(), Some(seed), 2)?;
        assert_eq!(
            fs::read_to_string(
                files
                    .authoring_directory("session", "replanned")
                    .join("docs/overview.md")
            )?,
            authored
        );
        let replanned = files.authoring_import(
            "session",
            &files.read_working_document("session", "replanned")?,
        )?;
        let design = replanned.design.unwrap();
        assert_eq!(design.proposed_source["source.rs"], proposed);
        assert_eq!(
            design.baseline["source.rs"].source_digest,
            super::super::digest(current.as_bytes())
        );
        design.check_workspace(temporary.path())?;
        Ok(())
    }

    /// Keep a fork's unfinished files independent and recover a precisely identified capture.
    #[test]
    fn interrupted_capture_and_fork_preserve_owned_files() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Capture.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let original = files.read_working_document("session", &plan_id)?;
        let (baseline, _) = original
            .design
            .as_ref()
            .unwrap()
            .capture_baseline("source.rs", "pub fn original() {}\n")?;
        super::super::write_json_atomically(
            &files
                .plan_dir("session", &plan_id)
                .join("capture-recovery.json"),
            &CaptureJournal {
                path: "source.rs".into(),
                baseline,
            },
        )?;
        assert!(files.authoring_import("session", &original).is_err());
        files.authoring_capture_recover("session", &plan_id)?;
        let source_root = files.authoring_directory("session", &plan_id);
        fs::write(source_root.join("files/source.rs"), "pub fn changed();\n")?;
        files.copy_plan("session", &plan_id, "fork", "fork-plan")?;
        let fork_root = files.authoring_directory("fork", "fork-plan");
        assert_eq!(
            fs::read_to_string(fork_root.join("files/source.rs"))?,
            "pub fn changed();\n"
        );
        fs::write(fork_root.join("files/source.rs"), "pub fn independent();\n")?;
        assert_eq!(
            fs::read_to_string(source_root.join("files/source.rs"))?,
            "pub fn changed();\n"
        );
        assert!(
            files
                .authoring_ledger("fork", "fork-plan")?
                .baseline
                .contains_key("source.rs")
        );
        Ok(())
    }

    /// Restore the committed baseline after a refresh is interrupted before ledger publication.
    #[test]
    fn interrupted_refresh_restores_copies_without_recreating_deleted_proposals() -> Result<()> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        fs::write(root.path().join("source.rs"), "pub fn existing() {}\n")?;
        let files = PlanFileStore::new(root.path().join("storage"), root.path())?;
        let plan = files
            .prepare_entry("session", "Revise the callable.".into(), None, 1)?
            .record;
        files.authoring_capture("session", &plan.id, "source.rs")?;
        let baseline = files
            .plan_dir("session", &plan.id)
            .join("baseline/source.rs");
        let committed = fs::read(&baseline)?;
        let proposal = files
            .authoring_directory("session", &plan.id)
            .join("files/source.rs");
        fs::remove_file(&proposal)?;
        fs::write(&baseline, "pub fn newer();\n")?;
        files.restore_baseline_copies("session", &plan.id)?;
        assert_eq!(fs::read(&baseline)?, committed);
        assert!(!proposal.exists());
        Ok(())
    }

    /// A planned-test deletion recomputes the check preview the tests feed and persists the full
    /// render of the edited document, so the review captured from the draft opens again.
    #[tokio::test]
    async fn deleted_test_leaves_a_draft_review_that_opens() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        fs::write(
            temporary.path().join(".forge.json"),
            r#"{"checks":{"test":{"trigger":{"auto":true},"action":{"category":"test","adapter":{"script":{"command":["unused-test-runner"]}}}}}}"#,
        )?;
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a plan.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        fs::write(
            directory.join("docs/overview.md"),
            "# Plan\n\n## Objective\nKeep state.\n\n## Background\nNo changes yet.\n\n## Design\nRetain the public contract.\n",
        )?;
        fs::write(
            directory.join("docs/tests.md"),
            "## tests.rs\n- new `checks_state`: State stays independent.\n- new `checks_order`: Order stays stable.\n",
        )?;
        let original = files.read_working_document("session", &plan_id)?;
        let mut document = files.authoring_import("session", &original)?;
        let validated = document
            .design
            .as_ref()
            .unwrap()
            .validated(temporary.path(), &document.title)
            .await?;
        document.design = Some(validated.into_design());
        assert!(document.design.as_ref().unwrap().check_preview_current()?);
        let (_, _, checksum) = files.submit_validated_document_revision_if_unchanged(
            "session",
            &plan_id,
            1,
            original.version,
            &original,
            document.clone(),
        )?;
        let (reviewed, _, checksum) = files
            .remove_tests(
                "session",
                &plan_id,
                document.version,
                &checksum,
                &[super::super::DesignTestSelection {
                    file: "tests.rs".into(),
                    name: "checks_state".into(),
                }],
                None,
            )
            .await?;
        assert!(reviewed.design.as_ref().unwrap().check_preview_current()?);
        assert_eq!(
            fs::read_to_string(files.plan_dir("session", &plan_id).join("working.md"))?,
            files.render_review(&reviewed)?.markdown
        );
        files.capture_review_source("session", &plan_id, None, &checksum)?;
        Ok(())
    }

    /// Toggling a check off and on again leaves the plan the planner submitted. The planned
    /// tests select the test check without `docs/checks.md` naming it, and the toggle regenerates
    /// that file, which a planner on Windows wrote with CRLF line endings, so the edited document
    /// spells the same selection differently and carries another authoring fingerprint; neither
    /// makes it a different plan.
    #[tokio::test]
    async fn undone_check_toggle_plans_the_submitted_work() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        fs::write(
            temporary.path().join(".forge.json"),
            r#"{"checks":{"test":{"trigger":{"auto":true},"action":{"category":"test","adapter":{"script":{"command":["unused-test-runner"]}}}}}}"#,
        )?;
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a plan.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        let crlf = |text: &str| text.replace('\n', "\r\n");
        fs::write(
            directory.join("docs/overview.md"),
            crlf(
                "# Plan\n\n## Objective\nKeep state.\n\n## Background\nNo changes yet.\n\n## Design\nRetain the public contract.\n",
            ),
        )?;
        fs::write(
            directory.join("docs/tests.md"),
            crlf("## tests.rs\n- new `checks_state`: State stays independent.\n"),
        )?;
        fs::write(directory.join("docs/checks.md"), crlf("\n"))?;
        let original = files.read_working_document("session", &plan_id)?;
        let mut document = files.authoring_import("session", &original)?;
        let validated = document
            .design
            .as_ref()
            .unwrap()
            .validated(temporary.path(), &document.title)
            .await?;
        document.design = Some(validated.into_design());
        let (submitted, _, mut checksum) = files.submit_validated_document_revision_if_unchanged(
            "session",
            &plan_id,
            1,
            original.version,
            &original,
            document,
        )?;
        let mut reviewed = submitted.clone();
        for enabled in [false, true] {
            let (edited, _, edited_checksum) = files
                .update_checks(
                    "session",
                    &plan_id,
                    reviewed.version,
                    &checksum,
                    super::super::PlanCheckSelection {
                        enabled: &BTreeMap::from([("test".to_owned(), enabled)]),
                        test_scopes: &BTreeMap::new(),
                        removed_tests: &[],
                    },
                    None,
                )
                .await?;
            (reviewed, checksum) = (edited, edited_checksum);
        }
        assert_ne!(
            reviewed, submitted,
            "the toggles must respell the selection for this case to prove anything"
        );
        assert!(
            files
                .read_submitted_document("session", &plan_id, 1)?
                .plans_same_work(&reviewed)?,
            "an undone check toggle changed the planned work"
        );
        Ok(())
    }

    /// Reject edits to reviewed drafts instead of accepting an obsolete snapshot.
    #[tokio::test]
    async fn review_updates_authored_tests_without_overwriting_newer_edits() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a plan.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        fs::write(
            directory.join("docs/overview.md"),
            "# Plan\n\n## Objective\nKeep state.\n\n## Background\nNo changes yet.\n\n## Design\nRetain the public contract.\n",
        )?;
        fs::write(
            directory.join("docs/tests.md"),
            "## tests.rs\n- new `checks_state`: State stays independent.\n",
        )?;
        let original = files.read_working_document("session", &plan_id)?;
        let mut document = files.authoring_import("session", &original)?;
        let validated = document
            .design
            .as_ref()
            .unwrap()
            .validated(temporary.path(), &document.title)
            .await?;
        document.design = Some(validated.into_design());
        let (_, _, checksum) = files.submit_validated_document_revision_if_unchanged(
            "session",
            &plan_id,
            1,
            original.version,
            &original,
            document.clone(),
        )?;
        assert_eq!(
            files.authoring_progress("session", &document)?,
            (true, false)
        );
        assert_eq!(
            files.authoring_progress("session", &document)?,
            (false, false)
        );
        let (reviewed, _, _) = files
            .remove_tests(
                "session",
                &plan_id,
                document.version,
                &checksum,
                &[super::super::DesignTestSelection {
                    file: "tests.rs".into(),
                    name: "checks_state".into(),
                }],
                None,
            )
            .await?;
        assert!(fs::read_to_string(directory.join("docs/tests.md"))?.is_empty());
        files.authoring_require_current("session", &reviewed)?;
        assert_eq!(
            files.authoring_progress("session", &reviewed)?,
            (true, false)
        );
        let mut edited = reviewed.clone();
        edited.version += 1;
        edited.design.as_mut().unwrap().artifact_text.insert(
            "plan:tests.md".into(),
            "## tests.rs\n- new `checks_state`: State stays independent.\n".into(),
        );
        files.authoring_review_prepare("session", &mut edited)?;
        let images = files.authoring_review_images("session", &edited)?;
        assert_eq!(images.len(), 1);
        assert_eq!(images[0].path, "docs/tests.md");
        fs::write(directory.join("docs/tests.md"), "newer ordinary edit\n")?;
        assert_eq!(
            files.authoring_progress("session", &reviewed)?,
            (true, true)
        );
        assert_eq!(
            files.authoring_progress("session", &reviewed)?,
            (false, true)
        );
        assert!(files.authoring_review_images("session", &edited).is_err());
        assert!(
            files
                .authoring_require_current("session", &reviewed)
                .is_err()
        );
        assert_eq!(
            files.read_submitted_document("session", &plan_id, 1)?,
            document
        );
        Ok(())
    }

    /// Exercise ordinary edits, deletion, restart, and capture without a virtual patch tool.
    #[test]
    fn capture_preserves_edits_and_deletions_across_restart() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let workspace = temporary.path().join("repository");
        fs::create_dir_all(workspace.join("src"))?;
        crate::checks::configure_fixture(&workspace);
        fs::write(
            workspace.join("src/original.rs"),
            "/// Original callable.\npub fn original() {}\n",
        )?;
        let files = PlanFileStore::new(temporary.path().join("state-owner"), &workspace)?;
        let prepared = files.prepare_entry("session", "Revise the original.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        files.authoring_capture("session", &plan_id, "src/original.rs")?;
        let directory = files.authoring_directory("session", &plan_id);
        let path = directory.join("files/src/original.rs");
        fs::write(
            &path,
            "/// Original callable.\npub fn original();\nChange\n  Retain the source identity.\n",
        )?;
        files.authoring_capture("session", &plan_id, "src/original.rs")?;
        assert!(fs::read_to_string(&path)?.contains("Change"));
        let original = files.read_working_document("session", &plan_id)?;
        let proposal = files.authoring_import("session", &original)?;
        assert_eq!(
            proposal.design.as_ref().unwrap().changed_paths(),
            ["src/original.rs"]
        );
        fs::remove_file(&path)?;
        drop(files);
        let files = PlanFileStore::new(temporary.path().join("state-owner"), &workspace)?;
        files.authoring_capture("session", &plan_id, "src/original.rs")?;
        assert!(!path.exists());
        let proposal = files.authoring_import("session", &original)?;
        assert!(proposal.design.unwrap().proposed_source.is_empty());
        assert!(!directory.join("capture-ledger.json").exists());
        assert!(!directory.join("working.json").exists());
        Ok(())
    }

    /// Bind snapshots to exact input bytes while leaving a failed submission's draft untouched.
    #[tokio::test]
    async fn snapshots_stay_outside_draft_and_reject_later_edits() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let workspace = temporary.path().join("repository");
        fs::create_dir_all(&workspace)?;
        crate::checks::configure_fixture(&workspace);
        let files = PlanFileStore::new(temporary.path().join("state-owner"), &workspace)?;
        let prepared = files.prepare_entry("session", "Create a callable.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        fs::write(
            directory.join("docs/overview.md"),
            "# Callable\n\n## Objective\nCreate a callable.\n\n## Background\nNo existing callable.\n\n## Design\nAdd the declaration.\n",
        )?;
        fs::write(
            directory.join("files/new.rs"),
            "/// Create a callable.\npub fn created();\n",
        )?;
        let original = files.read_working_document("session", &plan_id)?;
        let mut proposal = files.authoring_import("session", &original)?;
        let validated = proposal
            .design
            .as_ref()
            .unwrap()
            .validated(&workspace, &proposal.title)
            .await?;
        proposal.design = Some(validated.into_design());
        let snapshot = files.authoring_snapshot_save("session", &proposal)?;
        assert_eq!(
            files.authoring_snapshot_read("session", &plan_id, &snapshot)?,
            proposal
        );
        assert_eq!(fs::read_dir(&directory)?.count(), 2);
        fs::write(directory.join("files/new.rs"), "pub fn changed();\n")?;
        assert!(
            files
                .authoring_require_current("session", &proposal)
                .is_err()
        );
        assert!(
            files
                .authoring_snapshot_read("session", &plan_id, &snapshot)
                .is_err()
        );
        assert_eq!(
            fs::read_to_string(directory.join("files/new.rs"))?,
            "pub fn changed();\n"
        );
        assert_eq!(files.read_working_document("session", &plan_id)?, original);
        Ok(())
    }

    /// Reject traversal and capture collisions without overwriting authored content.
    #[test]
    fn capture_rejects_escape_and_uncaptured_proposal() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        fs::write(temporary.path().join("source.rs"), "pub fn source() {}\n")?;
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Capture.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        fs::write(directory.join("files/source.rs"), "pub fn proposed();\n")?;
        for path in ["source.rs", "../source.rs", "/source.rs", "C:\\source.rs"] {
            assert!(
                files.authoring_capture("session", &plan_id, path).is_err(),
                "{path}"
            );
        }
        assert_eq!(
            fs::read_to_string(directory.join("files/source.rs"))?,
            "pub fn proposed();\n"
        );
        #[cfg(unix)]
        {
            fs::create_dir(directory.join("files/nested"))?;
            fs::write(directory.join("files/nested/alias.rs"), "pub fn alias();\n")?;
            fs::write(
                directory.join("files/nested\\alias.rs"),
                "pub fn distinct();\n",
            )?;
            let original = files.read_working_document("session", &plan_id)?;
            assert!(files.authoring_import("session", &original).is_err());
        }
        Ok(())
    }

    /// Import authored docs written with CRLF as LF text, while the fingerprint still binds the
    /// raw directory bytes.
    #[test]
    fn import_stores_authored_docs_with_lf_line_endings() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a flow.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        fs::write(
            directory.join("docs/flows.md"),
            "## Flow\r\nRuns once.\r\n\r\n```mermaid\r\nflowchart TD\r\n  a --> b\r\n```\r\n",
        )?;
        let original = files.read_working_document("session", &plan_id)?;
        let document = files.authoring_import("session", &original)?;
        let design = document.design.as_ref().unwrap();
        assert_eq!(
            design.artifact_text[PlanArtifact::Flows.path()],
            "## Flow\nRuns once.\n\n```mermaid\nflowchart TD\n  a --> b\n```\n"
        );
        files.authoring_require_current("session", &document)?;
        Ok(())
    }

    /// A provider that writes CRLF plan files on Windows submits the same plan that Harness's
    /// LF rewrite of the directory then holds, so settlement must not find the draft changed.
    #[tokio::test]
    async fn line_endings_alone_leave_a_submitted_draft_current() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a plan.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        let overview = "# Plan\n\n## Objective\nKeep state.\n\n## Background\nNo changes yet.\n\n## Design\nRetain the public contract.\n";
        let tests = "## tests.rs\n- new `checks_state`: State stays independent.\n";
        fs::write(
            directory.join("docs/overview.md"),
            overview.replace('\n', "\r\n"),
        )?;
        fs::write(directory.join("docs/tests.md"), tests.replace('\n', "\r\n"))?;
        let original = files.read_working_document("session", &plan_id)?;
        let mut document = files.authoring_import("session", &original)?;
        let validated = document
            .design
            .as_ref()
            .unwrap()
            .validated(temporary.path(), &document.title)
            .await?;
        document.design = Some(validated.into_design());
        files.submit_validated_document_revision_if_unchanged(
            "session",
            &plan_id,
            1,
            original.version,
            &original,
            document.clone(),
        )?;
        fs::write(directory.join("docs/overview.md"), overview)?;
        fs::write(directory.join("docs/tests.md"), tests)?;
        let (_, dirty) = files.authoring_progress("session", &document)?;
        assert!(
            !dirty,
            "rewriting the same plan with LF line endings marked the draft changed"
        );
        files.authoring_require_current("session", &document)?;
        Ok(())
    }
}
