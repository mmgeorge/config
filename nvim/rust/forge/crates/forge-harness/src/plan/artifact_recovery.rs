//! Recover working-plan file changes across the separate artifact and SQLite commits.

use super::*;

/// Retain both complete file images until SQLite proves which view owns the plan.
#[derive(Serialize, Deserialize)]
struct ArtifactJournal {
    prior: PlanRecord,
    target: Option<PlanRecord>,
    files: Vec<ArtifactImage>,
}

#[derive(Serialize, Deserialize)]
struct ArtifactImage {
    path: ArtifactPath,
    prior: Option<Vec<u8>>,
    target: Vec<u8>,
}

/// Restrict replay to recognized working and submitted artifact paths.
#[derive(Serialize, Deserialize)]
enum ArtifactPath {
    Working { extension: String },
    Submitted { revision: u32, extension: String },
}

impl ArtifactPath {
    /// Resolve only known artifact names under the selected plan directory.
    fn resolve(&self, directory: &Path) -> Result<PathBuf> {
        let extension = match self {
            Self::Working { extension } | Self::Submitted { extension, .. } => extension,
        };
        anyhow::ensure!(
            ["json", "md", "index.json"].contains(&extension.as_str()),
            "invalid artifact journal file"
        );
        Ok(match self {
            Self::Working { extension } => directory.join(format!("working.{extension}")),
            Self::Submitted {
                revision,
                extension,
            } => directory
                .join("revisions")
                .join(format!("submitted-{revision:04}.{extension}")),
        })
    }
}

impl PlanFileStore {
    /// Journal a live working edit until the matching plan metadata commits.
    pub(crate) fn write_working_mutation_guarded(
        &self,
        prior: &PlanRecord,
        expected: &PlanDocument,
        target: &PlanDocument,
    ) -> Result<()> {
        anyhow::ensure!(
            expected.plan_id == prior.id && target.plan_id == prior.id,
            "working document plan id mismatch"
        );
        anyhow::ensure!(
            expected.version == prior.document_version,
            "working document metadata changed before mutation"
        );
        self.review_cache.validate(target, false)?;
        let gate = self.working_document_gate(&prior.session_id, &prior.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.reconcile_artifact_locked(prior)?;
        anyhow::ensure!(
            self.read_working_document(&prior.session_id, &prior.id)? == *expected,
            WorkingDocumentChanged
        );
        // Working drafts may be incomplete; preserve the original JSON-only edit contract.
        let directory = self.plan_dir(&prior.session_id, &prior.id);
        let json_path = directory.join("working.json");
        let journal = ArtifactJournal {
            prior: prior.clone(),
            target: None,
            files: vec![ArtifactImage {
                path: ArtifactPath::Working {
                    extension: "json".into(),
                },
                prior: Some(fs::read(json_path)?),
                target: crate::plan::codec::to_vec_pretty(target)?,
            }],
        };
        let journal_path = directory.join("artifact-recovery.json");
        write_json_atomically(&journal_path, &journal)?;
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(journal_path)?
            .sync_all()?;
        self.apply_artifact_images(&directory, &journal, true)
    }

    /// Restore a selected accepted revision while retaining rollback evidence until the domain commit.
    pub(crate) fn restore_revision_guarded(
        &self,
        prior: &PlanRecord,
        target: &PlanRecord,
        revision: u32,
        reviewed: &review_source::PlanReviewSource,
    ) -> Result<PlanDocument> {
        anyhow::ensure!(
            prior.id == target.id && prior.session_id == target.session_id,
            "artifact restoration cannot change plan identity"
        );
        anyhow::ensure!(
            artifact_proof(prior) != artifact_proof(target),
            "artifact restoration requires distinct database commit evidence"
        );
        let gate = self.working_document_gate(&prior.session_id, &prior.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.reconcile_artifact_locked(prior)?;
        let checksum = digest(&crate::plan::codec::to_vec(&reviewed.document)?);
        let captured = self.capture_review_source(&prior.session_id, &prior.id, None, &checksum)?;
        anyhow::ensure!(
            captured.document == reviewed.document
                && captured.saved_digest == reviewed.saved_digest,
            "working plan changed before artifact restoration"
        );
        let current = self.read_working_document(&prior.session_id, &prior.id)?;
        anyhow::ensure!(
            current.version == prior.document_version,
            "working plan changed before artifact restoration"
        );
        let document = self.read_submitted_document(&prior.session_id, &prior.id, revision)?;
        anyhow::ensure!(
            document.version == target.document_version,
            "accepted revision changed before artifact restoration"
        );
        let directory = self.plan_dir(&prior.session_id, &prior.id);
        let mut files = Vec::new();
        for extension in ["json", "md", "index.json"] {
            files.push(ArtifactImage {
                path: ArtifactPath::Working {
                    extension: extension.into(),
                },
                prior: Some(fs::read(directory.join(format!("working.{extension}")))?),
                target: fs::read(
                    directory
                        .join("revisions")
                        .join(format!("submitted-{revision:04}.{extension}")),
                )?,
            });
        }
        let journal = ArtifactJournal {
            prior: prior.clone(),
            target: Some(target.clone()),
            files,
        };
        let path = directory.join("artifact-recovery.json");
        write_json_atomically(&path, &journal)?;
        // Windows durability flushes require write access; opening does not create or truncate the file.
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)?
            .sync_all()?;
        self.apply_artifact_images(&directory, &journal, true)?;
        Ok(document)
    }

    /// Journal submitted and working artifact images before admitting any filesystem replacement.
    pub(crate) fn submit_reviewed_revision_guarded(
        &self,
        prior: &PlanRecord,
        revision: u32,
        reviewed: &review_source::PlanReviewSource,
    ) -> Result<()> {
        let gate = self.working_document_gate(&prior.session_id, &prior.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.reconcile_artifact_locked(prior)?;
        let checksum = digest(&crate::plan::codec::to_vec(&reviewed.document)?);
        let current = self.capture_review_source(&prior.session_id, &prior.id, None, &checksum)?;
        anyhow::ensure!(
            current.document == reviewed.document && current.saved_digest == reviewed.saved_digest,
            "working plan changed before the review decision"
        );
        self.journal_revision_locked(
            prior,
            Some(revision),
            &reviewed.document,
            &reviewed.rendered,
        )
    }

    /// Retain working and immutable revision images under the held plan mutation gate.
    pub(super) fn journal_revision_locked(
        &self,
        prior: &PlanRecord,
        revision: Option<u32>,
        document: &PlanDocument,
        rendered: &RenderedPlan,
    ) -> Result<()> {
        let directory = self.plan_dir(&prior.session_id, &prior.id);
        let contents = [
            ("json", crate::plan::codec::to_vec_pretty(document)?),
            ("md", rendered.markdown.as_bytes().to_vec()),
            (
                "index.json",
                crate::plan::codec::to_vec_pretty(&rendered.navigation)?,
            ),
        ];
        let mut files = Vec::new();
        for (extension, target) in contents {
            let mut paths = vec![ArtifactPath::Working {
                extension: extension.into(),
            }];
            if let Some(revision) = revision {
                paths.push(ArtifactPath::Submitted {
                    revision,
                    extension: extension.into(),
                });
            }
            for path in paths {
                let resolved = path.resolve(&directory)?;
                let prior = match fs::read(&resolved) {
                    Ok(bytes) => Some(bytes),
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                    Err(error) => return Err(error.into()),
                };
                if matches!(&path, ArtifactPath::Submitted { .. }) {
                    anyhow::ensure!(
                        prior.as_ref().is_none_or(|bytes| bytes == &target),
                        "submitted plan revision already has different contents"
                    );
                }
                files.push(ArtifactImage {
                    path,
                    prior,
                    target: target.clone(),
                });
            }
        }
        if files
            .iter()
            .all(|file| file.prior.as_ref() == Some(&file.target))
        {
            return Ok(());
        }
        let journal = ArtifactJournal {
            prior: prior.clone(),
            target: None,
            files,
        };
        let journal_path = directory.join("artifact-recovery.json");
        write_json_atomically(&journal_path, &journal)?;
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(journal_path)?
            .sync_all()?;
        self.apply_artifact_images(&directory, &journal, true)
    }

    /// Claim only a file-revision change for sealing; unrelated task metadata leaves live journals alone.
    pub(crate) fn claim_plan_artifacts(&self, target: &PlanRecord) -> Result<bool> {
        let gate = self.working_document_gate(&target.session_id, &target.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.claim_plan_artifacts_locked(target)
    }

    /// Claim a journal while the caller already owns the working-document lock.
    pub(super) fn claim_plan_artifacts_locked(&self, target: &PlanRecord) -> Result<bool> {
        let path = self
            .plan_dir(&target.session_id, &target.id)
            .join("artifact-recovery.json");
        let journal: ArtifactJournal = match fs::read(path) {
            Ok(bytes) => serde_json::from_slice(&bytes)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            Err(error) => return Err(error.into()),
        };
        if journal
            .target
            .as_ref()
            .is_some_and(|recorded| artifact_proof(recorded) == artifact_proof(target))
        {
            self.seal_plan_artifacts_locked(target)?;
            return Ok(true);
        }
        // Receipt acknowledgments, task status, and question metadata do not authorize file recovery.
        if target.document_version == journal.prior.document_version
            && target.model_revision == journal.prior.model_revision
            && target.submitted_version == journal.prior.submitted_version
        {
            return Ok(false);
        }
        self.seal_plan_artifacts_locked(target)?;
        Ok(true)
    }

    /// Seal the exact target database record after preparation and before the owner can commit it.
    pub(crate) fn seal_plan_artifacts(&self, target: &PlanRecord) -> Result<()> {
        let gate = self.working_document_gate(&target.session_id, &target.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.seal_plan_artifacts_locked(target)
    }

    /// Seal artifacts while the caller retains the exact working-document lock.
    pub(super) fn seal_plan_artifacts_locked(&self, target: &PlanRecord) -> Result<()> {
        let directory = self.plan_dir(&target.session_id, &target.id);
        let path = directory.join("artifact-recovery.json");
        if !path.exists() {
            return Ok(());
        }
        let mut journal: ArtifactJournal = serde_json::from_slice(&fs::read(&path)?)?;
        anyhow::ensure!(
            journal.prior.id == target.id && journal.prior.session_id == target.session_id,
            "artifact target identity changed"
        );
        anyhow::ensure!(
            artifact_proof(&journal.prior) != artifact_proof(target),
            "artifact preparation requires distinct database commit evidence"
        );
        if let Some(recorded) = &journal.target {
            anyhow::ensure!(
                artifact_proof(recorded) == artifact_proof(target),
                "artifact target changed after preparation"
            );
        } else {
            let working = journal
                .files
                .iter()
                .find_map(|file| match &file.path {
                    ArtifactPath::Working { extension } if extension == "json" => {
                        Some(&file.target)
                    }
                    ArtifactPath::Working { .. } | ArtifactPath::Submitted { .. } => None,
                })
                .context("artifact journal has no target document")?;
            let document: PlanDocument = crate::plan::codec::from_slice(working)?;
            anyhow::ensure!(
                document.plan_id == target.id && document.version == target.document_version,
                "database target does not authorize the prepared document"
            );
        }
        journal.target = Some(target.clone());
        write_json_atomically(&path, &journal)?;
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)?
            .sync_all()?;
        Ok(())
    }

    /// Reconcile an interrupted file operation using the atomically committed plan record as proof.
    pub(crate) fn reconcile_plan_artifacts(&self, committed: &PlanRecord) -> Result<()> {
        let gate = self.working_document_gate(&committed.session_id, &committed.id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.reconcile_artifact_locked(committed)
    }

    /// Block ordinary file mutations until retained database/file evidence has been reconciled.
    pub(super) fn require_reconciled_artifacts(
        &self,
        session_id: &str,
        plan_id: &str,
    ) -> Result<()> {
        anyhow::ensure!(
            !self
                .plan_dir(session_id, plan_id)
                .join("artifact-recovery.json")
                .exists(),
            "plan artifact recovery must complete before another mutation"
        );
        Ok(())
    }

    /// Resolve a retained journal without overwriting a file edited outside this operation.
    pub(super) fn reconcile_artifact_locked(&self, committed: &PlanRecord) -> Result<()> {
        let directory = self.plan_dir(&committed.session_id, &committed.id);
        let path = directory.join("artifact-recovery.json");
        if !path.exists() {
            return Ok(());
        }
        let journal: ArtifactJournal = serde_json::from_slice(&fs::read(&path)?)?;
        let committed = artifact_proof(committed);
        let target = if journal
            .target
            .as_ref()
            .is_some_and(|target| artifact_proof(target) == committed)
        {
            true
        } else if committed == artifact_proof(&journal.prior) {
            false
        } else {
            anyhow::bail!("plan artifact recovery requires its recorded database revision")
        };
        self.apply_artifact_images(&directory, &journal, target)?;
        self.review_cache.invalidate()?;
        fs::remove_file(path)?;
        Ok(())
    }

    /// Validate every file before changing any, and permit only the journal's original or replacement bytes.
    fn apply_artifact_images(
        &self,
        directory: &Path,
        journal: &ArtifactJournal,
        target: bool,
    ) -> Result<()> {
        for file in &journal.files {
            let path = file.path.resolve(directory)?;
            let current = match fs::read(&path) {
                Ok(bytes) => Some(bytes),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                Err(error) => return Err(error.into()),
            };
            anyhow::ensure!(
                current == file.prior || current.as_deref() == Some(file.target.as_slice()),
                "working plan has intervening edits; artifact recovery retained"
            );
        }
        for file in &journal.files {
            let path = file.path.resolve(directory)?;
            let expected = if target {
                Some(&file.target)
            } else {
                file.prior.as_ref()
            };
            match expected {
                Some(bytes) => {
                    if let Some(parent) = path.parent() {
                        fs::create_dir_all(parent)?;
                    }
                    write_bytes_atomically(&path, bytes)?;
                    fs::OpenOptions::new()
                        .read(true)
                        .write(true)
                        .open(path)?
                        .sync_all()?;
                }
                None if path.exists() => fs::remove_file(path)?,
                None => {}
            }
        }
        Ok(())
    }
}

/// Bind file authority while allowing independent answer and question-ledger transactions to advance.
fn artifact_proof(plan: &PlanRecord) -> serde_json::Value {
    serde_json::json!({
        "id": plan.id, "session_id": plan.session_id, "state": plan.state,
        "title": plan.title, "document_version": plan.document_version,
        "model_revision": plan.model_revision, "user_revision": plan.user_revision,
        "submitted_version": plan.submitted_version, "review_digest": plan.review_digest,
        "accepted_revision": plan.accepted_revision, "accepted_digest": plan.accepted_digest,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Recovery selects only an exact database endpoint and preserves intervening user edits.
    #[test]
    fn artifact_recovery_uses_commit_proof_and_refuses_unowned_bytes() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = PlanFileStore::new(root.path(), root.path()).unwrap();
        let prior: PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"plan", "session_id":"session", "request":"Review", "state":"awaiting_review",
            "working_path":"working.json", "document_version":2, "model_revision":2,
            "user_revision":0, "review_digest":"pending", "created_at_ms":1, "updated_at_ms":1
        }))?;
        let mut target = prior.clone();
        target.state = PlanState::Accepted;
        target.document_version = 1;
        target.review_digest = Some("accepted".into());
        let directory = store.plan_dir("session", "plan");
        fs::create_dir_all(&directory)?;
        let journal = ArtifactJournal {
            prior: prior.clone(),
            target: Some(target.clone()),
            files: vec![ArtifactImage {
                path: ArtifactPath::Working {
                    extension: "md".into(),
                },
                prior: Some(b"proposal".to_vec()),
                target: b"accepted".to_vec(),
            }],
        };
        let journal_path = directory.join("artifact-recovery.json");
        write_json_atomically(&journal_path, &journal)?;
        fs::write(directory.join("working.md"), b"accepted")?;
        store.reconcile_plan_artifacts(&prior)?;
        assert_eq!(fs::read(directory.join("working.md"))?, b"proposal");
        assert!(!journal_path.exists());

        write_json_atomically(&journal_path, &journal)?;
        fs::write(directory.join("working.md"), b"proposal")?;
        let mut independently_updated = target.clone();
        independently_updated.updated_at_ms = 9;
        store.reconcile_plan_artifacts(&independently_updated)?;
        assert_eq!(fs::read(directory.join("working.md"))?, b"accepted");

        write_json_atomically(&journal_path, &journal)?;
        fs::write(directory.join("working.md"), b"new user edit")?;
        assert!(store.reconcile_plan_artifacts(&target).is_err());
        assert!(journal_path.exists());
        assert_eq!(fs::read(directory.join("working.md"))?, b"new user edit");
        fs::remove_file(&journal_path)?;
        let revision_path = directory.join("revisions/submitted-0003.json");
        fs::create_dir_all(revision_path.parent().unwrap())?;
        fs::write(&revision_path, b"new revision")?;
        let pending = ArtifactJournal {
            prior: prior.clone(),
            target: None,
            files: vec![ArtifactImage {
                path: ArtifactPath::Submitted {
                    revision: 3,
                    extension: "json".into(),
                },
                prior: None,
                target: b"new revision".to_vec(),
            }],
        };
        write_json_atomically(&journal_path, &pending)?;
        store.reconcile_plan_artifacts(&prior)?;
        assert!(
            !revision_path.exists(),
            "uncommitted immutable artifacts must not survive recovery"
        );
        write_json_atomically(&journal_path, &journal)?;
        let mut unrelated = target;
        unrelated.document_version = 3;
        assert!(store.reconcile_plan_artifacts(&unrelated).is_err());
        assert!(journal_path.exists());
        Ok(())
    }
}
