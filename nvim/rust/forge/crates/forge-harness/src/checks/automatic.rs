use super::{CheckRun, CheckState};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Default, Deserialize, Serialize)]
/// Persists automatic validation and delivery separately from a provider exchange.
pub(crate) struct AutomaticChecks {
    #[serde(skip)]
    path: PathBuf,
    /// Task or exchange whose original change baseline remains authoritative.
    pub owner: String,
    /// Last resolved automatic run, including reused evidence.
    pub run_id: Option<String>,
    /// Failures already delivered to an automatic repair turn.
    pub delivered: Vec<String>,
    /// Whether validation still prevents successful completion.
    pub pending: bool,
    /// Explains a repair that stopped without relevant progress.
    pub blocked: Option<String>,
    /// Undelivered repair text retained until an exchange has admitted it.
    pub repair_prompt: Option<String>,
    /// Exchange proving that the saved repair request was delivered.
    pub repair_exchange: Option<String>,
}

impl AutomaticChecks {
    /// Restores the owner's pending validation without replaying completed delivery.
    pub(crate) fn open(root: &Path, owner: &str) -> Result<Self> {
        let path = root.join(format!(
            "automatic-{}.json",
            crate::plan::digest(owner.as_bytes())
        ));
        let mut state = match std::fs::read(&path) {
            Ok(source) => serde_json::from_slice(&source)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Self {
                owner: owner.into(),
                ..Default::default()
            },
            Err(error) => return Err(error.into()),
        };
        state.path = path;
        Ok(state)
    }

    /// Publishes pending validation before work or repair admission.
    pub(crate) fn save(&self) -> Result<()> {
        let temporary = self.path.with_extension("tmp");
        std::fs::write(&temporary, serde_json::to_vec(self)?)?;
        std::fs::rename(temporary, &self.path)?;
        Ok(())
    }

    /// Returns one repair instruction, or blocks repeated unchanged failure delivery.
    pub(crate) fn observe(&mut self, run: &CheckRun) -> Result<Option<String>> {
        self.run_id = Some(run.id.clone());
        let failures = run
            .commands
            .iter()
            .filter(|command| {
                matches!(
                    command.state,
                    CheckState::Failed | CheckState::Blocked | CheckState::ConfigurationError
                )
            })
            .collect::<Vec<_>>();
        if failures.is_empty() {
            self.pending = false;
            self.blocked = None;
            self.delivered.clear();
            self.repair_prompt = None;
            self.repair_exchange = None;
            self.save()?;
            return Ok(None);
        }
        self.pending = true;
        let identities = failures
            .iter()
            .map(|command| -> Result<String> {
                Ok(crate::plan::digest(&serde_json::to_vec(&(
                    &command.command,
                    command
                        .evidence
                        .as_ref()
                        .map(|evidence| (&evidence.signature, &evidence.source_identity)),
                    &command.error,
                ))?))
            })
            .collect::<Result<Vec<_>>>()?;
        if identities.len() == self.delivered.len()
            && identities
                .iter()
                .all(|identity| self.delivered.contains(identity))
        {
            if self.repair_exchange.is_none()
                && let Some(prompt) = &self.repair_prompt
            {
                return Ok(Some(prompt.clone()));
            }
            self.blocked = Some("Automatic repair stopped: the same checks still fail without relevant input changes.".into());
            self.save()?;
            return Ok(None);
        }
        let findings = failures
            .iter()
            .map(|command| {
                format!(
                    "{}: {:?}, exit {:?}. {}\nOutput: {}",
                    command.command,
                    command.state,
                    command.exit_code,
                    command.error.as_deref().unwrap_or(""),
                    command.output.display()
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n");
        if failures
            .iter()
            .any(|command| command.state == CheckState::ConfigurationError)
        {
            self.blocked = Some(format!(
                "Automatic validation needs corrected check configuration. Use the authorized checks setup workflow.\n{findings}"
            ));
            self.save()?;
            return Ok(None);
        }
        self.delivered = identities;
        self.blocked = None;
        let prompt = format!(
            "Automatic checks prevent task completion. Fix the failures below and verify through harness_checks_run and harness_checks_status. Do not run validation commands through terminal tools. Preserve the original task scope. A turn ending with the same failed inputs will stop automatic repair.\n\n{findings}"
        );
        self.repair_prompt = Some(prompt.clone());
        self.repair_exchange = None;
        self.save()?;
        Ok(Some(prompt))
    }
}
