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
    /// Undelivered repair failures retained until an exchange has admitted them.
    pub repair_prompt: Option<forge_provider_api::forge::CheckRepair>,
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

    /// Returns one repair's failures, or blocks repeated unchanged failure delivery.
    pub(crate) fn observe(
        &mut self,
        run: &CheckRun,
    ) -> Result<Option<forge_provider_api::forge::CheckRepair>> {
        self.run_id = Some(run.id.clone());
        if matches!(
            run.state,
            CheckState::Stopped | CheckState::Stale | CheckState::ExecutionError
        ) {
            self.pending = true;
            self.blocked = None;
            self.repair_prompt = None;
            self.repair_exchange = None;
            self.save()?;
            return Ok(None);
        }
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
        if run.cancelled_run_id.is_some() {
            self.blocked = Some("Automatic checks remain stopped after cancellation. Run checks explicitly to retry unchanged inputs.".into());
            self.repair_prompt = None;
            self.repair_exchange = None;
            self.save()?;
            return Ok(None);
        }
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
            .map(|command| blocked_finding(command))
            .collect::<Vec<_>>()
            .join("\n\n");
        if failures
            .iter()
            .any(|command| command.state == CheckState::ConfigurationError)
        {
            self.blocked = Some(format!(
                "Automatic validation needs a corrected check configuration. Fix it through the authorized checks setup workflow.\n{findings}"
            ));
            self.save()?;
            return Ok(None);
        }
        self.delivered = identities;
        self.blocked = None;
        let prompt = forge_provider_api::forge::CheckRepair::new(
            failures
                .iter()
                .map(|command| forge_provider_api::forge::CheckFailure {
                    check: command.command.clone(),
                    state: format!("{:?}", command.state),
                    exit_code: command.exit_code,
                    error: command.error.clone().filter(|error| !error.is_empty()),
                    log: (command.output_bytes > 0).then(|| command.output.display().to_string()),
                })
                .collect(),
        )
        .expect("repair has at least one failure");
        self.repair_prompt = Some(prompt.clone());
        self.repair_exchange = None;
        self.save()?;
        Ok(Some(prompt))
    }
}

/// Describe one failed command in the message that blocks automatic validation.
///
/// The exit reads as in check findings, `exit status 1` or `no exit status`, rather than as a
/// debug-formatted optional code.
fn blocked_finding(command: &super::CommandCheck) -> String {
    format!(
        "{}: {:?}, {}. {}\nOutput: {}",
        command.command,
        command.state,
        super::model::exit_status(command.exit_code),
        command.error.as_deref().unwrap_or(""),
        command.output.display()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn configuration_failure(exit_code: Option<i32>) -> super::super::CommandCheck {
        serde_json::from_value(json!({
            "selected":true,"id":"compiler","command":"cargo check",
            "state":CheckState::ConfigurationError,
            "started_at_ms":1,"completed_at_ms":2,"exit_code":exit_code,"error":"adapter missing",
            "stdout":"stdout","stderr":"stderr","output":"compiler.log","output_bytes":0,"preview":""
        }))
        .expect("command check decodes")
    }

    /// A blocked configuration names the exit status as check findings do.
    #[test]
    fn blocked_finding_states_the_exit_status_as_findings_do() {
        assert_eq!(
            blocked_finding(&configuration_failure(Some(2))),
            "cargo check: ConfigurationError, exit status 2. adapter missing\nOutput: compiler.log"
        );
        assert!(
            blocked_finding(&configuration_failure(None))
                .starts_with("cargo check: ConfigurationError, no exit status.")
        );
    }
}
