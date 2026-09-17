use super::*;
use forge_github::review_mutation::ReviewerSelection;

impl ReviewDocument {
    /// Refreshes requested reviewers without replacing local edits or an unresolved submission.
    pub(crate) fn refresh_reviewers(&mut self, detail: &[(String, String)]) -> Result<()> {
        let text = reviewer_text(
            detail,
            self.target
                .repository
                .repository_name()
                .split('/')
                .next()
                .expect("validated repository"),
        );
        let region = RegionId("reviewers".into());
        if !self.reviewers_loaded {
            self.edits.insert(region, RegionRevision(0), text)?;
            self.reviewers_loaded = true;
        } else {
            let field = self.edits.snapshot(&region)?;
            if !field.dirty
                && !field.uncertain
                && field.pending_saves == 0
                && field.remote.is_none()
            {
                self.edits.refresh(&region, field.revision, text)?;
            }
        }
        Ok(())
    }

    /// Retains submitted spelling when the remote reviewer set matches despite ordering or casing.
    pub(crate) fn observed_reviewers(&self, detail: &[(String, String)]) -> Result<String> {
        let repository = self.target.repository.repository_name();
        let owner = repository.split('/').next().expect("validated repository");
        let observed = reviewer_text(detail, owner);
        if let Some(submitted) = self
            .pending
            .iter()
            .find(|capture| capture.region().0 == "reviewers")
            && change(submitted.text(), &observed, owner)?.is_none()
        {
            return Ok(submitted.text().to_owned());
        }
        Ok(observed)
    }

    /// Returns the exact captured text used to settle each field independently of newer edits.
    pub(crate) fn submitted_fields(&self) -> BTreeMap<String, String> {
        self.pending
            .iter()
            .map(|capture| (capture.region().0.clone(), capture.text().to_owned()))
            .collect()
    }
}

/// Preserves the distinction between individual users and organization teams in editable text.
fn reviewer_text(detail: &[(String, String)], owner: &str) -> String {
    detail
        .iter()
        .map(|(kind, name)| {
            if kind == "teams" {
                format!("@{owner}/{name}")
            } else {
                format!("@{name}")
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Computes additions and removals while preserving unchanged names regardless of casing.
pub(super) fn change(baseline: &str, text: &str, owner: &str) -> Result<Option<ReviewMutation>> {
    let before = selection(baseline, owner)?;
    let after = selection(text, owner)?;
    let difference = |desired: &[String], current: &[String]| {
        desired
            .iter()
            .filter(|name| !current.iter().any(|value| value.eq_ignore_ascii_case(name)))
            .cloned()
            .collect()
    };
    let mutation = ReviewMutation::ReviewerChange {
        add: ReviewerSelection {
            reviewer: difference(&after.reviewer, &before.reviewer),
            team: difference(&after.team, &before.team),
        },
        remove: ReviewerSelection {
            reviewer: difference(&before.reviewer, &after.reviewer),
            team: difference(&before.team, &after.team),
        },
    };
    if let ReviewMutation::ReviewerChange { add, remove } = &mutation
        && add.reviewer.is_empty()
        && add.team.is_empty()
        && remove.reviewer.is_empty()
        && remove.team.is_empty()
    {
        return Ok(None);
    }
    Ok(Some(mutation))
}

/// Parses one line of GitHub mentions into bounded user and team selections.
fn selection(text: &str, owner: &str) -> Result<ReviewerSelection> {
    ensure!(
        !text.contains(['\n', '\r']),
        "requested reviewers must occupy one line"
    );
    let mut selection = ReviewerSelection {
        reviewer: Vec::new(),
        team: Vec::new(),
    };
    for token in text
        .split(|value: char| value.is_whitespace() || value == ',')
        .filter(|value| !value.is_empty())
    {
        let token = token.strip_prefix('@').unwrap_or(token);
        let (names, name) = if let Some((organization, team)) = token.split_once('/') {
            ensure!(
                organization.eq_ignore_ascii_case(owner),
                "reviewer team belongs to another organization"
            );
            (&mut selection.team, team)
        } else {
            (&mut selection.reviewer, token)
        };
        ensure!(
            !name.is_empty()
                && name.len() <= 100
                && name
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-'),
            "invalid reviewer name"
        );
        if !names
            .iter()
            .any(|value: &String| value.eq_ignore_ascii_case(name))
        {
            names.push(name.to_owned());
        }
    }
    ensure!(
        selection.reviewer.len() + selection.team.len() <= 100,
        "too many requested reviewers"
    );
    Ok(selection)
}
