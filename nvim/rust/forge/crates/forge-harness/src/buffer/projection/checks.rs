use super::*;
use crate::checks::adapter::{
    TestItem, TestItemId,
    events::{CaseResult, CaseStatus},
};
use crate::checks::{CheckCategory, CheckRun, CheckState, CommandCheck};

#[derive(Clone, Copy)]
enum LeafResult<'run> {
    Command,
    Case(&'run CaseResult),
    NotFound(&'run TestItemId),
}

struct CheckLeaf<'run> {
    check: &'run CommandCheck,
    result: LeafResult<'run>,
    group: String,
    scope: String,
    name: Option<String>,
}

impl CheckLeaf<'_> {
    /// Distinguish opaque case identities even when their display names coincide.
    fn identity(&self) -> Option<&str> {
        match self.result {
            LeafResult::Case(case) => Some(case.id.as_str()),
            LeafResult::NotFound(id) => Some(id.as_str()),
            LeafResult::Command => self.name.as_deref(),
        }
    }
    fn state(&self) -> CheckState {
        match self.result {
            LeafResult::Command => self.check.state,
            LeafResult::Case(case) => match case.status {
                Some(CaseStatus::Passed) => CheckState::Passed,
                Some(CaseStatus::Skipped) => CheckState::Skipped,
                Some(CaseStatus::Failed | CaseStatus::TimedOut | CaseStatus::Errored) | None => {
                    CheckState::Failed
                }
            },
            LeafResult::NotFound(_) => CheckState::Failed,
        }
    }

    fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        match self.result {
            LeafResult::Command => self.check.elapsed_ms(now_ms),
            LeafResult::Case(case) => case.duration_ms,
            LeafResult::NotFound(_) => None,
        }
    }

    /// Keep contract statuses visible rather than displaying every incomplete case as failed.
    fn status_label(&self) -> String {
        match self.result {
            LeafResult::Case(case) => match case.status {
                None => "Not run".into(),
                Some(CaseStatus::TimedOut) => "Timed out".into(),
                Some(status) => format!("{status:?}"),
            },
            LeafResult::NotFound(_) => "Not found".into(),
            LeafResult::Command => format!("{:?}", self.state()),
        }
    }
}

/// Selects known file or target identities without assigning package-wide results to guessed files.
fn check_scope(check: &CommandCheck) -> (String, Option<String>) {
    use crate::checks::CheckSelection;
    let Some(context) = &check.context else {
        return (check.command.clone(), None);
    };
    let invocation = context.invocation.as_ref();
    if let Some(invocation) = invocation
        && invocation.files.len() == 1
    {
        return (invocation.files[0].replace('\\', "/"), None);
    }
    if let CheckSelection::Files { files } = &context.selection
        && files.len() == 1
    {
        return (files[0].replace('\\', "/"), None);
    }
    (context.name.clone(), None)
}

/// Keeps independent execution identities even when the same check and source repeat.
fn check_leaves(run: &CheckRun, category: CheckCategory) -> Vec<CheckLeaf<'_>> {
    let mut result = Vec::new();
    for check in &run.commands {
        if check
            .context
            .as_ref()
            .map_or(CheckCategory::TypeCheck, |context| context.category)
            != category
            || matches!(check.state, CheckState::Pending | CheckState::Skipped)
        {
            continue;
        }
        if let Some(tests) = check
            .result
            .as_ref()
            .and_then(crate::checks::adapter::AdapterResult::tests)
        {
            for case in &tests.cases {
                result.push(CheckLeaf {
                    check,
                    result: LeafResult::Case(case),
                    group: format!("{}:checks", run.id),
                    scope: check_scope(check).0,
                    name: Some(
                        case.item
                            .as_ref()
                            .map_or_else(|| case.id.as_str().to_owned(), |item| item.label.clone()),
                    ),
                });
            }
            for id in &tests.not_found {
                result.push(CheckLeaf {
                    check,
                    result: LeafResult::NotFound(id),
                    group: format!("{}:checks", run.id),
                    scope: check_scope(check).0,
                    name: Some(id.as_str().to_owned()),
                });
            }
            if tests.error.is_some()
                || tests.protocol_error.is_some()
                || tests.unsupported
                || tests.cancelled
                || tests.cases.is_empty() && tests.not_found.is_empty()
            {
                result.push(CheckLeaf {
                    check,
                    result: LeafResult::Command,
                    group: format!("{}:checks", run.id),
                    scope: check_scope(check).0,
                    name: Some("Runner result".into()),
                });
            }
        } else {
            let (scope, name) = check_scope(check);
            result.push(CheckLeaf {
                check,
                result: LeafResult::Command,
                group: format!("{}:checks", run.id),
                scope,
                name,
            });
        }
    }
    result
}

fn category_key(category: CheckCategory) -> &'static str {
    match category {
        CheckCategory::Format => "format",
        CheckCategory::Lint => "lint",
        CheckCategory::TypeCheck => "typecheck",
        CheckCategory::Test => "test",
        CheckCategory::Build => "build",
        CheckCategory::Custom => "custom",
    }
}

fn category_label(category: CheckCategory) -> &'static str {
    match category {
        CheckCategory::Format => "Formatting",
        CheckCategory::Lint => "Lint",
        CheckCategory::TypeCheck => "Type checking",
        CheckCategory::Test => "Tests",
        CheckCategory::Build => "Builds",
        CheckCategory::Custom => "Custom",
    }
}

pub(super) fn check_summary(run: &CheckRun) -> String {
    let leaves = [
        CheckCategory::TypeCheck,
        CheckCategory::Test,
        CheckCategory::Format,
        CheckCategory::Lint,
        CheckCategory::Build,
        CheckCategory::Custom,
    ]
    .into_iter()
    .flat_map(|category| check_leaves(run, category))
    .collect::<Vec<_>>();
    summarize_check_leaves(&leaves)
}

fn summarize_check_leaves(leaves: &[CheckLeaf<'_>]) -> String {
    let mut latest = BTreeMap::new();
    for leaf in leaves {
        let state = leaf.state();
        let category = leaf
            .check
            .context
            .as_ref()
            .map_or(CheckCategory::TypeCheck, |context| context.category);
        let definition = leaf
            .check
            .context
            .as_ref()
            .map_or(leaf.check.command.as_str(), |context| {
                context.definition_id.as_str()
            });
        let key = (
            category_key(category),
            definition,
            leaf.scope.as_str(),
            leaf.identity(),
        );
        let result = latest.entry(key).or_insert((state, 0_usize));
        result.0 = state;
        result.1 += 1;
    }
    // Pending and skipped commands produce no leaves; a run with none executed no check,
    // and a vacuous `0/0 passed` reads as success.
    if latest.is_empty() {
        return "No automatic checks ran".into();
    }
    let attempts = latest
        .values()
        .map(|(_, attempts)| *attempts)
        .max()
        .unwrap_or(0);
    let summary =
        CheckRun::summarize_states(latest.values().map(|(state, _)| *state), Some(latest.len()));
    format!(
        "{summary} · {attempts} {}",
        if attempts == 1 { "attempt" } else { "attempts" }
    )
}

impl TimelineRenderer<'_> {
    pub(super) fn checks(
        &mut self,
        run: &crate::checks::CheckRun,
        separate: bool,
        summary: Option<&str>,
    ) -> Result<()> {
        ensure!(
            run.state != crate::checks::CheckState::Running,
            "active checks belong to session status"
        );
        if separate && self.leading_separator {
            self.literal(&format!("{}:separator", run.id), "", None)?;
        }
        let heading = if let Some(summary) = summary {
            summary.to_owned()
        } else if run.context.is_some() {
            if run.lifecycle.iter().any(|event| {
                matches!(
                    event.kind,
                    crate::checks::CheckEventKind::ConfigurationSaved
                )
            }) {
                run.lifecycle
                    .last()
                    .map(|event| event.text.clone())
                    .unwrap_or_else(|| "Check configuration saved".into())
            } else {
                format!(
                    "Checks {} · {} · {:.1}s",
                    if run.state == crate::checks::CheckState::Interrupted {
                        "interrupted"
                    } else {
                        "finished"
                    },
                    check_summary(run),
                    run.duration_ms() as f64 / 1000.0
                )
            }
        } else {
            format!(
                "Checks finished · {} · {:.1}s",
                check_summary(run),
                run.duration_ms() as f64 / 1000.0
            )
        };
        self.literal(&format!("{}:checks", run.id), &heading, None)?;
        let checks = self.begin_section(2);
        if summary.is_none()
            && run
                .context
                .as_ref()
                .is_none_or(|context| context.trigger == "Plan execution")
        {
            self.literal(
                &format!("{}:declarations", run.id),
                &format!(
                    "Declarations · {}",
                    if run.progress.conforms() {
                        "match"
                    } else {
                        "differ"
                    }
                ),
                None,
            )?;
            let declarations = self.begin_section(2);
            for (index, finding) in run.progress.findings().iter().enumerate() {
                self.literal(&format!("{}:finding:{index}", run.id), finding, None)?;
            }
            for (index, change) in run.progress.declaration_changes.iter().enumerate() {
                self.diff(
                    &format!("{}:declaration:{index}", run.id),
                    &change.name,
                    &change.diff,
                    "",
                    Some((&change.path, &change.name)),
                    None,
                )?;
            }
            self.finish_section(declarations, &format!("{}:declarations", run.id), true);
        }
        for category in [
            CheckCategory::TypeCheck,
            CheckCategory::Test,
            CheckCategory::Format,
            CheckCategory::Lint,
            CheckCategory::Build,
            CheckCategory::Custom,
        ] {
            let leaves = check_leaves(run, category);
            if leaves.is_empty() {
                continue;
            }
            let category_id = format!("{}:category:{}", run.id, category_key(category));
            let duration = run
                .commands
                .iter()
                .filter(|check| {
                    check
                        .context
                        .as_ref()
                        .map_or(CheckCategory::TypeCheck, |context| context.category)
                        == category
                })
                .filter_map(|check| check.elapsed_ms(self.now_ms))
                .fold(0, u64::saturating_add);
            self.literal(
                &category_id,
                &format!(
                    "{} · {} · {:.1}s",
                    category_label(category),
                    summarize_check_leaves(&leaves),
                    duration as f64 / 1000.0
                ),
                None,
            )?;
            let section = self.begin_section(2);
            for command in &run.commands {
                if command
                    .context
                    .as_ref()
                    .is_some_and(|context| context.category == category)
                {
                    match &command.result {
                        Some(crate::checks::adapter::AdapterResult::Diagnostics(result)) => {
                            self.check_diagnostics(command, result, &category_id)?;
                        }
                        Some(crate::checks::adapter::AdapterResult::Format(result)) => {
                            self.check_format(command, result, &category_id)?;
                        }
                        Some(crate::checks::adapter::AdapterResult::Tests(_)) | None => {}
                    }
                }
            }
            let (passed, other): (Vec<_>, Vec<_>) = leaves.into_iter().partition(|leaf| {
                matches!(leaf.result, LeafResult::Case(_))
                    && matches!(leaf.state(), CheckState::Passed | CheckState::Skipped)
            });
            for (leaves, group_id, collapsed) in [
                (other, category_id.clone(), false),
                (passed, format!("{category_id}:passed"), true),
            ] {
                if leaves.is_empty() {
                    continue;
                }
                let passed_section = if collapsed {
                    self.literal(
                        &group_id,
                        &format!("Passed and skipped · {} cases", leaves.len()),
                        None,
                    )?;
                    Some(self.begin_section(2))
                } else {
                    None
                };
                let mut scopes = BTreeMap::<String, Vec<CheckLeaf<'_>>>::new();
                let mut test_commands = BTreeMap::<&str, Vec<CheckLeaf<'_>>>::new();
                for leaf in leaves {
                    if matches!(leaf.result, LeafResult::Case(_) | LeafResult::NotFound(_)) {
                        test_commands.entry(&leaf.check.id).or_default().push(leaf);
                    } else {
                        scopes.entry(leaf.scope.clone()).or_default().push(leaf);
                    }
                }
                for leaves in test_commands.values() {
                    self.check_test_tree(leaves, &group_id)?;
                }
                for (scope, leaves) in scopes {
                    let scope_id = format!("{group_id}:{}", crate::plan::digest(scope.as_bytes()));
                    if leaves.len() == 1 && leaves[0].name.is_none() {
                        self.check_leaf(&leaves[0], &scope)?;
                        continue;
                    }
                    let outcomes =
                        CheckRun::summarize_states(leaves.iter().map(CheckLeaf::state), None);
                    let elapsed = leaves.iter().try_fold(0_u64, |total, leaf| {
                        Some(total.saturating_add(leaf.elapsed_ms(self.now_ms)?))
                    });
                    let duration = elapsed
                        .map(|elapsed| format!(" · {:.1}s", elapsed as f64 / 1000.0))
                        .unwrap_or_default();
                    self.literal(&scope_id, &format!("{scope} · {outcomes}{duration}"), None)?;
                    let scope_section = self.begin_section(2);
                    let mut names = BTreeMap::<(String, String), Vec<&CheckLeaf<'_>>>::new();
                    for leaf in &leaves {
                        names
                            .entry((
                                leaf.name.clone().unwrap_or_default(),
                                leaf.identity().unwrap_or_default().to_owned(),
                            ))
                            .or_default()
                            .push(leaf);
                    }
                    for ((name, _), attempts) in names {
                        if attempts.len() == 1 {
                            let label = if name.is_empty() {
                                attempts[0]
                                    .check
                                    .context
                                    .as_ref()
                                    .map_or("Check", |context| context.name.as_str())
                            } else {
                                &name
                            };
                            self.check_leaf(attempts[0], label)?;
                        } else {
                            let named_section = if name.is_empty() {
                                None
                            } else {
                                let id =
                                    format!("{scope_id}:{}", crate::plan::digest(name.as_bytes()));
                                self.literal(&id, &name, None)?;
                                Some((self.begin_section(2), id))
                            };
                            for (index, leaf) in attempts.iter().enumerate() {
                                let label = format!("Run {}", index + 1);
                                self.check_leaf(leaf, &label)?;
                            }
                            if let Some((section, id)) = named_section {
                                self.finish_section(section, &id, false);
                            }
                        }
                    }
                    self.finish_section(scope_section, &scope_id, false);
                }
                if let Some(section) = passed_section {
                    self.finish_section(section, &group_id, true);
                }
            }
            self.finish_section(section, &category_id, true);
        }
        self.finish_section(checks, &format!("{}:checks", run.id), true);
        if let Some(title) = &run.completed_plan {
            self.literal(
                &format!("{}:complete", run.id),
                &format!("◇ Plan complete: {title}"),
                None,
            )?;
        }
        Ok(())
    }

    /// Render located diagnostics, incomplete targets, and build artifacts with their source actions.
    fn check_diagnostics(
        &mut self,
        command: &CommandCheck,
        result: &crate::checks::adapter::events::DiagnosticResult,
        parent: &str,
    ) -> Result<()> {
        use crate::checks::adapter::events::{Severity, TargetStatus};
        let prefix = format!(
            "{parent}:diagnostics:{}",
            crate::plan::digest(command.id.as_bytes())
        );
        let items: Vec<_> = result
            .diagnostics
            .iter()
            .filter_map(|diagnostic| {
                diagnostic.path.as_ref().map(|path| super::QuickfixItem {
                    path: path.clone(),
                    line: diagnostic.range.map(|range| {
                        usize::try_from(range.start().line()).expect("source line fits usize")
                    }),
                    column: diagnostic.range.map(|range| {
                        usize::try_from(range.start().column()).expect("source column fits usize")
                    }),
                    message: diagnostic.code.as_ref().map_or_else(
                        || diagnostic.message.clone(),
                        |code| format!("{} [{code}]", diagnostic.message),
                    ),
                    severity: diagnostic.severity,
                })
            })
            .collect();
        if !items.is_empty() {
            self.literal(
                &format!("{prefix}:quickfix"),
                &format!(
                    "Load diagnostics into quickfix · {} located diagnostics",
                    items.len()
                ),
                Some(TranscriptAction::Quickfix {
                    title: command
                        .context
                        .as_ref()
                        .map_or_else(|| command.command.clone(), |context| context.name.clone()),
                    items,
                }),
            )?;
        }
        for (severity, label, collapsed) in [
            (Severity::Error, "Errors", false),
            (Severity::Warning, "Warnings", false),
            (Severity::Info, "Information", true),
            (Severity::Hint, "Hints", true),
        ] {
            let mut files = BTreeMap::<
                Option<&str>,
                Vec<(usize, &crate::checks::adapter::events::Diagnostic)>,
            >::new();
            for (index, diagnostic) in result
                .diagnostics
                .iter()
                .enumerate()
                .filter(|(_, diagnostic)| diagnostic.severity == severity)
            {
                files
                    .entry(diagnostic.path.as_deref())
                    .or_default()
                    .push((index, diagnostic));
            }
            let omitted = result.truncated.get(&severity).copied().unwrap_or_default();
            if files.is_empty() && omitted == 0 {
                continue;
            }
            let id = format!("{prefix}:{severity:?}");
            let count = files.values().map(Vec::len).sum::<usize>() + omitted;
            self.literal(&id, &format!("{label} · {count}"), None)?;
            let section = self.begin_section(2);
            for (path, diagnostics) in files {
                let file_id = format!(
                    "{id}:file:{}",
                    crate::plan::digest(path.unwrap_or_default().as_bytes())
                );
                self.literal(&file_id, path.unwrap_or("No source location"), None)?;
                let file_section = self.begin_section(2);
                for (index, diagnostic) in diagnostics {
                    let location = diagnostic
                        .range
                        .map(|range| {
                            format!("{}:{}: ", range.start().line(), range.start().column())
                        })
                        .unwrap_or_default();
                    let code = diagnostic
                        .code
                        .as_ref()
                        .map(|code| format!(" [{code}]"))
                        .unwrap_or_default();
                    let action = path.map(|path| TranscriptAction::File {
                        path: path.to_owned(),
                        line: diagnostic.range.map_or(1, |range| {
                            usize::try_from(range.start().line()).expect("source line fits usize")
                        }),
                        column: diagnostic.range.map(|range| {
                            usize::try_from(range.start().column())
                                .expect("source column fits usize")
                        }),
                    });
                    self.literal(
                        &format!("{prefix}:message:{index}"),
                        &format!("{location}{}{code}", diagnostic.message),
                        action,
                    )?;
                }
                self.finish_section(file_section, &file_id, false);
            }
            if omitted > 0 {
                self.literal(
                    &format!("{id}:omitted"),
                    &format!("{omitted} additional diagnostics omitted"),
                    None,
                )?;
            }
            self.finish_section(section, &id, collapsed);
        }
        for (index, target) in result.targets.iter().enumerate().filter(|(_, target)| {
            target
                .status
                .is_none_or(|status| status == TargetStatus::Failed)
        }) {
            let status = if target.status.is_none() {
                "Not run"
            } else {
                "Failed"
            };
            self.literal(
                &format!("{prefix}:target:{index}"),
                &format!("{} · {status}", target.id.as_str()),
                None,
            )?;
        }
        if !result.artifacts.is_empty() {
            let id = format!("{prefix}:artifacts");
            self.literal(
                &id,
                &format!("Artifacts · {}", result.artifacts.len()),
                None,
            )?;
            let section = self.begin_section(2);
            for (index, (path, kind)) in result.artifacts.iter().enumerate() {
                let label = kind
                    .as_ref()
                    .map_or_else(|| path.clone(), |kind| format!("{path} · {kind}"));
                self.literal(
                    &format!("{id}:{index}"),
                    &label,
                    Some(TranscriptAction::File {
                        path: path.clone(),
                        line: 1,
                        column: None,
                    }),
                )?;
            }
            self.finish_section(section, &id, true);
        }
        if result.truncated_bytes > 0 {
            self.literal(
                &format!("{prefix}:truncated"),
                &format!(
                    "{} bytes of diagnostic text omitted",
                    result.truncated_bytes
                ),
                None,
            )?;
        }
        Ok(())
    }

    /// Render formatter outcomes without treating an unknown processed set as checked files.
    fn check_format(
        &mut self,
        command: &CommandCheck,
        result: &crate::checks::adapter::events::FormatResult,
        parent: &str,
    ) -> Result<()> {
        use crate::checks::adapter::events::FormatStatus;
        let prefix = format!(
            "{parent}:format:{}",
            crate::plan::digest(command.id.as_bytes())
        );
        for file in &result.files {
            let status = match file.status {
                None => "Not run",
                Some(FormatStatus::Formatted) => "Formatted",
                Some(FormatStatus::Unformatted) => "Unformatted",
                Some(FormatStatus::Changed) => "Changed",
                Some(FormatStatus::Failed) => "Failed",
            };
            let id = format!("{prefix}:{}", crate::plan::digest(file.path.as_bytes()));
            self.literal(
                &id,
                &format!("{} · {status}", file.path),
                Some(TranscriptAction::File {
                    path: file.path.clone(),
                    line: 1,
                    column: None,
                }),
            )?;
            if file.message.is_some() || file.diff.is_some() {
                let section = self.begin_section(2);
                if let Some(message) = &file.message {
                    self.literal(&format!("{id}:message"), message, None)?;
                }
                if let Some(diff) = &file.diff {
                    self.literal(&format!("{id}:diff"), diff, None)?;
                }
                self.finish_section(section, &id, true);
            }
        }
        if let Some(reason) = &result.widened {
            self.literal(&format!("{prefix}:widened"), reason, None)?;
        }
        if result.truncated_bytes > 0 {
            self.literal(
                &format!("{prefix}:truncated"),
                &format!("{} bytes of format detail omitted", result.truncated_bytes),
                None,
            )?;
        }
        Ok(())
    }

    /// Render retained adapter ancestry using command and item identities, without recursive calls.
    fn check_test_tree(&mut self, leaves: &[CheckLeaf<'_>], group: &str) -> Result<()> {
        let Some(first) = leaves.first() else {
            return Ok(());
        };
        let Some(tests) = first
            .check
            .result
            .as_ref()
            .and_then(crate::checks::adapter::AdapterResult::tests)
        else {
            return Ok(());
        };
        let items = tests
            .items
            .iter()
            .map(|item| (&item.id, item))
            .collect::<BTreeMap<_, _>>();
        let mut active = BTreeMap::<&TestItemId, &TestItem>::new();
        let mut cases = BTreeMap::<Option<&TestItemId>, Vec<&CheckLeaf<'_>>>::new();
        for leaf in leaves {
            let parent = match leaf.result {
                LeafResult::Case(case) => case.item.as_ref().and_then(|item| item.parent.as_ref()),
                LeafResult::NotFound(_) | LeafResult::Command => None,
            };
            cases
                .entry(parent.filter(|id| items.contains_key(id)))
                .or_default()
                .push(leaf);
            let mut next = parent;
            while let Some(item) = next.and_then(|id| items.get(id)).copied() {
                if active.insert(&item.id, item).is_some() {
                    break;
                }
                next = item.parent.as_ref();
            }
        }
        let mut children = BTreeMap::<Option<&TestItemId>, Vec<&TestItem>>::new();
        for item in active.values() {
            children
                .entry(item.parent.as_ref().filter(|id| active.contains_key(id)))
                .or_default()
                .push(item);
        }

        // Retained trees can be deeply nested; explicit enter/exit events avoid stack overflow.
        enum Visit<'a, 'run> {
            Children(Option<&'run TestItemId>),
            Group(&'run TestItem),
            Leaf(&'a CheckLeaf<'run>),
            End(Section, String),
        }
        let mut visits = vec![Visit::Children(None)];
        while let Some(visit) = visits.pop() {
            match visit {
                Visit::Children(parent) => {
                    if let Some(leaves) = cases.get(&parent) {
                        let mut ordered = leaves.clone();
                        ordered.sort_by_key(|leaf| leaf.identity());
                        visits.extend(ordered.into_iter().rev().map(Visit::Leaf));
                    }
                    if let Some(items) = children.get(&parent) {
                        visits.extend(items.iter().rev().map(|item| Visit::Group(item)));
                    }
                }
                Visit::Group(item) => {
                    let identity = serde_json::to_vec(&(&first.check.id, &item.id))?;
                    let id = format!("{group}:item:{}", crate::plan::digest(&identity));
                    self.literal(&id, &item.label, None)?;
                    let section = self.begin_section(2);
                    visits.push(Visit::End(section, id));
                    visits.push(Visit::Children(Some(&item.id)));
                }
                Visit::Leaf(leaf) => {
                    self.check_leaf(leaf, leaf.name.as_deref().unwrap_or("Test"))?;
                }
                Visit::End(section, id) => {
                    self.finish_section(section, &id, false);
                }
            }
        }
        Ok(())
    }

    /// Renders one execution with compact status and lazily expanded diagnostics.
    fn check_leaf(&mut self, leaf: &CheckLeaf<'_>, label: &str) -> Result<()> {
        let check = leaf.check;
        let call_id = match leaf.result {
            LeafResult::Case(case) => self.display.test_case(&check.id, &case.id),
            LeafResult::NotFound(id) => self.display.test_case(&check.id, id),
            LeafResult::Command => self.display.check(&check.id),
        };
        let state = leaf.state();
        let elapsed = leaf.elapsed_ms(self.now_ms);
        let failed = matches!(
            state,
            CheckState::Failed
                | CheckState::Blocked
                | CheckState::ConfigurationError
                | CheckState::ExecutionError
        );
        let title = format!(
            "{} · {}",
            label.replace(['\r', '\n'], " "),
            leaf.status_label(),
        );
        let heading = crate::turn::ToolCall::projection(
            call_id.to_string(),
            "check".into(),
            title.clone(),
            if failed {
                crate::turn::ToolState::Failed
            } else if check.completed_at_ms.is_some() || elapsed.is_some() {
                crate::turn::ToolState::Completed
            } else {
                crate::turn::ToolState::Running
            },
            elapsed.map(|_| 0),
            elapsed.map(|value| i64::try_from(value).unwrap_or(i64::MAX)),
        );
        let mut output = if let LeafResult::Case(case) = leaf.result {
            if let Some(path) = &case.output {
                match std::fs::metadata(path) {
                    Ok(metadata) => ToolOutputView::file(call_id, path.clone(), metadata.len()),
                    Err(error) => ToolOutputView::new(
                        call_id,
                        &format!("Case output unavailable ({}): {error}", path.display()),
                    ),
                }
            } else {
                ToolOutputView::new(call_id, "No case output was captured.")
            }
        } else if let LeafResult::NotFound(_) = leaf.result {
            ToolOutputView::new(call_id, "The requested item has no case in the run plan.")
        } else {
            ToolOutputView::file(call_id, check.output.clone(), check.output_bytes)
        };
        output.heading(&heading, crate::turn::ToolInstant::wall(self.now_ms));
        output.group = leaf.group.clone();
        let expanded = self.expansion.get(&format!("{call_id}:tool")).copied() == Some(true);
        let limit = self
            .tool_limit
            .get(&format!("{call_id}:tool"))
            .copied()
            .unwrap_or(super::super::tool::INITIAL_INLINE_BYTES);
        if expanded {
            output.load_prefix(limit)?;
        }
        let width = self.content_width();
        let renderer = TranscriptRenderer::new(&width)?;
        let start = self.block.len();
        self.push(renderer.tool_header(
            BlockId(format!("{call_id}:tool")),
            TargetId(format!("{call_id}:tool")),
            crate::buffer::transcript::ToolHeading {
                shell: None,
                kind: "check",
                elapsed_ms: elapsed,
                failed,
                denied: false,
                title: &title,
                expanded,
            },
        )?)?;
        self.action.insert(
            TargetId(format!("{call_id}:tool")),
            TranscriptAction::Tool { call_id },
        );
        if expanded {
            for chunk in 0..output.inline_prefix_chunks(limit)? {
                self.push(renderer.tool_body(&call_id, &output, true, chunk)?)?;
            }
            self.push(renderer.tool_hidden(&call_id, &output, true)?)?;
        } else {
            for suffix in ["preview", "hidden"] {
                self.push(BufferBlock {
                    id: BlockId(format!("{call_id}:{suffix}")),
                    text: Default::default(),
                    metadata: Default::default(),
                })?;
            }
        }
        if let LeafResult::Case(case) = leaf.result {
            for (index, failure) in case.failures.iter().enumerate() {
                let location = failure
                    .path
                    .as_ref()
                    .map(|path| format!("{path}:{}: ", failure.line.unwrap_or(1)))
                    .unwrap_or_default();
                let action = failure.path.as_ref().map(|path| TranscriptAction::File {
                    path: path.clone(),
                    line: usize::try_from(failure.line.unwrap_or(1))
                        .expect("source line fits usize"),
                    column: None,
                });
                self.literal(
                    &format!("{call_id}:failure:{index}"),
                    &format!("{location}{}", failure.message),
                    action,
                )?;
            }
            if case.output_truncated_bytes > 0 {
                self.literal(
                    &format!("{call_id}:truncated"),
                    &format!(
                        "{} bytes of case output omitted",
                        case.output_truncated_bytes
                    ),
                    None,
                )?;
            }
        }
        if matches!(leaf.result, LeafResult::Command)
            && let Some(error) = &check.error
        {
            self.literal(&format!("{call_id}:error"), error, None)?;
        }
        self.fold(start, &format!("{call_id}:tool"), true, NodeKind::Tool);
        if let Some(node) = &mut self.block[start].metadata.node {
            node.more = expanded && output.has_unloaded_output();
        }
        self.tool.insert(call_id, output);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Render restored diagnostics with precise source links, severity groups, and artifacts.
    #[test]
    fn diagnostics_keep_source_actions_and_severity_after_restore() -> Result<()> {
        let run: CheckRun = serde_json::from_value(json!({
            "id":"diagnostic-run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":11,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":[{"id":"compiler","command":"compiler","state":"failed","started_at_ms":1,"completed_at_ms":11,"exit_code":0,
                "error":null,"stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                "context":{"purpose":"verify","definition_id":"compiler","name":"Compiler","area":".","reason":"selected","matched_files":[],"category":"build","directory":".","selection":{"kind":"all"},"resolved":null,"depends_on":[]},
                "result":{"category":"diagnostics","selection":{"paths":[]},"widened":null,
                    "targets":[{"id":"unfinished","status":null,"duration_ms":null}],
                    "diagnostics":[
                        {"severity":"warning","message":"unused value","path":"src/main.rs"},
                        {"severity":"error","message":"wrong type","path":"src/main.rs","range":{"start":{"line":7,"column":3}},"code":"E0001"},
                        {"severity":"info","message":"additional context"}],
                    "truncated":{"error":2},"truncated_bytes":9,"artifacts":[["target/program","binary"]],
                    "error":null,"protocol_error":null,"exit":0,"cancelled":false,"unsupported":false}}]
        }))?;
        let projected = project(
            &TimelineEntry::Checks {
                id: run.id.clone(),
                created_at_ms: 1,
                run: run.into(),
            },
            &WidthProfile::default(),
            FIXTURE_NOW_MS,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        for expected in [
            "Errors · 3",
            "Warnings · 1",
            "7:3: wrong type [E0001]",
            "unfinished · Not run",
            "target/program · binary",
            "2 additional diagnostics omitted",
            "9 bytes of diagnostic text omitted",
        ] {
            assert!(text.contains(expected), "{text}");
        }
        assert!(text.find("wrong type") < text.find("unused value"));
        assert!(projected.action.values().any(|action| matches!(action, TranscriptAction::File { path, line:7, column:Some(3) } if path == "src/main.rs")));
        let quickfix = projected
            .action
            .values()
            .find_map(|action| match action {
                TranscriptAction::Quickfix { items, .. } => Some(items),
                _ => None,
            })
            .expect("located diagnostics expose quickfix");
        assert_eq!(
            quickfix.len(),
            2,
            "unlocated diagnostics must not enter quickfix"
        );
        assert_eq!(quickfix[0].line, None);
        assert_eq!(quickfix[1].line, Some(7));
        assert_eq!(quickfix[1].column, Some(3));
        assert_eq!(quickfix[1].message, "wrong type [E0001]");
        assert_eq!(
            quickfix[1].severity,
            crate::checks::adapter::events::Severity::Error
        );
        assert!(text.contains("Load diagnostics into quickfix · 2 located diagnostics"));
        let info = projected
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("Information · 1"))
            })
            .unwrap();
        assert_eq!(
            info.metadata.node.as_ref().unwrap().default_display,
            NodeDisplay::Heading
        );
        Ok(())
    }

    /// Keep restored formatting failures, details, and file navigation visible.
    #[test]
    fn format_results_preserve_file_details_after_restore() -> Result<()> {
        let run: CheckRun = serde_json::from_value(json!({
            "id":"format-run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":11,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":[{"id":"formatter","command":"formatter","state":"failed","started_at_ms":1,"completed_at_ms":11,"exit_code":0,
                "error":null,"stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                "context":{"purpose":"verify","definition_id":"formatter","name":"Formatter","area":".","reason":"selected","matched_files":[],"category":"format","directory":".","selection":{"kind":"all"},"resolved":null,"depends_on":[]},
                "result":{"category":"format","operation":"check","selection":{"paths":[]},"widened":"checked workspace",
                    "files":[{"path":"src/main.rs","status":"unformatted","message":"needs formatting","diff":"-old\n+new"},
                             {"path":"src/missing.rs","status":null,"message":null,"diff":null}],
                    "truncated_bytes":9,"error":null,"protocol_error":null,"exit":0,"cancelled":false,"unsupported":false}}]
        }))?;
        let projected = project(
            &TimelineEntry::Checks {
                id: run.id.clone(),
                created_at_ms: 1,
                run: run.into(),
            },
            &WidthProfile::default(),
            FIXTURE_NOW_MS,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        for expected in [
            "src/main.rs · Unformatted",
            "src/missing.rs · Not run",
            "needs formatting",
            "-old",
            "+new",
            "checked workspace",
            "9 bytes of format detail omitted",
        ] {
            assert!(text.contains(expected), "{text}");
        }
        assert!(projected.action.values().any(|action| matches!(action, TranscriptAction::File { path, line:1, .. } if path == "src/main.rs")));
        Ok(())
    }

    /// Render contract outcomes, source navigation, and per-case output with stable identities.
    #[test]
    fn adapter_cases_preserve_status_locations_output_and_identity() -> Result<()> {
        let scratch = tempfile::tempdir()?;
        let output = scratch.path().join("case.output");
        std::fs::write(&output, "case output only\n")?;
        let items = json!([
            {"id":"root#","label":"Suite"},
            {"id":"parent#","parent":"root#","label":"spec.lua"},
            {"id":"other-root#","label":"Suite"},
            {"id":"other-parent#","parent":"other-root#","label":"spec.lua"}
        ]);
        let cases = json!([
            {"id":"pass#","status":"passed","duration_ms":3,"failures":[],"item":{"id":"pass#","parent":"parent#","label":"works","case":true},"output":null,"output_truncated_bytes":0},
            {"id":"fail#","status":"failed","duration_ms":4,"failures":[{"message":"expected value","path":"spec.lua","line":17}],"item":{"id":"fail#","parent":"parent#","label":"breaks","case":true},"output":output,"output_truncated_bytes":2},
            {"id":"other-fail#","status":"failed","duration_ms":4,"failures":[],"item":{"id":"other-fail#","parent":"other-parent#","label":"other failure","case":true},"output":null,"output_truncated_bytes":0},
            {"id":"pending#","status":null,"duration_ms":null,"failures":[],"item":{"id":"pending#","parent":"parent#","label":"unfinished","case":true},"output":null,"output_truncated_bytes":0}
        ]);
        let mut run: CheckRun = serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":11,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},
            "commands":[{"id":"command","command":"test","state":"failed","started_at_ms":1,"completed_at_ms":11,
                "exit_code":1,"error":null,"stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                "context":{"purpose":"verify","definition_id":"tests","name":"Tests","area":".","reason":"selected","matched_files":[],"category":"test","directory":".","selection":{"kind":"all"},"resolved":null,"depends_on":[]},
                "result":{"category":"tests","selection":{"ids":[],"affected":[]},"items":items,"widened":null,"cases":cases,"not_found":["missing#"],"error":null,"protocol_error":null,"exit":1,"cancelled":false,"unsupported":false,"output_truncated_bytes":2}}]
        }))?;
        let mut registry = crate::buffer::identity::DisplayRegistry::default();
        let render = |run: &CheckRun, registry: &mut crate::buffer::identity::DisplayRegistry| {
            project(
                &TimelineEntry::Checks {
                    id: "run".into(),
                    created_at_ms: 1,
                    run: run.clone().into(),
                },
                &WidthProfile::default(),
                FIXTURE_NOW_MS,
                false,
                &HashMap::new(),
                &HashMap::new(),
                registry,
            )
        };
        let mut projected = render(&run, &mut registry)?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        for expected in [
            "Suite",
            "spec.lua",
            "breaks · Failed",
            "unfinished · Not run",
            "missing# · Not found",
            "spec.lua:17: expected value",
            "2 bytes of case output omitted",
        ] {
            assert!(text.contains(expected), "{text}");
        }
        assert!(text.find("breaks · Failed") < text.find("Passed and skipped"));
        let successful = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0.ends_with(":passed"))
            .unwrap();
        assert_eq!(
            successful.metadata.node.as_ref().unwrap().default_display,
            NodeDisplay::Heading
        );
        assert!(projected.action.values().any(
            |action| matches!(action, TranscriptAction::File {path,line:17, ..} if path == "spec.lua")
        ));
        let failed = projected
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("breaks · Failed"))
            })
            .unwrap();
        let failed_id = failed.id.clone();
        let parent_id = failed
            .metadata
            .node
            .as_ref()
            .unwrap()
            .parent
            .as_ref()
            .unwrap()
            .clone();
        let parent = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == parent_id.0)
            .unwrap();
        assert!(
            parent
                .text
                .wire_rows()
                .iter()
                .any(|row| row.trim() == "spec.lua")
        );
        let root_id = parent
            .metadata
            .node
            .as_ref()
            .unwrap()
            .parent
            .as_ref()
            .unwrap();
        let root = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == root_id.0)
            .unwrap();
        assert!(
            root.text
                .wire_rows()
                .iter()
                .any(|row| row.trim() == "Suite")
        );
        let other = projected
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("other failure · Failed"))
            })
            .unwrap();
        assert_ne!(
            other
                .metadata
                .node
                .as_ref()
                .unwrap()
                .parent
                .as_ref()
                .unwrap(),
            &parent_id
        );
        let TranscriptAction::Tool { call_id } = projected
            .action
            .get(&TargetId(failed_id.0.clone()))
            .unwrap()
        else {
            panic!("case output action missing")
        };
        let view = projected.tool.get_mut(call_id).unwrap();
        view.load_prefix(1024)?;
        assert_eq!(view.preview(true)?.row, ["case output only"]);
        let Some(crate::checks::adapter::AdapterResult::Tests(tests)) = &mut run.commands[0].result
        else {
            panic!("test results missing")
        };
        tests.cases.reverse();
        tests.items.reverse();
        // Exercise the same retained tree after a history serialization round trip.
        run = serde_json::from_slice(&serde_json::to_vec(&run)?)?;
        let reordered = render(&run, &mut registry)?;
        let row = reordered
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("breaks · Failed"))
            })
            .unwrap();
        assert_eq!(row.id, failed_id);
        assert_eq!(
            row.metadata.node.as_ref().unwrap().parent.as_ref(),
            Some(&parent_id)
        );
        std::fs::remove_file(output)?;
        let missing_output = render(&run, &mut registry)?;
        assert!(
            missing_output
                .tool
                .values()
                .any(|view| view.preview(true).is_ok_and(|preview| preview
                    .row
                    .iter()
                    .any(|row| row.contains("Case output unavailable"))))
        );
        Ok(())
    }

    #[test]
    fn categories_preserve_file_groups_repeated_runs_and_original_outcomes() -> Result<()> {
        let mut commands = Vec::new();
        for (index, category, file, state) in [
            (0, "format", "src/main.rs", "passed"),
            (1, "format", "src/main.rs", "failed"),
            (2, "test", "tests/ui.lua", "passed"),
            (3, "lint", "src/main.rs", "passed"),
            (4, "build", "game", "passed"),
        ] {
            commands.push(json!({
                "id":format!("run:{index}"), "command":"echo checked", "state":state,
                "started_at_ms":1,"completed_at_ms":1 + (index + 1) * 100,"exit_code":if state == "failed" {1} else {0},
                "error":null,"stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                "context":{"purpose":"verify","definition_id":category,"name":format!("{category} check"),"area":".","reason":"selected",
                    "matched_files":[file],"category":category,"directory":".","selection":{"kind":"files","files":[file]},
                    "resolved":null,"depends_on":[]}
            }));
        }
        let run: CheckRun = serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":11,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":commands
        }))?;
        let projected = project(
            &TimelineEntry::Checks {
                id: "run".into(),

                created_at_ms: 1,
                run: (run.clone()).into(),
            },
            &WidthProfile::default(),
            crate::buffer::projection::FIXTURE_NOW_MS,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let blocks = &projected.entry.block;
        let text = blocks
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.contains("3/4 passed · 1 failed · 2 attempts"),
            "{text}"
        );
        for category in ["Formatting", "Lint", "Tests", "Builds"] {
            assert!(text.contains(category));
        }
        assert!(text.contains("Run 1") && text.contains("Run 2"));
        assert!(
            text.contains("src/main.rs · 1 passed · 1 failed · 0.3s"),
            "{text}"
        );
        assert!(text.contains("Run 1 · Passed · 0.1s"), "{text}");
        assert!(text.contains("Run 2 · Failed · 0.2s"), "{text}");
        assert!(
            text.contains("Formatting · 0/1 passed · 1 failed · 2 attempts · 0.3s"),
            "{text}"
        );
        assert!(
            text.contains("Tests · 1/1 passed · 1 attempt · 0.3s"),
            "{text}"
        );
        assert!(!text.contains("format check"), "{text}");
        assert!(text.contains("Failed"));
        for category in ["format", "lint", "test", "build"] {
            let block = blocks
                .iter()
                .find(|block| block.id.0 == format!("run:category:{category}"))
                .unwrap();
            assert_eq!(
                block.metadata.node.as_ref().unwrap().default_display,
                NodeDisplay::Heading
            );
            assert_eq!(
                block.metadata.node.as_ref().unwrap().display,
                NodeDisplay::Heading
            );
            assert_eq!(
                block
                    .metadata
                    .node
                    .as_ref()
                    .unwrap()
                    .parent
                    .as_ref()
                    .unwrap()
                    .0,
                "run:checks"
            );
        }
        let mut retried = run.clone();
        for attempt in 2..5 {
            let mut command = run.commands[1].clone();
            command.id = format!("retry-{attempt}");
            command.state = if attempt == 4 {
                CheckState::Passed
            } else {
                CheckState::Failed
            };
            retried.commands.push(command);
        }
        let mut independent = run.commands[0].clone();
        independent.id = "independent".into();
        independent.context.as_mut().unwrap().definition_id = "other-format".into();
        retried.commands.push(independent);
        assert_eq!(check_summary(&retried), "5/5 passed · 5 attempts");
        let mut regression = run.commands[1].clone();
        regression.id = "regression".into();
        retried.commands.push(regression.clone());
        assert_eq!(
            check_summary(&retried),
            "4/5 passed · 1 failed · 6 attempts"
        );
        regression.id = "excluded".into();
        regression.state = CheckState::Skipped;
        retried.commands.push(regression);
        assert_eq!(
            check_summary(&retried),
            "4/5 passed · 1 failed · 6 attempts"
        );

        let mut failed_run = run;
        failed_run.commands[0].state = CheckState::Failed;
        failed_run.commands[1].completed_at_ms = None;
        let failed_projection = project(
            &TimelineEntry::Checks {
                id: "run".into(),

                created_at_ms: 1,
                run: (failed_run).into(),
            },
            &WidthProfile::default(),
            crate::buffer::projection::FIXTURE_NOW_MS,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let heading = failed_projection
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .find(|row| row.contains("src/main.rs · 0 passed · 2 failed"))
            .expect("failed file group omitted its outcome counts");
        assert_eq!(heading.trim(), "src/main.rs · 0 passed · 2 failed");
        Ok(())
    }
}
