use super::*;
use crate::checks::{CheckCategory, CheckResult, CheckRun, CheckState, CommandCheck};

struct CheckLeaf<'run> {
    check: &'run CommandCheck,
    result: Option<&'run CheckResult>,
    call_id: String,
    group: String,
    scope: String,
    name: Option<String>,
}

impl CheckLeaf<'_> {
    fn state(&self) -> CheckState {
        self.result.map_or(self.check.state, |result| result.state)
    }

    fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        self.result.map_or_else(
            || self.check.elapsed_ms(now_ms),
            |result| result.duration_ms,
        )
    }
}

/// Selects known file or target identities without assigning package-wide results to guessed files.
fn check_scope(check: &CommandCheck) -> (String, Option<String>) {
    use crate::checks::CheckSelection;
    let Some(context) = &check.context else {
        return (check.command.clone(), None);
    };
    let invocation = context.invocation.as_ref();
    let cases = invocation
        .map(|invocation| invocation.cases.as_slice())
        .unwrap_or_default();
    let name = (!cases.is_empty()).then(|| {
        cases
            .iter()
            .map(|case| case.name.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    });
    if let CheckSelection::Cases { cases } = &context.selection
        && let Some(case) = cases.first()
        && cases.iter().all(|candidate| candidate.file == case.file)
    {
        return (
            case.file.replace('\\', "/"),
            name.or_else(|| Some(case.name.clone())),
        );
    }
    if let Some(invocation) = invocation
        && invocation.files.len() == 1
    {
        return (invocation.files[0].replace('\\', "/"), name);
    }
    if let CheckSelection::Files { files } = &context.selection
        && files.len() == 1
    {
        return (files[0].replace('\\', "/"), name);
    }
    (context.name.clone(), name)
}

/// Keeps independent execution identities even when the same check and source repeat.
fn check_leaves(run: &CheckRun, category: CheckCategory) -> Vec<CheckLeaf<'_>> {
    let mut result = Vec::new();
    for check in &run.commands {
        if check
            .context
            .as_ref()
            .map_or(CheckCategory::Check, |context| context.category)
            != category
            || matches!(check.state, CheckState::Pending | CheckState::Skipped)
        {
            continue;
        }
        if let Some(report) = &check.report
            && !report.result.is_empty()
        {
            for (index, fixture) in report.result.iter().enumerate() {
                result.push(CheckLeaf {
                    check,
                    result: Some(fixture),
                    call_id: format!("{}:result:{index}", check.id),
                    group: format!("{}:checks", run.id),
                    scope: fixture.file.clone(),
                    name: None,
                });
            }
            if !matches!(check.state, CheckState::Passed | CheckState::Failed)
                || check.state == CheckState::Failed
                    && report
                        .result
                        .iter()
                        .all(|result| result.state != CheckState::Failed)
            {
                let (scope, _) = check_scope(check);
                result.push(CheckLeaf {
                    check,
                    result: None,
                    call_id: check.id.clone(),
                    group: format!("{}:checks", run.id),
                    scope,
                    name: Some("Runner result".into()),
                });
            }
        } else {
            let (scope, name) = check_scope(check);
            result.push(CheckLeaf {
                check,
                result: None,
                call_id: check.id.clone(),
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
        CheckCategory::Check => "check",
        CheckCategory::Test => "test",
        CheckCategory::Build => "build",
    }
}

fn category_label(category: CheckCategory) -> &'static str {
    match category {
        CheckCategory::Format => "Formatting",
        CheckCategory::Lint => "Lint",
        CheckCategory::Check => "Validation",
        CheckCategory::Test => "Tests",
        CheckCategory::Build => "Builds",
    }
}

pub(super) fn check_summary(run: &CheckRun) -> String {
    let leaves = [
        CheckCategory::Format,
        CheckCategory::Lint,
        CheckCategory::Check,
        CheckCategory::Test,
        CheckCategory::Build,
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
            .map_or(CheckCategory::Check, |context| context.category);
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
            leaf.name.as_deref(),
        );
        let result = latest.entry(key).or_insert((state, 0_usize));
        result.0 = state;
        result.1 += 1;
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
            CheckCategory::Format,
            CheckCategory::Lint,
            CheckCategory::Check,
            CheckCategory::Test,
            CheckCategory::Build,
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
                        .map_or(CheckCategory::Check, |context| context.category)
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
            let mut scopes = BTreeMap::<String, Vec<CheckLeaf<'_>>>::new();
            for leaf in leaves {
                scopes.entry(leaf.scope.clone()).or_default().push(leaf);
            }
            for (scope, leaves) in scopes {
                let scope_id = format!("{category_id}:{}", crate::plan::digest(scope.as_bytes()));
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
                let mut names = BTreeMap::<String, Vec<&CheckLeaf<'_>>>::new();
                for leaf in &leaves {
                    names
                        .entry(leaf.name.clone().unwrap_or_default())
                        .or_default()
                        .push(leaf);
                }
                for (name, attempts) in names {
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
                            let id = format!("{scope_id}:{}", crate::plan::digest(name.as_bytes()));
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

    /// Renders one execution with compact status and lazily expanded diagnostics.
    fn check_leaf(&mut self, leaf: &CheckLeaf<'_>, label: &str) -> Result<()> {
        let check = leaf.check;
        let call_id = &leaf.call_id;
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
            "{} · {state:?}{}",
            label.replace(['\r', '\n'], " "),
            if check
                .report
                .as_ref()
                .is_some_and(|report| report.error.is_some())
            {
                " · result report unavailable"
            } else {
                ""
            }
        );
        let heading = crate::turn::ToolCall {
            id: call_id.clone(),
            kind: "check".into(),
            title: title.clone(),
            started_at_ms: elapsed.map(|_| 0),
            completed_at_ms: elapsed.map(|value| value.min(i64::MAX as u64) as i64),
            task_id: None,
            output: Default::default(),
            status: String::new(),
            failed,
            change: Default::default(),
        };
        let mut output = if let Some(result) = leaf.result {
            let details = format!("Command: {}\n{}", result.command, result.output);
            let raw =
                ToolOutputView::file(check.id.clone(), check.output.clone(), check.output_bytes)?;
            self.tool.insert(check.id.clone(), raw);
            ToolOutputView::new(call_id.clone(), &details)?
        } else {
            ToolOutputView::file(call_id.clone(), check.output.clone(), check.output_bytes)?
        };
        output.heading(&heading);
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
            "check",
            elapsed,
            failed,
            &title,
            expanded,
        )?)?;
        self.action.insert(
            TargetId(format!("{call_id}:tool")),
            TranscriptAction::Tool {
                call_id: check.id.clone(),
            },
        );
        if expanded {
            for chunk in 0..output.inline_prefix_chunks(limit)? {
                self.push(renderer.tool_body(call_id, &output, true, chunk)?)?;
            }
            self.push(renderer.tool_hidden(call_id, &output, true)?)?;
        } else {
            for suffix in ["preview", "hidden"] {
                self.push(BufferBlock {
                    id: BlockId(format!("{call_id}:{suffix}")),
                    text: Default::default(),
                    metadata: Default::default(),
                })?;
            }
        }
        if let Some(error) = &check.error {
            self.literal(&format!("{call_id}:error"), error, None)?;
        }
        if let Some(error) = check
            .report
            .as_ref()
            .and_then(|report| report.error.as_ref())
        {
            self.literal(
                &format!("{call_id}:report-error"),
                &format!("Result details unavailable: {error}"),
                None,
            )?;
        }
        self.fold(start, &format!("{call_id}:tool"), true, NodeKind::Tool);
        if let Some(node) = &mut self.block[start].metadata.node {
            node.more = expanded && output.has_unloaded_output();
        }
        self.tool.insert(call_id.clone(), output);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn concrete_file_scopes_do_not_override_package_ownership_or_hide_runner_failures() -> Result<()>
    {
        let mut source = json!({
            "id":"command", "command":"runner", "state":"failed", "started_at_ms":1,
            "completed_at_ms":2,"exit_code":1,"error":null,"stdout":"unused","stderr":"unused",
            "output":"unused","output_bytes":0,"preview":"",
            "report":{"result":[{"file":"tests/pass.lua","state":"passed","duration_ms":1,
                "command":"nvim","output":""}],"error":null},
            "context":{"definition_id":"test","name":"Runner","area":".","reason":"selected",
                "matched_files":["src/one.rs","src/two.rs"],"category":"test","directory":".",
                "selection":{"kind":"files","files":["src/one.rs","src/two.rs"]},
                "resolved":null,"depends_on":[],"receipt":"unused",
                "invocation":{"inputs":{},"program":"runner","args":[],"directory":".",
                    "package":null,"files":["src/one.rs"],"cases":[],"discovery":null}}
        });
        let command: CommandCheck = serde_json::from_value(source.clone())?;
        assert_eq!(check_scope(&command).0, "src/one.rs");
        source["context"]["invocation"]["package"] =
            json!({"name":"core","directory":"core","manifest":"core/Cargo.toml"});
        let command: CommandCheck = serde_json::from_value(source)?;
        assert_eq!(check_scope(&command).0, "core");
        let run: CheckRun = serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":2,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":[command]
        }))?;
        let leaves = check_leaves(&run, CheckCategory::Test);
        assert_eq!(leaves.len(), 2);
        assert!(leaves[0].result.is_some() && leaves[1].result.is_none());
        assert_eq!(leaves[1].name.as_deref(), Some("Runner result"));
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
                "context":{"definition_id":category,"name":format!("{category} check"),"area":".","reason":"selected",
                    "matched_files":[file],"category":category,"directory":".","selection":{"kind":"files","files":[file]},
                    "resolved":null,"depends_on":[],"receipt":"unused"}
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
                run: run.clone(),
            },
            &WidthProfile::default(),
            false,
            &HashMap::new(),
            &HashMap::new(),
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

        let mut reported = run.clone();
        reported.commands.truncate(1);
        reported.commands[0].context.as_mut().unwrap().category = CheckCategory::Test;
        reported.commands[0].state = CheckState::Failed;
        reported.commands[0].report = Some(serde_json::from_value(json!({
            "result":[
                {"file":"one.lua","state":"passed","duration_ms":10,"command":"lua","output":""},
                {"file":"two.lua","state":"failed","duration_ms":20,"command":"lua","output":"failed"}
            ],"error":null
        }))?);
        let mut repaired = reported.commands[0].clone();
        repaired.id = "reported-retry".into();
        repaired.state = CheckState::Passed;
        let results = &mut repaired.report.as_mut().unwrap().result;
        results[1].state = CheckState::Passed;
        results.reverse();
        reported.commands.push(repaired);
        assert_eq!(check_summary(&reported), "2/2 passed · 2 attempts");

        let mut failed_run = run;
        failed_run.commands[0].state = CheckState::Failed;
        failed_run.commands[1].completed_at_ms = None;
        let failed_projection = project(
            &TimelineEntry::Checks {
                id: "run".into(),
                created_at_ms: 1,
                run: failed_run,
            },
            &WidthProfile::default(),
            false,
            &HashMap::new(),
            &HashMap::new(),
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
