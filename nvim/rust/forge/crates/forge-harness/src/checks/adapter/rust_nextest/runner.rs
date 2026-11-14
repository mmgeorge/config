use super::super::{
    AdapterContext, AdapterError, CheckAdapter, DiscoverMode, DiscoverRequest, Discovery,
    ProcessSpec, RunExit, TestAdapter, TestSelection,
    events::{TestEvent, TestEventSink, TestPhase},
    process::ProcessOutput,
};
use super::{
    filter, static_index,
    targets::BuildArguments,
    translate,
    version::{MESSAGE_FORMAT, MESSAGE_FORMAT_VERSION, NextestVersion},
    workspace::{self, Case, WorkspaceModel},
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

const OUTPUT_LIMIT: usize = 32 * 1024 * 1024;

/// Configure Cargo compilation and nextest's independently named execution profile.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RustNextestOptions {
    /// Cargo compilation profile, shared by exact listing and execution.
    #[serde(default = "dev")]
    pub cargo_profile: String,
    /// Nextest execution profile and its default-filter policy.
    #[serde(default = "default_profile")]
    pub nextest_profile: String,
    /// Refuse lockfile changes during metadata, listing, and execution.
    #[serde(default)]
    pub locked: bool,
    /// Individual Cargo feature names enabled for listing and execution.
    #[serde(default)]
    pub features: Vec<String>,
    /// Enable all Cargo features for listing and execution.
    #[serde(default)]
    pub all_features: bool,
    /// Extra build arguments; selection and event-format options are adapter-owned.
    #[serde(default)]
    pub cargo_args: Vec<String>,
}

impl Default for RustNextestOptions {
    /// Create default profiles without adding target or feature restrictions.
    fn default() -> Self {
        Self {
            cargo_profile: dev(),
            nextest_profile: default_profile(),
            locked: false,
            features: Vec::new(),
            all_features: false,
            cargo_args: Vec::new(),
        }
    }
}

/// Retain every planned case while separating ambiguous native binary names for execution.
struct ExactPlan {
    items: Vec<super::super::TestItem>,
    groups: Vec<ExecutionGroup>,
}

/// Own one run's filter and native event identities.
struct ExecutionGroup {
    expression: String,
    plan: translate::PlanIndex,
}

/// Implement the test contract with Cargo metadata and owned nextest subprocesses.
pub struct RustNextestAdapter {
    options: RustNextestOptions,
    build: BuildArguments,
}

impl RustNextestAdapter {
    /// Validate fixed build options before admitting any subprocess.
    pub fn new(options: RustNextestOptions) -> Result<Self, AdapterError> {
        for profile in [&options.cargo_profile, &options.nextest_profile] {
            if profile.is_empty()
                || !profile
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'-')
            {
                return Err(AdapterError::Protocol(
                    "nextest profile names must contain letters, digits, underscores, or hyphens"
                        .into(),
                ));
            }
        }
        if options.features.iter().any(|feature| {
            feature.is_empty()
                || feature
                    .chars()
                    .any(|ch| ch.is_control() || ch.is_whitespace() || ch == ',')
        }) {
            return Err(AdapterError::Protocol(
                "nextest features must be nonempty individual feature names".into(),
            ));
        }
        let build = BuildArguments::parse(&options.cargo_args)?;
        Ok(Self { options, build })
    }

    /// Construct matching list/run options, including explicit workspace selection.
    fn arguments(&self, operation: &str, filter: &str) -> Vec<String> {
        let mut args = vec![
            "nextest".into(),
            operation.into(),
            "--workspace".into(),
            "--cargo-profile".into(),
            self.options.cargo_profile.clone(),
            "--profile".into(),
            self.options.nextest_profile.clone(),
            "-E".into(),
            filter.into(),
        ];
        if self.options.locked {
            args.push("--locked".into());
        }
        if self.options.all_features {
            args.push("--all-features".into());
        }
        if !self.options.features.is_empty() {
            args.extend(["--features".into(), self.options.features.join(",")]);
        }
        args.extend(self.build.arguments().iter().cloned());
        args
    }

    /// Load workspace roots without compiling code or retrying offline failures online.
    async fn workspace(
        &self,
        context: &AdapterContext<'_>,
    ) -> Result<WorkspaceModel, AdapterError> {
        let mut args = vec![
            "metadata".into(),
            "--format-version".into(),
            "1".into(),
            "--no-deps".into(),
            "--offline".into(),
        ];
        if self.options.locked {
            args.push("--locked".into());
        }
        let (output, result) = capture(context, &args).await?;
        if !result.exit.success {
            return Err(command_failure("offline cargo metadata".into(), &result));
        }
        WorkspaceModel::parse(&output, &self.build, &|path| std::fs::read_to_string(path))
    }

    /// Decode one bounded native list document after its process and output are collected.
    async fn list(
        &self,
        context: &AdapterContext<'_>,
        version: &NextestVersion,
        expression: &str,
    ) -> Result<nextest_metadata::TestListSummary, AdapterError> {
        let mut args = self.arguments("list", expression);
        args.extend(["--message-format".into(), "json".into()]);
        let (output, result) = capture(context, &args).await?;
        if !result.exit.success {
            return Err(command_failure(version.operation("list"), &result));
        }
        nextest_metadata::TestListSummary::parse_json(&output).map_err(|error| {
            AdapterError::Protocol(format!("{}: {error}", version.operation("list")))
        })
    }

    /// Resolve case selections before compressing execution filters against their exact inventories.
    async fn exact(
        &self,
        context: &AdapterContext<'_>,
        version: &NextestVersion,
        workspace: &WorkspaceModel,
        filters: &filter::DiscoveryFilters,
    ) -> Result<ExactPlan, AdapterError> {
        // Resolve every requested case without exceeding a native command's filter bound.
        let mut selected = BTreeMap::<String, BTreeSet<String>>::new();
        let mut suites = BTreeMap::new();
        for expression in &filters.expressions {
            for (binary, suite) in self.list(context, version, expression).await?.rust_suites {
                let mut matches = BTreeSet::new();
                for (name, case) in &suite.test_cases {
                    if case.filter_match.is_match() {
                        matches.insert(name.as_str().to_owned());
                    }
                }
                if matches.is_empty() {
                    continue;
                }
                selected
                    .entry(binary.as_str().to_owned())
                    .or_default()
                    .extend(matches);
                if filters.complete_inventories {
                    suites.insert(binary, suite);
                }
            }
        }

        // Case and module filters need full inventories; parent selections already contain them.
        if !filters.complete_inventories {
            for expression in filter::inventories(selected.keys())? {
                for (binary, suite) in self.list(context, version, &expression).await?.rust_suites {
                    if suite
                        .test_cases
                        .values()
                        .any(|case| case.filter_match.is_match())
                    {
                        suites.insert(binary, suite);
                    }
                }
            }
        }
        let locations = static_locations(workspace)?;
        let mut cases = Vec::new();
        let mut groups = Vec::new();
        let mut wire_names = BTreeSet::new();
        let mut collides = false;
        for (binary, suite) in suites {
            let Some(selected_cases) = selected.remove(binary.as_str()) else {
                continue;
            };
            let available = suite
                .test_cases
                .iter()
                .filter(|(_, case)| case.filter_match.is_match())
                .map(|(name, _)| name.as_str().to_owned())
                .collect();
            let expression = filter::compress(binary.as_str(), &selected_cases, &available)?;
            let mut names = BTreeMap::new();
            let mut ignored = BTreeSet::new();
            for (name, case) in suite.test_cases {
                let wire = format!(
                    "{}::{}${}",
                    suite.package_name,
                    suite.binary.binary_name,
                    name.as_str()
                );
                collides |= !wire_names.insert(wire.clone());
                if !case.filter_match.is_match() || !selected_cases.contains(name.as_str()) {
                    if case.ignored {
                        ignored.insert(wire);
                    }
                    continue;
                }
                let id = workspace::item_id(format!("{} {}", binary.as_str(), name.as_str()))?;
                names.insert(wire, id);
                let location =
                    locations.get(&(binary.as_str().to_owned(), name.as_str().to_owned()));
                let path = location
                    .and_then(|location| workspace::relative(&location.path, context.workspace));
                let line = path.as_ref().and(location.map(|location| location.line));
                cases.push(Case {
                    package: suite.binary.package_id.clone(),
                    binary: binary.as_str().to_owned(),
                    label: suite.binary.binary_name.clone(),
                    name: name.as_str().to_owned(),
                    path,
                    line,
                });
            }
            groups.push(ExecutionGroup {
                expression,
                plan: translate::PlanIndex::new(names, ignored),
            });
        }
        if let Some(binary) = selected.keys().next() {
            return Err(AdapterError::Selection(format!(
                "nextest binary disappeared while resolving selection: {binary}"
            )));
        }
        // Bound the entire selection even when native-name collisions require separate runs.
        let expression = filter::combine(groups.iter().map(|group| group.expression.as_str()))?;
        // Native names omit target kind; split collisions before launching their processes.
        if !collides {
            let mut names = BTreeMap::new();
            let mut ignored = BTreeSet::new();
            for group in groups {
                names.extend(group.plan.names);
                ignored.extend(group.plan.ignored);
            }
            groups = vec![ExecutionGroup {
                expression,
                plan: translate::PlanIndex::new(names, ignored),
            }];
        } else {
            groups.retain(|group| !group.plan.names.is_empty());
        }
        Ok(ExactPlan {
            items: workspace.tree(cases)?,
            groups,
        })
    }
}

impl CheckAdapter for RustNextestAdapter {
    /// Get the catalog key owned by this adapter.
    fn kind(&self) -> &'static str {
        "rust_nextest"
    }
    /// Include all build and execution options in evidence identity.
    fn evidence_identity(&self) -> serde_json::Value {
        serde_json::json!({"kind":self.kind(),"options":self.options})
    }
}

#[async_trait::async_trait]
impl TestAdapter for RustNextestAdapter {
    /// Discover static declarations or build an exact list without executing test cases.
    async fn discover(
        &self,
        context: &AdapterContext<'_>,
        request: &DiscoverRequest,
    ) -> Result<Discovery, AdapterError> {
        let workspace = self.workspace(context).await?;
        let items = match request.mode {
            DiscoverMode::Static => {
                let locations = static_locations(&workspace)?;
                let mut cases = Vec::new();
                for package in &workspace.packages {
                    for target in &package.targets {
                        for ((binary, _), location) in locations
                            .iter()
                            .filter(|((binary, _), _)| binary == &target.binary)
                        {
                            let path = workspace::relative(&location.path, context.workspace);
                            let line = path.as_ref().map(|_| location.line);
                            cases.push(Case {
                                package: package.id.clone(),
                                binary: binary.clone(),
                                label: target.name.clone(),
                                name: location.name.clone(),
                                path,
                                line,
                            });
                        }
                    }
                }
                workspace.tree(cases)?
            }
            DiscoverMode::Exact => {
                let version = installed_version(context).await?;
                let selection = TestSelection {
                    ids: request.within.clone(),
                    affected: Vec::new(),
                };
                let filter = filter::compile(&selection, &workspace, context.workspace)?;
                self.exact(context, &version, &workspace, &filter)
                    .await?
                    .items
            }
        };
        Discovery::new(items, request.mode)?.scoped(request)
    }

    /// Announce exact cases before running them under the owner's cancellation and deadline.
    async fn run(
        &self,
        context: &AdapterContext<'_>,
        selection: &TestSelection,
        events: &mut TestEventSink,
    ) -> Result<RunExit, AdapterError> {
        // Publish the exact case set before starting any test execution.
        events.accept(TestEvent::Phase {
            name: TestPhase::Build,
            message: None,
        })?;
        let workspace = self.workspace(context).await?;
        let version = installed_version(context).await?;
        let filter = filter::compile(selection, &workspace, context.workspace)?;
        let ExactPlan { items, groups } =
            self.exact(context, &version, &workspace, &filter).await?;
        events.accept(TestEvent::Plan {
            cases: items
                .iter()
                .filter(|item| item.case)
                .map(|item| item.id.clone())
                .collect(),
            items,
            widened: filter.widened,
        })?;
        events.accept(TestEvent::Phase {
            name: TestPhase::Run,
            message: None,
        })?;
        let mut result = RunExit {
            code: Some(0),
            success: true,
        };
        let mut execution_failure = None;
        // Retain every group outcome while preserving the first process failure.
        for ExecutionGroup {
            expression,
            mut plan,
        } in groups
        {
            if plan.names.is_empty() {
                continue;
            }
            let mut args = self.arguments("run", &expression);
            args.extend([
                "--message-format".into(),
                MESSAGE_FORMAT.into(),
                "--message-format-version".into(),
                MESSAGE_FORMAT_VERSION.into(),
                "--no-fail-fast".into(),
                "--retries".into(),
                "0".into(),
            ]);
            let env = [("NEXTEST_EXPERIMENTAL_LIBTEST_JSON".into(), "1".into())];
            let mut process = context
                .processes
                .spawn(ProcessSpec {
                    program: "cargo",
                    args: &args,
                    directory: context.directory,
                    env: &env,
                    stdin: None,
                    log_stdout: true,
                })
                .await
                .map_err(cargo_launch_error)?;
            let translated = async {
                while let Some(line) = process.next_line().await? {
                    for event in plan.translate(&line, &workspace.root, context.workspace)? {
                        events.accept(event)?;
                    }
                }
                Ok::<_, AdapterError>(())
            }
            .await;
            if let Err(error) = translated {
                process.cancel();
                let _ = process.wait().await;
                return Err(match error {
                    AdapterError::Protocol(message) => {
                        AdapterError::Protocol(format!("{}: {message}", version.operation("run")))
                    }
                    error => error,
                });
            }
            let output = process.collect().await?;
            if !plan.complete() {
                execution_failure.get_or_insert_with(|| {
                    if !output.exit.success {
                        command_failure(version.operation("run"), &output)
                    } else {
                        AdapterError::Protocol(format!(
                            "{} exited before every planned case finished; {}",
                            version.operation("run"),
                            output.stderr()
                        ))
                    }
                });
            }
            let exit = output.exit;
            if !exit.success && result.success {
                result = exit;
            }
        }
        // Certify completion only after every owned process and terminal case is collected.
        if let Some(error) = execution_failure {
            return Err(error);
        }
        events.accept(TestEvent::Complete)?;
        Ok(result)
    }
}

/// Capture one bounded document while retaining all native output in the command log.
async fn capture(
    context: &AdapterContext<'_>,
    args: &[String],
) -> Result<(String, ProcessOutput), AdapterError> {
    let mut process = context
        .processes
        .spawn(ProcessSpec {
            program: "cargo",
            args,
            directory: context.directory,
            env: &[],
            stdin: None,
            log_stdout: true,
        })
        .await
        .map_err(cargo_launch_error)?;
    let mut output = String::new();
    let read = async {
        while let Some(line) = process.next_line().await? {
            if output.len().saturating_add(line.len()).saturating_add(1) > OUTPUT_LIMIT {
                return Err(AdapterError::Protocol(
                    "nextest metadata exceeds 32 MiB".into(),
                ));
            }
            output.push_str(&line);
            output.push('\n');
        }
        Ok::<_, AdapterError>(())
    }
    .await;
    if let Err(error) = read {
        process.cancel();
        let _ = process.wait().await;
        return Err(error);
    }
    Ok((output, process.collect().await?))
}

/// Verify the executable once per exact operation without adding work to static discovery.
async fn installed_version(context: &AdapterContext<'_>) -> Result<NextestVersion, AdapterError> {
    let (stdout, output) = capture(context, &["nextest".into(), "--version".into()]).await?;
    if !output.exit.success {
        return Err(command_failure(
            "cargo nextest --version; install with `cargo install cargo-nextest --locked`".into(),
            &output,
        ));
    }
    NextestVersion::parse(&stdout)
}

/// Preserve a failed invocation's own diagnostics rather than referring only to a mixed log.
fn command_failure(command: String, output: &ProcessOutput) -> AdapterError {
    let stderr = output.stderr();
    AdapterError::Command {
        command,
        code: output.exit.code,
        stderr: if stderr.trim().is_empty() {
            "no stderr; inspect retained stdout in the command log".into()
        } else {
            stderr
        },
    }
}

/// Keep the launch cause while identifying Cargo and its installation command.
fn cargo_launch_error(error: AdapterError) -> AdapterError {
    match error {
        AdapterError::Launch(source) => AdapterError::ToolLaunch {
            tool: "cargo",
            install: "rustup toolchain install stable",
            source,
        },
        error => error,
    }
}

/// Index each target once per operation without sharing stale source results.
fn static_locations(
    workspace: &WorkspaceModel,
) -> Result<BTreeMap<(String, String), static_index::StaticTest>, AdapterError> {
    let mut locations = BTreeMap::new();
    for package in &workspace.packages {
        for target in &package.targets {
            for test in
                static_index::index_target(&target.source, &|path| std::fs::read_to_string(path))?
            {
                locations
                    .entry((target.binary.clone(), test.name.clone()))
                    .or_insert(test);
            }
        }
    }
    Ok(locations)
}

/// Get Cargo's default development profile.
fn dev() -> String {
    "dev".into()
}
/// Get nextest's default execution profile.
fn default_profile() -> String {
    "default".into()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Retain the operating-system launch cause and actionable Cargo installation guidance.
    #[test]
    fn missing_cargo_preserves_launch_cause() {
        let error = cargo_launch_error(AdapterError::Launch(std::io::Error::from(
            std::io::ErrorKind::NotFound,
        )));
        assert!(
            error
                .to_string()
                .contains("rustup toolchain install stable")
        );
        let AdapterError::ToolLaunch { tool, source, .. } = error else {
            panic!("launch error lost its typed cause");
        };
        assert_eq!(tool, "cargo");
        assert_eq!(source.kind(), std::io::ErrorKind::NotFound);
    }

    /// Reject argument injection and configuration that would desynchronize list and run.
    #[test]
    fn options_reject_selection_and_protocol_overrides() {
        for argument in [
            "-E",
            "--filterset=all()",
            "--package=other",
            "--workspace",
            "--message-format=json",
            "--cargo-profile=release",
            "--no-tests=pass",
            "case_name",
            "--",
            "--run-ignored=all",
            "--retries=2",
        ] {
            let options = RustNextestOptions {
                cargo_args: vec![argument.into()],
                ..Default::default()
            };
            assert!(
                RustNextestAdapter::new(options).is_err(),
                "accepted {argument}"
            );
        }
        let adapter = RustNextestAdapter::new(RustNextestOptions {
            locked: true,
            features: vec!["feature".into()],
            cargo_args: vec!["--lib".into(), "--target=x86_64-pc-windows-msvc".into()],
            ..Default::default()
        })
        .unwrap();
        let list = adapter.arguments("list", "all()");
        let run = adapter.arguments("run", "all()");
        assert_eq!(&list[2..], &run[2..]);
    }
}
