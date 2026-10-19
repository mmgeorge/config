use super::{
    CheckCase, CheckOperation, CheckSelection,
    packages::{Package, PackageCatalog, PackageIdentity, PackageManager},
};
use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    path::Path,
    sync::atomic::{AtomicBool, Ordering},
    time::Duration,
};
use tokio::io::AsyncReadExt;

const BINDINGS: [&str; 8] = [
    "{file.path}",
    "{package.name}",
    "{package.directory}",
    "{package.manifest}",
    "{workspace.directory}",
    "{case.file}",
    "{case.name}",
    "{case.target_args}",
];

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a resolved direct invocation and the input changes that selected it.
pub struct CheckInvocation {
    /// Manifest and lockfile identities captured before authorization.
    pub inputs: BTreeMap<String, String>,
    /// Executable name or path, never interpolated shell source.
    pub program: String,
    /// Fully expanded process arguments.
    pub args: Vec<String>,
    /// Workspace-relative process working directory.
    pub directory: String,
    /// Owning package, when package discovery was requested.
    pub package: Option<PackageIdentity>,
    /// Trigger files contributing to this deduplicated invocation.
    pub files: Vec<String>,
    /// Exact cases selected by this invocation.
    pub cases: Vec<CheckCase>,
    /// Cargo arguments used to list and validate cases before execution.
    pub discovery: Option<Vec<String>>,
}

impl CheckInvocation {
    /// Formats concrete arguments for review and the existing permission coordinator.
    pub fn display(&self) -> String {
        display(&self.program, &self.args)
    }
}

/// Rejects unsupported bindings before publishing executable check configuration.
pub(super) fn validate(operation: &CheckOperation) -> Result<()> {
    let Some(args) = &operation.args else {
        ensure!(
            operation.package_manager.is_none(),
            "package_manager requires direct args"
        );
        return Ok(());
    };
    ensure!(
        !operation.command.contains(['{', '}']),
        "executable cannot contain bindings"
    );
    for argument in args {
        ensure!(!argument.contains('\0'), "invalid direct argument");
        let mut literal = argument.clone();
        for binding in BINDINGS {
            literal = literal.replace(binding, "");
        }
        ensure!(
            !literal.contains(['{', '}']),
            "unknown check argument binding: {argument}"
        );
        ensure!(
            !argument.contains("{case.target_args}") || argument == "{case.target_args}",
            "case.target_args must occupy a complete argument"
        );
        if argument.contains("{package.") || argument.contains("{case.target_args}") {
            ensure!(
                operation.package_manager.is_some(),
                "package bindings require package_manager"
            );
        }
    }
    Ok(())
}

/// Renders argv without treating its values as executable shell syntax.
pub(super) fn display(program: &str, args: &[String]) -> String {
    std::iter::once(program)
        .chain(args.iter().map(String::as_str))
        .map(|value| {
            if !value.is_empty()
                && value.chars().all(|character| {
                    character.is_ascii_alphanumeric() || "_./:-=".contains(character)
                })
            {
                value.to_owned()
            } else {
                format!("\"{}\"", value.replace('`', "``").replace('"', "`\""))
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[derive(Default)]
/// Shares package discovery across all definitions resolved for one request.
pub(super) struct InvocationResolver {
    catalog: BTreeMap<(PackageManager, String, Option<String>), PackageCatalog>,
}

impl InvocationResolver {
    /// Expands input changes to concrete commands and merges identical invocations.
    pub(super) async fn resolve(
        &mut self,
        workspace: &Path,
        directory: &str,
        operation: &CheckOperation,
        selection: &CheckSelection,
    ) -> Result<Vec<CheckInvocation>> {
        validate(operation)?;
        let args = operation
            .args
            .as_ref()
            .context("direct invocation requires args")?;
        let catalog = if let Some(manager) = operation.package_manager {
            let toolchain = (manager == PackageManager::Cargo)
                .then(|| args.first().filter(|argument| argument.starts_with('+')))
                .flatten();
            let key = (manager, directory.to_owned(), toolchain.cloned());
            if !self.catalog.contains_key(&key) {
                self.catalog.insert(
                    key.clone(),
                    PackageCatalog::discover(
                        workspace,
                        directory,
                        manager,
                        toolchain.map(String::as_str),
                    )
                    .await?,
                );
            }
            self.catalog.get(&key)
        } else {
            None
        };
        let needs_file = args.iter().any(|argument| argument.contains("{file.path}"));
        let needs_package = args.iter().any(|argument| argument.contains("{package."));
        if args
            .iter()
            .any(|argument| argument.contains("{package.name}"))
        {
            let mut names = std::collections::BTreeSet::new();
            for package in &catalog.context("package context is unavailable")?.packages {
                if let Some(name) = &package.identity.name {
                    ensure!(
                        names.insert(name),
                        "Ambiguous package name {name}; use package.directory instead"
                    );
                }
            }
        }
        let needs_case = args.iter().any(|argument| argument.contains("{case."));
        if matches!(selection, CheckSelection::Cases { .. }) {
            ensure!(
                needs_case,
                "case-scoped direct commands must select case.name; use a scoped helper for other test runners"
            );
            ensure!(
                operation.package_manager == Some(PackageManager::Cargo)
                    && args.iter().any(|argument| argument == "{case.name}")
                    && args.iter().any(|argument| argument == "--exact"),
                "native case selection requires Cargo, case.name, and --exact; use a scoped helper for other test runners"
            );
            let package_bound = args
                .windows(2)
                .any(|pair| pair[0] == "--package" && pair[1] == "{package.name}")
                || args
                    .iter()
                    .any(|argument| argument == "--package={package.name}")
                || args
                    .windows(2)
                    .any(|pair| pair[0] == "--manifest-path" && pair[1] == "{package.manifest}");
            ensure!(
                package_bound && !args.iter().any(|argument| argument == "--workspace"),
                "native cases require an exact package or package manifest binding"
            );
            let targets = args
                .iter()
                .filter(|argument| {
                    matches!(
                        argument.as_str(),
                        "--lib" | "--bin" | "--test" | "{case.target_args}"
                    )
                })
                .count();
            ensure!(
                targets == 1
                    && !args.iter().any(|argument| matches!(
                        argument.as_str(),
                        "--tests" | "--bins" | "--all-targets" | "--examples" | "--doc"
                    )),
                "native cases require one explicit Cargo target"
            );
            for pair in args
                .windows(2)
                .filter(|pair| matches!(pair[0].as_str(), "--bin" | "--test"))
            {
                ensure!(
                    !pair[1].contains(['*', '?', '[', ']']),
                    "native case targets cannot use globs"
                );
            }
        }
        let mut result = Vec::new();
        match selection {
            CheckSelection::All if needs_file || needs_case => {
                bail!("file or case bindings require an explicit scoped operation")
            }
            CheckSelection::All if needs_package => {
                for package in &catalog.context("package context is unavailable")?.packages {
                    result.push(expand(
                        workspace,
                        directory,
                        operation,
                        Some(package),
                        None,
                        None,
                    )?);
                }
            }
            CheckSelection::Files { files } if needs_file || needs_package => {
                for file in files {
                    let package = catalog.map(|catalog| catalog.owner(file)).transpose()?;
                    if needs_package
                        && catalog.is_some_and(|catalog| catalog.packages.len() > 1)
                        && matches!(
                            file.as_str(),
                            "Cargo.toml"
                                | "Cargo.lock"
                                | "package.json"
                                | "package-lock.json"
                                | "pnpm-workspace.yaml"
                                | "pnpm-lock.yaml"
                        )
                    {
                        bail!(
                            "{file} affects the workspace. Configure a separate workspace-wide check for this path."
                        );
                    }
                    result.push(expand(
                        workspace,
                        directory,
                        operation,
                        package,
                        Some(file),
                        None,
                    )?);
                }
            }
            CheckSelection::Cases { cases } => {
                for case in cases {
                    let package = catalog
                        .context("case package context is unavailable")?
                        .owner(&case.file)?;
                    result.push(expand(
                        workspace,
                        directory,
                        operation,
                        Some(package),
                        Some(&case.file),
                        Some(case),
                    )?);
                }
            }
            _ => {
                let mut invocation = expand(workspace, directory, operation, None, None, None)?;
                if let CheckSelection::Files { files } = selection {
                    invocation.files = files.clone();
                }
                result.push(invocation);
            }
        }
        let mut unique: BTreeMap<(String, Vec<String>, String), CheckInvocation> = BTreeMap::new();
        for mut invocation in result {
            if let Some(catalog) = catalog {
                invocation.inputs = catalog.inputs.clone();
            }
            let key = (
                invocation.program.clone(),
                invocation.args.clone(),
                invocation.directory.clone(),
            );
            if let Some(existing) = unique.get_mut(&key) {
                existing.files.extend(invocation.files);
                existing.cases.extend(invocation.cases);
                existing.files.sort();
                existing.files.dedup();
                existing.cases.sort();
                existing.cases.dedup();
            } else {
                unique.insert(key, invocation);
            }
        }
        Ok(unique.into_values().collect())
    }
}

fn expand(
    workspace: &Path,
    directory: &str,
    operation: &CheckOperation,
    package: Option<&Package>,
    file: Option<&str>,
    case: Option<&CheckCase>,
) -> Result<CheckInvocation> {
    if let Some(case) = case {
        let mut selected = case.clone();
        let args = operation.args.as_ref().unwrap();
        if args.iter().any(|argument| argument == "--lib") {
            selected.target = Some("lib".into());
        }
        for pair in args.windows(2) {
            if pair[0] == "--bin" {
                selected.target = Some(format!("bin:{}", pair[1]));
            }
            if pair[0] == "--test" {
                selected.target = Some(format!("test:{}", pair[1]));
            }
        }
        target_args(package.context("case package is unavailable")?, &selected)?;
    }
    let mut bindings = BTreeMap::from([(
        "{workspace.directory}",
        workspace.to_string_lossy().into_owned(),
    )]);
    if let Some(package) = package {
        if let Some(name) = &package.identity.name {
            bindings.insert("{package.name}", name.clone());
        }
        bindings.insert(
            "{package.directory}",
            workspace
                .join(&package.identity.directory)
                .to_string_lossy()
                .into_owned(),
        );
        bindings.insert(
            "{package.manifest}",
            workspace
                .join(&package.identity.manifest)
                .to_string_lossy()
                .into_owned(),
        );
    }
    if let Some(file) = file {
        bindings.insert("{file.path}", file.into());
    }
    if let Some(case) = case {
        bindings.insert("{case.file}", case.file.clone());
        bindings.insert("{case.name}", case.name.clone());
    }
    let mut args = Vec::new();
    for argument in operation.args.as_ref().unwrap() {
        if argument == "{case.target_args}" {
            args.extend(target_args(
                package.context("case package is unavailable")?,
                case.context("case is unavailable")?,
            )?);
            continue;
        }
        let mut argument = argument.clone();
        for (binding, value) in &bindings {
            argument = argument.replace(binding, value);
        }
        ensure!(
            !BINDINGS.iter().any(|binding| argument.contains(binding)),
            "argument context is unavailable: {argument}"
        );
        args.push(argument);
    }
    let discovery = if let Some(case) = case {
        ensure!(
            !case.name.is_empty() && !case.name.starts_with('-'),
            "invalid Cargo test name"
        );
        let separator = args
            .iter()
            .position(|argument| argument == "--")
            .context("Cargo case arguments require -- before the test name")?;
        ensure!(
            args[separator + 1..].contains(&case.name),
            "Cargo case name must follow --"
        );
        let mut discovery = args[..=separator].to_vec();
        discovery.push("--list".into());
        discovery.push("--format=terse".into());
        Some(discovery)
    } else {
        None
    };
    Ok(CheckInvocation {
        inputs: BTreeMap::new(),
        program: operation.command.clone(),
        args,
        directory: directory.into(),
        package: package.map(|package| package.identity.clone()),
        files: file.map(|file| vec![file.into()]).unwrap_or_default(),
        cases: case.cloned().into_iter().collect(),
        discovery,
    })
}

fn target_args(package: &Package, case: &CheckCase) -> Result<Vec<String>> {
    let candidates: Vec<_> = package
        .targets
        .iter()
        .filter(|target| {
            target.test
                && target
                    .kind
                    .iter()
                    .any(|kind| matches!(kind.as_str(), "lib" | "proc-macro" | "bin" | "test"))
        })
        .filter(|target| {
            if let Some(requested) = &case.target {
                return target.kind.iter().any(|kind| {
                    requested == &format!("{kind}:{}", target.name)
                        || (requested == "lib" && matches!(kind.as_str(), "lib" | "proc-macro"))
                });
            }
            if case.file == target.src_path {
                return true;
            }
            let parent = Path::new(&target.src_path)
                .parent()
                .unwrap_or(Path::new("."));
            Path::new(&case.file).starts_with(parent)
        })
        .collect();
    let exact: Vec<_> = candidates
        .iter()
        .copied()
        .filter(|target| target.src_path == case.file)
        .collect();
    let candidates = if exact.is_empty() { candidates } else { exact };
    ensure!(
        candidates.len() == 1,
        "{}: test target is ambiguous or unavailable; set target to lib, bin:<name>, or test:<name> in the plan test inventory",
        case.file
    );
    let target = candidates[0];
    ensure!(
        target.harness,
        "{} uses a custom test harness; configure a runner-specific cases handler",
        target.name
    );
    if target
        .kind
        .iter()
        .any(|kind| matches!(kind.as_str(), "lib" | "proc-macro"))
    {
        Ok(vec!["--lib".into()])
    } else if target.kind.iter().any(|kind| kind == "bin") {
        Ok(vec!["--bin".into(), target.name.clone()])
    } else {
        Ok(vec!["--test".into(), target.name.clone()])
    }
}

#[derive(Debug)]
/// Retains unsuccessful discovery output independently of selector configuration errors.
pub(super) struct DiscoveryFailure {
    pub exit_code: Option<i32>,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl std::fmt::Display for DiscoveryFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "Check discovery exited {:?}\n{}\n{}",
            self.exit_code,
            String::from_utf8_lossy(&self.stdout),
            String::from_utf8_lossy(&self.stderr)
        )
    }
}

impl std::error::Error for DiscoveryFailure {}

impl DiscoveryFailure {
    /// Writes complete discovery logs under the command's final output identities.
    pub(super) fn record(&self, command: &mut super::CommandCheck) -> Result<()> {
        command.exit_code = self.exit_code;
        std::fs::write(&command.stdout, &self.stdout)?;
        std::fs::write(&command.stderr, &self.stderr)?;
        let mut output = self.stdout.clone();
        output.extend_from_slice(&self.stderr);
        std::fs::write(&command.output, &output)?;
        command.output_bytes = output.len() as u64;
        command.preview = String::from_utf8_lossy(&output).into_owned();
        Ok(())
    }
}

/// Capture discovery output while retaining process-tree cancellation ownership.
pub(super) async fn capture(
    program: &str,
    args: &[String],
    directory: &Path,
    cancelled: Option<&AtomicBool>,
    external: Option<&AtomicBool>,
    seconds: u64,
) -> Result<Vec<u8>> {
    let mut command = crate::process::command(program, args);
    command
        .current_dir(directory)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = crate::process::spawn_owned(command)
        .map_err(super::failure::CheckFailure::launch)
        .context("start check discovery")?;
    let mut stdout = child.stdout().take().context("discovery stdout missing")?;
    let mut stderr = child.stderr().take().context("discovery stderr missing")?;
    let work = async {
        let mut output = Vec::new();
        let mut errors = Vec::new();
        let (_, _, status) = tokio::try_join!(
            stdout.read_to_end(&mut output),
            stderr.read_to_end(&mut errors),
            child.wait()
        )?;
        if !status.success() {
            return Err(DiscoveryFailure {
                exit_code: status.code(),
                stdout: output,
                stderr: errors,
            }
            .into());
        }
        Ok(output)
    };
    tokio::pin!(work);
    let deadline = tokio::time::sleep(Duration::from_secs(seconds));
    tokio::pin!(deadline);
    let mut ticker = tokio::time::interval(Duration::from_millis(50));
    loop {
        tokio::select! {
            result = &mut work => return result,
            _ = &mut deadline => return Err(super::failure::CheckFailure::classified(super::CheckState::Blocked,
                anyhow::anyhow!("check discovery timed out after {seconds}s"))),
            _ = ticker.tick() => ensure!(!cancelled.is_some_and(|cancelled| cancelled.load(Ordering::Acquire)) && !external.is_some_and(|cancelled| cancelled.load(Ordering::Acquire)), "check discovery interrupted"),
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[tokio::test]
    async fn discovery_preserves_output_and_drains_errors_beyond_previous_limits() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let output = capture(
            "sh",
            &[
                "-c".into(),
                "printf '%08388609d' 0; printf '%01048577d' 0 >&2".into(),
            ],
            directory.path(),
            None,
            None,
            30,
        )
        .await?;
        assert_eq!(output.len(), 8 * 1024 * 1024 + 1);
        assert!(output.iter().all(|byte| *byte == b'0'));
        Ok(())
    }
}
