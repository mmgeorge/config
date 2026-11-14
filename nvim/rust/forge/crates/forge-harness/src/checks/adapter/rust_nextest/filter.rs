use super::{
    super::{AdapterError, TestSelection},
    workspace::WorkspaceModel,
};
use std::collections::BTreeSet;

const FILTER_LIMIT: usize = 8 * 1024;

/// Resolve selections through bounded requests before compressing their exact execution filter.
pub(super) struct DiscoveryFilters {
    pub expressions: Vec<String>,
    pub complete_inventories: bool,
    pub widened: Option<String>,
}

/// Compile opaque item selections only inside the nextest adapter.
pub(super) fn compile(
    selection: &TestSelection,
    workspace: &WorkspaceModel,
    repository: &std::path::Path,
) -> Result<DiscoveryFilters, AdapterError> {
    let repository = dunce::simplified(repository);
    let mut terms = BTreeSet::new();
    for id in &selection.ids {
        let id = id.as_str();
        let term = if let Some(package) = id.strip_prefix("package:") {
            format!("package(={})", escape(package))
        } else if let Some((binary, test)) = id.split_once(' ') {
            if let Some(module) = test.strip_suffix("::") {
                format!(
                    "binary_id(={}) & test(/^{}::/)",
                    escape(binary),
                    regex::escape(module).replace('/', "\\/")
                )
            } else {
                format!("binary_id(={}) & test(={})", escape(binary), escape(test))
            }
        } else {
            format!("binary_id(={})", escape(id))
        };
        terms.insert(format!("({term})"));
    }
    let mut widened = None;
    for path in &selection.affected {
        let absolute = repository.join(path);
        let control = absolute.strip_prefix(&workspace.root).is_ok_and(|path| {
            matches!(
                path.to_str(),
                Some(
                    "Cargo.toml"
                        | "Cargo.lock"
                        | "rust-toolchain"
                        | "rust-toolchain.toml"
                        | "clippy.toml"
                        | ".clippy.toml"
                )
            ) || path.starts_with(".cargo")
                || path == std::path::Path::new(".config/nextest.toml")
        });
        let package = workspace
            .packages
            .iter()
            .filter(|package| absolute.starts_with(&package.directory))
            .max_by_key(|package| package.directory.components().count());
        if control || package.is_none() {
            widened = Some(format!("workspace-wide change: {path}"));
            break;
        }
        if let Some(package) = package {
            terms.insert(format!("rdeps(={})", escape(&package.name)));
        }
    }
    let all = selection.is_all() || widened.is_some();
    let expressions = if all {
        vec!["all()".into()]
    } else {
        chunks(terms)?
    };
    Ok(DiscoveryFilters {
        expressions,
        complete_inventories: all || selection.ids.iter().all(|id| !id.as_str().contains(' ')),
        widened,
    })
}

/// List complete eligible case inventories without admitting unrelated binaries.
pub(super) fn inventories<'a>(
    binaries: impl IntoIterator<Item = &'a String>,
) -> Result<Vec<String>, AdapterError> {
    chunks(
        binaries
            .into_iter()
            .map(|binary| format!("binary_id(={})", escape(binary))),
    )
}

/// Replace complete binary selections with a parent predicate using the runtime inventory.
pub(super) fn compress(
    binary: &str,
    selected: &BTreeSet<String>,
    available: &BTreeSet<String>,
) -> Result<String, AdapterError> {
    if !selected.is_subset(available) {
        return Err(AdapterError::Selection(format!(
            "nextest cases changed while resolving binary {binary}"
        )));
    }
    if selected == available {
        return Ok(format!("binary_id(={})", escape(binary)));
    }
    let cases = selected
        .iter()
        .map(|name| format!("test(={})", escape(name)))
        .collect::<Vec<_>>()
        .join(" | ");
    Ok(format!("binary_id(={}) & ({cases})", escape(binary)))
}

/// Enforce the execution bound after exact-case compression, never by broadening the selection.
pub(super) fn combine<'a>(
    expressions: impl IntoIterator<Item = &'a str>,
) -> Result<String, AdapterError> {
    let expression = expressions
        .into_iter()
        .map(|expression| format!("({expression})"))
        .collect::<Vec<_>>()
        .join(" | ");
    if expression.len() > FILTER_LIMIT {
        return Err(AdapterError::Protocol(
            "nextest resolved selection exceeds the 8 KiB filter limit; select fewer cases or complete parent items".into(),
        ));
    }
    Ok(expression)
}

/// Partition discovery work so an oversized authored selection can still prove it is a whole binary.
fn chunks(terms: impl IntoIterator<Item = String>) -> Result<Vec<String>, AdapterError> {
    let mut expressions = Vec::new();
    let mut current = String::new();
    for term in terms {
        if term.len() > FILTER_LIMIT {
            return Err(AdapterError::Protocol(
                "nextest item exceeds the 8 KiB filter limit".into(),
            ));
        }
        if !current.is_empty() && current.len() + 3 + term.len() > FILTER_LIMIT {
            expressions.push(std::mem::take(&mut current));
        }
        if !current.is_empty() {
            current.push_str(" | ");
        }
        current.push_str(&term);
    }
    if !current.is_empty() {
        expressions.push(current);
    }
    Ok(expressions)
}

/// Escape exact matchers independently of shell quoting.
fn escape(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace(')', "\\)")
        .replace(',', "\\,")
        .replace('/', "\\/")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Root package ownership must not narrow workspace controls or sibling dependencies.
    #[test]
    fn workspace_controls_widen_before_package_ownership() {
        let workspace = WorkspaceModel {
            root: "/repo".into(),
            packages: vec![super::super::workspace::Package {
                id: "root".into(),
                name: "root".into(),
                directory: "/repo".into(),
                targets: vec![],
            }],
        };
        for path in [
            "Cargo.toml",
            "Cargo.lock",
            "rust-toolchain",
            "rust-toolchain.toml",
            ".cargo/config.toml",
            ".config/nextest.toml",
            "clippy.toml",
        ] {
            let result = compile(
                &TestSelection {
                    ids: vec![],
                    affected: vec![path.into()],
                },
                &workspace,
                std::path::Path::new("/repo"),
            )
            .unwrap();
            assert_eq!(result.expressions, ["all()"]);
            assert!(result.widened.unwrap().contains(path));
        }
        let result = compile(
            &TestSelection {
                ids: vec![],
                affected: vec!["fixtures/input.txt".into()],
            },
            &workspace,
            std::path::Path::new("/repo"),
        )
        .unwrap();
        assert_eq!(result.expressions, ["rdeps(=root)"]);
    }

    /// Large complete selections compress while partial selections retain the execution limit.
    #[test]
    fn compression_uses_runtime_inventory_without_widening_partial_cases() {
        let available: BTreeSet<_> = (0..1000)
            .map(|index| format!("module::case_{index}"))
            .collect();
        let full = compress("package", &available, &available).unwrap();
        assert_eq!(full, "binary_id(=package)");
        assert!(combine([full.as_str()]).is_ok());
        let partial: BTreeSet<_> = available.iter().take(999).cloned().collect();
        let partial = compress("package", &partial, &available).unwrap();
        assert!(combine([partial.as_str()]).is_err());
        let requests = chunks(
            available
                .iter()
                .map(|name| format!("test(={})", escape(name))),
        )
        .unwrap();
        assert!(requests.len() > 1);
        assert!(requests.iter().all(|request| request.len() <= 8192));
        let stale = BTreeSet::from(["missing".into()]);
        assert!(compress("package", &stale, &available).is_err());
        assert!(combine(["x".repeat(FILTER_LIMIT - 2).as_str()]).is_ok());
        assert!(combine(["x".repeat(FILTER_LIMIT - 1).as_str()]).is_err());
        assert!(chunks(["x".repeat(FILTER_LIMIT + 1)]).is_err());
        assert!(chunks(Vec::new()).unwrap().is_empty());
    }
}
