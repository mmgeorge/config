use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// Maximum number of structural records in one discovery or run.
pub const ITEM_LIMIT: usize = 100_000;

/// Describe a rejected adapter request or event stream.
#[derive(Debug, thiserror::Error)]
pub enum AdapterError {
    /// An adapter cannot implement the requested operation.
    #[error("adapter operation is not supported: {0}")]
    Unsupported(String),
    /// An adapter violated its category contract.
    #[error("adapter protocol error: {0}")]
    Protocol(String),
    /// An authored test selection cannot be resolved unambiguously.
    #[error("test selection could not be resolved: {0}")]
    Selection(String),
    /// An owned runner could not be launched.
    #[error("launch adapter process: {0}")]
    Launch(#[source] std::io::Error),
    /// A required executable could not start; preserve its cause and installation guidance.
    #[error("launch {tool}: {source}; install with `{install}`")]
    ToolLaunch {
        tool: &'static str,
        install: &'static str,
        #[source]
        source: std::io::Error,
    },
    /// A native invocation failed with retained diagnostic output.
    #[error("{command} failed (exit {code:?}):\n{stderr}")]
    Command {
        command: String,
        code: Option<i32>,
        stderr: String,
    },
    /// Structured output could not be retained in the command directory.
    #[error("retain adapter output: {0}")]
    Storage(#[source] std::io::Error),
    /// A running process failed during communication or collection.
    #[error("adapter process: {0}")]
    Process(#[source] std::io::Error),
    /// A supervised command exceeded the owner's absolute deadline.
    #[error("adapter command exceeded its execution deadline")]
    Timeout,
    /// The owner cancelled execution.
    #[error("adapter run cancelled")]
    Cancelled,
}

/// Validate a literal executable and argument prefix before admitting an adapter.
pub(super) fn validate_command(name: &str, command: &[String]) -> Result<(), AdapterError> {
    if command
        .first()
        .is_none_or(|program| program.trim().is_empty())
        || command.iter().any(|argument| argument.contains('\0'))
    {
        return Err(AdapterError::Protocol(format!(
            "{name} requires a nonempty executable and NUL-free arguments"
        )));
    }
    Ok(())
}

/// Identify an adapter-owned test item without exposing ID syntax to Harness.
#[derive(
    Clone, Debug, Deserialize, Eq, Hash, JsonSchema, Ord, PartialEq, PartialOrd, Serialize,
)]
#[serde(try_from = "String", into = "String")]
pub struct TestItemId(String);

impl TestItemId {
    /// Get the adapter's printable identifier for transport and display.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get a compact display suffix without changing the opaque execution identity.
    pub(crate) fn display_name(&self) -> &str {
        self.0
            .trim_end_matches("::")
            .rsplit("::")
            .next()
            .unwrap_or(&self.0)
            .rsplit(['/', '\\'])
            .next()
            .unwrap_or(&self.0)
    }
}

impl TryFrom<String> for TestItemId {
    type Error = AdapterError;

    /// Validate identifiers at both construction and deserialization boundaries.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() || value.len() > 1024 || value.chars().any(char::is_control) {
            return Err(AdapterError::Protocol(
                "test item ID must contain 1–1024 bytes without control characters".into(),
            ));
        }
        Ok(Self(value))
    }
}

impl From<TestItemId> for String {
    /// Transfer an identifier into its transport representation.
    fn from(value: TestItemId) -> Self {
        value.0
    }
}

/// Describe one case or group in an adapter's test tree.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct TestItem {
    pub id: TestItemId,
    #[serde(default)]
    pub parent: Option<TestItemId>,
    pub label: String,
    #[serde(default)]
    pub case: bool,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub line: Option<u32>,
}

/// Restrict the work allowed while discovering tests.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DiscoverMode {
    /// Discover without building or invoking test binaries.
    #[default]
    Static,
    /// Build and list tests when needed, without executing cases.
    Exact,
}

/// Select intersecting subtrees and source files for discovery.
#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DiscoverRequest {
    #[serde(default)]
    pub within: Vec<TestItemId>,
    #[serde(default)]
    pub files: Vec<String>,
    #[serde(default)]
    pub mode: DiscoverMode,
}

/// Request discovery from one configured test adapter.
#[derive(Clone, Debug, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct CheckDiscoverRequest {
    pub check: String,
    #[serde(default)]
    pub within: Vec<TestItemId>,
    #[serde(default)]
    pub files: Vec<String>,
    #[serde(default)]
    pub mode: DiscoverMode,
}

/// Store a complete, validated tree returned by discovery.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawDiscovery")]
pub struct Discovery {
    items: Vec<TestItem>,
    mode: DiscoverMode,
}

impl Discovery {
    /// Create discovery results after validating all tree and source references.
    pub fn new(items: Vec<TestItem>, mode: DiscoverMode) -> Result<Self, AdapterError> {
        validate_tree(&items)?;
        Ok(Self { items, mode })
    }

    /// Get the discovered cases and their ancestor metadata.
    pub fn items(&self) -> &[TestItem] {
        &self.items
    }

    /// Get result accuracy, independent of permission to run build tools.
    pub fn mode(&self) -> DiscoverMode {
        self.mode
    }

    /// Limit selectable items to the requested intersection, retaining their ancestor metadata.
    pub fn scoped(self, request: &DiscoverRequest) -> Result<Self, AdapterError> {
        if request.mode == DiscoverMode::Exact && self.mode != DiscoverMode::Exact {
            return Err(AdapterError::Protocol(
                "exact discovery returned a static result".into(),
            ));
        }
        for path in &request.files {
            validate_path(path)?;
        }
        let indexed: BTreeMap<_, _> = self.items.iter().map(|item| (&item.id, item)).collect();
        let mut children: BTreeMap<&TestItemId, Vec<&TestItemId>> = BTreeMap::new();
        for item in &self.items {
            if let Some(parent) = &item.parent {
                children.entry(parent).or_default().push(&item.id);
            }
        }
        let mut selected = BTreeSet::new();
        let mut pending = request.within.iter().collect::<Vec<_>>();
        for id in &pending {
            if !indexed.contains_key(id) {
                return Err(AdapterError::Protocol(format!(
                    "unknown discovery item {}",
                    id.as_str()
                )));
            }
        }
        if pending.is_empty() {
            pending.extend(self.items.iter().map(|item| &item.id));
        }
        while let Some(id) = pending.pop() {
            if selected.insert(id)
                && let Some(descendants) = children.get(id)
            {
                pending.extend(descendants);
            }
        }
        if !request.files.is_empty() {
            selected.retain(|id| {
                indexed[id]
                    .path
                    .as_ref()
                    .is_some_and(|path| request.files.contains(path))
            });
        }
        let mut included = BTreeSet::new();
        for id in selected {
            let mut current = id;
            while included.insert(current) {
                let Some(parent) = &indexed[current].parent else {
                    break;
                };
                current = parent;
            }
        }
        let ids: BTreeSet<_> = included.into_iter().cloned().collect();
        Ok(Self {
            items: self
                .items
                .into_iter()
                .filter(|item| ids.contains(&item.id))
                .collect(),
            mode: self.mode,
        })
    }
}

impl TryFrom<RawDiscovery> for Discovery {
    type Error = AdapterError;

    /// Validate discovery loaded from an external adapter.
    fn try_from(raw: RawDiscovery) -> Result<Self, Self::Error> {
        let RawDiscovery { items, mode } = raw;
        Self::new(items, mode)
    }
}

/// Select item subtrees and affected paths; empty lists explicitly select all.
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TestSelection {
    #[serde(default)]
    pub ids: Vec<TestItemId>,
    #[serde(default)]
    pub affected: Vec<String>,
}

impl TestSelection {
    /// Determine whether the adapter request explicitly selects the whole suite.
    pub fn is_all(&self) -> bool {
        self.ids.is_empty() && self.affected.is_empty()
    }

    /// Validate paths before atomically sorting and deduplicating the selection.
    pub fn normalize(&mut self) -> Result<(), AdapterError> {
        for path in &self.affected {
            validate_path(path)?;
        }
        self.ids.sort();
        self.ids.dedup();
        self.affected.sort();
        self.affected.dedup();
        Ok(())
    }
}

/// Select paths for diagnostics and formatting; empty explicitly selects all.
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PathSelection {
    #[serde(default)]
    pub paths: Vec<String>,
}

impl PathSelection {
    /// Validate paths before atomically sorting and deduplicating the selection.
    pub fn normalize(&mut self) -> Result<(), AdapterError> {
        for path in &self.paths {
            validate_path(path)?;
        }
        self.paths.sort();
        self.paths.dedup();
        Ok(())
    }
}

/// Preserve both successful exit and signal termination from an owned process.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct RunExit {
    pub code: Option<i32>,
    pub success: bool,
}

#[derive(Deserialize)]
struct RawDiscovery {
    items: Vec<TestItem>,
    mode: DiscoverMode,
}

/// Validate portable repository-relative paths at an adapter boundary.
pub(super) fn validate_path(path: &str) -> Result<(), AdapterError> {
    if path.chars().any(char::is_control) {
        return Err(AdapterError::Protocol(
            "path contains control characters".into(),
        ));
    }
    super::super::config::relative_path(path)
        .map(|_| ())
        .map_err(|error| AdapterError::Protocol(format!("invalid path {path:?}: {error}")))
}

/// Reject ambiguous IDs and invalid parent graphs without recursive traversal.
pub(super) fn validate_tree(items: &[TestItem]) -> Result<(), AdapterError> {
    if items.len() > ITEM_LIMIT {
        return Err(AdapterError::Protocol(
            "test tree exceeds 100000 items".into(),
        ));
    }
    let mut indexed = BTreeMap::new();
    for item in items {
        if indexed.insert(&item.id, item).is_some() {
            return Err(AdapterError::Protocol(format!(
                "duplicate test item {}",
                item.id.as_str()
            )));
        }
        if let Some(path) = &item.path {
            validate_path(path)?;
        }
        if item.line == Some(0) || (item.line.is_some() && item.path.is_none()) {
            return Err(AdapterError::Protocol(format!(
                "invalid source location for {}",
                item.id.as_str()
            )));
        }
    }
    let mut visited = BTreeSet::new();
    for item in items {
        let mut chain = BTreeSet::new();
        let mut current = item;
        while !visited.contains(&current.id) {
            if !chain.insert(current.id.clone()) {
                return Err(AdapterError::Protocol(format!(
                    "cycle at test item {}",
                    current.id.as_str()
                )));
            }
            let Some(parent) = &current.parent else { break };
            current = indexed.get(parent).copied().ok_or_else(|| {
                AdapterError::Protocol(format!("missing parent {}", parent.as_str()))
            })?;
            if current.case {
                return Err(AdapterError::Protocol(format!(
                    "case {} is used as a parent",
                    current.id.as_str()
                )));
            }
        }
        visited.extend(chain);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Decode a tree through the production validation boundary.
    fn discovery(items: serde_json::Value) -> Result<Discovery, serde_json::Error> {
        serde_json::from_value(serde_json::json!({"mode":"static", "items": items}))
    }

    #[test]
    fn invalid_ids_are_rejected_during_deserialization() {
        for value in [String::new(), "a\nb".into(), "x".repeat(1025)] {
            assert!(serde_json::from_value::<TestItemId>(value.into()).is_err());
        }
    }

    /// Strip source and module prefixes only in display labels, retaining the executable ID.
    #[test]
    fn display_names_preserve_execution_ids() {
        for (source, expected) in [
            (
                "nvim/lua/forge/tests/integration/views/harness/harness.test.lua::skill_marker_aligns_with_commentary",
                "skill_marker_aligns_with_commentary",
            ),
            (
                "forge-harness buffer::status::tests::selected_check_tree",
                "selected_check_tree",
            ),
            ("tests/nested/example.test.lua", "example.test.lua"),
            ("tests\\nested\\example.test.lua", "example.test.lua"),
            ("plain_case", "plain_case"),
        ] {
            let id = TestItemId::try_from(source.to_owned()).unwrap();
            assert_eq!(id.display_name(), expected);
            assert_eq!(id.as_str(), source);
        }
    }

    #[test]
    fn discovery_rejects_invalid_graphs_and_locations() {
        for items in [
            serde_json::json!([{"id":"a","label":"a"},{"id":"a","label":"a"}]),
            serde_json::json!([{"id":"a","label":"a","parent":"missing"}]),
            serde_json::json!([{"id":"a","label":"a","parent":"b"},{"id":"b","label":"b","parent":"a"}]),
            serde_json::json!([{"id":"a","label":"a","case":true},{"id":"b","label":"b","parent":"a"}]),
            serde_json::json!([{"id":"a","label":"a","path":"../escape.rs"}]),
            serde_json::json!([{"id":"a","label":"a","line":1}]),
        ] {
            assert!(discovery(items).is_err());
        }
        assert!(discovery(serde_json::json!([
            {"id":"suite","label":"suite"},
            {"id":"case","label":"case","parent":"suite","case":true,"path":"src/lib.rs","line":1}
        ])).is_ok());
    }

    #[test]
    fn normalization_rejects_escape_paths_without_partial_mutation() {
        for path in ["../escape", "C:/escape", "/escape", "src\\escape", "a\nb"] {
            let mut selection = TestSelection {
                ids: vec![TestItemId::try_from("z".to_owned()).unwrap(); 2],
                affected: vec!["valid.rs".into(), path.into()],
            };
            let before = selection.clone();
            assert!(selection.normalize().is_err());
            assert_eq!(selection, before);
        }
    }

    #[test]
    fn discovery_intersects_subtrees_and_files_and_retains_ancestors() {
        let items = serde_json::json!([
            {"id":"root","label":"root"},
            {"id":"suite","label":"suite","parent":"root"},
            {"id":"first","label":"first","parent":"suite","case":true,"path":"a.lua"},
            {"id":"second","label":"second","parent":"suite","case":true,"path":"b.lua"},
            {"id":"other","label":"other","parent":"root","case":true,"path":"a.lua"}
        ]);
        let request = DiscoverRequest {
            within: vec![TestItemId::try_from("suite".to_owned()).unwrap()],
            files: vec!["a.lua".into()],
            mode: DiscoverMode::Static,
        };
        let scoped = discovery(items.clone()).unwrap().scoped(&request).unwrap();
        assert_eq!(
            scoped
                .items()
                .iter()
                .map(|item| item.id.as_str())
                .collect::<Vec<_>>(),
            ["root", "suite", "first"]
        );
        let missing = DiscoverRequest {
            within: vec![TestItemId::try_from("missing".to_owned()).unwrap()],
            ..request
        };
        assert!(discovery(items).unwrap().scoped(&missing).is_err());
    }

    #[test]
    fn exact_discovery_cannot_be_satisfied_by_static_results() {
        let request = DiscoverRequest {
            mode: DiscoverMode::Exact,
            ..DiscoverRequest::default()
        };
        assert!(
            discovery(serde_json::json!([]))
                .unwrap()
                .scoped(&request)
                .is_err()
        );
        assert!(
            Discovery::new(Vec::new(), DiscoverMode::Exact)
                .unwrap()
                .scoped(&request)
                .is_ok()
        );
    }
}
