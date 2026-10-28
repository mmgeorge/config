use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

pub(super) const NAMES: &[&str] = &[
    "lsp_overview",
    "lsp_symbols",
    "lsp_definition",
    "lsp_context",
    "lsp_references",
    "lsp_calls",
    "lsp_paths",
    "lsp_diagnostics",
    "lsp_status",
];

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(super) enum Operation {
    Overview,
    Symbols,
    Definition,
    Context,
    References,
    Calls,
    Paths,
    Diagnostics,
    Status,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged, try_from = "TargetWire")]
pub(super) enum Target {
    Symbol {
        symbol: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<String>,
    },
    Position {
        path: String,
        line: std::num::NonZeroU32,
        column: std::num::NonZeroU32,
        #[serde(skip_serializing_if = "Option::is_none")]
        symbol: Option<String>,
    },
    LineSymbol {
        path: String,
        line: std::num::NonZeroU32,
        symbol: String,
    },
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetWire {
    path: Option<String>,
    symbol: Option<String>,
    line: Option<u32>,
    column: Option<u32>,
}

impl TryFrom<TargetWire> for Target {
    type Error = anyhow::Error;

    fn try_from(target: TargetWire) -> Result<Self> {
        ensure!(
            target
                .symbol
                .as_ref()
                .is_none_or(|symbol| !symbol.trim().is_empty()),
            "symbol must be nonempty"
        );
        ensure!(
            target.path.as_ref().is_none_or(|path| !path.is_empty()),
            "path must be nonempty"
        );
        let position = |value| {
            std::num::NonZeroU32::new(value)
                .ok_or_else(|| anyhow::anyhow!("positions are one-based"))
        };
        match (target.path, target.symbol, target.line, target.column) {
            (path, Some(symbol), None, None) => Ok(Self::Symbol { path, symbol }),
            (Some(path), symbol, Some(line), Some(column)) => Ok(Self::Position {
                path,
                symbol,
                line: position(line)?,
                column: position(column)?,
            }),
            (Some(path), Some(symbol), Some(line), None) => Ok(Self::LineSymbol {
                path,
                symbol,
                line: position(line)?,
            }),
            (_, _, None, Some(_)) => anyhow::bail!("column requires line"),
            (None, _, Some(_), _) => anyhow::bail!("line requires a file path"),
            _ => anyhow::bail!("target requires a symbol or path, line, and column"),
        }
    }
}

impl Target {
    pub(super) fn path_mut(&mut self) -> Option<&mut String> {
        match self {
            Self::Symbol { path, .. } => path.as_mut(),
            Self::Position { path, .. } | Self::LineSymbol { path, .. } => Some(path),
        }
    }
}
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Arguments {
    pub operation: Operation,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub from: Option<Target>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub to: Option<Target>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_depth: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_paths: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_requests: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub query: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direction: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub offset: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token_budget: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub depth: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub declaration_depth: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visibility: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scope: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_declaration: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_line: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub severity: Option<String>,
}

impl Arguments {
    pub fn parse(name: &str, mut value: Value) -> Result<Self> {
        ensure!(NAMES.contains(&name), "unknown LSP tool");
        let object = value
            .as_object_mut()
            .ok_or_else(|| anyhow::anyhow!("arguments must be an object"))?;
        ensure!(
            !object.contains_key("operation"),
            "operation is selected by the tool name"
        );
        object.insert("operation".into(), json!(name.trim_start_matches("lsp_")));
        let mut arguments: Self = serde_json::from_value(value)?;
        arguments.validate()?;
        if arguments.path.is_none()
            && matches!(
                arguments.operation,
                Operation::Overview | Operation::Symbols
            )
            && !(arguments.operation == Operation::Symbols && arguments.query.is_some())
        {
            arguments.path = Some(".".into());
        }
        Ok(arguments)
    }

    fn validate(&self) -> Result<()> {
        let position = matches!(
            self.operation,
            Operation::Definition | Operation::Context | Operation::References | Operation::Calls
        );
        if position {
            Target::try_from(TargetWire {
                path: self.path.clone(),
                symbol: self.symbol.clone(),
                line: self.line,
                column: self.column,
            })?;
        } else {
            ensure!(
                self.symbol.is_none() && self.line.is_none() && self.column.is_none(),
                "this tool does not accept a target"
            );
        }
        if self.operation == Operation::Paths {
            ensure!(self.from.is_some(), "paths requires from");
            ensure!(self.to.is_some(), "paths requires to");
            ensure!(
                self.path.is_none() && self.limit.is_none() && self.offset.is_none(),
                "paths uses endpoint targets without list pagination"
            );
            ensure!(
                self.max_depth.is_none_or(|value| (1..=8).contains(&value)),
                "max_depth must be 1-8"
            );
            ensure!(
                self.max_paths.is_none_or(|value| (1..=10).contains(&value)),
                "max_paths must be 1-10"
            );
            ensure!(
                self.max_requests
                    .is_none_or(|value| (2..=128).contains(&value)),
                "max_requests must be 2-128"
            );
        } else {
            ensure!(
                self.from.is_none()
                    && self.to.is_none()
                    && self.max_depth.is_none()
                    && self.max_paths.is_none()
                    && self.max_requests.is_none(),
                "path search options only apply to lsp_paths"
            );
        }
        ensure!(
            self.line.is_none_or(|value| value > 0) && self.column.is_none_or(|value| value > 0),
            "positions are one-based"
        );
        ensure!(
            self.path.as_ref().is_none_or(|value| !value.is_empty()),
            "path must be nonempty"
        );
        ensure!(
            self.query
                .as_ref()
                .is_none_or(|value| !value.trim().is_empty()),
            "query must be nonempty"
        );
        ensure!(
            self.query.is_none() || self.operation == Operation::Symbols,
            "query only applies to symbols"
        );
        ensure!(
            if self.operation == Operation::Calls {
                matches!(self.direction.as_deref(), Some("incoming" | "outgoing"))
            } else {
                self.direction.is_none()
            },
            "calls requires direction incoming or outgoing"
        );
        ensure!(
            self.kind.is_none()
                || self.operation == Operation::Definition
                    && matches!(
                        self.kind.as_deref(),
                        Some("definition" | "type" | "implementation")
                    ),
            "invalid definition kind"
        );
        ensure!(
            self.limit.is_none_or(|value| value > 0),
            "limit must be a positive integer"
        );
        ensure!(
            self.token_budget
                .is_none_or(|value| (1..=8000).contains(&value)),
            "token_budget must be 1-8000"
        );
        ensure!(
            self.depth.is_none_or(|value| value <= 8)
                && (self.depth.is_none()
                    || matches!(self.operation, Operation::Overview | Operation::Symbols)),
            "depth must be 0-8 for directory tools"
        );
        ensure!(
            self.declaration_depth
                .is_none_or(|value| (1..=8).contains(&value))
                && (self.declaration_depth.is_none() || self.operation == Operation::Overview),
            "declaration_depth must be 1-8 for overview"
        );
        ensure!(
            self.visibility.is_none()
                || matches!(self.operation, Operation::Overview | Operation::Symbols)
                    && matches!(
                        self.visibility.as_deref(),
                        Some("all" | "public" | "internal" | "private")
                    ),
            "invalid visibility filter"
        );
        ensure!(
            self.scope.is_none()
                || matches!(
                    self.operation,
                    Operation::Symbols
                        | Operation::References
                        | Operation::Calls
                        | Operation::Paths
                ) && matches!(self.scope.as_deref(), Some("workspace" | "all")),
            "invalid scope filter"
        );
        ensure!(
            self.include_declaration.is_none() || self.operation == Operation::References,
            "include_declaration only applies to references"
        );
        ensure!(
            self.include.as_ref().is_none_or(|values| values.len() <= 4
                && values.iter().all(|value| matches!(
                    value.as_str(),
                    "references" | "callers" | "callees" | "implementations"
                )))
                && (self.include.is_none() || self.operation == Operation::Context),
            "invalid context relationships"
        );
        ensure!(
            self.start_line.is_none_or(|value| value > 0)
                && (self.start_line.is_none() || self.operation == Operation::Context),
            "start_line only applies to context"
        );
        ensure!(
            self.severity.is_none()
                || self.operation == Operation::Diagnostics
                    && matches!(
                        self.severity.as_deref(),
                        Some("all" | "error" | "warning" | "information" | "hint")
                    ),
            "invalid diagnostics severity"
        );
        ensure!(
            !matches!(self.operation, Operation::Context | Operation::Status)
                || self.limit.is_none() && self.offset.is_none(),
            "this tool does not accept list pagination"
        );
        ensure!(
            self.operation != Operation::Status || self.token_budget.is_none(),
            "status has no content budget"
        );
        Ok(())
    }
}

pub(crate) fn definitions() -> Vec<super::super::ControlToolDefinition> {
    NAMES.iter().map(|&name| {
        let operation = name.trim_start_matches("lsp_");
        let position = matches!(operation, "definition" | "context" | "references" | "calls");
        let mut properties = serde_json::Map::from_iter([("path".into(), json!({"type":"string","minLength":1,"description":"Workspace-relative or authorized absolute path."}))]);
        let fields = &mut properties;
        let mut required = Vec::new();
        if position {
            fields.insert("line".into(), json!({"type":"integer","minimum":1,"description":"One-based source line from a tool result."}));
            fields.insert("column".into(), json!({"type":"integer","minimum":1,"description":"One-based Unicode code-point column of the symbol."}));
            fields.insert("symbol".into(), json!({"type":"string","minLength":1,"description":"Exact short or qualified name. With line, locate its spelling on that line. Ambiguity returns candidates."}));
        }
        if operation != "status" { fields.insert("token_budget".into(), json!({"type":"integer","minimum":1,"maximum":8000,"default":2000,"description":"Estimated output tokens, not an exact model tokenizer count."})); }
        if !matches!(operation, "context" | "status" | "paths") {
            fields.insert("limit".into(), json!({"type":"integer","minimum":1,"default":20,"description":"Requested maximum result count. Output budget and pagination still apply."}));
            fields.insert("offset".into(), json!({"type":"integer","minimum":0,"default":0,"description":"Use returned next_offset. Each page queries current source."}));
        }
        if matches!(operation, "overview" | "symbols") {
            fields.insert("depth".into(), json!({"type":"integer","minimum":0,"maximum":8,"default":2,"description":"Directory levels below path. Zero reads immediate files."}));
            fields.insert("visibility".into(), json!({"type":"string","enum":["all","public","internal","private"],"default":if operation == "overview" {"public"} else {"all"}}));
        }
        if matches!(operation, "symbols" | "references" | "calls" | "paths") { fields.insert("scope".into(), json!({"type":"string","enum":["workspace","all"],"default":"workspace"})); }
        let description = match operation {
            "overview" => { fields.insert("declaration_depth".into(), json!({"type":"integer","minimum":1,"maximum":8,"default":2})); "Map public declarations, members, and methods under a directory. Tree-sitter supplies source structure. Reuse returned positions with context and semantic tools." },
            "symbols" => { fields.insert("query".into(), json!({"type":"string","minLength":1,"description":"Optional workspace symbol name search. Omit to list declarations under path."})); "Find declarations by name or list a file/directory. Returns signatures, hierarchy, and reusable identifier positions, without bodies." },
            "definition" => { fields.insert("kind".into(), json!({"type":"string","enum":["definition","type","implementation"],"default":"definition"})); "Find a symbol's definition, its type declaration, or its implementations. Use a symbol name or source position. Name lookup selects a declaration, while a position resolves that occurrence." },
            "context" => { fields.insert("include".into(), json!({"type":"array","uniqueItems":true,"maxItems":4,"items":{"type":"string","enum":["references","callers","callees","implementations"]},"description":"Optional related signatures, at most five per relation, sharing the budget."})); fields.insert("start_line".into(), json!({"type":"integer","minimum":1,"description":"Continue at returned next_line, preserving the original target position."})); "Read the containing declaration, source docs, and full body within token_budget. Optional semantic relationships are signature summaries. Live unsaved source is included." },
            "references" => { fields.insert("include_declaration".into(), json!({"type":"boolean","default":false})); "Find semantic uses, grouped by file and enclosing signature, with one source line before and after each occurrence. Matching rows use >." },
            "calls" => { fields.insert("direction".into(), json!({"type":"string","enum":["incoming","outgoing"]})); required.push("direction"); "Find one level of incoming callers or outgoing callees. Returns a symbol tree with aligned definition locations. The root is the selected symbol. Incoming children call the root. The root calls outgoing children." },
            "paths" => {
                let target = json!({"type":"object","additionalProperties":false,"properties":{
                    "path":{"type":"string","minLength":1},
                    "symbol":{"type":"string","minLength":1},
                    "line":{"type":"integer","minimum":1},"column":{"type":"integer","minimum":1}
                },"description":"Use a symbol, optionally scoped to a file, or a file/line/column occurrence. A file/line/symbol selects a unique spelling on that line."});
                fields.remove("path");
                fields.insert("from".into(), target.clone()); fields.insert("to".into(), target);
                fields.insert("max_depth".into(), json!({"type":"integer","minimum":1,"maximum":8,"default":5,"description":"Maximum call edges in a returned path."}));
                fields.insert("max_paths".into(), json!({"type":"integer","minimum":1,"maximum":10,"default":3}));
                fields.insert("max_requests".into(), json!({"type":"integer","minimum":2,"maximum":128,"default":64,"description":"Maximum native LSP requests for the entire search, including endpoint lookup."}));
                required.extend(["from", "to"]);
                "Find bounded static call paths from one symbol to another. Returns complete aligned symbol trees. Cycles are excluded. Partial searches are explicit. This does not prove runtime reachability."
            },
            "diagnostics" => { fields.insert("severity".into(), json!({"type":"string","enum":["all","error","warning","information","hint"],"default":"all"})); "Read file diagnostics when path names a file, or aggregate workspace diagnostics when path is omitted or names a directory. Reports server coverage and freshness. Unsupported or failed servers leave explicit gaps. Empty results do not establish a successful project typecheck." },
            _ => "Inspect project language servers, roots, readiness, and supported capabilities. Normal queries already wait for initialization.",
        };
        super::super::ControlToolDefinition { name, description, input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}) }
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostics_accepts_file_and_workspace_requests() {
        assert!(
            Arguments::parse("lsp_diagnostics", json!({}))
                .unwrap()
                .path
                .is_none()
        );
        assert_eq!(
            Arguments::parse("lsp_diagnostics", json!({"path":"src/main.rs"}))
                .unwrap()
                .path
                .as_deref(),
            Some("src/main.rs")
        );
        let definition = definitions()
            .into_iter()
            .find(|definition| definition.name == "lsp_diagnostics")
            .unwrap();
        assert!(
            !definition.input_schema["required"]
                .as_array()
                .unwrap()
                .iter()
                .any(|field| field == "path")
        );
    }

    #[test]
    fn complete_paths_and_symbol_queries_have_no_byte_ceiling() {
        let path = format!("{}source.rs", "directory/".repeat(512));
        let symbol = "qualified_symbol_".repeat(512);
        let target = json!({"path":path,"symbol":symbol});
        assert!(Arguments::parse("lsp_context", target.clone()).is_ok());
        assert!(
            Arguments::parse("lsp_paths", json!({"from":target,"to":{"symbol":"finish"}})).is_ok()
        );
        assert!(Arguments::parse("lsp_symbols", json!({"path":path,"query":symbol})).is_ok());
        for definition in definitions() {
            let schema = serde_json::to_string(&definition.input_schema).unwrap();
            assert!(!schema.contains("maxLength"));
        }
    }

    #[test]
    fn requested_result_count_has_no_arbitrary_upper_bound() {
        for limit in [51, 1000, u64::MAX] {
            assert_eq!(
                Arguments::parse("lsp_symbols", json!({"limit":limit}))
                    .unwrap()
                    .limit,
                Some(limit)
            );
        }
        assert!(Arguments::parse("lsp_symbols", json!({"limit":0})).is_err());
        assert!(Arguments::parse("lsp_symbols", json!({"limit":-1})).is_err());
        assert!(Arguments::parse("lsp_symbols", json!({"limit":1.5})).is_err());
        let definition = definitions()
            .into_iter()
            .find(|definition| definition.name == "lsp_symbols")
            .unwrap();
        assert!(
            definition.input_schema["properties"]["limit"]
                .get("maximum")
                .is_none()
        );
    }

    #[test]
    fn dependency_symbol_search_has_no_implicit_workspace_path() {
        let all = Arguments::parse("lsp_symbols", json!({"query":"Store","scope":"all"})).unwrap();
        assert!(all.path.is_none());
        let workspace = Arguments::parse("lsp_symbols", json!({"query":"Store"})).unwrap();
        assert!(workspace.path.is_none());
        let file = Arguments::parse(
            "lsp_symbols",
            json!({"query":"Store","scope":"all","path":"store.rs"}),
        )
        .unwrap();
        assert_eq!(file.path.as_deref(), Some("store.rs"));
    }

    #[test]
    fn targets_accept_names_and_occurrences_without_silently_dropping_constraints() {
        for target in [
            json!({"symbol":"Store::save"}),
            json!({"path":"store.rs","symbol":"save"}),
            json!({"path":"store.rs","line":9,"symbol":"save"}),
            json!({"path":"store.rs","line":9,"column":7}),
        ] {
            for name in ["lsp_definition", "lsp_context", "lsp_references"] {
                assert!(Arguments::parse(name, target.clone()).is_ok());
            }
            assert!(
                Arguments::parse("lsp_paths", json!({"from":target,"to":{"symbol":"finish"}}))
                    .is_ok()
            );
        }
        for target in [
            json!({}),
            json!({"line":4,"symbol":"save"}),
            json!({"path":"a","column":2}),
            json!({"symbol":""}),
        ] {
            assert!(Arguments::parse("lsp_context", target.clone()).is_err());
            assert!(
                Arguments::parse("lsp_paths", json!({"from":target,"to":{"symbol":"finish"}}))
                    .is_err()
            );
        }
        assert!(
            Arguments::parse(
                "lsp_paths",
                json!({"from":{"symbol":"a"},"to":{"symbol":"b"},"max_depth":9})
            )
            .is_err()
        );
        assert!(
            Arguments::parse(
                "lsp_paths",
                json!({"from":{"symbol":"a"},"to":{"symbol":"b"},"offset":1})
            )
            .is_err()
        );
    }
}
