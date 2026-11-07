use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

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
        ensure!(super::catalog::is_tool(name), "unknown LSP tool");
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

#[cfg(test)]
mod tests {}
