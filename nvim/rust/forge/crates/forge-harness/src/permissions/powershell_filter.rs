use super::{CommandAnalysisFailure, CommandShell, shell_token_list, text};
use std::collections::BTreeSet;
use std::ops::Range;
use tree_sitter::Node;

const PARAMETER_LIST: &[(&str, FilterParameter)] = &[
    ("property", FilterParameter::Property),
    ("value", FilterParameter::Value),
    ("filterscript", FilterParameter::Script),
    ("inputobject", FilterParameter::Input),
    ("eq", FilterParameter::Comparison),
    ("ceq", FilterParameter::Comparison),
    ("ne", FilterParameter::Comparison),
    ("cne", FilterParameter::Comparison),
    ("gt", FilterParameter::Comparison),
    ("cgt", FilterParameter::Comparison),
    ("ge", FilterParameter::Comparison),
    ("cge", FilterParameter::Comparison),
    ("lt", FilterParameter::Comparison),
    ("clt", FilterParameter::Comparison),
    ("le", FilterParameter::Comparison),
    ("cle", FilterParameter::Comparison),
    ("like", FilterParameter::Comparison),
    ("clike", FilterParameter::Comparison),
    ("notlike", FilterParameter::Comparison),
    ("cnotlike", FilterParameter::Comparison),
    ("match", FilterParameter::Comparison),
    ("cmatch", FilterParameter::Comparison),
    ("notmatch", FilterParameter::Comparison),
    ("cnotmatch", FilterParameter::Comparison),
    ("contains", FilterParameter::Comparison),
    ("ccontains", FilterParameter::Comparison),
    ("notcontains", FilterParameter::Comparison),
    ("cnotcontains", FilterParameter::Comparison),
    ("in", FilterParameter::Comparison),
    ("cin", FilterParameter::Comparison),
    ("notin", FilterParameter::Comparison),
    ("cnotin", FilterParameter::Comparison),
    ("is", FilterParameter::TypeComparison),
    ("isnot", FilterParameter::TypeComparison),
    ("not", FilterParameter::Not),
    ("verbose", FilterParameter::CommonSwitch),
    ("debug", FilterParameter::CommonSwitch),
    ("erroraction", FilterParameter::CommonValue),
    ("errorvariable", FilterParameter::CommonValue),
    ("warningaction", FilterParameter::CommonValue),
    ("warningvariable", FilterParameter::CommonValue),
    ("informationaction", FilterParameter::CommonValue),
    ("informationvariable", FilterParameter::CommonValue),
    ("progressaction", FilterParameter::CommonValue),
    ("outvariable", FilterParameter::CommonValue),
    ("outbuffer", FilterParameter::CommonValue),
    ("pipelinevariable", FilterParameter::CommonValue),
];

const PARAMETER_ALIAS: &[(&str, &str)] = &[
    ("ieq", "eq"),
    ("ine", "ne"),
    ("igt", "gt"),
    ("ige", "ge"),
    ("ilt", "lt"),
    ("ile", "le"),
    ("ilike", "like"),
    ("inotlike", "notlike"),
    ("imatch", "match"),
    ("inotmatch", "notmatch"),
    ("icontains", "contains"),
    ("inotcontains", "notcontains"),
    ("iin", "in"),
    ("inotin", "notin"),
    ("vb", "verbose"),
    ("db", "debug"),
    ("ea", "erroraction"),
    ("ev", "errorvariable"),
    ("wa", "warningaction"),
    ("wv", "warningvariable"),
    ("infa", "informationaction"),
    ("iv", "informationvariable"),
    ("proga", "progressaction"),
    ("ov", "outvariable"),
    ("ob", "outbuffer"),
    ("pv", "pipelinevariable"),
];

#[derive(Clone, Copy, Eq, PartialEq)]
enum FilterParameter {
    Property,
    Value,
    Script,
    Input,
    Comparison,
    TypeComparison,
    Not,
    CommonSwitch,
    CommonValue,
}

#[derive(Default)]
struct FilterBinding<'tree> {
    property: Option<Node<'tree>>,
    value: Option<Node<'tree>>,
    script: Option<Node<'tree>>,
    comparison: Option<FilterParameter>,
    parameter: BTreeSet<&'static str>,
    positional: Vec<Node<'tree>>,
}

impl<'tree> FilterBinding<'tree> {
    fn bind(
        &mut self,
        name: &'static str,
        parameter: FilterParameter,
        argument: Option<Node<'tree>>,
    ) -> Result<(), CommandAnalysisFailure> {
        if !self.parameter.insert(name) {
            return Err(unsupported("duplicate parameter"));
        }
        match parameter {
            FilterParameter::Property => self.property = argument,
            FilterParameter::Value => self.value = argument,
            FilterParameter::Script => self.script = argument,
            FilterParameter::Input
            | FilterParameter::CommonValue
            | FilterParameter::CommonSwitch => {}
            comparison => {
                if self.comparison.replace(comparison).is_some() {
                    return Err(unsupported("conflicting comparison parameters"));
                }
            }
        }
        Ok(())
    }

    fn finish(mut self) -> Result<Option<Range<usize>>, CommandAnalysisFailure> {
        for argument in self.positional {
            if self.script.is_none()
                && self.property.is_none()
                && self.comparison.is_none()
                && expression_leaf(argument).kind() == "script_block_expression"
            {
                self.script = Some(argument);
            } else if self.script.is_some() {
                return Err(unsupported("extra script-block argument"));
            } else if self.property.is_none() {
                self.property = Some(argument);
            } else if self.value.is_none() && self.comparison != Some(FilterParameter::Not) {
                self.value = Some(argument);
            } else {
                return Err(unsupported("extra positional argument"));
            }
        }
        if let Some(script) = self.script {
            if self.property.is_some() || self.value.is_some() || self.comparison.is_some() {
                return Err(unsupported(
                    "mixed script-block and property parameter sets",
                ));
            }
            if expression_leaf(script).kind() != "script_block_expression" {
                return Err(unsupported("dynamic FilterScript"));
            }
            return Ok(None);
        }
        if self.property.is_none() {
            return Err(unsupported("missing property or filter script"));
        }
        if self.comparison == Some(FilterParameter::Not) && self.value.is_some() {
            return Err(unsupported("value supplied with -Not"));
        }
        Ok(self
            .value
            .filter(|_| self.comparison == Some(FilterParameter::TypeComparison))
            .map(expression_leaf)
            .filter(|value| value.kind() == "type_literal")
            .map(|value| value.byte_range()))
    }
}

/// Recognizes the cmdlet and its built-in aliases without evaluating command names.
pub(super) fn is_command(name: &str) -> bool {
    matches!(
        name.to_ascii_lowercase().as_str(),
        "where-object" | "where" | "?" | "microsoft.powershell.core\\where-object"
    )
}

/// Binds supported filter parameter sets while leaving every argument visible to traversal.
pub(super) fn analyze(
    command: Node<'_>,
    source: &str,
) -> Result<Option<Range<usize>>, CommandAnalysisFailure> {
    let mut cursor = command.walk();
    let elements = command
        .named_children(&mut cursor)
        .find(|child| child.kind() == "command_elements")
        .ok_or_else(|| unsupported("missing arguments"))?;
    let mut cursor = elements.walk();
    let argument_list = elements
        .named_children(&mut cursor)
        .filter(|child| !matches!(child.kind(), "command_argument_sep" | "comment"))
        .collect::<Vec<_>>();
    let mut binding = FilterBinding::default();
    let mut arguments = argument_list.into_iter().peekable();
    let mut positional_only = false;
    while let Some(argument) = arguments.next() {
        if positional_only || argument.kind() != "command_parameter" {
            binding.positional.push(argument);
            continue;
        }
        if text(argument, source) == "--" {
            positional_only = true;
            continue;
        }
        let tokens = shell_token_list(text(argument, source), CommandShell::PowerShell)
            .map_err(|_| unsupported("parameter encoding"))?;
        let [name] = tokens.as_slice() else {
            return Err(unsupported("parameter name"));
        };
        let (name, parameter) = resolve_parameter(name.trim_start_matches('-'))
            .ok_or_else(|| unsupported("unknown or ambiguous parameter"))?;
        let takes_value = matches!(
            parameter,
            FilterParameter::Property
                | FilterParameter::Value
                | FilterParameter::Script
                | FilterParameter::Input
                | FilterParameter::CommonValue
        );
        let inline = arguments
            .peek()
            .is_some_and(|next| source[argument.end_byte()..next.start_byte()].trim() == ":");
        let value = if takes_value || inline {
            Some(
                arguments
                    .next()
                    .filter(|value| value.kind() != "command_parameter")
                    .ok_or_else(|| unsupported("missing parameter value"))?,
            )
        } else {
            None
        };
        binding.bind(name, parameter, value)?;
    }
    binding.finish()
}

/// Permits a type literal only when it is the bound operand of a filter type comparison.
pub(super) fn is_type_operand(node: Node<'_>, source: &str) -> bool {
    let mut ancestor = node.parent();
    let mut type_comparison = false;
    while let Some(parent) = ancestor {
        if matches!(
            parent.kind(),
            "comparison_expression" | "comparison_argument_expression"
        ) {
            let mut cursor = parent.walk();
            let child_list = parent
                .named_children(&mut cursor)
                .filter(|child| child.kind() != "comment")
                .collect::<Vec<_>>();
            if let [_, operator, operand] = child_list.as_slice() {
                type_comparison |= matches!(
                    text(*operator, source).to_ascii_lowercase().as_str(),
                    "-is" | "-isnot"
                ) && expression_leaf(*operand) == node;
            }
        }
        if parent.kind() == "command" {
            let mut cursor = parent.walk();
            let name = parent
                .children_by_field_name("command_name", &mut cursor)
                .find(|child| child.is_named());
            return name.is_some_and(|name| {
                shell_token_list(text(name, source), CommandShell::PowerShell)
                    .is_ok_and(|tokens| tokens.len() == 1 && is_command(&tokens[0]))
            }) && analyze(parent, source)
                .is_ok_and(|range| type_comparison || range == Some(node.byte_range()));
        }
        ancestor = parent.parent();
    }
    false
}

fn expression_leaf(mut node: Node<'_>) -> Node<'_> {
    while matches!(
        node.kind(),
        "command_argument_array"
            | "unary_expression"
            | "parenthesized_expression"
            | "pipeline"
            | "pipeline_chain"
            | "statement_list"
            | "logical_expression"
            | "bitwise_expression"
            | "comparison_expression"
            | "additive_expression"
            | "multiplicative_expression"
            | "format_expression"
            | "range_expression"
            | "array_literal_expression"
    ) && node.named_child_count() == 1
    {
        let Some(child) = node.named_child(0) else {
            break;
        };
        node = child;
    }
    node
}

fn resolve_parameter(name: &str) -> Option<(&'static str, FilterParameter)> {
    let name = name.to_ascii_lowercase();
    let canonical = PARAMETER_ALIAS
        .iter()
        .find(|(alias, _)| *alias == name)
        .map_or(name.as_str(), |(_, canonical)| *canonical);
    if let Some(parameter) = PARAMETER_LIST.iter().find(|(name, _)| *name == canonical) {
        return Some(*parameter);
    }
    let mut selected = None;
    for &(candidate, parameter) in PARAMETER_LIST {
        let matched = candidate.starts_with(&name)
            || PARAMETER_ALIAS
                .iter()
                .any(|(alias, canonical)| *canonical == candidate && alias.starts_with(&name));
        if matched {
            if selected.is_some_and(|(previous, _)| previous != candidate) {
                return None;
            }
            selected = Some((candidate, parameter));
        }
    }
    selected
}

fn unsupported(reason: &str) -> CommandAnalysisFailure {
    CommandAnalysisFailure::Unsupported(format!("Where-Object {reason}"))
}
