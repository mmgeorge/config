use crate::plan::PlanCallableKind;
use anyhow::{Context, Result};
use serde::Deserialize;
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

const MIN_SUPPORTED_RUSTDOC_FORMAT_VERSION: u32 = 33;
const MAX_SUPPORTED_RUSTDOC_FORMAT_VERSION: u32 = 60;

#[derive(Debug, Deserialize)]
pub(crate) struct RustdocJson {
    pub format_version: u32,
    pub index: HashMap<String, Item>,
    pub paths: HashMap<String, PathEntry>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct Item {
    pub name: Option<String>,
    pub span: Option<RustdocSourceSpan>,
    pub docs: Option<String>,
    pub inner: Value,
}

impl Item {
    fn inner_for(&self, kind: &str) -> Option<&Value> {
        self.inner.get(kind)
    }
}

#[derive(Debug, Deserialize)]
pub(crate) struct PathEntry {
    pub path: Vec<String>,
    pub kind: Value,
}

impl PathEntry {
    fn full_path(&self) -> String {
        self.path.join("::")
    }

    fn kind_name(&self) -> &str {
        self.kind.as_str().unwrap_or("")
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct RustdocSourceSpan {
    pub filename: PathBuf,
    pub begin: (usize, usize),
    pub end: (usize, usize),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct RustdocItem {
    pub path: String,
    pub signature: String,
    pub docs: String,
    pub span: Option<RustdocSourceSpan>,
}

#[derive(Debug)]
pub(crate) struct RustdocIndex {
    item_by_name: HashMap<String, Vec<RustdocItem>>,
    receiver_by_name: HashMap<String, Vec<RustType>>,
    callable_list: Vec<RustdocCallable>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ResolvedReceiver {
    authored: String,
    canonical: RustType,
}

impl ResolvedReceiver {
    pub(crate) fn authored(&self) -> &str {
        &self.authored
    }
}

#[derive(Clone, Debug)]
struct RustdocCallable {
    receiver: RustType,
    name: String,
    item: RustdocItem,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum RustType {
    Path {
        item_id: Option<String>,
        path: String,
        arguments: Vec<RustGenericArgument>,
    },
    Generic(String),
    Primitive(String),
    Borrowed {
        mutable: bool,
        target: Box<RustType>,
    },
    Tuple(Vec<RustType>),
    Slice(Box<RustType>),
    Array {
        target: Box<RustType>,
        length: String,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum RustGenericArgument {
    Type(RustType),
    Lifetime(String),
    Const(String),
    Infer,
}

#[derive(Clone, Debug)]
struct AliasTemplate {
    parameter_list: Vec<String>,
    target: RustType,
}

impl RustdocIndex {
    pub(crate) fn parse(source: &[u8]) -> Result<Self> {
        let document: RustdocJson =
            serde_json::from_slice(source).context("decode Rustdoc JSON")?;
        anyhow::ensure!(
            (MIN_SUPPORTED_RUSTDOC_FORMAT_VERSION..=MAX_SUPPORTED_RUSTDOC_FORMAT_VERSION)
                .contains(&document.format_version),
            "unsupported Rustdoc JSON format {}; supported versions are {} through {}",
            document.format_version,
            MIN_SUPPORTED_RUSTDOC_FORMAT_VERSION,
            MAX_SUPPORTED_RUSTDOC_FORMAT_VERSION
        );
        Self::build(&document)
    }

    fn build(document: &RustdocJson) -> Result<Self> {
        let mut item_by_name = HashMap::<String, Vec<RustdocItem>>::new();
        for (item_id, path) in &document.paths {
            let Some(item) = document.index.get(item_id) else {
                continue;
            };
            let Some(name) = item.name.as_deref() else {
                continue;
            };
            item_by_name
                .entry(name.to_owned())
                .or_default()
                .push(RustdocItem {
                    path: path.full_path(),
                    signature: item_signature(item, path.kind_name()),
                    docs: item.docs.clone().unwrap_or_default(),
                    span: item.span.clone(),
                });
        }

        let alias_by_id = document
            .index
            .iter()
            .filter_map(|(item_id, item)| {
                let alias = item.inner_for("type_alias")?;
                let target = alias
                    .get("type")
                    .and_then(|value| rust_type(document, value))?;
                Some((
                    item_id.clone(),
                    AliasTemplate {
                        parameter_list: alias_parameter_list(alias),
                        target,
                    },
                ))
            })
            .collect::<HashMap<_, _>>();
        let mut receiver_by_name = HashMap::<String, Vec<RustType>>::new();
        for (item_id, path) in &document.paths {
            let Some(item) = document.index.get(item_id) else {
                continue;
            };
            let Some(name) = item.name.as_deref() else {
                continue;
            };
            let receiver = if alias_by_id.contains_key(item_id) {
                expand_alias(
                    RustType::Path {
                        item_id: Some(item_id.clone()),
                        path: path.full_path(),
                        arguments: Vec::new(),
                    },
                    &alias_by_id,
                    &mut HashSet::new(),
                )?
            } else {
                RustType::Path {
                    item_id: Some(item_id.clone()),
                    path: path.full_path(),
                    arguments: Vec::new(),
                }
            };
            receiver_by_name
                .entry(name.to_owned())
                .or_default()
                .push(receiver);
        }

        let mut callable_list = Vec::new();
        for item in document.index.values() {
            let Some(implementation) = item.inner_for("impl") else {
                continue;
            };
            let Some(receiver) = implementation
                .get("for")
                .and_then(|value| rust_type(document, value))
            else {
                continue;
            };
            let receiver = expand_alias(receiver, &alias_by_id, &mut HashSet::new())?;
            let trait_reference = implementation.get("trait").filter(|value| !value.is_null());
            let trait_path = trait_reference.map(|value| type_name(document, value));
            let mut method_id_list = implementation
                .get("items")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            if let Some(trait_item) = trait_reference
                .and_then(type_id)
                .and_then(|trait_id| document.index.get(&trait_id))
                .and_then(|trait_item| trait_item.inner_for("trait"))
            {
                method_id_list.extend(
                    trait_item
                        .get("items")
                        .and_then(Value::as_array)
                        .cloned()
                        .unwrap_or_default(),
                );
            }
            for method_id in method_id_list {
                let Some(method_id) = id_string(&method_id) else {
                    continue;
                };
                let Some(method) = document.index.get(&method_id) else {
                    continue;
                };
                let Some(method_name) = method.name.as_deref() else {
                    continue;
                };
                if method.inner_for("function").is_none() {
                    continue;
                }
                let owner_path = trait_path
                    .clone()
                    .unwrap_or_else(|| rust_type_display(&receiver));
                let resolved = RustdocItem {
                    path: format!("{owner_path}::{method_name}"),
                    signature: function_signature(method),
                    docs: method.docs.clone().unwrap_or_default(),
                    span: method.span.clone(),
                };
                if !callable_list.iter().any(|entry: &RustdocCallable| {
                    entry.receiver == receiver
                        && entry.name == method_name
                        && entry.item.path == resolved.path
                }) {
                    callable_list.push(RustdocCallable {
                        receiver: receiver.clone(),
                        name: method_name.to_owned(),
                        item: resolved,
                    });
                }
            }
        }

        Ok(Self {
            item_by_name,
            receiver_by_name,
            callable_list,
        })
    }

    pub(crate) fn type_item(&self, name: &str) -> Result<RustdocItem, LookupError> {
        unique(self.item_by_name.get(name), "type", name)
    }

    pub(crate) fn resolve_receiver(&self, authored: &str) -> Result<ResolvedReceiver, LookupError> {
        let receiver_list = self
            .receiver_by_name
            .get(short_name(authored).as_str())
            .cloned()
            .unwrap_or_default();
        match receiver_list.as_slice() {
            [canonical] => Ok(ResolvedReceiver {
                authored: authored.to_owned(),
                canonical: canonical.clone(),
            }),
            [] => Err(LookupError::Missing(format!(
                "type `{authored}` does not exist"
            ))),
            _ => Err(LookupError::Ambiguous(format!(
                "type `{authored}` resolves to {} items",
                receiver_list.len()
            ))),
        }
    }

    pub(crate) fn callable(
        &self,
        receiver: &ResolvedReceiver,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocItem, LookupError> {
        let mut item_list = self
            .callable_list
            .iter()
            .filter(|entry| entry.name == callable)
            .filter(|entry| callable_kind(&entry.item.signature) == kind)
            .filter(|entry| receiver_matches(&receiver.canonical, &entry.receiver))
            .map(|entry| entry.item.clone())
            .collect::<Vec<_>>();
        item_list.sort_by(|left, right| left.path.cmp(&right.path));
        item_list.dedup_by(|left, right| left.path == right.path);
        unique(Some(&item_list), "callable", callable)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum LookupError {
    Missing(String),
    Ambiguous(String),
}

fn unique(
    item_list: Option<&Vec<RustdocItem>>,
    kind: &str,
    name: &str,
) -> Result<RustdocItem, LookupError> {
    let item_list = item_list.cloned().unwrap_or_default();
    match item_list.as_slice() {
        [] => Err(LookupError::Missing(format!(
            "{kind} `{name}` does not exist"
        ))),
        [item] => Ok(item.clone()),
        _ => Err(LookupError::Ambiguous(format!(
            "{kind} `{name}` resolves to {} items",
            item_list.len()
        ))),
    }
}

fn callable_kind(signature: &str) -> PlanCallableKind {
    if signature.split_once('(').is_some_and(|(_, parameters)| {
        parameters.trim_start().starts_with("&self")
            || parameters.trim_start().starts_with("&mut self")
            || parameters.trim_start().starts_with("self")
    }) {
        PlanCallableKind::Method
    } else {
        PlanCallableKind::Function
    }
}

fn short_name(value: &str) -> String {
    value.rsplit("::").next().unwrap_or(value).to_owned()
}

fn id_string(value: &Value) -> Option<String> {
    value
        .as_str()
        .map(str::to_owned)
        .or_else(|| value.as_u64().map(|value| value.to_string()))
}

fn type_id(value: &Value) -> Option<String> {
    let value = value.get("resolved_path").unwrap_or(value);
    value.get("id").and_then(id_string)
}

fn type_name(document: &RustdocJson, value: &Value) -> String {
    if let Some(path) = type_id(value).and_then(|item_id| document.paths.get(&item_id)) {
        return path.full_path();
    }
    let value = value.get("resolved_path").unwrap_or(value);
    value
        .get("path")
        .and_then(Value::as_str)
        .or_else(|| value.get("name").and_then(Value::as_str))
        .unwrap_or("")
        .to_owned()
}

fn rust_type(document: &RustdocJson, value: &Value) -> Option<RustType> {
    if let Some(generic) = value.get("generic").and_then(Value::as_str) {
        return Some(RustType::Generic(generic.to_owned()));
    }
    if let Some(primitive) = value.get("primitive").and_then(Value::as_str) {
        return Some(RustType::Primitive(primitive.to_owned()));
    }
    if let Some(reference) = value.get("borrowed_ref") {
        return Some(RustType::Borrowed {
            mutable: reference
                .get("is_mutable")
                .and_then(Value::as_bool)
                .unwrap_or(false),
            target: Box::new(rust_type(document, reference.get("type")?)?),
        });
    }
    if let Some(tuple) = value.get("tuple").and_then(Value::as_array) {
        return Some(RustType::Tuple(
            tuple
                .iter()
                .map(|value| rust_type(document, value))
                .collect::<Option<Vec<_>>>()?,
        ));
    }
    if let Some(slice) = value.get("slice") {
        return Some(RustType::Slice(Box::new(rust_type(document, slice)?)));
    }
    if let Some(array) = value.get("array") {
        return Some(RustType::Array {
            target: Box::new(rust_type(document, array.get("type")?)?),
            length: array
                .get("len")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
        });
    }
    let path = value.get("resolved_path").unwrap_or(value);
    let path_name = type_id(path)
        .and_then(|item_id| document.paths.get(&item_id))
        .map(PathEntry::full_path)
        .or_else(|| {
            path.get("path")
                .and_then(Value::as_str)
                .or_else(|| path.get("name").and_then(Value::as_str))
                .map(str::to_owned)
        })?;
    Some(RustType::Path {
        item_id: type_id(path),
        path: path_name,
        arguments: generic_argument_list(document, path.get("args")),
    })
}

fn generic_argument_list(
    document: &RustdocJson,
    value: Option<&Value>,
) -> Vec<RustGenericArgument> {
    let Some(value) = value else {
        return Vec::new();
    };
    let argument_list = value
        .get("angle_bracketed")
        .and_then(|value| value.get("args"))
        .and_then(Value::as_array)
        .or_else(|| value.get("args").and_then(Value::as_array))
        .cloned()
        .unwrap_or_default();
    argument_list
        .into_iter()
        .filter_map(|argument| {
            if let Some(value) = argument.get("type") {
                return rust_type(document, value).map(RustGenericArgument::Type);
            }
            if let Some(lifetime) = argument.get("lifetime").and_then(Value::as_str) {
                return Some(RustGenericArgument::Lifetime(lifetime.to_owned()));
            }
            if let Some(constant) = argument.get("const") {
                return Some(RustGenericArgument::Const(constant.to_string()));
            }
            argument
                .get("infer")
                .is_some()
                .then_some(RustGenericArgument::Infer)
        })
        .collect()
}

fn alias_parameter_list(alias: &Value) -> Vec<String> {
    alias
        .get("generics")
        .and_then(|value| value.get("params"))
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|parameter| parameter.get("name").and_then(Value::as_str))
        .map(str::to_owned)
        .collect()
}

fn expand_alias(
    receiver: RustType,
    alias_by_id: &HashMap<String, AliasTemplate>,
    visiting: &mut HashSet<String>,
) -> Result<RustType> {
    let RustType::Path {
        item_id: Some(item_id),
        arguments,
        ..
    } = &receiver
    else {
        return Ok(receiver);
    };
    let Some(alias) = alias_by_id.get(item_id) else {
        return Ok(receiver);
    };
    anyhow::ensure!(
        visiting.insert(item_id.clone()),
        "cyclic Rust type alias involving item {item_id}"
    );
    let substitution = alias
        .parameter_list
        .iter()
        .cloned()
        .zip(arguments.iter().filter_map(|argument| match argument {
            RustGenericArgument::Type(value) => Some(value.clone()),
            _ => None,
        }))
        .collect::<HashMap<_, _>>();
    let expanded = substitute_type(&alias.target, &substitution);
    let expanded = expand_alias(expanded, alias_by_id, visiting);
    visiting.remove(item_id);
    expanded
}

fn substitute_type(value: &RustType, substitution: &HashMap<String, RustType>) -> RustType {
    match value {
        RustType::Generic(name) => substitution
            .get(name)
            .cloned()
            .unwrap_or_else(|| value.clone()),
        RustType::Path {
            item_id,
            path,
            arguments,
        } => RustType::Path {
            item_id: item_id.clone(),
            path: path.clone(),
            arguments: arguments
                .iter()
                .map(|argument| match argument {
                    RustGenericArgument::Type(value) => {
                        RustGenericArgument::Type(substitute_type(value, substitution))
                    }
                    argument => argument.clone(),
                })
                .collect(),
        },
        RustType::Borrowed { mutable, target } => RustType::Borrowed {
            mutable: *mutable,
            target: Box::new(substitute_type(target, substitution)),
        },
        RustType::Tuple(value_list) => RustType::Tuple(
            value_list
                .iter()
                .map(|value| substitute_type(value, substitution))
                .collect(),
        ),
        RustType::Slice(target) => RustType::Slice(Box::new(substitute_type(target, substitution))),
        RustType::Array { target, length } => RustType::Array {
            target: Box::new(substitute_type(target, substitution)),
            length: length.clone(),
        },
        RustType::Primitive(_) => value.clone(),
    }
}

fn receiver_matches(query: &RustType, implementation: &RustType) -> bool {
    match_receiver(query, implementation, &mut HashMap::new())
}

fn match_receiver(
    query: &RustType,
    implementation: &RustType,
    binding: &mut HashMap<String, RustType>,
) -> bool {
    if let RustType::Generic(name) = implementation {
        return match binding.get(name) {
            Some(existing) => existing == query,
            None => {
                binding.insert(name.clone(), query.clone());
                true
            }
        };
    }
    match (query, implementation) {
        (
            RustType::Path {
                path: query_path,
                arguments: query_arguments,
                ..
            },
            RustType::Path {
                path: implementation_path,
                arguments: implementation_arguments,
                ..
            },
        ) => {
            query_path == implementation_path
                && (query_arguments.is_empty()
                    || query_arguments.len() == implementation_arguments.len()
                        && query_arguments.iter().zip(implementation_arguments).all(
                            |(query, implementation)| {
                                match_generic_argument(query, implementation, binding)
                            },
                        ))
        }
        (
            RustType::Borrowed {
                mutable: query_mutable,
                target: query_target,
            },
            RustType::Borrowed {
                mutable: implementation_mutable,
                target: implementation_target,
            },
        ) => {
            query_mutable == implementation_mutable
                && match_receiver(query_target, implementation_target, binding)
        }
        (RustType::Tuple(query), RustType::Tuple(implementation)) => {
            query.len() == implementation.len()
                && query
                    .iter()
                    .zip(implementation)
                    .all(|(query, implementation)| match_receiver(query, implementation, binding))
        }
        (RustType::Slice(query), RustType::Slice(implementation)) => {
            match_receiver(query, implementation, binding)
        }
        (
            RustType::Array {
                target: query_target,
                length: query_length,
            },
            RustType::Array {
                target: implementation_target,
                length: implementation_length,
            },
        ) => {
            query_length == implementation_length
                && match_receiver(query_target, implementation_target, binding)
        }
        _ => query == implementation,
    }
}

fn match_generic_argument(
    query: &RustGenericArgument,
    implementation: &RustGenericArgument,
    binding: &mut HashMap<String, RustType>,
) -> bool {
    match (query, implementation) {
        (RustGenericArgument::Type(query), RustGenericArgument::Type(implementation)) => {
            match_receiver(query, implementation, binding)
        }
        _ => query == implementation,
    }
}

fn rust_type_display(value: &RustType) -> String {
    match value {
        RustType::Path {
            path, arguments, ..
        } if arguments.is_empty() => path.clone(),
        RustType::Path {
            path, arguments, ..
        } => format!(
            "{path}<{}>",
            arguments
                .iter()
                .map(rust_generic_argument_display)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        RustType::Generic(name) | RustType::Primitive(name) => name.clone(),
        RustType::Borrowed { mutable, target } => {
            format!(
                "&{}{}",
                if *mutable { "mut " } else { "" },
                rust_type_display(target)
            )
        }
        RustType::Tuple(value_list) => format!(
            "({})",
            value_list
                .iter()
                .map(rust_type_display)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        RustType::Slice(target) => format!("[{}]", rust_type_display(target)),
        RustType::Array { target, length } => {
            format!("[{}; {length}]", rust_type_display(target))
        }
    }
}

fn rust_generic_argument_display(value: &RustGenericArgument) -> String {
    match value {
        RustGenericArgument::Type(value) => rust_type_display(value),
        RustGenericArgument::Lifetime(value) | RustGenericArgument::Const(value) => value.clone(),
        RustGenericArgument::Infer => "_".into(),
    }
}

fn item_signature(item: &Item, kind: &str) -> String {
    if kind == "function" {
        function_signature(item)
    } else {
        format!("{kind} {}", item.name.as_deref().unwrap_or("_"))
    }
}

fn function_signature(item: &Item) -> String {
    let Some(function) = item.inner_for("function") else {
        return String::new();
    };
    let name = item.name.as_deref().unwrap_or("_");
    let Some(signature) = function.get("sig") else {
        return format!("fn {name}()");
    };
    let parameters = signature
        .get("inputs")
        .and_then(Value::as_array)
        .map(|input_list| {
            input_list
                .iter()
                .filter_map(Value::as_array)
                .map(|input| {
                    let parameter = input.first().and_then(Value::as_str).unwrap_or("_");
                    let type_name = input.get(1).map(type_to_string).unwrap_or_default();
                    match (parameter, type_name.as_str()) {
                        ("self", "Self") => "self".to_owned(),
                        ("self", "&Self") => "&self".to_owned(),
                        ("self", "&mut Self") => "&mut self".to_owned(),
                        _ => format!("{parameter}: {type_name}"),
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        })
        .unwrap_or_default();
    let output = signature
        .get("output")
        .filter(|value| !value.is_null())
        .map(type_to_string)
        .filter(|value| value != "()");
    match output {
        Some(output) => format!("fn {name}({parameters}) -> {output}"),
        None => format!("fn {name}({parameters})"),
    }
}

fn type_to_string(value: &Value) -> String {
    if let Some(primitive) = value.get("primitive").and_then(Value::as_str) {
        return primitive.to_owned();
    }
    if let Some(generic) = value.get("generic").and_then(Value::as_str) {
        return generic.to_owned();
    }
    if let Some(reference) = value.get("borrowed_ref") {
        let prefix = if reference
            .get("is_mutable")
            .and_then(Value::as_bool)
            .unwrap_or(false)
        {
            "&mut "
        } else {
            "&"
        };
        return format!(
            "{prefix}{}",
            reference
                .get("type")
                .map(type_to_string)
                .unwrap_or_else(|| "_".into())
        );
    }
    let path = value.get("resolved_path").unwrap_or(value);
    if let Some(name) = path
        .get("path")
        .and_then(Value::as_str)
        .or_else(|| path.get("name").and_then(Value::as_str))
    {
        return name.to_owned();
    }
    if let Some(tuple) = value.get("tuple").and_then(Value::as_array) {
        return format!(
            "({})",
            tuple
                .iter()
                .map(type_to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    "_".to_owned()
}

#[cfg(test)]
mod test {
    use super::*;

    fn fixture() -> Vec<u8> {
        serde_json::to_vec(&serde_json::json!({
            "format_version": 57,
            "paths": {
                "1": { "path": ["parquet", "ParquetRecordBatchReaderBuilder"], "kind": "struct" },
                "2": { "path": ["geoparquet", "GeoParquetMetadataExt"], "kind": "trait" }
            },
            "index": {
                "1": {
                    "name": "ParquetRecordBatchReaderBuilder",
                    "span": {
                        "filename": "src/reader.rs",
                        "begin": [10, 1],
                        "end": [20, 2]
                    },
                    "docs": "Builds readers for one Parquet input.",
                    "inner": { "struct": { "impls": [3] } }
                },
                "2": {
                    "name": "GeoParquetMetadataExt",
                    "span": {
                        "filename": "src/metadata.rs",
                        "begin": [30, 1],
                        "end": [40, 2]
                    },
                    "docs": "Reads GeoParquet metadata.",
                    "inner": { "trait": { "items": [4] } }
                },
                "3": {
                    "name": null,
                    "span": null,
                    "docs": null,
                    "inner": {
                        "impl": {
                            "trait": { "id": 2, "path": "GeoParquetMetadataExt" },
                            "for": { "id": 1, "path": "ParquetRecordBatchReaderBuilder" },
                            "items": []
                        }
                    }
                },
                "4": {
                    "name": "geoparquet_metadata",
                    "span": {
                        "filename": "src/metadata.rs",
                        "begin": [35, 5],
                        "end": [38, 6]
                    },
                    "docs": "Returns decoded GeoParquet file metadata.",
                    "inner": {
                        "function": {
                            "sig": {
                                "inputs": [["self", { "borrowed_ref": { "is_mutable": false, "type": { "generic": "Self" } } }]],
                                "output": { "resolved_path": { "path": "GeoParquetMetadata", "id": 5 } }
                            }
                        }
                    }
                }
            }
        }))
        .unwrap()
    }

    fn generic_alias_fixture() -> Vec<u8> {
        serde_json::to_vec(&serde_json::json!({
            "format_version": 57,
            "paths": {
                "1": { "path": ["parquet", "ArrowReaderBuilder"], "kind": "struct" },
                "2": { "path": ["parquet", "SyncReader"], "kind": "struct" },
                "3": {
                    "path": ["parquet", "ParquetRecordBatchReaderBuilder"],
                    "kind": "type_alias"
                }
            },
            "index": {
                "1": {
                    "name": "ArrowReaderBuilder",
                    "span": null,
                    "docs": "Builds one Arrow reader.",
                    "inner": { "struct": { "impls": [4] } }
                },
                "2": {
                    "name": "SyncReader",
                    "span": null,
                    "docs": "Reads Parquet bytes synchronously.",
                    "inner": { "struct": { "impls": [] } }
                },
                "3": {
                    "name": "ParquetRecordBatchReaderBuilder",
                    "span": null,
                    "docs": "Names the synchronous Parquet reader builder.",
                    "inner": {
                        "type_alias": {
                            "type": {
                                "resolved_path": {
                                    "name": "ArrowReaderBuilder",
                                    "id": 1,
                                    "args": {
                                        "angle_bracketed": {
                                            "args": [{
                                                "type": {
                                                    "resolved_path": {
                                                        "name": "SyncReader",
                                                        "id": 2,
                                                        "args": {
                                                            "angle_bracketed": {
                                                                "args": [{
                                                                    "type": { "generic": "T" }
                                                                }]
                                                            }
                                                        }
                                                    }
                                                }
                                            }]
                                        }
                                    }
                                }
                            },
                            "generics": {
                                "params": [{
                                    "name": "T",
                                    "kind": { "type": {} }
                                }]
                            }
                        }
                    }
                },
                "4": {
                    "name": null,
                    "span": null,
                    "docs": null,
                    "inner": {
                        "impl": {
                            "trait": null,
                            "for": {
                                "resolved_path": {
                                    "name": "ArrowReaderBuilder",
                                    "id": 1,
                                    "args": {
                                        "angle_bracketed": {
                                            "args": [{
                                                "type": {
                                                    "resolved_path": {
                                                        "name": "SyncReader",
                                                        "id": 2,
                                                        "args": {
                                                            "angle_bracketed": {
                                                                "args": [{
                                                                    "type": { "generic": "T" }
                                                                }]
                                                            }
                                                        }
                                                    }
                                                }
                                            }]
                                        }
                                    }
                                }
                            },
                            "items": [5]
                        }
                    }
                },
                "5": {
                    "name": "try_new",
                    "span": {
                        "filename": "src/arrow_reader/mod.rs",
                        "begin": [120, 5],
                        "end": [123, 6]
                    },
                    "docs": "Builds a synchronous Parquet record batch reader.",
                    "inner": {
                        "function": {
                            "sig": {
                                "inputs": [[
                                    "reader",
                                    { "generic": "T" }
                                ]],
                                "output": {
                                    "resolved_path": {
                                        "path": "Result",
                                        "id": 6
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }))
        .unwrap()
    }

    #[test]
    fn resolves_generic_alias_methods_without_rewriting_the_authored_receiver() {
        let index = RustdocIndex::parse(&generic_alias_fixture()).unwrap();
        let receiver = index
            .resolve_receiver("ParquetRecordBatchReaderBuilder")
            .unwrap();

        assert_eq!(receiver.authored, "ParquetRecordBatchReaderBuilder");
        assert_eq!(
            rust_type_display(&receiver.canonical),
            "parquet::ArrowReaderBuilder<parquet::SyncReader<T>>"
        );
        let method = index
            .callable(&receiver, "try_new", PlanCallableKind::Function)
            .unwrap();
        assert_eq!(
            method.path,
            "parquet::ArrowReaderBuilder<parquet::SyncReader<T>>::try_new"
        );
    }

    #[test]
    fn resolves_extension_trait_methods_for_external_receivers() {
        let index = RustdocIndex::parse(&fixture()).unwrap();
        let receiver = index
            .resolve_receiver("ParquetRecordBatchReaderBuilder")
            .unwrap();
        let method = index
            .callable(&receiver, "geoparquet_metadata", PlanCallableKind::Method)
            .unwrap();
        assert_eq!(
            method.path,
            "geoparquet::GeoParquetMetadataExt::geoparquet_metadata"
        );
        assert_eq!(
            method.signature,
            "fn geoparquet_metadata(&self) -> GeoParquetMetadata"
        );
        assert_eq!(method.docs, "Returns decoded GeoParquet file metadata.");
        assert_eq!(
            method.span,
            Some(RustdocSourceSpan {
                filename: PathBuf::from("src/metadata.rs"),
                begin: (35, 5),
                end: (38, 6),
            })
        );
    }

    #[test]
    fn rejects_a_callable_kind_that_conflicts_with_its_receiver() {
        let index = RustdocIndex::parse(&fixture()).unwrap();
        let receiver = index
            .resolve_receiver("ParquetRecordBatchReaderBuilder")
            .unwrap();
        assert!(matches!(
            index.callable(&receiver, "geoparquet_metadata", PlanCallableKind::Function,),
            Err(LookupError::Missing(_))
        ));
    }
}
