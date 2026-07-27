use crate::plan::PlanCallableKind;
use anyhow::{Context, Result};
use serde::Deserialize;
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;

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
    callable_by_receiver: HashMap<(String, String), Vec<RustdocItem>>,
}

impl RustdocIndex {
    pub(crate) fn parse(source: &[u8]) -> Result<Self> {
        let document: RustdocJson =
            serde_json::from_slice(source).context("decode Rustdoc JSON")?;
        anyhow::ensure!(
            document.format_version >= 33,
            "unsupported Rustdoc JSON format {}",
            document.format_version
        );
        Ok(Self::build(&document))
    }

    fn build(document: &RustdocJson) -> Self {
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

        let mut callable_by_receiver = HashMap::<(String, String), Vec<RustdocItem>>::new();
        for item in document.index.values() {
            let Some(implementation) = item.inner_for("impl") else {
                continue;
            };
            let receiver = implementation
                .get("for")
                .map(|value| type_name(document, value))
                .unwrap_or_default();
            if receiver.is_empty() {
                continue;
            }
            let receiver_name = receiver
                .rsplit("::")
                .next()
                .unwrap_or(receiver.as_str())
                .to_owned();
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
                let owner_path = trait_path.clone().unwrap_or_else(|| receiver.clone());
                let resolved = RustdocItem {
                    path: format!("{owner_path}::{method_name}"),
                    signature: function_signature(method),
                    docs: method.docs.clone().unwrap_or_default(),
                    span: method.span.clone(),
                };
                let item_list = callable_by_receiver
                    .entry((receiver_name.clone(), method_name.to_owned()))
                    .or_default();
                if !item_list.iter().any(|item| item.path == resolved.path) {
                    item_list.push(resolved);
                }
            }
        }

        Self {
            item_by_name,
            callable_by_receiver,
        }
    }

    pub(crate) fn type_item(&self, name: &str) -> Result<RustdocItem, LookupError> {
        unique(self.item_by_name.get(name), "type", name)
    }

    pub(crate) fn callable(
        &self,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocItem, LookupError> {
        let item_list = self
            .callable_by_receiver
            .get(&(short_name(receiver), callable.to_owned()))
            .map(|item_list| {
                item_list
                    .iter()
                    .filter(|item| callable_kind(&item.signature) == kind)
                    .cloned()
                    .collect::<Vec<_>>()
            });
        unique(item_list.as_ref(), "callable", callable)
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

    #[test]
    fn resolves_extension_trait_methods_for_external_receivers() {
        let index = RustdocIndex::parse(&fixture()).unwrap();
        let method = index
            .callable(
                "ParquetRecordBatchReaderBuilder",
                "geoparquet_metadata",
                PlanCallableKind::Method,
            )
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
        assert!(matches!(
            index.callable(
                "ParquetRecordBatchReaderBuilder",
                "geoparquet_metadata",
                PlanCallableKind::Function,
            ),
            Err(LookupError::Missing(_))
        ));
    }
}
