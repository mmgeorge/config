use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Result, ensure};
use forge_diff::syntax::{DeclarationCalls, DeclarationIndex, DeclarationRole};
use serde::Serialize;

use super::{PlanDocument, PlanNavigationAnchor, PlanReviewTarget};
use crate::declaration::{DeclarationResolution, DeclarationResolver};

/// A snapshot-bound structured usage in a declaration plan.
#[derive(Clone, Serialize)]
pub(crate) struct PlanReference {
    pub id: String,
    pub path: String,
    pub side: String,
    pub owner: String,
    pub owner_capture: Vec<Option<&'static str>>,
    pub name: String,
    pub kind: String,
    pub line: u32,
    pub column: u32,
    #[serde(skip)]
    pub symbol: String,
    #[serde(skip)]
    pub anchor: Option<PlanNavigationAnchor>,
}

#[derive(Eq, Ord, PartialEq, PartialOrd)]
struct AlgorithmLocation {
    path: String,
    owner: String,
    line: u32,
    column: u32,
}

struct ResolvedAlgorithmReference {
    symbol: String,
    length: usize,
    line_width: usize,
}

struct ReferenceSite<'site> {
    path: &'site str,
    side: &'site str,
    owner: &'site str,
    line: u32,
    column: u32,
}

/// Indexes reverse usages independently of sorting, folding, and visible diff context.
#[derive(Default)]
pub(crate) struct PlanReferenceIndex {
    pub declaration: BTreeMap<String, std::sync::Arc<DeclarationIndex>>,
    pub introduced: BTreeSet<(String, u32, u32)>,
    pub module_source: BTreeMap<(String, u32, u32), std::path::PathBuf>,
    baseline_digest: BTreeMap<String, String>,
    pub occurrence: Vec<PlanReference>,
    pub definition: BTreeMap<(String, u32, u32), (String, usize)>,
    pub call: BTreeMap<(String, String, String, super::CallKind), String>,
    pub structured: BTreeMap<String, String>,
    pub unresolved: BTreeMap<(String, String, String, super::CallKind), String>,
    algorithm: BTreeMap<AlgorithmLocation, ResolvedAlgorithmReference>,
}

impl PlanReferenceIndex {
    /// Resolve saved call and type references without reading uncaptured caller files.
    pub(crate) fn build(
        document: &PlanDocument,
        workspace: &std::path::Path,
        baseline: bool,
    ) -> Result<Self> {
        let Some(design) = &document.design else {
            return Self::legacy(document, workspace);
        };
        let mut resolver = DeclarationResolver::planned(workspace, design, baseline)?;
        resolver.discard_snapshot_originals();
        Self::extract(design, baseline, resolver)
    }

    /// Index rename identities and uses from the saved plan declarations alone.
    pub(crate) fn planned(document: &PlanDocument, workspace: &std::path::Path) -> Result<Self> {
        let design = document
            .design
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("rename requires a declaration plan"))?;
        Self::extract(
            design,
            false,
            DeclarationResolver::planned(workspace, design, false)?,
        )
    }

    /// Index proposed usages and introduced identities without consulting uncaptured callers.
    pub(crate) fn design(
        design: &super::DeclarationDesign,
        workspace: &std::path::Path,
    ) -> Result<Self> {
        Self::extract(
            design,
            false,
            DeclarationResolver::planned(workspace, design, false)?,
        )
    }

    fn extract(
        design: &super::DeclarationDesign,
        baseline: bool,
        mut resolver: DeclarationResolver,
    ) -> Result<Self> {
        let files: BTreeMap<String, String> = if baseline {
            design
                .baseline
                .iter()
                .map(|(path, file)| (path.clone(), file.text.clone()))
                .collect()
        } else {
            design.proposed.clone()
        };
        let calls = if baseline {
            &design.baseline_calls
        } else {
            &design.proposed_calls
        };
        let side = if baseline { "baseline" } else { "proposed" };
        let mut output = Self::default();
        let mut lua_function = BTreeMap::<(String, String), (u32, u32)>::new();
        let mut lua_index = BTreeMap::new();
        for (path, text) in files.iter().filter(|(path, _)| path.ends_with(".lua")) {
            let index = std::sync::Arc::new(
                DeclarationIndex::extract(path, text)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?,
            );
            for symbol in &index.symbol {
                lua_function.insert(
                    (
                        path.clone(),
                        symbol
                            .scope
                            .iter()
                            .chain(std::iter::once(&symbol.name))
                            .cloned()
                            .collect::<Vec<_>>()
                            .join("."),
                    ),
                    (symbol.position.line, symbol.position.column),
                );
            }
            lua_index.insert(path.clone(), index);
        }
        for (path, text) in &files {
            if forge_diff::syntax::ConfigurationFormat::for_path(path).is_some() {
                continue;
            }
            let index = match resolver.index(path) {
                Some(index) => index,
                None => match lua_index.get(path) {
                    Some(index) => index.clone(),
                    None => std::sync::Arc::new(
                        DeclarationIndex::extract(path, text)
                            .map_err(|error| anyhow::anyhow!("{error:?}"))?,
                    ),
                },
            };
            if !baseline {
                let origin = design
                    .moved
                    .iter()
                    .find(|(_, destination)| *destination == path)
                    .map(|(source, _)| source)
                    .unwrap_or(path);
                let previous = design
                    .baseline
                    .get(origin)
                    .map(|file| DeclarationIndex::extract(origin, &file.text))
                    .transpose()
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                let existing = previous
                    .iter()
                    .flat_map(|index| &index.symbol)
                    .map(symbol_key)
                    .collect::<BTreeSet<_>>();
                if let Some(file) = design.baseline.get(origin) {
                    output
                        .baseline_digest
                        .insert(origin.clone(), super::digest(file.text.as_bytes()));
                }
                for symbol in &index.symbol {
                    if !symbol.parameter && !existing.contains(&symbol_key(symbol)) {
                        output.introduced.insert((
                            path.clone(),
                            symbol.position.line,
                            symbol.position.column,
                        ));
                    }
                }
            }
            for symbol in &index.symbol {
                if symbol.parameter && symbol.name == "Self" {
                    continue;
                }
                if let Some(identity) = if path.ends_with(".lua") {
                    Some(lua_identity(
                        path,
                        &symbol
                            .scope
                            .iter()
                            .chain(std::iter::once(&symbol.name))
                            .cloned()
                            .collect::<Vec<_>>()
                            .join("."),
                    ))
                } else {
                    let resolution =
                        resolver.at(path, symbol.position.line, symbol.position.column);
                    if let DeclarationResolution::Resolved { destination } = &resolution
                        && destination.module_file
                    {
                        output.module_source.insert(
                            (path.clone(), symbol.position.line, symbol.position.column),
                            destination.path.clone().into(),
                        );
                    }
                    identity(resolution)
                } {
                    output.definition.insert(
                        (path.clone(), symbol.position.line, symbol.position.column),
                        (identity, symbol.name.len()),
                    );
                }
            }
            for reference in &index.reference {
                if let Some(symbol) = if path.ends_with(".lua") {
                    lua_target(path, &reference.path.join("."), &index, &lua_function)
                } else {
                    identity(resolver.at(path, reference.position.line, reference.position.column))
                } {
                    output.push(
                        ReferenceSite {
                            path,
                            side,
                            owner: &reference.scope.join("::"),
                            line: reference.position.line,
                            column: reference.position.column,
                        },
                        &reference.path.join("::"),
                        if index.symbol.iter().any(|symbol| {
                            symbol.parameter
                                && symbol.name == "Self"
                                && symbol.position == reference.position
                        }) {
                            "owner"
                        } else {
                            "type"
                        },
                        symbol,
                        None,
                    );
                }
            }
            for reference in &index.reference {
                if reference.conditional {
                    continue;
                }
                let row = text
                    .lines()
                    .nth(reference.position.line as usize - 1)
                    .unwrap_or_default();
                let mut offset = reference.position.column as usize;
                for (position, part) in reference
                    .path
                    .iter()
                    .enumerate()
                    .take(reference.path.len().saturating_sub(1))
                {
                    let Some(found) = row.get(offset..).and_then(|tail| tail.find(part)) else {
                        break;
                    };
                    offset += found;
                    if let Some(symbol) = identity(resolver.type_target(
                        path,
                        &reference.scope,
                        &reference.path[..=position],
                    )) {
                        output.push(
                            ReferenceSite {
                                path,
                                side,
                                owner: &reference.scope.join("::"),
                                line: reference.position.line,
                                column: offset as u32,
                            },
                            part,
                            "qualifier",
                            symbol,
                            None,
                        );
                    }
                    offset += part.len();
                }
            }
            for import in &index.import {
                if let Some(symbol) =
                    identity(resolver.at(path, import.position.line, import.position.column))
                        .or_else(|| {
                            identity(resolver.callable(
                                path,
                                &import.scope.join("::"),
                                import.alias.as_deref().unwrap_or_else(|| {
                                    import.path.last().map(String::as_str).unwrap_or("")
                                }),
                            ))
                        })
                {
                    output.push(
                        ReferenceSite {
                            path,
                            side,
                            owner: &import.scope.join("::"),
                            line: import.position.line,
                            column: import.position.column,
                        },
                        &import.path.join("::"),
                        if path.ends_with(".rs") && import.export {
                            "export"
                        } else {
                            "import"
                        },
                        symbol,
                        None,
                    );
                }
            }
            let callable = DeclarationCalls::extract(path, text, true)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let callable = callable
                .iter()
                .map(|function| (function.owner.as_str(), function))
                .collect::<BTreeMap<_, _>>();
            for function in calls.get(path).into_iter().flatten() {
                let position = callable.get(function.owner.as_str());
                let Some(position) = position else { continue };
                let mut target = BTreeMap::<(&str, super::CallKind), bool>::new();
                for reference in function.call.iter().flatten() {
                    *target.entry((&reference.name, reference.kind)).or_default() |=
                        reference.unresolved;
                }
                for ((name, kind), unresolved) in target {
                    if unresolved {
                        output.unresolved.insert(
                            (path.clone(), function.owner.clone(), name.into(), kind),
                            "This target includes an opaque local binding in the captured source."
                                .into(),
                        );
                        continue;
                    }
                    let separator = if path.ends_with(".rs") { "::" } else { "." };
                    let parts = name.split(separator).map(str::to_owned).collect::<Vec<_>>();
                    for prefix in 1..parts.len() {
                        let target = if path.ends_with(".lua") {
                            lua_target(
                                path,
                                &parts[..prefix].join(separator),
                                &index,
                                &lua_function,
                            )
                        } else {
                            identity(
                                resolver.type_target(
                                    path,
                                    &function
                                        .owner
                                        .split(separator)
                                        .map(str::to_owned)
                                        .collect::<Vec<_>>(),
                                    &parts[..prefix],
                                ),
                            )
                        };
                        if let Some(symbol) = target {
                            output.push(
                                ReferenceSite {
                                    path,
                                    side,
                                    owner: &function.owner,
                                    line: position.line,
                                    column: position.column,
                                },
                                &parts[..prefix].join(separator),
                                "body_qualifier",
                                symbol,
                                None,
                            );
                        }
                    }
                    let symbol = if path.ends_with(".lua") {
                        lua_target(path, name, &index, &lua_function)
                    } else {
                        let resolution = match kind {
                            super::CallKind::Call | super::CallKind::Callback => {
                                resolver.callable(path, &function.owner, name)
                            }
                            super::CallKind::Value => resolver.value(path, &function.owner, name),
                            super::CallKind::Construct => {
                                resolver.construct(path, &function.owner, name)
                            }
                            super::CallKind::Property => {
                                resolver.property(path, &function.owner, name)
                            }
                        };
                        match &resolution {
                            DeclarationResolution::Invalid { reason }
                            | DeclarationResolution::Ambiguous { reason }
                            | DeclarationResolution::Unverified { reason } => {
                                output.unresolved.insert(
                                    (path.clone(), function.owner.clone(), name.into(), kind),
                                    reason.clone(),
                                );
                            }
                            _ => (),
                        }
                        identity(resolution)
                    };
                    if let Some(symbol) = symbol {
                        output.call.insert(
                            (path.clone(), function.owner.clone(), name.into(), kind),
                            symbol.clone(),
                        );
                        output.push(
                            ReferenceSite {
                                path,
                                side,
                                owner: &function.owner,
                                line: position.line,
                                column: position.column,
                            },
                            name,
                            kind.label(),
                            symbol,
                            None,
                        );
                    }
                }
            }
            for body in calls.get(path).into_iter().flatten() {
                let Some(super::FunctionBehavior::Algorithm(source)) = &body.behavior else {
                    continue;
                };
                let Some(function) = callable.get(body.owner.as_str()) else {
                    continue;
                };
                let lines = source.lines().collect::<Vec<_>>();
                for reference in forge_diff::syntax::algorithm_references(source)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?
                {
                    let key = AlgorithmLocation {
                        path: path.clone(),
                        owner: body.owner.clone(),
                        line: reference.line as u32,
                        column: reference.column as u32 - 1,
                    };
                    let length = reference.segment.len();
                    let width = lines[reference.line - 1].len();
                    let Ok(Some(super::calls::AlgorithmTarget::Reference(target))) =
                        super::calls::resolve_algorithm_reference(path, function, body, reference)
                    else {
                        continue;
                    };
                    let resolution = match target.kind {
                        super::CallKind::Call | super::CallKind::Callback => {
                            resolver.callable(path, &body.owner, &target.name)
                        }
                        super::CallKind::Construct => {
                            resolver.construct(path, &body.owner, &target.name)
                        }
                        super::CallKind::Property => {
                            resolver.property(path, &body.owner, &target.name)
                        }
                        super::CallKind::Value => resolver.value(path, &body.owner, &target.name),
                    };
                    if let Some(symbol) = identity(resolution) {
                        output.algorithm.insert(
                            key,
                            ResolvedAlgorithmReference {
                                symbol,
                                length,
                                line_width: width,
                            },
                        );
                    }
                }
            }
            output.declaration.insert(path.clone(), index);
        }
        let mut capture_by_path = BTreeMap::new();
        let mut capture_by_identity = BTreeMap::new();
        for (path, index) in &output.declaration {
            let mut capture_by_owner = BTreeMap::new();
            for symbol in &index.symbol {
                if symbol.parameter {
                    continue;
                }
                let capture = match symbol.role {
                    DeclarationRole::Callable => "@function",
                    DeclarationRole::Type => "@type",
                    DeclarationRole::Property => "@variable.member",
                    DeclarationRole::Module => "@module",
                    DeclarationRole::Value | DeclarationRole::Variant => "@constant",
                    DeclarationRole::Binding => "@variable",
                };
                capture_by_owner.insert(
                    symbol
                        .scope
                        .iter()
                        .chain(std::iter::once(&symbol.name))
                        .cloned()
                        .collect::<Vec<_>>()
                        .join("::"),
                    capture,
                );
                if let Some((identity, _)) = output.definition.get(&(
                    path.clone(),
                    symbol.position.line,
                    symbol.position.column,
                )) {
                    capture_by_identity.insert(identity.as_str(), capture);
                }
            }
            capture_by_path.insert(path.as_str(), capture_by_owner);
        }
        for reference in &mut output.occurrence {
            let owner = if reference.owner.is_empty() {
                &reference.name
            } else {
                &reference.owner
            };
            let mut prefix = String::new();
            let parts = owner.split("::").collect::<Vec<_>>();
            for (position, part) in parts.iter().enumerate() {
                if !prefix.is_empty() {
                    prefix.push_str("::");
                }
                prefix.push_str(part);
                let capture = if reference.owner.is_empty() && position + 1 == parts.len() {
                    capture_by_identity.get(reference.symbol.as_str()).copied()
                } else {
                    capture_by_path
                        .get(reference.path.as_str())
                        .and_then(|capture| capture.get(&prefix))
                        .copied()
                };
                reference.owner_capture.push(capture);
            }
        }
        Ok(output)
    }

    fn push(
        &mut self,
        site: ReferenceSite<'_>,
        name: &str,
        kind: &str,
        symbol: String,
        anchor: Option<PlanNavigationAnchor>,
    ) {
        let ReferenceSite {
            path,
            side,
            owner,
            line,
            column,
        } = site;
        let id = super::digest(
            format!("{side}:{path}:{owner}:{name}:{kind}:{line}:{column}").as_bytes(),
        );
        self.occurrence.push(PlanReference {
            id,
            path: path.into(),
            side: side.into(),
            owner: owner.into(),
            owner_capture: Vec::new(),
            name: name.into(),
            kind: kind.into(),
            line,
            column,
            symbol,
            anchor,
        });
    }

    fn legacy(document: &PlanDocument, workspace: &std::path::Path) -> Result<Self> {
        let rendered = super::render_plan_at(document, workspace)?;
        let canonical = crate::plan::codec::to_value(document)?;
        let mut output = Self::default();
        let mut seen = BTreeSet::new();
        for anchor in rendered.navigation.anchor {
            let (name, symbol, usage) = match &anchor.target {
                PlanReviewTarget::Entity { name }
                | PlanReviewTarget::FileTreeEntity { name, .. } => {
                    (name.clone(), format!("planned:{name}"), false)
                }
                PlanReviewTarget::EntityMember { entity, member } => (
                    format!("{entity}::{member}"),
                    format!("planned:{entity}::{member}"),
                    false,
                ),
                PlanReviewTarget::FlowStep {
                    target_name,
                    reference_kind,
                    workspace_path,
                    workspace_line,
                    ..
                }
                | PlanReviewTarget::FlowEdge {
                    target_name,
                    reference_kind,
                    workspace_path,
                    workspace_line,
                    ..
                } => {
                    let identity = match reference_kind {
                        super::PlanReviewReferenceKind::PlannedEntity => {
                            format!("planned:{target_name}")
                        }
                        super::PlanReviewReferenceKind::WorkspaceEntity => format!(
                            "workspace:{}:{}:{target_name}",
                            workspace_path.as_deref().unwrap_or_default(),
                            workspace_line.unwrap_or_default()
                        ),
                        super::PlanReviewReferenceKind::ExternalEntity => {
                            format!("external:{target_name}")
                        }
                    };
                    (target_name.clone(), identity, true)
                }
                PlanReviewTarget::Subtask { .. } | PlanReviewTarget::Test { .. } => {
                    let value = canonical.pointer(&anchor.json_path);
                    let names = value
                        .and_then(|value| {
                            value
                                .get("entities")
                                .or_else(|| value.get("covers_entities"))
                        })
                        .and_then(serde_json::Value::as_array);
                    for name in names
                        .into_iter()
                        .flatten()
                        .filter_map(serde_json::Value::as_str)
                    {
                        if !seen.insert((anchor.json_path.clone(), name.to_owned())) {
                            continue;
                        }
                        output.push(
                            ReferenceSite {
                                path: anchor.path.as_deref().unwrap_or("plan"),
                                side: "proposed",
                                owner: &anchor.label,
                                line: anchor.line,
                                column: 0,
                            },
                            name,
                            "task",
                            format!("planned:{name}"),
                            Some(anchor.clone()),
                        );
                    }
                    continue;
                }
                _ => continue,
            };
            output
                .structured
                .insert(anchor.json_path.clone(), symbol.clone());
            if usage && seen.insert((anchor.json_path.clone(), name.clone())) {
                output.push(
                    ReferenceSite {
                        path: anchor.path.as_deref().unwrap_or("plan"),
                        side: "proposed",
                        owner: &anchor.label,
                        line: anchor.line,
                        column: 0,
                    },
                    &name,
                    "flow",
                    symbol,
                    Some(anchor.clone()),
                );
            }
        }
        Ok(output)
    }

    /// Locate a Calls definition in the selected snapshot or its unchanged source symbol.
    pub(crate) fn destination(
        &self,
        document: &PlanDocument,
        workspace: &std::path::Path,
        identity: &str,
        baseline: bool,
    ) -> Result<Option<crate::declaration::DeclarationDestination>> {
        let Some(((path, line, column), _)) = self
            .definition
            .iter()
            .find(|(_, (symbol, _))| symbol == identity)
        else {
            return Ok(None);
        };
        let Some(design) = document.design.as_ref() else {
            return Ok(None);
        };
        let text = if baseline {
            &design.baseline[path].text
        } else {
            &design.proposed[path]
        };
        let callable = DeclarationCalls::extract(path, text, true)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let index =
            DeclarationIndex::extract(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let symbol = index
            .symbol
            .iter()
            .find(|symbol| symbol.position.line == *line && symbol.position.column == *column);
        let function = callable
            .iter()
            .find(|function| function.line == *line && function.column == *column);
        let name = symbol
            .map(|symbol| symbol.name.clone())
            .or_else(|| function.map(|function| function.owner.clone()))
            .unwrap_or_default();
        let destination = crate::declaration::DeclarationDestination {
            path: workspace.join(path).to_string_lossy().into_owned(),
            line: *line,
            column: *column,
            proposed: true,
            name,
            module_file: false,
        };
        Ok(Some(destination))
    }

    /// Require a unique proposed definition that has no corresponding source declaration.
    pub(crate) fn rename_definition(
        &self,
        document: &PlanDocument,
        identity: &str,
    ) -> Result<(String, forge_diff::syntax::DeclarationSymbol)> {
        let design = document
            .design
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("rename requires a declaration plan"))?;
        let candidates = self
            .definition
            .iter()
            .filter(|(_, (symbol, _))| symbol == identity)
            .collect::<Vec<_>>();
        let [((path, line, column), _)] = candidates.as_slice() else {
            anyhow::bail!("select a unique symbol defined in the plan");
        };
        let index = self
            .declaration
            .get(path)
            .ok_or_else(|| anyhow::anyhow!("selected declaration index is unavailable"))?;
        let symbol = index
            .symbol
            .iter()
            .find(|symbol| symbol.position.line == *line && symbol.position.column == *column)
            .cloned()
            .or_else(|| {
                path.ends_with(".lua")
                    .then(|| lua_symbol(path, &design.proposed[path], *line, *column))
                    .flatten()
            })
            .ok_or_else(|| {
                anyhow::anyhow!("selected definition is not a renameable declaration")
            })?;
        ensure!(
            !symbol.parameter && !symbol.conditional,
            "parameter and conditional definitions cannot be renamed"
        );
        ensure!(
            index
                .symbol
                .iter()
                .filter(|candidate| candidate.name == symbol.name
                    && candidate.property == symbol.property
                    && same_scope(&candidate.scope, &symbol.scope))
                .count()
                <= 1,
            "merged or overloaded definitions require a unique symbol identity before rename"
        );
        ensure!(
            self.introduced.contains(&((*path).clone(), *line, *column)),
            "only symbols introduced by the plan can be renamed"
        );
        let origin = design
            .moved
            .iter()
            .find(|(_, destination)| *destination == path)
            .map(|(source, _)| source)
            .unwrap_or(path);
        if let Some(file) = design.baseline.get(origin)
            && self.baseline_digest.get(origin) != Some(&super::digest(file.text.as_bytes()))
        {
            let previous = DeclarationIndex::extract(origin, &file.text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            ensure!(
                !previous
                    .symbol
                    .iter()
                    .any(|previous| symbol_key(previous) == symbol_key(&symbol)),
                "only symbols introduced by the plan can be renamed"
            );
        }
        Ok(((*path).clone(), symbol))
    }

    /// Rename resolved usages atomically while preserving extraction order and immutable baselines.
    pub(crate) fn renamed(
        &self,
        document: &PlanDocument,
        identity: &str,
        name: &str,
    ) -> Result<PlanDocument> {
        let original_design = document
            .design
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("rename requires a declaration plan"))?;
        let (definition_path, symbol) = self.rename_definition(document, identity)?;
        let identifier = if symbol.name.starts_with('#') {
            name.strip_prefix('#').ok_or_else(|| {
                anyhow::anyhow!("private property names must retain their # prefix")
            })?
        } else {
            name
        };
        ensure!(
            !identifier.is_empty()
                && identifier
                    .chars()
                    .enumerate()
                    .all(|(position, character)| character == '_'
                        || if position == 0 {
                            character.is_alphabetic()
                        } else {
                            character.is_alphanumeric()
                        }),
            "enter one valid identifier"
        );
        ensure!(name != symbol.name, "new name matches the current name");
        let keyword = if definition_path.ends_with(".rs") {
            "as break const continue crate else enum extern false fn for if impl in let loop match mod move mut pub ref return self Self static struct super trait true type unsafe use where while async await dyn abstract become box do final macro override priv typeof unsized virtual yield try gen"
        } else if definition_path.ends_with(".lua") {
            "and break do else elseif end false for function goto if in local nil not or repeat return then true until while"
        } else {
            "break case catch class const continue debugger default delete do else enum export extends false finally for function if import in instanceof new null return super switch this throw true try typeof var void while with yield let static implements interface package private protected public await"
        };
        ensure!(
            !keyword.split_whitespace().any(|keyword| keyword == name),
            "reserved language keywords cannot be symbol names"
        );

        let mut output = document.clone();
        let mut design = original_design.clone();
        let mut algorithm_edits = BTreeMap::<(String, String), BTreeSet<(u32, u32)>>::new();
        for (location, reference) in &self.algorithm {
            if reference.symbol == identity {
                algorithm_edits
                    .entry((location.path.clone(), location.owner.clone()))
                    .or_default()
                    .insert((location.line, location.column));
            }
        }
        for ((path, owner), positions) in algorithm_edits {
            let body = design
                .proposed_calls
                .get_mut(&path)
                .into_iter()
                .flatten()
                .find(|body| body.owner == owner)
                .ok_or_else(|| anyhow::anyhow!("Algorithm owner is unavailable"))?;
            let Some(super::FunctionBehavior::Algorithm(source)) = &mut body.behavior else {
                continue;
            };
            *source = super::source_edit::SourceRevision::new(source)
                .prepare_tokens(positions, &symbol.name, name)
                .map_err(|error| anyhow::anyhow!("Algorithm rename token changed: {error:#}"))?
                .apply();
        }
        let index = DeclarationIndex::extract(&definition_path, &design.proposed[&definition_path])
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        ensure!(
            !index.symbol.iter().any(|candidate| candidate.name == name
                && (!definition_path.ends_with(".rs") || candidate.property == symbol.property)
                && same_scope(&candidate.scope, &symbol.scope))
                && !index.import.iter().any(|import| !import.export
                    && import.alias.as_deref() == Some(name)
                    && same_scope(&import.scope, &symbol.scope)),
            "a symbol or import with this name already exists in this scope"
        );
        if definition_path.ends_with(".lua") {
            let owner = symbol
                .scope
                .iter()
                .cloned()
                .chain(std::iter::once(name.to_owned()))
                .collect::<Vec<_>>()
                .join(".");
            ensure!(
                !DeclarationCalls::extract(
                    &definition_path,
                    &design.proposed[&definition_path],
                    true
                )
                .map_err(|error| anyhow::anyhow!("{error:?}"))?
                .iter()
                .any(|function| function.owner.replace(':', ".") == owner),
                "a symbol with this name already exists in this scope"
            );
        }
        let mut edits = BTreeMap::<String, BTreeSet<(u32, u32)>>::new();
        edits
            .entry(definition_path)
            .or_default()
            .insert((symbol.position.line, symbol.position.column));
        for reference in self.occurrence.iter().filter(|reference| {
            reference.symbol == identity
                && !matches!(
                    reference.kind.as_str(),
                    "call" | "property" | "callback" | "value" | "construct" | "body_qualifier"
                )
        }) {
            let text = &design.proposed[&reference.path];
            let row = text
                .lines()
                .nth(reference.line as usize - 1)
                .unwrap_or_default();
            let start = reference.column as usize;
            let tail = row.get(start..).unwrap_or_default();
            let extent = if !matches!(reference.kind.as_str(), "import" | "export") {
                reference.name.len()
            } else {
                tail.find(|character: char| {
                    character == ';' || character == ',' || character.is_whitespace()
                })
                .unwrap_or(tail.len())
            };
            let token = tail.get(..extent).unwrap_or(tail);
            if let Some(offset) = token.rfind(&symbol.name) {
                let before = &token[..offset];
                let after = &token[offset + symbol.name.len()..];
                if before
                    .chars()
                    .last()
                    .is_none_or(|character| !character.is_alphanumeric() && character != '_')
                    && after
                        .chars()
                        .next()
                        .is_none_or(|character| !character.is_alphanumeric() && character != '_')
                {
                    edits
                        .entry(reference.path.clone())
                        .or_default()
                        .insert((reference.line, reference.column + offset as u32));
                }
            }
        }
        let qualifier = self
            .occurrence
            .iter()
            .filter(|reference| reference.kind == "body_qualifier")
            .map(|reference| {
                (
                    (
                        reference.path.as_str(),
                        reference.owner.as_str(),
                        reference.name.as_str(),
                    ),
                    reference.symbol.as_str(),
                )
            })
            .collect::<BTreeMap<_, _>>();
        for (path, functions) in &mut design.proposed_calls {
            for function in functions {
                for call in function.call.iter_mut().flatten() {
                    if call.unresolved {
                        continue;
                    }
                    let selected = self
                        .call
                        .get(&(
                            path.clone(),
                            function.owner.clone(),
                            call.name.clone(),
                            call.kind,
                        ))
                        .is_some_and(|target| target == identity);
                    let separator = if path.ends_with(".rs") { "::" } else { "." };
                    let normalized = call
                        .name
                        .replace(':', if separator == "." { "." } else { ":" });
                    let parts = normalized.split(separator).collect::<Vec<_>>();
                    let mut replacement = parts
                        .iter()
                        .map(|part| (*part).to_owned())
                        .collect::<Vec<_>>();
                    for (position, part) in parts.iter().enumerate() {
                        if *part != symbol.name {
                            continue;
                        }
                        let matches = if position + 1 == parts.len() {
                            selected
                        } else {
                            qualifier
                                .get(&(
                                    path.as_str(),
                                    function.owner.as_str(),
                                    parts[..=position].join(separator).as_str(),
                                ))
                                .copied()
                                == Some(identity)
                        };
                        if matches {
                            replacement[position] = name.into();
                        }
                    }
                    let mut offset = 0;
                    let mut edits = Vec::new();
                    for (replacement, original) in replacement.iter().zip(&parts) {
                        if replacement != original {
                            edits.push(super::source_edit::SourceReplacement {
                                range: offset..offset + original.len(),
                                expected: original,
                                replacement,
                            });
                        }
                        offset += original.len() + separator.len();
                    }
                    if !edits.is_empty() {
                        call.name = super::source_edit::SourceRevision::new(&call.name)
                            .prepare(edits)?
                            .apply();
                    }
                }
            }
        }
        for (path, text) in &mut design.proposed {
            let Some(positions) = edits.remove(path) else {
                continue;
            };
            *text = super::source_edit::SourceRevision::new(text)
                .prepare_tokens(positions, &symbol.name, name)?
                .apply();
        }
        ensure!(
            edits.is_empty(),
            "rename source changed outside the proposal"
        );
        for (path, functions) in &mut design.proposed_calls {
            if design.proposed.get(path) == original_design.proposed.get(path) {
                continue;
            }
            let before = DeclarationCalls::extract(path, &original_design.proposed[path], true)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let after = DeclarationCalls::extract(path, &design.proposed[path], true)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            ensure!(
                before.len() == after.len(),
                "rename changed declaration structure"
            );
            for function in functions {
                let position = before
                    .iter()
                    .position(|callable| callable.owner == function.owner)
                    .ok_or_else(|| anyhow::anyhow!("call owner is unavailable"))?;
                function.owner.clone_from(&after[position].owner);
            }
        }
        design.validation = None;
        for (path, text) in &design.proposed {
            let previous = original_design;
            if previous.proposed.get(path) == Some(text)
                && previous.proposed_calls.get(path) == design.proposed_calls.get(path)
            {
                continue;
            }
            forge_diff::syntax::DeclarationOverview::parse(path, text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let functions = design
                .proposed_calls
                .get(path)
                .map(Vec::as_slice)
                .unwrap_or_default();
            let combined = super::calls::combined(path, text, functions)?;
            let (_, parsed) = super::calls::parse(path, &combined, functions)?;
            ensure!(
                parsed.len() == functions.len(),
                "rename changed callable ownership"
            );
        }
        output.design = Some(design);
        output.version = output
            .version
            .checked_add(1)
            .ok_or_else(|| anyhow::anyhow!("plan version overflow"))?;
        Ok(output)
    }

    /// Identify a symbol at a saved declaration position or a Calls entry.
    pub(crate) fn selected(&self, anchor: &PlanNavigationAnchor, column: u32) -> Option<&str> {
        match &anchor.target {
            PlanReviewTarget::Call {
                path,
                owner,
                name,
                kind,
                ..
            } => self
                .call
                .get(&(path.clone(), owner.clone(), name.clone(), *kind))
                .map(String::as_str),
            PlanReviewTarget::Declaration { path, line, .. } => self
                .occurrence
                .iter()
                .filter(|reference| {
                    !matches!(
                        reference.kind.as_str(),
                        "call" | "property" | "callback" | "value" | "construct" | "body_qualifier"
                    ) && reference.path == *path
                        && reference.line == *line
                        && reference.column <= column
                        && column < reference.column + reference.name.len() as u32
                })
                .min_by_key(|reference| reference.name.len())
                .map(|reference| reference.symbol.as_str())
                .or_else(|| {
                    self.definition
                        .range((path.clone(), *line, 0)..=(path.clone(), *line, column))
                        .next_back()
                        .filter(|((_, _, start), (_, length))| column < *start + *length as u32)
                        .map(|(_, (symbol, _))| symbol.as_str())
                }),
            _ => self.structured.get(&anchor.json_path).map(String::as_str),
        }
    }

    /// Select an Algorithm token after removing its presentation indentation.
    pub(crate) fn selected_algorithm(
        &self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Option<&str> {
        let PlanReviewTarget::Behavior {
            path,
            owner,
            offset,
            side,
        } = &anchor.target
        else {
            return None;
        };
        if side != "proposed" {
            return None;
        }
        self.algorithm
            .range(
                AlgorithmLocation {
                    path: path.clone(),
                    owner: owner.clone(),
                    line: *offset,
                    column: 0,
                }..=AlgorithmLocation {
                    path: path.clone(),
                    owner: owner.clone(),
                    line: *offset,
                    column: u32::MAX,
                },
            )
            .find_map(|(location, reference)| {
                let selected = column.checked_sub(row.len().checked_sub(reference.line_width)?)?;
                (location.column as usize <= selected
                    && selected < location.column as usize + reference.length)
                    .then_some(reference.symbol.as_str())
            })
    }
}

fn same_scope(previous: &[String], proposed: &[String]) -> bool {
    previous
        .iter()
        .filter(|scope| !scope.starts_with("@impl:"))
        .eq(proposed.iter().filter(|scope| !scope.starts_with("@impl:")))
}

fn symbol_key(
    symbol: &forge_diff::syntax::DeclarationSymbol,
) -> (String, forge_diff::syntax::DeclarationRole, Vec<String>) {
    (
        symbol.name.clone(),
        symbol.role,
        symbol
            .scope
            .iter()
            .filter(|scope| !scope.starts_with("@impl:"))
            .cloned()
            .collect(),
    )
}

fn lua_symbol(
    path: &str,
    text: &str,
    line: u32,
    column: u32,
) -> Option<forge_diff::syntax::DeclarationSymbol> {
    let functions = DeclarationCalls::extract(path, text, true).ok()?;
    let function = functions
        .iter()
        .find(|function| function.line == line && function.column == column)?;
    let normalized = function.owner.replace(':', ".");
    let mut scope = normalized.split('.').map(str::to_owned).collect::<Vec<_>>();
    let name = scope.pop()?;
    Some(forge_diff::syntax::DeclarationSymbol {
        role: forge_diff::syntax::DeclarationRole::Callable,
        external_entry: false,
        property: false,
        position: forge_diff::syntax::DeclarationPosition {
            line,
            column: column + function.owner.len() as u32 - name.len() as u32,
        },
        name,
        scope,
        visibility: forge_diff::syntax::SymbolVisibility::Public,
        type_namespace: false,
        value_namespace: true,
        macro_namespace: false,
        conditional: false,
        global: false,
        parameter: false,
    })
}

fn identity(resolution: DeclarationResolution) -> Option<String> {
    match resolution {
        DeclarationResolution::Resolved { destination } => Some(format!(
            "{}:{}:{}",
            destination.path.replace('\\', "/"),
            destination.line,
            destination.column
        )),
        _ => None,
    }
}

fn lua_identity(path: &str, owner: &str) -> String {
    format!("lua:{path}:{owner}")
}

fn lua_target(
    path: &str,
    name: &str,
    index: &DeclarationIndex,
    functions: &BTreeMap<(String, String), (u32, u32)>,
) -> Option<String> {
    let name = name.replace(':', ".");
    if functions.contains_key(&(path.into(), name.clone())) {
        return Some(lua_identity(path, &name));
    }
    let (prefix, member) = name.split_once('.')?;
    let import = index
        .import
        .iter()
        .find(|import| import.alias.as_deref() == Some(prefix))?;
    let module = import.source.as_deref()?.replace('.', "/");
    let candidates = functions
        .keys()
        .filter(|(file, owner)| {
            (file.ends_with(&format!("{module}.lua"))
                || file.ends_with(&format!("{module}/init.lua")))
                && owner.rsplit('.').next() == Some(member)
        })
        .collect::<Vec<_>>();
    if let [(file, owner)] = candidates.as_slice() {
        Some(lua_identity(file, owner))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rename_updates_recursive_owner_and_rejects_scope_collisions() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("rename", "Rename");
        let mut design = super::super::DeclarationDesign::default();
        design.document.objective = "Rename the API".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Keep all plan references consistent".into();
        design.proposed.insert(
            "client.ts".into(),
            "export function send(): void;\nexport function dispatch(): void;\n".into(),
        );
        design.proposed_calls.insert(
            "client.ts".into(),
            vec![super::super::FunctionBody {
                evidence: None,
                behavior: Some(crate::plan::FunctionBehavior::Change(
                    "Stop retrying authentication failures in send.".into(),
                )),
                owner: "send".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let identity = index.call.values().next().unwrap();
        assert!(index.renamed(&document, identity, "dispatch").is_err());
        let renamed = index.renamed(&document, identity, "deliver").unwrap();
        let function = &renamed.design.as_ref().unwrap().proposed_calls["client.ts"][0];
        assert_eq!(function.owner, "deliver");
        assert_eq!(function.call.as_ref().unwrap()[0].name, "deliver");
        assert_eq!(
            function
                .behavior
                .as_ref()
                .map(crate::plan::FunctionBehavior::text),
            Some("Stop retrying authentication failures in send.")
        );
        assert_eq!(index.call.len(), 1);
    }

    #[test]
    fn existing_methods_remain_protected_after_impl_order_changes() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname='guard'\nversion='0.1.0'\nedition='2021'\n",
        )
        .unwrap();
        let source = "pub struct Client;\nimpl Client { pub fn send(&self) {} }\n";
        std::fs::write(workspace.path().join("src/lib.rs"), source).unwrap();
        let mut document = super::super::document::test_fixture("guard", "Guard");
        let mut design = super::super::DeclarationDesign::default();
        let baseline =
            forge_diff::syntax::DeclarationOverview::extract("src/lib.rs", source).unwrap();
        design.baseline.insert(
            "src/lib.rs".into(),
            super::super::DeclarationFile {
                text: baseline,
                source_digest: super::super::digest(source.as_bytes()),
            },
        );
        design.proposed.insert("src/lib.rs".into(),"pub struct Extra;\nimpl Extra { pub fn prepare(&self); }\npub struct Client;\nimpl Client { pub fn send(&self); }\npub fn run();\n".into());
        design.proposed_calls.insert(
            "src/lib.rs".into(),
            vec![super::super::FunctionBody {
                evidence: None,
                behavior: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "Client::send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        let identity = index.call.values().next().unwrap();
        assert!(
            index
                .rename_definition(&document, identity)
                .unwrap_err()
                .to_string()
                .contains("introduced")
        );
        let destination = index
            .destination(&document, workspace.path(), identity, false)
            .unwrap()
            .unwrap();
        assert!(destination.proposed);
        assert_eq!(destination.line, 4);
    }

    #[test]
    fn rust_references_use_saved_calls_and_distinguish_type_tokens() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname = \"references\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        std::fs::write(workspace.path().join("src/lib.rs"), "pub struct Client;\n").unwrap();
        let mut document = super::super::document::test_fixture("calls", "Calls");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("src/lib.rs".into(), "pub struct Client;\nimpl Client {\n  pub fn send(&self, other: &Client);\n}\npub fn run(client: &Client);\npub struct Settings { pub client: Client }\n".into());
        design.proposed_calls.insert(
            "src/lib.rs".into(),
            vec![super::super::FunctionBody {
                evidence: None,
                behavior: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "Client::send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        let method_reference = index
            .occurrence
            .iter()
            .find(|reference| reference.owner.ends_with("::send") && reference.kind == "type")
            .unwrap();
        assert_eq!(
            method_reference.owner_capture,
            vec![Some("@type"), None, Some("@function")]
        );
        let property_reference = index
            .occurrence
            .iter()
            .find(|reference| reference.owner == "Settings::client")
            .unwrap();
        assert_eq!(
            property_reference.owner_capture,
            vec![Some("@type"), Some("@variable.member")]
        );
        let call = index
            .call
            .get(&(
                "src/lib.rs".into(),
                "run".into(),
                "Client::send".into(),
                super::super::CallKind::Call,
            ))
            .expect("call resolves");
        assert_eq!(
            index
                .occurrence
                .iter()
                .filter(|reference| &reference.symbol == call && reference.kind == "call")
                .count(),
            1
        );
        let anchor = PlanNavigationAnchor {
            line: 1,
            target: PlanReviewTarget::Declaration {
                path: "src/lib.rs".into(),
                side: "proposed".into(),
                line: 5,
                column: Some(0),
            },
            json_path: String::new(),
            path: None,
            label: String::new(),
        };
        let function = index.selected(&anchor, 8).unwrap();
        assert_eq!(
            function,
            index
                .definition
                .get(&("src/lib.rs".into(), 5, 7))
                .unwrap()
                .0
        );
        let parameter_type = index.selected(&anchor, 22).unwrap();
        assert_ne!(function, parameter_type);
    }

    #[test]
    fn typescript_cross_file_calls_resolve_import_identity() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("calls", "Calls");
        let mut design = super::super::DeclarationDesign::default();
        design
            .proposed
            .insert("client.ts".into(), "export function send(): void;\n".into());
        design.proposed.insert(
            "run.ts".into(),
            "import { send } from './client';\nexport function run(): void;\n".into(),
        );
        design.proposed_calls.insert(
            "run.ts".into(),
            vec![super::super::FunctionBody {
                evidence: None,
                behavior: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        assert!(index.call.contains_key(&(
            "run.ts".into(),
            "run".into(),
            "send".into(),
            super::super::CallKind::Call
        )));
    }

    #[test]
    fn opaque_call_evidence_blocks_name_only_reference_resolution() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("opaque", "Opaque calls");
        let mut design = super::super::DeclarationDesign::default();
        design
            .proposed
            .insert("client.ts".into(), "export function send(): void;\n".into());
        design.proposed.insert(
            "run.ts".into(),
            "import { send } from './client';\nexport function run(send: () => void): void;\n"
                .into(),
        );
        design.proposed_calls.insert(
            "run.ts".into(),
            vec![super::super::FunctionBody {
                evidence: None,
                behavior: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "send".into(),
                    source: None,
                    unresolved: true,
                }]),
            }],
        );
        design.document.objective = "Rename the exported send API".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Preserve opaque callback bindings".into();
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        let key = (
            "run.ts".into(),
            "run".into(),
            "send".into(),
            super::super::CallKind::Call,
        );
        assert!(!index.call.contains_key(&key));
        assert!(index.unresolved.contains_key(&key));
        let identity = &index
            .definition
            .get(&("client.ts".into(), 1, 16))
            .unwrap()
            .0;
        let renamed = index.renamed(&document, identity, "dispatch").unwrap();
        assert!(
            renamed.design.as_ref().unwrap().proposed["run.ts"].contains("import { dispatch }")
        );
        assert!(renamed.design.as_ref().unwrap().proposed["run.ts"].contains("run(send:"));
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["run.ts"][0]
                .call
                .as_ref()
                .unwrap()[0]
                .name,
            "send"
        );
    }
}
