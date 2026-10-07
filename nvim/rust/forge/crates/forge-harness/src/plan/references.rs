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
        let mut resolver = DeclarationResolver::local(workspace, design, baseline)?;
        resolver.bound_reference_files();
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
    pub(crate) fn design(design: &super::DeclarationDesign, workspace: &std::path::Path) -> Result<Self> {
        Self::extract(design, false, DeclarationResolver::planned(workspace, design, false)?)
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
            let index = std::sync::Arc::new(DeclarationIndex::extract(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?);
            for symbol in &index.symbol {
                lua_function.insert((path.clone(), symbol.scope.iter().chain(std::iter::once(&symbol.name)).cloned().collect::<Vec<_>>().join(".")), (symbol.position.line, symbol.position.column));
            }
            lua_index.insert(path.clone(), index);

        }
        for (path, text) in &files {
            if forge_diff::syntax::ConfigurationFormat::for_path(path).is_some() {
                continue;
            }
            let index = match resolver.index(path) {
                Some(index) => index,
                None => match lua_index.get(path) { Some(index) => index.clone(), None => std::sync::Arc::new(DeclarationIndex::extract(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?) },
            };
            if !baseline {
                let origin = design.moved.iter().find(|(_, destination)| *destination == path).map(|(source, _)| source).unwrap_or(path);
                let previous = design.baseline.get(origin).map(|file| DeclarationIndex::extract(origin, &file.text)).transpose().map_err(|error| anyhow::anyhow!("{error:?}"))?;
                let existing = previous.iter().flat_map(|index| &index.symbol).map(symbol_key).collect::<BTreeSet<_>>();
                if let Some(file) = design.baseline.get(origin) { output.baseline_digest.insert(origin.clone(), super::digest(file.text.as_bytes())); }
                for symbol in &index.symbol {
                    if !symbol.parameter && !existing.contains(&symbol_key(symbol)) {
                        output.introduced.insert((path.clone(), symbol.position.line, symbol.position.column));
                    }
                }
            }
            for symbol in &index.symbol {
                if symbol.parameter && symbol.name == "Self" {
                    continue;
                }
                if let Some(identity) = if path.ends_with(".lua") { Some(lua_identity(path, &symbol.scope.iter().chain(std::iter::once(&symbol.name)).cloned().collect::<Vec<_>>().join("."))) } else {
                    let resolution = resolver.at(path, symbol.position.line, symbol.position.column);
                    if let DeclarationResolution::Resolved { destination } = &resolution && destination.module_file {
                        output.module_source.insert((path.clone(), symbol.position.line, symbol.position.column), destination.path.clone().into());
                    }
                    identity(resolution) }
                {
                    output.definition.insert(
                        (path.clone(), symbol.position.line, symbol.position.column),
                        (identity, symbol.name.len()),
                    );
                }
            }
            for reference in &index.reference {
                if let Some(symbol) = if path.ends_with(".lua") { lua_target(path, &reference.path.join("."), &index, &lua_function) } else {
                    identity(resolver.at(path, reference.position.line, reference.position.column)) }
                {
                    output.push(
                        path,
                        side,
                        &reference.scope.join("::"),
                        &reference.path.join("::"),
                        if index.symbol.iter().any(|symbol| symbol.parameter && symbol.name == "Self" && symbol.position == reference.position) { "owner" } else { "type" },
                        reference.position.line,
                        reference.position.column,
                        symbol,
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
                            path,
                            side,
                            &reference.scope.join("::"),
                            part,
                            "qualifier",
                            reference.position.line,
                            offset as u32,
                            symbol,
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
                        path,
                        side,
                        &import.scope.join("::"),
                        &import.path.join("::"),
                        "import",
                        import.position.line,
                        import.position.column,
                        symbol,
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
                        let target = if path.ends_with(".lua") { lua_target(path, &parts[..prefix].join(separator), &index, &lua_function) } else { identity(resolver.type_target(path, &function.owner.split(separator).map(str::to_owned).collect::<Vec<_>>(), &parts[..prefix])) };
                        if let Some(symbol) = target {
                            output.push(path, side, &function.owner, &parts[..prefix].join(separator), "body_qualifier", position.line, position.column, symbol);
                        }
                    }
                    let symbol = if path.ends_with(".lua") {
                        lua_target(path, name, &index, &lua_function)
                    } else {
                        let resolution = match kind {
                            super::CallKind::Call | super::CallKind::Callback => resolver.callable(path, &function.owner, name),
                            super::CallKind::Value => resolver.value(path, &function.owner, name),
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
                            path,
                            side,
                            &function.owner,
                            name,
                            kind.label(),
                            position.line,
                            position.column,
                            symbol,
                        );
                    }
                }
            }
            output.declaration.insert(path.clone(), index);
        }
        ensure!(
            output.occurrence.len() <= 65536,
            "plan reference index exceeds 65536 occurrences"
        );
        let mut capture_by_path = BTreeMap::new();
        let mut capture_by_identity = BTreeMap::new();
        for (path, index) in &output.declaration {
            let mut capture_by_owner = BTreeMap::new();
            for symbol in &index.symbol {
                if symbol.parameter { continue; }
                let capture = match symbol.role {
                    DeclarationRole::Callable => "@function",
                    DeclarationRole::Type => "@type",
                    DeclarationRole::Property => "@variable.member",
                    DeclarationRole::Module => "@module",
                    DeclarationRole::Value | DeclarationRole::Variant => "@constant",
                    DeclarationRole::Binding => "@variable",
                };
                capture_by_owner.insert(symbol.scope.iter().chain(std::iter::once(&symbol.name)).cloned().collect::<Vec<_>>().join("::"), capture);
                if let Some((identity, _)) = output.definition.get(&(path.clone(), symbol.position.line, symbol.position.column)) {
                    capture_by_identity.insert(identity.as_str(), capture);
                }
            }
            capture_by_path.insert(path.as_str(), capture_by_owner);
        }
        for reference in &mut output.occurrence {
            let owner = if reference.owner.is_empty() { &reference.name } else { &reference.owner };
            let mut prefix = String::new();
            let parts = owner.split("::").collect::<Vec<_>>();
            for (position, part) in parts.iter().enumerate() {
                if !prefix.is_empty() { prefix.push_str("::"); }
                prefix.push_str(part);
                let capture = if reference.owner.is_empty() && position + 1 == parts.len() {
                    capture_by_identity.get(reference.symbol.as_str()).copied()
                } else {
                    capture_by_path.get(reference.path.as_str()).and_then(|capture| capture.get(&prefix)).copied()
                };
                reference.owner_capture.push(capture);
            }
        }
        Ok(output)
    }

    fn push(
        &mut self,
        path: &str,
        side: &str,
        owner: &str,
        name: &str,
        kind: &str,
        line: u32,
        column: u32,
        symbol: String,
    ) {
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
            anchor: None,
        });
    }

    fn legacy(document: &PlanDocument, workspace: &std::path::Path) -> Result<Self> {
        let rendered = super::render_plan_at(document, workspace)?;
        let canonical = serde_json::to_value(document)?;
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
                            anchor.path.as_deref().unwrap_or("plan"),
                            "proposed",
                            &anchor.label,
                            name,
                            "task",
                            anchor.line,
                            0,
                            format!("planned:{name}"),
                        );
                        output.occurrence.last_mut().unwrap().anchor = Some(anchor.clone());
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
                    anchor.path.as_deref().unwrap_or("plan"),
                    "proposed",
                    &anchor.label,
                    &name,
                    "flow",
                    anchor.line,
                    0,
                    symbol,
                );
                output.occurrence.last_mut().unwrap().anchor = Some(anchor);
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
        let design = document.design.as_ref().unwrap();
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
        let mut destination = crate::declaration::DeclarationDestination {
            path: workspace.join(path).to_string_lossy().into_owned(),
            line: *line,
            column: *column,
            proposed: true,
            name,
            module_file: false,
        };
        if !baseline && let Some(original) = design.baseline.get(path) {
            let previous = DeclarationIndex::extract(path, &original.text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let existed = symbol.is_some_and(|symbol| {
                previous.symbol.iter().any(|previous| {
                    previous.name == symbol.name && same_scope(&previous.scope, &symbol.scope)
                })
            }) || function.is_some_and(|function| {
                DeclarationCalls::extract(path, &original.text, true).is_ok_and(|previous| {
                    previous
                        .iter()
                        .any(|previous| previous.owner == function.owner)
                })
            });
            if existed {
                let Some(source) = super::design::workspace_source(workspace, path)? else {
                    return Ok(Some(destination));
                };
                let original = DeclarationIndex::extract(path, &source)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                let position = symbol.and_then(|symbol| {
                    original
                        .symbol
                        .iter()
                        .find(|original| {
                            original.name == symbol.name && original.scope == symbol.scope
                        })
                        .map(|symbol| symbol.position)
                });
                let position = position.or_else(|| {
                    function.and_then(|function| {
                        DeclarationCalls::extract(path, &source, false)
                            .ok()?
                            .into_iter()
                            .find(|original| original.owner == function.owner)
                            .map(|function| forge_diff::syntax::DeclarationPosition {
                                line: function.line,
                                column: function.column,
                            })
                    })
                });
                if let Some(position) = position {
                    destination.line = position.line;
                    destination.column = position.column;
                    destination.proposed = false;
                }
            }
        }
        Ok(Some(destination))
    }

    /// Resolve an uncaptured Lua require target by reading bounded module candidates lazily.
    pub(crate) fn lua_source_call(
        document: &PlanDocument,
        workspace: &std::path::Path,
        path: &str,
        name: &str,
        baseline: bool,
    ) -> Result<Option<crate::declaration::DeclarationDestination>> {
        if !path.ends_with(".lua") {
            return Ok(None);
        }
        let design = document.design.as_ref().unwrap();
        let text = if baseline {
            &design.baseline[path].text
        } else {
            &design.proposed[path]
        };
        let index =
            DeclarationIndex::extract(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let normalized = name.replace(':', ".");
        let Some((alias, member)) = normalized.split_once('.') else {
            return Ok(None);
        };
        let Some(module) = index
            .import
            .iter()
            .find(|import| import.alias.as_deref() == Some(alias))
            .and_then(|import| import.source.as_deref())
        else {
            return Ok(None);
        };
        let module = module.replace('.', "/");
        let mut candidates = BTreeSet::new();
        for directory in std::path::Path::new(path).ancestors().skip(1).take(16) {
            for prefix in [directory.to_path_buf(), directory.join("lua")] {
                candidates.insert(
                    prefix
                        .join(format!("{module}.lua"))
                        .to_string_lossy()
                        .replace('\\', "/"),
                );
                candidates.insert(
                    prefix
                        .join(format!("{module}/init.lua"))
                        .to_string_lossy()
                        .replace('\\', "/"),
                );
            }
        }
        let mut destination = Vec::new();
        for path in candidates {
            if design.proposed.contains_key(&path) || design.baseline.contains_key(&path) {
                continue;
            }
            let Some(source) = super::design::workspace_source(workspace, &path)? else {
                continue;
            };
            for function in DeclarationCalls::extract(&path, &source, false)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?
            {
                if function
                    .owner
                    .replace(':', ".")
                    .split_once('.')
                    .map_or(function.owner.as_str(), |(_, member)| member)
                    == member
                {
                    destination.push(crate::declaration::DeclarationDestination {
                        path: workspace.join(&path).to_string_lossy().into_owned(),
                        line: function.line,
                        column: function.column,
                        proposed: false,
                        name: function.owner,
                        module_file: false,
                    });
                }
            }
        }
        Ok(if destination.len() == 1 {
            destination.pop()
        } else {
            None
        })
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
        let index = self.declaration.get(path).ok_or_else(|| anyhow::anyhow!("selected declaration index is unavailable"))?;
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
        ensure!(self.introduced.contains(&((*path).clone(), *line, *column)), "only symbols introduced by the plan can be renamed");
        let origin = design.moved.iter().find(|(_, destination)| *destination == path).map(|(source, _)| source).unwrap_or(path);
        if let Some(file) = design.baseline.get(origin) && self.baseline_digest.get(origin) != Some(&super::digest(file.text.as_bytes())) {
            let previous = DeclarationIndex::extract(origin, &file.text).map_err(|error| anyhow::anyhow!("{error:?}"))?;
            ensure!(!previous.symbol.iter().any(|previous| symbol_key(previous) == symbol_key(&symbol)), "only symbols introduced by the plan can be renamed");
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
        let design = output.design.as_mut().unwrap();
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
            reference.symbol == identity && !matches!(reference.kind.as_str(), "call" | "property" | "callback" | "value" | "body_qualifier")
        }) {
            let text = &design.proposed[&reference.path];
            let row = text
                .lines()
                .nth(reference.line as usize - 1)
                .unwrap_or_default();
            let start = reference.column as usize;
            let tail = row.get(start..).unwrap_or_default();
            let extent = if reference.kind != "import" {
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
        let qualifier = self.occurrence.iter().filter(|reference| reference.kind == "body_qualifier")
            .map(|reference| ((reference.path.as_str(), reference.owner.as_str(), reference.name.as_str()), reference.symbol.as_str())).collect::<BTreeMap<_, _>>();
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
                            qualifier.get(&(path.as_str(), function.owner.as_str(), parts[..=position].join(separator).as_str())).copied() == Some(identity)
                        };
                        if matches {
                            replacement[position] = name.into();
                        }
                    }
                    let mut offset = 0;
                    let mut edits = Vec::new();
                    for (replacement, original) in replacement.iter().zip(&parts) {
                        if replacement != original {
                            edits.push((offset, original.len(), replacement));
                        }
                        offset += original.len() + separator.len();
                    }
                    for (offset, length, replacement) in edits.into_iter().rev() {
                        call.name
                            .replace_range(offset..offset + length, replacement);
                    }
                }
            }
        }
        for (path, positions) in edits {
            let text = design.proposed.get_mut(&path).unwrap();
            let mut offsets = vec![0];
            for (offset, byte) in text.bytes().enumerate() {
                if byte == b'\n' {
                    offsets.push(offset + 1);
                }
            }
            for (line, column) in positions.iter().rev() {
                let start = offsets[*line as usize - 1] + *column as usize;
                ensure!(
                    text.get(start..start + symbol.name.len()) == Some(symbol.name.as_str()),
                    "rename token changed"
                );
                text.replace_range(start..start + symbol.name.len(), name);
            }
        }
        for (path, functions) in &mut design.proposed_calls {
            let before = DeclarationCalls::extract(
                path,
                &document.design.as_ref().unwrap().proposed[path],
                true,
            )
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
        design.validate()?;
        output.version = output
            .version
            .checked_add(1)
            .ok_or_else(|| anyhow::anyhow!("plan version overflow"))?;
        output.validate_for_submission()?;
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
                    !matches!(reference.kind.as_str(), "call" | "property" | "callback" | "value" | "body_qualifier")
                        && reference.path == *path
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
}

fn same_scope(previous: &[String], proposed: &[String]) -> bool {
    previous
        .iter()
        .filter(|scope| !scope.starts_with("@impl:"))
        .eq(proposed.iter().filter(|scope| !scope.starts_with("@impl:")))
}

fn symbol_key(symbol: &forge_diff::syntax::DeclarationSymbol) -> (String, forge_diff::syntax::DeclarationRole, Vec<String>) {
    (symbol.name.clone(), symbol.role, symbol.scope.iter().filter(|scope| !scope.starts_with("@impl:")).cloned().collect())
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
    fn property_reference_index_handles_large_ordered_occurrence_lists() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("scale", "Property scale");
        let mut design = super::super::DeclarationDesign::default();
        let mut declaration = String::from("pub struct Client {\n");
        for field in 0..1000 {
            declaration.push_str(&format!("pub field_{field}: usize,\n"));
        }
        declaration.push_str("}\npub fn run(client: &Client);\n");
        design.proposed.insert("lib.rs".into(), declaration);
        let mut source = String::from("fn run(client: &Client) {\n");
        for field in 0..1000 {
            for _ in 0..10 {
                source.push_str(&format!("client.field_{field};\n"));
            }
        }
        source.push_str("}\n");
        let started = std::time::Instant::now();
        let capture = super::super::calls::extract("lib.rs", &source).unwrap();
        let extraction = started.elapsed();
        assert_eq!(capture[0].call.as_ref().unwrap().len(), 10000);
        design.proposed_calls.insert("lib.rs".into(), capture);
        document.design = Some(design);
        let started = std::time::Instant::now();
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let indexing = started.elapsed();
        assert_eq!(index.call.len(), 1000);
        assert_eq!(
            index
                .occurrence
                .iter()
                .filter(|reference| reference.kind == "property")
                .count(),
            1000
        );
        assert!(index.unresolved.is_empty());
        eprintln!(
            "property scale: 10000 occurrences, 1000 unique targets, capture={extraction:?}, index={indexing:?}"
        );
    }

    #[test]
    fn typescript_properties_follow_aliases_and_include_methods_taken_as_values() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document =
            super::super::document::test_fixture("properties", "Inherited properties");
        let mut design = super::super::DeclarationDesign::default();
        design.document.task = "Introduce a property and a callable member".into();
        design.document.description = "Resolve their inherited and value references".into();
        design.proposed.insert("client.ts".into(), "export interface Base { count: number; send(): void; }\nexport interface Derived extends Base {}\nexport type Alias = Derived;\nexport type Cycle = Cycle;\n".into());
        design.proposed.insert("run.ts".into(), "import { Alias, Cycle } from './client';\nfunction run(client: Alias, cyclic: Cycle): void;\n".into());
        design.proposed_calls.insert("run.ts".into(), super::super::calls::extract("run.ts", "function run(client: Alias, cyclic: Cycle) { client.count++; const callback = client.send; cyclic.count; }").unwrap());
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        assert!(!index.call.contains_key(&(
            "run.ts".into(),
            "run".into(),
            "Cycle.count".into(),
            super::super::CallKind::Property
        )));
        assert!(
            index.unresolved[&(
                "run.ts".into(),
                "run".into(),
                "Cycle.count".into(),
                super::super::CallKind::Property
            )]
                .contains("cyclic")
        );
        let property = index
            .call
            .get(&(
                "run.ts".into(),
                "run".into(),
                "Alias.count".into(),
                super::super::CallKind::Property,
            ))
            .expect("inherited aliased property should resolve");
        let renamed = index
            .renamed(&document, property, "total")
            .unwrap();
        assert!(renamed.design.as_ref().unwrap().proposed["client.ts"].contains("total: number"));
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["run.ts"][0].call.as_ref().unwrap()[0].name,
            "Alias.total"
        );
        let method = index
            .call
            .get(&(
                "run.ts".into(),
                "run".into(),
                "Alias.send".into(),
                super::super::CallKind::Property,
            ))
            .expect("method value should resolve");
        let renamed = index
            .renamed(&document, method, "dispatch")
            .unwrap();
        assert!(renamed.design.as_ref().unwrap().proposed["client.ts"].contains("dispatch()"));
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["run.ts"][0].call.as_ref().unwrap()[1].name,
            "Alias.dispatch"
        );
    }

    #[test]
    fn property_uses_resolve_by_owner_and_rename_only_introduced_fields() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document =
            super::super::document::test_fixture("properties", "Property references");
        let mut design = super::super::DeclarationDesign::default();
        design.document.task = "Add a count field".into();
        design.document.description =
            "Preserve the property identity across calls and accesses".into();
        let baseline =
            "pub struct Client { pub existing: usize }\nimpl Client { pub fn count(&self); }\n";
        let proposed = "pub struct Client { pub existing: usize, pub count: usize }\nimpl Client { pub fn count(&self); }\npub struct Other { pub count: usize }\n";
        design.baseline.insert(
            "client.rs".into(),
            super::super::DeclarationFile {
                text: baseline.into(),
                source_digest: "captured".into(),
            },
        );
        design.proposed.insert("client.rs".into(), proposed.into());
        let caller = "use crate::client::{Client, Other};\npub fn run(client: &mut Client, other: &Other, opaque: Unknown);\n";
        design.proposed.insert("lib.rs".into(), caller.into());
        design.proposed.insert(
            "Cargo.toml".into(),
            "[package]\nname='properties'\nversion='0.1.0'\nedition='2024'\n[lib]\npath='lib.rs'\n"
                .into(),
        );
        // The module declaration gives the snapshot resolver ownership of the captured file.
        design
            .proposed
            .get_mut("lib.rs")
            .unwrap()
            .insert_str(0, "mod client;\n");
        let source = "use crate::client::{Client, Other}; fn run(client: &mut Client, other: &Other, opaque: Unknown) { client.count += 1; client.count(); other.count; opaque.count; client.existing; }";
        design.proposed_calls.insert(
            "lib.rs".into(),
            super::super::calls::extract("lib.rs", source).unwrap(),
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let key = (
            "lib.rs".into(),
            "run".into(),
            "Client::count".into(),
            super::super::CallKind::Property,
        );
        let identity = index
            .call
            .get(&key)
            .expect("property target should resolve");
        let method = index
            .call
            .get(&(
                "lib.rs".into(),
                "run".into(),
                "Client::count".into(),
                super::super::CallKind::Call,
            ))
            .expect("method target should resolve");
        assert_ne!(identity, method);
        let anchor = PlanNavigationAnchor {
            line: 1,
            target: PlanReviewTarget::Call {
                kind: super::super::CallKind::Property,
                path: "lib.rs".into(),
                side: "proposed".into(),
                owner: "run".into(),
                name: "Client::count".into(),
            },
            json_path: String::new(),
            path: Some("lib.rs".into()),
            label: String::new(),
        };
        assert_eq!(index.selected(&anchor, 0), Some(identity.as_str()));
        let destination = index
            .destination(&document, workspace.path(), identity, false)
            .unwrap()
            .unwrap();
        assert!(destination.proposed);
        assert_eq!(destination.name, "count");
        assert_eq!(
            index
                .occurrence
                .iter()
                .filter(|reference| reference.symbol == *identity && reference.kind == "property")
                .count(),
            1
        );
        let renamed = index
            .renamed(&document, identity, "total")
            .unwrap();
        let design = renamed.design.as_ref().unwrap();
        assert!(design.proposed["client.rs"].contains("pub total: usize"));
        assert!(design.proposed["client.rs"].contains("fn count"));
        assert!(design.proposed["client.rs"].contains("Other { pub count"));
        let reference = design.proposed_calls["lib.rs"][0].call.as_ref().unwrap();
        assert_eq!(
            reference
                .iter()
                .map(|reference| (reference.name.as_str(), reference.kind))
                .collect::<Vec<_>>(),
            [
                ("Client::total", super::super::CallKind::Property),
                ("Client::count", super::super::CallKind::Call),
                ("Other::count", super::super::CallKind::Property),
                ("Unknown::count", super::super::CallKind::Property),
                ("Client::existing", super::super::CallKind::Property)
            ]
        );
        assert_eq!(design.baseline, document.design.as_ref().unwrap().baseline);
        let existing = index
            .call
            .get(&(
                "lib.rs".into(),
                "run".into(),
                "Client::existing".into(),
                super::super::CallKind::Property,
            ))
            .unwrap();
        assert!(index.rename_definition(&document, existing).is_err());
        assert!(!index.call.contains_key(&(
            "lib.rs".into(),
            "run".into(),
            "Unknown::count".into(),
            super::super::CallKind::Property
        )));
        let combined = super::super::calls::combined(
            "lib.rs",
            &design.proposed["lib.rs"],
            &design.proposed_calls["lib.rs"],
        )
        .unwrap();
        assert!(combined.contains("Calls\n  Client::count"));
        assert!(combined.contains("Accesses\n  Client::total"));
        let (_, round_trip) =
            super::super::calls::parse("lib.rs", &combined, &design.proposed_calls["lib.rs"])
                .unwrap();
        assert_eq!(round_trip, design.proposed_calls["lib.rs"]);
    }

    #[test]
    fn rename_updates_cross_file_calls_in_order_without_touching_baselines() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("rename", "Rename");
        let mut design = super::super::DeclarationDesign::default();
        design.document.task = "Rename the API".into();
        design.document.description = "Keep all plan references consistent".into();
        design
            .proposed
            .insert("client.ts".into(), "export function send(): void;\n".into());
        design.proposed.insert(
            "run.ts".into(),
            "import { send } from './client';\nexport function run(): void;\n".into(),
        );
        design.proposed_calls.insert(
            "run.ts".into(),
            vec![super::super::FunctionBody { change: None,
                owner: "run".into(),
                call: Some(vec![
                    super::super::CallSite {
                        kind: crate::plan::CallKind::Call,
                        name: "send".into(),
                        source: None,
                        unresolved: false
                    };
                    2
                ]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let identity = index.call.values().next().unwrap();
        let renamed = index
            .renamed(&document, identity, "dispatch")
            .unwrap();
        let design = renamed.design.unwrap();
        assert!(design.proposed["client.ts"].contains("function dispatch"));
        assert!(design.proposed["run.ts"].contains("import { dispatch }"));
        assert_eq!(
            design.proposed_calls["run.ts"][0]
                .call.iter().flatten()
                .map(|call| call.name.as_str())
                .collect::<Vec<_>>(),
            vec!["dispatch", "dispatch"]
        );
        assert_eq!(
            document.design.as_ref().unwrap().proposed_calls["run.ts"][0].call.as_ref().unwrap()[0].name,
            "send"
        );
        assert!(
            index
                .renamed(&document, identity, "function")
                .is_err()
        );
        std::fs::write(workspace.path().join("client.ts"), "invalid source {").unwrap();
        let snapshot = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let identity = snapshot.call.values().next().unwrap();
        assert!(
            snapshot
                .renamed(&document, identity, "dispatch")
                .is_ok()
        );
        document.design.as_mut().unwrap().baseline.insert(
            "client.ts".into(),
            super::super::DeclarationFile {
                text: "export function send(): void;\n".into(),
                source_digest: "saved".into(),
            },
        );
        assert!(
            snapshot
                .rename_definition(&document, identity)
                .unwrap_err()
                .to_string()
                .contains("introduced")
        );
    }

    #[test]
    fn rename_updates_recursive_owner_and_rejects_scope_collisions() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("rename", "Rename");
        let mut design = super::super::DeclarationDesign::default();
        design.document.task = "Rename the API".into();
        design.document.description = "Keep all plan references consistent".into();
        design.proposed.insert(
            "client.ts".into(),
            "export function send(): void;\nexport function dispatch(): void;\n".into(),
        );
        design.proposed_calls.insert(
            "client.ts".into(),
            vec![super::super::FunctionBody { change: Some("Stop retrying authentication failures in send.".into()),
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
        assert!(
            index
                .renamed(&document, identity, "dispatch")
                .is_err()
        );
        let renamed = index
            .renamed(&document, identity, "deliver")
            .unwrap();
        let function = &renamed.design.as_ref().unwrap().proposed_calls["client.ts"][0];
        assert_eq!(function.owner, "deliver");
        assert_eq!(function.call.as_ref().unwrap()[0].name, "deliver");
        assert_eq!(function.change.as_deref(), Some("Stop retrying authentication failures in send."));
        assert_eq!(index.call.len(), 1);
    }

    #[test]
    fn rust_type_rename_updates_method_qualifiers_and_function_owners() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname='rename'\nversion='0.1.0'\nedition='2021'\n",
        )
        .unwrap();
        let mut document = super::super::document::test_fixture("rename", "Rename");
        let mut design = super::super::DeclarationDesign::default();
        design.document.task = "Rename the owner".into();
        design.document.description = "Keep Calls consistent".into();
        design.proposed.insert("src/lib.rs".into(),"pub struct Client { pub endpoint: u32 }\nimpl Client {\n    pub fn send(&self);\n}\npub fn run(client: &Client);\n".into());
        design.proposed_calls.insert(
            "src/lib.rs".into(),
            vec![
                super::super::FunctionBody { change: None,
                    owner: "Client::send".into(),
                    call: Some(Vec::new()),
                },
                super::super::FunctionBody { change: None,
                    owner: "run".into(),
                    call: Some(vec![super::super::CallSite {
                        kind: crate::plan::CallKind::Call,
                        name: "Client::send".into(),
                        source: None,
                        unresolved: false,
                    }]),
                },
            ],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let identity = &index
            .definition
            .get(&("src/lib.rs".into(), 1, 11))
            .unwrap()
            .0;
        let renamed = index
            .renamed(&document, identity, "Transport")
            .unwrap();
        let design = renamed.design.unwrap();
        assert!(design.proposed["src/lib.rs"].contains("impl Transport"));
        assert!(design.proposed["src/lib.rs"].contains("client: &Transport"));
        assert_eq!(
            design.proposed_calls["src/lib.rs"][0].owner,
            "Transport::send"
        );
        assert_eq!(
            design.proposed_calls["src/lib.rs"][1].call.as_ref().unwrap()[0].name,
            "Transport::send"
        );
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
            vec![super::super::FunctionBody { change: None,
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
        assert!(!destination.proposed);
        assert_eq!(destination.line, 2);
    }

    #[test]
    fn lua_source_calls_read_only_the_required_module() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(workspace.path().join("lua")).unwrap();
        std::fs::write(
            workspace.path().join("lua/client.lua"),
            "local M = {}\nfunction M.send() return 1 end\nreturn M\n",
        )
        .unwrap();
        std::fs::write(
            workspace.path().join("lua/unrelated.lua"),
            "invalid source {",
        )
        .unwrap();
        let mut document = super::super::document::test_fixture("lua", "Lua");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert(
            "lua/run.lua".into(),
            "local client = require('client')\nlocal function run()\n".into(),
        );
        document.design = Some(design);
        let destination = PlanReferenceIndex::lua_source_call(
            &document,
            workspace.path(),
            "lua/run.lua",
            "client.send",
            false,
        )
        .unwrap()
        .unwrap();
        assert!(
            destination.path.ends_with("lua/client.lua")
                || destination.path.ends_with("lua\\client.lua")
        );
        assert_eq!(destination.line, 2);
        assert!(!destination.proposed);
        assert_eq!(document.design.as_ref().unwrap().proposed.len(), 1);
    }

    #[test]
    fn lua_calls_jump_to_saved_definitions() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("lua", "Lua");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert(
            "client.lua".into(),
            "local M\nfunction M.send()\nreturn M\n".into(),
        );
        design.proposed.insert(
            "run.lua".into(),
            "local client = require('client')\nlocal function run()\n".into(),
        );
        design.proposed_calls.insert(
            "run.lua".into(),
            vec![super::super::FunctionBody { change: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "client.send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        let identity = index.call.values().next().unwrap();
        let destination = index
            .destination(&document, workspace.path(), identity, false)
            .unwrap()
            .unwrap();
        assert!(destination.path.ends_with("client.lua"));
        assert_eq!(destination.line, 2);
        assert!(destination.proposed);
        document.design.as_mut().unwrap().document.task = "Rename Lua API".into();
        document.design.as_mut().unwrap().document.description = "Update saved calls".into();
        let renamed = index
            .renamed(&document, identity, "deliver")
            .unwrap();
        assert!(renamed.design.as_ref().unwrap().proposed["client.lua"].contains("M.deliver"));
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["run.lua"][0].call.as_ref().unwrap()[0].name,
            "client.deliver"
        );
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
            vec![super::super::FunctionBody { change: None,
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
        let method_reference = index.occurrence.iter().find(|reference| reference.owner.ends_with("::send") && reference.kind == "type").unwrap();
        assert_eq!(method_reference.owner_capture, vec![Some("@type"), None, Some("@function")]);
        let property_reference = index.occurrence.iter().find(|reference| reference.owner == "Settings::client").unwrap();
        assert_eq!(property_reference.owner_capture, vec![Some("@type"), Some("@variable.member")]);
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
            vec![super::super::FunctionBody { change: None,
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
    fn lua_require_calls_resolve_saved_module_functions() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = super::super::document::test_fixture("lua", "Lua calls");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert(
            "client.lua".into(),
            "local M\nfunction M.send()\nreturn M\n".into(),
        );
        design.proposed.insert(
            "run.lua".into(),
            "local client = require(\"client\")\nfunction run()\n".into(),
        );
        design.proposed_calls.insert(
            "run.lua".into(),
            vec![super::super::FunctionBody { change: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "client.send".into(),
                    source: None,
                    unresolved: false,
                }]),
            }],
        );
        document.design = Some(design);
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        assert!(index.call.contains_key(&(
            "run.lua".into(),
            "run".into(),
            "client.send".into(),
            super::super::CallKind::Call
        )));
    }

    #[test]
    fn lua_module_rename_updates_declaration_owners_and_body_qualifiers() {
        let workspace = tempfile::tempdir().unwrap();
        let source = "local M = {}\nfunction M.run() M.other() end\nfunction M.other() end\nreturn M\n";
        let mut document = super::super::document::test_fixture("module", "Rename module");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("api.lua".into(), forge_diff::syntax::DeclarationOverview::extract("api.lua", source).unwrap());
        design.proposed_calls.insert("api.lua".into(), super::super::calls::extract("api.lua", source).unwrap());
        design.document.task = "Rename the module".into();
        design.document.description = "Preserve module definitions and calls".into();
        document.design = Some(design);
        let index = PlanReferenceIndex::planned(&document, workspace.path()).unwrap();
        let symbol = index.declaration["api.lua"].symbol.iter().find(|symbol| symbol.name == "M").unwrap();
        let identity = &index.definition[&("api.lua".into(), symbol.position.line, symbol.position.column)].0;
        let renamed = index.renamed(&document, identity, "Module").unwrap();
        let design = renamed.design.unwrap();
        assert!(design.proposed["api.lua"].contains("function Module.run"));
        assert!(design.proposed["api.lua"].contains("return Module"));
        assert_eq!(design.proposed_calls["api.lua"][0].owner, "Module.run");
        assert_eq!(design.proposed_calls["api.lua"][0].call.as_ref().unwrap()[0].name, "Module.other");
    }

    #[test]
    fn legacy_references_include_explicit_tasks_and_flows_without_prose_matches() {
        let workspace = tempfile::tempdir().unwrap();
        let document = super::super::document::test_fixture("legacy", "Legacy references");
        let index = PlanReferenceIndex::build(&document, workspace.path(), false).unwrap();
        assert!(
            index
                .occurrence
                .iter()
                .any(|reference| reference.kind == "flow")
        );
        assert!(
            index
                .occurrence
                .iter()
                .any(|reference| reference.kind == "task")
        );
        assert!(
            index
                .occurrence
                .iter()
                .all(|reference| reference.kind == "task" || reference.kind == "flow")
        );
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
            vec![super::super::FunctionBody { change: None,
                owner: "run".into(),
                call: Some(vec![super::super::CallSite {
                    kind: crate::plan::CallKind::Call,
                    name: "send".into(),
                    source: None,
                    unresolved: true,
                }]),
            }],
        );
        design.document.task = "Rename the exported send API".into();
        design.document.description = "Preserve opaque callback bindings".into();
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
        let renamed = index
            .renamed(&document, identity, "dispatch")
            .unwrap();
        assert!(
            renamed.design.as_ref().unwrap().proposed["run.ts"].contains("import { dispatch }")
        );
        assert!(renamed.design.as_ref().unwrap().proposed["run.ts"].contains("run(send:"));
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["run.ts"][0].call.as_ref().unwrap()[0].name,
            "send"
        );
    }
}
