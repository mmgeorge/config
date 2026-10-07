use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};
use std::path::Path;

use anyhow::Result;
use forge_diff::syntax::{DeclarationRole, SymbolVisibility};

use super::{DeclarationResolution, DeclarationResolver, IndexedFile};
use crate::plan::DeclarationDesign;

type Scope = (String, Vec<String>);
type Position = (String, u32, u32);

/// Determine external contracts through saved exports and bounded owning-module metadata.
pub(crate) fn symbols(workspace: &Path, design: &DeclarationDesign) -> Result<BTreeSet<Position>> {
    let mut metadata = design.clone();
    let mut inspected = BTreeSet::new();
    for path in design.proposed.keys().filter(|path| path.ends_with(".rs")) {
        for directory in Path::new(path).ancestors().skip(1) {
            let manifest = directory
                .join("Cargo.toml")
                .to_string_lossy()
                .replace('\\', "/");
            if read(workspace, &manifest, &mut metadata, &mut inspected)? {
                let value: toml::Value = toml::from_str(&metadata.proposed[&manifest])?;
                let root = value
                    .get("lib")
                    .and_then(|library| library.get("path"))
                    .and_then(toml::Value::as_str)
                    .unwrap_or("src/lib.rs");
                read(
                    workspace,
                    &directory.join(root).to_string_lossy().replace('\\', "/"),
                    &mut metadata,
                    &mut inspected,
                )?;
                read(
                    workspace,
                    &directory
                        .join("src/main.rs")
                        .to_string_lossy()
                        .replace('\\', "/"),
                    &mut metadata,
                    &mut inspected,
                )?;
                break;
            }
        }
        for directory in Path::new(path).ancestors().skip(1) {
            if directory.as_os_str().is_empty() {
                break;
            }
            let direct = directory
                .with_extension("rs")
                .to_string_lossy()
                .replace('\\', "/");
            if !read(workspace, &direct, &mut metadata, &mut inspected)? {
                read(
                    workspace,
                    &directory
                        .join("mod.rs")
                        .to_string_lossy()
                        .replace('\\', "/"),
                    &mut metadata,
                    &mut inspected,
                )?;
            }
        }
    }
    let mut resolver = DeclarationResolver::planned(workspace, &metadata, false)?;
    let mut children = BTreeMap::<Scope, Vec<(IndexedFile, usize)>>::new();
    let mut exports = BTreeMap::<Scope, Vec<(IndexedFile, usize)>>::new();
    let mut queue = VecDeque::new();
    let mut public = BTreeSet::new();
    let files = resolver.file.values().cloned().collect::<Vec<_>>();
    let root_package = files
        .iter()
        .filter(|file| file.module.is_empty())
        .map(|file| file.package.clone())
        .collect::<HashSet<_>>();
    for file in &files {
        let root = if file.package.is_empty() {
            file.path.to_string_lossy().into_owned()
        } else {
            file.package.clone()
        };
        let loose = !file.package.is_empty()
            && resolver
                .package
                .get(&file.package)
                .is_some_and(|package| package.source_manifest.is_none())
            && !root_package.contains(&file.package);
        if file.package.is_empty()
            || file.module.is_empty() && library_root(&resolver, file)
            || loose
        {
            queue.push_back((
                root.clone(),
                if loose {
                    file.module.clone()
                } else {
                    Vec::new()
                },
            ));
        }
        for (position, symbol) in file.index.symbol.iter().enumerate() {
            let key = location(
                workspace,
                file,
                symbol.position.line,
                symbol.position.column,
            );
            if symbol.external_entry
                || symbol.role == DeclarationRole::Callable
                    && symbol.name == "main"
                    && symbol.scope.is_empty()
                    && binary_root(&resolver, file)
            {
                public.insert(key);
            }
            let mut owner = file.module.clone();
            owner.extend(
                symbol
                    .scope
                    .iter()
                    .filter(|scope| !scope.starts_with("@impl:"))
                    .cloned(),
            );
            children
                .entry((root.clone(), owner))
                .or_default()
                .push((file.clone(), position));
        }
        for (position, import) in file.index.import.iter().enumerate() {
            if import.export && import.visibility == SymbolVisibility::Public && !import.conditional
            {
                let mut owner = file.module.clone();
                owner.extend(import.scope.clone());
                exports
                    .entry((root.clone(), owner))
                    .or_default()
                    .push((file.clone(), position));
            }
        }
    }
    let mut visited = HashSet::new();
    while let Some(scope) = queue.pop_front() {
        if !visited.insert(scope.clone()) {
            continue;
        }
        for (file, position) in children.get(&scope).into_iter().flatten() {
            let symbol = &file.index.symbol[*position];
            if symbol.parameter || symbol.visibility != SymbolVisibility::Public && !symbol.global {
                continue;
            }
            public.insert(location(
                workspace,
                file,
                symbol.position.line,
                symbol.position.column,
            ));
            let mut child = scope.1.clone();
            child.push(symbol.name.clone());
            queue.push_back((scope.0.clone(), child));
        }
        for (file, position) in exports.get(&scope).into_iter().flatten() {
            let import = &file.index.import[*position];
            let relative = file
                .path
                .strip_prefix(workspace)?
                .to_string_lossy()
                .replace('\\', "/");
            let mut resolution =
                resolver.at(&relative, import.position.line, import.position.column);
            if !matches!(resolution, DeclarationResolution::Resolved { .. }) && !import.glob {
                resolution = resolver.value(
                    &relative,
                    &import.scope.join("::"),
                    import
                        .alias
                        .as_deref()
                        .unwrap_or_else(|| import.path.last().map(String::as_str).unwrap_or("")),
                );
            }
            if let DeclarationResolution::Resolved { destination } = resolution {
                if let Some(target) = resolver.file.get(Path::new(&destination.path)) {
                    if let Some(symbol) = target.index.symbol.iter().find(|symbol| {
                        symbol.position.line == destination.line
                            && symbol.position.column == destination.column
                    }) {
                        if !target.package.is_empty()
                            && symbol.visibility != SymbolVisibility::Public
                            && !(import.glob && symbol.role == DeclarationRole::Module)
                        {
                            continue;
                        }
                        public.insert(location(
                            workspace,
                            target,
                            symbol.position.line,
                            symbol.position.column,
                        ));
                        let root = if target.package.is_empty() {
                            target.path.to_string_lossy().into_owned()
                        } else {
                            target.package.clone()
                        };
                        let mut owner = target.module.clone();
                        owner.extend(
                            symbol
                                .scope
                                .iter()
                                .filter(|scope| !scope.starts_with("@impl:"))
                                .cloned(),
                        );
                        owner.push(symbol.name.clone());
                        queue.push_back((root, owner));
                    }
                }
            }
        }
    }
    for (path, text) in design
        .proposed
        .iter()
        .filter(|(path, _)| path.ends_with(".lua"))
    {
        let index = forge_diff::syntax::DeclarationIndex::extract(path, text)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        for symbol in index
            .symbol
            .iter()
            .filter(|symbol| symbol.visibility == SymbolVisibility::Public)
        {
            public.insert((path.clone(), symbol.position.line, symbol.position.column));
        }
    }
    Ok(public)
}

fn read(
    workspace: &Path,
    path: &str,
    design: &mut DeclarationDesign,
    inspected: &mut BTreeSet<String>,
) -> Result<bool> {
    if design.proposed.contains_key(path) {
        return Ok(true);
    }
    if design.baseline.contains_key(path) || !inspected.insert(path.into()) {
        return Ok(false);
    }
    anyhow::ensure!(
        inspected.len() <= 256,
        "public API exposure exceeds 256 owning metadata probes"
    );
    if let Some(text) = crate::plan::workspace_source(workspace, path)? {
        anyhow::ensure!(
            design.proposed.len() <= 16384,
            "public API exposure exceeds 16384 files"
        );
        design.proposed.insert(path.into(), text);
        Ok(true)
    } else {
        Ok(false)
    }
}

fn location(workspace: &Path, file: &IndexedFile, line: u32, column: u32) -> Position {
    (
        file.path
            .strip_prefix(workspace)
            .unwrap_or(&file.path)
            .to_string_lossy()
            .replace('\\', "/"),
        line,
        column,
    )
}

fn library_root(resolver: &DeclarationResolver, file: &IndexedFile) -> bool {
    if file.path.file_name().is_some_and(|name| name == "main.rs") {
        return false;
    }
    let manifest = resolver
        .package
        .get(&file.package)
        .and_then(|package| package.source_manifest.as_ref());
    manifest.is_none()
        || manifest
            .and_then(|path| resolver.read(path))
            .and_then(|text| toml::from_str::<toml::Value>(&text).ok())
            .is_some_and(|manifest| {
                let root = manifest
                    .get("lib")
                    .and_then(|library| library.get("path"))
                    .and_then(toml::Value::as_str)
                    .unwrap_or("src/lib.rs");
                resolver.package[&file.package]
                    .source_manifest
                    .as_ref()
                    .unwrap()
                    .parent()
                    .unwrap()
                    .join(root)
                    == file.path
            })
}

fn binary_root(resolver: &DeclarationResolver, file: &IndexedFile) -> bool {
    if file.path.file_name().is_some_and(|name| name == "main.rs") {
        return true;
    }
    if file
        .path
        .parent()
        .and_then(Path::file_name)
        .is_some_and(|name| name == "examples" || name == "bin")
    {
        return true;
    }
    resolver
        .package
        .get(&file.package)
        .and_then(|package| package.source_manifest.as_ref())
        .and_then(|path| resolver.read(path).map(|text| (path, text)))
        .and_then(|(path, text)| {
            toml::from_str::<toml::Value>(&text)
                .ok()
                .map(|manifest| (path, manifest))
        })
        .is_some_and(|(path, manifest)| {
            manifest
                .get("bin")
                .and_then(toml::Value::as_array)
                .into_iter()
                .flatten()
                .any(|target| {
                    target
                        .get("path")
                        .and_then(toml::Value::as_str)
                        .is_some_and(|target| path.parent().unwrap().join(target) == file.path)
                })
        })
}
