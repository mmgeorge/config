use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use forge_diff::syntax::{ConfigurationFormat, DeclarationIndex};

use super::DeclarationDesign;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct ReviewFile {
    pub baseline: String,
    pub proposed: String,
}

#[derive(Clone, Debug)]
struct Package {
    manifest: String,
    directory: String,
    name: String,
    language: Language,
    root: Vec<(String, u8)>,
}

#[derive(Clone, Debug)]
struct Workspace {
    path: String,
    directory: String,
    language: Language,
    member: Vec<String>,
    exclude: Vec<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Language {
    Rust,
    Typescript,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Edge {
    destination: String,
    kind: u8,
    position: u32,
}

pub(super) fn order(design: &DeclarationDesign) -> Vec<ReviewFile> {
    let mut source = design.proposed.clone();
    for (path, file) in &design.baseline {
        if design.moved.contains_key(path) {
            continue;
        }
        source
            .entry(path.clone())
            .or_insert_with(|| file.text.clone());
    }
    let packages = discover_packages(&source);
    let workspaces = discover_workspaces(&source);
    let package_workspace = assign_workspaces(&packages, &workspaces);
    let owner = assign_owners(&source, &packages);
    let mut edge = HashMap::<String, Vec<Edge>>::new();
    for (path, text) in &source {
        let mut outgoing = Vec::new();
        match extension(path) {
            "rs" | "ts" | "tsx" | "mts" | "cts" | "lua" => {
                if let Ok(index) = DeclarationIndex::extract(path, text) {
                    match extension(path) {
                        "rs" => rust_edges(path, &index, &source, &packages, &owner, &mut outgoing),
                        "lua" => lua_edges(path, &index, &source, &mut outgoing),
                        _ => typescript_edges(
                            path,
                            &index,
                            &source,
                            &packages,
                            &owner,
                            &mut outgoing,
                        ),
                    }
                }
            }
            _ => {}
        }
        outgoing.retain(|item| item.destination != *path);
        outgoing.sort_by(|left, right| {
            (left.kind, left.position, &left.destination).cmp(&(
                right.kind,
                right.position,
                &right.destination,
            ))
        });
        outgoing.dedup_by(|left, right| left.destination == right.destination);
        edge.insert(path.clone(), outgoing);
    }

    let changed = design.changed_paths();
    let destination = design.moved.values().collect::<HashSet<_>>();
    let mut changed_identity = HashMap::<String, ReviewFile>::new();
    for path in changed {
        if destination.contains(&path) {
            continue;
        }
        let proposed = design
            .moved
            .get(&path)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let file = ReviewFile {
            baseline: path.clone(),
            proposed: proposed.clone(),
        };
        changed_identity.insert(proposed, file.clone());
        changed_identity.insert(path, file);
    }

    let package_edge = package_edges(&packages, &owner, &edge, &source);
    let package_order = topological_groups(
        &packages.keys().cloned().collect::<Vec<_>>(),
        &package_edge,
        &package_workspace,
    );
    let mut file_order = Vec::new();
    let mut visited = HashSet::new();
    let mut workspace_configuration = source
        .keys()
        .filter(|path| {
            !owner.contains_key(*path) && parent(path).is_empty() && is_configuration(path)
        })
        .cloned()
        .collect::<Vec<_>>();
    workspace_configuration.extend(workspaces.keys().cloned());
    workspace_configuration.sort();
    workspace_configuration.dedup();
    traverse_scope(
        &workspace_configuration,
        &workspace_configuration,
        &edge,
        &mut visited,
        &mut file_order,
    );
    for package_group in package_order {
        for package_id in package_group {
            let package = &packages[&package_id];
            let files = owner
                .iter()
                .filter(|(_, identity)| **identity == package_id)
                .map(|(path, _)| path.clone())
                .collect::<Vec<_>>();
            let mut roots = vec![package.manifest.clone()];
            roots.extend(package_configuration(&package.directory, &source));
            let mut target = package.root.clone();
            target.sort_by(|left, right| (left.1, &left.0).cmp(&(right.1, &right.0)));
            roots.extend(target.into_iter().map(|(path, _)| path));
            traverse_scope(&files, &roots, &edge, &mut visited, &mut file_order);
        }
    }
    let loose = source
        .keys()
        .filter(|path| !owner.contains_key(*path))
        .cloned()
        .collect::<Vec<_>>();
    let loose_roots = loose
        .iter()
        .filter(|path| is_configuration(path))
        .cloned()
        .collect::<Vec<_>>();
    traverse_scope(&loose, &loose_roots, &edge, &mut visited, &mut file_order);
    let mut emitted = HashSet::new();
    let mut result = Vec::new();
    for path in file_order {
        if let Some(file) = changed_identity.get(&path)
            && emitted.insert(file.baseline.clone())
        {
            result.push(file.clone());
        }
    }
    result
}

fn discover_packages(source: &BTreeMap<String, String>) -> BTreeMap<String, Package> {
    let mut result = BTreeMap::new();
    for (path, text) in source {
        let directory = parent(path);
        if path.ends_with("Cargo.toml") {
            let Ok(manifest) = toml::from_str::<toml::Value>(text) else {
                continue;
            };
            let Some(package) = manifest.get("package") else {
                continue;
            };
            let name = package
                .get("name")
                .and_then(toml::Value::as_str)
                .unwrap_or(&directory)
                .to_owned();
            let mut root = Vec::new();
            if let Some(library) = manifest.get("lib") {
                let path = library
                    .get("path")
                    .and_then(toml::Value::as_str)
                    .unwrap_or("src/lib.rs");
                add_root(&mut root, &directory, path, 1, source);
            } else {
                add_root(&mut root, &directory, "src/lib.rs", 1, source);
            }
            for (key, priority, fallback) in [
                ("bin", 0, "src/main.rs"),
                ("example", 2, "examples"),
                ("test", 3, "tests"),
                ("bench", 4, "benches"),
            ] {
                if let Some(targets) = manifest.get(key).and_then(toml::Value::as_array) {
                    for target in targets {
                        if let Some(path) = target.get("path").and_then(toml::Value::as_str) {
                            add_root(&mut root, &directory, path, priority, source);
                        } else if let Some(name) = target.get("name").and_then(toml::Value::as_str)
                        {
                            let root_directory = if key == "bin" { "src/bin" } else { fallback };
                            for path in [
                                format!("{root_directory}/{name}.rs"),
                                format!("{root_directory}/{name}/main.rs"),
                            ] {
                                add_root(&mut root, &directory, &path, priority, source);
                            }
                        }
                    }
                }
                let auto = format!("auto{key}s");
                if manifest
                    .get("package")
                    .and_then(|package| package.get(&auto))
                    .and_then(toml::Value::as_bool)
                    != Some(false)
                {
                    if key == "bin" {
                        add_root(&mut root, &directory, fallback, priority, source);
                    }
                    {
                        let prefix =
                            join(&directory, if key == "bin" { "src/bin" } else { fallback });
                        for candidate in source.keys().filter(|candidate| {
                            candidate.starts_with(&format!("{prefix}/"))
                                && candidate.ends_with(".rs")
                        }) {
                            if candidate.matches('/').count() <= prefix.matches('/').count() + 1
                                || candidate.ends_with("/main.rs")
                            {
                                root.push((candidate.clone(), priority));
                            }
                        }
                    }
                }
            }
            if let Some(build) = package.get("build").and_then(toml::Value::as_str) {
                add_root(&mut root, &directory, build, 5, source);
            } else if package.get("build").and_then(toml::Value::as_bool) != Some(false) {
                add_root(&mut root, &directory, "build.rs", 5, source);
            }
            result.insert(
                path.clone(),
                Package {
                    manifest: path.clone(),
                    directory,
                    name,
                    language: Language::Rust,
                    root,
                },
            );
        } else if path.ends_with("package.json") {
            let Ok(manifest) = serde_json::from_str::<serde_json::Value>(text) else {
                continue;
            };
            let name = manifest
                .get("name")
                .and_then(serde_json::Value::as_str)
                .unwrap_or(&directory)
                .to_owned();
            let mut root = Vec::new();
            for key in [
                "exports", "source", "module", "main", "types", "typings", "bin",
            ] {
                if let Some(value) = manifest.get(key) {
                    collect_json_roots(
                        value,
                        &directory,
                        source,
                        &mut root,
                        if key == "bin" { 0 } else { 1 },
                    );
                }
            }
            result.insert(
                path.clone(),
                Package {
                    manifest: path.clone(),
                    directory,
                    name,
                    language: Language::Typescript,
                    root,
                },
            );
        }
    }
    result
}

fn discover_workspaces(source: &BTreeMap<String, String>) -> BTreeMap<String, Workspace> {
    let mut result = BTreeMap::new();
    for (path, text) in source {
        let directory = parent(path);
        let (language, patterns, excludes) = if path.ends_with("Cargo.toml") {
            let Some(manifest) = toml::from_str::<toml::Value>(text).ok() else {
                continue;
            };
            let Some(workspace) = manifest.get("workspace") else {
                continue;
            };
            let patterns = workspace
                .get("members")
                .and_then(toml::Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(toml::Value::as_str)
                .map(str::to_owned)
                .collect::<Vec<_>>();
            let excludes = workspace
                .get("exclude")
                .and_then(toml::Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(toml::Value::as_str)
                .map(str::to_owned)
                .collect::<Vec<_>>();
            (Language::Rust, patterns, excludes)
        } else if path.ends_with("package.json") {
            let Some(manifest) = serde_json::from_str::<serde_json::Value>(text).ok() else {
                continue;
            };
            let Some(workspace) = manifest.get("workspaces") else {
                continue;
            };
            let members = workspace.as_array().or_else(|| {
                workspace
                    .get("packages")
                    .and_then(serde_json::Value::as_array)
            });
            let patterns = members
                .into_iter()
                .flatten()
                .filter_map(serde_json::Value::as_str)
                .map(str::to_owned)
                .collect::<Vec<_>>();
            (Language::Typescript, patterns, Vec::new())
        } else if path.ends_with("pnpm-workspace.yaml") {
            let mut patterns = Vec::new();
            let mut excludes = Vec::new();
            let mut in_packages = false;
            for line in text.lines() {
                let trimmed = line.trim();
                if trimmed == "packages:" {
                    in_packages = true;
                    continue;
                }
                if !in_packages {
                    continue;
                }
                if !line.starts_with([' ', '\t']) && !trimmed.is_empty() {
                    break;
                }
                let Some(value) = trimmed.strip_prefix('-') else {
                    continue;
                };
                let value = value.trim().trim_matches(['\'', '"']);
                if let Some(excluded) = value.strip_prefix('!') {
                    excludes.push(excluded.to_owned());
                } else if !value.is_empty() {
                    patterns.push(value.to_owned());
                }
            }
            (Language::Typescript, patterns, excludes)
        } else {
            continue;
        };
        result.insert(
            path.clone(),
            Workspace {
                path: path.clone(),
                directory,
                language,
                member: patterns,
                exclude: excludes,
            },
        );
    }
    result
}

fn assign_workspaces(
    packages: &BTreeMap<String, Package>,
    workspaces: &BTreeMap<String, Workspace>,
) -> HashMap<String, String> {
    let mut result = HashMap::new();
    for (identity, package) in packages {
        let workspace = workspaces
            .values()
            .filter(|workspace| workspace.language == package.language)
            .filter_map(|workspace| {
                let relative = if workspace.directory.is_empty() {
                    package.directory.as_str()
                } else if package.directory == workspace.directory {
                    ""
                } else {
                    package
                        .directory
                        .strip_prefix(&format!("{}/", workspace.directory))?
                };
                let included = relative.is_empty()
                    || workspace
                        .member
                        .iter()
                        .any(|pattern| workspace_pattern(pattern, relative));
                let excluded = workspace
                    .exclude
                    .iter()
                    .any(|pattern| workspace_pattern(pattern, relative));
                (included && !excluded).then_some(workspace)
            })
            .max_by_key(|workspace| workspace.directory.len());
        if let Some(workspace) = workspace {
            result.insert(identity.clone(), workspace.path.clone());
        }
    }
    result
}

fn workspace_pattern(pattern: &str, relative: &str) -> bool {
    let pattern = pattern.trim_start_matches("./");
    let pattern = pattern.split('/').collect::<Vec<_>>();
    let path = relative.split('/').collect::<Vec<_>>();
    let mut reachable = vec![false; path.len() + 1];
    reachable[0] = true;
    for segment in pattern {
        let mut next = vec![false; path.len() + 1];
        if segment == "**" {
            let mut prefix = false;
            for index in 0..=path.len() {
                prefix |= reachable[index];
                next[index] = prefix;
            }
        } else {
            for index in 0..path.len() {
                next[index + 1] = reachable[index] && workspace_segment(segment, path[index]);
            }
        }
        reachable = next;
    }
    reachable[path.len()]
}

fn workspace_segment(pattern: &str, name: &str) -> bool {
    let pattern = pattern.as_bytes();
    let name = name.as_bytes();
    let mut previous = vec![false; name.len() + 1];
    previous[0] = true;
    for character in pattern {
        let mut current = vec![false; name.len() + 1];
        if *character == b'*' {
            current[0] = previous[0];
        }
        for index in 0..name.len() {
            current[index + 1] = if *character == b'*' {
                current[index] || previous[index + 1]
            } else {
                previous[index] && (*character == b'?' || *character == name[index])
            };
        }
        previous = current;
    }
    previous[name.len()]
}

fn add_root(
    root: &mut Vec<(String, u8)>,
    directory: &str,
    path: &str,
    priority: u8,
    source: &BTreeMap<String, String>,
) {
    if let Some(path) = normalized(&join(directory, path))
        && source.contains_key(&path)
    {
        root.push((path, priority));
    }
}

fn collect_json_roots(
    value: &serde_json::Value,
    directory: &str,
    source: &BTreeMap<String, String>,
    root: &mut Vec<(String, u8)>,
    priority: u8,
) {
    match value {
        serde_json::Value::String(path) if !path.contains('*') => {
            add_root(root, directory, path, priority, source)
        }
        serde_json::Value::Object(entry) => {
            for value in entry.values() {
                collect_json_roots(value, directory, source, root, priority);
            }
        }
        serde_json::Value::Array(entry) => {
            for value in entry {
                collect_json_roots(value, directory, source, root, priority);
            }
        }
        _ => {}
    }
}

fn assign_owners(
    source: &BTreeMap<String, String>,
    packages: &BTreeMap<String, Package>,
) -> HashMap<String, String> {
    let mut result = HashMap::new();
    for path in source.keys() {
        let language = match extension(path) {
            "rs" => Some(Language::Rust),
            "ts" | "tsx" | "mts" | "cts" => Some(Language::Typescript),
            "json" | "jsonc"
                if path.ends_with("tsconfig.json") || path.ends_with("jsconfig.json") =>
            {
                Some(Language::Typescript)
            }
            _ => None,
        };
        if language.is_none() {
            continue;
        }
        let candidates = packages
            .iter()
            .filter(|(_, package)| {
                language == Some(package.language)
                    && (package.directory.is_empty()
                        || path.starts_with(&format!("{}/", package.directory)))
            })
            .collect::<Vec<_>>();
        if let Some(longest) = candidates
            .iter()
            .map(|(_, package)| package.directory.len())
            .max()
        {
            let owners = candidates
                .into_iter()
                .filter(|(_, package)| package.directory.len() == longest)
                .collect::<Vec<_>>();
            if owners.len() == 1 {
                result.insert(path.clone(), owners[0].0.clone());
            }
        }
    }
    for (identity, package) in packages {
        result.insert(package.manifest.clone(), identity.clone());
    }
    result
}

fn rust_edges(
    path: &str,
    index: &DeclarationIndex,
    source: &BTreeMap<String, String>,
    packages: &BTreeMap<String, Package>,
    owner: &HashMap<String, String>,
    result: &mut Vec<Edge>,
) {
    let directory = parent(path);
    let stem = path
        .rsplit('/')
        .next()
        .unwrap_or(path)
        .trim_end_matches(".rs");
    let module_base = if matches!(stem, "lib" | "main" | "mod") {
        directory.clone()
    } else {
        join(&directory, stem)
    };
    for (position, module) in index.module.iter().enumerate() {
        if module.inline {
            continue;
        }
        let module_directory = module
            .scope
            .iter()
            .fold(module_base.clone(), |directory, name| {
                join(&directory, name)
            });
        let bases = if let Some(explicit) = &module.path {
            vec![join(&directory, explicit)]
        } else {
            vec![
                join(&module_directory, &format!("{}.rs", module.name)),
                join(&module_directory, &format!("{}/mod.rs", module.name)),
            ]
        };
        for candidate in bases {
            if let Some(destination) = normalized(&candidate)
                && source.contains_key(&destination)
            {
                result.push(Edge {
                    destination,
                    kind: 0,
                    position: position as u32,
                });
                break;
            }
        }
    }
    let Some(package_id) = owner.get(path) else {
        return;
    };
    let package = &packages[package_id];
    let library = package
        .root
        .iter()
        .find(|(root, priority)| *priority == 1 && root.ends_with(".rs"))
        .map(|(root, _)| root.as_str());
    for import in &index.import {
        let Some(first) = import.path.first() else {
            continue;
        };
        let first = first.as_str();
        let remainder = if first == "crate" || first == package.name.replace('-', "_") {
            import.path.get(1..).unwrap_or_default()
        } else if first == "self" {
            import.path.get(1..).unwrap_or_default()
        } else if first == "super" {
            import.path.get(1..).unwrap_or_default()
        } else {
            continue;
        };
        let root = if first == "self" {
            path.to_owned()
        } else if first == "super" {
            directory.clone()
        } else {
            library.unwrap_or(path).to_owned()
        };
        if first == package.name.replace('-', "_")
            && let Some(library) = library
        {
            result.push(Edge {
                destination: library.to_owned(),
                kind: 0,
                position: import.position.line,
            });
        }
        if let Some(destination) = resolve_rust_module(&root, remainder, source) {
            result.push(Edge {
                destination,
                kind: 1,
                position: import.position.line,
            });
        }
    }
}

fn resolve_rust_module(
    root: &str,
    parts: &[String],
    source: &BTreeMap<String, String>,
) -> Option<String> {
    if parts.is_empty() {
        return source.contains_key(root).then(|| root.to_owned());
    }
    let directory = parent(root);
    let mut segment = parts.to_vec();
    while !segment.is_empty() {
        for suffix in [
            format!("{}.rs", segment.join("/")),
            format!("{}/mod.rs", segment.join("/")),
        ] {
            let candidate = normalized(&join(&directory, &suffix))?;
            if source.contains_key(&candidate) {
                return Some(candidate);
            }
        }
        segment.pop();
    }
    None
}

fn typescript_edges(
    path: &str,
    index: &DeclarationIndex,
    source: &BTreeMap<String, String>,
    packages: &BTreeMap<String, Package>,
    owner: &HashMap<String, String>,
    result: &mut Vec<Edge>,
) {
    for import in &index.import {
        let Some(origin) = &import.source else {
            continue;
        };
        let destination = if origin.starts_with('.') {
            resolve_typescript(&join(&parent(path), origin), source)
        } else {
            resolve_typescript_alias(origin, path, source)
                .or_else(|| resolve_package_import(origin, path, source, packages, owner))
        };
        if let Some(destination) = destination {
            result.push(Edge {
                destination,
                kind: 1,
                position: import.position.line,
            });
        }
    }
    for reference in &index.path_reference {
        if let Some(destination) = resolve_typescript(&join(&parent(path), reference), source) {
            result.push(Edge {
                destination,
                kind: 1,
                position: 0,
            });
        }
    }
}

fn resolve_typescript(base: &str, source: &BTreeMap<String, String>) -> Option<String> {
    let base = normalized(base)?;
    for (suffix, replacement) in [
        (".js", ".ts"),
        (".jsx", ".tsx"),
        (".mjs", ".mts"),
        (".cjs", ".cts"),
    ] {
        if let Some(stem) = base.strip_suffix(suffix) {
            let candidate = format!("{stem}{replacement}");
            if source.contains_key(&candidate) {
                return Some(candidate);
            }
        }
    }
    for suffix in [
        "",
        ".ts",
        ".tsx",
        ".mts",
        ".cts",
        "/index.ts",
        "/index.tsx",
        "/index.mts",
        "/index.cts",
    ] {
        let candidate = format!("{base}{suffix}");
        if source.contains_key(&candidate) {
            return Some(candidate);
        }
    }
    None
}

fn resolve_package_import(
    origin: &str,
    path: &str,
    source: &BTreeMap<String, String>,
    packages: &BTreeMap<String, Package>,
    owner: &HashMap<String, String>,
) -> Option<String> {
    let current = owner.get(path);
    for (identity, package) in packages {
        if package.language != Language::Typescript || current == Some(identity) {
            continue;
        }
        if origin == package.name || origin.starts_with(&format!("{}/", package.name)) {
            let subpath = origin
                .strip_prefix(&package.name)
                .unwrap_or("")
                .trim_start_matches('/');
            if !subpath.is_empty() {
                if let Some(candidate) =
                    resolve_typescript(&join(&package.directory, subpath), source)
                {
                    return Some(candidate);
                }
            }
            return package
                .root
                .iter()
                .min_by_key(|(path, priority)| (priority, path))
                .map(|(path, _)| path.clone());
        }
    }
    None
}

fn resolve_typescript_alias(
    origin: &str,
    path: &str,
    source: &BTreeMap<String, String>,
) -> Option<String> {
    let mut config = parent(path);
    loop {
        let config_path = join(&config, "tsconfig.json");
        if let Some(text) = source.get(&config_path) {
            if let Ok(config_value) = ConfigurationFormat::json_value(text) {
                let options = &config_value["compilerOptions"];
                let base = options["baseUrl"].as_str().unwrap_or(".");
                if let Some(aliases) = options["paths"].as_object() {
                    for (pattern, targets) in aliases {
                        let capture = if let Some((prefix, suffix)) = pattern.split_once('*') {
                            origin
                                .strip_prefix(prefix)
                                .and_then(|tail| tail.strip_suffix(suffix))
                        } else {
                            (pattern == origin).then_some("")
                        };
                        if let Some(capture) = capture
                            && let Some(targets) = targets.as_array()
                        {
                            for target in targets.iter().filter_map(serde_json::Value::as_str) {
                                let target = target.replace('*', capture);
                                if let Some(destination) =
                                    resolve_typescript(&join(&join(&config, base), &target), source)
                                {
                                    return Some(destination);
                                }
                            }
                        }
                    }
                }
            }
        }
        if config.is_empty() {
            break;
        }
        config = parent(&config);
    }
    None
}

fn lua_edges(
    path: &str,
    index: &DeclarationIndex,
    source: &BTreeMap<String, String>,
    result: &mut Vec<Edge>,
) {
    for import in &index.import {
        let Some(module) = &import.source else {
            continue;
        };
        let module_path = module.replace('.', "/");
        let mut directory = parent(path);
        loop {
            for base in [directory.clone(), join(&directory, "lua")] {
                for suffix in [
                    format!("{module_path}.lua"),
                    format!("{module_path}/init.lua"),
                ] {
                    if let Some(destination) = normalized(&join(&base, &suffix))
                        && source.contains_key(&destination)
                    {
                        result.push(Edge {
                            destination,
                            kind: 1,
                            position: import.position.line,
                        });
                        break;
                    }
                }
            }
            if directory.is_empty() {
                break;
            }
            directory = parent(&directory);
        }
    }
}

fn package_configuration(directory: &str, source: &BTreeMap<String, String>) -> Vec<String> {
    source
        .keys()
        .filter(|path| parent(path) == directory && is_configuration(path))
        .cloned()
        .collect()
}

fn is_configuration(path: &str) -> bool {
    let name = path.rsplit('/').next().unwrap_or(path);
    matches!(
        name,
        "Cargo.toml"
            | "package.json"
            | "pnpm-workspace.yaml"
            | "tsconfig.json"
            | "jsconfig.json"
            | ".forge.json"
    ) || name.starts_with("tsconfig.") && name.ends_with(".json")
}

fn package_edges(
    packages: &BTreeMap<String, Package>,
    owner: &HashMap<String, String>,
    edge: &HashMap<String, Vec<Edge>>,
    source: &BTreeMap<String, String>,
) -> HashMap<String, Vec<String>> {
    let mut result = HashMap::<String, Vec<String>>::new();
    for (path, outgoing) in edge {
        let Some(origin) = owner.get(path) else {
            continue;
        };
        for item in outgoing {
            if let Some(destination) = owner.get(&item.destination)
                && origin != destination
            {
                result
                    .entry(origin.clone())
                    .or_default()
                    .push(destination.clone());
            }
        }
    }
    for (identity, package) in packages {
        let Some(text) = source.get(identity) else {
            continue;
        };
        match package.language {
            Language::Rust => {
                if let Ok(manifest) = toml::from_str::<toml::Value>(text) {
                    for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
                        if let Some(dependencies) =
                            manifest.get(section).and_then(toml::Value::as_table)
                        {
                            for (name, value) in dependencies {
                                let Some(path) = value.get("path").and_then(toml::Value::as_str)
                                else {
                                    continue;
                                };
                                let target = normalized(&join(
                                    &package.directory,
                                    &format!("{path}/Cargo.toml"),
                                ));
                                if let Some(target) = target
                                    && packages.contains_key(&target)
                                    && target != *identity
                                {
                                    result.entry(identity.clone()).or_default().push(target);
                                } else if let Some(target) = packages
                                    .iter()
                                    .find(|(_, package)| package.name == *name)
                                    .map(|(identity, _)| identity.clone())
                                {
                                    result.entry(identity.clone()).or_default().push(target);
                                }
                            }
                        }
                    }
                }
            }
            Language::Typescript => {
                if let Ok(manifest) = serde_json::from_str::<serde_json::Value>(text) {
                    for section in ["dependencies", "devDependencies", "peerDependencies"] {
                        if let Some(dependencies) =
                            manifest.get(section).and_then(serde_json::Value::as_object)
                        {
                            for (name, _) in dependencies {
                                if let Some(target) = packages
                                    .iter()
                                    .find(|(_, package)| package.name == *name)
                                    .map(|(identity, _)| identity.clone())
                                {
                                    if target != *identity {
                                        result.entry(identity.clone()).or_default().push(target);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    for edges in result.values_mut() {
        edges.sort();
        edges.dedup();
    }
    result
}

fn traverse_scope(
    files: &[String],
    roots: &[String],
    edge: &HashMap<String, Vec<Edge>>,
    visited: &mut HashSet<String>,
    result: &mut Vec<String>,
) {
    let allowed = files.iter().cloned().collect::<HashSet<_>>();
    let graph = files
        .iter()
        .map(|path| {
            (
                path.clone(),
                edge.get(path)
                    .into_iter()
                    .flatten()
                    .filter(|item| allowed.contains(&item.destination))
                    .map(|item| item.destination.clone())
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    let groups = strongly_connected(files, &graph);
    let mut membership = HashMap::new();
    for (index, group) in groups.iter().enumerate() {
        for path in group {
            membership.insert(path.clone(), index);
        }
    }
    let mut outgoing = vec![Vec::<usize>::new(); groups.len()];
    let mut incoming = vec![0usize; groups.len()];
    let mut relation_order = HashMap::<(usize, usize), (u8, u32, String)>::new();
    for (path, neighbors) in edge {
        if !allowed.contains(path) {
            continue;
        }
        let origin = membership[path];
        for neighbor in neighbors {
            let Some(&target) = membership.get(&neighbor.destination) else {
                continue;
            };
            if origin != target && !relation_order.contains_key(&(origin, target)) {
                outgoing[origin].push(target);
                incoming[target] += 1;
            }
            if origin != target {
                let candidate = (
                    neighbor.kind,
                    neighbor.position,
                    neighbor.destination.clone(),
                );
                relation_order
                    .entry((origin, target))
                    .and_modify(|current| {
                        if candidate < *current {
                            *current = candidate.clone();
                        }
                    })
                    .or_insert(candidate);
            }
        }
    }
    for (origin, neighbors) in outgoing.iter_mut().enumerate() {
        neighbors.sort_by_key(|target| relation_order.get(&(origin, *target)).cloned());
    }
    let mut start = roots
        .iter()
        .filter_map(|root| membership.get(root).copied())
        .collect::<Vec<_>>();
    let mut inferred = (0..groups.len())
        .filter(|index| incoming[*index] == 0)
        .collect::<Vec<_>>();
    inferred.sort_by_key(|index| groups[*index].first().cloned().unwrap_or_default());
    start.extend(inferred);
    let mut remainder = (0..groups.len()).collect::<Vec<_>>();
    remainder.sort_by_key(|index| groups[*index].first().cloned().unwrap_or_default());
    start.extend(remainder);
    let mut seen_group = HashSet::new();
    for root in start {
        let mut stack = vec![root];
        while let Some(group_id) = stack.pop() {
            if !seen_group.insert(group_id) {
                continue;
            }
            for path in &groups[group_id] {
                if visited.insert(path.clone()) {
                    result.push(path.clone());
                }
            }
            for target in outgoing[group_id].iter().rev() {
                stack.push(*target);
            }
        }
    }
}

fn topological_groups(
    nodes: &[String],
    edge: &HashMap<String, Vec<String>>,
    workspace: &HashMap<String, String>,
) -> Vec<Vec<String>> {
    let groups = strongly_connected(nodes, edge);
    let priority = groups
        .iter()
        .map(|group| {
            group
                .iter()
                .map(|identity| workspace.get(identity).unwrap_or(identity))
                .min()
                .cloned()
                .unwrap_or_default()
        })
        .collect::<Vec<_>>();
    let mut membership = HashMap::new();
    for (index, group) in groups.iter().enumerate() {
        for path in group {
            membership.insert(path.clone(), index);
        }
    }
    let mut incoming = vec![0usize; groups.len()];
    let mut outgoing = vec![BTreeSet::<usize>::new(); groups.len()];
    for (source, neighbors) in edge {
        let Some(origin) = membership.get(source) else {
            continue;
        };
        for destination in neighbors {
            let Some(target) = membership.get(destination) else {
                continue;
            };
            if origin != target && outgoing[*origin].insert(*target) {
                incoming[*target] += 1;
            }
        }
    }
    let mut ready = BTreeSet::new();
    for (index, count) in incoming.iter().enumerate() {
        if *count == 0 {
            ready.insert((priority[index].clone(), groups[index][0].clone(), index));
        }
    }
    let mut result = Vec::new();
    while let Some((_, _, index)) = ready.pop_first() {
        result.push(groups[index].clone());
        for target in &outgoing[index] {
            incoming[*target] -= 1;
            if incoming[*target] == 0 {
                ready.insert((
                    priority[*target].clone(),
                    groups[*target][0].clone(),
                    *target,
                ));
            }
        }
    }
    result
}

fn strongly_connected(nodes: &[String], edge: &HashMap<String, Vec<String>>) -> Vec<Vec<String>> {
    let allowed = nodes.iter().cloned().collect::<HashSet<_>>();
    let mut seen = HashSet::new();
    let mut finish = Vec::new();
    for node in nodes {
        if !seen.insert(node.clone()) {
            continue;
        }
        let mut stack = vec![(node.clone(), 0usize)];
        while let Some((current, next)) = stack.last_mut() {
            let neighbors = edge.get(current).map(Vec::as_slice).unwrap_or_default();
            if *next == neighbors.len() {
                finish.push(current.clone());
                stack.pop();
                continue;
            }
            let destination = &neighbors[*next];
            *next += 1;
            if allowed.contains(destination) && seen.insert(destination.clone()) {
                stack.push((destination.clone(), 0));
            }
        }
    }
    let mut reverse = HashMap::<String, Vec<String>>::new();
    for (origin, destinations) in edge {
        for destination in destinations {
            if allowed.contains(destination) {
                reverse
                    .entry(destination.clone())
                    .or_default()
                    .push(origin.clone());
            }
        }
    }
    let mut group_seen = HashSet::new();
    let mut groups = Vec::new();
    for node in finish.into_iter().rev() {
        if !group_seen.insert(node.clone()) {
            continue;
        }
        let mut group = Vec::new();
        let mut stack = vec![node];
        while let Some(current) = stack.pop() {
            group.push(current.clone());
            for origin in reverse.get(&current).into_iter().flatten() {
                if group_seen.insert(origin.clone()) {
                    stack.push(origin.clone());
                }
            }
        }
        group.sort();
        groups.push(group);
    }
    groups
}

fn extension(path: &str) -> &str {
    path.rsplit_once('.')
        .map(|(_, extension)| extension)
        .unwrap_or("")
}
fn parent(path: &str) -> String {
    path.rsplit_once('/')
        .map(|(parent, _)| parent.to_owned())
        .unwrap_or_default()
}
fn join(directory: &str, path: &str) -> String {
    if directory.is_empty() {
        path.to_owned()
    } else {
        format!("{directory}/{path}")
    }
}
fn normalized(path: &str) -> Option<String> {
    let mut parts = Vec::new();
    for part in path.replace('\\', "/").split('/') {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop()?;
            }
            _ => parts.push(part.to_owned()),
        }
    }
    Some(parts.join("/"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::DeclarationFile;

    fn design(files: &[(&str, &str, Option<&str>)]) -> DeclarationDesign {
        let mut design = DeclarationDesign::default();
        for (path, baseline, proposed) in files {
            design.baseline.insert(
                (*path).into(),
                DeclarationFile {
                    text: (*baseline).into(),
                    source_digest: String::new(),
                },
            );
            if let Some(proposed) = proposed {
                design.proposed.insert((*path).into(), (*proposed).into());
            }
        }
        design
    }

    fn paths(design: &DeclarationDesign) -> Vec<String> {
        order(design)
            .into_iter()
            .map(|file| file.proposed)
            .collect()
    }

    #[test]
    fn unchanged_intermediary_connects_changed_files() {
        let design = design(&[
            (
                "src/a.ts",
                "import './b';\nexport type A = string;\n",
                Some("import './b';\nexport type A = number;\n"),
            ),
            (
                "src/b.ts",
                "import './c';\nexport type B = string;\n",
                Some("import './c';\nexport type B = string;\n"),
            ),
            (
                "src/c.ts",
                "export type C = string;\n",
                Some("export type C = number;\n"),
            ),
        ]);
        assert_eq!(paths(&design), ["src/a.ts", "src/c.ts"]);
    }

    #[test]
    fn cycles_and_shared_dependencies_emit_once() {
        let design = design(&[
            (
                "a.ts",
                "import './b';\n",
                Some("import './b';\nexport type A = string;\n"),
            ),
            (
                "b.ts",
                "import './a';\nimport './c';\n",
                Some("import './a';\nimport './c';\nexport type B = string;\n"),
            ),
            (
                "c.ts",
                "export type C = string;\n",
                Some("export type C = number;\n"),
            ),
        ]);
        assert_eq!(paths(&design), ["a.ts", "b.ts", "c.ts"]);
    }

    #[test]
    fn multiple_cargo_targets_keep_package_order() {
        let design = design(&[
            (
                "app/Cargo.toml",
                "[package]\nname='app'\nversion='0.1.0'\n[dependencies]\ncore={path='../core'}\n",
                Some(
                    "[package]\nname='app'\nversion='0.2.0'\n[dependencies]\ncore={path='../core'}\n",
                ),
            ),
            (
                "app/src/main.rs",
                "use app::feature;\nfn main() {}\n",
                Some("use app::feature;\nfn main() { feature(); }\n"),
            ),
            (
                "app/src/lib.rs",
                "pub mod feature;\n",
                Some("pub mod feature;\npub fn exposed() {}\n"),
            ),
            (
                "app/src/feature.rs",
                "pub fn feature() {}\n",
                Some("pub fn feature() { println!(\"feature\"); }\n"),
            ),
            (
                "core/Cargo.toml",
                "[package]\nname='core'\nversion='0.1.0'\n",
                Some("[package]\nname='core'\nversion='0.2.0'\n"),
            ),
            (
                "core/src/lib.rs",
                "pub fn core() {}\n",
                Some("pub fn core() { println!(\"core\"); }\n"),
            ),
        ]);
        assert_eq!(
            paths(&design),
            [
                "app/Cargo.toml",
                "app/src/main.rs",
                "app/src/lib.rs",
                "app/src/feature.rs",
                "core/Cargo.toml",
                "core/src/lib.rs"
            ]
        );
    }

    #[test]
    fn lua_require_and_loose_files_have_stable_order() {
        let design = design(&[
            (
                "init.lua",
                "local module = require('plugin.module')\n",
                Some("local module = require('plugin.module')\nfunction run() end\n"),
            ),
            (
                "lua/plugin/module.lua",
                "local M = {}\nreturn M\n",
                Some("local M = {}\nfunction M.run() end\nreturn M\n"),
            ),
            ("notes.yaml", "name: old\n", Some("name: new\n")),
        ]);
        assert_eq!(
            paths(&design),
            ["init.lua", "lua/plugin/module.lua", "notes.yaml"]
        );
    }

    #[test]
    fn node_packages_and_path_aliases_keep_changed_descendants_in_order() {
        let design = design(&[
            (
                "pnpm-workspace.yaml",
                "packages: ['web', 'lib']\n",
                Some("packages: ['web', 'lib', 'tools']\n"),
            ),
            (
                "web/package.json",
                "{\"name\":\"web\",\"exports\":\"./src/main.ts\",\"dependencies\":{\"@local/core\":\"workspace:*\"}}",
                Some(
                    "{\"name\":\"web\",\"exports\":\"./src/main.ts\",\"dependencies\":{\"@local/core\":\"workspace:*\"},\"private\":true}",
                ),
            ),
            (
                "web/tsconfig.json",
                "{\"compilerOptions\":{\"paths\":{\"@ui/*\":[\"src/*\"]}}}",
                Some("{\"compilerOptions\":{\"paths\":{\"@ui/*\":[\"src/*\"]}}}"),
            ),
            (
                "web/src/main.ts",
                "import './bridge';\nexport type Main = string;\n",
                Some("import './bridge';\nexport type Main = number;\n"),
            ),
            (
                "web/src/bridge.ts",
                "import '@ui/view';\n",
                Some("import '@ui/view';\n"),
            ),
            (
                "web/src/view.ts",
                "import '@local/core';\nexport type View = string;\n",
                Some("import '@local/core';\nexport type View = number;\n"),
            ),
            (
                "lib/package.json",
                "{\"name\":\"@local/core\",\"exports\":\"./src/index.ts\"}",
                Some("{\"name\":\"@local/core\",\"exports\":\"./src/index.ts\",\"private\":true}"),
            ),
            (
                "lib/src/index.ts",
                "export type Core = string;\n",
                Some("export type Core = number;\n"),
            ),
        ]);
        assert_eq!(
            paths(&design),
            [
                "pnpm-workspace.yaml",
                "web/package.json",
                "web/src/main.ts",
                "web/src/view.ts",
                "lib/package.json",
                "lib/src/index.ts"
            ]
        );
    }

    #[test]
    fn moved_file_is_emitted_once_at_its_proposed_location() {
        let mut design = design(&[
            ("src/old.ts", "export type Old = string;\n", None),
            (
                "src/main.ts",
                "import './old';\n",
                Some("import './new';\n"),
            ),
        ]);
        design
            .proposed
            .insert("src/new.ts".into(), "export type New = string;\n".into());
        design
            .moved
            .insert("src/old.ts".into(), "src/new.ts".into());
        assert_eq!(paths(&design), ["src/main.ts", "src/new.ts"]);
    }

    #[test]
    fn mixed_packages_and_unknown_files_share_one_order() {
        let design = design(&[
            (
                "Cargo.toml",
                "[package]\nname='native'\nversion='0.1.0'\n",
                Some("[package]\nname='native'\nversion='0.2.0'\n"),
            ),
            (
                "src/lib.rs",
                "pub struct Native;\n",
                Some("pub struct Native { pub id: u64 }\n"),
            ),
            (
                "web/package.json",
                "{\"name\":\"web\",\"exports\":\"./src/index.ts\"}",
                Some("{\"name\":\"web\",\"exports\":\"./src/index.ts\",\"private\":true}"),
            ),
            (
                "web/src/index.ts",
                "export type Web = string;\n",
                Some("export type Web = number;\n"),
            ),
            ("misc/task.py", "value = 1\n", Some("value = 2\n")),
        ]);
        assert_eq!(
            paths(&design),
            [
                "Cargo.toml",
                "src/lib.rs",
                "web/package.json",
                "web/src/index.ts",
                "misc/task.py"
            ]
        );
    }

    #[test]
    fn declared_cargo_targets_override_conventional_paths() {
        let design = design(&[
            (
                "pkg/Cargo.toml",
                "[package]\nname='pkg'\nversion='0.1.0'\nautobins=false\n[lib]\npath='custom/library.rs'\n[[bin]]\nname='cli'\npath='tools/cli.rs'\n",
                Some(
                    "[package]\nname='pkg'\nversion='0.2.0'\nautobins=false\n[lib]\npath='custom/library.rs'\n[[bin]]\nname='cli'\npath='tools/cli.rs'\n",
                ),
            ),
            (
                "pkg/tools/cli.rs",
                "fn main() {}\n",
                Some("fn main() { println!(\"cli\"); }\n"),
            ),
            (
                "pkg/custom/library.rs",
                "pub struct Library;\n",
                Some("pub struct Library { pub id: u64 }\n"),
            ),
            (
                "pkg/src/main.rs",
                "fn old() {}\n",
                Some("fn old() { println!(\"old\"); }\n"),
            ),
        ]);
        assert_eq!(
            paths(&design),
            [
                "pkg/Cargo.toml",
                "pkg/tools/cli.rs",
                "pkg/custom/library.rs",
                "pkg/src/main.rs"
            ]
        );
    }

    #[test]
    fn shared_dependencies_and_deleted_files_keep_depth_first_order() {
        let design = design(&[
            (
                "main.ts",
                "import './a';\nimport './b';\n",
                Some("import './a';\nimport './b';\nexport type Main = string;\n"),
            ),
            (
                "a.ts",
                "import './shared';\n",
                Some("import './shared';\nexport type A = string;\n"),
            ),
            (
                "b.ts",
                "import './shared';\n",
                Some("import './shared';\nexport type B = string;\n"),
            ),
            (
                "shared.ts",
                "export type Shared = string;\n",
                Some("export type Shared = number;\n"),
            ),
            ("old.ts", "export type Old = string;\n", None),
        ]);
        assert_eq!(
            paths(&design),
            ["main.ts", "a.ts", "shared.ts", "b.ts", "old.ts"]
        );
    }

    #[test]
    fn workspace_membership_places_nested_metadata_before_member_packages() {
        let design = design(&[
            (
                "project/Cargo.toml",
                "[workspace]\nmembers=['crates/*']\n",
                Some("[workspace]\nmembers=['crates/*']\nresolver='2'\n"),
            ),
            (
                "project/crates/app/Cargo.toml",
                "[package]\nname='app'\nversion='0.1.0'\n",
                Some("[package]\nname='app'\nversion='0.2.0'\n"),
            ),
            (
                "project/crates/app/src/main.rs",
                "fn main() {}\n",
                Some("fn main() { println!(\"app\"); }\n"),
            ),
            (
                "project/crates/core/Cargo.toml",
                "[package]\nname='core'\nversion='0.1.0'\n",
                Some("[package]\nname='core'\nversion='0.2.0'\n"),
            ),
            (
                "project/other/Cargo.toml",
                "[package]\nname='other'\nversion='0.1.0'\n",
                Some("[package]\nname='other'\nversion='0.2.0'\n"),
            ),
        ]);
        let source = design.proposed.clone();
        let member = assign_workspaces(&discover_packages(&source), &discover_workspaces(&source));
        assert_eq!(
            member
                .get("project/crates/app/Cargo.toml")
                .map(String::as_str),
            Some("project/Cargo.toml")
        );
        assert_eq!(
            member
                .get("project/crates/core/Cargo.toml")
                .map(String::as_str),
            Some("project/Cargo.toml")
        );
        assert!(!member.contains_key("project/other/Cargo.toml"));
        assert_eq!(
            paths(&design),
            [
                "project/Cargo.toml",
                "project/crates/app/Cargo.toml",
                "project/crates/app/src/main.rs",
                "project/crates/core/Cargo.toml",
                "project/other/Cargo.toml"
            ]
        );
    }

    #[test]
    fn node_and_pnpm_workspace_patterns_select_local_members() {
        let source = BTreeMap::from([
            (
                "monorepo/package.json".into(),
                "{\"name\":\"root\",\"workspaces\":[\"modules/*\"]}".into(),
            ),
            (
                "monorepo/modules/ui/package.json".into(),
                "{\"name\":\"ui\"}".into(),
            ),
            (
                "monorepo/elsewhere/package.json".into(),
                "{\"name\":\"elsewhere\"}".into(),
            ),
            (
                "pnpm/pnpm-workspace.yaml".into(),
                "packages:\n  - 'packages/*'\n  - '!packages/ignored'\n".into(),
            ),
            (
                "pnpm/packages/app/package.json".into(),
                "{\"name\":\"app\"}".into(),
            ),
            (
                "pnpm/packages/ignored/package.json".into(),
                "{\"name\":\"ignored\"}".into(),
            ),
        ]);
        let member = assign_workspaces(&discover_packages(&source), &discover_workspaces(&source));
        assert_eq!(
            member
                .get("monorepo/modules/ui/package.json")
                .map(String::as_str),
            Some("monorepo/package.json")
        );
        assert!(!member.contains_key("monorepo/elsewhere/package.json"));
        assert_eq!(
            member
                .get("pnpm/packages/app/package.json")
                .map(String::as_str),
            Some("pnpm/pnpm-workspace.yaml")
        );
        assert!(!member.contains_key("pnpm/packages/ignored/package.json"));
    }
}
