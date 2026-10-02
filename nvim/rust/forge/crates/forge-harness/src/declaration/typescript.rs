//! TypeScript modules and globals derive from configuration and declaration files.

use super::*;
use forge_diff::syntax::DeclarationImport;
use serde_json::Value;

#[derive(Clone, Default)]
pub(super) struct TypescriptEnvironment {
    config: BTreeMap<PathBuf, TypescriptConfig>,
}

#[derive(Clone, Default)]
struct TypescriptConfig {
    directory: PathBuf,
    option: Value,
    config: Value,
    globals: Vec<PathBuf>,
    libraries_available: bool,
    incomplete: bool,
    resolution_incomplete: bool,
}

impl DeclarationResolver {
    pub(super) fn typescript_environment(&mut self) -> Result<()> {
        let files = self
            .file
            .keys()
            .filter(|path| {
                matches!(
                    path.extension().and_then(|extension| extension.to_str()),
                    Some("ts" | "tsx" | "mts" | "cts")
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        for file in &files {
            let config_path = self.nearest_tsconfig(file);
            if !self.typescript.config.contains_key(&config_path) {
                let mut config = self.read_tsconfig(&config_path, &mut HashSet::new())?;
                let package = find_package_directory(&config.directory, "typescript");
                let mut version = 0;
                if let Some(package) = package {
                    version = read_json(&package.join("package.json"))
                        .and_then(|manifest| {
                            manifest
                                .get("version")
                                .and_then(Value::as_str)
                                .map(str::to_owned)
                        })
                        .and_then(|version| version.split('.').next()?.parse::<u32>().ok())
                        .unwrap_or(0);
                    config.libraries_available = true;
                    if config.option.get("noLib").and_then(Value::as_bool) != Some(true) {
                        let libraries = config
                            .option
                            .get("lib")
                            .and_then(Value::as_array)
                            .map(|libraries| {
                                libraries
                                    .iter()
                                    .filter_map(Value::as_str)
                                    .map(|name| format!("lib.{}.d.ts", name.to_lowercase()))
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_else(|| {
                                let target = config
                                    .option
                                    .get("target")
                                    .and_then(Value::as_str)
                                    .unwrap_or("es5")
                                    .to_lowercase();
                                let file = match target.as_str() {
                                    "es3" | "es5" => "lib.d.ts".into(),
                                    "es6" | "es2015" => "lib.es6.d.ts".into(),
                                    _ => format!("lib.{target}.full.d.ts"),
                                };
                                vec![file]
                            });
                        for library in libraries {
                            self.load_ts_library(
                                &package.join("lib").join(library),
                                &mut config,
                                &mut HashSet::new(),
                            )?;
                        }
                    }
                } else if config.option.get("noLib").and_then(Value::as_bool) == Some(true)
                    || config
                        .option
                        .get("lib")
                        .and_then(Value::as_array)
                        .is_some_and(Vec::is_empty)
                {
                    config.libraries_available = true;
                } else {
                    self.warning.push(format!("TypeScript standard declarations are unavailable for {}. Install the project's TypeScript dependency. Global library references remain unverified.", config.directory.display()));
                }
                let mut types = config
                    .option
                    .get("types")
                    .and_then(Value::as_array)
                    .map(|types| {
                        types
                            .iter()
                            .filter_map(Value::as_str)
                            .map(str::to_owned)
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                let explicit = config.option.get("types").is_some();
                let roots = config
                    .option
                    .get("typeRoots")
                    .and_then(Value::as_array)
                    .map(|roots| {
                        roots
                            .iter()
                            .filter_map(Value::as_str)
                            .map(|root| normalize(&config.directory.join(root)))
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_else(|| {
                        config
                            .directory
                            .ancestors()
                            .map(|directory| directory.join("node_modules/@types"))
                            .collect()
                    });
                if !explicit && version > 0 && version < 6 {
                    for root in &roots {
                        for entry in std::fs::read_dir(root).into_iter().flatten().flatten() {
                            if entry.path().is_dir() {
                                types.push(entry.file_name().to_string_lossy().into_owned());
                            }
                        }
                    }
                }
                for name in types {
                    let name = name.trim_start_matches('@').replace('/', "__");
                    if let Some(root) = roots
                        .iter()
                        .map(|root| root.join(&name))
                        .find(|path| path.is_dir())
                    {
                        if let Some(entry) = self.package_entry(&root, ".", &config, None) {
                            self.load_ts_global(&entry, &mut config, &mut HashSet::new())?;
                        } else {
                            config.incomplete = true;
                            self.warning.push(format!("Global type package `{name}` has no available declaration entry point."));
                        }
                    } else {
                        config.incomplete = true;
                        self.warning
                            .push(format!("Global type package `{name}` is not installed."));
                    }
                }
                let mut global_visited = HashSet::new();
                for global in &files {
                    if self.nearest_tsconfig(global) == config_path && included(global, &config) {
                        self.load_ts_global(global, &mut config, &mut global_visited)?;
                    }
                }
                self.typescript.config.insert(config_path, config);
            }
        }
        Ok(())
    }

    fn nearest_tsconfig(&self, file: &Path) -> PathBuf {
        for directory in file.parent().unwrap_or(&self.workspace).ancestors() {
            if !directory.starts_with(&self.workspace) {
                break;
            }
            let path = directory.join("tsconfig.json");
            if self.read(&path).is_some() {
                return path;
            }
        }
        self.workspace.join("tsconfig.json")
    }

    fn read_tsconfig(
        &self,
        path: &Path,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<TypescriptConfig> {
        let path = normalize(path);
        anyhow::ensure!(
            visited.len() < 32 && visited.insert(path.clone()),
            "cyclic or excessively deep TypeScript configuration inheritance"
        );
        let directory = path.parent().unwrap_or(&self.workspace).to_path_buf();
        let Some(text) = self.read(&path) else {
            return Ok(TypescriptConfig {
                directory,
                option: serde_json::json!({}),
                config: serde_json::json!({}),
                ..Default::default()
            });
        };
        let config = forge_diff::syntax::ConfigurationFormat::json_value(&text)
            .map_err(|error| anyhow::anyhow!("{}: {error:?}", path.display()))?;
        let mut option = serde_json::json!({});
        let mut effective = serde_json::json!({});
        let mut incomplete = false;
        let extends = match config.get("extends") {
            Some(Value::String(name)) => vec![name.clone()],
            Some(Value::Array(names)) => names
                .iter()
                .filter_map(Value::as_str)
                .map(str::to_owned)
                .collect(),
            _ => Vec::new(),
        };
        for name in extends {
            let parent = if name.starts_with('.') || Path::new(&name).is_absolute() {
                self.config_candidate(&directory.join(&name))
            } else {
                let (package, subpath) = package_parts(&name);
                find_package_directory(&directory, &package).and_then(|root| {
                    if subpath.is_empty() {
                        Some(
                            read_json(&root.join("package.json"))
                                .and_then(|manifest| {
                                    manifest
                                        .get("tsconfig")
                                        .and_then(Value::as_str)
                                        .map(|path| root.join(path))
                                })
                                .unwrap_or_else(|| root.join("tsconfig.json")),
                        )
                    } else {
                        self.config_candidate(&root.join(subpath))
                    }
                })
            };
            if let Some(parent) = parent {
                let inherited = self.read_tsconfig(&parent, visited)?;
                option = inherited.option;
                // Resolve inherited filesystem options relative to their declaring config.
                for key in ["baseUrl", "rootDir"] {
                    if let Some(value) = option.get_mut(key).filter(|value| value.is_string()) {
                        *value = Value::String(
                            normalize(&inherited.directory.join(value.as_str().unwrap()))
                                .to_string_lossy()
                                .into_owned(),
                        );
                    }
                }
                if let Some(Value::Array(roots)) = option.get_mut("typeRoots") {
                    for root in roots {
                        if let Some(value) = root.as_str() {
                            *root = Value::String(
                                normalize(&inherited.directory.join(value))
                                    .to_string_lossy()
                                    .into_owned(),
                            );
                        }
                    }
                }
                let base = option
                    .get("baseUrl")
                    .and_then(Value::as_str)
                    .map(PathBuf::from)
                    .unwrap_or_else(|| inherited.directory.clone());
                if let Some(Value::Object(paths)) = option.get_mut("paths") {
                    for targets in paths.values_mut().filter_map(Value::as_array_mut) {
                        for target in targets {
                            if let Some(path) = target.as_str() {
                                *target = Value::String(
                                    normalize(&base.join(path)).to_string_lossy().into_owned(),
                                );
                            }
                        }
                    }
                }
                for name in ["files", "include", "exclude"] {
                    if let Some(Value::Array(paths)) = inherited.config.get(name) {
                        effective[name] = Value::Array(
                            paths
                                .iter()
                                .map(|path| {
                                    path.as_str()
                                        .map(|path| {
                                            Value::String(
                                                normalize(&inherited.directory.join(path))
                                                    .to_string_lossy()
                                                    .into_owned(),
                                            )
                                        })
                                        .unwrap_or_else(|| path.clone())
                                })
                                .collect(),
                        );
                    }
                }
                incomplete |= inherited.incomplete;
            } else {
                incomplete = true;
            }
        }
        if let Some(local) = config.get("compilerOptions").and_then(Value::as_object) {
            for (name, value) in local {
                option
                    .as_object_mut()
                    .unwrap()
                    .insert(name.clone(), value.clone());
            }
        }
        incomplete |= ["rootDirs", "moduleSuffixes"]
            .iter()
            .any(|name| option.get(*name).is_some());
        for (name, value) in config
            .as_object()
            .context("TypeScript configuration must be an object")?
        {
            effective[name] = value.clone();
        }
        visited.remove(&path);
        Ok(TypescriptConfig {
            directory,
            config: effective,
            option,
            incomplete,
            resolution_incomplete: incomplete,
            ..Default::default()
        })
    }

    fn load_ts_library(
        &mut self,
        path: &Path,
        config: &mut TypescriptConfig,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<()> {
        let path = normalize(path);
        if !visited.insert(path.clone()) || visited.len() > 256 {
            return Ok(());
        }
        if !self.load_file(&path, "", &[])? {
            config.libraries_available = false;
            config.incomplete = true;
            self.warning.push(format!(
                "TypeScript library declaration {} is unavailable.",
                path.display()
            ));
            return Ok(());
        }
        let index = self.file[&path].index.clone();
        config.globals.push(path.clone());
        config.incomplete |= index.incomplete;
        for name in &index.library_reference {
            let name = format!("lib.{}.d.ts", name.to_lowercase());
            // TypeScript permits npm packages to override selected library declarations.
            let override_name = name
                .trim_start_matches("lib.")
                .trim_end_matches(".d.ts")
                .replace('.', "/");
            let target = find_package_directory(
                &config.directory,
                &format!("@typescript/lib-{override_name}"),
            )
            .and_then(|root| self.package_entry(&root, ".", config, None))
            .unwrap_or_else(|| path.parent().unwrap().join(name));
            self.load_ts_library(&target, config, visited)?;
        }
        Ok(())
    }

    fn load_ts_global(
        &mut self,
        path: &Path,
        config: &mut TypescriptConfig,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<()> {
        let path = normalize(path);
        if !visited.insert(path.clone()) || visited.len() > 512 {
            return Ok(());
        }
        if !self.load_file(&path, "", &[])? {
            config.incomplete = true;
            return Ok(());
        }
        config.globals.push(path.clone());
        let index = self.file[&path].index.clone();
        config.incomplete |= index.incomplete;
        for reference in &index.path_reference {
            self.load_ts_global(
                &normalize(&path.parent().unwrap().join(reference)),
                config,
                visited,
            )?;
        }
        for import in &index.import {
            if let Some(origin) = &import.source {
                if let Some(target) = self.ts_module(&path, origin, config) {
                    self.load_ts_global(&target, config, visited)?;
                } else {
                    config.incomplete = true;
                }
            }
        }
        Ok(())
    }

    pub(super) fn resolve_ts(
        &mut self,
        file: &IndexedFile,
        reference: &DeclarationReference,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        if reference.path.len() == 1
            && matches!(
                reference.path[0].as_str(),
                "string"
                    | "number"
                    | "boolean"
                    | "bigint"
                    | "symbol"
                    | "object"
                    | "any"
                    | "unknown"
                    | "never"
                    | "void"
                    | "undefined"
                    | "null"
                    | "intrinsic"
            )
        {
            return DeclarationResolution::Intrinsic;
        }
        for depth in (0..=reference.scope.len()).rev() {
            let candidates = file
                .index
                .symbol
                .iter()
                .filter(|symbol| {
                    symbol.scope == reference.scope[..depth]
                        && symbol.name == reference.path[0]
                        && if reference.value_namespace {
                            symbol.value_namespace
                        } else {
                            symbol.type_namespace
                        }
                })
                .collect::<Vec<_>>();
            if let Some(symbol) = candidates.first() {
                if reference.path.len() == 1 {
                    return resolved(file, symbol);
                }
                return self.ts_member(
                    &file.path,
                    &joined(&symbol.scope, &[symbol.name.clone()]),
                    &reference.path[1..],
                    reference.value_namespace,
                    false,
                    visited,
                );
            }
            let imports = file
                .index
                .import
                .iter()
                .filter(|import| {
                    !import.export
                        && import.scope == reference.scope[..depth]
                        && import.alias.as_ref() == reference.path.first()
                })
                .cloned()
                .collect::<Vec<_>>();
            if !imports.is_empty() {
                return combine(
                    imports
                        .iter()
                        .map(|import| {
                            let path = if import.namespace {
                                reference.path[1..].to_vec()
                            } else {
                                joined(&import.path, &reference.path[1..])
                            };
                            self.resolve_ts_import(
                                file,
                                import,
                                &path,
                                reference.value_namespace,
                                visited,
                            )
                        })
                        .collect(),
                    || invalid("import does not expose the requested type"),
                    false,
                );
            }
        }
        let config = self
            .typescript
            .config
            .get(&self.nearest_tsconfig(&file.path))
            .cloned()
            .unwrap_or_default();
        let mut results = Vec::new();
        for global in &config.globals {
            let Some(global) = self.file.get(global) else {
                continue;
            };
            if let Some(symbol) = global.index.symbol.iter().find(|symbol| {
                symbol.global
                    && symbol.scope.is_empty()
                    && symbol.name == reference.path[0]
                    && if reference.value_namespace {
                        symbol.value_namespace
                    } else {
                        symbol.type_namespace
                    }
            }) {
                // Ambient interfaces and namespaces merge across library files.
                if reference.path.len() == 1 {
                    return resolved(global, symbol);
                }
                results.push(self.ts_member(
                    &global.path.clone(),
                    &[symbol.name.clone()],
                    &reference.path[1..],
                    reference.value_namespace,
                    false,
                    visited,
                ));
            }
        }
        combine(results, || {
            if !config.libraries_available || config.incomplete || file.index.incomplete {
                unverified("global declarations or syntax evidence are unavailable or incomplete")
            } else {
                invalid(&format!(
                    "no local, imported, or global type resolves `{}`",
                    reference.path.join(".")
                ))
            }
        }, false)
    }

    pub(super) fn resolve_ts_import(
        &mut self,
        file: &IndexedFile,
        import: &DeclarationImport,
        path: &[String],
        value: bool,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        let config = self
            .typescript
            .config
            .get(&self.nearest_tsconfig(&file.path))
            .cloned()
            .unwrap_or_default();
        let target = match &import.source {
            Some(source) => match self.ts_module(&file.path, source, &config) {
                Some(path) => path,
                None => {
                    for global in &config.globals {
                        let Some(declaration) = self.file.get(global) else {
                            continue;
                        };
                        if declaration.index.symbol.iter().any(|symbol| {
                            symbol.scope.is_empty() && glob_match(&symbol.name, source)
                        }) {
                            let name = declaration
                                .index
                                .symbol
                                .iter()
                                .find(|symbol| {
                                    symbol.scope.is_empty() && glob_match(&symbol.name, source)
                                })
                                .unwrap()
                                .name
                                .clone();
                            return self.ts_member(global, &[name], path, value, true, visited);
                        }
                    }
                    if source.starts_with('.') && !config.resolution_incomplete {
                        return invalid(&format!(
                            "module `{source}` has no source or declaration file"
                        ));
                    }
                    return unverified(&format!(
                        "package declaration entry point for `{source}` is unavailable or unsupported"
                    ));
                }
            },
            None => file.path.clone(),
        };
        if import.source.is_some() && import.path.is_empty() && !import.glob && !import.namespace {
            return match self.load_file(&target, "", &[]) {
                Ok(true) => DeclarationResolution::Intrinsic,
                _ => unverified("side-effect import source is unavailable"),
            };
        }
        self.ts_member(&target, &[], path, value, import.source.is_some(), visited)
    }

    fn ts_member(
        &mut self,
        path: &Path,
        scope: &[String],
        names: &[String],
        value: bool,
        exported: bool,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        let key = format!(
            "{}:{}:{}:{value}:{exported}",
            path.display(),
            scope.join("."),
            names.join(".")
        );
        if visited.len() > 256 || !visited.insert(key.clone()) {
            return unverified("cyclic or excessively deep TypeScript re-export chain");
        }
        let result = self.ts_member_inner(path, scope, names, value, exported, visited);
        visited.remove(&key);
        result
    }

    fn ts_member_inner(
        &mut self,
        path: &Path,
        scope: &[String],
        names: &[String],
        value: bool,
        exported: bool,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        match self.load_file(path, "", &[]) {
            Ok(true) => {}
            Ok(false) => return unverified("dependency declaration source is unavailable"),
            Err(error) => return unverified(&error.to_string()),
        }
        let file = self.file[&normalize(path)].clone();
        if names.is_empty() || names == ["*"] {
            return if file.index.external_module {
                DeclarationResolution::Intrinsic
            } else {
                invalid("file is a global script, not an importable module")
            };
        }
        let name = &names[0];
        let symbols = file
            .index
            .symbol
            .iter()
            .filter(|symbol| {
                symbol.scope == scope
                    && &symbol.name == name
                    && (!exported || symbol.visibility == SymbolVisibility::Public)
                    && if value {
                        symbol.value_namespace
                    } else {
                        symbol.type_namespace
                    }
            })
            .collect::<Vec<_>>();
        if let Some(symbol) = symbols.first() {
            if names.len() == 1 {
                return resolved(&file, symbol);
            }
            return self.ts_member(
                path,
                &joined(scope, &[name.clone()]),
                &names[1..],
                value,
                exported,
                visited,
            );
        }
        let mut results = Vec::new();
        for import in file.index.import.iter().filter(|import| {
            import.export
                && import.scope == scope
                && (import.glob && name != "default" || import.alias.as_ref() == Some(name))
        }) {
            let target = if import.namespace {
                names[1..].to_vec()
            } else if import.glob {
                names.to_vec()
            } else {
                joined(&import.path, &names[1..])
            };
            if import.source.is_none() {
                results.push(self.ts_member(path, scope, &target, value, false, visited));
            } else {
                results.push(self.resolve_ts_import(&file, import, &target, value, visited));
            }
        }
        combine(results, || {
            if file.index.incomplete {
                unverified("module declaration syntax is incomplete or unsupported")
            } else {
                invalid(&format!(
                    "module does not export `{}` in the requested namespace",
                    names.join(".")
                ))
            }
        }, false)
    }

    fn ts_module(&self, origin: &Path, source: &str, config: &TypescriptConfig) -> Option<PathBuf> {
        if ["rootDirs", "moduleSuffixes"]
            .iter()
            .any(|name| config.option.get(*name).is_some())
        {
            return None;
        }
        let directory = origin.parent()?;
        if source.starts_with('.') || Path::new(source).is_absolute() {
            return self.ts_candidate(&directory.join(source));
        }
        let base = config
            .option
            .get("baseUrl")
            .and_then(Value::as_str)
            .map(|base| config.directory.join(base))
            .unwrap_or_else(|| config.directory.clone());
        if let Some(paths) = config.option.get("paths").and_then(Value::as_object) {
            let mut patterns = paths.iter().collect::<Vec<_>>();
            patterns.sort_by_key(|(pattern, _)| {
                std::cmp::Reverse(pattern.split('*').next().unwrap_or("").len())
            });
            for (pattern, targets) in patterns {
                let capture = if let Some((prefix, suffix)) = pattern.split_once('*') {
                    source
                        .strip_prefix(prefix)
                        .and_then(|rest| rest.strip_suffix(suffix))
                } else if pattern == source {
                    Some("")
                } else {
                    None
                };
                if let Some(capture) = capture {
                    for target in targets
                        .as_array()
                        .into_iter()
                        .flatten()
                        .filter_map(Value::as_str)
                    {
                        if let Some(path) =
                            self.ts_candidate(&base.join(target.replace('*', capture)))
                        {
                            return Some(path);
                        }
                    }
                }
            }
        }
        if config.option.get("baseUrl").is_some() {
            if let Some(path) = self.ts_candidate(&base.join(source)) {
                return Some(path);
            }
        }
        let source = source.strip_prefix("node:").unwrap_or(source);
        let (package, subpath) = package_parts(source);
        let export_path = if subpath.is_empty() {
            ".".to_owned()
        } else {
            format!("./{subpath}")
        };
        if let Some(root) = find_package_directory(directory, &package) {
            return self.package_entry(&root, &export_path, config, Some(origin));
        }
        let types = package.trim_start_matches('@').replace('/', "__");
        find_package_directory(directory, &format!("@types/{types}"))
            .and_then(|root| self.package_entry(&root, &export_path, config, Some(origin)))
    }

    fn ts_candidate(&self, path: &Path) -> Option<PathBuf> {
        let path = normalize(path);
        let extension = path.extension().and_then(|extension| extension.to_str());
        let mut candidates = Vec::new();
        if matches!(extension, Some("js" | "jsx" | "mjs" | "cjs")) {
            let base = path.with_extension("");
            let extensions = match extension {
                Some("mjs") => vec!["mts", "d.mts"],
                Some("cjs") => vec!["cts", "d.cts"],
                _ => vec!["ts", "tsx", "d.ts"],
            };
            for extension in extensions {
                candidates.push(base.with_extension(extension));
            }
        } else {
            candidates.push(path.clone());
            if extension.is_none() {
                for extension in ["ts", "tsx", "d.ts"] {
                    candidates.push(path.with_extension(extension));
                }
            }
        }
        for file in ["index.ts", "index.tsx", "index.d.ts"] {
            candidates.push(path.join(file));
        }
        candidates.into_iter().find(|candidate| {
            matches!(
                candidate
                    .extension()
                    .and_then(|extension| extension.to_str()),
                Some("ts" | "tsx" | "mts" | "cts")
            ) && self.read(candidate).is_some()
        })
    }

    fn config_candidate(&self, path: &Path) -> Option<PathBuf> {
        [
            path.to_path_buf(),
            path.with_extension("json"),
            path.join("tsconfig.json"),
        ]
        .into_iter()
        .find(|path| self.read(path).is_some())
    }

    fn package_entry(
        &self,
        root: &Path,
        subpath: &str,
        config: &TypescriptConfig,
        origin: Option<&Path>,
    ) -> Option<PathBuf> {
        let manifest = self
            .read(&root.join("package.json"))
            .and_then(|text| serde_json::from_str::<Value>(&text).ok());
        if let Some(manifest) = &manifest {
            let mode = config
                .option
                .get("moduleResolution")
                .and_then(Value::as_str)
                .unwrap_or("node10")
                .to_ascii_lowercase();
            if matches!(mode.as_str(), "node16" | "nodenext" | "bundler") {
                if let Some(exports) = manifest.get("exports") {
                    let mut capture = None;
                    let export = if exports
                        .as_object()
                        .is_some_and(|object| object.keys().any(|name| name.starts_with('.')))
                    {
                        exports.get(subpath).or_else(|| {
                            let mut patterns = exports.as_object()?.iter().collect::<Vec<_>>();
                            patterns.sort_by_key(|(pattern, _)| {
                                std::cmp::Reverse(pattern.split('*').next().unwrap_or("").len())
                            });
                            patterns.into_iter().find_map(|(pattern, target)| {
                                let (prefix, suffix) = pattern.split_once('*')?;
                                capture = subpath.strip_prefix(prefix)?.strip_suffix(suffix);
                                capture.map(|_| target)
                            })
                        })
                    } else if subpath == "." {
                        Some(exports)
                    } else {
                        None
                    };
                    let importer = origin.unwrap_or(&config.directory);
                    let esm = mode == "bundler"
                        || importer
                            .extension()
                            .is_some_and(|extension| extension == "mts")
                        || importer
                            .ancestors()
                            .find_map(|directory| {
                                self.read(&directory.join("package.json"))
                                    .and_then(|text| serde_json::from_str::<Value>(&text).ok())
                            })
                            .is_some_and(|manifest| {
                                manifest.get("type").and_then(Value::as_str) == Some("module")
                            });
                    let mut conditions =
                        vec!["types", if esm { "import" } else { "require" }, "default"];
                    if let Some(custom) = config
                        .option
                        .get("customConditions")
                        .and_then(Value::as_array)
                    {
                        conditions.extend(custom.iter().filter_map(Value::as_str));
                    }
                    if let Some(target) = export.and_then(|value| export_target(value, &conditions))
                    {
                        return self.ts_candidate(&root.join(if let Some(capture) = capture {
                            target.replace('*', capture)
                        } else {
                            target.into()
                        }));
                    }
                    return None;
                }
            }
            if subpath == "." {
                if manifest.get("typesVersions").is_some() {
                    return None;
                }
                for key in ["types", "typings", "main"] {
                    if let Some(path) = manifest.get(key).and_then(Value::as_str) {
                        if let Some(path) = self.ts_candidate(&root.join(path)) {
                            return Some(path);
                        }
                    }
                }
            }
        }
        self.ts_candidate(&root.join(if subpath == "." {
            "index"
        } else {
            subpath.trim_start_matches("./")
        }))
    }
}

fn export_target<'value>(value: &'value Value, conditions: &[&str]) -> Option<&'value str> {
    match value {
        Value::String(target) => Some(target),
        Value::Array(values) => values
            .iter()
            .find_map(|value| export_target(value, conditions)),
        Value::Object(object) => conditions.iter().find_map(|condition| {
            object
                .get(*condition)
                .and_then(|value| export_target(value, conditions))
        }),
        _ => None,
    }
}

fn read_json(path: &Path) -> Option<Value> {
    std::fs::read_to_string(path)
        .ok()
        .and_then(|text| serde_json::from_str(&text).ok())
}
fn find_package_directory(directory: &Path, name: &str) -> Option<PathBuf> {
    directory
        .ancestors()
        .map(|directory| directory.join("node_modules").join(name))
        .find(|path| path.is_dir())
}
fn package_parts(source: &str) -> (String, String) {
    let mut parts = source.split('/');
    let first = parts.next().unwrap_or("");
    let package = if first.starts_with('@') {
        format!("{first}/{}", parts.next().unwrap_or(""))
    } else {
        first.into()
    };
    (package, parts.collect::<Vec<_>>().join("/"))
}

fn included(path: &Path, config: &TypescriptConfig) -> bool {
    let Ok(relative) = path.strip_prefix(&config.directory) else {
        return false;
    };
    let relative = relative.to_string_lossy().replace('\\', "/");
    let matches = |pattern: &str| {
        if Path::new(pattern).is_absolute() {
            glob_match(
                &pattern.replace('\\', "/"),
                &path.to_string_lossy().replace('\\', "/"),
            )
        } else {
            glob_match(pattern, &relative)
        }
    };
    if let Some(files) = config.config.get("files").and_then(Value::as_array) {
        if files.iter().filter_map(Value::as_str).any(matches) {
            return true;
        }
    }
    if config
        .config
        .get("exclude")
        .and_then(Value::as_array)
        .is_some_and(|patterns| patterns.iter().filter_map(Value::as_str).any(matches))
    {
        return false;
    }
    if let Some(patterns) = config.config.get("include").and_then(Value::as_array) {
        return patterns.iter().filter_map(Value::as_str).any(matches);
    }
    config.config.get("files").is_none()
}

fn glob_match(pattern: &str, text: &str) -> bool {
    if !pattern.contains('*') {
        return text == pattern || text.starts_with(&format!("{}/", pattern.trim_end_matches('/')));
    }
    let parts = pattern
        .split('*')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    let mut remainder = text;
    for (index, part) in parts.iter().enumerate() {
        let part = part.trim_start_matches('/');
        let Some(offset) = remainder.find(part) else {
            return false;
        };
        if index == 0 && !pattern.starts_with('*') && offset != 0 {
            return false;
        }
        remainder = &remainder[offset + part.len()..];
    }
    pattern.ends_with('*') || remainder.is_empty()
}
