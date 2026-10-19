//! Resolves TypeScript names within captured plan declarations.

use super::*;
use forge_diff::syntax::DeclarationImport;

impl DeclarationResolver {
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
        let globals = self
            .file
            .values()
            .filter_map(|global| {
                global
                    .index
                    .symbol
                    .iter()
                    .find(|symbol| {
                        symbol.global
                            && symbol.scope.is_empty()
                            && symbol.name == reference.path[0]
                            && if reference.value_namespace {
                                symbol.value_namespace
                            } else {
                                symbol.type_namespace
                            }
                    })
                    .map(|symbol| (global.path.clone(), symbol.clone()))
            })
            .collect::<Vec<_>>();
        let results = globals
            .into_iter()
            .map(|(path, symbol)| {
                if reference.path.len() == 1 {
                    resolved(&self.file[&path], &symbol)
                } else {
                    self.ts_member(
                        &path,
                        &[symbol.name],
                        &reference.path[1..],
                        reference.value_namespace,
                        false,
                        visited,
                    )
                }
            })
            .collect();
        let internal = self.file.values().any(|candidate| {
            candidate
                .index
                .symbol
                .iter()
                .any(|symbol| symbol.name == reference.path[0])
        });
        combine(
            results,
            || {
                if internal {
                    invalid("planned declaration is not visible in this scope")
                } else {
                    unverified(
                        "Unbound global or external references are unchecked during planning",
                    )
                }
            },
            false,
        )
    }

    pub(super) fn resolve_ts_import(
        &mut self,
        file: &IndexedFile,
        import: &DeclarationImport,
        path: &[String],
        value: bool,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        let target = match &import.source {
            Some(source) => match self.ts_module(&file.path, source) {
                Some(target) => target,
                None => {
                    if source.starts_with('.') {
                        let base = normalize(&file.path.parent().unwrap().join(source));
                        let removed = self.workspace_files.iter().any(|path| {
                            let candidate = normalize(&self.workspace.join(path));
                            (candidate == base
                                || candidate.with_extension("") == base
                                || candidate == base.join("index.ts"))
                                && !self.snapshot.contains_key(path)
                        });
                        if removed {
                            return invalid("import refers to a file removed by the plan");
                        }
                    }
                    return unverified(
                        "Imports outside the captured plan are unchecked during planning",
                    );
                }
            },
            None => file.path.clone(),
        };
        if import.source.is_some() && import.path.is_empty() && !import.glob && !import.namespace {
            return DeclarationResolution::Intrinsic;
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
        combine(
            results,
            || {
                if file.index.incomplete {
                    unverified("module declaration syntax is incomplete or unsupported")
                } else {
                    invalid(&format!(
                        "module does not export `{}` in the requested namespace",
                        names.join(".")
                    ))
                }
            },
            false,
        )
    }

    fn ts_module(&self, origin: &Path, source: &str) -> Option<PathBuf> {
        if source.starts_with('.') || Path::new(source).is_absolute() {
            self.ts_candidate(&origin.parent()?.join(source))
        } else {
            None
        }
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
}
