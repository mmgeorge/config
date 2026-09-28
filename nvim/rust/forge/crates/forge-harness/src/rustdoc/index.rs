use super::RustdocError;
use super::source::{RustdocSourceLocation, SourceGraph, SourcePackage};
use crate::plan::PlanCallableKind;
use quote::ToTokens;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Clone)]
/// Source declaration shared by signature checking, hover, and navigation.
pub(crate) struct SourceItem {
    pub package_id: String,
    pub path: String,
    pub signature: String,
    pub docs: String,
    pub location: RustdocSourceLocation,
    pub input: Option<Vec<String>>,
    pub output: Option<String>,
    pub generic: Vec<String>,
    alias: Option<String>,
}

#[derive(Clone)]
struct Import {
    module: String,
    name: Option<String>,
    target: String,
}

#[derive(Clone)]
struct Callable {
    module: String,
    receiver: String,
    kind: PlanCallableKind,
    name: String,
    item: SourceItem,
}

#[derive(Default)]
struct PackageIndex {
    item: HashMap<String, Vec<SourceItem>>,
    module: HashSet<String>,
    import: Vec<Import>,
    callable: Vec<Callable>,
}

/// Resolves declarations through source modules and re-exports without compiling dependencies.
pub(crate) struct SourceIndex {
    package: HashMap<String, SourcePackage>,
    dependency: HashMap<String, HashMap<String, String>>,
    parsed: HashMap<String, PackageIndex>,
}

impl SourceIndex {
    pub(crate) fn new(graph: SourceGraph) -> Self {
        Self {
            package: graph
                .packages
                .into_iter()
                .map(|package| (package.id.clone(), package))
                .collect(),
            dependency: graph
                .resolve
                .nodes
                .into_iter()
                .map(|node| {
                    (
                        node.id,
                        node.deps
                            .into_iter()
                            .map(|dependency| (dependency.name, dependency.pkg))
                            .collect(),
                    )
                })
                .collect(),
            parsed: HashMap::new(),
        }
    }

    fn load(&mut self, package_id: &str) -> Result<(), RustdocError> {
        if self.parsed.contains_key(package_id) {
            return Ok(());
        }
        let package = self
            .package
            .get(package_id)
            .ok_or_else(|| unavailable("package absent from Cargo graph"))?;
        let target = package
            .targets
            .iter()
            .find(|target| {
                target
                    .kind
                    .iter()
                    .any(|kind| matches!(kind.as_str(), "lib" | "proc-macro" | "rlib"))
            })
            .ok_or_else(|| unavailable("dependency has no library source"))?;
        let root = package
            .manifest_path
            .parent()
            .ok_or_else(|| unavailable("package has no source directory"))?;
        let mut index = PackageIndex::default();
        parse_file(
            package,
            root,
            &target.src_path,
            "",
            &mut index,
            &mut HashSet::new(),
        )?;
        self.parsed.insert(package_id.into(), index);
        Ok(())
    }

    fn package_id(&self, package: &str, version: &str) -> Result<String, RustdocError> {
        let candidate = self
            .package
            .values()
            .filter(|candidate| candidate.name == package && candidate.version == version)
            .collect::<Vec<_>>();
        match candidate.as_slice() {
            [package] => Ok(package.id.clone()),
            [] => Err(unavailable(format!(
                "Cargo graph does not contain `{package}` {version}"
            ))),
            _ => Err(RustdocError::Ambiguous(format!(
                "multiple source identities for `{package}` {version}"
            ))),
        }
    }

    pub(crate) fn type_item(
        &mut self,
        package: &str,
        version: &str,
        receiver: &str,
    ) -> Result<SourceItem, RustdocError> {
        let package_id = self.package_id(package, version)?;
        let path = type_path(receiver)?;
        let mut candidate = self.resolve(&package_id, "", &path, &mut HashSet::new())?;
        if candidate.is_empty() && !path.contains("::") {
            candidate = self.resolve(
                &package_id,
                "",
                &format!("prelude::{path}"),
                &mut HashSet::new(),
            )?;
        }
        unique(candidate, receiver)
    }

    pub(crate) fn callable(
        &mut self,
        package: &str,
        version: &str,
        receiver: &str,
        name: &str,
        kind: PlanCallableKind,
    ) -> Result<SourceItem, RustdocError> {
        let mut owner = self.type_item(package, version, receiver)?;
        let mut alias_seen = HashSet::new();
        while let Some(alias) = owner.alias.clone() {
            if !alias_seen.insert((owner.package_id.clone(), owner.path.clone())) {
                return Err(unavailable(format!("cyclic source alias `{receiver}`")));
            }
            let module = parent(&owner.path);
            owner = unique(
                self.resolve(
                    &owner.package_id,
                    module,
                    &type_path(&alias)?,
                    &mut HashSet::new(),
                )?,
                &alias,
            )?;
        }
        self.load(&owner.package_id)?;
        let callable = self.parsed[&owner.package_id].callable.clone();
        let mut candidate = Vec::new();
        for callable in callable
            .into_iter()
            .filter(|callable| callable.name == name && callable.kind == kind)
        {
            let resolved = self.resolve(
                &owner.package_id,
                &callable.module,
                &callable.receiver,
                &mut HashSet::new(),
            )?;
            if resolved.iter().any(|resolved| {
                resolved.package_id == owner.package_id && resolved.path == owner.path
            }) {
                candidate.push(callable.item);
            }
        }
        unique(candidate, &format!("{receiver}::{name}"))
    }

    fn resolve(
        &mut self,
        package_id: &str,
        module: &str,
        path: &str,
        visited: &mut HashSet<String>,
    ) -> Result<Vec<SourceItem>, RustdocError> {
        let key = format!("{package_id}|{module}|{path}");
        if visited.len() >= 256 || !visited.insert(key.clone()) {
            return Ok(Vec::new());
        }
        let result = self.resolve_inner(package_id, module, path, visited);
        visited.remove(&key);
        result
    }

    fn resolve_inner(
        &mut self,
        package_id: &str,
        module: &str,
        path: &str,
        visited: &mut HashSet<String>,
    ) -> Result<Vec<SourceItem>, RustdocError> {
        self.load(package_id)?;
        let (head, tail) = path.split_once("::").unwrap_or((path, ""));
        if head == "crate" {
            return self.resolve(package_id, "", tail, visited);
        }
        if head == "self" {
            return self.resolve(package_id, module, tail, visited);
        }
        if head == "super" {
            return self.resolve(package_id, parent(module), tail, visited);
        }
        let package = &self.package[package_id];
        if package.targets.iter().any(|target| target.name == head) && !tail.is_empty() {
            return self.resolve(package_id, "", tail, visited);
        }
        let full = join(module, path);
        if let Some(item) = self.parsed[package_id].item.get(&full) {
            return Ok(item.clone());
        }
        let nested = join(module, head);
        if !tail.is_empty() && self.parsed[package_id].module.contains(&nested) {
            return self.resolve(package_id, &nested, tail, visited);
        }
        let import = self.parsed[package_id]
            .import
            .iter()
            .filter(|import| import.module == module)
            .cloned()
            .collect::<Vec<_>>();
        let named = import
            .iter()
            .filter(|import| import.name.as_deref() == Some(head))
            .collect::<Vec<_>>();
        if !named.is_empty() {
            let mut candidate = Vec::new();
            for import in named {
                candidate.extend(self.resolve(
                    package_id,
                    module,
                    &join(&import.target, tail),
                    visited,
                )?);
            }
            return Ok(candidate);
        }
        if let Some(dependency) = self
            .dependency
            .get(package_id)
            .and_then(|map| map.get(head))
            .cloned()
        {
            if !tail.is_empty() {
                return self.resolve(&dependency, "", tail, visited);
            }
        }
        let mut candidate = Vec::new();
        for import in import.iter().filter(|import| import.name.is_none()) {
            candidate.extend(self.resolve(
                package_id,
                module,
                &join(&import.target, path),
                visited,
            )?);
        }
        Ok(candidate)
    }
}

fn parse_file(
    package: &SourcePackage,
    root: &Path,
    file: &Path,
    module: &str,
    index: &mut PackageIndex,
    visited: &mut HashSet<PathBuf>,
) -> Result<(), RustdocError> {
    let file = file
        .canonicalize()
        .map_err(|error| unavailable(format!("read source {}: {error}", file.display())))?;
    let root = root
        .canonicalize()
        .map_err(|error| unavailable(error.to_string()))?;
    if !file.starts_with(&root) {
        return Err(unavailable("source module escapes its Cargo package"));
    }
    if !visited.insert(file.clone()) {
        return Ok(());
    }
    if visited.len() > 4096 {
        return Err(unavailable(
            "source index exceeded 4096 module files per package",
        ));
    }
    let source = std::fs::read_to_string(&file).map_err(|error| unavailable(error.to_string()))?;
    let parsed = syn::parse_file(&source)
        .map_err(|error| unavailable(format!("parse source {}: {error}", file.display())))?;
    let directory = if matches!(
        file.file_name().and_then(|name| name.to_str()),
        Some("lib.rs" | "mod.rs")
    ) {
        file.parent().unwrap().to_path_buf()
    } else {
        file.with_extension("")
    };
    parse_items(
        package,
        &root,
        &file,
        &directory,
        module,
        parsed.items,
        index,
        visited,
    )
}

fn parse_items(
    package: &SourcePackage,
    root: &Path,
    file: &Path,
    directory: &Path,
    module: &str,
    items: Vec<syn::Item>,
    index: &mut PackageIndex,
    visited: &mut HashSet<PathBuf>,
) -> Result<(), RustdocError> {
    index.module.insert(module.into());
    for item in items {
        match item {
            syn::Item::Mod(declaration) => {
                if declaration.attrs.iter().any(is_test_only) {
                    continue;
                }
                let nested = join(module, &declaration.ident.to_string());
                if let Some((_, items)) = declaration.content {
                    parse_items(
                        package,
                        root,
                        file,
                        &directory.join(declaration.ident.to_string()),
                        &nested,
                        items,
                        index,
                        visited,
                    )?;
                } else {
                    let explicit = declaration.attrs.iter().find_map(|attribute| {
                        if !attribute.path().is_ident("path") {
                            return None;
                        }
                        let syn::Meta::NameValue(value) = &attribute.meta else {
                            return None;
                        };
                        let syn::Expr::Lit(value) = &value.value else {
                            return None;
                        };
                        let syn::Lit::Str(value) = &value.lit else {
                            return None;
                        };
                        Some(directory.join(value.value()))
                    });
                    let direct = directory.join(format!("{}.rs", declaration.ident));
                    let nested_file = directory.join(declaration.ident.to_string()).join("mod.rs");
                    let path = explicit.unwrap_or_else(|| {
                        if direct.is_file() {
                            direct
                        } else {
                            nested_file
                        }
                    });
                    if path.is_file() {
                        parse_file(package, root, &path, &nested, index, visited)?;
                    }
                }
            }
            syn::Item::Use(declaration) => {
                flatten_use(module, "", declaration.tree, &mut index.import)
            }
            syn::Item::ExternCrate(declaration) => {
                let target = declaration.ident.to_string();
                let name = declaration
                    .rename
                    .map(|(_, name)| name.to_string())
                    .unwrap_or_else(|| target.clone());
                index.import.push(Import {
                    module: module.into(),
                    name: Some(name),
                    target,
                });
            }
            syn::Item::Struct(declaration) => {
                let signature = format!(
                    "{} struct {}{}",
                    declaration.vis.to_token_stream(),
                    declaration.ident,
                    declaration.generics.to_token_stream()
                );
                insert_type(
                    package,
                    file,
                    module,
                    &declaration.ident,
                    &declaration.attrs,
                    signature,
                    None,
                    index,
                );
            }
            syn::Item::Enum(declaration) => {
                let signature = format!(
                    "{} enum {}{}",
                    declaration.vis.to_token_stream(),
                    declaration.ident,
                    declaration.generics.to_token_stream()
                );
                insert_type(
                    package,
                    file,
                    module,
                    &declaration.ident,
                    &declaration.attrs,
                    signature,
                    None,
                    index,
                );
            }
            syn::Item::Trait(declaration) => {
                let signature = format!(
                    "{} trait {}{}",
                    declaration.vis.to_token_stream(),
                    declaration.ident,
                    declaration.generics.to_token_stream()
                );
                insert_type(
                    package,
                    file,
                    module,
                    &declaration.ident,
                    &declaration.attrs,
                    signature,
                    None,
                    index,
                );
            }
            syn::Item::Type(declaration) => {
                let signature = declaration.to_token_stream().to_string();
                insert_type(
                    package,
                    file,
                    module,
                    &declaration.ident,
                    &declaration.attrs,
                    signature,
                    Some(declaration.ty.to_token_stream().to_string()),
                    index,
                );
            }
            syn::Item::Impl(declaration) => {
                let receiver = match type_path(&declaration.self_ty.to_token_stream().to_string()) {
                    Ok(path) => path,
                    Err(_) => continue,
                };
                for item in declaration.items {
                    let syn::ImplItem::Fn(function) = item else {
                        continue;
                    };
                    if declaration.trait_.is_none()
                        && !matches!(function.vis, syn::Visibility::Public(_))
                    {
                        continue;
                    }
                    let kind = if function.sig.receiver().is_some() {
                        PlanCallableKind::Method
                    } else {
                        PlanCallableKind::Function
                    };
                    let name = function.sig.ident.to_string();
                    let mut item = source_item(
                        package,
                        file,
                        &join(module, &format!("{receiver}::{name}")),
                        &function.sig.ident,
                        &function.attrs,
                        function.sig.to_token_stream().to_string(),
                    );
                    item.generic = declaration
                        .generics
                        .type_params()
                        .chain(function.sig.generics.type_params())
                        .map(|parameter| parameter.ident.to_string())
                        .collect();
                    item.input = Some(
                        function
                            .sig
                            .inputs
                            .iter()
                            .filter_map(|argument| match argument {
                                syn::FnArg::Typed(argument) => {
                                    Some(argument.ty.to_token_stream().to_string())
                                }
                                _ => None,
                            })
                            .collect(),
                    );
                    item.output = Some(match function.sig.output {
                        syn::ReturnType::Default => "()".into(),
                        syn::ReturnType::Type(_, output) => output.to_token_stream().to_string(),
                    });
                    index.callable.push(Callable {
                        module: module.into(),
                        receiver: receiver.clone(),
                        kind,
                        name,
                        item,
                    });
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn insert_type(
    package: &SourcePackage,
    file: &Path,
    module: &str,
    name: &syn::Ident,
    attributes: &[syn::Attribute],
    signature: String,
    alias: Option<String>,
    index: &mut PackageIndex,
) {
    let path = join(module, &name.to_string());
    let mut item = source_item(package, file, &path, name, attributes, signature);
    item.alias = alias;
    index.item.entry(path).or_default().push(item);
}

fn source_item(
    package: &SourcePackage,
    file: &Path,
    path: &str,
    name: &syn::Ident,
    attributes: &[syn::Attribute],
    signature: String,
) -> SourceItem {
    let start = name.span().start();
    SourceItem {
        package_id: package.id.clone(),
        path: path.into(),
        signature,
        docs: attributes
            .iter()
            .filter_map(|attribute| {
                if !attribute.path().is_ident("doc") {
                    return None;
                }
                let syn::Meta::NameValue(value) = &attribute.meta else {
                    return None;
                };
                let syn::Expr::Lit(value) = &value.value else {
                    return None;
                };
                let syn::Lit::Str(value) = &value.lit else {
                    return None;
                };
                Some(value.value().trim_start().to_string())
            })
            .collect::<Vec<_>>()
            .join("\n"),
        location: RustdocSourceLocation {
            package: package.name.clone(),
            version: package.version.clone(),
            path: dunce::simplified(file).into(),
            line: start.line,
            column: start.column + 1,
        },
        input: None,
        output: None,
        generic: Vec::new(),
        alias: None,
    }
}

fn flatten_use(module: &str, prefix: &str, tree: syn::UseTree, imports: &mut Vec<Import>) {
    match tree {
        syn::UseTree::Path(path) => flatten_use(
            module,
            &join(prefix, &path.ident.to_string()),
            *path.tree,
            imports,
        ),
        syn::UseTree::Name(name) => {
            let (target, name) = if name.ident == "self" {
                (
                    prefix.into(),
                    prefix.rsplit("::").next().unwrap_or(prefix).into(),
                )
            } else {
                (
                    join(prefix, &name.ident.to_string()),
                    name.ident.to_string(),
                )
            };
            imports.push(Import {
                module: module.into(),
                name: Some(name),
                target,
            });
        }
        syn::UseTree::Rename(rename) => imports.push(Import {
            module: module.into(),
            name: Some(rename.rename.to_string()),
            target: if rename.ident == "self" {
                prefix.into()
            } else {
                join(prefix, &rename.ident.to_string())
            },
        }),
        syn::UseTree::Glob(_) => imports.push(Import {
            module: module.into(),
            name: None,
            target: prefix.into(),
        }),
        syn::UseTree::Group(group) => {
            for tree in group.items {
                flatten_use(module, prefix, tree, imports);
            }
        }
    }
}

fn is_test_only(attribute: &syn::Attribute) -> bool {
    attribute.path().is_ident("cfg")
        && matches!(&attribute.meta, syn::Meta::List(list) if list.tokens.to_string() == "test")
}

fn type_path(source: &str) -> Result<String, RustdocError> {
    let parsed = syn::parse_str::<syn::Type>(source)
        .map_err(|error| unavailable(format!("cannot parse type `{source}`: {error}")))?;
    let syn::Type::Path(path) = parsed else {
        return Err(unavailable(format!("unsupported receiver `{source}`")));
    };
    Ok(path
        .path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>()
        .join("::"))
}

fn unique(mut candidate: Vec<SourceItem>, name: &str) -> Result<SourceItem, RustdocError> {
    candidate.sort_by(|left, right| {
        (
            &left.location.path,
            left.location.line,
            left.location.column,
        )
            .cmp(&(
                &right.location.path,
                right.location.line,
                right.location.column,
            ))
    });
    candidate.dedup_by(|left, right| left.location == right.location);
    match candidate.len() {
        1 => Ok(candidate.remove(0)),
        0 => Err(unavailable(format!(
            "could not resolve `{name}` in Cargo source (macro-generated declarations and inactive optional dependencies may be unavailable)"
        ))),
        count => Err(RustdocError::Ambiguous(format!(
            "`{name}` has {count} source declarations; use a qualified path"
        ))),
    }
}

fn join(prefix: &str, suffix: &str) -> String {
    if prefix.is_empty() {
        suffix.into()
    } else if suffix.is_empty() {
        prefix.into()
    } else {
        format!("{prefix}::{suffix}")
    }
}
fn parent(path: &str) -> &str {
    path.rsplit_once("::").map_or("", |(parent, _)| parent)
}
fn unavailable(message: impl Into<String>) -> RustdocError {
    RustdocError::Unavailable(message.into())
}

#[cfg(test)]
pub(super) mod test {
    use super::*;

    pub(in crate::rustdoc) fn fixture() -> (tempfile::TempDir, SourceIndex) {
        let directory = tempfile::tempdir().unwrap();
        let facade = directory.path().join("facade");
        let engine = directory.path().join("engine");
        std::fs::create_dir_all(&facade).unwrap();
        std::fs::create_dir_all(&engine).unwrap();
        std::fs::write(facade.join("lib.rs"), "pub use dependency::prelude; pub use dependency::Clock as Timer; pub type Alias<T> = dependency::Clock<T>;").unwrap();
        std::fs::write(
            engine.join("lib.rs"),
            r#"
mod clock;
pub use clock::Clock;
pub mod prelude { pub use crate::Clock; }
pub mod other { pub struct Clock; impl Clock { pub fn elapsed(&self) -> bool { false } } }
"#,
        )
        .unwrap();
        std::fs::write(
            engine.join("clock.rs"),
            r#"
/// Measures elapsed time.
pub struct Clock<T = ()> { marker: T }
impl<T> Clock<T> {
    /// Reads the elapsed seconds.
    pub fn elapsed(&self) -> f32 { 0.0 }
    pub fn reset(&mut self, elapsed: f32) -> Result<(), std::io::Error> { Ok(()) }
    pub fn generic(&self, value: T) -> T { value }
    pub fn new() -> Self { todo!() }
}
"#,
        )
        .unwrap();
        let graph = serde_json::from_value(serde_json::json!({
            "packages": [
                { "id": "facade", "name": "facade", "version": "1.0.0", "manifest_path": facade.join("Cargo.toml"), "targets": [{"name": "facade", "kind": ["lib"], "src_path": facade.join("lib.rs")}] },
                { "id": "engine", "name": "engine", "version": "1.2.3", "manifest_path": engine.join("Cargo.toml"), "targets": [{"name": "engine", "kind": ["lib"], "src_path": engine.join("lib.rs")}] }
            ],
            "resolve": { "nodes": [{"id": "facade", "deps": [{"name": "dependency", "pkg": "engine"}]}, {"id": "engine", "deps": []}] }
        })).unwrap();
        (directory, SourceIndex::new(graph))
    }

    #[test]
    fn follows_dependency_reexports_to_the_exact_method_source() {
        let (_directory, mut index) = fixture();
        let declaration = index
            .callable(
                "facade",
                "1.0.0",
                "Clock",
                "elapsed",
                PlanCallableKind::Method,
            )
            .unwrap();
        #[cfg(windows)]
        assert!(!declaration.location.path.to_string_lossy().starts_with(r"\\?\"));
        assert_eq!(declaration.location.package, "engine");
        assert_eq!(declaration.location.version, "1.2.3");
        assert_eq!(declaration.output.as_deref(), Some("f32"));
        assert_eq!(declaration.docs, "Reads the elapsed seconds.");
        let source = std::fs::read_to_string(&declaration.location.path).unwrap();
        assert!(
            source.lines().nth(declaration.location.line - 1).unwrap()
                [declaration.location.column - 1..]
                .starts_with("elapsed")
        );
        let renamed = index
            .callable(
                "facade",
                "1.0.0",
                "Timer",
                "elapsed",
                PlanCallableKind::Method,
            )
            .unwrap();
        assert_eq!(renamed.location, declaration.location);
        let alias = index
            .callable(
                "facade",
                "1.0.0",
                "Alias<u32>",
                "elapsed",
                PlanCallableKind::Method,
            )
            .unwrap();
        assert_eq!(alias.location, declaration.location);
        assert!(
            index
                .callable(
                    "facade",
                    "1.0.0",
                    "Clock",
                    "elapsed",
                    PlanCallableKind::Function
                )
                .is_err()
        );
    }

    #[test]
    fn reports_ambiguous_exports_instead_of_selecting_a_same_named_type() {
        let (directory, mut index) = fixture();
        std::fs::write(
            directory.path().join("facade/lib.rs"),
            "pub use dependency::*; pub use dependency::other::*;",
        )
        .unwrap();
        assert!(matches!(
            index.type_item("facade", "1.0.0", "Clock"),
            Err(RustdocError::Ambiguous(_))
        ));
    }
}
