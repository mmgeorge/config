use super::super::AdapterError;
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};
use tree_sitter::{Node, Parser};

/// Retain one syntactic test declaration before exact runtime discovery.
#[derive(Clone)]
pub(super) struct StaticTest {
    pub name: String,
    pub path: PathBuf,
    pub line: u32,
}

/// Select the filesystem lookup rules for a target root or an external module.
enum SourceOrigin {
    Root,
    Module,
}

/// Carry the logical module and the two filesystem bases used by rustc.
struct ModuleScope<'a> {
    file: &'a Path,
    directory: &'a Path,
    attribute_directory: &'a Path,
    prefix: &'a str,
    depth: usize,
}

/// Own the parser and file cache only for this target-indexing operation.
struct Indexer<'a, Read> {
    read: &'a Read,
    parser: Parser,
    files: BTreeMap<PathBuf, String>,
    modules: BTreeSet<String>,
    cases: Vec<StaticTest>,
}

impl<Read: Fn(&Path) -> Result<String, std::io::Error>> Indexer<'_, Read> {
    /// Traverse a source file with separate module lookup and attribute bases.
    fn file(
        &mut self,
        file: &Path,
        prefix: &str,
        origin: SourceOrigin,
        depth: usize,
    ) -> Result<(), AdapterError> {
        if depth > 128 {
            return Err(AdapterError::Protocol(
                "Rust module nesting exceeds 128 levels".into(),
            ));
        }
        let source = match self.files.get(file) {
            Some(source) => source.clone(),
            None => {
                let source = (self.read)(file).map_err(AdapterError::Storage)?;
                self.files.insert(file.to_owned(), source.clone());
                source
            }
        };
        let tree = self
            .parser
            .parse(&source, None)
            .ok_or_else(|| AdapterError::Protocol("Rust syntax parsing was cancelled".into()))?;
        let parent = file
            .parent()
            .ok_or_else(|| AdapterError::Protocol("Rust source has no parent directory".into()))?;
        let module_dir = if matches!(origin, SourceOrigin::Root)
            || file.file_name().is_some_and(|name| name == "mod.rs")
        {
            parent.to_owned()
        } else {
            parent.join(file.file_stem().unwrap_or_default())
        };
        self.nodes(
            tree.root_node(),
            &source,
            ModuleScope {
                file,
                directory: &module_dir,
                attribute_directory: parent,
                prefix,
                depth,
            },
        )
    }

    /// Read only module-level attributes, test functions, and module declarations.
    fn nodes(
        &mut self,
        node: Node<'_>,
        source: &str,
        scope: ModuleScope<'_>,
    ) -> Result<(), AdapterError> {
        let ModuleScope {
            file,
            directory: module_dir,
            attribute_directory: attribute_dir,
            prefix,
            depth,
        } = scope;
        if depth > 128 {
            return Err(AdapterError::Protocol(
                "Rust module nesting exceeds 128 levels".into(),
            ));
        }
        let mut attributes = Vec::new();
        for child in node.named_children(&mut node.walk()) {
            match child.kind() {
                "attribute_item" => {
                    attributes.push(
                        child
                            .utf8_text(source.as_bytes())
                            .unwrap_or_default()
                            .to_owned(),
                    );
                }
                "line_comment" | "block_comment" => {}
                "function_item" => {
                    if attributes.iter().any(|attribute| test_attribute(attribute))
                        && let Some(name) = child.child_by_field_name("name")
                    {
                        let name = qualified(
                            prefix,
                            name.utf8_text(source.as_bytes()).unwrap_or_default(),
                        );
                        let line = u32::try_from(child.start_position().row + 1).map_err(|_| {
                            AdapterError::Protocol("Rust source line overflow".into())
                        })?;
                        self.cases.push(StaticTest {
                            name,
                            path: file.to_owned(),
                            line,
                        });
                        if self.cases.len() > super::super::contract::ITEM_LIMIT {
                            return Err(AdapterError::Protocol(
                                "Rust static index exceeds the test item limit".into(),
                            ));
                        }
                    }
                    attributes.clear();
                }
                "mod_item" => {
                    let Some(name) = child.child_by_field_name("name") else {
                        continue;
                    };
                    let name = name
                        .utf8_text(source.as_bytes())
                        .unwrap_or_default()
                        .trim_start_matches("r#");
                    let qualified = qualified(prefix, name);
                    if !self.modules.insert(qualified.clone()) {
                        attributes.clear();
                        continue;
                    }
                    let override_path = path_attribute(&attributes).map_err(|error| {
                        AdapterError::Protocol(format!("{}: {error}", file.display()))
                    })?;
                    if let Some(body) = child.child_by_field_name("body") {
                        let directory = override_path
                            .map_or_else(|| module_dir.join(name), |path| attribute_dir.join(path));
                        self.nodes(
                            body,
                            source,
                            ModuleScope {
                                file,
                                directory: &directory,
                                attribute_directory: &directory,
                                prefix: &qualified,
                                depth: depth + 1,
                            },
                        )?;
                    } else {
                        let target = if let Some(path) = override_path {
                            attribute_dir.join(path)
                        } else {
                            let flat = module_dir.join(format!("{name}.rs"));
                            if (self.read)(&flat).is_ok() {
                                flat
                            } else {
                                module_dir.join(name).join("mod.rs")
                            }
                        };
                        self.file(&target, &qualified, SourceOrigin::Module, depth + 1)?;
                    }
                    attributes.clear();
                }
                _ => attributes.clear(),
            }
        }
        Ok(())
    }
}

/// Index declared tests without compiling or executing a target.
pub(super) fn index_target(
    root: &Path,
    read: &impl Fn(&Path) -> Result<String, std::io::Error>,
) -> Result<Vec<StaticTest>, AdapterError> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_rust::LANGUAGE.into())
        .map_err(|error| AdapterError::Protocol(format!("Rust grammar: {error}")))?;
    let mut indexer = Indexer {
        read,
        parser,
        files: BTreeMap::new(),
        modules: BTreeSet::new(),
        cases: Vec::new(),
    };
    indexer.file(root, "", SourceOrigin::Root, 0)?;
    Ok(indexer.cases)
}

/// Construct rustc's qualified module or function name.
fn qualified(prefix: &str, name: &str) -> String {
    let name = name.trim_start_matches("r#");
    if prefix.is_empty() {
        name.to_owned()
    } else {
        format!("{prefix}::{name}")
    }
}

/// Recognize test attributes by their final path segment, including async runtimes.
fn test_attribute(attribute: &str) -> bool {
    attribute
        .trim()
        .strip_prefix("#[")
        .and_then(|attribute| attribute.strip_suffix(']'))
        .is_some_and(|attribute| {
            attribute
                .split('(')
                .next()
                .unwrap_or(attribute)
                .trim()
                .rsplit("::")
                .next()
                == Some("test")
        })
}

/// Decode an explicit module path without executing macro attributes.
fn path_attribute(attributes: &[String]) -> Result<Option<PathBuf>, AdapterError> {
    for attribute in attributes {
        let Some(value) = attribute
            .trim()
            .strip_prefix("#[")
            .and_then(|attribute| attribute.strip_suffix(']'))
            .and_then(|attribute| attribute.trim().strip_prefix("path"))
            .and_then(|attribute| attribute.trim().strip_prefix('='))
        else {
            continue;
        };
        let value = syn::parse_str::<syn::LitStr>(value.trim()).map_err(|error| {
            AdapterError::Protocol(format!("invalid Rust module path: {error}"))
        })?;
        if !value.suffix().is_empty() {
            return Err(AdapterError::Protocol(
                "Rust module paths cannot have literal suffixes".into(),
            ));
        }
        return Ok(Some(value.value().into()));
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Rust paths accept Rust escapes and raw strings, not JSON string syntax.
    #[test]
    fn path_literals_follow_rust_syntax() {
        for (attribute, expected) in [
            (r##"#[path = r#"nested/child.rs"#]"##, "nested/child.rs"),
            (r#"#[path = "\x63hild.rs"]"#, "child.rs"),
            (r#"#[path = "\u{03c0}.rs"]"#, "π.rs"),
            ("#[path = \"nested/\\\n    child.rs\"]", "nested/child.rs"),
        ] {
            assert_eq!(
                path_attribute(&[attribute.into()]).unwrap(),
                Some(expected.into())
            );
        }
        for attribute in [
            r#"#[path = "\u03c0.rs"]"#,
            r##"#[path = r#"child.rs"]"##,
            r#"#[path = "child.rs"suffix]"#,
            r#"#[path = "child.rs" + "other.rs"]"#,
        ] {
            assert!(
                path_attribute(&[attribute.into()]).is_err(),
                "accepted {attribute}"
            );
        }
    }

    /// Follow flat and directory modules, path overrides, inline modules, and async test attributes.
    #[test]
    fn static_discovery_follows_rust_module_layout() {
        let files = BTreeMap::from([
            (
                PathBuf::from("src/lib.rs"),
                "mod a; mod nested { #[path = \"custom.rs\"] mod chosen; } #[test] fn root() {}",
            ),
            (
                PathBuf::from("src/a.rs"),
                "mod child; #[path = \"renamed.rs\"] mod override_mod; #[tokio::test] async fn works() {}",
            ),
            (PathBuf::from("src/a/child/mod.rs"), "#[test] fn child() {}"),
            (PathBuf::from("src/renamed.rs"), "#[test] fn renamed() {}"),
            (
                PathBuf::from("src/nested/custom.rs"),
                "#[test] fn chosen() {}",
            ),
        ]);
        let cases = index_target(Path::new("src/lib.rs"), &|path| {
            files
                .get(path)
                .map(|source| source.to_string())
                .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::NotFound))
        })
        .unwrap();
        assert_eq!(
            cases
                .iter()
                .map(|case| case.name.as_str())
                .collect::<Vec<_>>(),
            [
                "a::child::child",
                "a::override_mod::renamed",
                "a::works",
                "nested::chosen::chosen",
                "root"
            ]
        );
    }
}
