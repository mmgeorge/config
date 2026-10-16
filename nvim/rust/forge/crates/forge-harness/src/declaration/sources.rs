//! Project module ownership comes only from captured plan declarations.

use super::*;

pub(super) fn project(resolver: &mut DeclarationResolver) -> Result<()> {
    if !resolver.snapshot.keys().any(|path| path.ends_with(".rs")) {
        return Ok(());
    }
    let manifests = resolver
        .snapshot
        .iter()
        .filter(|(path, _)| path.ends_with("Cargo.toml"))
        .map(|(path, text)| (path.clone(), text.clone()))
        .collect::<Vec<_>>();
    if manifests.is_empty() {
        resolver.package.insert(
            "workspace".into(),
            RustPackage::default(),
        );
        let mut paths = resolver
            .snapshot
            .keys()
            .filter(|path| path.ends_with(".rs"))
            .cloned()
            .collect::<Vec<_>>();
        paths.sort_by_key(|path| {
            let entry = matches!(Path::new(path).file_stem().and_then(|stem| stem.to_str()), Some("lib" | "main"));
            (!entry, path.clone())
        });
        for path in paths {
            let stem = Path::new(&path)
                .file_stem()
                .and_then(|stem| stem.to_str())
                .unwrap_or("");
            let module = if matches!(stem, "lib" | "main") {
                Vec::new()
            } else {
                vec![stem.into()]
            };
            resolver.load_rust(&resolver.workspace.join(path), "workspace", &module, 0)?;
        }
    } else {
        install_project_fallback(resolver, &manifests)?;
    }
    loose_project_files(resolver)?;
    Ok(())
}

fn loose_project_files(resolver: &mut DeclarationResolver) -> Result<()> {
    let paths = resolver
        .snapshot
        .keys()
        .filter(|path| {
            path.ends_with(".rs")
                && !resolver
                    .file
                    .contains_key(&normalize(&resolver.workspace.join(path)))
        })
        .cloned()
        .collect::<Vec<_>>();
    for path in paths {
        let identity = format!("unregistered:{path}");
        let absolute = resolver.workspace.join(&path);
        let owner = resolver.package.iter().filter(|(_, package)| {
            package.source_manifest.as_ref().and_then(|path| path.parent())
                .is_some_and(|directory| absolute.starts_with(directory))
        }).max_by_key(|(_, package)| package.source_manifest.as_ref().unwrap().components().count());
        let mut state = owner.map(|(_, package)| package.clone()).unwrap_or_default();
        if let Some((package, state_owner)) = owner {
            let manifest = state_owner.source_manifest.as_ref().unwrap();
            if resolver.read(manifest.parent().unwrap().join("src/lib.rs").as_path()).is_some() {
                if let Some(name) = resolver.read(manifest)
                    .and_then(|text| toml::from_str::<toml::Value>(&text).ok())
                    .and_then(|manifest| manifest.get("lib").and_then(|library| library.get("name"))
                        .or_else(|| manifest.get("package").and_then(|package| package.get("name")))
                        .and_then(toml::Value::as_str).map(|name| name.replace('-', "_"))) {
                    state.dependency.insert(name, package.clone());
                }
            }
        }
        state.incomplete = true;
        resolver.package.insert(identity.clone(), state);
        resolver.load_rust(&resolver.workspace.join(path), &identity, &[], 0)?;
    }
    Ok(())
}

fn install_project_fallback(
    resolver: &mut DeclarationResolver,
    manifests: &[(String, String)],
) -> Result<()> {
    for (path, text) in manifests {
        let manifest: toml::Value = toml::from_str(text)?;
        if manifest.get("package").is_none() {
            continue;
        }
        let directory = resolver
            .workspace
            .join(path)
            .parent()
            .unwrap()
            .to_path_buf();
        let identity = path.clone();
        let dependency = dependency_aliases(&manifest).into_iter()
            .map(|alias| (alias.clone(), format!("unavailable:{alias}"))).collect();
    resolver.register_package(identity.clone(), resolver.workspace.join(path), dependency);
        let root = manifest
            .get("lib")
            .and_then(|library| library.get("path"))
            .and_then(toml::Value::as_str)
            .map(|path| directory.join(path))
            .unwrap_or_else(|| {
                if resolver.read(&directory.join("src/lib.rs")).is_some() {
                    directory.join("src/lib.rs")
                } else {
                    directory.join("src/main.rs")
                }
            });
        resolver.register_root(&identity, root)?;
    }
    Ok(())
}

pub(super) fn dependency_aliases(manifest: &toml::Value) -> Vec<String> {
    dependency_tables(manifest)
        .into_iter()
        .flat_map(|table| table.keys().map(|name| name.replace('-', "_")))
        .collect()
}

fn dependency_tables(manifest: &toml::Value) -> Vec<&toml::map::Map<String, toml::Value>> {
    let mut tables = Vec::new();
    for owner in std::iter::once(manifest).chain(
        manifest
            .get("target")
            .and_then(toml::Value::as_table)
            .into_iter()
            .flat_map(|target| target.values()),
    ) {
        for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
            if let Some(table) = owner.get(section).and_then(toml::Value::as_table) {
                tables.push(table);
            }
        }
    }
    tables
}
