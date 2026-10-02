//! Cargo and toolchain acquisition leave project files untouched.

use super::*;
use std::time::Duration;
use tokio::process::Command;

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
            RustPackage {
                edition: "2024".into(),
                ..Default::default()
            },
        );
        let paths = resolver
            .snapshot
            .keys()
            .filter(|path| path.ends_with(".rs"))
            .cloned()
            .collect::<Vec<_>>();
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

pub(super) async fn acquire(resolver: &mut DeclarationResolver) -> Result<bool> {
    if !resolver.snapshot.keys().any(|path| path.ends_with(".rs")) {
        return Ok(false);
    }
    let manifests = resolver
        .snapshot
        .iter()
        .filter(|(path, _)| path.ends_with("Cargo.toml"))
        .map(|(path, text)| (path.clone(), text.clone()))
        .collect::<Vec<_>>();
    if manifests.is_empty() {
        return Ok(false);
    } else {
        match cargo_graph(resolver, &manifests).await {
            Ok((graph, mirror)) => {
                let stage = resolver.trace.as_ref().map(|trace| trace.stage("index_dependencies", Some(resolver.baseline)));
                install_graph(resolver, graph, &mirror)?;
                if let Some(stage) = stage { stage.complete(resolver.statistics()); }
            },
            Err(error) => {
                resolver.warning.push(format!("Cargo dependency resolution is unavailable: {error:#}. External references remain unverified."));
                return Ok(false);
            }
        }
    }
    let stage = resolver.trace.as_ref().map(|trace| trace.stage("index_workspace", Some(resolver.baseline)));
    loose_project_files(resolver)?;
    if let Some(stage) = stage { stage.complete(resolver.statistics()); }
    let stage = resolver.trace.as_ref().map(|trace| trace.stage("standard_library", Some(resolver.baseline)));
    standard_library(resolver).await?;
    if let Some(stage) = stage { stage.complete(resolver.statistics()); }
    Ok(true)
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

async fn cargo_graph(
    resolver: &DeclarationResolver,
    manifests: &[(String, String)],
) -> Result<(crate::rustdoc::SourceGraph, PathBuf)> {
    let stage = resolver.trace.as_ref().map(|trace| trace.stage("cargo_graph", Some(resolver.baseline)));
    let identity = crate::plan::digest(
        serde_json::to_vec(&(
            resolver.workspace.to_string_lossy(),
            manifests,
            &resolver.cargo_lock,
        ))?
        .as_slice(),
    );
    let mirror = home::cargo_home()?
        .join("forge-declarations")
        .join(identity);
    tokio::fs::create_dir_all(&mirror).await?;
    let cached_graph = mirror.join("graph.json");
    if let Ok(bytes) = tokio::fs::read(&cached_graph).await {
        if let Ok(graph) = serde_json::from_slice::<crate::rustdoc::SourceGraph>(&bytes) {
            let missing_module = resolver.source_request.keys().any(|request| {
                let path = Path::new(request);
                path.is_absolute() && !path.is_file()
            });
            let roots_available = graph.packages.iter().flat_map(|package| &package.targets)
                .filter(|target| target.kind.iter().any(|kind| matches!(kind.as_str(), "lib" | "rlib" | "proc-macro")))
                .all(|target| target.src_path.is_file());
            if !missing_module && roots_available {
                if let Some(stage) = stage { stage.complete(serde_json::json!({"cached":true})); }
                return Ok((graph, mirror));
            }
        }
    }
    for (path, text) in manifests {
        let mut manifest: toml::Value = toml::from_str(text)?;
        // Preserve dependency semantics while relocating paths outside this workspace.
        relocate_paths(
            &mut manifest,
            &resolver
                .workspace
                .join(path)
                .parent()
                .unwrap()
                .to_path_buf(),
            &resolver.workspace,
            &mirror,
        );
        let destination = mirror.join(path);
        tokio::fs::create_dir_all(destination.parent().unwrap()).await?;
        tokio::fs::write(destination, toml::to_string(&manifest)?).await?;
    }
    for path in resolver
        .snapshot
        .keys()
        .filter(|path| path.ends_with(".rs"))
    {
        let destination = mirror.join(path);
        tokio::fs::create_dir_all(destination.parent().unwrap()).await?;
        tokio::fs::write(destination, "").await?;
    }
    for (path, text) in &resolver.cargo_lock {
        let destination = mirror.join(path);
        tokio::fs::create_dir_all(destination.parent().unwrap()).await?;
        tokio::fs::write(destination, text).await?;
    }
    let root = if mirror.join("Cargo.toml").is_file() {
        mirror.join("Cargo.toml")
    } else {
        mirror.join(&manifests[0].0)
    };
    // Future targets may not exist in the checkout yet. Metadata only needs stubs.
    for (path, text) in manifests {
        let manifest: toml::Value = toml::from_str(text)?;
        let directory = mirror.join(path).parent().unwrap().to_path_buf();
        let mut targets = Vec::new();
        if let Some(target) = manifest
            .get("lib")
            .and_then(|target| target.get("path"))
            .and_then(toml::Value::as_str)
        {
            targets.push(target.to_owned());
        }
        for kind in ["bin", "example", "test", "bench"] {
            for target in manifest
                .get(kind)
                .and_then(toml::Value::as_array)
                .into_iter()
                .flatten()
            {
                if let Some(path) = target.get("path").and_then(toml::Value::as_str) {
                    targets.push(path.to_owned());
                }
            }
        }
        if manifest.get("package").is_some()
            && !directory.join("src/lib.rs").exists()
            && !directory.join("src/main.rs").exists()
            && targets.is_empty()
        {
            targets.push("src/lib.rs".into());
        }
        for target in targets {
            let destination = directory.join(target);
            if !destination.exists() {
                tokio::fs::create_dir_all(destination.parent().unwrap()).await?;
                tokio::fs::write(destination, "").await?;
            }
        }
    }
    let mut command = Command::new("cargo");
    command
        .current_dir(&resolver.workspace)
        .args(["metadata", "--format-version", "1", "--manifest-path"])
        .arg(root)
        .args(["--color", "never"])
        .kill_on_drop(true);
    // Cargo owns its global registry/git cache. Only this isolated lockfile changes.
    let metadata_stage = resolver.trace.as_ref().map(|trace| trace.stage("cargo_metadata", Some(resolver.baseline)));
    let output = tokio::time::timeout(Duration::from_secs(120), command.output())
        .await
        .context("Cargo declaration metadata exceeded 120 seconds")??;
    anyhow::ensure!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr).trim()
    );
    if let Some(stage) = metadata_stage { stage.complete(serde_json::json!({"bytes":output.stdout.len()})); }
    tokio::fs::write(cached_graph, &output.stdout).await?;
    let graph = serde_json::from_slice(&output.stdout)?;
    if let Some(stage) = stage { stage.complete(serde_json::json!({"cached":false})); }
    Ok((graph, mirror))
}

fn relocate_paths(value: &mut toml::Value, directory: &Path, workspace: &Path, mirror: &Path) {
    match value {
        toml::Value::Table(table) => {
            if let Some(toml::Value::String(path)) = table.get_mut("path") {
                let absolute = normalize(&directory.join(&*path));
                if let Ok(relative) = absolute.strip_prefix(workspace) {
                    *path = mirror.join(relative).to_string_lossy().into_owned();
                } else {
                    *path = absolute.to_string_lossy().into_owned();
                }
            }
            for (_, child) in table.iter_mut() {
                relocate_paths(child, directory, workspace, mirror);
            }
        }
        toml::Value::Array(values) => {
            for child in values {
                relocate_paths(child, directory, workspace, mirror);
            }
        }
        _ => {}
    }
}

fn install_graph(
    resolver: &mut DeclarationResolver,
    graph: crate::rustdoc::SourceGraph,
    mirror: &Path,
) -> Result<()> {
    let dependencies = graph
        .resolve
        .nodes
        .into_iter()
        .map(|node| {
            (
                node.id,
                node.deps
                    .into_iter()
                    .map(|dependency| (dependency.name, dependency.pkg))
                    .collect::<HashMap<_, _>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    for package in &graph.packages {
        let manifest = normalize(&package.manifest_path);
        let source_manifest = manifest
            .strip_prefix(mirror)
            .map(|relative| resolver.workspace.join(relative))
            .unwrap_or_else(|_| manifest.clone());
        let manifest = resolver
            .read(&source_manifest)
            .and_then(|text| toml::from_str::<toml::Value>(&text).ok())
            .unwrap_or(toml::Value::Table(Default::default()));
        resolver.register_package(
            package.id.clone(),
            source_manifest,
            &manifest,
            dependencies.get(&package.id).cloned().unwrap_or_default(),
        );
    }
    for package in &graph.packages {
        // Register libraries first so a binary can import its package's library.
        for target in package.targets.iter().filter(|target| {
            target
                .kind
                .iter()
                .any(|kind| matches!(kind.as_str(), "lib" | "rlib" | "proc-macro"))
        }) {
            let root = normalize(&target.src_path);
            let root = root
                .strip_prefix(mirror)
                .map(|relative| resolver.workspace.join(relative))
                .unwrap_or(root);
            resolver.register_root(&package.id, root)?;
        }
    }
    for package in &graph.packages {
        for target in package.targets.iter().filter(|target| {
            !target.kind.iter().any(|kind| {
                matches!(
                    kind.as_str(),
                    "lib" | "rlib" | "proc-macro" | "custom-build"
                )
            })
        }) {
            let root = normalize(&target.src_path);
            if let Ok(relative) = root.strip_prefix(mirror) {
                let root = resolver.workspace.join(relative);
                let identity = format!("{}#{}", package.id, target.name);
                let mut state = resolver.package[&package.id].clone();
                state
                    .dependency
                    .insert(package.name.replace('-', "_"), package.id.clone());
                resolver.package.insert(identity.clone(), state);
                resolver.load_rust(&root, &identity, &[], 0)?;
            }
        }
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
        let dependency = cache::dependency_aliases(&manifest).into_iter()
            .map(|alias| (alias.clone(), format!("unavailable:{alias}"))).collect();
        resolver.register_package(identity.clone(), resolver.workspace.join(path), &manifest, dependency);
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

pub(super) async fn standard_library(resolver: &mut DeclarationResolver) -> Result<()> {
    resolver.library_checked = true;
    let mut command = Command::new("rustc");
    command
        .current_dir(&resolver.workspace)
        .args(["--print", "sysroot"])
        .kill_on_drop(true);
    let stage = resolver.trace.as_ref().map(|trace| trace.stage("rustc_sysroot", Some(resolver.baseline)));
    let result = tokio::time::timeout(Duration::from_secs(10), command.output()).await;
    let sysroot = match result {
        Ok(Ok(output)) if output.status.success() => {
            PathBuf::from(String::from_utf8_lossy(&output.stdout).trim())
        }
        _ => PathBuf::new(),
    };
    if let Some(stage) = stage { stage.complete(serde_json::json!({"available":!sysroot.as_os_str().is_empty()})); }
    let library = sysroot.join("lib/rustlib/src/rust/library");
    if !library.join("core/src/lib.rs").is_file() {
        resolver.warning.push("rust-src is unavailable for this workspace toolchain. Run `rustup component add rust-src` in the workspace. Standard-library and prelude checks are disabled.".into());
        return Ok(());
    }
    for name in ["core", "alloc", "std"] {
        resolver.package.insert(
            name.into(),
            RustPackage {
                dependency: ["core", "alloc", "std"]
                    .into_iter()
                    .map(|dependency| (dependency.into(), dependency.into()))
                    .collect(),
                ..Default::default()
            },
        );
        resolver.pending_module.insert(
            (name.into(), Vec::new()),
            (library.join(name).join("src/lib.rs"), false),
        );
    }
    resolver.rust_library = true;
    Ok(())
}
