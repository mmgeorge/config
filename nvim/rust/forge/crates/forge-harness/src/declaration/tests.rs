use super::*;

fn design(files: &[(&str, &str)]) -> DeclarationDesign {
    DeclarationDesign {
        proposed: files
            .iter()
            .map(|(path, text)| (path.to_string(), text.to_string()))
            .collect(),
        ..Default::default()
    }
}

fn local(root: &Path, files: &[(&str, &str)]) -> (DeclarationDesign, DeclarationResolver) {
    let design = design(files);
    let resolver = DeclarationResolver::local(root, &design, false).unwrap();
    (design, resolver)
}

#[test]
fn rust_aliases_generics_and_source_positions_agree() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = local(
        root.path(),
        &[
            (
                "lib.rs",
                "pub mod model;\npub use crate::model::Item as PublicItem;\nuse crate::model::Item as Entry;\npub struct Store<T> { pub entry: Entry, pub generic: T }\n",
            ),
            ("model.rs", "pub struct Item;\n"),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report.diagnostic.iter().all(|diagnostic| !diagnostic.error),
        "{report:?}"
    );
    let result = resolver.at("lib.rs", 4, 34);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("model.rs") && destination.line == 1),
        "{result:?}"
    );
}

#[test]
fn rust_local_and_explicit_types_shadow_unavailable_glob_imports() {
    let root = tempfile::tempdir().unwrap();
    let (_, mut resolver) = local(root.path(), &[
        ("Cargo.toml", "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n[dependencies]\nbevy = \"=0.19.1\"\n"),
        ("src/lib.rs", "mod controls;\nmod arena;\n"),
        ("src/controls.rs", "use bevy::prelude::*;\npub(crate) struct MovementInput { pub(crate) direction: Vec2 }\npub(crate) fn movement_input(mut movement: ResMut<MovementInput>);\n"),
        ("src/arena.rs", "use bevy::prelude::*;\nuse crate::controls::MovementInput;\npub(crate) fn move_player(movement: Res<MovementInput>);\n"),
    ]);
    for (path, column) in [("src/controls.rs", 50), ("src/arena.rs", 40)] {
        let result = resolver.at(path, 3, column);
        assert!(matches!(result, DeclarationResolution::Resolved { ref destination }
            if destination.path.ends_with("controls.rs") && destination.line == 2), "{path}: {result:?}");
    }
}

#[test]
fn invalid_import_blocks_but_missing_prelude_does_not() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = local(
        root.path(),
        &[
            (
                "lib.rs",
                "mod model;\nuse crate::model::Absent;\npub struct Store { pub item: Option<u32> }\n",
            ),
            ("model.rs", "pub struct Item;\n"),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report.ensure_valid().is_err(),
        "{report:?}; files={:?}",
        resolver
            .file
            .iter()
            .map(|(path, file)| (
                path,
                file.package.clone(),
                file.module.clone(),
                file.index.incomplete
            ))
            .collect::<Vec<_>>()
    );
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.error && diagnostic.reference.contains("Absent"))
    );
    assert_eq!(
        report
            .diagnostic
            .iter()
            .filter(|diagnostic| !diagnostic.error && diagnostic.reason.contains("rust-src"))
            .count(),
        1
    );
}

#[test]
fn rust_facade_navigation_follows_relative_reexports_and_retains_validation_uncertainty() {
    let fixture = tempfile::tempdir().unwrap();
    let workspace = fixture.path().join("app");
    std::fs::create_dir_all(&workspace).unwrap();
    let (_, mut resolver) = local(
        &workspace,
        &[(
            "lib.rs",
            "use facade::prelude::*;\npub struct State;\npub fn update(shared: Res<State>, unique: ResMut<State>);\n",
        )],
    );
    resolver
        .package
        .get_mut("workspace")
        .unwrap()
        .dependency
        .insert("facade".into(), "facade".into());
    for (package, dependency) in [
        ("facade", Some(("internal", "internal"))),
        ("internal", Some(("ecs", "ecs"))),
        ("ecs", None),
    ] {
        resolver.package.insert(
            package.into(),
            RustPackage {
                dependency: dependency
                    .into_iter()
                    .map(|(alias, target)| (alias.into(), target.into()))
                    .collect(),
                ..Default::default()
            },
        );
    }
    for (path, contents) in [
        ("facade/lib.rs", "pub use internal::*;\n"),
        (
            "internal/lib.rs",
            "pub mod prelude;\npub use ecs as engine;\npub mod looping { pub use crate::looping::*; }\n",
        ),
        (
            "internal/prelude.rs",
            "pub use crate::engine::prelude::*;\npub use crate::looping::*;\n#[cfg(feature=\"optional\")]\npub use optional::*;\n",
        ),
        (
            "ecs/lib.rs",
            "mod system;\npub mod prelude { pub use crate::system::{Res, ResMut}; }\n",
        ),
        (
            "ecs/system/mod.rs",
            "mod parameter;\npub use parameter::*;\n",
        ),
        (
            "ecs/system/parameter.rs",
            "pub struct Res<T> { pub value: T }\npub struct ResMut<T> { pub value: T }\n",
        ),
    ] {
        let destination = fixture.path().join(path);
        std::fs::create_dir_all(destination.parent().unwrap()).unwrap();
        std::fs::write(destination, contents).unwrap();
    }
    for package in ["facade", "internal", "ecs"] {
        resolver
            .load_rust(
                &fixture.path().join(package).join("lib.rs"),
                package,
                &[],
                0,
            )
            .unwrap();
    }
    for (name, column, line) in [("Res", 22, 1), ("ResMut", 42, 2)] {
        let result = resolver.at("lib.rs", 3, column);
        assert!(
            matches!(result, DeclarationResolution::Resolved { ref destination }
            if destination.name == name && destination.path.ends_with("parameter.rs") && destination.line == line && !destination.proposed),
            "{result:?}"
        );
    }
    let strict = resolver.resolve_rust_path(
        "workspace",
        &[],
        &["facade".into(), "prelude".into(), "Res".into()],
        false,
        RustAccess {
            package: "workspace",
            scope: &[],
            navigation: false,
        },
        &mut HashSet::new(),
    );
    assert!(
        matches!(strict, DeclarationResolution::Unverified { .. }),
        "{strict:?}"
    );
}

#[test]
fn rust_navigation_does_not_choose_between_distinct_glob_destinations() {
    let root = tempfile::tempdir().unwrap();
    let (_, mut resolver) = local(
        root.path(),
        &[(
            "lib.rs",
            "pub mod left { pub struct Item; }\npub mod right { pub struct Item; }\nuse crate::left::*;\nuse crate::right::*;\npub fn inspect(value: Item);\n",
        )],
    );
    let result = resolver.at("lib.rs", 5, 22);
    assert!(
        matches!(result, DeclarationResolution::Ambiguous { .. }),
        "{result:?}"
    );
}

#[test]
fn prelude_exports_resolve_to_defining_enum() {
    let root = tempfile::tempdir().unwrap();
    let (_, mut resolver) = local(
        root.path(),
        &[("lib.rs", "pub struct Store<T> { pub item: Option<T> }\n")],
    );
    let core = root.path().join("external/core");
    std::fs::create_dir_all(core.join("prelude")).unwrap();
    std::fs::write(core.join("lib.rs"), "pub mod prelude; pub mod option;\n").unwrap();
    std::fs::write(core.join("prelude.rs"), "pub mod rust_2024;\n").unwrap();
    std::fs::write(
        core.join("prelude/rust_2024.rs"),
        "pub use crate::option::Option;\n",
    )
    .unwrap();
    std::fs::write(
        core.join("option.rs"),
        "pub enum Option<T> { None, Some(T) }\n",
    )
    .unwrap();
    resolver
        .package
        .insert("std".into(), RustPackage::default());
    resolver
        .load_rust(&core.join("lib.rs"), "std", &[], 0)
        .unwrap();
    resolver.rust_library = true;
    let result = resolver.at("lib.rs", 1, 32);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("option.rs")),
        "{result:?}; files={:?}",
        resolver
            .file
            .iter()
            .map(|(path, file)| (
                path,
                file.package.clone(),
                file.module.clone(),
                file.index.module.clone()
            ))
            .collect::<Vec<_>>()
    );
}

fn typescript(root: &Path, files: &[(&str, &str)]) -> (DeclarationDesign, DeclarationResolver) {
    let mut files = files.to_vec();
    files.push(("tsconfig.json", "{\"compilerOptions\":{\"noLib\":true}}"));
    local(root, &files)
}

#[test]
fn typescript_barrel_and_namespace_exports_resolve() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = typescript(
        root.path(),
        &[
            ("model.ts", "export interface User { id: string; }\n"),
            ("barrel.ts", "export { User as Person } from './model';\n"),
            (
                "api.ts",
                "import type { Person } from './barrel';\nimport * as Model from './model';\nexport interface Api { person: Person; user: Model.User; }\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(report.diagnostic.is_empty(), "{report:?}");
    let result = resolver.at("api.ts", 3, 33);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("model.ts")),
        "{result:?}"
    );
}

#[test]
fn globals_generics_and_shadowing_need_no_import() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = typescript(
        root.path(),
        &[
            (
                "globals.d.ts",
                "interface Promise<T> {}\ninterface ProjectGlobal {}\n",
            ),
            (
                "api.ts",
                "export interface Api<T> { load(): Promise<T>; context: ProjectGlobal; }\n",
            ),
            (
                "shadow.ts",
                "interface Promise<T> {}\nexport interface Api { load(): Promise<string>; }\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(report.diagnostic.is_empty(), "{report:?}");
    let result = resolver.at("shadow.ts", 2, 31);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("shadow.ts")),
        "{result:?}"
    );
}

#[test]
fn modules_do_not_leak_globals_and_export_cycles_terminate() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = typescript(
        root.path(),
        &[
            ("model.ts", "export interface HiddenGlobal {}\n"),
            ("api.ts", "export interface Api { value: HiddenGlobal; }\n"),
            ("a.ts", "export * from './b';\n"),
            ("b.ts", "export * from './a';\n"),
            (
                "cycle.ts",
                "import type { Missing } from './a';\nexport interface Api { value: Missing; }\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "HiddenGlobal" && diagnostic.error),
        "{report:?}"
    );
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reason.contains("cyclic") && !diagnostic.error),
        "{report:?}"
    );
}

#[test]
fn deleted_files_are_not_resurrected_from_disk() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join("deleted.ts"), "export interface Item {}\n").unwrap();
    let mut design = design(&[
        ("tsconfig.json", "{\"compilerOptions\":{\"noLib\":true}}"),
        ("api.ts", "import { Item } from './deleted';\n"),
    ]);
    design.baseline.insert(
        "deleted.ts".into(),
        crate::plan::DeclarationFile {
            text: "export interface Item {}\n".into(),
            source_digest: String::new(),
        },
    );
    let mut resolver = DeclarationResolver::local(root.path(), &design, false).unwrap();
    assert!(resolver.validate(&design).ensure_valid().is_err());
}

#[test]
fn configured_standard_library_is_indexed() {
    let root = tempfile::tempdir().unwrap();
    let library = root.path().join("node_modules/typescript/lib");
    std::fs::create_dir_all(&library).unwrap();
    std::fs::write(
        library.parent().unwrap().join("package.json"),
        "{\"version\":\"6.0.0\"}",
    )
    .unwrap();
    std::fs::write(
        library.join("lib.es5.d.ts"),
        "/// <reference lib=\"extra\" />\ninterface Promise<T> {}\n",
    )
    .unwrap();
    std::fs::write(library.join("lib.extra.d.ts"), "interface Extra {}\n").unwrap();
    let design = design(&[
        ("tsconfig.json", "{\"compilerOptions\":{\"lib\":[\"es5\"]}}"),
        (
            "api.ts",
            "export interface Api<T> { load(): Promise<T>; extra: Extra; }\n",
        ),
    ]);
    let mut resolver = DeclarationResolver::local(root.path(), &design, false).unwrap();
    assert!(resolver.validate(&design).diagnostic.is_empty());
    let result = resolver.at("api.ts", 1, 34);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("lib.es5.d.ts") && !destination.proposed),
        "{result:?}"
    );
}

#[test]
fn rust_visibility_reexports_and_generic_associated_types() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = local(
        root.path(),
        &[
            (
                "lib.rs",
                "mod model;\npub use crate::model::Item;\nuse crate::model::Hidden;\npub struct Api<T> { pub item: Item, pub output: T::Output }\n",
            ),
            ("model.rs", "pub struct Item;\nstruct Hidden;\n"),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference.contains("Hidden") && diagnostic.error),
        "{report:?}"
    );
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "T::Output" && !diagnostic.error),
        "{report:?}"
    );
    assert!(
        !report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "Item")
    );
}

#[test]
fn typescript_defaults_function_imports_and_value_namespace() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = typescript(
        root.path(),
        &[
            (
                "model.ts",
                "export default class User {}\nexport function load(): string;\n",
            ),
            (
                "api.ts",
                "import User, { load } from './model';\nexport interface Api { user: User; loader: typeof load; }\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(report.diagnostic.is_empty(), "{report:?}");
}

#[test]
fn type_aliases_defaults_and_heritage_are_checked() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = typescript(
        root.path(),
        &[(
            "api.ts",
            "export type Result = Missing;\nexport interface Api<T = Absent> {}\nexport class Derived extends MissingBase {}\n",
        )],
    );
    let report = resolver.validate(&design);
    for name in ["Missing", "Absent", "MissingBase"] {
        assert!(
            report
                .diagnostic
                .iter()
                .any(|diagnostic| diagnostic.reference == name && diagnostic.error),
            "{name}: {report:?}"
        );
    }
}

#[test]
fn path_aliases_and_selected_package_exports() {
    let root = tempfile::tempdir().unwrap();
    let package = root.path().join("node_modules/models");
    std::fs::create_dir_all(&package).unwrap();
    std::fs::write(
        package.join("package.json"),
        "{\"exports\":{\".\":{\"types\":\"./index.d.ts\",\"default\":\"./index.js\"}}}",
    )
    .unwrap();
    std::fs::write(package.join("index.d.ts"), "export interface External {}\n").unwrap();
    let design = design(&[
        (
            "tsconfig.json",
            "{\"compilerOptions\":{\"noLib\":true,\"moduleResolution\":\"bundler\",\"paths\":{\"@unused/*\":[\"unused/*\"],\"@model/*\":[\"model/*\"]}}}",
        ),
        ("model/user.ts", "export interface User {}\n"),
        (
            "api.ts",
            "import { User } from '@model/user';\nimport { External } from 'models';\nexport interface Api { user: User; other: External; }\n",
        ),
    ]);
    let mut resolver = DeclarationResolver::local(root.path(), &design, false).unwrap();
    assert!(resolver.validate(&design).diagnostic.is_empty());
}

#[tokio::test]
async fn cargo_acquires_reusable_graph_without_touching_workspace() {
    let fixture = tempfile::tempdir().unwrap();
    let workspace = fixture.path().join("app");
    let dependency = fixture.path().join("engine");
    let unused = fixture.path().join("unused");
    std::fs::create_dir_all(workspace.join("src")).unwrap();
    std::fs::create_dir_all(dependency.join("src")).unwrap();
    std::fs::create_dir_all(unused.join("src")).unwrap();
    std::fs::write(unused.join("Cargo.toml"),
        "[package]\nname='unused'\nversion='0.1.0'\nedition='2024'\n").unwrap();
    std::fs::write(unused.join("src/lib.rs"), "pub struct Unrelated;\n").unwrap();
    let manifest = "[package]\nname='app'\nversion='0.1.0'\nedition='2024'\n[dependencies]\nengine={package='engine-core',path='../engine'}\nunused={path='../unused'}\n";
    std::fs::write(workspace.join("Cargo.toml"), manifest).unwrap();
    std::fs::write(
        dependency.join("Cargo.toml"),
        "[package]\nname='engine-core'\nversion='0.1.0'\nedition='2024'\n",
    )
    .unwrap();
    std::fs::write(
        dependency.join("src/lib.rs"),
        "mod model;\npub use crate::model::Point as Position;\n",
    )
    .unwrap();
    std::fs::write(dependency.join("src/model.rs"), "pub struct Point;\n").unwrap();
    let design = design(&[
        ("Cargo.toml", manifest),
        (
            "src/lib.rs",
            "use engine::Position;\npub struct Api { pub position: Position }\n",
        ),
    ]);
    let mut resolver = DeclarationResolver::prepare(&workspace, &design, false, None)
        .await
        .unwrap();
    for root in [&dependency, &unused] {
        assert!(!resolver.file.contains_key(&normalize(&root.join("src/lib.rs"))),
            "preparation eagerly indexed external source: {}", root.display());
    }
    let result = resolver.at("src/lib.rs", 2, 32);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("model.rs") && !destination.proposed),
        "{result:?}"
    );
    assert!(resolver.file.contains_key(&normalize(&dependency.join("src/lib.rs"))),
        "navigation did not index the referenced dependency root");
    assert!(resolver.file.contains_key(&normalize(&dependency.join("src/model.rs"))),
        "navigation did not follow the re-export to its defining module");
    assert!(!resolver.file.contains_key(&normalize(&unused.join("src/lib.rs"))),
        "navigation indexed an unrelated dependency");
    let loaded = resolver.file.len();
    assert_eq!(resolver.at("src/lib.rs", 2, 32), result);
    assert_eq!(resolver.file.len(), loaded, "a repeated jump reloaded dependency sources");
    let report = resolver.validate(&design);
    assert!(report.diagnostic.iter().all(|diagnostic| !diagnostic.error), "{report:?}");
    assert!(!resolver.file.contains_key(&normalize(&unused.join("src/lib.rs"))),
        "validation indexed an unrelated dependency");
    assert_eq!(
        std::fs::read_to_string(workspace.join("Cargo.toml")).unwrap(),
        manifest
    );
    assert!(!workspace.join("Cargo.lock").exists());
    // Subsequent checkout edits must not change the captured dependency graph.
    std::fs::write(workspace.join("Cargo.lock"), "invalid checkout lockfile").unwrap();
    let mut reused = DeclarationResolver::prepare(&workspace, &design, false, None)
        .await
        .unwrap();
    assert_eq!(reused.at("src/lib.rs", 2, 32), result);
    assert_eq!(
        std::fs::read_to_string(workspace.join("Cargo.lock")).unwrap(),
        "invalid checkout lockfile"
    );
}

#[test]
fn conditional_module_references_remain_unverified() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = local(
        root.path(),
        &[
            (
                "lib.rs",
                "#[cfg(feature = \"optional\")]\npub mod optional;\n",
            ),
            (
                "optional.rs",
                "pub struct Optional { pub value: GeneratedType }\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report.diagnostic.iter().all(|diagnostic| !diagnostic.error),
        "{report:?}"
    );
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reason.contains("conditionally compiled")),
        "{report:?}"
    );
}

#[tokio::test]
#[ignore = "acquires Bevy sources through Cargo"]
async fn bevy_prelude_navigation_reaches_resource_definitions() {
    let workspace = tempfile::tempdir().unwrap();
    std::fs::write(
        workspace.path().join("rust-toolchain.toml"),
        "[toolchain]\nchannel='1.94.0'\n",
    )
    .unwrap();
    let design = design(&[
        (
            "Cargo.toml",
            "[package]\nname='bevy-navigation'\nversion='0.1.0'\nedition='2024'\n[dependencies]\nbevy={version='=0.19.1',default-features=false}\n",
        ),
        (
            "src/lib.rs",
            "use bevy::prelude::*;\npub struct MovementInput;\npub fn update(shared: Res<MovementInput>, unique: ResMut<MovementInput>);\n",
        ),
    ]);
    let mut resolver = DeclarationResolver::prepare(workspace.path(), &design, false, None)
        .await
        .unwrap();
    for (name, column) in [("Res", 22), ("ResMut", 50)] {
        let result = resolver.at("src/lib.rs", 3, column);
        assert!(
            matches!(result, DeclarationResolution::Resolved { ref destination }
            if destination.name == name && destination.path.replace('\\', "/").contains("bevy_ecs-0.19.1/src/change_detection/params.rs") && !destination.proposed),
            "{name}: {result:?}; warnings={:?}",
            resolver.warning
        );
    }
    assert!(!workspace.path().join("Cargo.toml").exists());
    assert!(!workspace.path().join("Cargo.lock").exists());
}

#[test]
fn cached_navigation_prefers_latest_compatible_patch_and_follows_manifest_aliases() {
    let fixture = tempfile::tempdir().unwrap();
    let workspace = fixture.path().join("app");
    let cargo = fixture.path().join("cargo");
    let registry = cargo.join("registry/src/test-registry");
    std::fs::create_dir_all(&workspace).unwrap();
    for (name, version, manifest, source) in [
        ("engine-core", "0.19.1", "", "pub struct Outdated;\n"),
        ("engine-core", "0.19.8", "[dependencies]\nmodel={package='model-types',version='0.19'}\n", "pub use model::Position;\n"),
        ("engine-core", "0.20.0", "", "pub struct Incompatible;\n"),
        ("model-types", "0.19.4", "", "pub struct Position;\n"),
        ("unused", "0.1.0", "", "pub struct Unrelated;\n"),
    ] {
        let directory = registry.join(format!("{name}-{version}"));
        std::fs::create_dir_all(directory.join("src")).unwrap();
        std::fs::write(directory.join("Cargo.toml"), format!("[package]\nname='{name}'\nversion='{version}'\nedition='2024'\n{manifest}")).unwrap();
        std::fs::write(directory.join("src/lib.rs"), source).unwrap();
    }
    let manifest = "[package]\nname='app'\nversion='0.1.0'\nedition='2024'\n[dependencies]\nengine={package='engine-core',version='0.19'}\nunused='9.9'\n";
    let design = design(&[("Cargo.toml", manifest), ("src/lib.rs", "use engine::Position;\npub fn update(position: Position);\n")]);
    std::fs::write(workspace.join("Cargo.lock"), "invalid lockfile must not block cached navigation").unwrap();
    let mut resolver = DeclarationResolver::local(&workspace, &design, false).unwrap();
    resolver.source_cache = Some(cache::RustSourceCache::new(cargo.clone()));
    let result = resolver.at("src/lib.rs", 2, 24);
    assert!(matches!(result, DeclarationResolution::Resolved { ref destination }
        if destination.name == "Position" && destination.path.contains("model-types-0.19.4")), "{result:?}");
    assert!(resolver.file.keys().any(|path| path.to_string_lossy().contains("engine-core-0.19.8")));
    assert!(!resolver.file.keys().any(|path| path.to_string_lossy().contains("engine-core-0.19.1")
        || path.to_string_lossy().contains("engine-core-0.20.0") || path.to_string_lossy().contains("unused-")));
    assert!(!resolver.requires_source_fetch(), "unreached dependencies requested Cargo acquisition");
    assert!(!cargo.join("forge-declarations").exists(), "cached navigation prepared a Cargo metadata mirror");
    let loaded = resolver.file.len();
    assert_eq!(resolver.at("src/lib.rs", 2, 24), result);
    assert_eq!(resolver.file.len(), loaded);
    assert_eq!(std::fs::read_to_string(workspace.join("Cargo.lock")).unwrap(), "invalid lockfile must not block cached navigation");
}

#[test]
fn cached_navigation_respects_exact_versions_workspace_paths_and_missing_sources() {
    let fixture = tempfile::tempdir().unwrap();
    let workspace = fixture.path().join("app");
    let model = fixture.path().join("model");
    std::fs::create_dir_all(workspace.join("src")).unwrap();
    std::fs::create_dir_all(model.join("src")).unwrap();
    std::fs::write(model.join("Cargo.toml"), "[package]\nname='model-types'\nversion='0.1.0'\nedition='2024'\n").unwrap();
    std::fs::write(model.join("src/lib.rs"), "pub struct Position;\n").unwrap();
    let incompatible = fixture.path().join("cargo/registry/src/test-registry/missing-0.19.8");
    std::fs::create_dir_all(incompatible.join("src")).unwrap();
    std::fs::write(incompatible.join("Cargo.toml"), "[package]\nname='missing'\nversion='0.19.8'\nedition='2024'\n").unwrap();
    std::fs::write(incompatible.join("src/lib.rs"), "pub struct Unknown;\n").unwrap();
    let design = design(&[("Cargo.toml", "[workspace]\n[workspace.dependencies]\nmodel={package='model-types',path='../model'}\n[package]\nname='app'\nversion='0.1.0'\nedition='2024'\n[dependencies]\nmodel={workspace=true}\nmissing='=0.19.1'\n"),
        ("src/lib.rs", "pub fn update(position: model::Position);\npub fn other(value: missing::Unknown);\n")]);
    let mut resolver = DeclarationResolver::local(&workspace, &design, false).unwrap();
    resolver.source_cache = Some(cache::RustSourceCache::new(fixture.path().join("cargo")));
    let result = resolver.at("src/lib.rs", 1, 27);
    assert!(matches!(result, DeclarationResolution::Resolved { ref destination } if destination.name == "Position"), "{result:?}");
    assert!(!resolver.requires_source_fetch());
    let missing = resolver.at("src/lib.rs", 2, 29);
    assert!(matches!(missing, DeclarationResolution::Unverified { .. }), "{missing:?}");
    assert!(resolver.requires_source_fetch());
}

#[test]
fn cached_navigation_binary_can_use_its_package_library() {
    let root = tempfile::tempdir().unwrap();
    let design = design(&[("Cargo.toml", "[package]\nname='arena'\nversion='0.1.0'\nedition='2024'\n"),
        ("src/lib.rs", "pub struct Game;\n"),
        ("src/main.rs", "use arena::Game;\nfn main(game: Game);\n")]);
    let mut resolver = DeclarationResolver::local(root.path(), &design, false).unwrap();
    resolver.source_cache = Some(cache::RustSourceCache::new(root.path().join("cargo")));
    let result = resolver.at("src/main.rs", 2, 15);
    assert!(matches!(result, DeclarationResolution::Resolved { ref destination }
        if destination.path.ends_with("lib.rs") && destination.name == "Game"), "{result:?}");
    assert!(!resolver.requires_source_fetch());
}
