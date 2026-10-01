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
    std::fs::create_dir_all(workspace.join("src")).unwrap();
    std::fs::create_dir_all(dependency.join("src")).unwrap();
    let manifest = "[package]\nname='app'\nversion='0.1.0'\nedition='2024'\n[dependencies]\nengine={package='engine-core',path='../engine'}\n";
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
    let mut resolver = DeclarationResolver::prepare(&workspace, &design, false)
        .await
        .unwrap();
    let report = resolver.validate(&design);
    assert!(
        report.diagnostic.iter().all(|diagnostic| !diagnostic.error),
        "{report:?}"
    );
    let result = resolver.at("src/lib.rs", 2, 32);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination } if destination.path.ends_with("model.rs") && !destination.proposed),
        "{result:?}"
    );
    assert_eq!(
        std::fs::read_to_string(workspace.join("Cargo.toml")).unwrap(),
        manifest
    );
    assert!(!workspace.join("Cargo.lock").exists());
    // Subsequent checkout edits must not change the captured dependency graph.
    std::fs::write(workspace.join("Cargo.lock"), "invalid checkout lockfile").unwrap();
    let mut reused = DeclarationResolver::prepare(&workspace, &design, false)
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
