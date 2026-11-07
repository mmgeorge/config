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
    let resolver = DeclarationResolver::planned(root, &design, false).unwrap();
    (design, resolver)
}

fn typescript(root: &Path, files: &[(&str, &str)]) -> (DeclarationDesign, DeclarationResolver) {
    local(root, files)
}

#[test]
fn external_module_navigation_selects_the_module_file() {
    let root = tempfile::tempdir().unwrap();
    for (declaration, destination) in [
        ("pub mod assets;\n", "assets.rs"),
        ("pub mod assets;\n", "assets/mod.rs"),
        (
            "#[path = \"asset_impl.rs\"]\npub mod assets;\n",
            "asset_impl.rs",
        ),
    ] {
        let (_, mut resolver) = local(
            root.path(),
            &[
                ("lib.rs", declaration),
                (destination, "pub struct Asset;\n"),
            ],
        );
        let line = declaration.lines().count() as u32;
        let result = resolver.at("lib.rs", line, 8);
        assert!(
            matches!(result, DeclarationResolution::Resolved { destination: ref target }
            if target.module_file && Path::new(&target.path).ends_with(destination) && target.line == 1),
            "{result:?}"
        );
    }
    let (_, mut resolver) = local(
        root.path(),
        &[("lib.rs", "pub mod assets { pub struct Asset; }\n")],
    );
    assert!(
        matches!(resolver.at("lib.rs", 1, 8), DeclarationResolution::Resolved { destination } if !destination.module_file)
    );
}

#[test]
fn rust_local_and_explicit_types_shadow_unavailable_glob_imports() {
    let root = tempfile::tempdir().unwrap();
    let (_, mut resolver) = local(
        root.path(),
        &[
            (
                "Cargo.toml",
                "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n[dependencies]\nbevy = \"=0.19.1\"\n",
            ),
            ("src/lib.rs", "mod controls;\nmod arena;\n"),
            (
                "src/controls.rs",
                "use bevy::prelude::*;\npub(crate) struct MovementInput { pub(crate) direction: Vec2 }\npub(crate) fn movement_input(mut movement: ResMut<MovementInput>);\n",
            ),
            (
                "src/arena.rs",
                "use bevy::prelude::*;\nuse crate::controls::MovementInput;\npub(crate) fn move_player(movement: Res<MovementInput>);\n",
            ),
        ],
    );
    for (path, column) in [("src/controls.rs", 50), ("src/arena.rs", 40)] {
        let result = resolver.at(path, 3, column);
        assert!(
            matches!(result, DeclarationResolution::Resolved { ref destination }
            if destination.path.ends_with("controls.rs") && destination.line == 2),
            "{path}: {result:?}"
        );
    }
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
    let mut resolver = DeclarationResolver::planned(root.path(), &design, false).unwrap();
    assert!(resolver.validate(&design).ensure_valid().is_err());
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

#[test]
fn derive_navigation_selects_macro_instead_of_same_named_trait() {
    let workspace = tempfile::tempdir().unwrap();
    let design = design(&[
        (
            "Cargo.toml",
            "[package]\nname='app'\nversion='0.1.0'\nedition='2024'\n",
        ),
        (
            "src/lib.rs",
            "pub mod macros;\npub mod traits;\nuse crate::{macros::Component, traits::Component};\n#[derive(Component)]\npub struct Player;\npub fn inspect(value: Component);\n",
        ),
        (
            "src/macros.rs",
            "#[proc_macro_derive(Component)]\npub fn derive_component(input: TokenStream) -> TokenStream {}\n",
        ),
        ("src/traits.rs", "pub trait Component {}\n"),
    ]);
    let mut resolver = DeclarationResolver::planned(workspace.path(), &design, false).unwrap();
    let result = resolver.at("src/lib.rs", 4, 11);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination }
        if destination.name == "Component" && destination.path.ends_with("macros.rs")),
        "{result:?}"
    );
    let result = resolver.at("src/lib.rs", 6, 25);
    assert!(
        matches!(result, DeclarationResolution::Resolved { ref destination }
        if destination.name == "Component" && destination.path.ends_with("traits.rs")),
        "{result:?}"
    );
}
