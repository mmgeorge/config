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
fn deep_rust_modules_preserve_ownership_conditions_and_cycle_termination() {
    for conditional in [false, true] {
        let root = tempfile::tempdir().unwrap();
        let mut design = DeclarationDesign::default();
        design.proposed.insert(
            "lib.rs".into(),
            format!(
                "{}#[path = \"layer/0/mod.rs\"]\npub mod nested;\n",
                if conditional {
                    "#[cfg(feature = \"optional\")]\n"
                } else {
                    ""
                },
            ),
        );
        for index in 0..96 {
            let text = if index == 95 {
                "pub struct Final { pub value: u32 }\n#[path = \"../0/mod.rs\"]\npub mod cycle;\n"
                    .to_owned()
            } else {
                format!("#[path = \"../{}/mod.rs\"]\npub mod nested;\n", index + 1)
            };
            design
                .proposed
                .insert(format!("layer/{index}/mod.rs"), text);
        }
        let resolver = DeclarationResolver::planned(root.path(), &design, false).unwrap();
        assert_eq!(resolver.file.len(), 97);
        assert!(!resolver.package["workspace"].incomplete);
        let leaf = &resolver.file[&normalize(&root.path().join("layer/95/mod.rs"))];
        assert_eq!(leaf.module, vec!["nested"; 96]);
        assert_eq!(leaf.package, "workspace");
        assert!(
            leaf.index
                .symbol
                .iter()
                .any(|symbol| symbol.name == "Final")
        );
        assert!(
            leaf.index
                .symbol
                .iter()
                .all(|symbol| symbol.conditional == conditional)
        );
        assert!(
            leaf.index
                .reference
                .iter()
                .all(|reference| reference.conditional == conditional)
        );
    }
}

#[test]
fn constructs_validate_types_and_reject_known_non_types() {
    let root = tempfile::tempdir().unwrap();
    let text = "pub struct Weapon { pub cooldown: f32 }\npub const LIMIT: u32;\npub fn factory();\npub fn setup();\nConstructs\n  Weapon\n  factory\n  LIMIT\n  ExternalWeapon\n";
    let (declaration, calls) = crate::plan::calls::parse("lib.rs", text, &[]).unwrap();
    let mut design = design(&[("lib.rs", &declaration)]);
    design.proposed_calls.insert("lib.rs".into(), calls);
    let mut resolver = DeclarationResolver::planned(root.path(), &design, false).unwrap();
    let report = resolver.validate(&design);
    assert!(
        !report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "Weapon")
    );
    for name in ["factory", "LIMIT"] {
        assert!(
            report
                .diagnostic
                .iter()
                .any(|diagnostic| diagnostic.reference == name
                    && diagnostic.error
                    && diagnostic.reason.contains("Constructs requires a type")),
            "{report:?}"
        );
    }
    assert!(
        report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "ExternalWeapon" && !diagnostic.error)
    );
}

#[test]
fn captured_rust_constructors_keep_factories_and_type_mentions_separate() {
    let root = tempfile::tempdir().unwrap();
    let source = "pub struct Unit;\npub struct Tuple(u32);\npub struct Record { pub count: u32 }\nimpl Record { pub fn new() -> Self { Self { count: 0 } } pub fn ready() -> Record { Self::new() } pub fn view(&self) -> &Record { self } pub fn count(&self) -> u32 { self.count } }\npub fn make() -> Record { Record::new() }\npub fn run(input: Option<&Record>) { let _typed: Option<&Record> = None; inspect::<Record>(); consume(Unit); consume(Tuple(1)); consume(Record { count: 1 }); Record::new(); Record::ready(); make(); register(Record::new); }";
    let declaration = forge_diff::syntax::DeclarationOverview::extract("lib.rs", source).unwrap();
    let (_, mut resolver) = local(root.path(), &[("lib.rs", &declaration)]);
    let mut functions = crate::plan::calls::extract("lib.rs", source).unwrap();
    crate::plan::calls::classify("lib.rs", &mut functions, &mut resolver);
    let run = functions
        .iter()
        .find(|function| function.owner == "run")
        .unwrap();
    let constructors = run
        .call
        .iter()
        .flatten()
        .filter(|reference| reference.kind == crate::plan::CallKind::Construct)
        .map(|reference| reference.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        constructors,
        [
            "Unit",
            "Tuple",
            "Record",
            "Record::new",
            "Record::ready",
            "make"
        ]
    );
    assert!(
        run.call
            .iter()
            .flatten()
            .any(|reference| reference.name == "Record::new"
                && reference.kind == crate::plan::CallKind::Callback)
    );
    for (name, factory) in [
        ("Record::new", true),
        ("Record::ready", true),
        ("make", true),
        ("Record::view", false),
        ("Record::count", false),
    ] {
        let target = resolver.callable("lib.rs", "run", name);
        assert_eq!(resolver.factory(&target), factory, "{name}: {target:?}");
    }
    let (declaration, mut authored) = crate::plan::calls::parse(
        "lib.rs",
        &format!("{declaration}Constructs\n  Record::new\nCalls\n  Record::ready\n"),
        &[],
    )
    .unwrap();
    crate::plan::calls::classify("lib.rs", &mut authored, &mut resolver);
    let combined = crate::plan::calls::combined("lib.rs", &declaration, &authored).unwrap();
    assert!(
        combined.contains("Constructs\n  Record::new\n"),
        "{combined}"
    );
    assert!(
        combined.contains("Calls\n  Record::ready\n"),
        "authored callback lost its category: {combined}"
    );
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
            .filter(|diagnostic| !diagnostic.error
                && diagnostic.reason.contains("unchecked during planning"))
            .count(),
        1
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
    let mut resolver = DeclarationResolver::planned(root.path(), &design, false).unwrap();
    assert!(resolver.validate(&design).ensure_valid().is_err());
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
fn unbound_aliases_defaults_and_heritage_remain_unverified() {
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
                .any(|diagnostic| diagnostic.reference == name && !diagnostic.error),
            "{name}: {report:?}"
        );
    }
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
fn qualified_module_paths_do_not_expand_same_named_function_reexports() {
    let root = tempfile::tempdir().unwrap();
    let (design, mut resolver) = local(
        root.path(),
        &[
            (
                "lib.rs",
                "mod vector;\npub use vector::{vector, Vector};\npub struct Api { pub position: Vector }\n",
            ),
            (
                "vector.rs",
                "pub fn vector() -> Vector;\n#[cfg(feature = \"optional\")]\npub struct Vector;\n",
            ),
        ],
    );
    let report = resolver.validate(&design);
    assert!(
        report.diagnostic.iter().all(|diagnostic| !diagnostic.error),
        "{report:?}"
    );
    assert!(matches!(
        resolver.at("lib.rs", 3, 34),
        DeclarationResolution::Unverified { .. }
    ));
    assert!(
        !report
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.reference == "vector::vector"),
        "same-named value re-export did not validate its function: {report:?}"
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
