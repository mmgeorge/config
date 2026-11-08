use forge_diff::syntax::{DeclarationCalls, DeclarationIndex, DeclarationOverview};

#[test]
fn deep_declarations_preserve_scopes_references_and_comments() {
    for (path, source, expected_owner) in [
        (
            "deep.ts",
            format!(
                "{}export class Store {{\n// retained\nrun(input: Writer): Writer {{ return input; }}\n}}\n{}",
                "export namespace Nested {\n".repeat(160),
                "}\n".repeat(160),
            ),
            format!("{}Store.run", "Nested.".repeat(160)),
        ),
        (
            "deep.rs",
            format!(
                "{}pub struct Store;\nimpl Store {{\n// retained\npub fn run(input: Writer) -> Writer {{ input }}\n}}\n{}",
                "pub mod nested {\n".repeat(160),
                "}\n".repeat(160),
            ),
            format!("{}Store::run", "nested::".repeat(160)),
        ),
        (
            "deep.lua",
            format!(
                "{}-- retained\nfunction run(input) return input end\n{}",
                "do\n".repeat(512),
                "end\n".repeat(512)
            ),
            "run".into(),
        ),
    ] {
        let comments = DeclarationOverview::comments(path, &source).unwrap();
        assert!(
            comments
                .iter()
                .any(|comment| comment.text.contains("retained")),
            "{path}"
        );
        let protected = DeclarationCalls::protected_regions(path, &source).unwrap();
        let comment_line = comments
            .iter()
            .find(|comment| comment.text.contains("retained"))
            .unwrap()
            .position
            .line as usize
            - 1;
        assert!(
            protected
                .iter()
                .any(|region| region.contains(&comment_line)),
            "{path}"
        );
        let callable = DeclarationCalls::extract(path, &source, false).unwrap();
        assert!(
            callable
                .iter()
                .any(|callable| callable.owner == expected_owner),
            "{path}"
        );
        let index = DeclarationIndex::extract(path, &source).unwrap();
        assert!(!index.incomplete, "{path}");
        assert!(
            index.symbol.iter().any(|symbol| symbol.name == "run"),
            "{path}"
        );
        if !path.ends_with(".lua") {
            let (overview, captured) =
                DeclarationOverview::extract_with_calls(path, &source).unwrap();
            assert_eq!(captured, callable);
            DeclarationOverview::parse(path, &overview).unwrap();
            assert!(
                index
                    .reference
                    .iter()
                    .any(|reference| reference.path == ["Writer"]),
                "{path}"
            );
        }
    }
}
