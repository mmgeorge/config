use forge_diff::syntax::{
    DeclarationCallKind, DeclarationCalls, DeclarationIndex, DeclarationOverview,
};

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

#[test]
fn deep_body_traversal_preserves_binding_lifetimes_and_call_order() {
    for (path, source, expected) in [
        (
            "deep.ts",
            format!(
                "export class Store {{ run(input: Writer) {{ input.send(); {} let input: Other = create(input); input.send(); {} input.send(); }} }}",
                "{".repeat(1024),
                "}".repeat(1024),
            ),
            ["Writer.send", "create", "Other.send", "Writer.send"],
        ),
        (
            "deep.rs",
            format!(
                "pub fn run(input: Writer) {{ input.send(); {} let input: Other = create(input); input.send(); {} input.send(); }}",
                "{".repeat(1024),
                "}".repeat(1024),
            ),
            ["Writer::send", "create", "Other::send", "Writer::send"],
        ),
    ] {
        let callable = DeclarationCalls::extract(path, &source, false).unwrap();
        let actual = callable[0]
            .call
            .iter()
            .filter(|reference| reference.kind == DeclarationCallKind::Call)
            .map(|reference| reference.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(actual, expected, "{path}");
        for reference in &callable[0].call {
            assert!(
                source.lines().nth(reference.line as usize - 1).unwrap()
                    [reference.column as usize..]
                    .starts_with(reference.expression.rsplit('.').next().unwrap()),
                "{path}: {reference:?}"
            );
        }
    }
}

#[test]
fn deep_receiver_inference_does_not_recurse() {
    let source = format!(
        "export class Writer {{ next(): Writer {{ return this; }} }}\nexport function run(input: Writer) {{ const selected = {}input{}; selected.send(); input{}.send(); }}",
        "(".repeat(4096),
        ")".repeat(4096),
        ".next()".repeat(256),
    );
    let callable = DeclarationCalls::extract("deep.ts", &source, false).unwrap();
    let run = callable
        .iter()
        .find(|callable| callable.owner == "run")
        .unwrap();
    let calls = run
        .call
        .iter()
        .filter(|reference| reference.kind == DeclarationCallKind::Call)
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 258);
    assert_eq!(calls[0].name, "Writer.send");
    assert_eq!(calls[1].name, "Writer.send");
    assert!(
        calls[2..]
            .iter()
            .all(|reference| reference.name == "Writer.next")
    );
}

#[test]
fn deep_public_macro_projection_preserves_signature() {
    let source = format!(
        "{}pub macro retained($value:expr) {{ $value }}\n{}",
        "pub mod nested {\n".repeat(160),
        "}\n".repeat(160)
    );
    let index = DeclarationIndex::extract("deep.rs", &source).unwrap();
    assert!(!index.incomplete);
    let declaration = index
        .symbol
        .iter()
        .find(|symbol| symbol.name == "retained")
        .unwrap();
    assert!(declaration.macro_namespace);
    assert_eq!(declaration.scope.len(), 160);
}
