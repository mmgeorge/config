use super::*;

#[tokio::test]
async fn added_and_removed_inline_functions_retain_only_their_signatures() {
    let fixture = Fixture::new();
    for (path, source, signature) in [
        ("source.rs", "fn run() { hidden_body(); }\n", "fn run() {"),
        (
            "source.ts",
            "function run() { hidden_body(); }\n",
            "function run() {",
        ),
        (
            "source.js",
            "const run = value => hidden_body(value);\n",
            "const run = value =>",
        ),
        (
            "source.lua",
            "local function run() hidden_body() end\n",
            "local function run()",
        ),
        ("source.py", "def run(): hidden_body()\n", "def run():"),
        (
            "source.py",
            "run = lambda value: hidden_body(value)\n",
            "run = lambda value:",
        ),
        (
            "source.py",
            "run = lambda: hidden_body()\n",
            "run = lambda:",
        ),
    ] {
        for (old, new, marker) in [("", source, '+'), (source, "", '-')] {
            let output = fixture.text(path, old, new).await;
            assert!(
                output.contains(&format!("{marker} {signature}\n")),
                "{path}: {output}"
            );
            assert!(!output.contains("hidden_body"), "{path}: {output}");
        }
    }
}

#[tokio::test]
async fn single_line_gap_cannot_overflow_the_global_byte_limit() {
    let fixture = Fixture::new();
    let source = format!("{}{}\n", "short\n".repeat(20), "large".repeat(2000));
    let mut files = [
        fixture.file("a.unknown", "", &source).await,
        fixture.file("z.txt", "", "later\n").await,
    ];
    let output = prompt(&mut files, &fixture.syntax, 1000).await;
    assert!(output.len() <= 1000);
    assert!(!output.contains("large"), "{output}");
    assert!(!output.contains("... 1 lines"), "{output}");
    assert!(!output.contains("+ later\n"), "{output}");
}

#[tokio::test]
async fn inserted_helpers_cannot_separate_old_and_new_function_signatures() {
    let fixture = Fixture::new();
    for (path, helper, old_signature, new_signature, old_body, new_body, closing) in [
        (
            "source.rs",
            "fn helper() {\n    helper_one();\n    helper_two();\n}\n",
            "fn target(value: i32) -> i32 {\n",
            "fn target(\n    value: i64,\n) -> i64 {\n",
            "    previous(value)\n",
            "    updated(value)\n",
            "}\n",
        ),
        (
            "source.ts",
            "function helper() {\n    helper_one();\n    helper_two();\n}\n",
            "function target(value: number): number {\n",
            "function target(\n    value: string,\n): string {\n",
            "    return previous(value);\n",
            "    return updated(value);\n",
            "}\n",
        ),
        (
            "source.js",
            "function helper() {\n    helper_one();\n    helper_two();\n}\n",
            "function target(value) {\n",
            "function target(\n    value, options,\n) {\n",
            "    return previous(value);\n",
            "    return updated(value);\n",
            "}\n",
        ),
        (
            "source.lua",
            "local function helper()\n    helper_one()\n    helper_two()\nend\n",
            "local function target(value)\n",
            "local function target(\n    value, options\n)\n",
            "    return previous(value)\n",
            "    return updated(value)\n",
            "end\n",
        ),
        (
            "source.py",
            "def helper():\n    helper_one()\n    helper_two()\n",
            "def target(value: int) -> int:\n",
            "def target(\n    value: str,\n) -> str:\n",
            "    return previous(value)\n",
            "    return updated(value)\n",
            "",
        ),
    ] {
        let old = format!("{old_signature}{old_body}{closing}");
        let new = format!("{helper}{new_signature}{new_body}{closing}");
        let output = fixture.text(path, &old, &new).await;
        let paired = old_signature
            .lines()
            .map(|line| format!("- {line}\n"))
            .chain(new_signature.lines().map(|line| format!("+ {line}\n")))
            .collect::<String>();
        assert!(output.contains(&paired), "{path}: {output}");
        assert!(
            output.find("+ ").unwrap() < output.find(&paired).unwrap(),
            "{path}: {output}"
        );
        assert_eq!(
            output
                .matches(&format!("- {}\n", old_signature.trim_end()))
                .count(),
            1,
            "{path}: {output}"
        );
        assert!(
            output.contains(old_body.trim()) && output.contains(new_body.trim()),
            "{path}: {output}"
        );
    }
}

#[tokio::test]
async fn regrouped_signatures_are_atomic_at_the_byte_limit() {
    let fixture = Fixture::new();
    let old = "fn target(value: i32) -> i32 {\n    value\n}\n";
    let new = "fn helper() {\n    first();\n    second();\n}\nfn target(\n    value: i64,\n) -> i64 {\n    value\n}\n";
    let mut files = [fixture.file("source.rs", old, new).await];
    let full = prompt(&mut files, &fixture.syntax, 20_000).await;
    let old_signature = "- fn target(value: i32) -> i32 {\n";
    let new_signature = "+ fn target(\n+     value: i64,\n+ ) -> i64 {\n";
    assert!(
        full.contains(&format!("{old_signature}{new_signature}")),
        "{full}"
    );
    let limit = full.find("+     value: i64,").unwrap() + 5;
    let limited = prompt(&mut files, &fixture.syntax, limit).await;
    assert!(limited.contains("+ fn helper() {\n"), "{limited}");
    assert!(
        !limited.contains(old_signature) && !limited.contains("+ fn target("),
        "{limited}"
    );
    assert!(limited.len() <= limit);
}
