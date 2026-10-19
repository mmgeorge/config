use super::*;
use forge_diff::cache::CacheLimits;
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair};
use forge_harness::backend::TextGeneration;

#[path = "excerpt_regression_tests.rs"]
mod regression;

struct Fixture {
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
}

impl Fixture {
    fn new() -> Self {
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        Self { diff, syntax }
    }

    async fn file(&self, path: &str, old: &str, new: &str) -> ChangedFile {
        let source = |text: &str| {
            SourceVersion::new(text.as_bytes().to_vec(), Representation::GitCanonical).unwrap()
        };
        let analysis = self
            .diff
            .compare(DiffRequest {
                source: SourcePair {
                    old: source(old),
                    new: source(new),
                },
                priority: WorkPriority::Speculative,
            })
            .await
            .unwrap();
        ChangedFile {
            path: path.into(),
            note: String::new(),
            analysis: Some(analysis),
        }
    }

    async fn text(&self, path: &str, old: &str, new: &str) -> String {
        prompt(
            &mut [self.file(path, old, new).await],
            &self.syntax,
            MAX_INPUT_BYTES,
        )
        .await
    }
}

#[tokio::test]
async fn parsed_functions_keep_multiline_signatures_and_gutters() {
    let fixture = Fixture::new();
    let cases = [
        (
            "code.rs",
            "fn calculate(\n    value: i32,\n) -> i32 {\n",
            "    value\n",
            "}\n",
        ),
        (
            "code.ts",
            "function calculate(\n    value: number,\n): number {\n",
            "    return value;\n",
            "}\n",
        ),
        (
            "code.js",
            "function calculate(\n    value,\n) {\n",
            "    return value;\n",
            "}\n",
        ),
        (
            "code.lua",
            "local function calculate(\n    value\n)\n",
            "    return value\n",
            "end\n",
        ),
        (
            "code.py",
            "def calculate(\n    value: int,\n) -> int:\n",
            "    return value\n",
            "",
        ),
        (
            "code.c",
            "int calculate(\n    int value\n) {\n",
            "    return value;\n",
            "}\n",
        ),
        (
            "code.cpp",
            "template<typename Value>\nValue calculate(\n    Value value\n) {\n",
            "    return value;\n",
            "}\n",
        ),
    ];
    for (path, signature, operation, closing) in cases {
        let padding = if path.ends_with("lua") {
            "    -- padding\n".repeat(8)
        } else if path.ends_with("py") {
            "    # padding\n".repeat(8)
        } else {
            "    // padding\n".repeat(8)
        };
        let old = format!("{signature}{padding}{operation}{closing}");
        let new = old.replace(operation, &operation.replace("value", "value + 1"));
        let output = fixture.text(path, &old, &new).await;
        for line in signature.lines() {
            assert!(output.contains(&format!("  {line}\n")), "{path}: {output}");
        }
        assert!(
            output.contains(&format!("- {}", operation.trim_end())),
            "{path}: {output}"
        );
        assert!(
            output.contains(&format!(
                "+ {}",
                operation.trim_end().replace("value", "value + 1")
            )),
            "{path}: {output}"
        );
        assert!(output.contains("... 6 lines (+0 -0)"), "{path}: {output}");
    }
}

#[tokio::test]
async fn first_twenty_body_changes_are_fixed_and_reset_for_each_function() {
    let fixture = Fixture::new();
    let function = |name: &str, changed: bool| {
        let mut source = format!("fn {name}() {{\n");
        for position in 0..15 {
            source.push_str(&format!(
                "    step_{position}({});\n",
                if changed { 1 } else { 0 }
            ));
            source.push_str("    stable_a();\n    stable_b();\n    stable_c();\n");
        }
        source.push_str("}\n");
        source
    };
    let old = format!("{}{}", function("first", false), function("second", false));
    let new = format!("{}{}", function("first", true), function("second", true));
    let output = fixture.text("source.rs", &old, &new).await;
    assert_eq!(
        output
            .lines()
            .filter(|line| line.starts_with("+     step_"))
            .count(),
        20,
        "{output}"
    );
    assert_eq!(
        output
            .lines()
            .filter(|line| line.starts_with("-     step_"))
            .count(),
        20,
        "{output}"
    );
    assert!(!output.contains("step_10(1)"), "{output}");
    assert!(output.contains("  fn first() {\n") && output.contains("  fn second() {\n"));
    assert_eq!(output.matches("(+5 -5)").count(), 2, "{output}");
}

#[tokio::test]
async fn context_windows_merge_and_stop_at_two_unchanged_lines() {
    let fixture = Fixture::new();
    let old = "fn run() {\n    far_before_extra();\n    far_before();\n    before_two();\n    before_one();\n    first(0);\n    bridge();\n    second(0);\n    after_one();\n    after_two();\n    far_after();\n    far_after_extra();\n}\n";
    let new = old
        .replace("first(0)", "first(1)")
        .replace("second(0)", "second(1)");
    let output = fixture.text("source.rs", old, &new).await;
    assert!(
        !output.contains("far_before();") && !output.contains("far_after();"),
        "{output}"
    );
    for retained in [
        "before_two();",
        "before_one();",
        "bridge();",
        "after_one();",
        "after_two();",
    ] {
        assert_eq!(output.matches(retained).count(), 1, "{output}");
    }
}

#[tokio::test]
async fn new_deleted_and_changed_signatures_retain_exact_source() {
    let fixture = Fixture::new();
    let source = "fn added(\n    argument: i32,\n) -> i32 {\n    argument + 1\n}\n";
    let added = fixture.text("source.rs", "", source).await;
    let removed = fixture.text("source.rs", source, "").await;
    for line in source.lines().take(3) {
        assert!(added.contains(&format!("+ {line}\n")), "{added}");
    }
    for line in source.lines().take(3) {
        assert!(removed.contains(&format!("- {line}\n")), "{removed}");
    }
    assert!(!removed.contains("argument + 1"), "{removed}");
    assert!(removed.contains("... 2 lines (+0 -2)"), "{removed}");
    assert!(!added.contains("argument + 1"), "{added}");
    assert!(added.contains("... 2 lines (+2 -0)"), "{added}");
    let changed = fixture
        .text("source.rs", source, &source.replace("i32", "i64"))
        .await;
    assert!(
        changed.contains("-     argument: i32,\n") && changed.contains("+     argument: i64,\n"),
        "{changed}"
    );
}

#[tokio::test]
async fn nested_functions_emit_each_source_line_once() {
    let fixture = Fixture::new();
    let old =
        "function outer(value) {\n  function inner() {\n    return 1;\n  }\n  return inner();\n}\n";
    let new = old
        .replace("return 1", "return 2")
        .replace("return inner()", "return inner() + value");
    let output = fixture.text("source.ts", old, &new).await;
    assert_eq!(output.matches("function inner()").count(), 1, "{output}");
    assert_eq!(output.matches("+     return 2;").count(), 1, "{output}");
    assert_eq!(
        output.matches("+   return inner() + value;").count(),
        1,
        "{output}"
    );
}

#[tokio::test]
async fn recognized_imports_are_inventory_only() {
    let fixture = Fixture::new();
    let cases = [
        (
            "source.rs",
            "use old::Item;\nfn run() {}\n",
            "use new::Item;\nfn run() {}\n",
        ),
        (
            "source.ts",
            "import { old } from 'old';\nfunction run() {}\n",
            "import { next } from 'next';\nfunction run() {}\n",
        ),
        (
            "source.js",
            "import { old } from 'old';\nfunction run() {}\n",
            "import { next } from 'next';\nfunction run() {}\n",
        ),
        (
            "source.lua",
            "local dependency = require('old')\nlocal function run() end\n",
            "local dependency = require('new')\nlocal function run() end\n",
        ),
        (
            "source.lua",
            "local dependency = require 'old'\nlocal function run() end\n",
            "local dependency = require 'new'\nlocal function run() end\n",
        ),
        (
            "source.js",
            "const dependency = require('old');\nfunction run() {}\n",
            "const dependency = require('new');\nfunction run() {}\n",
        ),
        (
            "source.py",
            "from old import Item\ndef run():\n    pass\n",
            "from new import Item\ndef run():\n    pass\n",
        ),
        (
            "schema.cue",
            "package schema\nimport \"strings\"\nvalue: 1\n",
            "package schema\nimport \"math\"\nvalue: 1\n",
        ),
    ];
    for (path, old, new) in cases {
        let output = fixture.text(path, old, new).await;
        assert!(output.contains(&format!("  {path} (+1 -1)")), "{output}");
        assert!(
            !output.contains("\n+ ") && !output.contains("\n- "),
            "{path}: {output}"
        );
        assert!(!output.contains(&format!("\n{path} (+1 -1)\n")), "{output}");
    }
}

#[tokio::test]
async fn configuration_and_unknown_languages_retain_changed_rows() {
    let fixture = Fixture::new();
    let cases = [
        (
            "config.json",
            "{\n  \"count\": 1\n}\n",
            "{\n  \"count\": 2\n}\n",
            "\"count\": 2",
        ),
        (
            "config.toml",
            "[server]\ncount = 1\n",
            "[server]\ncount = 2\n",
            "count = 2",
        ),
        (
            "config.yaml",
            "server:\n  count: 1\n",
            "server:\n  count: 2\n",
            "count: 2",
        ),
        (
            "schema.cue",
            "package schema\n#Server: {\n    port: int & >0\n}\nserver: #Server & {port: 80}\n",
            "package schema\n#Server: {\n    port: int & >1024\n}\nserver: #Server & {port: 8080}\n",
            "+     port: int & >1024",
        ),
        ("unknown.ext", "old\n", "new\n", "+ new"),
    ];
    for (path, old, new, expected) in cases {
        let output = fixture.text(path, old, new).await;
        assert!(output.contains(expected), "{path}: {output}");
        assert!(
            output.contains("\n- ") && output.contains("\n+ "),
            "{path}: {output}"
        );
    }
}

#[tokio::test]
async fn markdown_and_plain_text_keep_every_changed_line_but_only_two_context_lines() {
    let fixture = Fixture::new();
    let old = (0..80)
        .map(|position| format!("old paragraph {position}\n"))
        .collect::<String>();
    let new = old.replace("old paragraph", "new paragraph");
    for path in ["README.md", "notes.txt", "NOTES.TXT"] {
        let output = fixture.text(path, &old, &new).await;
        assert_eq!(
            output
                .lines()
                .filter(|line| line.starts_with("+ new paragraph"))
                .count(),
            80,
            "{path}"
        );
        assert_eq!(
            output
                .lines()
                .filter(|line| line.starts_with("- old paragraph"))
                .count(),
            80,
            "{path}"
        );
        let small = fixture
            .text(
                path,
                &old,
                &old.replace("old paragraph 40\n", "changed paragraph\n"),
            )
            .await;
        assert!(
            small.contains("  old paragraph 38\n") && small.contains("  old paragraph 42\n"),
            "{small}"
        );
        assert!(
            !small.contains("  old paragraph 37\n") && !small.contains("  old paragraph 43\n"),
            "{small}"
        );
    }
}

#[tokio::test]
async fn test_and_spec_files_are_last_and_fully_omitted_sections_are_absent() {
    let fixture = Fixture::new();
    let mut files = vec![
        fixture.file("a_test.rs", "", "fn check() {}\n").await,
        fixture.file("tests/a.rs", "", "fn other() {}\n").await,
        fixture.file("b_spec.ts", "", "function check() {}\n").await,
        fixture.file("source.rs", "", "fn run() {}\n").await,
    ];
    let full = prompt(&mut files, &fixture.syntax, 20_000).await;
    assert_eq!(
        files
            .iter()
            .map(|file| file.path.as_str())
            .collect::<Vec<_>>(),
        ["source.rs", "a_test.rs", "b_spec.ts", "tests/a.rs"]
    );
    let limit = full.find("\na_test.rs (+1 -0)\n").unwrap();
    let output = prompt(&mut files, &fixture.syntax, limit).await;
    assert!(output.contains("  a_test.rs (+1 -0)"));
    assert!(output.contains("\nsource.rs (+1 -0)\n"));
    assert!(!output.contains("\na_test.rs (+1 -0)\n"));
    assert!(!output.contains("function check()") && !output.contains("fn other()"));
}

#[tokio::test]
async fn global_limit_stops_without_shrinking_the_twenty_line_allowance_or_splitting_utf8() {
    let fixture = Fixture::new();
    let source = format!(
        "fn run() {{\n{}\n}}\n",
        (0..30)
            .map(|position| format!("    step_{position}(\"λ🦀\");\n"))
            .collect::<String>()
    );
    let mut files = vec![fixture.file("source.rs", "fn run() {\n}\n", &source).await];
    let full = prompt(&mut files, &fixture.syntax, 20_000).await;
    for limit in [500, 600, 700, 800, 900, 1000] {
        let output = prompt(&mut files, &fixture.syntax, limit).await;
        assert!(output.len() <= limit);
        assert!(full.starts_with(&output), "limit {limit}: {output}");
        assert!(output.is_empty() || output.ends_with('\n'));
    }
    assert_eq!(
        full.lines()
            .filter(|line| line.starts_with("+     step_"))
            .count(),
        20,
        "{full}"
    );
}

#[tokio::test]
async fn oversized_first_line_stops_before_emitting_an_empty_file_section() {
    let fixture = Fixture::new();
    let mut files = vec![
        fixture
            .file("a.txt", "", &format!("{}\n", "λ".repeat(300_000)))
            .await,
        fixture.file("b.txt", "", "later\n").await,
    ];
    let output = prompt(
        &mut files,
        &fixture.syntax,
        MAX_INPUT_BYTES - TextGeneration::CommitMessage.instructions().len(),
    )
    .await;
    assert!(output.len() + TextGeneration::CommitMessage.instructions().len() <= MAX_INPUT_BYTES);
    assert!(output.contains("  a.txt (+1 -0)") && output.contains("  b.txt (+1 -0)"));
    assert!(
        !output.contains("\na.txt (+1 -0)\n") && !output.contains("+ later"),
        "{output}"
    );
}

#[tokio::test]
async fn malformed_syntax_does_not_lose_changes() {
    let fixture = Fixture::new();
    let output = fixture
        .text("source.rs", "fn broken( {\nold\n", "fn broken( {\nnew\n")
        .await;
    assert!(
        output.contains("- old\n") && output.contains("+ new\n"),
        "{output}"
    );
}

#[tokio::test]
async fn crlf_unicode_and_missing_final_newlines_preserve_visible_text() {
    let fixture = Fixture::new();
    let output = fixture
        .text(
            "notes.txt",
            "before λ\r\nunchanged",
            "after 🦀\r\nunchanged",
        )
        .await;
    assert!(
        output.contains("- before λ\n") && output.contains("+ after 🦀\n"),
        "{output}"
    );
    assert!(!output.contains('\r'));
    assert!(output.contains("  unchanged\n"));
}

#[tokio::test]
async fn every_parsed_function_language_omits_added_and_removed_bodies() {
    let fixture = Fixture::new();
    for (path, signature, statement, closing) in [
        ("source.rs", "fn run() {\n", "    step();\n", "}\n"),
        ("source.ts", "function run() {\n", "    step();\n", "}\n"),
        ("source.js", "function run() {\n", "    step();\n", "}\n"),
        (
            "source.lua",
            "local function run()\n",
            "    step()\n",
            "end\n",
        ),
        ("source.py", "def run():\n", "    step()\n", ""),
    ] {
        let source = format!("{signature}{}{closing}", statement.repeat(35));
        for (old, new, marker) in [("", source.as_str(), '+'), (source.as_str(), "", '-')] {
            let output = fixture.text(path, old, new).await;
            assert_eq!(
                output
                    .lines()
                    .filter(|line| line.starts_with(&format!("{marker}     step")))
                    .count(),
                0,
                "{path}: {output}"
            );
            assert!(
                output.contains(&format!("{marker} {}", signature.trim_end())),
                "{path}: {output}"
            );
            if !closing.is_empty() {
                assert_eq!(
                    output.contains(&format!("{marker} {}", closing.trim_end())),
                    false,
                    "{path}: {output}"
                );
            }
            assert!(
                output.contains(&{
                    let omitted = 35 + usize::from(!closing.is_empty());
                    if marker == '+' {
                        format!("{omitted} lines (+{omitted} -0)")
                    } else {
                        format!("{omitted} lines (+0 -{omitted})")
                    }
                }),
                "{path}: {output}"
            );
        }
    }
}

#[tokio::test]
async fn multiline_signatures_are_not_cut_by_the_global_byte_limit() {
    let fixture = Fixture::new();
    let source = "fn run(\n    first: i32,\n    second: i32,\n) -> i32 {\n    first + second\n}\n";
    let mut files = [fixture.file("source.rs", "", source).await];
    let full = prompt(&mut files, &fixture.syntax, 20_000).await;
    let cut = full.find("+     second").unwrap();
    let output = prompt(&mut files, &fixture.syntax, cut).await;
    assert!(!output.contains("+ fn run("), "{output}");
    assert!(!output.contains("\nsource.rs (+6 -0)\n"), "{output}");
}

#[tokio::test]
async fn inventory_itself_cannot_exceed_the_global_byte_limit() {
    let fixture = Fixture::new();
    let mut files: Vec<_> = (0..100)
        .map(|position| ChangedFile {
            path: format!("{position:04}-{}", "λ".repeat(50)),
            note: "binary".into(),
            analysis: None,
        })
        .collect();
    let output = prompt(&mut files, &fixture.syntax, 1024).await;
    assert!(output.len() <= 1024);
    assert!(output.contains("0000-") && !output.contains("0099-"));
    assert!(output.ends_with('\n'));
}

#[tokio::test]
async fn python_decorated_async_methods_retain_their_complete_signature() {
    let fixture = Fixture::new();
    let signature = "    @classmethod\n    @cache(ttl=30)\n    async def fetch(\n        cls,\n        value: int,\n    ) -> dict[str, int]:\n";
    let source = format!(
        "class Client:\n{signature}{}        return {{\"value\": value}}\n",
        "        # padding\n".repeat(8)
    );
    let updated = source.replace("\"value\": value", "\"value\": value + 1");
    for path in ["client.py", "client.pyi", "client.pyw"] {
        let output = fixture.text(path, &source, &updated).await;
        for line in signature.lines() {
            assert_eq!(
                output.matches(&format!("  {line}\n")).count(),
                1,
                "{output}"
            );
        }
        assert!(
            output.contains("+         return {\"value\": value + 1}"),
            "{output}"
        );
        assert!(output.contains("6 lines (+0 -0)"), "{output}");
    }
}

#[tokio::test]
async fn python_additions_deletions_and_signature_edits_preserve_decorators() {
    let fixture = Fixture::new();
    let source = "@cache\ndef calculate(\n    value: int,\n) -> int:\n    return value + 1\n";
    for (old, new, marker) in [("", source, '+'), (source, "", '-')] {
        let output = fixture.text("source.py", old, new).await;
        for line in source.lines().take(4) {
            assert!(output.contains(&format!("{marker} {line}\n")), "{output}");
        }
        assert!(
            output.contains(&format!("{marker}     return value + 1\n")),
            "{output}"
        );
    }
    let output = fixture
        .text("source.py", source, &source.replace("int", "float"))
        .await;
    assert!(output.contains("  @cache\n"), "{output}");
    assert!(output.contains("-     value: int,\n"), "{output}");
    assert!(output.contains("+     value: float,\n"), "{output}");
}

#[tokio::test]
async fn python_nested_functions_emit_each_changed_line_once() {
    let fixture = Fixture::new();
    let old = "def outer(value):\n    def inner():\n        return 1\n    return inner()\n";
    let new = old
        .replace("return 1", "return 2")
        .replace("return inner()", "return inner() + value");
    let output = fixture.text("source.py", old, &new).await;
    for line in [
        "  def outer(value):\n",
        "      def inner():\n",
        "+         return 2\n",
        "+     return inner() + value\n",
    ] {
        assert_eq!(output.matches(line).count(), 1, "{output}");
    }
}

#[tokio::test]
async fn python_multiline_imports_do_not_hide_neighboring_function_changes() {
    let fixture = Fixture::new();
    let old = "import old\nfrom dependency import (\n    First,\n    Second,\n)\ndef run():\n    return 1\n";
    let new = old
        .replace("import old", "import next")
        .replace("Second", "Third")
        .replace("return 1", "return 2");
    let output = fixture.text("source.py", old, &new).await;
    assert!(output.contains("  source.py (+3 -3)"), "{output}");
    assert!(output.contains("+     return 2"), "{output}");
    assert!(
        !output.contains("Third") && !output.contains("import next"),
        "{output}"
    );
}

#[tokio::test]
async fn malformed_python_retains_ordinary_changed_rows() {
    let fixture = Fixture::new();
    let output = fixture
        .text("broken.py", "def broken(\nold\n", "def broken(\nnew\n")
        .await;
    assert!(
        output.contains("- old\n") && output.contains("+ new\n"),
        "{output}"
    );
}
