use super::*;

#[tokio::test]
async fn c_headers_with_recovered_preprocessor_errors_keep_function_excerpts() {
    let fixture = Fixture::new();
    let source = include_str!("../../crates/forge-diff/tests/fixtures/c_headers/array.h");
    let output = fixture.text("array.h", "", source).await;
    for name in [
        "delete", "erase", "reserve", "assign", "swap", "grow", "splice",
    ] {
        assert!(
            output.contains(&format!("+ static inline void _array__{name}(")),
            "{output}"
        );
    }
    assert!(!output.contains("#include"), "{output}");
    assert!(!output.contains("ts_free(self->contents)"), "{output}");
    assert!(!output.contains("Array swap = *other;"), "{output}");

    let changed = source.replace("self->capacity * 2", "self->capacity * 3");
    let output = fixture.text("array.h", source, &changed).await;
    assert!(
        output.contains("  static inline void _array__grow("),
        "{output}"
    );
    assert!(
        output.contains("-     uint32_t new_capacity = self->capacity * 2;\n"),
        "{output}"
    );
    assert!(
        output.contains("+     uint32_t new_capacity = self->capacity * 3;\n"),
        "{output}"
    );
}

#[tokio::test]
async fn malformed_regions_keep_changed_rows_beside_valid_new_functions() {
    let fixture = Fixture::new();
    for (path, source, signature, malformed) in [
        (
            "mixed.c",
            "int valid() {\n    first();\n    second();\n    third();\n}\nint broken( {\n    damaged();\n}\n",
            "int valid() {",
            "    damaged();",
        ),
        (
            "mixed.rs",
            "fn valid() {\n    first();\n    second();\n    third();\n}\nfn broken( {\n    damaged();\n}\n",
            "fn valid() {",
            "    damaged();",
        ),
        (
            "mixed.ts",
            "function valid() {\n    first();\n    second();\n    third();\n}\nfunction broken( {\n    damaged();\n}\n",
            "function valid() {",
            "    damaged();",
        ),
        (
            "mixed.py",
            "def valid():\n    first()\n    second()\n    third()\ndef broken(:\n    damaged()\n",
            "def valid():",
            "    damaged()",
        ),
    ] {
        let output = fixture.text(path, "", source).await;
        assert!(
            output.contains(&format!("+ {signature}\n")),
            "{path}: {output}"
        );
        assert!(
            output.contains(&format!("+ {malformed}\n")),
            "{path}: {output}"
        );
        assert!(!output.contains("second()"), "{path}: {output}");
    }
}

#[tokio::test]
async fn c_and_cpp_functions_obey_lifecycle_and_body_budget() {
    let fixture = Fixture::new();
    for (path, signature) in [
        ("lookup.c", "const char *lookup(int value) {"),
        ("lookup.h", "static int lookup(int value) {"),
        (
            "lookup.cpp",
            "template<typename Value>\nValue lookup(Value value) {",
        ),
        ("lookup.hpp", "inline int Runner::lookup(int value) const {"),
    ] {
        let old_body: String = (0..40)
            .map(|index| format!("    previous({index});\n"))
            .collect();
        let new_body: String = (0..40)
            .map(|index| format!("    updated({index});\n"))
            .collect();
        let old = format!("#include <old.h>\n{signature}\n{old_body}}}\n");
        let new = format!("#include <new.h>\n{signature}\n{new_body}}}\n");
        let modified = fixture.text(path, &old, &new).await;
        assert!(!modified.contains("#include"), "{modified}");
        assert!(modified.contains("lookup("), "{modified}");
        let body_rows = modified
            .lines()
            .filter(|line| {
                (line.starts_with("+ ") || line.starts_with("- "))
                    && (line.contains("previous(") || line.contains("updated("))
            })
            .count();
        assert_eq!(body_rows, 20, "{modified}");
        assert!(modified.contains("... "), "{modified}");
        for (before, after, gutter) in [("", new.as_str(), "+ "), (old.as_str(), "", "- ")] {
            let output = fixture.text(path, before, after).await;
            for line in signature.lines() {
                assert!(output.contains(&format!("{gutter}{line}\n")), "{output}");
            }
            assert!(!output.contains("previous("), "{output}");
            assert!(!output.contains("updated("), "{output}");
        }
    }
}

#[tokio::test]
async fn cpp_inline_callbacks_stay_in_modified_body_and_bound_lambdas_keep_signatures() {
    let fixture = Fixture::new();
    let old = "void run() {\n    previous();\n}\n";
    let new = "void run() {\n    consume([](int value) {\n        return value + 1;\n    });\n    finish();\n}\n";
    let output = fixture.text("client.cc", old, new).await;
    let added = additions(&output);
    assert_eq!(
        added,
        new.lines().skip(1).take(4).collect::<Vec<_>>(),
        "{output}"
    );
    let new =
        "auto calculate = [](int value) {\n    first();\n    second();\n    return value;\n};\n";
    let output = fixture.text("client.cxx", "", new).await;
    assert!(
        output.contains("+ auto calculate = [](int value) {\n"),
        "{output}"
    );
    assert!(!output.contains("first();"), "{output}");
    assert!(!output.contains("return value;"), "{output}");
}

#[tokio::test]
async fn scm_query_changes_keep_patterns_captures_and_predicates() {
    let fixture = Fixture::new();
    let old = "((identifier) @variable\n (#eq? @variable \"old\"))\n";
    let new = "((identifier) @constant\n (#eq? @constant \"NEW\"))\n";
    let output = fixture.text("queries/highlights.scm", old, new).await;
    for line in old.lines() {
        assert!(output.contains(&format!("- {line}\n")), "{output}");
    }
    for line in new.lines() {
        assert!(output.contains(&format!("+ {line}\n")), "{output}");
    }
}

fn additions(output: &str) -> Vec<&str> {
    output
        .lines()
        .filter_map(|line| line.strip_prefix("+ "))
        .collect()
}

#[tokio::test]
async fn inline_callbacks_and_nested_declarations_preserve_source_order() {
    let fixture = Fixture::new();
    let cases = [
        (
            "chain.rs",
            "fn run() {\n    previous();\n}\n",
            "fn run() {\n    let result = values\n        .iter()\n        .filter(|value| value.active())\n        .map(|value| {\n            transform(value)\n        })\n        .collect();\n    finish(result);\n}\n",
        ),
        (
            "chain.ts",
            "function run() {\n    previous();\n}\n",
            "function run() {\n    const result = values\n        .filter(value => value.active)\n        .map(function transform(value) {\n            return value.name;\n        });\n    finish(result);\n}\n",
        ),
        (
            "chain.js",
            "function run() {\n    previous();\n}\n",
            "function run() {\n    const result = values\n        .filter(value => value.active)\n        .map(value => {\n            return value.name;\n        });\n    finish(result);\n}\n",
        ),
        (
            "chain.lua",
            "local function run()\n    previous()\nend\n",
            "local function run()\n    consume(function(value)\n        return transform(value)\n    end)\n    finish()\nend\n",
        ),
        (
            "chain.py",
            "def run():\n    previous()\n",
            "def run():\n    result = sorted(\n        values,\n        key=lambda value: value.name,\n    )\n    finish(result)\n",
        ),
        (
            "nested.rs",
            "fn outer() {\n    fn inner() {\n        previous();\n    }\n}\n",
            "fn outer() {\n    before();\n    fn inner() {\n        inside();\n    }\n    after();\n}\n",
        ),
        (
            "nested.ts",
            "function outer() {\n    function inner() {\n        previous();\n    }\n}\n",
            "function outer() {\n    before();\n    function inner() {\n        inside();\n    }\n    after();\n}\n",
        ),
        (
            "nested.lua",
            "local function outer()\n    local function inner()\n        previous()\n    end\nend\n",
            "local function outer()\n    before()\n    local function inner()\n        inside()\n    end\n    after()\nend\n",
        ),
        (
            "nested.py",
            "def outer():\n    def inner():\n        previous()\n",
            "def outer():\n    before()\n    def inner():\n        inside()\n    after()\n",
        ),
        (
            "default.ts",
            "function run(\n    transform = (value) => value.name,\n) {\n    previous();\n}\n",
            "function run(\n    transform = (value) => value.name,\n) {\n    finish(transform(input));\n}\n",
        ),
        (
            "chain.cs",
            "class Runner {\n    int Run() {\n        return previous();\n    }\n}\n",
            "class Runner {\n    int Run() {\n        return values.Where(value => value > 0)\n            .Select(value => {\n                return value + 1;\n            }).Count();\n    }\n}\n",
        ),
    ];
    let mut failures = Vec::new();
    for (path, old, source) in cases {
        let output = fixture.text(path, old, source).await;
        let retained: Vec<_> = output
            .split_once(&format!("\n{path} ("))
            .unwrap()
            .1
            .split_once("\n\n")
            .unwrap()
            .1
            .lines()
            .filter(|line| !line.starts_with("  ... "))
            .filter_map(|line| line.strip_prefix("+ ").or_else(|| line.strip_prefix("  ")))
            .collect();
        if retained != source.lines().collect::<Vec<_>>() {
            failures.push(format!("{path}:\n{output}"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[tokio::test]
async fn top_level_statements_stay_between_neighboring_declarations() {
    let fixture = Fixture::new();
    for (path, source) in [
        (
            "source.rs",
            "const BEFORE: i32 = 1;\nfn first() {\n    work();\n}\nconst BETWEEN: i32 = 2;\nfn second() {\n    more();\n}\nconst AFTER: i32 = 3;\n",
        ),
        (
            "source.ts",
            "const before = 1;\nfunction first() {\n    work();\n}\nconst between = 2;\nfunction second() {\n    more();\n}\nconst after = 3;\n",
        ),
        (
            "source.py",
            "before = 1\ndef first():\n    work()\nbetween = 2\ndef second():\n    more()\nafter = 3\n",
        ),
        (
            "source.lua",
            "local before = 1\nlocal function first()\n    work()\nend\nlocal between = 2\nlocal function second()\n    more()\nend\nlocal after = 3\n",
        ),
    ] {
        let output = fixture.text(path, "", source).await;
        assert_eq!(
            additions(&output),
            source
                .lines()
                .filter(|line| !line.contains("work()")
                    && !line.contains("more()")
                    && !matches!(line.trim(), "}" | "end")
                    || path.ends_with("py"))
                .collect::<Vec<_>>(),
            "{path}: {output}"
        );
    }
}

#[tokio::test]
async fn inline_callbacks_share_the_containing_function_body_limit() {
    let fixture = Fixture::new();
    for (path, opening, statement, closing) in [
        (
            "source.rs",
            "fn run() {\n",
            "    values.iter().map(|value| transform(value));\n",
            "}\n",
        ),
        (
            "source.ts",
            "function run() {\n",
            "    values.map(value => transform(value));\n",
            "}\n",
        ),
        (
            "source.lua",
            "local function run()\n",
            "    consume(function(value) return transform(value) end)\n",
            "end\n",
        ),
        (
            "source.py",
            "def run():\n",
            "    consume(lambda value: transform(value))\n",
            "",
        ),
    ] {
        let old = format!("{opening}{statement}{closing}");
        let source = format!("{opening}{}{closing}", statement.repeat(36));
        let output = fixture.text(path, &old, &source).await;
        assert_eq!(
            additions(&output)
                .iter()
                .filter(|line| line.contains("transform(value)"))
                .count(),
            20,
            "{path}: {output}"
        );
        assert!(output.contains("15 lines (+15 -0)"), "{path}: {output}");
    }
}

#[tokio::test]
async fn import_filter_keeps_mixed_statements_and_runtime_calls() {
    let fixture = Fixture::new();
    for (path, old, new) in [
        (
            "source.rs",
            "use dependency::Item; fn run() { old(); }\n",
            "use dependency::Item; fn run() { next(); }\n",
        ),
        (
            "source.ts",
            "import { item } from 'dep'; old();\n",
            "import { item } from 'dep'; next();\n",
        ),
        (
            "source.py",
            "import dependency; old()\n",
            "import dependency; next()\n",
        ),
        (
            "source.lua",
            "local dep = require('dep'); old()\n",
            "local dep = require('dep'); next()\n",
        ),
        (
            "dynamic.ts",
            "async function load() {\n    return import('old');\n}\n",
            "async function load() {\n    return import('next');\n}\n",
        ),
        (
            "runtime.lua",
            "local function load()\n    return require('old')\nend\n",
            "local function load()\n    return require('next')\nend\n",
        ),
        (
            "runtime.rs",
            "fn load() {\n    let value = require(1);\n}\n",
            "fn load() {\n    let value = require(2);\n}\n",
        ),
    ] {
        let output = fixture.text(path, old, new).await;
        for line in new
            .lines()
            .filter(|line| line.contains("next") || line.contains("require(2)"))
        {
            assert!(output.contains(&format!("+ {line}\n")), "{path}: {output}");
        }
    }
}

#[tokio::test]
async fn methods_with_the_same_name_keep_independent_old_and_new_budgets() {
    let fixture = Fixture::new();
    for path in ["source.ts", "source.js", "source.rs", "source.py"] {
        let class = |name: &str, operation: &str, value: usize| {
            if path.ends_with("py") {
                format!(
                    "class {name}:\n    def run(self):\n{}",
                    format!("        {operation}({value})\n").repeat(12)
                )
            } else if path.ends_with("rs") {
                format!(
                    "struct {name};\nimpl {name} {{\n    fn run(&self) {{\n{}    }}\n}}\n",
                    format!("        {operation}({value});\n").repeat(12)
                )
            } else {
                format!(
                    "class {name} {{\n    run() {{\n{}    }}\n}}\n",
                    format!("        {operation}({value});\n").repeat(12)
                )
            }
        };
        let old = format!(
            "{}{}",
            class("First", "first", 0),
            class("Second", "second", 0)
        );
        let new = format!(
            "{}{}{}",
            class("Before", "before", 1),
            class("First", "first", 1),
            class("Second", "second", 1)
        );
        let output = fixture.text(path, &old, &new).await;
        for operation in ["first", "second"] {
            assert_eq!(
                additions(&output)
                    .iter()
                    .filter(|line| line.contains(&format!("{operation}(1)")))
                    .count(),
                8,
                "{path}/{operation}: {output}"
            );
        }
    }
}

#[tokio::test]
async fn bound_functions_keep_the_binding_name_and_multiline_signature() {
    let fixture = Fixture::new();
    for (path, signature, statement, closing) in [
        (
            "source.ts",
            "export const calculate = (\n    value: number,\n): number => {\n",
            "    return value;\n",
            "};\n",
        ),
        (
            "source.js",
            "const calculate = function(\n    value,\n) {\n",
            "    return value;\n",
            "};\n",
        ),
        (
            "source.lua",
            "local calculate = function(\n    value\n)\n",
            "    return value\n",
            "end\n",
        ),
        (
            "source.rs",
            "fn outer() {\n    let calculate = |\n        value: i32\n    | {\n",
            "        value\n",
            "    };\n}\n",
        ),
    ] {
        let padding = if path.ends_with("lua") {
            "    -- padding\n"
        } else {
            "    // padding\n"
        };
        let old = format!("{signature}{}{statement}{closing}", padding.repeat(8));
        let output = fixture
            .text(
                path,
                &old,
                &old.replace(statement, &statement.replace("value", "value + 1")),
            )
            .await;
        for line in signature.lines() {
            assert!(output.contains(&format!("  {line}\n")), "{path}: {output}");
        }
    }
}

#[tokio::test]
async fn nested_unchanged_declarations_do_not_disappear_between_context_rows() {
    let fixture = Fixture::new();
    let old = format!(
        "fn outer() {{\n    before(0);\n    fn inner() {{\n{}    }}\n    after(0);\n}}\n",
        "        unchanged();\n".repeat(20)
    );
    let new = old
        .replace("before(0)", "before(1)")
        .replace("after(0)", "after(1)");
    let output = fixture.text("source.rs", &old, &new).await;
    assert!(output.contains("22 lines (+0 -0)"), "{output}");
    assert!(!output.contains("unchanged();"), "{output}");
    let omission = output.find("22 lines").unwrap();
    assert!(output.find("before(1)").unwrap() < omission, "{output}");
    assert!(omission < output.find("after(0)").unwrap(), "{output}");
}

#[tokio::test]
async fn retained_rows_preserve_both_source_versions_in_order() {
    let fixture = Fixture::new();
    let cases = [
        (
            "source.rs",
            "fn outer() {\n    before(0);\n    fn inner() {\n        inside(0);\n    }\n    after(0);\n}\n",
        ),
        (
            "source.ts",
            "function outer() {\n    before(0);\n    function inner() {\n        inside(0);\n    }\n    after(0);\n}\n",
        ),
        (
            "source.lua",
            "local function outer()\n    before(0)\n    local function inner()\n        inside(0)\n    end\n    after(0)\nend\n",
        ),
        (
            "source.py",
            "def outer():\n    before(0)\n    def inner():\n        inside(0)\n    after(0)\n",
        ),
        (
            "config.json",
            "{\n  \"before\": 0,\n  \"inner\": {\n    \"inside\": 0\n  },\n  \"after\": 0\n}\n",
        ),
        (
            "config.toml",
            "before = 0\nafter = 0\n[inner]\ninside = 0\n",
        ),
        ("config.yaml", "before: 0\ninner:\n  inside: 0\nafter: 0\n"),
    ];
    for (path, source) in cases {
        for (old, new) in [
            (source.to_owned(), source.replace('0', "1")),
            (source.to_owned(), String::new()),
        ] {
            let output = fixture.text(path, &old, &new).await;
            let detail = output
                .split_once(&format!("\n{path} ("))
                .unwrap()
                .1
                .split_once("\n\n")
                .unwrap()
                .1;
            let old_lines: Vec<_> = old.lines().collect();
            let new_lines: Vec<_> = new.lines().collect();
            let mut old_cursor = 0;
            let mut new_cursor = 0;
            for line in detail.lines() {
                if line.starts_with("  ... ") || line.is_empty() {
                    continue;
                }
                let marker = line.as_bytes()[0];
                let content = &line[2..];
                for (excluded_marker, expected, cursor) in [
                    (b'+', &old_lines, &mut old_cursor),
                    (b'-', &new_lines, &mut new_cursor),
                ] {
                    if marker == excluded_marker {
                        continue;
                    }
                    let found = expected[*cursor..].iter().position(|row| *row == content);
                    assert!(
                        found.is_some(),
                        "{path}: reordered or invented row {line}\n{output}"
                    );
                    *cursor += found.unwrap() + 1;
                }
            }
        }
    }
}

#[tokio::test]
async fn callbacks_in_default_parameters_remain_in_the_complete_signature() {
    let fixture = Fixture::new();
    let signature = format!(
        "function run(\n    transform = (value) => {{\n{}        return value;\n    }},\n) {{\n",
        "        prepare(value);\n".repeat(30)
    );
    let old = format!("{signature}    return transform(0);\n}}\n");
    let new = old.replace("transform(0)", "transform(1)");
    let output = fixture.text("source.ts", &old, &new).await;
    for line in signature.lines() {
        assert!(output.contains(&format!("  {line}\n")), "{output}");
    }
    assert_eq!(
        output.matches("        prepare(value);").count(),
        30,
        "{output}"
    );
    let mut files = [fixture.file("source.ts", "", &new).await];
    let full = render(&mut files, &fixture.syntax, 20_000).await;
    let limit = full.find("+         prepare").unwrap() + 10;
    let limited = render(&mut files, &fixture.syntax, limit).await;
    assert!(!limited.contains("+ function run("), "{limited}");
}

#[tokio::test]
async fn inserted_overloads_do_not_reassign_existing_method_budgets() {
    let fixture = Fixture::new();
    let method = |parameter: &str, operation: &str, value: usize| {
        format!(
            "    void Run({parameter} value) {{\n{}    }}\n",
            format!("        {operation}({value});\n").repeat(12)
        )
    };
    let old = format!(
        "class Runner {{\n{}{} }}\n",
        method("int", "first", 0),
        method("string", "second", 0)
    );
    let new = format!(
        "class Runner {{\n{}{}{} }}\n",
        method("bool", "before", 1),
        method("int", "first", 1),
        method("string", "second", 1)
    );
    let output = fixture.text("source.cs", &old, &new).await;
    for operation in ["first", "second"] {
        assert_eq!(
            additions(&output)
                .iter()
                .filter(|line| line.contains(&format!("{operation}(1)")))
                .count(),
            8,
            "{operation}: {output}"
        );
    }
}

#[tokio::test]
async fn rust_attributes_stay_attached_to_each_function_signature() {
    let fixture = Fixture::new();
    let source = (0..30)
        .map(|position| {
            format!("#[cfg(test)]\n#[test]\nfn case_{position}() {{\n    step_{position}();\n}}\n")
        })
        .collect::<String>();
    let output = fixture.text("source.rs", "", &source).await;
    assert_eq!(
        additions(&output),
        source
            .lines()
            .filter(|line| line.starts_with('#') || line.starts_with("fn "))
            .collect::<Vec<_>>(),
        "{output}"
    );
    assert_eq!(
        output.matches("... 2 lines (+2 -0)").count(),
        30,
        "{output}"
    );
}

#[tokio::test]
async fn attribute_or_body_edits_retain_the_whole_annotated_signature() {
    let fixture = Fixture::new();
    let signature = "/// Tests the operation.\n#[cfg(unix)]\n#[tokio::test]\nasync fn run(\n    value: i32,\n) -> i32 {\n";
    let source = format!("{signature}{}    value\n}}\n", "    // padding\n".repeat(8));
    for updated in [
        source.replace("cfg(unix)", "cfg(windows)"),
        source.replace("    value\n", "    value + 1\n"),
    ] {
        let output = fixture.text("source.rs", &source, &updated).await;
        for line in signature.lines().filter(|line| !line.contains("cfg(unix)")) {
            assert_eq!(
                output.matches(&format!("  {line}\n")).count(),
                1,
                "{output}"
            );
        }
        assert!(output.contains("#[cfg("), "{output}");
        if updated.contains("    value + 1\n") {
            assert!(output.contains("-     value\n"), "{output}");
            assert!(output.contains("+     value + 1\n"), "{output}");
            assert_eq!(output.matches("  // padding").count(), 2, "{output}");
        }
    }
}

#[tokio::test]
async fn rust_tests_use_the_same_lifecycle_rules_as_other_functions() {
    let fixture = Fixture::new();
    for attribute in [
        "#[test]",
        "#[tokio::test]",
        "#[tokio::test(flavor = \"multi_thread\", worker_threads = 2)]",
        "#[tokio :: test]",
    ] {
        let source = format!(
            "{attribute}\nasync fn case() -> Result<(), Error> {{\n    fn nested_helper() {{\n        hidden_body();\n    }}\n    let callback = || {{\n        hidden_callback();\n    }};\n    hidden_body();\n    Ok(())\n}}\n"
        );
        for (old, new) in [("", source.as_str()), (source.as_str(), "")] {
            let output = fixture.text("source.rs", old, new).await;
            assert!(output.contains(attribute), "{output}");
            assert!(
                output.contains("async fn case() -> Result<(), Error> {"),
                "{output}"
            );
            assert!(!output.contains("nested_helper"), "{output}");
            assert!(!output.contains("hidden_"), "{output}");
            assert!(!output.contains("Ok(())"), "{output}");
            assert!(
                !output.lines().any(|line| line == "+ }" || line == "- }"),
                "{output}"
            );
            assert!(output.contains("... 9 lines ("), "{output}");
        }
        let updated = source.replace("hidden_body();", "replacement_body();");
        let output = fixture.text("source.rs", &source, &updated).await;
        assert!(
            output.contains("  async fn case() -> Result<(), Error> {"),
            "{output}"
        );
        assert!(output.contains("-     hidden_body();\n"), "{output}");
        assert!(output.contains("+     replacement_body();\n"), "{output}");
        assert!(
            output.contains("+         replacement_body();\n"),
            "{output}"
        );
    }
}

#[tokio::test]
async fn inline_rust_tests_clip_body_bytes_after_the_opening_brace() {
    let fixture = Fixture::new();
    for source in [
        "#[test]\nfn case() { hidden_body(); }\n",
        "#[tokio::test] async fn case() { hidden_body(); }\n",
        "#[test]\nfn case(\n    value: &str,\n) { hidden_body(); }\n",
    ] {
        for (old, new) in [("", source), (source, "")] {
            let output = fixture.text("source.rs", old, new).await;
            assert!(output.contains("fn case("), "{output}");
            assert!(output.contains(" {\n"), "{output}");
            assert!(!output.contains("hidden_body"), "{output}");
            assert!(!output.contains("; }"), "{output}");
        }
    }
}

#[tokio::test]
async fn test_attributes_do_not_change_body_selection() {
    let fixture = Fixture::new();
    let source = "#[cfg(test)]\nmod tests {\n    #[test]\n    fn case() {\n        hidden_body();\n    }\n    fn helper() {\n        visible_helper();\n    }\n}\n// #[test]\n#[test_case(1)]\nfn ordinary() {\n    visible_ordinary();\n}\n#[other::test]\nfn custom() {\n    visible_custom();\n}\n";
    let output = fixture.text("source.rs", "", source).await;
    assert!(!output.contains("hidden_body"), "{output}");
    for body in [
        "visible_helper();",
        "visible_ordinary();",
        "visible_custom();",
    ] {
        assert!(!output.contains(body), "{output}");
    }
    let updated = source
        .replace("visible_", "changed_")
        .replace("hidden_body", "changed_test_body");
    let output = fixture.text("source.rs", source, &updated).await;
    for body in [
        "changed_helper();",
        "changed_ordinary();",
        "changed_custom();",
    ] {
        assert!(output.contains(body), "{output}");
    }
    assert!(output.contains("-         hidden_body();\n"), "{output}");
    assert!(
        output.contains("+         changed_test_body();\n"),
        "{output}"
    );
}

#[tokio::test]
async fn modified_rust_test_bodies_keep_the_twenty_changed_line_limit() {
    let fixture = Fixture::new();
    for attribute in ["#[test]", "#[tokio::test(flavor = \"current_thread\")]"] {
        let source = format!("{attribute}\nasync fn case() {{\n{}}}\n", (0..15)
            .map(|index| format!("    check_{index}(0);\n    // stable {index} first\n    // stable {index} second\n    // stable {index} third\n"))
            .collect::<String>());
        let updated = source.replace("(0);", "(1);");
        let output = fixture.text("tests/cases.rs", &source, &updated).await;
        assert_eq!(
            output
                .lines()
                .filter(|line| (line.starts_with("+ ") || line.starts_with("- "))
                    && line.contains("check_"))
                .count(),
            20,
            "{output}"
        );
        assert!(output.contains("+     check_9(1);\n"), "{output}");
        assert!(!output.contains("check_10(1)"), "{output}");
    }
}

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
async fn added_and_removed_nested_functions_do_not_hide_modified_parent_bodies() {
    let fixture = Fixture::new();
    for (path, before, after, nested_signature) in [
        (
            "source.rs",
            "fn outer() {\n    existing(0);\n}\n",
            "fn outer() {\n    fn inner() {\n        hidden_body();\n    }\n    existing(1);\n}\n",
            "fn inner() {",
        ),
        (
            "source.ts",
            "function outer() {\n    existing(0);\n}\n",
            "function outer() {\n    function inner() {\n        hidden_body();\n    }\n    existing(1);\n}\n",
            "function inner() {",
        ),
        (
            "source.lua",
            "local function outer()\n    existing(0)\nend\n",
            "local function outer()\n    local function inner()\n        hidden_body()\n    end\n    existing(1)\nend\n",
            "local function inner()",
        ),
        (
            "source.py",
            "def outer():\n    existing(0)\n",
            "def outer():\n    def inner():\n        hidden_body()\n        another_hidden_body()\n    existing(1)\n",
            "def inner():",
        ),
    ] {
        for (old, new) in [(before, after), (after, before)] {
            let output = fixture.text(path, old, new).await;
            assert!(output.contains(nested_signature), "{path}: {output}");
            assert!(!output.contains("hidden_body"), "{path}: {output}");
            assert!(
                output.contains("existing(0)") && output.contains("existing(1)"),
                "{path}: {output}"
            );
        }
        for (old, new) in [("", after), (after, "")] {
            let output = fixture.text(path, old, new).await;
            assert!(!output.contains(nested_signature), "{path}: {output}");
            assert!(
                !output.contains("hidden_body") && !output.contains("existing(1)"),
                "{path}: {output}"
            );
        }
    }
}

#[tokio::test]
async fn rewritten_functions_keep_bodies_and_non_function_declarations_keep_fields() {
    let fixture = Fixture::new();
    for (path, old, new) in [
        (
            "source.rs",
            "fn run(value: i32) -> i32 {\n    old_body(value)\n}\n",
            "fn run(value: i64) -> i64 {\n    new_body(value)\n}\n",
        ),
        (
            "source.ts",
            "function run(value: number) {\n    return old_body(value);\n}\n",
            "function run(value: string) {\n    return new_body(value);\n}\n",
        ),
        (
            "source.py",
            "def run(value: int):\n    return old_body(value)\n",
            "def run(value: str):\n    return new_body(value)\n",
        ),
    ] {
        let output = fixture.text(path, old, new).await;
        assert!(
            output.contains("old_body(value)") && output.contains("new_body(value)"),
            "{path}: {output}"
        );
    }
    for (path, source) in [
        ("source.rs", "struct Config {\n    visible_field: i32,\n}\n"),
        (
            "source.ts",
            "interface Config {\n    visible_field: number;\n}\n",
        ),
        ("source.py", "class Config:\n    visible_field = 1\n"),
    ] {
        for (old, new) in [("", source), (source, "")] {
            let output = fixture.text(path, old, new).await;
            assert!(output.contains("visible_field"), "{path}: {output}");
        }
    }
}

#[tokio::test]
async fn ambiguous_overload_rewrites_keep_bodies_without_guessing_additions_or_removals() {
    let fixture = Fixture::new();
    let old = "class Runner {\n    void Run(int value) {\n        old_first();\n    }\n    void Run(string value) {\n        old_second();\n    }\n}\n";
    let new = "class Runner {\n    void Run(bool value) {\n        new_first();\n    }\n    void Run(float value) {\n        new_second();\n    }\n}\n";
    let output = fixture.text("source.cs", old, new).await;
    for body in [
        "old_first();",
        "old_second();",
        "new_first();",
        "new_second();",
    ] {
        assert!(output.contains(body), "{output}");
    }
}

#[tokio::test]
async fn shared_diff_context_does_not_change_function_lifecycle_classification() {
    let fixture = Fixture::new();
    let before = "fn run() {\n    previous();\n}\nfn removed() {\n    hidden_body();\n    another_hidden_body();\n}\n";
    let after = "fn run() {\n    modified();\n}\n";
    let removed = fixture.text("source.rs", before, after).await;
    assert!(removed.contains("- fn removed() {\n"), "{removed}");
    assert!(!removed.contains("hidden_body"), "{removed}");
    assert!(removed.contains("+     modified();\n"), "{removed}");
    assert!(removed.contains("  }\n"), "{removed}");
    let added = fixture.text("source.rs", after, before).await;
    assert!(added.contains("+ fn removed() {\n"), "{added}");
    assert!(!added.contains("hidden_body"), "{added}");
    assert!(added.contains("+     previous();\n"), "{added}");
}

#[tokio::test]
async fn license_and_lockfile_changes_keep_counts_without_excerpts() {
    let fixture = Fixture::new();
    for path in [
        "LICENSE",
        "legal/license",
        "LICENSE.md",
        "LICENSE.txt",
        "LICENSE.rst",
        "Cargo.lock",
        "yarn.lock",
        "uv.lock",
        "poetry.lock",
        "Pipfile.lock",
        "bun.lockb",
        "composer.lock",
        "Gemfile.lock",
        "pubspec.lock",
        "mix.lock",
        "deno.lock",
        "flake.lock",
        "conan.lock",
        "gradle.lockfile",
        "packages.lock.json",
        "paket.lock",
        "renv.lock",
        "opam.locked",
        "custom.lock.backup",
        "nested\\CARGO.LOCK",
        "package-lock.json",
        ".package-lock.json",
        "pnpm-lock.yaml",
        "npm-shrinkwrap.json",
        "Package.resolved",
        "pylock.toml",
        "pylock.production.toml",
        "conda-lock.yml",
        "dev.conda-lock.yaml",
        "cabal.project.freeze",
        "custom.project.freeze",
        "Manifest.toml",
        "JuliaManifest.toml",
        "Manifest-v1.11.toml",
    ] {
        for (old, new, counts) in [
            ("before\n", "inventory_hidden_marker\n", "+1 -1"),
            ("", "inventory_hidden_marker\n", "+1 -0"),
            ("inventory_hidden_marker\n", "", "+0 -1"),
        ] {
            let output = fixture.text(path, old, new).await;
            assert!(
                output.contains(&format!("  {path} ({counts})\n")),
                "{path}: {output}"
            );
            assert!(!output.contains(&format!("\n{path} (")), "{path}: {output}");
            assert!(
                !output.contains("inventory_hidden_marker"),
                "{path}: {output}"
            );
        }
    }
}

#[tokio::test]
async fn lock_directories_and_similar_source_names_do_not_hide_changes() {
    let fixture = Fixture::new();
    for path in [
        "license.rs",
        "licenses.md",
        "LICENSE/source.rs",
        "cache.lock/source.py",
        "lock.rs",
        "package.json",
        "pyproject.toml",
        "Project.toml",
        "Manifest-view.toml",
        "pylock.production.example.toml",
        "pylock..toml",
        "package-lock.json.rs",
        "go.sum",
    ] {
        let output = fixture.text(path, "before\n", "visible_marker\n").await;
        assert!(output.contains("+ visible_marker\n"), "{path}: {output}");
    }
}

#[tokio::test]
async fn ignored_lockfile_excerpts_leave_the_byte_budget_for_other_changes() {
    let fixture = Fixture::new();
    let mut files = [
        fixture.file("a.lock", "", &"ignored\n".repeat(2000)).await,
        fixture.file("z.txt", "before\n", "retained\n").await,
    ];
    let output = render(&mut files, &fixture.syntax, 1000).await;
    assert!(output.contains("  a.lock (+2000 -0)\n"), "{output}");
    assert!(!output.contains("+ ignored\n"), "{output}");
    assert!(output.contains("+ retained\n"), "{output}");
    assert!(output.len() <= 1000);
}

#[tokio::test]
async fn grammar_rule_separators_do_not_produce_blank_only_gap_markers() {
    let fixture = Fixture::new();
    let source = format!(
        "const rules = {{\n{}\n}};\n",
        (0..30)
            .map(|index| format!("    rule_{index}: $ => seq($.identifier),\n\n \t\n"))
            .collect::<String>()
            .trim_end()
    );
    let output = fixture.text("grammar.js", "", &source).await;
    for index in 0..30 {
        assert!(
            output.contains(&format!("+     rule_{index}: $ =>\n")),
            "{output}"
        );
    }
    assert!(!output.contains("  ... "), "{output}");
}

#[tokio::test]
async fn single_line_gaps_display_added_removed_and_unchanged_content() {
    let fixture = Fixture::new();
    let source = (0..21)
        .map(|index| format!("entry_{index}\n"))
        .collect::<String>();
    for (old, new, marker) in [("", source.as_str(), '+'), (source.as_str(), "", '-')] {
        let output = fixture.text("source.unknown", old, new).await;
        for line in source.lines() {
            assert!(output.contains(&format!("{marker} {line}\n")), "{output}");
        }
        assert!(!output.contains("  ... "), "{output}");
    }
    let old = "fn run() {\n    first(0);\n    before_one();\n    before_two();\n    retained_middle();\n    after_two();\n    after_one();\n    last(0);\n}\n";
    let new = old
        .replace("first(0)", "first(1)")
        .replace("last(0)", "last(1)");
    let output = fixture.text("source.rs", old, &new).await;
    assert!(output.contains("      retained_middle();\n"), "{output}");
    assert!(!output.contains("  ... "), "{output}");
}

#[tokio::test]
async fn single_line_gap_cannot_overflow_the_global_byte_limit() {
    let fixture = Fixture::new();
    let source = format!("{}{}\n", "short\n".repeat(20), "large".repeat(2000));
    let mut files = [
        fixture.file("a.unknown", "", &source).await,
        fixture.file("z.txt", "", "later\n").await,
    ];
    let output = render(&mut files, &fixture.syntax, 1000).await;
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
    let full = render(&mut files, &fixture.syntax, 20_000).await;
    let old_signature = "- fn target(value: i32) -> i32 {\n";
    let new_signature = "+ fn target(\n+     value: i64,\n+ ) -> i64 {\n";
    assert!(
        full.contains(&format!("{old_signature}{new_signature}")),
        "{full}"
    );
    let limit = full.find("+     value: i64,").unwrap() + 5;
    let limited = render(&mut files, &fixture.syntax, limit).await;
    assert!(limited.contains("+ fn helper() {\n"), "{limited}");
    assert!(
        !limited.contains(old_signature) && !limited.contains("+ fn target("),
        "{limited}"
    );
    assert!(limited.len() <= limit);
}

#[tokio::test]
async fn nested_signature_pairs_leave_enclosing_body_rows_in_order() {
    let fixture = Fixture::new();
    let old = "fn outer() {\n    before(0);\n    fn inner(value: i32) -> i32 {\n        previous(value)\n    }\n    after(0);\n}\n";
    let new = "fn outer() {\n    before(1);\n    fn helper() {\n        first();\n        second();\n    }\n    fn inner(\n        value: i64,\n    ) -> i64 {\n        updated(value)\n    }\n    after(1);\n}\n";
    let output = fixture.text("source.rs", old, new).await;
    let pair = "-     fn inner(value: i32) -> i32 {\n+     fn inner(\n+         value: i64,\n+     ) -> i64 {\n";
    let before = output.find("+     before(1);").unwrap();
    let helper = output.find("+     fn helper() {").unwrap();
    let inner = output.find(pair).unwrap_or_else(|| panic!("{output}"));
    let body = output.find("+         updated(value)").unwrap();
    let after = output.find("+     after(1);").unwrap();
    assert!(
        before < helper && helper < inner && inner < body && body < after,
        "{output}"
    );
}

#[tokio::test]
async fn same_named_methods_pair_signatures_with_their_own_class() {
    let fixture = Fixture::new();
    let mut old = String::new();
    let mut new = String::new();
    for name in ["First", "Second"] {
        old.push_str(&format!("class {name} {{\n    int Run(int value) {{\n        return previous(value);\n    }}\n}}\n"));
        new.push_str(&format!("class {name} {{\n    void Helper() {{\n        first();\n        second();\n    }}\n    long Run(\n        long value\n    ) {{\n        return updated(value);\n    }}\n}}\n"));
    }
    let output = fixture.text("source.cs", &old, &new).await;
    let pair = "-     int Run(int value) {\n+     long Run(\n+         long value\n+     ) {\n";
    assert_eq!(output.matches(pair).count(), 2, "{output}");
    for (name, next) in [("First", Some("Second")), ("Second", None)] {
        let start = output.find(&format!("class {name} {{")).unwrap();
        let end = next.map_or(output.len(), |next| {
            output.find(&format!("class {next} {{")).unwrap()
        });
        assert_eq!(output[start..end].matches(pair).count(), 1, "{output}");
    }
}
