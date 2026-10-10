# Declaration formatting corpus

Formatting must preserve declaration structure and literal contents. Successful
parsing alone is insufficient: moving a tuple field into a line comment produces
valid Rust while deleting the field.

The tests in `src/syntax/declaration_formatting_tests.rs` exercise the production
`DeclarationOverview::format_with_width` and `present` entry points. A separate
tree walk compares syntax nodes and leaf contents before and after formatting.
It ignores comments and optional separators in the syntax comparison, then checks
comment words separately to allow paragraph reflow. String-literal whitespace is
significant. The oracle has mutation checks for a swallowed field and a changed
literal. It does not prove semantic equivalence or exhaust every grammar production.

Each source corpus runs as a complete file and as individual top-level
declarations, so an early failure does not hide later declaration families. A
second test expands each corpus to at least 1,000 lines to exercise whole-file
interactions. Tests use LF and CRLF, widths 40, 60, 80, 100, 120, and 240, and three
formatting passes. The second and third passes must be identical to the first.

| Corpus | Declaration coverage |
| --- | --- |
| Rust | Imports, constants, statics, aliases, lifetimes, const generics, named/unit/tuple structs, attributes, enum variants, traits, implementations, associated items, callbacks, async/const/unsafe functions, foreign declarations, nested modules, documentation fences and inline comments |
| TypeScript (`ts`, `mts`, `cts`) | Imports/re-exports, aliases, template/conditional/mapped/indexed types, labeled tuples, interfaces, call/construct/index signatures, overloads, enums, generic abstract classes, accessors, parameter properties, namespaces, ambient declarations, nested objects and documentation |
| TSX | Component signatures, generic arrows, function expressions, callback props, template literal attributes, abstract controllers, nested namespaces and JSX text in documentation |
| Lua | Literal imports, module fields, nested field paths, module/method/local function signatures, varargs, callbacks, LuaLS annotations, documentation fences and multiline parameters |
| Configuration | JSON, JSONC, TOML, YAML, XML and gitignore values, comments, escaping, multiline content and ordering are preserved byte for byte |

Admission regressions separately exercise Rust unions and `unsafe extern` blocks,
typed TypeScript function expressions, and Lua's parenthesis-free literal imports.
Those tests expose unsupported forms before formatting rather than silently
removing them from the corpus.

When adding a declaration language or a new supported declaration family, add
representative fixtures here and run the same preservation, repeated-pass, width,
newline and large-document checks. Include comments next to delimiters and fields,
nested types, visibility, attributes, and literal text for that grammar. Keep a
minimal regression beside the broad corpus for every discovered corruption.

Run from `nvim/rust/forge` using the shared release target directory:

```text
cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-diff --lib formatting_tests
```
