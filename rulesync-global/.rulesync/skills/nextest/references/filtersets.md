# Filtersets

*When to read this:* When writing a `-E`/`--filterset` expression, a `default-filter`, or the `filter` key of a per-test override or profile script.

Filtersets are nextest's set-algebra DSL for selecting tests. The same grammar appears on the command line (`-E`), in `default-filter`, and in every `filter` key of `.config/nextest.toml`. `cargo nextest help filterset` prints the reference for the installed version.

## Contents
- Predicates
- Name matchers and defaults
- Operators and precedence
- Escape sequences
- Combining filters on the command line
- Restrictions in configuration

## Predicates

| Predicate | Selects |
| --- | --- |
| `all()` | Every test. |
| `none()` | No tests. |
| `test(matcher)` | Tests whose name matches. |
| `package(matcher)` | Tests in matching packages (crates). |
| `deps(matcher)` | Tests in matching crates and all their transitive dependencies. |
| `rdeps(matcher)` | Tests in matching crates and all crates that transitively depend on them. |
| `kind(matcher)` | Tests in binaries of a Cargo kind: `lib`, `test`, `bench`, `proc-macro`, `bin`, `example`. |
| `binary(matcher)` | Tests in matching binary names. A unit-test binary is named after its crate; other binaries carry their target name. |
| `binary_id(matcher)` | Tests in matching binary IDs, such as `my-crate::integration`. Covers `package()`, `kind()`, and `binary()` together. |
| `platform(host)` / `platform(target)` | Tests built for the host or the target platform. |
| `default()` | The profile's `default-filter` set (since 0.9.75). |
| `group(matcher)` | Tests in matching test groups; `@global` names tests in no group (since 0.9.133, command line only). |

A filterset that always excludes a binary keeps nextest from executing that binary even to list its tests. `cargo nextest list -E 'platform(host)'` therefore never runs target-platform binaries. To list them anyway without running their tests, add a `test()` term: `-E 'platform(host) + not test(/.*/)'`.

## Name matchers and defaults

| Syntax | Matcher |
| --- | --- |
| `=string` | Equality. |
| `~string` | Substring. |
| `/regex/` | Regex that matches any part of the name; anchor with `/^...$/` for a whole-name match. Uses the `regex` crate. |
| `#glob` | Glob over the full name. Uses `globset`; match a literal `*` or `?` with `[*]` or `[?]`. |
| `string` | The predicate's default matcher. |

The default matcher depends on the predicate. `test()` defaults to substring. `package()`, `deps()`, `rdeps()`, `binary()`, `binary_id()`, and `group()` default to glob. `kind()` and `platform()` default to equality. Always write an explicit prefix in generated expressions, because an unprefixed string changes meaning between predicates.

Test names include their module path, such as `parser::tests::rejects_empty`. `test(=parser::tests::rejects_empty)` selects exactly one test, and `test(parser::)` selects every test in the module.

## Operators and precedence

| Operator | Meaning |
| --- | --- |
| `a & b`, `a and b` | Intersection. |
| `a \| b`, `a + b`, `a or b` | Union. |
| `not a`, `!a` | Complement. |
| `a - b` | Difference; equivalent to `a and not b`. |
| `(a)` | Grouping. |

Binding runs from tightest to loosest as `()`, then `not`/`!`, then `and`/`&`/`-`, then `or`/`|`/`+`. Operators of equal precedence associate left to right. `test(a) | test(b) & test(c)` therefore means `test(a) | (test(b) & test(c))`, and `not test(a) | test(b)` means `(not test(a)) | test(b)`.

## Escape sequences

Equality, substring, and glob matchers accept `\n`, `\r`, `\t`, `\\`, `\/`, `\)`, `\,`, and `\u{XXXXXX}`; every other backslash sequence is an error. Regex matchers accept the `regex` crate's escapes, including classes such as `\d`, and additionally read `\/` as a literal slash.

## Combining filters on the command line

Positional name filters (`cargo nextest run parser` or `cargo nextest run -- parser`) match substrings. `--skip` and `--exact` emulate `cargo test` and apply only to name filters passed after `--`. Each has a filterset equivalent:

| `cargo test` form | Filterset form |
| --- | --- |
| `-- --skip skip1 --skip skip2 test3` | `-E 'test(test3) - test(/skip[12]/)'` |
| `-- test1 test2 --exact` | `-E 'test(=test1) + test(=test2)'` |
| `-- --exact --skip slow_module::my_test` | `-E 'not test(=slow_module::my_test)'` |

Multiple `-E` flags form a union. When both filtersets and name filters appear, nextest intersects the union of filtersets with the union of name filters: `-E 'package(foo)' -- test_bar test_baz` runs tests in `foo` whose names contain `test_bar` or `test_baz`.

Every command-line selection, including `-E 'all()'`, is intersected with the profile's `default-filter` (since 0.9.77). Pass `--ignore-default-filter` to select outside it, such as `--ignore-default-filter -E 'test(=slow_case)'`. Nextest prints how many tests and binaries the default filter skipped.

Prefer `default-filter` over `#[ignore]` for tests that are slow or need special resources, because `default-filter` varies per profile and per platform while `#[ignore]` is global to the repository. Reserve `#[ignore]` for tests that must also stay out of `cargo test`.

Quote filtersets in single quotes in shells and in TOML literal strings (`filter = 'test(/^flaky::/)'`), so the shell and TOML do not consume backslashes or double quotes.

## Restrictions in configuration

Filtersets written in configuration, such as override `filter` keys, profile-script `filter` keys, and `default-filter` itself, ignore the default filter. Add `default()` to an override or script filter to restrict it to the default set. `default()` cannot appear inside `default-filter`, and `group()` works only on the command line.
