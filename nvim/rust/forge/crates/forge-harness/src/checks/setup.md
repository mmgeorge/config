# Project checks setup

Inspect project manifests, existing scripts, CI, test runners, and repository instructions.
For an empty directory, inspect the active plan's proposed tooling before choosing commands.
Discovery must be followed by a user conversation. Call harness_question_ask with one to three
concrete questions before saving. Propose discovered functional areas and ask which suites run
automatically, which remain explicit, and whether routine verification uses targeted or full
coverage. Ask about acceptable cost when it changes that choice. Do not invent the user's
preferences. Configuration publication is rejected until the setup exchange receives a user
answer. Continue the same setup exchange after the answers, including after host recovery.
Present the proposed areas, triggers, coverage, dependencies, and expected cost before saving.
For a narrow edit, focus the questions on changed decisions rather than repeating settled ones.
Configure checks only through harness_checks_configure. Read harness_checks_list first for the
current revision, definitions, overrides, and history. Preserve existing project settings and
checks that remain useful. Do not run configured checks during setup.

Categories are format, lint, check, test, and build. Unit, integration, screenshot, accessibility,
and other suites are named test definitions, not new categories. Format commands verify without
rewriting files. Build is optional and defaults to development unless the project explicitly
requires another profile. Enabled checks run automatically. Disabled checks cannot be run by
the model until the user enables them. Task overrides remain separate from saved defaults.

Configuration is {version: 2, definitions: [...]}. Each definition has id, name, category, area,
and a required trigger independent of its scope. Area is a descriptive functional grouping.
Trigger is {kind: "any_change"}, {kind: "manual"}, or
{kind: "paths", include: ["src/**", "Cargo.toml"], exclude: ["src/generated/**"]}.
Paths are project-relative and case-sensitive. `*` and `?` match within a path component, and
whole-component `**` matches zero or more components. No other glob syntax is supported.
Use explicit source, manifest, lockfile, generator-input, and shared-dependency paths. Areas
may overlap. Any-change triggers require at least one actual task change. Manual triggers
run only when explicitly requested or required by a selected check. A full-suite scope does
not select the check for unrelated changes. Task checkpoints include additions, deletions,
and both rename paths while excluding pre-existing changes. Never substitute all dirty files.
Changed-case defaults use the accepted plan's added/modified test inventory. Standalone runs
do not discover this inventory automatically. For routine source-driven coverage, configure
changed_files with a helper that maps source files to real tests, or all within the affected area.
A cases handler can still support explicit case selection. Use changed_cases defaults only
for checks intentionally restricted to cases added or modified by an accepted plan.
Each definition also has
enabled, scope (all, changed_files, changed_cases), directory (project-relative, default "."),
all: {command}, optional files: {command}, optional cases: {command}, and depends_on: [id].
Use static single-line commands for the configured shell. Choose execution order in definitions.
Prefer build/check, test, lint, then format when no project constraint requires another order.
Declare dependencies only when a check truly requires a successful prerequisite. Independent
checks continue after failures. An explicitly empty definitions list is valid for projects
without automated checks. Do not introduce dummy successful commands.

Scoped handlers read one JSON object from stdin:
{version: 1, check_id, workspace, selection: {kind: "files", files: [...]}
or {kind: "cases", cases: [{file, name}, ...]}, receipt_path}.
Resolve against actual files or the runner's test discovery. Write a receipt to receipt_path:
{version: 1, selection: {kind: "files", files: [actual selected files]}}
or {version: 1, selection: {kind: "cases", cases: [{file, name}, ...]}}.
Scoped handlers receive only trigger-matching task files or cases. Configure scoped coverage
only when the runner can prove coverage for those source paths. Otherwise select the full
suite within its affected area. Do not assume that every changed source path is a test file.
Only record coverage actually selected. Empty, broader, unsupported, or missing selections are
hard configuration errors, never successful test coverage. Do not silently run all tests as a
fallback. Forward the underlying runner's output and exit status. Helper files must be Nushell
under .forge/checks/, use project-relative forward slash paths, and work on Windows and macOS.
Pass helper source and expected_digest (null for a new file) to the configuration tool.
Define scoped helpers with `def main [] { ... }` and read the request with
`let request = ($in | from json)` inside main. Launch them with
`nu --no-config-file --stdin .forge/checks/<name>.nu` so they receive stdin consistently.

For Cargo, use the project's selected toolchain, profile, features, target directory, inherited
configuration, and compiler flags consistently. Prefer development artifacts unless repository
instructions require release. Do not add a release build for ordinary verification. Avoid
redundant cargo check when Clippy covers the same configuration, duplicate test invocations,
unnecessary separate builds, and cargo clean. Keep compiler and lint flags stable. --locked
requires an existing maintained Cargo.lock and does not prevent build-cache reuse. Tests and
Clippy still require their own test executables and checking artifacts. Inspect existing scripts
before selecting replacements. Configure file/case helpers when the runner does not accept files
directly. Do not claim a scope that the handler cannot implement.
