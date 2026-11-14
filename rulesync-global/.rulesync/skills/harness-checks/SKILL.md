---
name: harness-checks
description: >-
  Configure Forge Harness project checks, category adapters, automatic triggers,
  and custom command inputs through check_definition_edit. Use when adding,
  editing, or repairing .forge.json checks or managed .forge/checks/ helpers.
  Do not use for merely running checks or changing task-specific selection.
targets:
  - '*'
---

# Harness Checks

Configure checks through `check_definition_edit` when check editing is enabled. Preserve unrelated definitions and settings. Every submission requires review outside YOLO. Configuration publication does not execute checks or validate helper-language syntax.

## Inspect and publish

1. Read `check_definition_list` for the current revision, compiled `adapters` catalog, live automatic-check policy, and `check_editing` permission. Read `check_definition_details` with `check_id` for every existing definition being preserved or edited; the list omits complete commands and adapter options. If configuration is invalid, inspect the saved definitions before replacing them.
2. Inspect repository manifests, scripts, and tool versions. Choose an explicit category: `typecheck`, `test`, `format`, `lint`, `build`, or `custom`. There is no default category; `check` is obsolete.
3. For a contract category, choose a built-in adapter whose catalog `categories` include the category and whose `use_when` fits the project. Use the catalog's script adapter when no built-in adapter fits. Read its options schema and protocol documentation; do not invent adapter names, options, supported operations, or minimum tool versions.
4. Submit `expected_revision`, the complete intended `config` map, and any managed `helpers`. A helper includes its inspected `expected_digest`, or null for a new file. If a revision or digest changed, read the current contents and reconcile before resubmitting. Helpers must live under `.forge/checks/`.
5. Inspect the returned revision and changed names, then reread affected definitions. Validate helper syntax separately and exercise the configured argv through Harness. A successful configuration edit is not execution evidence.

## Categories and adapters

Use `action.adapter` for structured contract results and `action.command` for `custom` checks. Never supply both. Adapter checks reject `inputs`, callback `input`, and `prepare`; format and lint mutation belongs to their supported apply operation. Keep `trigger`, `directory`, `label`, and `after` as explicit configuration.

Get the concrete adapter configuration from the running build's catalog. Do not copy a remembered built-in configuration: installed adapters and their options can differ between builds. A repository script must implement the declared category's protocol, including selection validation, streaming results, and completion. A script adapter starts at the repository root and receives the check directory in its request. For Nushell, include `--stdin` and `--no-config-file` in the configured argv so the JSON request reaches `$in`.

Use `custom` for commands judged by exit status that fit no category contract, such as a configuration preview. A custom action can declare typed `inputs`: each input has `type` and `description`, optional `required` and type-correct `default`, and a supported contextual `source`. A complete argv element `{name}` substitutes a scalar or splices an array of scalars; embedded `prefix-{name}` remains literal. Custom commands receive closed stdin. The removed `input: true` callback mode is rejected; callback receipts and generic file reports are no longer read.

<details>
<summary>Staged migration of existing command checks</summary>

Some builds still accept commands in contract categories while those adapters are being delivered. Preserve accepted existing command definitions until their category adapter is available; this is a rollout exception, not the final contract. In this repository, the initial migration consolidates the Lua checks into one test adapter, puts non-test commands in `custom`, and retains the Rust test command until its built-in adapter is delivered. Keep legacy plan-selection consumers until plan integration replaces them. Report unsupported settings explicitly instead of discarding authored inputs or resetting the configuration.

</details>

## Execution and verification

Read the running tool schemas before selecting work. Test adapters accept test item IDs and affected paths; discovery supplies opaque IDs that must be passed back unchanged. Use static discovery for inspection that must not build or execute test binaries. Exact discovery can build and requires execution authority. A discovery result is not a test pass.

An explicit all request is mutually exclusive with a selection. Do not turn a missing baseline, unknown ID, or empty targeted result into a whole-suite run. Where supported, path-category checks take paths and format/lint checks expose apply; inspect advertised capabilities before requesting an operation. After a source-changing operation, rerun verification for the resulting workspace.

`check_run` returns a run ID. Poll `check_status` with that ID and an allowed `wait_ms`; use `check_detail` after completion for invocation metadata, logs, and supported structured result pages. Follow returned cursors with the same ID and stream. `check_cancel` requests cancellation; wait for terminal status before claiming collection. Preserve actual execution and I/O errors.

Configure compiler/type validation before tests, formatting verification, lint, and build. Preserve the project's compiler, profile, features, target selection, and artifact directory across stages. A later source fix requires verification again. In this dotfiles repository, use Nushell for shared scripts and validate their syntax before running them.

`trigger.auto` defaults to false. Trigger paths are repository-relative globs; exclusions remove matches. Triggers select checks, not package names or runner arguments. `after` declares required successful dependencies. Do not assume category ordering substitutes for a dependency.
