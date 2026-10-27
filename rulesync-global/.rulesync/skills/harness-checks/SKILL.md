---
name: harness-checks
description: >-
  Configure Forge Harness project checks, automatic triggers, typed command inputs,
  and repository-owned callbacks through check_definition_edit. Use when adding, editing,
  or repairing .forge.json checks or managed .forge/checks/ helpers. Do not use for
  merely running existing checks or changing task-specific check selection.
targets:
  - '*'
---

# Harness Checks

Configure checks only through `check_definition_edit` when check editing is enabled.
Read `check_definition_list` for the current revision, definitions, and live automatic-check
policy. Preserve unrelated configuration. Every submission requires review outside
YOLO. Configuration does not execute checks or validate helper-language syntax.
The list contains input descriptions but omits full commands, history, and previews.
Read `check_definition_details` with `check_id` for each existing definition that
must be preserved or edited. Publish the complete intended definition map, not a
partial replacement. Successful edits return the new revision and changed names.

Inspect repository scripts and manifests before choosing commands. The check
executor treats all commands as opaque. It never detects Cargo, npm, a language,
or a runner, never performs test discovery, and never adds runner flags.

## Declared inputs and templates

Checks map names to `trigger` and `action`. A command array starts with a fixed
executable. A string runs through the configured shell.

```json
{
  "unit": {
    "trigger": {"auto": false},
    "action": {
      "command": ["cargo", "test", "--package", "{package}", "--lib", "--", "{test}", "--exact"],
      "category": "test",
      "inputs": {
        "package": {"type": "string", "description": "Package containing the test", "required": true},
        "test": {"type": "string", "description": "Exact library test name", "required": true}
      }
    }
  }
}
```

Call this check with `{"check":"unit","inputs":{"package":"engine","test":"tests::example"}}`.
`package` and `test` are ordinary names declared by this check. No package manager
field or reserved runner-specific selectors exist.

Each input declares `type` and `description`. Types are string, integer, number,
boolean, array, and object. `required` defaults to false. An optional `default`
must match the type. Optional automatic `source` bindings are `changed_files`
(array), `planned_cases` (array), and `workspace` (string). Explicit values override
context sources and defaults. Missing required values and unknown inputs fail.

A complete argv element `{name}` substitutes a scalar or splices an array of
scalars. Other text stays literal, including `prefix-{name}` and JSON arguments.
No shell interpolation or implicit per-element fan-out occurs. Use a callback
for conditionals, multiple commands, object inputs, or source-to-package mapping.
Shell-string commands receive declared inputs through stdin, not interpolation.

## Repository callbacks

Set `input: true` to send JSON on stdin. The callback uses the same declared input
contract as a template. Example action:

```json
{
  "command": ["nu", "--no-config-file", "--stdin", ".forge/checks/test.nu"],
  "input": true,
  "category": "test",
  "inputs": {
    "files": {"type": "array", "description": "Changed repository paths", "source": "changed_files"},
    "package": {"type": "string", "description": "Explicit package", "default": ""},
    "test": {"type": "string", "description": "Explicit test", "default": ""}
  }
}
```

The request contains `version: 1`, `check_id`, `workspace`, resolved `inputs`,
`changed_files`, `selection`, `receipt_path`, and `report_path`.
`selection` records full, changed-file, planned-case, or explicit-input context.
The callback owns runner arguments, any manifest lookup, test filtering,
zero-match detection, and failure interpretation. It must propagate nonzero exits
and preserve full diagnostics rather than returning success-shaped fallbacks.

On success write `{"version":1,"selection":<request.selection>}` to `receipt_path`.
Explicit-input receipts must match the requested input map exactly.
An optional report at `report_path` contains `version: 1` and a `result` array.
Each result has repository-relative `file`, terminal `state`, optional
`duration_ms`, `command`, and complete `output`. Translate runner output in the
callback, not in Harness. Malformed reports and reported failures cannot count
as success. Plain commands use their exit status without implicit coverage parsing.

Use Nushell for cross-platform callbacks in this dotfiles repository. Managed
helper files under `.forge/checks/` are published through `check_definition_edit` with
their inspected `expected_digest`, or null for a new file. Harness does not select
an interpreter from an extension or secretly run syntax validation. Configure
language-specific validation explicitly as another check.

## Triggers and execution

`trigger.auto` defaults to false. Paths are repository-relative globs, with `*`,
`?`, and `**`. Exclusions remove matching paths. Automatic triggers select checks,
not package names or runner arguments. Empty check maps are valid.

`action.directory` is repository-relative. Optional `label`, `category`, `prepare`,
and `after` control display, ordering, mutation, and prerequisites. Every command
defaults to category `check` unless explicitly configured. Use `prepare: true` for
source-changing commands. Dependencies express required success, not category order.

`check_run` accepts a check name or list, plus optional `inputs` or `all: true`.
These two options cannot be combined. Full context does not synthesize a broader
command or supply missing required inputs. Define full behavior in a callback or
separate saved check. Omitted inputs use configured defaults and automatic sources.

`check_run` immediately returns only `run_id`. Use `check_status` with that ID
and optional `wait_ms` (0 through 60000) to wait for completion. Status returns
compact progress and the latest nonempty output line, not full logs. `check_list`
returns those summaries for currently running IDs. Use `check_detail` only after
completion for exact invocation metadata and logs. A running detail request directs
the caller to wait with `check_status`. Follow `next_cursor` using the same ID and
stream to retrieve the complete log. `check_cancel` requests cancellation without
claiming process cleanup has finished. Unknown IDs and I/O errors remain failures.

Preserve the selected compiler, profile, flags, and artifact directory in the
repository command. Follow the global smallest-sufficient-coverage policy.
Never silently broaden a failed or empty targeted request.
