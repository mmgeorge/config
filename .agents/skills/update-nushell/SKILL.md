---
name: update-nushell
description: Update Nushell configuration in this repository, including interactive commands, fzf pickers, and documented command parameters.
allowed-tools: rg,glob,view,bash,apply_patch
---

# Update Nushell configuration

Use this skill when the user asks to add, modify, or troubleshoot Nushell configuration in this repository.

## Workflow

1. Inspect `nushell/config.nu` and nearby existing patterns before editing. Preserve user changes and keep edits surgical.
2. Prefer `def` over `alias` when the command needs flags, arguments, branching, external commands, or interactive buffer editing. Nu aliases are not appropriate for `commandline edit` workflows.
3. Use `commandline edit --insert` when a command should insert text into the current shell prompt instead of executing it immediately.
4. Use explicit external command calls such as `^fd` and `^fzf` when invoking external tools from Nu code.
5. Disable fzf previews for compact pickers with `--preview=""` unless the user explicitly wants a preview pane.
6. Document every user-facing positional parameter and flag in the Nu signature with inline comments so `help <command>` exposes the docs.
7. Validate Nushell syntax after edits, then verify that help output includes the parameter documentation.

## Parameter documentation pattern

Add comments directly after parameters and flags:

```nu
def test [
  mode?: string # Pass "run" to insert "pnpm vitest run --browser.headless" instead of "pnpm vitest".
  --all # Include spec files whose paths contain "3d" or "widget"; filtered out by default.
] {
  # command body
}
```

Check the rendered docs with:

```sh
nu --config /Users/matt9222/config/nushell/config.nu -c 'help test | str join (char nl)'
```

## Interactive fzf picker pattern

For a command that inserts a prefix, opens fzf, then inserts a selected token:

```nu
def test [
  mode?: string # Pass "run" to insert "pnpm vitest run --browser.headless" instead of "pnpm vitest".
  --all # Include spec files whose paths contain "3d" or "widget"; filtered out by default.
] {
  let prefix = if $mode == "run" { "pnpm vitest run --browser.headless " } else { "pnpm vitest " }
  commandline edit --insert $prefix

  let all_files = (^fd --type f --hidden --exclude .git --glob "*.spec.ts" | lines)
  let files = if $all {
    $all_files
  } else {
    $all_files | where {|path| $path !~ '(?i)(3d|widget)' }
  }

  let path = ($files | str join (char nl) | ^fzf --preview="" --height 40% --layout=reverse --border | str trim)
  if ($path != "") {
    commandline edit --insert $path
  }
}
```

When excluding names like `3d`, `3D`, or `widget`, filter the full path if directory names should be excluded too. Filtering only `path basename` will still allow matches from excluded directories.

## Validation

Use the Nu version installed in the environment. Do not use `nu --check`; this version does not support that flag.

For `config.nu`, run:

```sh
nu --ide-check 10 /Users/matt9222/config/nushell/config.nu >/tmp/nu-ide-check.out
rg '"type":"error"|"type":"diagnostic"' /tmp/nu-ide-check.out
```

The `rg` command should return no matches. `nu --ide-check` may emit type hints; hints are not failures.

For command documentation, run targeted `help <command>` checks and confirm parameter and flag descriptions appear under `Parameters:` and `Flags:`.
