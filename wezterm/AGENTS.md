# Agent Guide: WezTerm Extensions

This document captures the main lessons from building the worktree picker and
related WezTerm workflows in this repo. Use it when changing files under
`wezterm/`.

---

## Recommended structure

- Keep `wezterm/wezterm.lua` thin. It should mostly wire modules into:
  - `require(...)`
  - keybindings
  - event hooks
  - top-level config values
- Put stateful workflow logic in dedicated modules like `wezterm/worktree.lua`.
- If the live GUI is loading `~/.wezterm.lua` instead of the checked-in repo
  config, make the local file a thin loader over the repo config rather than
  duplicating the config in two places.
- Gate platform-specific settings so shared config is safe across macOS and
  Windows.

## Overlay sequencing

WezTerm overlay flows are fragile.

- Do **not** open a new `InputSelector` or `PromptInputLine` directly from the
  callback of a selector/prompt that is still closing.
- When chaining overlays, defer the next step:

```lua
local function after_overlay(callback)
  wezterm.time.call_after(0.05, callback)
end
```

- Reuse the original pane when continuing a multi-step flow. The overlay pane
  that invoked the callback may no longer be the right pane target.
- For multi-step flows, prefer an explicit `Back` action over silently closing
  the flow.

## Discovery and performance

Picker-open work must stay cheap. `wezterm.run_child_process(...)` is
synchronous and blocks the UI.

- Avoid doing repeated Git calls on every keypress or picker open.
- Linked Git worktrees often have a `.git` **file**, not a `.git` directory, so
  file-system-only repo detection is not reliable.
- In this config, `roots` are **container directories** like `~/Developer`, not
  repo roots.
- Discovery should follow this pattern:
  1. collect container-root children as candidates
  2. run `git rev-parse --git-common-dir`
  3. dedupe by common dir
  4. run `git worktree list --porcelain` once per unique repo
- Do **not** run `git worktree list` separately for every linked worktree path
  that belongs to the same repo.

## Workspace vs worktree semantics

Do not conflate Git worktrees with WezTerm workspaces.

- **Git worktree**: entry from `git worktree list`
- **WezTerm workspace**: GUI session/window grouping in WezTerm

In this config:

- **open worktree** = a WezTerm workspace currently exists for it
- **closed worktree** = the Git worktree exists, but no WezTerm workspace is
  currently open for it

Keep this distinction explicit in labels and removal logic.

## Creating and switching worktrees

- If a workspace does not yet exist, queue first-open state before calling
  `SwitchToWorkspace(...)`.
- Apply layout after the workspace becomes active, not inline with the Git
  creation command.
- If you create multiple tabs with `spawn_tab()`, WezTerm leaves the **last**
  spawned tab active. If the first tab should remain focused, explicitly call
  `ActivateTab(0)`.
- Repo-specific bootstrap commands should run only on first workspace creation,
  after the layout exists, usually in the primary pane.

## Safe removal

- Removing an open worktree requires closing panes in its WezTerm workspace or
  switching away first.
- Do **not** use `window:perform_action(CloseCurrentPane, pane)` with panes from
  another workspace. It can target the wrong GUI window and close the current
  session. For cross-workspace closure, use `wezterm cli kill-pane --pane-id`
  against the target pane ids instead.
- Closing the other workspace and removing the worktree is racy. After closing
  panes, retry `git worktree remove --force` for a short period, then prune and
  verify the directory is actually gone.
- Remove worktrees with `git worktree remove --force` when you need directory
  cleanup for dirty/generated trees.
- After removal, run `git worktree prune` to clean stale metadata.
- Removing a worktree is **not** the same as deleting its branch. Do not add
  branch deletion unless the user explicitly asks for it.

## Path and platform notes

- Normalize paths before comparing them.
- Use `pane:get_current_working_dir().file_path` as the source of the pane cwd.
- Shared config should keep Windows-only defaults behind a platform guard.

## Validation

- For config-only edits, `wezterm ls-fonts >/dev/null` is a fast parse/load
  check.
- If a keybinding appears broken, confirm which config file the live GUI is
  loading before debugging the module logic.
- Prefer toast notifications for user-visible failures instead of silent no-op
  returns.

## Files in this directory

- `wezterm.lua` - top-level wiring for the shared WezTerm config
- `worktree.lua` - worktree picker, creation, removal, layout, and bootstrap
  logic
