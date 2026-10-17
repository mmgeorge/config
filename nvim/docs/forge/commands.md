# Forge commands

These commands are registered by `nvim/lua/plugins/forge.lua`. This reference
describes the current local plugin configuration. The source registration defines
argument parsing and completion. Contributor documentation starts at
[Contributing to Forge](../../contributing/forge/README.md).

## Git review

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeStatus` | None | Open the current repository's Git review view |
| `:ForgeBranchDiff <revision>` | One branch or revision | Compare the working tree against that revision |
| `:ForgeBranchDiffFile <file> <revision>` | File, then branch or revision | Open that comparison for one file |
| `:ForgeFileRevision <file> <commit>` | File, then revision | Open a read-only historical file |
| `:ForgeDiffCompactPreview` | None | Preview the compacted working-tree diff |
| `:ForgeDiffCompactPreview!` | Bang | Preview the compacted staged diff |

For example, `:ForgeFileRevision README.md HEAD` opens the committed README.
Revision arguments use the shared revision completion source. File-specific
commands complete the file argument before the revision argument.

## Generated commit messages

Automatic About drafts and commit-message regeneration send a compact diff to the
selected model. The context starts with changed files and added/deleted line counts.
Source excerpts retain function signatures, the first 20 changed body lines, and two
nearby unchanged lines. Test/spec files follow other files. Import changes remain in
the counts but are omitted from excerpts. Markdown and plain-text changes retain all
changed lines instead of applying the 20-line limit.

The constructed input is capped at 500,000 UTF-8 bytes. At that limit, further excerpts
stop, including signatures. Files without an included excerpt appear only in the file
list. Omitted ranges within an excerpt show `... N lines omitted (+A -D)`. Unsupported
or malformed syntax uses ordinary diff excerpts rather than signature-aware excerpts.

## Harness and permissions

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeHarness` | None | Open the Harness |
| `:ForgeHarnessNew` | None | Create a fresh Harness session |
| `:ForgeHarnessLog` | Optional `on`, `off`, or `open` | Control or open the active session's protocol trace |
| `:ForgePermissions` | None | Edit the Harness permissions document |

`ForgeHarnessLog` requires an open Harness session. Inside Harness, `/sessions`
opens durable session search and `/undo` opens checkpoint rollback.

`/config` includes an **LSP tools** toggle. It enables the read-only
[`lsp` tool](lsp-tools.md) for model-driven code exploration without
changing interactive plan navigation or starting language servers.

`/checks` opens project checks, and `/checks setup` configures their commands and
selection policies. See [Project checks](checks.md) for package-aware commands and
new-test selection.

PlanReview shows declaration changes with nearby context lines and enclosing
declaration boundaries, using the regular diff grouping rules. Nearby method
changes share their enclosing `impl` context. Distant changes appear in separate
groups without repeating that `impl` header. Explicit reference navigation can
reveal nearby context around a declaration outside the displayed changes.
Regular diffs and PlanReview render enclosing declarations as syntax-highlighted
source lines. An indented `...` marks omitted source between visible regions.

Inside PlanReview, `oc` opens plan-specific check selection. Space toggles a
check, with `●` for enabled and `○` for disabled. Visual `j` deletes selected
planned tests or disables selected checks after confirmation.

## Build and diagnostics

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeBuild` | None | Explicitly build Forge and report the selected executable |
| `:ForgeStartupLog` | None | Open the startup diagnostic log |
| `:ForgeGitConfigCacheReset` | None | Clear the cached Git configuration location |

A successful build does not replace an executable already loaded by a running
host. Restart Neovim to use the rebuilt host. Compilation errors appear as build
failure notifications. See [Building and testing](../../contributing/forge/building-and-testing.md)
for the contributor workflow.

GitHub PR and review views are opened by their integration, rather than by an
additional standalone Forge PR command.
