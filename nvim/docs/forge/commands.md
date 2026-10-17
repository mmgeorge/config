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
Source excerpts retain function signatures. Modified functions include the first 20
changed body lines and two nearby unchanged lines. Added and removed functions retain
only their attributes and signatures. Test functions follow the same rule.
Changed old/new signature pairs appear together. Body lines
stay in original diff order, including inline
callbacks and nested functions. Test/spec files follow other files. Standalone imports
remain in the counts but are omitted from excerpts. Mixed import/code rows and runtime
import calls remain visible. Markdown and plain-text changes retain all
changed lines instead of applying the 20-line limit.

The constructed input is capped at 500,000 UTF-8 bytes. At that limit, further excerpts
stop, including signatures. Files without an included excerpt appear only in the file
list. Omitted ranges within an excerpt show `... N lines (+A -D)`.
Whitespace-only gaps have no marker. A single omitted line is shown directly, even
when the function body would otherwise be hidden, subject to the global byte limit.
Unsupported or malformed syntax uses ordinary diff excerpts rather than signature-aware excerpts.
License files and common dependency lockfiles keep their paths and change counts in the
file list but have no excerpts, including license files with Markdown or text extensions.

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
| `:ForgeDebugCommitPromptPreview [comparison]` | Optional `head`, `staged`, or `latest_commit` | Inspect the exact system instructions and compact commit context without calling a model |

`ForgeDebugCommitPromptPreview` defaults to `head`, including staged, unstaged,
and untracked changes. It uses the current Forge Status workspace when invoked
there, otherwise the current working directory. The read-only scratch buffer shows
the UTF-8 input size, system instructions, and user prompt. Press `q` to close it.
Long lines wrap at word boundaries without changing the prompt text.
Each invocation collects current repository state. Empty comparisons appear in
the buffer, and failed requests produce an error notification.
Renamed files appear once in the compact file list with their original path and
have no detailed content excerpt, including when the rename also contains edits.

A successful build does not replace an executable already loaded by a running
host. Restart Neovim to use the rebuilt host. Compilation errors appear as build
failure notifications. See [Building and testing](../../contributing/forge/building-and-testing.md)
for the contributor workflow.

GitHub PR and review views are opened by their integration, rather than by an
additional standalone Forge PR command.

PR creation checks the selected base and head for merge conflicts before preparing
the diff or requesting a description. Conflicts stop the workflow with "Merge
conflicts. Resolve them before creating the PR." The check leaves your branch,
index, and working files unchanged.
