--- DiffReview — a self-contained, in-editor Git review UI.
---
--- Renders staged/unstaged changes, single-file diffs, working-tree-vs-branch diffs,
--- historical file revisions, GitHub pull requests, a batched PR review mode, and an
--- LLM-authored guided walkthrough — all as native Neovim buffers with tree-sitter
--- syntax, intraline highlights, gutter line numbers, and folding. No external diff
--- viewer or webview: every surface is a real, navigable buffer.
---
--- ===========================================================================
--- USER COMMANDS  (registered in lua/plugins/diff_review.lua, which calls into
--- the public API below)
--- ===========================================================================
---   :GitStatus
---       Open the working-tree review buffer: staged/unstaged sections, the
---       Head/Merge/Push commit rows, the About summary, and — when the branch
---       has a GitHub PR — the PR row. Stage/unstage/discard hunks or files,
---       fold, jump into the file, and launch the PR/review flows from here.
---   :GitBranchDiff <rev>
---       Diff the whole working tree against a branch or revision.
---   :GitBranchDiffFile <file> <rev>
---       Diff a single file against a branch or revision.
---   :GitFileRevision <file> <rev>
---       Open a file read-only as it existed at a git revision.
---   :GitDiffCompactPreview[!]
---       Preview the compacted unified diff (with `!`, the staged diff).
---   :Harness / :HarnessNew
---       Open the AI transcript/composer or start a fresh durable Harness session.
---   :Interactions / :Sessions
---       Review per-user-action diffs and browse durable repository/global sessions.
---
--- GitHub PR review surfaces (the PR overview, in-place PR title/body/reviewer
--- editing, and the batched review mode with draft comments + verdict) are
--- launched from inside the :GitStatus view via its keymaps (`op`/`opP`/`ogp`
--- to open/create a PR, `or` to start a review), not from a top-level command.
---
--- ===========================================================================
--- PUBLIC LUA API  (require("diff_review"))
--- ===========================================================================
---   setup(opts)                 Merge config, register highlights + per-view
---                               controllers. Call once (the plugin spec does).
---   open()                      Open the :GitStatus working-tree review buffer.
---   open_branch_diff(rev, opts) Branch diff; opts.file scopes it to one file.
---   open_file_revision(f, rev)  Read-only view of file `f` at revision `rev`.
---   open_compact_preview(opts)  Compacted diff preview (opts.staged for cached).
---   open_pr(pr, opts)           PR overview for an already-resolved PR object.
---   open_pr_number(num, opts)   PR overview fetched by number.
---   open_review(pr, opts)       Batched review mode for a PR.
---   get(cb, ctx)                Async-collect git status items, then `cb(items)`
---                               (a Trouble/quickfix-style source entry point).
---   set_git_backend(b) /        Inject / restore the git command backend — the
---     reset_git_backend()       seam the headless test suite drives.
---
--- ===========================================================================
--- CONFIGURATION & KEYBINDINGS  (lazy.nvim)
--- ===========================================================================
--- Configure via require("diff_review").setup(opts), passed from a lazy.nvim spec.
--- In-buffer ACTION keys default to the table below and are overridden per-command
--- in opts.keymaps; keys that LAUNCH a view go in the spec's keys=.
---
---   -- e.g. lua/plugins/diff_review.lua
---   return {
---     "you/diff_review",                      -- or `dir = vim.fn.stdpath("config")` for a local plugin
---     cmd = {                                  -- lazy-load on these user commands
---       "GitStatus", "GitBranchDiff", "GitBranchDiffFile",
---       "GitFileRevision", "GitDiffCompactPreview",
---     },
---     keys = {                                 -- your keys to LAUNCH a view
---       { "<leader>gs", "<cmd>GitStatus<cr>", desc = "Git status review" },
---       { "<leader>gd", function() require("diff_review").open_compact_preview() end,
---         desc = "Compact diff preview" },
---     },
---     opts = {
---       about_auto_generate = false,           -- a few common options (see infra/config.lua defaults)
---       walkthrough_inventory = "sem",         -- use Sem inventory; false disables it
---       keymaps = {
---         -- In-buffer ACTION keys. Override per command; unspecified keys keep
---         -- their defaults (deep-merge). A value is a key string, a LIST of keys,
---         -- or false to DISABLE that binding.
---         status = {                            -- GitStatus / PR / branch-diff / diff views
---           discard = "X",                      -- rebind one command
---           open = { "<cr>", "o" },             -- bind multiple keys
---           walkthrough = false,                -- disable a binding
---         },
---         review = {                            -- batched PR review mode (its own key group)
---           submit = "<C-CR>",
---         },
---       },
---     },
---     config = function(_, opts)
---       require("diff_review").setup(opts)
---       -- the spec also registers the :Git* user commands here (see the real
---       -- lua/plugins/diff_review.lua); a packaged plugin would do that in plugin/.
---     end,
---   }
---
--- Keymap model — two groups by MODE, not per view. The status-family views
--- (status / pr / diff / branch-diff) share one consistent vocabulary from
--- `keymaps.status`; review mode redefines keys and has its own `keymaps.review`.
--- Which commands appear in which view is data in shared/command_specs.lua (the
--- `views` field), and the in-buffer `?` help lists the live bindings per view.
--- Defaults:
---   status:  close=q  refresh=R  toggle=<Tab>  collapse_parent=N
---            visual_line_with_gutter=W  stage=S  unstage=U  discard=j
---            open={o,<CR>,.}  commit=cc  push=opp  pull=opP  pr=ogp
---            branch_create=bc  browse=b  walkthrough=ow  review=or  help=?
---   review:  viewed=S  unviewed=U  comment=C  delete=J  next_comment=y
---            prev_comment=z  sync=<C-s>  submit=cc
---
--- ===========================================================================
--- BUFFERS / VIEWS
--- ===========================================================================
---   GitStatus       The main review buffer (views/status/*).
---   diff://<file>   Standalone single-file diff with hunk-level stage/unstage.
---   GitBranchDiff   Branch / single-file branch diff (a status-style buffer).
---   GitFileRevision A file at a past revision, red winbar, read-only.
---   PR overview     Metadata, checks, review summaries, inline comments.
---   Review          Draft-comment CRUD, viewed-state, verdict, submission.
---   Walkthrough     LLM-authored guided review: summary + anchored comment boxes.
---   Harness         Real-line AI transcript plus a multiline composer split.
---   PlanReview      Physical Markdown plan with edits, annotations, and oY/oN decisions.
---   Interactions    Foldable session interaction diffs with review and rollback.
---   Sessions        Current Repo / All Repos Harness session browser.
---
--- ===========================================================================
--- ARCHITECTURE  (see lua/diff_review/docs/architecture.md for the full reference)
--- ===========================================================================
--- A layered package with a strict inward dependency direction:
---
---     views/  →  render/  →  git/  →  infra/        (+ shared/, integrations/)
---
--- Modules `require()` their dependencies DIRECTLY (the gitsigns / telescope /
--- lazy.nvim idiom); there is no `dr()` re-export wall. The few genuinely cyclic
--- edges in the status/PR/render clusters use a lazy in-function require accessor
--- (`local function owner() return require("diff_review....") end`), the same
--- pattern Neogit uses. Shared mutable state (the active status buffer, the
--- per-session diff caches) lives in `session.lua`, which depends on nothing, so
--- every layer imports it without a cycle.
---
--- init.lua is therefore a thin **public-API facade**: the `open*`/`setup`/`get`
--- entry points (a passthrough to views/commands and git/git_backend) plus the
--- one deliberate `require("diff_review")` seam left in the tree — the
--- gitstatus-debug dev/test flags below, which views/status/status_debug.lua
--- reads and the test suite overrides.

---@class DiffReviewModule
---@field config DiffReviewConfig?
---@field _gitstatus_debug_log_file string?
local M = {}

-- Register the plugin's bundled treesitter queries on the runtimepath (see query_runtime).
require("diff_review.query_runtime")

local config = require("diff_review.infra.config")
local highlights = require("diff_review.infra.highlights")
local commands = require("diff_review.views.commands")
local git_backend = require("diff_review.git.git_backend")
local perf = require("diff_review.infra.perf")

highlights.setup()

---@class DiffReviewGutterSpec
---@field width integer
---@field old_width integer
---@field new_width integer

---@class DiffReviewHunkChangeRegion
---@field first_item integer
---@field last_item integer
---@field context_line integer?
---@field context DiffReviewHunkTreeSitterContext|string?
---@field context_key string?
---@field changed_line integer?
---@field after_line integer?
---@field added integer
---@field removed integer

---@alias DiffReviewDiffSyntaxSide "old"|"new"

---@class DiffReviewHunkContextPaddingLine
---@field line_number integer
---@field old_line? integer
---@field new_line? integer
---@field text string

---@class DiffReviewHunkRenderPlan
---@field block DiffReviewParsedBlock
---@field hunk DiffReviewParsedHunk
---@field hunk_index integer
---@field region DiffReviewHunkChangeRegion
---@field region_index integer
---@field region_count integer
---@field render_items table[]
---@field include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@field gutter DiffReviewGutterSpec
---@field source_lines string[]?
---@field occupied_lines table<integer, boolean>
---@field syntax_by_item table<integer, { syntax?: DiffReviewTreeSitterSyntax, row?: integer }>
---@field visible_source_lines table<string, boolean>
---@field before_padding_lines DiffReviewHunkContextPaddingLine[]
---@field after_padding_lines DiffReviewHunkContextPaddingLine[]
---@field changed_lines table<integer, boolean>
---@field display_start integer?
---@field display_end integer?

---@class DiffReviewHunkDisplayGroup
---@field plans DiffReviewHunkRenderPlan[]
---@field gutter DiffReviewGutterSpec
---@field added integer
---@field removed integer
---@field display_start integer?
---@field display_end integer?
---@field changed_lines table<integer, boolean>

---@class DiffReviewSectionConfig
---@field name string
---@field title string
---@field default_folded boolean

---@class DiffReviewStatusCommandSpec
---@field id string
---@field label string
---@field desc string
---@field modes string|string[]
---@field keymap? "status"|"review"
---@field visual? boolean
---@field pinned boolean
---@field views? table<DiffReviewStatusViewKind, boolean>

---@param opts? DiffReviewConfig
function M.setup(opts)
  M.config = config.setup(opts)
  perf.configure_from_diff_review_options(M.config)
  highlights.setup()
  require("diff_review.views.status.state").register_view_controllers()
end

---@param cb fun(items: table[])
---@param _ctx table?
function M.get(cb, _ctx)
  git_backend.git_root_async(function(cwd)
    if not cwd then
      cb({})
      return
    end
    require("diff_review.git.git_data")._collect_items_from_git(cwd, cb, _ctx)
  end)
end

-- Public API: the user-facing entry points, a thin facade over the command module.
M.open = commands.open
M.open_pr = commands.open_pr
M.open_pr_number = commands.open_pr_number
M.open_review = commands.open_review
M.open_branch_diff = commands.open_branch_diff
M.open_file_revision = commands.open_file_revision
M.open_compact_preview = commands.open_compact_preview
M.open_harness = function() require("diff_review.views.harness").open() end
M.new_harness_session = function() require("diff_review.views.harness").new_session() end
M.open_interactions = function() require("diff_review.views.interactions").open() end
M.open_sessions = function() require("diff_review.views.sessions").open() end
M.set_git_backend = git_backend.set_backend
M.reset_git_backend = git_backend.reset_backend

--- Resolve the gitstatus debug log path. Kept on init as a dev/test override seam: tests swap
--- M._gitstatus_debug_log_path to redirect the log file, and views/status/status_debug.lua reads
--- the debug flags (_gitstatus_debug_force / _gitstatus_debug_enabled / _gitstatus_debug_perf_*)
--- that also live on M. This is the one intentional require("diff_review") seam left in the tree.
---@return string
function M._gitstatus_debug_log_path()
  if M._gitstatus_debug_log_file then return M._gitstatus_debug_log_file end
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  M._gitstatus_debug_log_file = dir .. "/gitstatus-debug.log"
  return M._gitstatus_debug_log_file
end

return M
