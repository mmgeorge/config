--- Declares the status/PR/review/diff command vocabulary: per-command metadata (id,
--- label, keymap group, modes, applicable views) and the hint-bar ordering per view.
--- Pure data shared by keymap setup, the hint bar, and command visibility checks.
---@class DiffReviewCommandSpecsModule
local M = {}

---@type DiffReviewStatusCommandSpec[]
M.specs = {
  { id = "toggle", label = "toggle", desc = "Toggle fold", modes = "n", pinned = true, views = { status = true, pr = true, diff = true, review = true } },
  { id = "collapse_parent", label = "Collapse Parent", desc = "Collapse Parent", modes = "n", pinned = true, views = { status = true, pr = true, diff = true, review = true } },
  { id = "visual_line_with_gutter", label = "select gutter", desc = "Start visual line selection including the diff gutter", modes = "n", pinned = false, views = { status = true, pr = true, diff = true, review = true } },
  { id = "stage", label = "stage", desc = "Stage hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "unstage", label = "unstage", desc = "Unstage hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "discard", label = "discard", desc = "Discard hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "commit", label = "commit", desc = "Commit", modes = "n", pinned = true, views = { status = true } },
  { id = "push", label = "push", desc = "Push", modes = "n", pinned = true, views = { status = true } },
  { id = "pull", label = "pull", desc = "Pull", modes = "n", pinned = true, views = { status = true } },
  { id = "pr", label = "pr", desc = "Open pull request", modes = "n", pinned = true, views = { status = true } },
  { id = "branch_create", label = "branch", desc = "Create a branch", modes = "n", pinned = false, views = { status = true } },
  { id = "walkthrough", label = "walkthrough", desc = "Review walkthrough", modes = "n", pinned = false, views = { status = true } },
  { id = "review", label = "review", desc = "Start PR review", modes = "n", pinned = true, views = { status = true, pr = true } },
  { id = "browse", label = "browse", desc = "Browse pull request", modes = { "n", "x" }, visual = true, pinned = true, views = { pr = true, review = true } },
  { id = "open", label = "open", desc = "Open PR/about or jump to file", modes = "n", pinned = true },
  { id = "refresh", label = "refresh", desc = "Refresh DiffReview", modes = "n", pinned = true },
  { id = "close", label = "close", desc = "Close DiffReview", modes = "n", pinned = true },
  { id = "help", label = "help", desc = "Show help", modes = "n", pinned = true },
  { id = "viewed", label = "viewed", desc = "Mark file as viewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "unviewed", label = "unviewed", desc = "Move file back to unviewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "comment", label = "comment", desc = "Add or edit an inline comment", modes = { "n", "x" }, keymap = "review", visual = true, pinned = true, views = { pr = true, review = true } },
  { id = "delete", label = "delete", desc = "Delete draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "next_comment", label = "next", desc = "Jump to next draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "prev_comment", label = "prev", desc = "Jump to previous draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "sync", label = "sync", desc = "Sync inline comments to GitHub", modes = { "n", "i" }, keymap = "review", pinned = true, views = { pr = true, review = true } },
  { id = "submit", label = "submit", desc = "Submit review to GitHub", modes = "n", keymap = "review", pinned = true, views = { review = true } },
}

M.hint_command_ids_by_view = {
  status = {
    "stage",
    "unstage",
    "discard",
    "commit",
    "open",
    "refresh",
    "close",
    "help",
  },
  pr = {
    "toggle",
    "browse",
    "review",
    "comment",
    "open",
    "refresh",
    "close",
    "help",
  },
  review = {
    "toggle",
    "collapse_parent",
    "viewed",
    "unviewed",
    "comment",
    "delete",
    "sync",
    "submit",
    "open",
    "close",
    "help",
  },
  diff = {
    "open",
    "refresh",
    "close",
    "help",
  },
}

---@type table<string, DiffReviewStatusCommandSpec>
M.by_id = {}
for _, spec in ipairs(M.specs) do
  M.by_id[spec.id] = spec
end

return M
