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
  { id = "refresh", label = "refresh", desc = "Refresh DiffReview", modes = "n", pinned = true, views = { status = true, diff = true, review = true } },
  { id = "close", label = "close", desc = "Close DiffReview", modes = "n", pinned = true },
  { id = "help", label = "help", desc = "Show help", modes = "n", pinned = true },
  { id = "viewed", label = "viewed", desc = "Mark file as viewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "unviewed", label = "unviewed", desc = "Move file back to unviewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "comment", label = "comment", desc = "Add an inline comment", modes = { "n", "x" }, keymap = "review", visual = true, pinned = true, views = { pr = true, review = true } },
  { id = "reply", label = "reply", desc = "Reply to comment, otherwise refresh PR", modes = "n", pinned = true, views = { pr = true } },
  { id = "delete", label = "delete", desc = "Delete inline comment", modes = "n", keymap = "review", pinned = true, views = { pr = true, review = true } },
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
    "delete",
    "open",
    "reply",
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

---@type table<string, DiffReviewStatusCommandSpec[]>
M.view_specs = {
  harness = {
    { id = "submit", label = "send", desc = "Submit the composer", modes = { "n", "i" }, pinned = true },
    { id = "steer", label = "steer", desc = "Send the composer into the active provider turn", modes = { "n", "i" }, pinned = true },
    { id = "cancel", label = "cancel", desc = "Cancel the active Harness turn", modes = { "n", "i" }, pinned = false },
    { id = "edit_queued", label = "edit queued", desc = "Remove and edit the newest queued prompt", modes = { "n", "i" }, pinned = true },
    { id = "toggle_mode", label = "mode", desc = "Toggle Read and Write mode", modes = { "n", "i" }, pinned = true },
    { id = "previous_prompt", label = "previous", desc = "Jump to the previous user prompt", modes = "n", pinned = false },
    { id = "next_prompt", label = "next", desc = "Jump to the next user prompt", modes = "n", pinned = false },
    { id = "history_previous", label = "history-", desc = "Recall the previous submitted prompt", modes = { "n", "i" }, pinned = false },
    { id = "history_next", label = "history+", desc = "Recall the next submitted prompt", modes = { "n", "i" }, pinned = false },
    { id = "toggle_activity", label = "activity", desc = "Toggle complete tool output", modes = "n", pinned = true },
    { id = "open_artifact", label = "artifacts", desc = "Open a session artifact", modes = "n", pinned = true },
    { id = "agent", label = "agents", desc = "Switch or start a child-agent timeline", modes = "n", pinned = true },
    { id = "sessions", label = "sessions", desc = "Search and preview durable Harness sessions", modes = "n", pinned = true },
    { id = "open_timeline", label = "open", desc = "Open the session linked by this timeline row", modes = "n", pinned = false },
    { id = "reopen_question", label = "feedback", desc = "Reopen pending Harness questions", modes = "n", pinned = false },
    { id = "model", label = "model", desc = "Select the model at the next safe boundary", modes = "n", pinned = false },
    { id = "effort_down", label = "effort-", desc = "Decrease reasoning effort", modes = "n", pinned = false },
    { id = "effort_up", label = "effort+", desc = "Increase reasoning effort", modes = "n", pinned = false },
    { id = "close", label = "close", desc = "Close Harness", modes = "n", pinned = true },
    { id = "help", label = "help", desc = "Show Harness help", modes = "n", pinned = true },
  },
  plan_review = {
    { id = "open", label = "open", desc = "Open the source file for this plan node", modes = "n", pinned = true },
    { id = "comment", label = "comment", desc = "Annotate the current plan line", modes = "n", pinned = true },
    { id = "accept", label = "accept", desc = "Accept the exact saved plan and execute it", modes = "n", pinned = true },
    { id = "request_changes", label = "changes", desc = "Request a semantic revision from review comments", modes = "n", pinned = true },
    { id = "close", label = "close", desc = "Return to Harness without deciding", modes = "n", pinned = true },
    { id = "help", label = "help", desc = "Show PlanReview help", modes = "n", pinned = true },
  },
  picker = {
    { id = "previous", label = "previous", desc = "Highlight the previous planning option", modes = "n", pinned = false },
    { id = "next", label = "next", desc = "Highlight the next planning option", modes = "n", pinned = false },
    { id = "select", label = "select", desc = "Select the highlighted planning option", modes = "n", pinned = false },
    { id = "feedback", label = "feedback", desc = "Select with additional feedback", modes = "n", pinned = false },
    { id = "page_previous", label = "previous page", desc = "Show the previous picker page", modes = "n", pinned = false },
    { id = "page_next", label = "next page", desc = "Show the next picker page", modes = "n", pinned = false },
    { id = "focus_input", label = "edit", desc = "Switch between Harness options and feedback input", modes = "n", pinned = false },
    { id = "submit_input", label = "submit", desc = "Submit Harness option input", modes = { "n", "i" }, pinned = false },
    { id = "close", label = "close", desc = "Close the active picker", modes = "n", pinned = false },
  },
}

M.view_spec_by_id = {}
for group, spec_list in pairs(M.view_specs) do
  M.view_spec_by_id[group] = {}
  for _, spec in ipairs(spec_list) do M.view_spec_by_id[group][spec.id] = spec end
end

---@type table<string, DiffReviewStatusCommandSpec>
M.by_id = {}
for _, spec in ipairs(M.specs) do
  M.by_id[spec.id] = spec
end

return M
