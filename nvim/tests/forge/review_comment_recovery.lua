vim.loader.enable(false)
local buffer = require("forge.buffer")
local comments = require("forge.review_comments")
local review = require("forge.review_document")
local editable = require("forge.editable")
local request = {}
review._set_runner_for_test(function(method, params, callback)
  request[#request + 1] = { method = method, params = params, callback = callback }
end)
local function metadata()
  return { target = {}, decoration = {}, visible_decoration = {}, gutter = {}, fold = {}, editable_region = {} }
end
local title = metadata()
title.editable_region = { { id = "title", revision = 0, sequence = 0,
  range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 8 } } } }
local code = metadata()
code.target = { { id = "inline:assets", range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } }
local snapshot = { document = "review-local-comments", revision = 0, block = {
  { id = "region:title", text = { "Original" }, metadata = title },
  { id = "diff:assets", text = { "+ pub mod assets;" }, metadata = code },
} }
local replica = buffer.open(snapshot.document, { editable = {} })
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, replica.buffer)
local state = { active = true, shown = true, explicit_repository = true, document = snapshot.document,
  window = window, replica = replica, view = { id = "local-view" },
  notice = function(message) error(message) end }
local anchor = { revision = string.rep("a", 40), path = "src/assets.rs", side = "right", first_line = 44, last_line = 44 }
comments.attach(state, { snapshot = snapshot, comment = {}, inline_anchor = { ["inline:assets"] = anchor } })
vim.api.nvim_win_set_cursor(window, { 2, 0 })
assert(review.add_comment(state), "inline comment creation must use the local renderer")
vim.wait(20)
vim.cmd("stopinsert")
assert(#request == 0, "creation must not send a host request")
local region = state.comment_focus.region
local bounds = replica.editable.native.anchor[region]
assert(bounds and editable.capture_draft(replica.editable)[1].text == "")
vim.api.nvim_buf_set_text(replica.buffer, bounds.start.row, 0, bounds.finish.row, bounds.finish.column,
  { "Sp", "second line αβ", "" })
vim.api.nvim_buf_set_text(replica.buffer, 0, 0, 0, 8, { "Unsaved title" })
vim.api.nvim_win_set_cursor(window, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = replica.buffer })
assert(#request == 0, "collapse must not send a host request")
assert(vim.api.nvim_buf_get_lines(replica.buffer, 0, 1, false)[1] == "Unsaved title",
  "comment transitions must preserve other unsaved fields")

local recovery = comments.capture(state)
assert(recovery.annotation[1].body == "Sp\nsecond line αβ\n")
assert(recovery.annotation[1].source_id == "diff:assets:1")
local retained_buffer = replica.buffer
comments.detach(state)
buffer.invalidate(replica)
local replacement = vim.deepcopy(snapshot)
replacement.document = "replacement-review"
state.document = replacement.document
state.replica = buffer.open(replacement.document, { buffer = retained_buffer, generated = true,
  expected_changedtick = vim.api.nvim_buf_get_changedtick(retained_buffer), editable = {} })
assert(buffer.apply_snapshot(state.replica, replacement).kind == "Applied")
vim.bo[retained_buffer].buftype = "acwrite"
comments.attach(state, { snapshot = replacement, comment = {}, inline_anchor = { ["inline:assets"] = anchor } }, recovery)
assert(state.replica.buffer == retained_buffer)
assert(state.comment_by_region[region].local_draft)
local capture = editable.capture_draft(state.replica.editable)
assert(capture[1].region == region and capture[1].text == "Sp\nsecond line αβ\n")
assert(capture[1].document == replacement.document and capture[1].sequence > recovery.sequence)
assert(vim.bo[retained_buffer].modified)
state.fields = { { region = "title", baseline = "Original" } }
review.sync_dirty(state)
assert(vim.bo[retained_buffer].modified, "compact local draft lost dirty state when native fields were clean")
local range = state.local_comment_view.range_list[1]
vim.api.nvim_win_set_cursor(window, { range.first_row + 2, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = retained_buffer })
assert(state.replica.editable.native.anchor[region])
assert(comments.capture(state).annotation[1].body == "Sp\nsecond line αβ\n")
assert(#request == 0, "restoration sent a draft edit request")
print("review_comment_recovery: source identities, raw text, draft ownership, and retained buffer passed")
