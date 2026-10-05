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
local range = state.local_comment_view.range_list[1]
vim.api.nvim_win_set_cursor(window, { range.first_row + 2, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = replica.buffer })
review.sync_comment_focus(state)
assert(#request == 0 and state.comment_focus.region == region, "re-entry must focus the same local draft")
assert(review.save_comment(state, "save"))
assert(#request == 1 and request[1].method == "review.comment")
assert(request[1].params.command.operation == "save_draft")
assert(#request[1].params.capture == 1 and request[1].params.capture[1].region == region)
assert(request[1].params.capture[1].text == "Sp\nsecond line αβ\n")
assert(vim.deep_equal(request[1].params.draft_comment[1].anchor, anchor))
assert(replica.editable.region.title.pending, "comment save must leave the title unsent")
bounds = replica.editable.native.anchor[region]
vim.api.nvim_buf_set_text(replica.buffer, bounds.start.row, 2, bounds.start.row, 2, { " newer" })
assert(request[1].params.capture[1].text == "Sp\nsecond line αβ\n", "newer typing must not change a submitted capture")
assert(editable.capture_draft(replica.editable)[1].text == "Sp newer\nsecond line αβ\n")
assert(not review.save_comment(state, "delete"), "an in-flight creation must retain its draft identity")
request[1].callback({ snapshot = { document = snapshot.document, comment = 1, region = region,
  revision = 1, sequence = request[1].params.capture[1].sequence,
  text = request[1].params.capture[1].text, baseline = request[1].params.capture[1].text,
  anchor = anchor, viewer_did_author = true, dirty = false } })
assert(vim.wait(1000, function() return not state.comment_running end))
assert(replica.editable.region[region].revision == 1 and replica.editable.region[region].pending,
  "save completion must adopt the accepted revision and retain newer typing")
vim.api.nvim_win_set_cursor(window, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = replica.buffer })
assert(not replica.editable.native.anchor[region], "accepted compact comments must not leave editable box rows")
vim.api.nvim_win_set_cursor(window, { replica.physical_row(1) + 1, 0 })
assert(review.add_comment(state))
vim.wait(20)
vim.cmd("stopinsert")
local discarded = state.comment_focus.region
assert(review.save_comment(state, "delete"), "unsaved local comments must delete locally")
assert(#request == 1 and not state.comment_by_region[discarded] and not replica.editable.region[discarded],
  "local deletion must release the draft without a host request")
vim.api.nvim_win_set_cursor(window, { replica.physical_row(1) + 1, 0 })
assert(review.add_comment(state))
vim.wait(20)
vim.cmd("stopinsert")
local empty = state.comment_focus.region
vim.api.nvim_win_set_cursor(window, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = replica.buffer })
assert(not state.comment_by_region[empty] and not replica.editable.region[empty],
  "abandoned empty drafts must not leave a body queued for submission")
assert(#request == 1)
vim.api.nvim_win_set_cursor(window, { 1, 0 })
assert(review.add_comment(state), "conversation drafts must create locally")
vim.wait(20)
vim.cmd("stopinsert")
assert(state.comment_focus.local_draft and not state.comment_focus.anchor)
assert(#request == 1, "conversation creation must not contact Rust")
assert(review.save_comment(state, "delete"))
local parent = state.comment_by_region[region]
state.comment_focus = parent
state.mode = "overview"
assert(review.reply_comment(state), "reply drafts must create locally")
vim.wait(20)
vim.cmd("stopinsert")
local reply = state.comment_focus
assert(reply.reply_to == parent.comment and vim.deep_equal(reply.anchor, parent.anchor))
state.comment_focus = parent
assert(review.reply_comment(state) and state.comment_focus == reply, "repeated reply creation must reuse one draft")
assert(#request == 1, "reply creation and re-entry must not contact Rust")
state.mode, state.comment_focus = "batched", parent
assert(not review.reply_comment(state), "batched review must reject reply creation locally")
state.mode, state.comment_focus = "overview", reply
assert(review.save_comment(state, "delete"))
comments.detach(state)
buffer.close(replica)
print("review_comments: zero-request creation/focus, unsaved fields, exact captures, and newer typing passed")
