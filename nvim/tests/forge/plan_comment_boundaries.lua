vim.loader.enable(false)
local comments = require("forge.draft_comments")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
local notice = {}
local original_notify = vim.notify
vim.notify = function(message) notice[#notice + 1] = tostring(message) end
local source = { "Source one", "Source two", "Source three" }
local state = comments.attach(buffer, window, source, {}, { guard_source = true })
comments.add(buffer, { id = "draft", source_line = 1, end_source_line = 1, body = "Keep this body" }, false)
local original = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
vim.cmd("normal! 999dd")
assert(vim.wait(1000, function() return not state.guard.native.rejecting end))
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), original),
  "counted deletion crossed the comment footer and changed generated source")
assert(comments.capture(buffer)[1].source.body == "Keep this body", "rejected deletion changed the draft")
assert(#notice == 1 and notice[1]:find("read-only boundary", 1, true), "rejected edit omitted its boundary error")
comments.focus(buffer, "draft")
local body = vim.api.nvim_win_get_cursor(window)[1] - 1
vim.api.nvim_buf_set_text(buffer, body, 0, body, 0, { "First paragraph", "", "" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
assert(comments.capture(buffer)[1].source.body == "First paragraph\n\nKeep this body", "valid multiline insertion was rejected")
comments.focus(buffer, "draft")
vim.cmd("normal! kdd")
assert(vim.wait(1000, function() return not state.guard.native.rejecting end))
assert(comments.capture(buffer)[1].source.body == "First paragraph\n\nKeep this body", "deleting the header changed the draft")
assert(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1] == "Source one")
comments.focus(buffer, "draft")
vim.api.nvim_win_set_cursor(window, { vim.api.nvim_win_get_cursor(window)[1], 0 })
vim.cmd("normal! dd")
vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
assert(comments.capture(buffer)[1].source.body == "\nKeep this body", "deleting a body line was rejected")
comments.detach(buffer)
vim.notify = original_notify
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), source))
vim.api.nvim_buf_delete(buffer, { force = true })
print("plan_comment_boundaries: counted deletion, header protection, restoration, and multiline editing passed")
