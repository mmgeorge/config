vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local buffer = require("forge.buffer")
local StatusView = require("forge.views.harness.status_view")
local parent = buffer.open("status-async")
vim.api.nvim_win_set_buf(0, parent.buffer)
assert(buffer.apply_snapshot(parent, { document = parent.document, revision = 0, block = {} }).kind == "Applied")
local notices = {}
local alive = true
local view = StatusView.new({ transcript = parent, transcript_window = vim.api.nvim_get_current_win(),
  alive = function() return alive end, notice = function(message) notices[#notices + 1] = message end,
  recover = function() end })
local function snapshot(revision, rows)
  return { document = parent.document .. ":status", revision = revision, block = {
    { id = "status", text = rows, metadata = { target = {}, decoration = {}, editable_region = {},
      status = { row = 0, animated = true } } },
  } }
end
local ready
view:apply(snapshot(1, { "Working" }), function(ok) ready = ok end)
assert(vim.wait(3000, function() return ready end, 1))
local rows = {}
for index = 1, 10000 do rows[index] = "Status row " .. index end
local finished
view:apply(snapshot(2, rows), function(ok) finished = ok end)
if view.native.update_pending then
  assert(vim.api.nvim_buf_get_lines(view.buffer, 0, 1, false)[1] == "  Working")
end
assert(vim.wait(10000, function() return finished ~= nil end, 1) and finished)
assert(vim.api.nvim_buf_line_count(view.buffer) == #rows)
local previous = vim.api.nvim_buf_get_lines(view.buffer, 0, -1, false)
view:apply(snapshot(1, { "obsolete" }), function(ok) assert(not ok) end)
assert(vim.deep_equal(previous, vim.api.nvim_buf_get_lines(view.buffer, 0, -1, false)))
alive = false
view:close()
assert(not vim.api.nvim_buf_is_valid(view.buffer))
buffer.close(parent)
print("harness_status_hint_async: passed")
