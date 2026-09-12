local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")
local commands = require("forge.views.commands")
local backend = require("forge.git.git_backend")
local original_root, original_read = backend.git_root_async, backend.systemlist_async
local pending_root, pending_read
backend.git_root_async = function(callback) pending_root = callback end
backend.systemlist_async = function(_, callback) pending_read = callback end

local succeeded, failure = xpcall(function()
  local origin_window = vim.api.nvim_get_current_win()
  local source = vim.api.nvim_get_current_buf()
  local replacement = vim.api.nvim_create_buf(true, false)
  commands.open_compact_preview()
  vim.api.nvim_win_set_buf(origin_window, replacement)
  pending_root(workspace)
  assert(pending_read == nil, "abandoned root lookup admitted a diff read")

  vim.api.nvim_win_set_buf(origin_window, source)
  commands.open_compact_preview({ cwd = workspace })
  vim.api.nvim_win_set_buf(origin_window, replacement)
  local buffer_count = #vim.api.nvim_list_bufs()
  pending_read({ "late diff" }, 0, "")
  assert(vim.api.nvim_get_current_buf() == replacement, "late diff replaced the user's current buffer")
  assert(#vim.api.nvim_list_bufs() == buffer_count, "abandoned diff allocated a preview buffer")

  commands.open_compact_preview({ cwd = workspace })
  vim.cmd("vsplit")
  local other_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(other_window, source)
  pending_read({ "accepted diff" }, 0, "")
  assert(vim.api.nvim_get_current_win() == other_window and vim.api.nvim_get_current_buf() == source,
    "preview completion stole another window")
  local preview = vim.api.nvim_win_get_buf(origin_window)
  assert(vim.api.nvim_buf_get_lines(preview, 0, -1, true)[1] == "accepted diff")
  assert(not vim.bo[preview].modifiable)
end, debug.traceback)

backend.git_root_async, backend.systemlist_async = original_root, original_read
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("compact preview lifecycle passed")
vim.cmd("qa!")
