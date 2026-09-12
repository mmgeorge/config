vim.loader.enable(false)
local buffer = require("forge.buffer")
local input = require("forge.input")
local presentation = require("forge.window_presentation")
local window = vim.api.nvim_get_current_win()
local ordinary = vim.api.nvim_get_current_buf()
local baseline = { number = true, relativenumber = true, signcolumn = "yes:2", foldcolumn = "2",
  statuscolumn = "%l %=%s",
  foldmethod = "manual", foldexpr = "0", foldenable = false, foldlevel = 3, virtualedit = "block",
  wrap = false, linebreak = false, breakindent = true, conceallevel = 2, concealcursor = "nv" }
for name, value in pairs(baseline) do vim.wo[window][name] = value end
local function document(id)
  local state = buffer.open(id)
  buffer.apply_snapshot(state, { document = id, revision = 1, block = {
    { id = "body", text = { "first", "second" }, metadata = { decoration = {}, target = {}, editable_region = {} } },
  } })
  return state
end
local first, second = document("native-layout:first"), document("native-layout:second")
local function original(attached)
  for name, value in pairs(baseline) do assert(vim.wo[attached][name] == value, name .. " did not restore") end
end
local function native(attached)
  assert(not vim.wo[attached].number and not vim.wo[attached].relativenumber)
  assert(vim.wo[attached].signcolumn == "no" and vim.wo[attached].foldcolumn == "0")
  assert(vim.wo[attached].foldmethod == "expr" and vim.wo[attached].wrap and vim.wo[attached].linebreak)
  assert(require("forge.width").capture(attached).columns == vim.api.nvim_win_get_width(attached) - 1)
  local original_window_info = vim.fn.getwininfo
  vim.fn.getwininfo = function() return { { textoff = 0 } } end
  assert(require("forge.width").capture(attached).columns == vim.api.nvim_win_get_width(attached) - 1,
    "initial width capture depended on Neovim's deferred status column layout")
  vim.fn.getwininfo = original_window_info
end
local success, failure = xpcall(function()
  vim.api.nvim_win_set_buf(window, first.buffer)
  local first_view = input.open(first, window)
  native(window)
  vim.cmd("vsplit")
  local split = vim.api.nvim_get_current_win()
  local split_view = input.open(first, split)
  native(split)
  vim.api.nvim_win_set_buf(split, ordinary)
  original(split)
  input.close(split_view)
  vim.api.nvim_win_close(split, true)
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_buf(window, ordinary)
  original(window)
  vim.api.nvim_win_set_buf(window, first.buffer)
  native(window)
  vim.api.nvim_win_set_buf(window, second.buffer)
  local second_view = input.open(second, window)
  input.close(first_view)
  native(window)
  vim.wo[window].foldmethod = "indent"
  vim.wo[window].conceallevel = 1
  input.close(second_view)
  assert(vim.wo[window].foldmethod == "indent" and vim.wo[window].conceallevel == 1,
    "release overwrote an unrelated option change")
  for name, value in pairs(baseline) do vim.wo[window][name] = value end
  vim.api.nvim_win_set_buf(window, first.buffer)
  first_view = input.open(first, window)
  local captured = presentation.capture(window)
  vim.api.nvim_win_set_buf(window, second.buffer)
  local exact = input.open(second, window, { exact_source = true })
  original(window)
  local source_owner = {}
  presentation.retain(window, source_owner, captured)
  input.close(first_view)
  original(window)
  vim.api.nvim_win_set_buf(window, first.buffer)
  local replacement = input.open(first, window)
  input.close(exact)
  assert(not presentation.release(window, source_owner), "late source release removed new ownership")
  native(window)
  input.close(replacement)
  original(window)
  local transcript = input.open(first, window, { margin = 0 })
  assert(vim.wo[window].statuscolumn == "")
  assert(require("forge.width").capture(window).columns == vim.api.nvim_win_get_width(window))
  vim.api.nvim_win_set_buf(window, ordinary)
  original(window)
  vim.api.nvim_win_set_buf(window, first.buffer)
  assert(vim.wo[window].statuscolumn == "", "transcript margin changed on reentry")
  input.close(transcript)
  original(window)
  local issue = input.open(first, window, { columns = {
    number = true, relativenumber = false, signcolumn = "yes:2", foldcolumn = "1", statuscolumn = "%l %=%s",
  } })
  assert(vim.wo[window].number and not vim.wo[window].relativenumber)
  assert(vim.wo[window].signcolumn == "yes:2" and vim.wo[window].foldcolumn == "1")
  assert(vim.wo[window].foldmethod == "expr", "inherited columns disabled document folds")
  input.close(issue)
  original(window)
end, debug.traceback)
buffer.close(first)
buffer.close(second)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("native_window_layout OK")
vim.cmd("qa!")
