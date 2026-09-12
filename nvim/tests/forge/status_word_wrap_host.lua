local root = vim.fn.getcwd()
dofile(root .. "/nvim/tests/forge/fixtures/commit_reuse_manual.lua")
local status = require("forge.status")
local client = require("forge.client")
local state = forge_reuse.state
local notices = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then notices[#notices + 1] = message end end
local function idle()
  return state.ready and not state.request_active and next(client._client.pending) == nil
end
local text = "The default profile is `release` for development, runtime verification, and profiling."
local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
end
local success, failure = xpcall(function()
  vim.o.columns, vim.o.lines = 80, 40
  assert(vim.wait(15000, idle, 10))
  vim.fn.writefile({ text }, forge_reuse.fixture .. "/tracked.txt")
  status.refresh(state)
  assert(vim.wait(10000, idle, 10))
  local file
  for _, candidate in ipairs(state.replica.inventory.file) do if candidate.path == "tracked.txt" then file = candidate end end
  local _, first = state.replica.sequence:position("file:" .. assert(file).id)
  vim.api.nvim_win_set_cursor(0, { first + 1, 0 })
  press("<Tab>")
  local row
  assert(vim.wait(10000, function()
    for index, line in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
      if line == text then row = index break end
    end
    return row and idle()
  end, 10))
  local function audit()
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.cmd("normal! zt")
    vim.cmd.redraw()
    local previous_row, continuation_count = nil, 0
    for column, word in text:gmatch("()(%S+)") do
      local first_cell = vim.fn.screenpos(0, row, column)
      local last_cell = vim.fn.screenpos(0, row, column + #word - 1)
      assert(first_cell.row > 0 and last_cell.row > 0, "wrapped source word was clipped: " .. word)
      assert(first_cell.row == last_cell.row, "native wrapping split a word: " .. word)
      if previous_row and first_cell.row ~= previous_row then
        continuation_count = continuation_count + 1
        assert(first_cell.col == vim.api.nvim_win_get_position(0)[2] + 2, "wrapped source lost its one-cell left margin")
      end
      previous_row = last_cell.row
    end
    assert(continuation_count > 0, "fixture did not wrap")
    assert(vim.api.nvim_buf_get_lines(state.replica.buffer, row - 1, row, false)[1] == text, "wrapping changed source text")
  end
  audit()
  vim.cmd("enew")
  vim.api.nvim_set_current_buf(state.replica.buffer)
  assert(vim.wait(10000, idle, 10))
  audit()
  vim.cmd("vsplit")
  vim.cmd("vertical resize 48")
  assert(vim.api.nvim_win_get_width(0) == 48)
  audit()
  vim.cmd("close")
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
status.close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20))
assert(success, failure)
print("expanded Status diff word wrapping and buffer return passed")
vim.cmd("qa!")
