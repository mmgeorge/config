-- Unit tests for `forge.views.harness.tabline` (tabline.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- A focused popup, such as a plan decision prompt, holds an unnamed scratch buffer, and naming the
-- tab after it read `[No Name]` until the popup closed. A tab is named after the window the popup
-- floats over.
T["a focused float does not rename its tab"] = function()
  local tabline = require("forge.views.harness.tabline")
  local named = vim.fn.tempname() .. "-working.md"
  vim.cmd.edit(vim.fn.fnameescape(named))
  local float = vim.api.nvim_open_win(buffer.scratch({ "decision" }), true,
    { relative = "editor", row = 1, col = 1, width = 20, height = 1 })
  MiniTest.finally(function() pcall(vim.api.nvim_win_close, float, true) end)
  local rendered = tabline.render()
  eq({ rendered:find(vim.fn.fnamemodify(named, ":t"), 1, true) ~= nil, rendered:find("[No Name]", 1, true) }, { true, nil })
end

return T
