-- Unit tests for `forge.views.harness.composer_interrupt` (views/harness/composer_interrupt.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- Neovim delivers Ctrl-C as an interrupt, bypassing the Insert-mode mapping, when the key arrives
-- while the main loop is busy. A child without that mapping receives every Ctrl-C the same way:
-- `vim.on_key` observes the key, and the built-in `i_CTRL-C` then leaves Insert mode.
T["unmapped_interrupt_cancels_and_restores_insert_mode"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    _G.cancel_count = 0
    local buffer = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_lines(buffer, 0, -1, true, { "draft text" })
    require("forge.views.harness.composer_interrupt").attach(buffer,
      function() return true end,
      function() _G.cancel_count = _G.cancel_count + 1 end)
  ]])
  local function mode() return child.api.nvim_get_mode().mode end
  local function interrupt_at(column, cancel_count)
    child.api.nvim_win_set_cursor(0, { 1, column })
    child.type_keys("<C-c>")
    wait_until(function() return child.lua_get("_G.cancel_count") == cancel_count end,
      "the interrupt never reached the cancel callback")
    wait_until(function() return mode() == "i" end,
      "the composer stayed in Normal mode after the interrupt")
    MiniTest.expect.equality(child.api.nvim_win_get_cursor(0), { 1, column })
  end

  child.type_keys("i")
  MiniTest.expect.equality(mode(), "i")
  -- Inside the line, Insert mode resumes before the same character.
  interrupt_at(5, 1)
  -- Past the last character, Insert mode resumes at the append position.
  interrupt_at(#"draft text", 2)
  MiniTest.expect.equality(child.api.nvim_buf_get_lines(0, 0, -1, true), { "draft text" })
end

return T
