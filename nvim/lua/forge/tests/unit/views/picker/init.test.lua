-- Unit tests for `forge.views.picker` (init.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["a picker opened from another picker returns to where the chain began"] = function()
  require("forge.infra.config").setup({})
  local popup_window = require("forge.infra.popup_window")
  local picker = require("forge.views.picker")
  local origin_win = vim.api.nvim_get_current_win()
  local capture = popup_window.capture_origin
  -- The first picker opens from Insert mode; any later capture sees the Normal mode a picker uses.
  local captured = 0
  stub(popup_window, "capture_origin", function()
    captured = captured + 1
    local origin = capture()
    if captured == 1 then origin.mode = "i" end
    return origin
  end)
  local restored = {}
  stub(popup_window, "restore_origin", function(origin) restored[#restored + 1] = vim.deepcopy(origin) end)
  MiniTest.finally(function() pcall(picker.close, false) end)
  local function choice(title, on_confirm)
    return { host = { control_win = origin_win, window_list = { origin_win } },
      page_list = { { id = title, title = title, option_list = { { id = "only", label = "Only", value = "only" } } } },
      on_confirm = on_confirm }
  end
  picker.open(choice("First", function() picker.open(choice("Second", function() end)) end))
  vim.api.nvim_feedkeys(vim.keycode("<CR>"), "x", false)
  eq(picker.is_open(), true)
  picker.close(true)
  eq(restored[#restored], { win = origin_win, mode = "i" })
end

-- A picker that takes focus from someone typing elsewhere, such as a permission review opening
-- over the composer, must not take the typed keys as decisions.
local function settling_choice(origin_win, decision_list)
  return {
    host = { control_win = origin_win, window_list = { origin_win } },
    input_settle_ms = 350,
    page_list = { { id = "decision", title = "Decision", option_list = {
      { id = "yes", key = "y", label = "Yes", value = "yes" },
      { id = "no", key = "n", label = "No", value = "no" },
    } } },
    on_confirm = function(result) decision_list[#decision_list + 1] = result.option.value end,
  }
end

local function type_keys(keys) vim.api.nvim_feedkeys(vim.keycode(keys), "xt", false) end

T["a picker opened while typing discards keys until typing stops"] = function()
  require("forge.infra.config").setup({})
  local now = 1000
  stub(vim.uv, "now", function() return now end)
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  local origin_win = vim.api.nvim_get_current_win()
  type_keys("h")
  now = now + 20
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  type_keys("n<CR>")
  eq(decision_list, {})
  eq(picker.is_open(), true)
  local instance = picker._state_for_test()
  local function shown()
    return table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
      .. vim.inspect(vim.api.nvim_win_get_config(instance.win).footer)
  end
  eq(shown():find("Waiting for typing to stop", 1, true) ~= nil, true)
  -- Each discarded key restarts the interval, so a key just inside it is still discarded.
  now = now + 300
  type_keys("n")
  now = now + 300
  type_keys("<CR>")
  eq(decision_list, {})
  now = now + 351
  type_keys("n")
  vim.wait(0)
  eq(shown():find("Waiting for typing to stop", 1, true), nil)
  type_keys("<CR>")
  eq(decision_list, { "no" })
end

T["a picker opened with no recent key takes keys at once"] = function()
  require("forge.infra.config").setup({})
  local now = 1000
  stub(vim.uv, "now", function() return now end)
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  local origin_win = vim.api.nvim_get_current_win()
  type_keys("h")
  now = now + 351
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  type_keys("n<CR>")
  eq(decision_list, { "no" })
end

T["keys aimed at a picker that closed by itself do not reach the window beneath"] = function()
  require("forge.infra.config").setup({})
  local now = 1000
  stub(vim.uv, "now", function() return now end)
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  local origin_win = vim.api.nvim_get_current_win()
  local origin_buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(origin_buf, 0, -1, false, { "" })
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  type_keys("j")
  now = now + 20
  -- An owner closes the picker without a decision, as an expiry or a host resolution does.
  picker.close(true)
  eq(vim.api.nvim_get_current_win(), origin_win)
  type_keys("ix<Esc>")
  eq(vim.api.nvim_buf_get_lines(origin_buf, 0, -1, false), { "" })
  now = now + 351
  type_keys("iy<Esc>")
  eq(vim.api.nvim_buf_get_lines(origin_buf, 0, -1, false), { "y" })
  eq(decision_list, {})
end

return T
