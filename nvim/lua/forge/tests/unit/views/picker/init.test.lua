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

---Install a stubbed loop clock and the deterministic release timer of the settle guard.
local function settle_fixture()
  require("forge.infra.config").setup({})
  local clock = { now = 1000 }
  stub(vim.uv, "now", function() return clock.now end)
  local timers = dofile("nvim/lua/forge/tests/support/deferred.lua").install({ 350 })
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  ---Advance the clock and run every release timer that is due.
  function clock.advance(ms)
    clock.now = clock.now + ms
    timers.fire(350)
  end
  return clock, picker
end

---@param picker table
---@return string text and border footer of the open picker
local function shown(picker)
  local instance = picker._state_for_test()
  return table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
    .. vim.inspect(vim.api.nvim_win_get_config(instance.win).footer)
end

T["a picker opened while typing discards keys until typing stops"] = function()
  local clock, picker = settle_fixture()
  local origin_win = vim.api.nvim_get_current_win()
  type_keys("h")
  clock.now = clock.now + 20
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  type_keys("n<CR>")
  eq(decision_list, {})
  eq(picker.is_open(), true)
  eq(shown(picker):find("Waiting for typing to stop", 1, true) ~= nil, true)
  -- Each discarded key restarts the interval, so a key just inside it is still discarded.
  clock.advance(300)
  type_keys("n")
  clock.advance(300)
  type_keys("<CR>")
  eq(decision_list, {})
  clock.advance(351)
  eq(shown(picker):find("Waiting for typing to stop", 1, true), nil)
  type_keys("n<CR>")
  eq(decision_list, { "no" })
end

-- The notice and the discard are one state: a key the guard lets through is one the footer no
-- longer warns about. A key that arrives after the interval but before the release has run, as
-- when the main loop is busy, is discarded like the keys before it, and so are multi-key
-- mappings, global ones included.
T["a settling picker stays inert until its release runs"] = function()
  local clock, picker = settle_fixture()
  local origin_win = vim.api.nvim_get_current_win()
  local global_ran = 0
  vim.keymap.set("n", "on", function() global_ran = global_ran + 1 end)
  MiniTest.finally(function() pcall(vim.keymap.del, "n", "on") end)
  type_keys("h")
  clock.now = clock.now + 20
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  clock.now = clock.now + 400
  type_keys("on")
  type_keys("n<CR>")
  eq({ decision_list, global_ran, picker.is_open() }, { {}, 0, true })
  eq(shown(picker):find("Waiting for typing to stop", 1, true) ~= nil, true)
end

-- A guarded picker opens by itself, so a key already on its way, even after a pause, must not
-- decide; one such Enter saved an "always allow" rule. Every guarded open discards keys for the
-- interval.
T["a guarded picker discards keys for the interval after it opens"] = function()
  local clock, picker = settle_fixture()
  local origin_win = vim.api.nvim_get_current_win()
  type_keys("h")
  clock.now = clock.now + 5000
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  type_keys("n<CR>")
  eq({ decision_list, picker.is_open() }, { {}, true })
  clock.advance(351)
  type_keys("n<CR>")
  eq(decision_list, { "no" })
end

-- An Enter that chose nothing, such as one on an approval with no decision selected, left the
-- picker marked as decided, so its later expiry let the next keys through to the composer.
T["keys after a picker that expired following an undecided Enter do not reach the window beneath"] = function()
  local clock, picker = settle_fixture()
  local origin_win = vim.api.nvim_get_current_win()
  local origin_buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(origin_buf, 0, -1, false, { "" })
  local spec = settling_choice(origin_win, {})
  spec.on_confirm = function() return false end
  picker.open(spec)
  clock.advance(351)
  type_keys("<CR>")
  eq(picker.is_open(), true)
  clock.now = clock.now + 2000
  picker.close(true)
  type_keys("ix<Esc>")
  eq(vim.api.nvim_buf_get_lines(origin_buf, 0, -1, false), { "" })
end

-- The picker buffer is not modifiable, so an editing command it does not map raised E21 into
-- `:messages`; the picker takes such keys without effect.
T["unmapped editing keys do nothing in a picker"] = function()
  require("forge.infra.config").setup({})
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  local origin_win = vim.api.nvim_get_current_win()
  picker.open({ host = { control_win = origin_win, window_list = { origin_win } },
    page_list = { { id = "choice", option_list = { { id = "only", label = "Only" } } } } })
  local buf = picker._state_for_test().buf
  local before = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.cmd("messages clear")
  local typed, failure = pcall(type_keys, "oY")
  eq({ typed, failure }, { true, nil })
  eq(vim.fn.execute("messages"):find("E21", 1, true), nil)
  eq({ picker.is_open(), vim.api.nvim_buf_get_lines(buf, 0, -1, false) }, { true, before })
end

-- A global mapping is no more the picker's than a built-in command: `Y` mapped to `P` pasted into
-- the unmodifiable list and raised E21, and so did a global `o` that opens a line. Only the
-- picker's own buffer mappings, the command line, and special keys reach the list.
T["global mappings that edit do nothing in a picker"] = function()
  require("forge.infra.config").setup({})
  local picker = require("forge.views.picker")
  vim.keymap.set("n", "Y", "P")
  vim.keymap.set("n", "o", "o", { noremap = true })
  vim.keymap.set("n", "X", "ddp")
  MiniTest.finally(function()
    pcall(picker.close, false)
    for _, key in ipairs({ "Y", "o", "X" }) do pcall(vim.keymap.del, "n", key) end
  end)
  vim.fn.setreg('"', "pasted")
  local origin_win = vim.api.nvim_get_current_win()
  local decision_list = {}
  picker.open({ host = { control_win = origin_win, window_list = { origin_win } },
    page_list = { { id = "choice", option_list = { { id = "only", key = "y", label = "Only", value = "only" } } } },
    on_confirm = function(result) decision_list[#decision_list + 1] = result.option.value end })
  local buf = picker._state_for_test().buf
  local before = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.cmd("messages clear")
  local typed, failure = pcall(type_keys, "YoX")
  eq({ typed, failure }, { true, nil })
  eq(vim.fn.execute("messages"):find("E21", 1, true), nil)
  eq({ picker.is_open(), vim.api.nvim_buf_get_lines(buf, 0, -1, false) }, { true, before })
  -- The picker's own mappings still work after the discarded ones.
  type_keys("y<CR>")
  eq(decision_list, { "only" })
end

-- Insert mode is global, so returning from a tab where the user was typing put the picker that
-- held focus in its own tab into Insert mode: its hotkeys then edited the unmodifiable list and
-- raised E21. The picker takes its keys again once its tab is entered.
T["a picker keeps its keys after its tab is left from insert mode and entered again"] = function()
  -- Insert mode only persists across a tab change between typed keys, so a child Neovim receives them.
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    require("forge.infra.config").setup({})
    _G.decision_list = {}
    local origin_win = vim.api.nvim_get_current_win()
    require("forge.views.picker").open({ host = { control_win = origin_win, window_list = { origin_win } },
      page_list = { { id = "choice", option_list = { { id = "only", key = "y", label = "Only", value = "only" } } } },
      on_confirm = function(result) table.insert(_G.decision_list, result.option.value) end })
    _G.picker_win = vim.api.nvim_get_current_win()
    vim.cmd("tabnew")
  ]])
  -- The user types in the other tab and returns while still in Insert mode.
  child.type_keys("i", "<Cmd>tabprevious<CR>")
  eq(child.lua_get("vim.api.nvim_get_current_win() == _G.picker_win"), true)
  child.type_keys("y", "<CR>")
  eq({ child.api.nvim_get_mode().mode, child.lua_get("_G.decision_list"), child.lua_get("vim.v.errmsg") },
    { "n", { "only" }, "" })
end

T["keys after a guarded picker closed by itself do not reach the window beneath"] = function()
  local clock, picker = settle_fixture()
  local origin_win = vim.api.nvim_get_current_win()
  local origin_buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(origin_buf, 0, -1, false, { "" })
  local decision_list = {}
  picker.open(settling_choice(origin_win, decision_list))
  clock.now = clock.now + 2000
  -- An owner closes the picker without a decision, as an expiry or a host resolution does, after
  -- a pause in which no key reached it.
  picker.close(true)
  eq(vim.api.nvim_get_current_win(), origin_win)
  clock.now = clock.now + 150
  type_keys("ix<Esc>")
  eq(vim.api.nvim_buf_get_lines(origin_buf, 0, -1, false), { "" })
  clock.advance(351)
  type_keys("iy<Esc>")
  eq(vim.api.nvim_buf_get_lines(origin_buf, 0, -1, false), { "y" })
  eq(decision_list, {})
end

-- A list taller than its window scrolls with the selection, so rows past the window's edge looked
-- absent rather than hidden: the session picker seemed to hold only the sessions that fit. The
-- border says which rows the window shows and how many the list holds.
T["a list taller than its window says which rows it shows"] = function()
  require("forge.infra.config").setup({})
  local picker = require("forge.views.picker")
  MiniTest.finally(function() pcall(picker.close, false) end)
  local origin_win = vim.api.nvim_get_current_win()
  local option_list = {}
  for index = 1, 20 do option_list[index] = { id = "row-" .. index, label = "Row " .. index, value = index } end
  picker.open({ host = { control_win = origin_win, window_list = { origin_win } }, size_ratio = 0.35,
    placement = "bottom", command_footer = true,
    page_list = { { id = "rows", title = "Rows", option_list = option_list } } })
  local function footer() return shown(picker):match("Rows %d+–%d+ of %d+") end
  local first_footer = assert(footer(), "the border did not count the hidden rows: " .. shown(picker))
  local first, last = first_footer:match("Rows (%d+)–(%d+) of 20")
  eq(first, "1")
  type_keys(string.rep("<Down>", 19))
  eq(footer(), ("Rows %d–20 of 20"):format(20 - tonumber(last) + 1))
end

return T
