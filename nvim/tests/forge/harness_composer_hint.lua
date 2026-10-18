vim.loader.enable(false)
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local commands = require("forge.views.harness.controller").command_set()
local composer_commands = require("forge.views.harness.controller").composer_command_set()
local original = vim.deepcopy(config.options.keymaps.harness)
local buffer = vim.api.nvim_create_buf(false, true)
local ok, failure = xpcall(function()
  local pinned = keymaps.view_hint_entries("harness", commands)
  assert(#pinned == 1 and pinned[1].id == "help", "top bar must advertise only help")
  local context = { { text = "75% left (200K)", group = "ForgeStatusHint" } }
  local function top_bar(status)
    keymaps.apply_view_winbar(0, "", "harness", commands, status, nil, context)
    return vim.api.nvim_eval_statusline(vim.wo.winbar, { use_winbar = true, maxwidth = 2000 }).str
  end
  for _, status in ipairs({ "Read", string.rep("Read ", vim.api.nvim_win_get_width(0)) }) do
    local text = top_bar(status)
    assert(text:find("75% left (200K) • ? help", 1, true), "context must precede help in wide and compact bars")
    assert(not text:find("submit", 1, true), "top bar contains additional command hints")
  end
  config.options.keymaps.harness.help = "<F1>"
  assert(top_bar("Read"):find("<F1> help", 1, true), "help hint must use the configured binding")
  config.options.keymaps.harness.help = false
  assert(not top_bar("Read"):find("help", 1, true), "disabled help must not be advertised")
  config.options.keymaps.harness.help = original.help
  local function display(state, width)
    return vim.api.nvim_eval_statusline(keymaps.render_hintbar(
      keymaps.view_hint_entries("harness", composer_commands, state, "composer"), width or 140),
      { use_winbar = true, maxwidth = width or 140 }).str
  end
  assert(display({}):find("<C-s> submit", 1, true))
  assert(not display({}):find("help", 1, true))
  assert(not display({}):find("q close", 1, true))
  assert(not display({}):find("edit queued", 1, true))
  local busy = { busy = true, capability = { native_steer = true }, queue = { "next" } }
  local text = display(busy)
  for _, expected in ipairs({ "<C-s> submit", "<C-q> queue", "<C-c> cancel", "<M-s> edit queued" }) do
    assert(text:find(expected, 1, true), text)
  end
  assert(display(busy, 50):find("<M-s> edit queued", 1, true), "queued editing must survive narrow composer hints")
  busy.queue = {}
  assert(not display(busy, 50):find("edit queued", 1, true), "empty queue must clear the editing hint")
  busy.queue = { "next" }
  busy.queue_edit = {}
  assert(display(busy):find("<C-q> unqueue", 1, true), "editing must advertise moving the message back to the prompt")
  assert(display(busy):find("<M-t> next queued", 1, true), "queue editing must advertise forward navigation")
  local editing_hints = keymaps.view_hint_entries("harness", commands, busy, "queue_edit")
  assert(#editing_hints == 3 and editing_hints[1].id == "queue")
  assert(editing_hints[3].id == "next_queued" and editing_hints[3].key == "<M-t>")
  config.options.keymaps.harness.queue = "<F6>"
  assert(display(busy):find("<F6> unqueue", 1, true), "unqueue must use the configured queue binding")
  config.options.keymaps.harness.queue = original.queue
  busy.queue_edit = nil
  config.options.keymaps.harness.edit_queued = "<M-Up>"
  assert(display(busy, 50):find("<M-Up> edit queued", 1, true), "queue hint must use the configured binding")
  config.options.keymaps.harness.edit_queued = false
  assert(not display(busy):find("edit queued", 1, true), "disabled binding must hide the queue hint")
  config.options.keymaps.harness.edit_queued = original.edit_queued
  busy.active_wait = {}
  assert(display(busy):find("<C-s> submit", 1, true))
  busy.selected_agent_run_id = "child"
  assert(display(busy):find("<C-s> submit", 1, true))
  config.options.keymaps.harness.submit = "<F5>"
  config.options.keymaps.harness.queue = false
  text = display(busy)
  assert(text:find("<F5> submit", 1, true))
  assert(not text:find("<C-q>", 1, true))
  keymaps.setup_view_keymaps(buffer, "harness", composer_commands)
  vim.api.nvim_buf_call(buffer, function()
    assert(vim.fn.maparg("q", "n", false, true).buffer ~= 1)
    assert(vim.fn.maparg("<F5>", "i", false, true).buffer == 1)
    assert(vim.fn.maparg("<C-s>", "i", false, true).buffer ~= 1)
    assert(vim.fn.maparg("<C-q>", "i", false, true).buffer ~= 1)
  end)
  require("forge.shared.view_command_set").unregister(composer_commands, "submit")
  assert(not display(busy):find("<F5>", 1, true), "hint advertised an unregistered action")
  local escaped = keymaps.render_hintbar({ { id = "help", key = "%", label = "100% help" } }, 80)
  assert(vim.api.nvim_eval_statusline(escaped, { use_winbar = true, maxwidth = 80 }).str:find("% 100% help", 1, true))
end, debug.traceback)
config.options.keymaps.harness = original
vim.api.nvim_buf_delete(buffer, { force = true })
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_composer_hint: passed") vim.cmd("qa!") end
