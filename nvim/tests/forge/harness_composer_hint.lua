vim.loader.enable(false)
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local commands = require("forge.views.harness.controller").command_set()
local original = vim.deepcopy(config.options.keymaps.harness)
local buffer = vim.api.nvim_create_buf(false, true)
local ok, failure = xpcall(function()
  local function display(state, width)
    return vim.api.nvim_eval_statusline(keymaps.render_hintbar(
      keymaps.view_hint_entries("harness", commands, state, "composer"), width or 140),
      { use_winbar = true, maxwidth = width or 140 }).str
  end
  assert(display({}):find("<C-s> send", 1, true))
  assert(display({}):find("? help (normal)", 1, true))
  local busy = { busy = true, capability = { native_steer = true }, queue = { "next" } }
  local text = display(busy)
  for _, expected in ipairs({ "<C-s> queue", "<C-q> steer", "<C-c> cancel", "<M-s> edit queued" }) do
    assert(text:find(expected, 1, true), text)
  end
  assert(display(busy, 50):find("<C-q> steer", 1, true))
  busy.active_wait = {}
  assert(display(busy):find("<C-s> steer", 1, true))
  busy.selected_agent_run_id = "child"
  assert(display(busy):find("<C-s> steer child", 1, true))
  config.options.keymaps.harness.submit = "<F5>"
  config.options.keymaps.harness.steer = false
  text = display(busy)
  assert(text:find("<F5> steer child", 1, true))
  assert(not text:find("<C-q>", 1, true))
  keymaps.setup_view_keymaps(buffer, "harness", commands)
  vim.api.nvim_buf_call(buffer, function()
    assert(vim.fn.maparg("<F5>", "i", false, true).buffer == 1)
    assert(vim.fn.maparg("<C-s>", "i", false, true).buffer ~= 1)
    assert(vim.fn.maparg("<C-q>", "i", false, true).buffer ~= 1)
  end)
  require("forge.shared.view_command_set").unregister(commands, "submit")
  assert(not display(busy):find("<F5>", 1, true), "hint advertised an unregistered action")
  local escaped = keymaps.render_hintbar({ { id = "help", key = "%", label = "100% help" } }, 80)
  assert(vim.api.nvim_eval_statusline(escaped, { use_winbar = true, maxwidth = 80 }).str:find("% 100% help", 1, true))
end, debug.traceback)
config.options.keymaps.harness = original
vim.api.nvim_buf_delete(buffer, { force = true })
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_composer_hint: passed") vim.cmd("qa!") end
