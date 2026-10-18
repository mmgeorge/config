vim.loader.enable(false)
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local controller = require("forge.views.harness.controller")
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
local commands = controller.composer_command_set()
keymaps.setup_view_keymaps(buffer, "harness", commands)
for _, id in ipairs({ "help", "close", "toggle_activity", "open_timeline", "open_artifact",
  "previous_prompt", "next_prompt", "sessions", "background" }) do
  assert(commands.action_by_id[id] == nil, "composer inherited transcript command: " .. id)
end
for _, key in ipairs({ "d", "c", "x", "p", "P", "q", "?", "<Tab>", "<CR>", "<Up>", "<Down>" }) do
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.buffer ~= 1, "composer shadowed ordinary editing key: " .. key)
end
for _, id in ipairs({ "submit", "queue", "cancel", "edit_queued", "next_queued", "toggle_mode", "history_previous", "history_next" }) do
  for _, key in ipairs(keymaps.view_keys_for("harness", id)) do
    assert(vim.fn.maparg(key, "n", false, true).buffer == 1, "missing composer control: " .. id)
  end
end
config.options.keymaps.harness.submit = "<M-CR>"
local custom = vim.api.nvim_create_buf(false, true)
keymaps.setup_view_keymaps(custom, "harness", controller.composer_command_set())
vim.api.nvim_set_current_buf(custom)
assert(vim.fn.maparg("<M-CR>", "n", false, true).buffer == 1, "custom submit binding was ignored")
print("harness_composer_keymaps: passed")
