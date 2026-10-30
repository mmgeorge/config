vim.loader.enable(false)
local config = require("forge.infra.config")
local keymaps = require("forge.shared.keymaps")
local command_specs = require("forge.shared.command_specs")
local controller = require("forge.views.harness.controller")
local dispatched = {}
controller.open_artifact_picker = function() dispatched[#dispatched + 1] = "open_artifact" end
controller.open_agent_picker = function() dispatched[#dispatched + 1] = "agent" end
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
local commands = controller.composer_command_set()
keymaps.setup_view_keymaps(buffer, "harness", commands)
local transcript_only = { visual_line_with_gutter = true, open_timeline = true, toggle_visibility = true, toggle_presentation = true }
for id in pairs(transcript_only) do
  assert(commands.action_by_id[id] == nil, "composer inherited transcript command: " .. id)
end
for _, key in ipairs({ "d", "c", "x", "p", "P", "W", ".", "<CR>", "ov", "ox" }) do
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.buffer ~= 1, "composer shadowed ordinary editing key: " .. key)
end
for _, spec in ipairs(command_specs.view_specs.harness) do
  if not transcript_only[spec.id] then
    assert(commands.action_by_id[spec.id], "missing composer command: " .. spec.id)
    local modes = type(spec.modes) == "table" and spec.modes or { spec.modes or "n" }
    for _, key in ipairs(keymaps.view_keys_for("harness", spec.id)) do
      for _, mode in ipairs(modes) do
        assert(vim.fn.maparg(key, mode, false, true).buffer == 1,
          "missing composer binding: " .. spec.id .. " in " .. mode)
      end
      if not vim.tbl_contains(modes, "i") then
        assert(vim.fn.maparg(key, "i", false, true).buffer ~= 1,
          "session command shadowed Insert-mode typing: " .. spec.id)
      end
    end
  end
end
vim.fn.maparg("op", "n", false, true).callback()
vim.fn.maparg("og", "n", false, true).callback()
assert(vim.deep_equal(dispatched, { "open_artifact", "agent" }), "session hotkeys did not dispatch")
local transcript = controller.command_set()
for _, spec in ipairs(command_specs.view_specs.harness) do
  if spec.id ~= "history_previous" and spec.id ~= "history_next" then
    assert(transcript.action_by_id[spec.id], "missing transcript command: " .. spec.id)
  end
end
config.options.keymaps.harness.submit = "<M-CR>"
config.options.keymaps.harness.open_artifact = "<F6>"
config.options.keymaps.harness.agent = false
local custom = vim.api.nvim_create_buf(false, true)
keymaps.setup_view_keymaps(custom, "harness", controller.composer_command_set())
vim.api.nvim_set_current_buf(custom)
assert(vim.fn.maparg("<M-CR>", "n", false, true).buffer == 1, "custom submit binding was ignored")
assert(vim.fn.maparg("<F6>", "n", false, true).buffer == 1, "custom artifact binding was ignored")
assert(vim.fn.maparg("op", "n", false, true).buffer ~= 1, "default artifact binding survived override")
assert(vim.fn.maparg("og", "n", false, true).buffer ~= 1, "disabled agent binding was installed")
print("harness_composer_keymaps: passed")
