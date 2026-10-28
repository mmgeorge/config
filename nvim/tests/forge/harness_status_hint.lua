vim.opt.runtimepath:prepend("nvim")
local hint = require("forge.views.harness.status_hint")
local commands = require("forge.shared.view_command_set")
local config = require("forge.infra.config")
local actions = commands.new()
for _, action in ipairs({ "open_artifact", "cancel", "background", "reopen_question", "abort_plan" }) do
  commands.register(actions, action, function() end)
end
local snapshot = { block = { { text = { "Waiting for plan review" }, metadata = {
  status = { row = 0, animated = false, hint = "review" }, decoration = {},
} } } }
local initial = vim.deepcopy(snapshot)
local function displayed(context) return table.concat(hint.compose(snapshot, context or {}, actions).lines, "\n") end
assert(displayed():find("op open plan", 1, true))
assert(displayed():find("or abort plan", 1, true))
local original = config.options.keymaps.harness.open_artifact
config.options.keymaps.harness.open_artifact = "<F6>"
assert(displayed():find("<F6> open plan", 1, true))
config.options.keymaps.harness.open_artifact = false
assert(not displayed():find("open plan", 1, true))
config.options.keymaps.harness.open_artifact = original
local notice = ("Long Unicode λ notice\n"):rep(500) .. "final notice"
assert(displayed({ message_notice = { text = notice, failed = true } }):find("final notice", 1, true))
assert(vim.deep_equal(snapshot, initial), "local notices mutated the native snapshot")
assert(displayed({ rename_status = "Renaming" }):find("Renaming", 1, true))
assert(displayed({ restore_recovery = {} }):find("Restore interrupted", 1, true))
assert(displayed({ background_terminals = { supported = true, terminal = { {} } } }):find("1 terminal running", 1, true))
assert(displayed({ recap = { text = "complete recap" } }):find("complete recap", 1, true))
local empty = hint.compose({ block = {} }, {}, actions)
assert(#empty.lines == 0)
assert(#hint.compose({ block = {} }, { status_notice = { text = "Disconnected" } }, actions).lines == 1)
print("harness_status_hint: passed")
