vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.o.columns, vim.o.lines = 130, 50
require("forge").setup({ harness = { backend = "mock" } })
local client = require("forge.client")
local store = require("forge.integrations.background_settings")
local settings = require("forge.views.harness.settings")
local picker = require("forge.views.picker")
local fail_save = false
local revision = 0
local preferences = { auto_compaction = { enabled = true, threshold_tokens = 128000 }, models = {
  codex = { model = "gpt-6-luna", thinking = "low" },
} }
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
store.request = function(snapshot, changed, callback)
  if changed then
    if fail_save then callback(nil, "database denied") return end
    assert(snapshot.revision == revision)
    preferences, revision = vim.deepcopy(changed), revision + 1
  end
  callback({ revision = revision, settings = vim.deepcopy(preferences) })
end
client.request_for = function(_, method, _, callback)
  if method == "trace.status" then callback({ enabled = false })
  elseif method == "backend.models" then callback({
    { id = "gpt-6-luna", reasoning = { "low", "medium", "high" }, default_reasoning = "low" },
    { id = "second-model", reasoning = { "low", "high" }, default_reasoning = "low" },
  })
  else error("Commit settings used a conversation mutation: " .. method) end
end
local state = { session = { id = "background-config", backend = "codex", model = "chat-model", effort = "high" },
  capability = { os_sandbox = true } }
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
local function open()
  settings.open(state, host)
  return picker._state_for_test()
end
local function option(id)
  for _, entry in ipairs(picker._state_for_test().spec.page_list[1].option_list) do
    if entry.id == id then return entry end
  end
  error("Missing option " .. id)
end
local function change(id)
  local instance = picker._state_for_test()
  instance.spec.action_list[2].callback({ option = option(id) })
end
open()
assert(option("background-model").columns[2]:find("gpt-6-luna", 1, true))
assert(not pcall(option, "background-provider"), "provider selector should be removed")
change("background-thinking")
assert(preferences.models.codex.thinking == "medium")
change("background-compact")
assert(not preferences.auto_compaction.enabled)
picker._state_for_test().spec.on_confirm({ option = option("background-model") })
local model_picker = picker._state_for_test()
model_picker.spec.on_confirm({ option = { value = { id = "second-model", picker_reasoning = "high" } } })
assert(preferences.models.codex.model == "second-model" and preferences.models.codex.thinking == "high")
assert(picker.is_open("harness-config"), "model selection did not return to config")
fail_save = true
change("background-thinking")
assert(errors[#errors] == "database denied", "save failure was silent")
assert(preferences.models.codex.thinking == "high", "failed save replaced prior settings")
assert(state.session.model == "chat-model" and state.session.effort == "high", "commit config changed the active chat")
picker.close()
open()
assert(option("background-model").columns[2]:find("second-model", 1, true))
picker.close()
print("Background settings: model, thinking, compaction defaults, persistence, errors, and chat isolation passed")
vim.cmd("qa!")
