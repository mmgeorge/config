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
local state = { session = { id = "background-config", backend = "codex", model = "chat-model", effort = "high", service_tier = "default" },
  capability = { os_sandbox = true, fast_mode = true, ultrafast_mode = true } }
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
local function open()
  settings.open(state, host)
  return picker._state_for_test()
end
local function option(id)
  for _, entry in ipairs(picker._state_for_test().spec.page_list[1].option_list) do
    for _, field in ipairs(entry.fields or {}) do if field.id == id then return field end end
  end
  error("Missing option " .. id)
end
local function change(id)
  local instance = picker._state_for_test()
  instance.spec.on_field_action({ field = option(id) }, option(id).kind == "toggle" and "toggle" or "change", 1)
end
open()
assert(option("background-model").value:find("gpt-6-luna", 1, true))
assert(not pcall(option, "background-provider"), "provider selector should be removed")
change("background-thinking")
assert(preferences.models.codex.thinking == "medium")
change("background-speed")
assert(preferences.models.codex.service_tier == "fast")
change("background-speed")
assert(preferences.models.codex.service_tier == "ultrafast")
change("background-compact")
assert(not preferences.auto_compaction.enabled)
picker._state_for_test().spec.on_field_action({ field = option("background-model") }, "activate", 1)
local model_picker = picker._state_for_test()
model_picker.spec.on_confirm({ option = { value = { id = "second-model", picker_reasoning = "high" } } })
assert(preferences.models.codex.model == "second-model" and preferences.models.codex.thinking == "high")
assert(preferences.models.codex.service_tier == "ultrafast", "model change lost Background speed")
assert(picker.is_open("harness-config"), "model selection did not return to config")
fail_save = true
change("background-thinking")
assert(errors[#errors] == "database denied", "save failure was silent")
assert(preferences.models.codex.thinking == "high", "failed save replaced prior settings")
change("background-speed")
assert(preferences.models.codex.service_tier == "ultrafast", "failed save replaced Background speed")
assert(state.session.model == "chat-model" and state.session.effort == "high", "commit config changed the active chat")
assert(state.session.service_tier == "default", "Background speed changed the active chat")
picker.close()
open()
assert(option("background-model").value:find("second-model", 1, true))
assert(option("background-speed").value == "ultrafast")
picker.close()
state.capability.fast_mode, state.capability.ultrafast_mode = false, false
open()
assert(not pcall(option, "background-speed"), "unsupported provider exposed Background speed")
picker.close()
print("Background settings: model, thinking, compaction defaults, persistence, errors, and chat isolation passed")
vim.cmd("qa!")
