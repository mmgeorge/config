vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.o.columns, vim.o.lines = 130, 50
-- A Background-settings failure belongs to the Background rows; the main and Ask model controls
-- read the main session's provider catalog and must stay usable.
require("forge").setup({ harness = { provider = "mock" } })
local client = require("forge.client")
dofile("nvim/tests/forge/support/provider_catalog.lua").install()
local store = require("forge.integrations.background_settings")
local settings = require("forge.views.harness.settings")
local picker = require("forge.views.picker")
local controller = require("forge.views.harness.controller")
local errors = {}
local catalog_session_list = {}
local configured = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
store.request = function(_, _, callback) callback(nil, "Unknown Harness provider: codex") end
client.request = function(request_specification, callback)
  local method = request_specification.method
  if method == "host.trace.status" then callback({ enabled = false })
  elseif method == "session.provider.model.list" then
    catalog_session_list[#catalog_session_list + 1] = request_specification.session_id
    callback({
      { id = "chat-model", reasoning = { "low", "high" }, default_reasoning = "low" },
      { id = "second-model", reasoning = { "low", "medium" }, default_reasoning = "medium" },
    })
  else error("Unexpected request: " .. method) end
end
local state = { session = { id = "main-session", provider = "fixture_alpha", model = "chat-model", effort = "high",
  service_tier = "default" }, capability = { model_selection = true, effort_selection = true } }
controller.configure = function(params, completed)
  configured[#configured + 1] = vim.deepcopy(params)
  state.session = vim.tbl_extend("force", state.session, params)
  completed(true)
end
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
local function option(id)
  for _, entry in ipairs(picker._state_for_test().spec.page_list[1].option_list) do
    for _, field in ipairs(entry.fields or {}) do if field.id == id then return field end end
  end
  error("Missing option " .. id)
end

settings.open(state, host)
assert(errors[1] == "Unknown Harness provider: codex", "Background failure was silent")
assert(option("background-model").disabled_reason == "Unknown Harness provider: codex",
  "Background rows must show their own failure")
assert(option("main-model").disabled_reason == nil,
  "Background failure disabled the main model: " .. tostring(option("main-model").disabled_reason))
assert(option("main-thinking").disabled_reason == nil, "Background failure disabled main thinking")
assert(option("ask-override").disabled_reason == nil, "Background failure disabled the Ask override")
assert(vim.tbl_contains(catalog_session_list, "main-session"), "main catalog was not requested for the main session")
picker._state_for_test().spec.on_field_action({ field = option("main-model") }, "change", 1)
assert(configured[#configured] and configured[#configured].model == "second-model",
  "main model did not cycle through the main provider catalog")
picker._state_for_test().spec.on_field_action({ field = option("main-thinking") }, "change", 1)
assert(configured[#configured].effort == "low", "main thinking did not use the main catalog descriptor")
picker.close()
print("Background failure leaves main model controls usable passed")
vim.cmd("qa!")
