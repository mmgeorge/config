local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["standalone_defaults_do_not_touch_live_session"] = function()
  vim.o.columns, vim.o.lines = 140, 55
  require("forge").setup({ harness = { provider = "mock" } })
  local client = require("forge.client")
  local session = require("forge.session")
  local picker = require("forge.views.picker")
  local live = { id = "live-session", model = "live-model", approval_mode = "ask" }
  session.harness.session = vim.deepcopy(live)
  require("forge.harness.provider_preference").load = function() return "mock" end
  client.session_initialize = function() error("global settings created a session") end
  local revision, saved = 0, { status_thoughts = true, check_editing = true }
  local provider_saved = { model = "mock-model" }
  local writes = {}
  local catalog_failure = false
  local catalog = { capability = { model_selection = true, effort_selection = true, native_compact = true },
    models = {
      { id = "mock-model", reasoning = { "low", "high" }, default_reasoning = "low" },
      { id = "other-model", reasoning = { "low", "high" }, default_reasoning = "low" },
    } }
  require("forge.integrations.background_settings").request = function(_, _, callback)
    callback({ revision = 0, settings = { provider = "mock", models = { mock = { model = "mock-model" } },
      auto_compaction = { enabled = true, threshold_tokens = 128000 } } })
  end
  client.request = function(request, callback)
    assert(request.session_id == nil, "global configuration sent a session-bound request")
    if request.method == "settings.get" then
      if request.params.include_models and catalog_failure then callback(nil, "catalog offline") return end
      callback({ revision = revision, layers = { global = vim.deepcopy(saved) },
        selected_provider = "mock", provider = vim.deepcopy(provider_saved),
        model_catalog = request.params.include_models and vim.deepcopy(catalog) or nil })
    elseif request.method == "settings.update" then
      eq(request.params.scope, "global")
      eq(request.params.expected_revision, revision)
      writes[#writes + 1] = request.params
      revision = revision + 1
      if request.params.desired then saved = vim.deepcopy(request.params.desired)
      else provider_saved = vim.tbl_extend("force", provider_saved, request.params.provider_settings) end
      callback({ revision = revision, state = "saved" })
    elseif request.method == "host.trace.status" then callback({ level = "info" })
    else error("unexpected global configuration request: " .. request.method) end
  end
  local function field(id)
    for _, page in ipairs(picker._state_for_test().spec.page_list) do
      for _, row in ipairs(page.option_list) do
        for _, item in ipairs(row.fields or {}) do if item.id == id then return item end end
      end
    end
    error("missing configuration field: " .. id)
  end
  local function change(id)
    picker._state_for_test().spec.on_field_action({ field = field(id) }, "change", 1)
  end
  local tabs = #vim.api.nvim_list_tabpages()
  require("forge.views.harness.controller").open_config()
  eq(picker._state_for_test().spec.title, "Global defaults")
  change("status-thoughts")
  eq(saved.status_thoughts, false)
  change("approval-mode")
  eq(saved.approval_mode, "deny")
  change("check-editing")
  eq(saved.check_editing, false)
  change("main-model")
  eq(provider_saved.model, "other-model")
  change("ask-override")
  eq(provider_saved.ask_model.model, "other-model")
  eq(#writes, 5)
  eq(session.harness.session, live)
  eq(#vim.api.nvim_list_tabpages(), tabs)
  picker.close()
  require("forge.views.harness.controller").open_config()
  eq(field("status-thoughts").value, false)
  eq(field("main-model").value, "other-model")
  eq(session.harness.session, live)
  picker.close()
  session.harness.session = nil
  require("forge.infra.config").options.harness.provider = nil
  require("forge.harness.provider_preference").load = function() return nil end
  require("forge.views.harness.controller").open_config()
  change("status-thoughts")
  eq(writes[#writes].provider, "mock")
  eq(session.harness.session, nil)
  picker.close()
  catalog_failure = true
  local errors = {}
  require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
  require("forge.views.harness.controller").open_config()
  eq(picker._state_for_test().spec.title, "Global defaults")
  change("status-thoughts")
  eq(session.harness.session, nil)
  assert(#errors > 0 and errors[1] == "catalog offline")
  picker.close()
end

return T
