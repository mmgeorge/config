local M = {}
local client = require("forge.client")
local config = require("forge.infra.config")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local provider_preference = require("forge.harness.provider_preference")
local sequence = 0

local provider_fields = { model = true, effort = true, context_window = true, service_tier = true, ask_model = true }

local function present(value)
  if value == vim.NIL then return nil end
  if type(value) ~= "table" then return value end
  local result = {}
  for key, field in pairs(value) do result[key] = present(field) end
  return result
end

---Edit saved defaults without creating, resuming, or registering a Harness session.
---@param host table
function M.open(host)
  sequence = sequence + 1
  local revision = sequence
  local provider = provider_preference.load(config.options.harness.providers, config.options.harness.provider)
  local workspace = vim.fn.getcwd()
  local function current()
    return revision == sequence and vim.api.nvim_win_is_valid(host.control_win)
      and vim.api.nvim_win_get_tabpage(host.control_win) == vim.api.nvim_get_current_tabpage()
  end
  client.request({ method = "settings.get", params = { provider = provider, workspace = workspace } },
    function(snapshot, failure)
      if not current() then return end
      if failure then notifications.error(failure, "ForgeConfig") return end
      snapshot = present(snapshot)
      provider = snapshot.selected_provider or provider or config.options.harness.provider
      local global = snapshot.layers.global or {}
      local selected = vim.tbl_deep_extend("force", {
        provider = provider, workspace = workspace, model = config.options.harness.model or "default",
        effort = config.options.harness.effort or "default", service_tier = "default", approval_mode = "ask",
        status_thoughts = true, session_tools = true, subagent_tools = false, lsp_tools = false,
        check_editing = false, automatic_checks = false, check_timing = "after_queue", shell = "system",
        approval_timeout = { amount = 10, unit = "m" }, tool_timeout = { amount = 10, unit = "m" },
      }, global, snapshot.provider or {})
      -- The shared form reads a settings record here; this owner has no session identity.
      local descriptor = config.options.harness.providers[provider] or {}
      local catalog = snapshot.model_catalog
      local state = { session = selected, capability = catalog and catalog.capability or descriptor.capability or {} }
      local function sync()
        for _, key in ipairs({ "status_thoughts", "session_tools", "subagent_tools", "lsp_tools", "check_editing",
          "automatic_checks", "check_timing" }) do state[key] = selected[key] end
        state.ask_model = selected.ask_model or {}
      end
      sync()
      local target = { current = current }
      target.models = function(callback)
        if catalog then callback(catalog.models) return end
        client.request({ method = "settings.get", params = { provider = provider, workspace = workspace, include_models = true } },
          function(result, catalog_failure)
            if not current() then return end
            if catalog_failure then callback(nil, catalog_failure) return end
            catalog = result.model_catalog
            state.capability = catalog.capability
            callback(catalog.models)
          end)
      end
      target.configure = function(params, completed)
        local patch = { scope = "global", provider = provider, workspace = workspace, expected_revision = snapshot.revision }
        local provider_patch, portable = {}, {}
        for key, value in pairs(params) do
          if provider_fields[key] then provider_patch[key] = value else portable[key] = value end
        end
        if next(provider_patch) then
          assert(not next(portable), "configuration update mixed provider and portable defaults")
          patch.provider_settings = provider_patch
        else patch.desired = vim.tbl_deep_extend("force", global, portable) end
        client.request({ method = "settings.update", params = patch }, function(result, save_failure)
          if not current() then return end
          if save_failure then notifications.error(save_failure, "ForgeConfig")
          else
            snapshot.revision = result.revision
            if patch.desired then global = patch.desired end
            selected = vim.tbl_deep_extend("force", selected, params)
            if params.ask_model then selected.ask_model = vim.deepcopy(params.ask_model) end
            state.session = selected
            sync()
          end
          completed(save_failure == nil)
        end)
      end
      target.select_provider = function(provider_host)
        local entries = {}
        for id, descriptor in pairs(config.options.harness.providers) do
          if descriptor.selectable ~= false then entries[#entries + 1] = { id = id, label = descriptor.label or id } end
        end
        table.sort(entries, function(a, b) return a.label < b.label end)
        picker.open({ host = provider_host, page_list = { { id = "default-provider", title = "Default provider", option_list = entries } },
          on_confirm = function(result)
            local ok, save_failure = provider_preference.save(result.option.id)
            if not ok then notifications.error(save_failure, "ForgeConfig") return end
            picker.close(false)
            M.open(host)
          end })
      end
      require("forge.views.harness.settings").open(state, host, nil, target)
    end)
end

return M
