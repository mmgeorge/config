local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")

---@param state ForgeHarnessPresentationState
---@param name string
---@param model string
---@param refresh fun()
function M.rename(state, name, model, refresh)
  local active = state.session
  if not active or not active.id then
    notifications.error("No active Harness session to rename", "ForgeHarness")
    return
  end
  state.rename_revision = (state.rename_revision or 0) + 1
  local revision, generation = state.rename_revision, client.host_generation()
  local session_id, previous_name = active.id, active.name or ""
  local function progress(text)
    state.rename_status = text
    refresh()
  end
  local function current()
    if state.rename_revision ~= revision then return false end
    if client.host_generation() ~= generation or not state.session or state.session.id ~= session_id then
      progress(nil)
      return false
    end
    return true
  end
  local function save(value, generated)
    if not current() then return end
    client.request_for(session_id, "session.rename", {
      session_id = session_id, name = value, expected_name = generated and previous_name or nil,
    }, function(result, failure)
      if not current() then return end
      state.rename_status = nil
      if failure then
        notifications.error(failure, "Harness rename")
      elseif result and type(result.name) == "string" then
        state.session.name = result.name
      else
        notifications.error("Session rename completed without a name", "Harness rename")
      end
      refresh()
    end)
  end
  name = vim.trim(name)
  if name ~= "" then save(name, false) return end
  progress("Generating session name…")
  client.request_for(session_id, "harness.document", { operation = "session_name", model = model },
    function(result, failure)
      if not current() then return end
      if failure then
        progress(nil)
        notifications.error(failure, "Harness rename")
      elseif not result or type(result.text) ~= "string" or vim.trim(result.text) == "" then
        progress(nil)
        notifications.error("Session name generation completed without text", "Harness rename")
      else
        save(vim.trim(result.text), true)
      end
    end)
end

return M
