local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")

local M = {}

---@param host table
function M.open(host)
  client.request("backend.capabilities", {}, function(manifest, request_error)
    if request_error then
      notifications.error(request_error, "Harness capabilities")
      return
    end
    local option_list = {}
    local availability = table.concat({ manifest.availability or "unknown", manifest.provider_version or "",
      manifest.availability_reason or "" }, " · ")
    option_list[#option_list + 1] = {
      id = "availability", label = "Provider availability", detail = availability,
      columns = { "Provider availability", "", availability },
    }
    for index, notice in ipairs(manifest.notice or {}) do
      option_list[#option_list + 1] = { id = "notice-" .. index, label = "Resource unavailable", detail = notice,
        columns = { "Resource unavailable", "", notice } }
    end
    for _, capability in ipairs(manifest.capability or {}) do
      local detail = table.concat({ capability.support, capability.implementation,
        capability.application, capability.reason }, " · ")
      option_list[#option_list + 1] = {
        id = capability.id,
        label = capability.label,
        detail = detail,
        columns = { capability.label, capability.required and "Required" or "Optional", detail },
        value = capability,
      }
    end
    picker.open({
      owner = "harness-capabilities",
      host = host,
      page_list = { {
        id = "capabilities",
        title = manifest.label .. " capabilities",
        column_headers = { "Capability", "Contract", "Support and outcome" },
        option_list = option_list,
        footer = "↑↓ select  / search  q close",
      } },
      on_confirm = function() return false end,
    })
  end)
end

return M
