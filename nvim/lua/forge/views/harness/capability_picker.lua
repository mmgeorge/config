local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")

local M = {}

---@alias ForgeCapabilityRestriction "permission_callbacks_only"|"unconfined_reads"|"unstructured_diffs"|"enterprise_terminal_policy"|"inherited_skills"|"checkpoint_required"|"settled_only"
---@alias ForgeCapabilitySupport "supported"|"unsupported"|{limited:{restrictions:ForgeCapabilityRestriction[]}}

---@param support ForgeCapabilitySupport
---@return string
local function support_text(support)
  if support == "supported" or support == "unsupported" then return support end
  assert(type(support) == "table" and type(support.limited) == "table", "invalid capability support")
  local labels = {
    permission_callbacks_only = "permission callbacks only",
    unconfined_reads = "reads are not OS-confined",
    unstructured_diffs = "native edits may omit structured diffs",
    enterprise_terminal_policy = "enterprise terminal policy may bypass approval callbacks",
    inherited_skills = "inherited native skills remain available",
    checkpoint_required = "checkpoint required",
    settled_only = "settled invocation required",
  }
  local restrictions = {}
  for _, restriction in ipairs(support.limited.restrictions) do
    assert(labels[restriction], "invalid capability restriction")
    restrictions[#restrictions + 1] = labels[restriction]
  end
  return "limited (" .. table.concat(restrictions, "; ") .. ")"
end

---@param host table
function M.open(host, session_id)
  client.request({ session_id = session_id, method = "session.provider.capabilities.get", params = {} }, function(manifest, request_error)
    if request_error then
      notifications.error(request_error, "Harness capabilities")
      return
    end
    local option_list = {}
    local revision = "Schema " .. tostring(manifest.revision) .. " · Negotiated support revision "
      .. tostring(manifest.effective_revision)
    option_list[#option_list + 1] = {
      id = "revision", label = "Capability revision", detail = revision,
      columns = { "Capability revision", "Metadata", revision },
    }
    if manifest.provider_version then
      option_list[#option_list + 1] = {
        id = "version", label = "Provider version", detail = manifest.provider_version,
        columns = { "Provider version", "Metadata", manifest.provider_version },
      }
    end
    for index, notice in ipairs(manifest.notice or {}) do
      option_list[#option_list + 1] = { id = "notice-" .. index, label = "Resource unavailable", detail = notice,
        columns = { "Resource unavailable", "Notice", notice } }
    end
    for _, capability in ipairs(manifest.capability or {}) do
      local detail = table.concat({ support_text(capability.support), capability.implementation,
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
        wrap_column = 3,
        option_list = option_list,
        footer = "↑↓ select  / search  q close",
      } },
      on_confirm = function() return false end,
    })
  end)
end

return M
