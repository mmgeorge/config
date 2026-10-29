local M = {}
local path = assert(vim.api.nvim_get_runtime_file("lua/forge/protocol_contract.json", false)[1], "Forge event contract is missing")
local contract = vim.json.decode(table.concat(vim.fn.readfile(path), "\n"))
M.version = contract.version

---@class ForgeMethodPolicy
---@field session_context "none"|"optional"|"required"
---@field readiness "host"|"runtime"
---@field admission "ordinary"|"control"
---@field recovery "none"|"session_command"|"service"

local policy_values = {
  session_context = { none = true, optional = true, required = true },
  readiness = { host = true, runtime = true },
  admission = { ordinary = true, control = true },
  recovery = { none = true, session_command = true, service = true },
}
assert(type(contract.methods) == "table", "Forge method policy is missing")
for name, policy in pairs(contract.methods) do
  assert(type(name) == "string" and name ~= "" and type(policy) == "table", "Invalid Forge method policy")
  for field, values in pairs(policy_values) do
    assert(values[policy[field]], "Invalid Forge method policy " .. name .. "." .. field)
  end
  for field in pairs(policy) do assert(policy_values[field], "Unknown Forge method policy field: " .. field) end
end

---@param name string
---@return ForgeMethodPolicy
function M.method_policy(name)
  return assert(contract.methods[name], "Unknown Forge method: " .. tostring(name))
end

---@param spec table
---@return ForgeMethodPolicy
function M.validate_request(spec)
  assert(type(spec) == "table", "Forge request requires a specification object")
  for field in pairs(spec) do
    assert(field == "method" or field == "params" or field == "session_id", "Unexpected Forge request field: " .. tostring(field))
  end
  assert(type(spec.method) == "string", "Forge request method is required")
  assert(spec.params == nil or type(spec.params) == "table", "Forge request params must be an object")
  for field in pairs(spec.params or {}) do
    assert(type(field) == "string", "Forge request params must have string field names")
  end
  local policy = M.method_policy(spec.method)
  if spec.session_id ~= nil then
    assert(type(spec.session_id) == "string" and spec.session_id:find("%S"), "Forge session_id must be a nonempty identifier")
    assert(policy.session_context ~= "none", "Forge method does not accept session_id")
  else
    assert(policy.session_context ~= "required", "Forge method requires an explicit session_id")
  end
  return policy
end

---@param node table
function M.validate_node(node)
  for _, field in ipairs({ "kind", "lifecycle", "display", "default_display" }) do
    local values = contract.node[field == "default_display" and "display" or field]
    assert(vim.list_contains(values, node[field]), "invalid node " .. field)
  end
  assert(type(node.more) == "boolean", "invalid node paging state")
end

---@param channel string
---@param name string
---@return string
function M.route(channel, name)
  local event = contract[channel] and contract[channel][name]
  assert(event, ("Unknown Forge %s event: %s"):format(channel, tostring(name)))
  return event.route
end

---@param channel string
---@param name string
---@param payload table
function M.validate(channel, name, payload)
  M.route(channel, name)
  assert(type(payload) == "table", "Forge event requires an object payload")
  for path, expected in pairs(contract[channel][name].required) do
    ---@type any
    local value = payload
    for key in path:gmatch("[^.]+") do
      if type(value) == "table" then value = value[key] else value = nil end
    end
    local valid = expected == "string" and type(value) == "string" and #value > 0
      or expected == "nullable_string" and (value == vim.NIL or type(value) == "string" and #value > 0)
      or expected == "integer" and type(value) == "number" and value >= 0 and value <= 9007199254740991 and value == math.floor(value)
      or expected == "object" and type(value) == "table"
      or expected == "boolean" and type(value) == "boolean"
    assert(valid, ("Forge %s event %s: %s requires %s"):format(channel, name, path, expected))
  end
  if channel == "session" and name == "backend_event" then M.validate("backend", payload.kind, payload) end
end

return M
