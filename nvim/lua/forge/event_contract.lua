local M = {}
local path = assert(vim.api.nvim_get_runtime_file("lua/forge/protocol_contract.json", false)[1], "Forge event contract is missing")
local contract = vim.json.decode(table.concat(vim.fn.readfile(path), "\n"))
M.version = contract.version

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
