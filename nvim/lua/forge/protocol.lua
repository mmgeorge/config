local M = {}
local contract = require("forge.event_contract")
M.VERSION = contract.version

---@param id integer
---@param method string
---@param params? table
---@param session_id? string
---@return string
function M.encode_request(id, method, params, session_id)
  return vim.json.encode({ id = id, method = method, params = params or {}, session_id = session_id }) .. "\n"
end

---@param line string
---@return table?
---@return string?
function M.decode_message(line)
  local ok, message = pcall(function()
    local value = vim.json.decode(line)
    assert(type(value) == "table", "expected a message object")
    local identity_key, channel
    for key, name in pairs({ id = "response", request_id = "request", document = "document", session_id = "session" }) do
      if value[key] ~= nil then
        assert(not identity_key, "ambiguous message identity")
        identity_key, channel = key, name
      end
    end
    assert(identity_key, "missing message identity")
    local allowed = channel == "response" and { id = true, result = true, error = true }
      or { [identity_key] = true, event = true, payload = true }
    for key in pairs(value) do assert(allowed[key], "unexpected message field: " .. tostring(key)) end
    if channel == "response" or channel == "request" then
      local id = value[identity_key]
      assert(type(id) == "number" and id >= (channel == "request" and 1 or 0)
        and id <= 9007199254740991 and id == math.floor(id), "invalid message counter")
    else
      assert(type(value[identity_key]) == "string" and (channel == "document" or #value[identity_key] > 0), "invalid message identity")
    end
    if channel == "response" then
      assert((value.result ~= nil) ~= (value.error ~= nil), "response requires exactly one outcome")
      if value.error ~= nil then
        assert(type(value.error) == "table" and type(value.error.code) == "string"
          and type(value.error.message) == "string", "invalid response error")
      end
    else
      contract.validate(channel, value.event, value.payload)
    end
    local function normalize(object)
      for key, item in pairs(object) do
        if item == vim.NIL and type(key) ~= "number" then
          object[key] = nil
        elseif type(item) == "table" then normalize(item) end
      end
    end
    normalize(value)
    return value
  end)
  if not ok then return nil, "Invalid Forge host message: " .. tostring(message) end
  return message
end

return M
