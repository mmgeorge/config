local M = {}
M.VERSION = 4

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
  local ok, message = pcall(vim.json.decode, line, { luanil = { object = true, array = true } })
  if not ok or type(message) ~= "table" then
    return nil, "Invalid Forge host JSON: " .. tostring(message)
  end
  if message.request_id ~= nil then
    if type(message.request_id) ~= "number" or message.request_id < 1 or message.request_id > 9007199254740991
      or message.request_id % 1 ~= 0 or type(message.event) ~= "string" or type(message.payload) ~= "table"
    then
      return nil, "Invalid Forge request progress event"
    end
    for key in pairs(message) do
      if key ~= "request_id" and key ~= "event" and key ~= "payload" then
        return nil, "Unexpected field in Forge request progress event: " .. tostring(key)
      end
    end
  end
  if message.document ~= nil then
    if type(message.document) ~= "string" or type(message.event) ~= "string" or type(message.payload) ~= "table" then
      return nil, "Invalid Forge document event"
    end
    for key in pairs(message) do
      if key ~= "document" and key ~= "event" and key ~= "payload" then
        return nil, "Unexpected field in Forge document event: " .. tostring(key)
      end
    end
  end
  return message, nil
end

return M
