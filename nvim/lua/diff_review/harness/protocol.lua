local M = {}

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
    return nil, "Invalid Harness broker JSON: " .. tostring(message)
  end
  return message, nil
end

return M
