local M = {}

---@param id integer
---@param method string
---@param params? table
---@return string
function M.encode_request(id, method, params)
  return vim.json.encode({ id = id, method = method, params = params or {} }) .. "\n"
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

