---@class AIHttpRequest
---@field method "GET"|"POST"
---@field url string
---@field headers table<string, string>
---@field body? string

---@class AIHttpResponse
---@field status integer
---@field body string

---@alias AIHttpCallback fun(err: string?, response: AIHttpResponse?)

local M = {}

local status_marker = "__AI_HTTP_STATUS__:"

---@param request AIHttpRequest
---@param cb AIHttpCallback
function M.request(request, cb)
  local command = {
    "curl",
    "--silent",
    "--show-error",
    "--max-time", "120",
    "--request", request.method,
    "--write-out", "\n" .. status_marker .. "%{http_code}",
  }
  for name, value in pairs(request.headers or {}) do
    command[#command + 1] = "--header"
    command[#command + 1] = name .. ": " .. value
  end
  if request.body then
    command[#command + 1] = "--data-binary"
    command[#command + 1] = "@-"
  end
  command[#command + 1] = request.url

  local ok, process = pcall(vim.system, command, {
    text = true,
    stdin = request.body,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local body, status = stdout:match("^(.*)\n" .. status_marker .. "(%d+)%s*$")
      if result.code ~= 0 or not status then
        local stderr = vim.trim(result.stderr or "")
        cb(stderr ~= "" and stderr or ("curl exited with code " .. tostring(result.code)))
        return
      end
      cb(nil, { status = tonumber(status) --[[@as integer]], body = body })
    end)
  end)
  if not ok then
    vim.schedule(function()
      cb(tostring(process))
    end)
  end
end

---@param body string
---@return table? decoded
function M.decode_json(body)
  local ok, decoded = pcall(vim.json.decode, body, { luanil = { object = true, array = true } })
  if not ok or type(decoded) ~= "table" then return nil end
  return decoded
end

--- Format an error for a non-2xx response, preferring the standard
--- `{ error = { message = ... } }` shape all three providers share.
---@param response AIHttpResponse
---@return string
function M.response_error(response)
  local decoded = M.decode_json(response.body)
  local err = decoded and decoded.error
  if type(err) == "table" and type(err.message) == "string" and vim.trim(err.message) ~= "" then
    return err.message
  end
  local body = vim.trim(response.body or "")
  if #body > 200 then body = body:sub(1, 200) .. "..." end
  return "HTTP " .. response.status .. (body ~= "" and (": " .. body) or "")
end

return M
