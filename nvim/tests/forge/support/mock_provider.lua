local client = require("forge.client")
local M = {}

---@class ForgeMockPendingResponse
---@field id string
---@field session_id string
---@field mode string
---@field prompt string

---@class ForgeMockResponseHold
---@field id string
---@field session_id string
local Hold = {}
Hold.__index = Hold

local function request(session_id, params)
  local completed, result, failure = false, nil, nil
  client.request_for(session_id, "backend.mock_response", params, function(response, diagnostic)
    result, failure, completed = response, diagnostic, true
  end)
  assert(vim.wait(10000, function() return completed end),
    "Mock provider " .. params.operation .. " did not complete within 10 seconds")
  assert(not failure, failure)
  return result
end

---Arm a single response before submitting its prompt. Omit mode to match any prompt.
---@param session_id string
---@param mode? string
---@return ForgeMockResponseHold
function M.hold(session_id, mode)
  local result = request(session_id, { operation = "hold", mode = mode })
  return setmetatable({ id = result.id, session_id = session_id }, Hold)
end

---Wait until the provider holds the matching request without releasing its response.
---@return ForgeMockPendingResponse
function Hold:wait()
  return request(self.session_id, { operation = "wait", id = self.id })
end

---Release this arrived response. Unknown, cancelled, and unclaimed holds fail explicitly.
function Hold:release()
  return request(self.session_id, { operation = "release", id = self.id })
end

---Discard this armed hold or cancel its pending response.
function Hold:cancel()
  return request(self.session_id, { operation = "cancel", id = self.id })
end

return M
