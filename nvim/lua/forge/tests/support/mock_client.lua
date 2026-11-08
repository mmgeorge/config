-- Mocks `forge.client.request` for the rest of the current case, restoring it when the case ends.
-- `record` keeps every request so the case can inspect and answer it later; `respond` answers by
-- method from a table of values or handlers, and records the requests too.
--
--   local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
--   local requests = mock_client.record()
--   controller.submit()
--   eq(requests[1].spec.method, "session.prompt.submit")
--   requests[1].callback({ accepted = true })
--
--   mock_client.respond({
--     ["session.input.get"] = { input = { items = {} } },
--     ["session.plan.accept"] = function(spec, callback) callback(nil, "rejected") end,
--   })
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")

local mock_client = {}

---@class RecordedRequest
---@field spec table the request spec, with `method`, `params`, and `session_id`
---@field callback fun(result: any, error: any)?
---@field progress function?

---@class MockClientOptions
---@field unknown? "error"|"record" how `respond` treats a method with no entry; defaults to "error"
---@field wrap_callback? fun(callback: function): function transforms each callback before it is
---  recorded or handed to a handler, such as an encoder that delivers results as the host would
---@field copy_params? boolean record and hand on a deep copy of `spec.params`, so later mutation by
---  the caller cannot change what the case inspects

---@param options MockClientOptions
---@param requests RecordedRequest[]
---@return fun(spec: table, callback: function?, progress: function?): RecordedRequest
local function intake(options, requests)
  return function(spec, callback, progress)
    if options.copy_params then
      spec = vim.tbl_extend("force", {}, spec, { params = vim.deepcopy(spec.params) })
    end
    if callback and options.wrap_callback then callback = options.wrap_callback(callback) end
    local request = { spec = spec, callback = callback, progress = progress }
    requests[#requests + 1] = request
    return request
  end
end

---@param options? MockClientOptions
---@return RecordedRequest[] requests in the order they were made
function mock_client.record(options)
  local requests = {}
  local receive = intake(options or {}, requests)
  stub(require("forge.client"), "request", function(spec, callback, progress)
    receive(spec, callback, progress)
  end)
  return requests
end

---@param responses table<string, any|fun(spec: table, callback: function, progress: function?)>
---  a value is passed to the callback as the result; a function handles the request itself
---@param options? MockClientOptions
---@return RecordedRequest[] requests in the order they were made
function mock_client.respond(responses, options)
  options = options or {}
  local unknown = options.unknown or "error"
  local requests = {}
  local receive = intake(options, requests)
  stub(require("forge.client"), "request", function(spec, callback, progress)
    local request = receive(spec, callback, progress)
    local response = responses[request.spec.method]
    if type(response) == "function" then return response(request.spec, request.callback, request.progress) end
    if response ~= nil then
      if request.callback then request.callback(response) end
    elseif unknown == "error" then
      error("unexpected client request " .. tostring(request.spec.method), 2)
    end
  end)
  return requests
end

return mock_client
