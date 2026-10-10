vim.loader.enable(false)
local protocol = require("forge.protocol")
local function decode(value)
  local result, failure = protocol.decode_message(vim.json.encode(value))
  assert(result, failure)
  return result
end
local function reject(value)
  local result, failure = protocol.decode_message(vim.json.encode(value))
  assert(not result and failure:find("Invalid Forge host message", 1, true))
end

local ok, failure = xpcall(function()
  assert(decode({ id = 1, result = vim.NIL }).result == nil)
  local result = decode({ id = 1, result = { optional = vim.NIL, list = { vim.NIL, "second" } } }).result
  assert(result.optional == nil and result.list[1] == vim.NIL and result.list[2] == "second")
  reject({ id = 1, result = {}, error = { code = "conflict", message = "conflict" } })
  reject({ id = 1, session_id = "session", result = {} })
  reject({ id = -1, result = {} })
  reject({ id = 1, error = "failure" })
  reject({ id = 1, result = {}, unexpected = true })
  reject({ session_id = "session", event = "unregistered", payload = {} })
  reject({ session_id = "session", event = "backend_event", payload = { kind = "unregistered" } })
  reject({ session_id = "session", event = "document_changed", payload = { session_id = "session", revision = -1 } })
  reject({ session_id = "session", event = "backend_event", payload = {
    kind = "prompt_submission", data = { document = "document", token = "1", state = "accepted" },
  } })
  decode({ session_id = "session", event = "backend_event", payload = {
    kind = "runtime_resolved", data = { session_id = "session", provider = "Codex", model = vim.NIL },
  } })
  decode({ session_id = "session", event = "backend_event", payload = {
    kind = "prompt_submission", data = { document = "document", token = 1, state = "accepted" },
  } })
end, debug.traceback)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
