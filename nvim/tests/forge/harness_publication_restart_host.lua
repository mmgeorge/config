-- Reopening a session after its host stopped must deliver later publications.
--
-- A host that stops while offering a publication leaves that offer uncertain, which
-- blocks every later publication until the client reconciles through
-- `session.state.get`. This fixture never requests state itself, so a reopen that
-- skips reconciliation leaves the second turn's reply undelivered and fails here.
vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
local fixture_directory = vim.fs.dirname(debug.getinfo(1, "S").source:sub(2))
local fixture_evidence = dofile(fixture_directory .. "/support/evidence_directory.lua")("harness-publication-restart")
local workspace, data = fixture_evidence.path .. "/workspace", fixture_evidence.path .. "/data"
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local initialized = vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait()
assert(initialized.code == 0, initialized.stderr)
local binary = dofile(fixture_directory .. "/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local errors = {}
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
notifications.error = function(message, title)
  errors[#errors + 1] = tostring(message)
  return original_error(message, title)
end

---@param predicate fun(): boolean
---@param message string
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end

---@param method string
---@param params table
local function request(method, params)
  local completion = {}
  client.request({ method = method, params = params, session_id = state.session.id }, function(value, failure)
    completion.value, completion.failure, completion.done = value, failure, true
  end)
  await(function() return completion.done end, method .. " did not finish")
  assert(not completion.failure, tostring(completion.failure))
  return completion.value
end

---@param text string
---@return boolean
local function transcript_contains(text)
  if not (state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)) then return false end
  for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find(text, 1, true) then return true end
  end
  return false
end

---@param label string
local function reply_turn(label)
  local reply = "Fixture reply " .. label
  request("session.provider.mock_response", { operation = "script",
    script = { steps = { { kind = "message", content = "response", text = reply } }, terminal = "completed" } })
  request("session.prompt.submit", { text = "Prompt " .. label })
  await(function() return transcript_contains(reply) end, "reply was not delivered: " .. label)
end

local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness = { provider = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready and state.session ~= nil end,
    "Harness did not open")
  reply_turn("before restart")
  -- Several restarts make the shutdown offer race lose at least once.
  for restart = 1, 5 do
    local session_id = state.session.id
    client.host_stop()
    await(function() return client._client.process == nil end, "host was not collected")
    local reopened
    client.session_initialize(function(snapshot, problem)
      assert(not problem, problem)
      controller.activate_snapshot(snapshot)
      reopened = true
    end, { session_id = session_id })
    await(function() return reopened and state.presentation and state.presentation.ready end,
      "session did not reopen after restart " .. restart)
    reply_turn("after restart " .. restart)
  end
  require("forge.views.harness.workspace").release(state)
end, debug.traceback)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath, notifications.error = original_stdpath, original_error
local release_failure = fixture_evidence.release(success and collected)
assert(success and collected, failure or "publication restart host was not collected")
assert(not release_failure, release_failure)
print("harness_publication_restart_host: passed")
