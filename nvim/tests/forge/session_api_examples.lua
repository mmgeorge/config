vim.loader.enable(false)
local root, directory = vim.fn.getcwd(), vim.fn.tempname()
local workspace, data = directory .. "/workspace", directory .. "/data"
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
vim.fn.chdir(workspace)
require("forge").setup({ harness_logging = false, harness = { backend = "mock" } })
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local capture = dofile(root .. "/nvim/tests/forge/support/wire_capture.lua").start()
local session_id
local examples = {}
local function request(method, params, expected_failure)
  local result, failure, completed
  client.request({ method = method, params = params, session_id = session_id }, function(value, problem)
    result, failure, completed = value, problem, true
  end)
  assert(vim.wait(10000, function() return completed end, 5), method .. " timed out")
  if expected_failure then
    assert(failure and failure:find(expected_failure, 1, true), method .. ": " .. tostring(failure))
  else
    assert(not failure, method .. ": " .. tostring(failure))
  end
  examples[#examples + 1] = capture.completed(method)
  return result
end
local success, failure = xpcall(function()
  local initialized, problem
  client.session_initialize(function(value, error) initialized, problem = value, error end)
  assert(vim.wait(10000, function() return initialized or problem end, 5), "initialization timed out")
  assert(not problem, problem)
  session_id = initialized.session.id
  assert(request("session.task.get") == nil)
  assert(request("session.turn.restart").restart_requested == true)
  assert(request("session.command.status.get", { id = "unsubmitted-example" }).state == "not_admitted")
  assert(request("session.input.operation.get", { id = "unsubmitted-example" }) == nil)
  assert(request("session.preview", { session_id = session_id }).session.id == session_id)
  assert(type(request("session.agent.model.list", { provider = "mock" })) == "table")
  assert(type(request("session.provider.capabilities.get")) == "table")
  request("session.provider.skill.list", nil, "unsupported provider capability")
  assert(request("session.approval.continue").session.id == session_id)
  assert(type(request("session.check.override", {
    enabled = vim.empty_dict(), auto = vim.empty_dict(), scopes = vim.empty_dict(),
  })) == "table")
  assert(request("session.check.complete").session.id == session_id)
  assert(request("session.exchange.recovery") == nil)
  request("session.provider.setup", {}, "unsupported provider capability")
  request("session.provider.authenticate", { method = "fixture" }, "unsupported provider capability")
  local attachment = directory .. "/example.txt"
  vim.fn.writefile({ "Example attachment" }, attachment)
  assert(type(request("session.attachment.import", { path = attachment, number = 1 })) == "table")
  request("session.prompt.submit", { text = "Establish a compactable fixture conversation" })
  assert(request("session.compact").session.id == session_id)
  request("session.configure", { subagent_tools = true })
  local child = request("session.agent.start", { text = "Return the fixture result", operation_id = "example-agent" })
  assert(child.status == "admitted" and child.agent.id)
  local target = child.agent.id
  local message = request("session.agent.message.send", { target = target, message = "Keep the result concise", operation_id = "example-agent-message" })
  assert(message.id)
  local followup = request("session.agent.followup", { target = target, message = "Verify the fixture result", operation_id = "example-agent-followup" })
  assert(followup.status == "admitted")
  assert(request("session.agent.interrupt", { target = target, operation_id = "example-agent-interrupt" }).status == "interrupting")
  assert(request("session.agent.interrupt_all", { operation_id = "example-agent-interrupt-all" }).status == "interrupting")
  local permission = request("session.permission.set", {
    change = { kind = "set_approval", mode = "ask" }, operation_id = "example-permission",
  })
  assert(permission.id == "example-permission" and (permission.state == "accepted" or permission.state == "completed"))
  local held = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua").hold(session_id)
  assert(request("session.goal.set", { text = "Verify explicit task admission", operation_id = "example-goal" }).state == "completed")
  held:wait()
  local steering = request("session.turn.steer", { text = "Keep working on the fixture", operation_id = "example-steering" })
  assert(steering)
  assert(request("session.task.get").kind == "goal")
  assert(request("session.goal.pause", { operation_id = "example-pause" }).state == "completed")
  assert(request("session.goal.clear", { operation_id = "example-clear" }).state == "completed")
  request("session.turn.cancel")
  if arg[1] then vim.fn.writefile({ vim.json.encode({ root = directory, examples = examples }) }, arg[1]) end
end, debug.traceback)
client.host_stop()
local stopped = vim.wait(6000, function() return client._client.process == nil end, 10)
capture.close()
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and stopped, failure or "session API fixture host did not stop")
print("session_api_examples: explicit session contracts passed")
