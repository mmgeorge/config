vim.loader.enable(false)
local root, directory = vim.fn.getcwd(), vim.fn.tempname()
local workspace, data = directory .. "/workspace", directory .. "/data"
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({ vim.json.encode({ checks = { verify = {
  trigger = { auto = false }, action = { command = "exit 0", category = "test" },
} } }) }, workspace .. "/.forge.json")
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
require("forge").setup({ harness_logging = false, harness = { backend = "mock", config_dir = data } })
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local session_id
local function request(method, params)
  local result, failure, completed
  client.request({ method = method, params = params, session_id = session_id }, function(value, problem)
    result, failure, completed = value, problem, true
  end)
  assert(vim.wait(10000, function() return completed end, 5), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function reviewed_plan(previous_revision)
  local plan
  assert(vim.wait(10000, function()
    local snapshot = request("session.state.get")
    plan = snapshot.active_plan
    return plan and plan ~= vim.NIL and plan.state == "awaiting_review"
      and snapshot.status.kind == "awaiting_plan_review"
      and #request("session.input.get").messages == 0
      and plan.model_revision > (previous_revision or 0)
  end, 10), "plan did not reach review")
  return plan
end
local success, failure = xpcall(function()
  local initialized, problem
  client.session_initialize(function(value, error) initialized, problem = value, error end)
  assert(vim.wait(10000, function() return initialized or problem end, 5), "initialization timed out")
  assert(not problem, problem)
  session_id = initialized.session.id
  request("session.prompt.submit", { text = "/plan Build the API fixture" })
  local plan = reviewed_plan()
  local declaration = request("session.plan.declaration.get", { plan_id = plan.id, path = "src/change.rs", revision = plan.model_revision })
  assert(declaration.text:find("requested_change", 1, true))
  local changed = request("session.plan.check.update", { plan_id = plan.id, digest = plan.review_digest,
    expected_version = plan.document_version, enabled = { verify = true } })
  plan = changed.plan
  local acceptance = request("session.plan.acceptance.begin", { plan_id = plan.id, digest = plan.review_digest,
    operation_id = "example-acceptance" })
  assert(acceptance.active_elicitation and acceptance.active_elicitation ~= vim.NIL)
  local cancelled = request("session.plan.acceptance.cancel", { plan_id = plan.id, operation_id = "example-cancel-acceptance" })
  assert(cancelled.active_elicitation == nil or cancelled.active_elicitation == vim.NIL)
  plan = request("session.state.get").active_plan
  local revision = plan.model_revision
  request("session.plan.changes.request", { plan_id = plan.id, digest = plan.review_digest,
    comment = "Clarify the fixture behavior", operation_id = "example-request-changes" })
  plan = reviewed_plan(revision)
  local stopped = request("session.plan.cancel", { plan_id = plan.id, digest = plan.review_digest,
    operation_id = "example-cancel-plan" })
  assert(stopped.task_operation.state == "completed")
end, debug.traceback)
client.host_stop()
local stopped = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and stopped, failure or "plan API fixture host did not stop")
print("plan_api_examples: plan editing and review lifecycle passed")
