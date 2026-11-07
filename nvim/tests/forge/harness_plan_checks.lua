vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local approval = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/approval.lua")
local responder = approval.responder()
local configuration_reviews = 0
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function answer_approvals()
  approval.answer_pending(responder, function(pending)
    for _, target in ipairs(pending.targets) do
      if target.title == "Update project checks" then
        configuration_reviews = configuration_reviews + 1
        assert(vim.fn.filereadable(workspace .. "/.forge.json") == 0
          or configuration_reviews > 1, "setup published before approval")
      end
    end
  end)
end
local function request(method, params)
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function()
    answer_approvals()
    return settled
  end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
-- Prompt submission returns at admission, so await the plan that the submitted request produces.
---@param previous_id? string
---@return table
local function await_plan(previous_id)
  local snapshot
  local settled = vim.wait(15000, function()
    answer_approvals()
    snapshot = state_snapshot.get()
    local plan = snapshot.active_plan
    return plan ~= nil and plan.id ~= previous_id and plan.state == "awaiting_review" and not require("forge.views.harness.activity").busy(state)
  end, 20)
  if not settled then
    error("plan did not reach review: " .. vim.inspect({ plan = snapshot and snapshot.active_plan,
      activity = state.activity, snapshot_activity = snapshot and snapshot.activity,
      pending_approvals = vim.tbl_map(function(pending) return pending.request_id end, state.approval or {}),
      snapshot_approvals = snapshot and snapshot.approval, answered = responder.answered }))
  end
  return snapshot
end
local function reopen(session_id)
  require("forge.views.harness.plan_question").close()
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
  client.host_stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "host did not stop")
  local restored, failure
  client.session_initialize(function(value, error) restored, failure = value, error end, { session_id = session_id })
  assert(vim.wait(10000, function() return restored ~= nil or failure ~= nil end, 10), "reopen timeout")
  assert(not failure, failure)
  require("forge.views.harness").open()
  state = require("forge.session").harness
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { command = { "retained-control" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  local original_request = "create the requested starter game with movement and combat"
  request("session.prompt.submit", { text = "/plan " .. original_request })
  local planned = await_plan()
  assert(planned.active_elicitation == nil, "check discovery required a setup question")
  assert(configuration_reviews == 1, "configuration did not receive exactly one review")
  local plan_id, task_id = planned.active_plan.id, planned.session.active_task.id
  local session_id = state.session.id
  assert(request("session.check.list").configured, "setup did not save configuration")
  assert(planned.active_plan.id == plan_id and planned.session.active_task.id == task_id, "handoff replaced the original task or plan")
  assert(planned.active_plan.state == "awaiting_review", vim.inspect(planned.active_plan))
  assert(planned.active_plan.request == original_request, "handoff lost the original request")
  local draft_count = 0
  for _, exchange in ipairs(request("session.exchange.list")) do
    if exchange.check_setup == nil then draft_count = draft_count + 1 end
  end
  assert(draft_count == 1, "handoff must start exactly one planning exchange: " .. draft_count)
  reopen(session_id)
  local restored = state_snapshot.get()
  assert(restored.active_plan.id == plan_id and restored.active_plan.state == "awaiting_review",
    "reopening lost the completed handoff")
  local restored_count = 0
  for _, exchange in ipairs(request("session.exchange.list")) do
    if exchange.check_setup == nil then restored_count = restored_count + 1 end
  end
  assert(restored_count == draft_count, "reopening replayed the planning handoff")
  request("session.prompt.submit", { text = "/plan use the existing intentionally empty checks" })
  local direct = await_plan(plan_id)
  assert(direct.active_plan.state == "awaiting_review" and direct.active_elicitation == nil,
    "an explicitly empty configuration must bypass setup")
  vim.fn.writefile({ vim.json.encode({ branch_prefix = "keep", checks = { version = 77, definitions = {} } }) }, workspace .. "/.forge.json")
  request("session.prompt.submit", { text = "/plan repair invalid checks before planning" })
  local invalid = await_plan(direct.active_plan.id)
  assert(invalid.active_plan.state == "awaiting_review" and invalid.active_elicitation == nil,
    "invalid configuration must be repaired through approval without mandatory questions")
  assert(configuration_reviews == 2, "invalid configuration repair omitted approval")
  assert(vim.json.decode(table.concat(vim.fn.readfile(workspace .. "/.forge.json"), "\n")).branch_prefix == "keep",
    "setup admission changed unrelated project settings")
  request("session.prompt.submit", { text = "/plan cancel" })
  assert(vim.wait(10000, function() return not require("forge.views.harness.activity").busy(state) end, 20), "plan cancellation did not settle")
  local cancelled = state_snapshot.get()
  assert(cancelled.active_elicitation == nil, "cancelling the plan retained an active setup question")
  assert(request("session.check.list").configured, "cancelling the plan removed approved check configuration")
end, debug.traceback)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
if not succeeded or not collected then
  vim.api.nvim_err_writeln(failure or "host was not collected")
  vim.cmd("cquit 1")
else
  print("harness_plan_checks: passed")
  vim.cmd("qa!")
end
