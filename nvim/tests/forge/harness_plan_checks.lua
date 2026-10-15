vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local answered = {}
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function()
    for _, approval in ipairs(state.approval or {}) do
      if not answered[approval.id] then
        answered[approval.id] = true
        local answers = {}
        for _, item in ipairs(approval.item_list) do answers[#answers + 1] = { item_id = item.id, choice_id = "allow_once" } end
        client.request("approval.resolve", { approval_id = approval.id, answer_list = answers }, function(_, error)
          assert(not error, tostring(error))
        end)
      end
    end
    return settled
  end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local function reopen(session_id)
  require("forge.views.harness.plan_question").close()
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "host did not stop")
  local restored, failure
  client.start_harness(function(value, error) restored, failure = value, error end, { session_id = session_id })
  assert(vim.wait(10000, function() return restored ~= nil or failure ~= nil end, 10), "reopen timeout")
  assert(not failure, failure)
  require("forge.views.harness").open()
  state = require("forge.session").harness
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "retained-control" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  local original_request = "create the requested starter game with movement and combat"
  request("prompt.submit", { text = "/plan " .. original_request })
  local pending = request("state.get")
  assert(pending.active_plan.state == "configuring_checks", vim.inspect(pending.active_plan))
  assert(pending.task[1].phase == "checks_setup", vim.inspect(pending.task))
  assert(pending.active_plan.request == original_request, "setup replaced the original planning request")
  assert(pending.active_plan.generation.turn_count == 0, "planning ran before check setup")
  assert(pending.active_elicitation.owner == "interaction", "setup must own a separate question exchange")
  local plan_id, task_id = pending.active_plan.id, pending.task[1].id
  local question = pending.active_elicitation.elicitation.question_set.questions[1]
  require("forge.views.harness.plan_question").close()
  state.plan_question_open = false
  require("forge.views.harness.controller").reopen_question()
  assert(state.plan_question_open, "closing the picker lost the setup question")
  require("forge.views.harness.plan_question").close()
  state.plan_question_open = false
  local session_id = state.session.id
  reopen(session_id)
  pending = request("state.get")
  assert(pending.active_plan.id == plan_id and pending.active_plan.state == "configuring_checks",
    "recovery must retain the pending plan")
  assert(pending.active_plan.generation.turn_count == 0, "recovery started planning without checks")
  assert(pending.active_elicitation.elicitation.question_set.questions[1].id == question.id,
    "recovery replaced the unanswered setup question")
  request("question.answer", { question_id = question.id, response = { kind = "other", text = "Save no automated checks for this fixture." } })
  request("question.continue")
  local planned = request("state.get")
  assert(request("checks.list").configured, "setup did not save configuration")
  assert(planned.active_plan.id == plan_id and planned.task[1].id == task_id, "handoff replaced the original task or plan")
  assert(planned.active_plan.state == "awaiting_review", vim.inspect(planned.active_plan))
  assert(planned.active_plan.request == original_request, "handoff lost the original request")
  local draft_count = 0
  for _, exchange in ipairs(request("exchange.list")) do
    if exchange.kind == "plan_draft" then draft_count = draft_count + 1 end
  end
  assert(draft_count == 1, "handoff must start exactly one planning exchange: " .. draft_count)
  reopen(session_id)
  local restored = request("state.get")
  assert(restored.active_plan.id == plan_id and restored.active_plan.state == "awaiting_review",
    "reopening lost the completed handoff")
  local restored_count = 0
  for _, exchange in ipairs(request("exchange.list")) do
    if exchange.kind == "plan_draft" then restored_count = restored_count + 1 end
  end
  assert(restored_count == draft_count, "reopening replayed the planning handoff")
  request("prompt.submit", { text = "/plan use the existing intentionally empty checks" })
  local direct = request("state.get")
  assert(direct.active_plan.state == "awaiting_review" and direct.active_elicitation == nil,
    "an explicitly empty configuration must bypass setup")
  vim.fn.writefile({ vim.json.encode({ branch_prefix = "keep", checks = { version = 77, definitions = {} } }) }, workspace .. "/.forge.json")
  request("prompt.submit", { text = "/plan repair invalid checks before planning" })
  local invalid = request("state.get")
  assert(invalid.active_plan.state == "configuring_checks" and invalid.active_elicitation,
    "invalid configuration must enter setup without generating a plan")
  request("question.skip", { question_id = invalid.active_elicitation.elicitation.question_set.questions[1].id })
  request("question.continue")
  invalid = request("state.get")
  assert(invalid.active_plan.state == "configuring_checks" and invalid.active_elicitation,
    "skipping setup choices must not publish defaults or start planning")
  assert(vim.json.decode(table.concat(vim.fn.readfile(workspace .. "/.forge.json"), "\n")).branch_prefix == "keep",
    "setup admission changed unrelated project settings")
  request("prompt.submit", { text = "/plan cancel" })
  local cancelled = request("state.get")
  assert(cancelled.active_elicitation == nil, "cancelling the plan retained an active setup question")
  assert(request("checks.list").snapshot == nil, "cancelling setup unexpectedly published configuration")
end, debug.traceback)
client.stop()
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
