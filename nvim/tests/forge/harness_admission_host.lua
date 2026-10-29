vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
vim.opt.runtimepath:prepend(original_directory .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local function start(method, params)
  local completion = {}
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, failure, detail)
    completion.value, completion.failure, completion.detail, completion.done = value, failure, detail, true
  end)
  return completion
end
local function finish(completion)
  assert(vim.wait(10000, function() return completion.done end, 10), "request did not finish")
  return completion
end
local function request(method, params)
  local completion = finish(start(method, params))
  assert(not completion.failure, tostring(completion.failure))
  return completion.value
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "session-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  request("session.prompt.submit", { text = "Ask a fixture question." })
  local before = request("session.state.get")
  local elicitation = assert(before.active_elicitation).elicitation
  local question_id = elicitation.question_set.questions[1].id
  local active = start("session.question.ask", { question_id = question_id, text = "Explain the available choices." })
  local rejected = finish(start("session.question.ask", { question_id = question_id, text = "A second clarification." }))
  assert(rejected.detail and rejected.detail.code == "not_admitted", vim.inspect(rejected))
  assert(not finish(active).failure, tostring(active.failure))
  local after = request("session.state.get")
  assert(after.status.kind == "awaiting_input", vim.inspect(after.status))
  assert(after.active_elicitation.elicitation.question_set.id == elicitation.question_set.id,
    "admission rejection replaced the pending question")
  assert(#after.active_elicitation.elicitation.answer == 0, "admission rejection answered the question")
  local controller = require("forge.views.harness.controller")
  require("forge.views.harness.plan_question").close()
  state.plan_question_open = false
  controller.reopen_question()
  assert(state.plan_question_open, "pending question could not reopen after rejection")
  assert(not (state.execution_notice or ""):find("Stopped", 1, true))
  for _, change in ipairs({
    { kind = "toggle_write_enabled" },
    { kind = "set_approval", mode = "deny" },
    { kind = "set_approval", mode = "allow" },
  }) do
    controller.task_transition({ action = "permission", change = change })
    assert(vim.wait(10000, function() return not state.task_operation and not state.busy end, 10),
      "permission change did not settle while a question waited")
    local waiting = request("session.state.get")
    assert(waiting.active_elicitation.elicitation.question_set.id == elicitation.question_set.id,
      "permission change replaced the unanswered question")
    assert(#waiting.active_elicitation.elicitation.answer == 0, "permission change answered the question")
    assert(waiting.session.access.write_access == "off", "approval change restored writes")
  end
  local skipped = request("session.question.skip", { question_id = question_id })
  assert(skipped.active_elicitation.elicitation.answer[1].response.kind == "skipped")
  local answered = request("session.question.answer", {
    question_id = question_id, response = { kind = "other", text = "Use the local fixture" },
  })
  assert(answered.active_elicitation.elicitation.answer[1].response.text == "Use the local fixture")
  local continued = request("session.question.continue")
  assert(continued.active_elicitation == nil or continued.active_elicitation == vim.NIL)
end, debug.traceback)
if vim.g.forge_admission_inspect and success then
  print("harness_admission_host: passed, inspection ready")
  return
end
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_admission_host: passed")
