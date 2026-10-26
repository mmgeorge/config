vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
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
local function start(method, params)
  local completion = {}
  client.request(method, params or {}, function(value, failure, detail)
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
  request("prompt.submit", { text = "Ask a fixture question." })
  local before = request("state.get")
  local elicitation = assert(before.active_elicitation).elicitation
  local question_id = elicitation.question_set.questions[1].id
  local active = start("question.ask", { question_id = question_id, text = "Explain the available choices." })
  local rejected = finish(start("question.ask", { question_id = question_id, text = "A second clarification." }))
  assert(rejected.detail and rejected.detail.code == "not_admitted", vim.inspect(rejected))
  assert(not finish(active).failure, tostring(active.failure))
  local after = request("state.get")
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
end, debug.traceback)
if vim.g.forge_admission_inspect and success then
  print("harness_admission_host: passed, inspection ready")
  return
end
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_admission_host: passed")
