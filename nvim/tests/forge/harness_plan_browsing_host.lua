vim.loader.enable(false)
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local original_directory = vim.fn.getcwd()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function request(method, params, rejected)
  local result, failure, done
  client.request({ method = method, params = params, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, error) result, failure, done = value, error, true end)
  assert(vim.wait(10000, function() return done end, 10), method .. " timed out")
  if rejected then assert(failure, "lookup unexpectedly succeeded") else assert(not failure, failure) end
  return result
end
local function submit_plan(title, previous)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan " .. title })
  controller.submit()
  assert(vim.wait(15000, function()
    return #errors > 0 or not state.busy and not state.task_operation and state.active_plan
      and state.active_plan.id ~= previous and state.active_plan.state == "awaiting_review"
  end, 10), "planning did not settle")
  assert(#errors == 0, table.concat(errors, "\n"))
  return request("session.state.get", {})
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  assert(vim.wait(10000, function() return state.ready and state.session end, 10), "Harness did not open")
  local first = submit_plan("First review")
  local second = submit_plan("Second review", first.active_plan.id)
  local selected = second.session.active_task.revision
  for _, params in ipairs({ { plan_id = first.active_plan.id }, { plan_id = first.active_plan.id, revision = 1 } }) do
    assert(request("session.plan.get", params).id == first.active_plan.id)
    local observed = request("session.state.get", {})
    assert(observed.session.active_task.revision == selected and observed.active_plan.id == second.active_plan.id,
      "viewing the earlier plan changed workflow ownership")
  end
  request("session.preview", { session_id = "missing-session" }, true)
  request("session.plan.get", { plan_id = "missing-plan" }, true)
  local observed = request("session.state.get", {})
  assert(observed.session.active_task.revision == selected and observed.active_plan.id == second.active_plan.id)
  for _, task in ipairs(observed.task) do
    if task.id == selected then assert(task.status == "waiting", "failed lookup changed task status") end
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath = original_stdpath
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success, failure)
print("harness_plan_browsing_host: review reads and rejected lookups preserve the selected task")
vim.cmd("qa!")
