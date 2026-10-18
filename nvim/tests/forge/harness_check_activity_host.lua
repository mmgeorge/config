vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local hold = vim.g.forge_check_activity_hold or 0
vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = {
  { id = "activity", name = "Activity fixture", category = "test", area = "Fixture", enabled = true,
    auto = false, trigger = { kind = "manual" }, scope = "all", directory = ".",
    all = { command = "nu", args = { "--no-config-file", "-c",
      ("sleep %dms; print 'retained check output'"):format(hold + 1200) } }, depends_on = {} },
} } }) }, workspace .. "/.forge.json")
local executable = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors, answered = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end

local function answer_permissions()
  for _, approval in ipairs(state.approval or {}) do
    if not answered[approval.id] then
      answered[approval.id] = true
      local answers = {}
      for _, item in ipairs(approval.item_list) do
        answers[#answers + 1] = { item_id = item.id, choice_id = "allow_once" }
      end
      client.request("approval.resolve", { approval_id = approval.id, answer_list = answers }, function(_, failure)
        assert(not failure, failure)
      end)
    end
  end
end

---@param method string
---@param params? table
---@return table
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, message) value, failure, settled = result, message, true end)
  assert(vim.wait(10000, function() answer_permissions() return settled end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end

local function transcript()
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end

local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "visible-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), table.concat(errors, "\n"))
  local prompt_failure
  client.request("prompt.submit", { text = "Read while checks run" }, function(_, message) prompt_failure = message end)
  assert(vim.wait(3000, function() return transcript():find("Working", 1, true) ~= nil end, 10),
    "provider did not start: " .. tostring(prompt_failure) .. "\n" .. transcript() .. "\n" .. table.concat(errors, "\n"))
  request("turn.cancel")
  assert(vim.wait(3000, function() return transcript():find("Idle", 1, true) ~= nil end, 10))
  local run_id = request("checks.run", { ids = { "activity" } }).run_id
  assert(vim.wait(3000, function()
    answer_permissions()
    return transcript():find("Activity fixture", 1, true) ~= nil
      and transcript():find("Checks running", 1, true) ~= nil
  end, 10), "live check did not reach the status area")
  local snapshot = request("state.get")
  assert(snapshot.checks_running and snapshot.active_check_run == run_id)
  assert(snapshot.status.kind == "idle", "check replaced the primary phase")
  local running = transcript()
  assert(running:find("Idle", 1, true) < running:find("Checks running", 1, true))
  assert(not running:find("retained check output", 1, true), "running status exposed check output")
  if hold > 0 then vim.cmd("redraw") vim.wait(hold, function() return false end, 20) end
  local result
  assert(vim.wait(10000, function()
    result = request("checks.status", { run_id = run_id })
    return result.state ~= "running"
  end, 20), "check did not finish")
  assert(result.state == "passed", vim.inspect(result))
  assert(vim.wait(3000, function()
    return transcript():find("Checks finished", 1, true) ~= nil
      and transcript():find("Checks running", 1, true) == nil
  end, 10), "completion did not move the result into history")
  local completed = request("state.get")
  assert(not completed.checks_running and (completed.active_check_run == nil or completed.active_check_run == vim.NIL))
  assert(completed.status.kind == "idle", "completion changed primary work")
  local _, count = transcript():gsub("Checks finished", "")
  assert(count == 1, "result history was duplicated")
  assert(not transcript():find("retained check output", 1, true), "result output was not collapsed")
  assert(#errors == 0, table.concat(errors, "\n"))
  if hold > 0 then vim.cmd("redraw") vim.wait(hold, function() return false end, 20) end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "check activity host was not collected")
print("harness_check_activity_host: passed")
vim.cmd("qa!")
