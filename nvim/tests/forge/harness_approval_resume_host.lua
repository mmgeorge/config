vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
vim.opt.runtimepath:prepend(root .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
local approval_requests = 0
local resolution_list = {}
client.session_events_subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "approval_requested" then
    approval_requests = approval_requests + 1
  elseif event == "backend_event" and payload.kind == "approval_resolved" then
    resolution_list[#resolution_list + 1] = payload.data
  end
end)
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
local function request(method, params)
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local function status()
  local transcript = state.presentation and state.presentation.transcript
  if not transcript then return nil end
  for index = transcript.sequence:count() - 1, 0, -1 do
    local metadata = transcript.sequence:at(index).entry.metadata.status
    if metadata and metadata ~= vim.NIL then return metadata end
  end
end
local function hints()
  return vim.inspect(vim.api.nvim_buf_get_extmarks(state.transcript_buf,
    vim.api.nvim_create_namespace("ForgeHarnessStatusHint"), 0, -1, { details = true }))
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "approval-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10),
    "startup timeout: " .. vim.inspect(errors))
  local picker = require("forge.views.picker")
  local controller = require("forge.views.harness.controller")
  local permission_path = vim.fs.joinpath(data, "forge", "permissions.json")
  local permission_before = vim.fn.readfile(permission_path)
  request("session.input.timeout.configure", {
    approval_timeout = { amount = 1, unit = "s" }, answer_timeout = { amount = 0, unit = "s" },
  })
  for iteration = 1, 2 do
    local before = approval_requests
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Continue after approval expires " .. iteration })
    controller.submit()
    assert(state.busy, "composer submission did not enter busy state")
    assert(vim.wait(10000, function()
      local current = status()
      return #state.approval == 1 and current and current.hint == "permission" and picker.is_open()
    end, 10), "timed approval popup never appeared: " .. vim.inspect(errors))
    local approval_id = state.approval[1].id
    assert(vim.wait(5000, function()
      local current = status()
      return #state.approval == 0 and not picker.is_open()
        and current and current.hint == "working" and current.animated
    end, 10), "timeout did not clear the popup and resume work: " .. vim.inspect(status()))
    assert(state.busy, "mock finished before timeout recovery was observable")
    local resolution = resolution_list[#resolution_list]
    assert(resolution.id == approval_id and resolution.resolution_source == "timeout",
      "expiry was not recorded as an automatic timeout: " .. vim.inspect(resolution))
    assert(vim.wait(10000, function()
      local current = status()
      return state.status and state.status.kind == "idle" and not state.busy
        and (not current or not current.animated)
    end, 10), "completed timeout recovery retained Working: " .. vim.inspect(status()))
    assert(not hints():find("review permission", 1, true), "expired permission hint remained visible")
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    assert(text:find("Automatically rejected because the approval timeout expired", 1, true), text)
    assert(text:find("The requested operation was not executed", 1, true), text)
    assert(text:find("Continuing with permitted work", 1, true), text)
    local snapshot = request("session.state.get", {})
    assert(snapshot.status.kind == "idle" and #snapshot.approval == 0)
    local history = request("session.exchange.list", {})
    local exchange_count = #history
    assert(history[exchange_count].state == "complete", "expired approval did not settle")
    vim.wait(1500, function() return false end, 10)
    local stable = request("session.state.get", {})
    assert(stable.status.kind == "idle" and #request("session.exchange.list", {}) == exchange_count,
      "answered timeout scheduled a duplicate continuation")
    assert(approval_requests == before + 1, "timeout retried the rejected operation")
    assert(vim.deep_equal(vim.fn.readfile(permission_path), permission_before), "timeout saved a permission rule")
  end
  request("session.input.timeout.configure", {
    approval_timeout = { amount = 0, unit = "s" }, answer_timeout = { amount = 0, unit = "s" },
  })
  for iteration, choice in ipairs({ "allow_once", "deny_once", "allow_once" }) do
    local settled, result, problem
    client.request({ method = "session.prompt.submit", params = { text = "Resume after approval " .. iteration }, session_id = assert(require("forge.session").harness.session).id }, function(value, error)
      settled, result, problem = true, value, error
    end)
    assert(vim.wait(10000, function()
      local current = status()
      return #(state.approval or {}) == 1 and current and current.hint == "permission"
    end, 10), "approval never reached the timeline: " .. vim.inspect(errors))
    assert(vim.wait(1000, function() return hints():find("review permission", 1, true) ~= nil end, 10),
      "permission action was not rendered: " .. hints())
    local approval = state.approval[1]
    local answers = {}
    for _, item in ipairs(approval.item_list) do answers[#answers + 1] = { item_id = item.id, choice_id = choice } end
    request("session.approval.resolve", { approval_id = approval.id, answer_list = answers })
    assert(vim.wait(1500, function()
      local current = status()
      return #(state.approval or {}) == 0 and current and current.animated and current.hint == "working"
    end, 10), "approval cleared without resuming timeline activity: " .. vim.inspect(status()))
    assert(not settled, "provider finished before the resumed status was observed")
    assert(not hints():find("review permission", 1, true), "stale permission action remained visible")
    assert(vim.wait(10000, function() return settled end, 10), "provider did not complete")
    assert(not problem, tostring(problem))
    assert(result.exchange.state == "complete", "approved exchange did not complete")
    if choice == "deny_once" then
      assert(resolution_list[#resolution_list].resolution_source == "user")
      assert(vim.wait(5000, function()
        return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
          :find("Rejected by the user", 1, true) ~= nil
      end, 10), "user denial lost its origin")
      assert(vim.deep_equal(vim.fn.readfile(permission_path), permission_before), "deny-once saved a rule")
    end
  end
  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
  for _, mode in ipairs({ "deny", "allow" }) do
    controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = mode } })
    assert(vim.wait(10000, function()
      return state.session.approval_mode == mode and not state.task_operation
    end, 10), "approval selection did not settle")
    local held = mock.hold(state.session.id, "chat")
    local settled, result, problem
    client.request({ method = "session.prompt.submit", params = { text = "Automatic approval decision: " .. mode }, session_id = assert(require("forge.session").harness.session).id }, function(value, error)
      settled, result, problem = true, value, error
    end)
    held:wait()
    local before = approval_requests
    held:release()
    assert(vim.wait(10000, function() return settled end, 10), "automatic approval did not resolve")
    assert(approval_requests == before and #state.approval == 0, "automatic approval opened a popup")
    assert(not problem and result.exchange.state == "complete", tostring(problem))
    if mode == "deny" then
      assert(vim.wait(5000, function()
        return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
          :find("Automatically rejected in reject-all approval mode", 1, true) ~= nil
      end, 10), "automatic denial lost its origin")
    end
    assert(vim.wait(5000, function()
      local current = status()
      return state.status and state.status.kind == "idle" and not state.busy
        and (not current or not current.animated)
    end, 10), "automatic approval decision left stale Working status")
  end
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "approval host was not collected")
print("harness_approval_resume_host: passed")
