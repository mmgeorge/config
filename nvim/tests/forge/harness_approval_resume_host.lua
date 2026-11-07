vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
vim.opt.runtimepath:prepend(root .. "/nvim")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
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
  if event == "provider_event" and payload.kind == "approval_requested" then
    approval_requests = approval_requests + 1
  elseif event == "provider_event" and payload.kind == "approval_resolved" then
    resolution_list[#resolution_list + 1] = payload.data
  end
end)
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then return state_snapshot.get() end
  local settled, value, failure
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
-- The status row lives in the presentation's separate status document, not the transcript.
local function status()
  local view = state.presentation and state.presentation.status_view
  local snapshot = view and view.snapshot
  if not snapshot then return nil end
  for index = #snapshot.block, 1, -1 do
    local metadata = snapshot.block[index].metadata
    if metadata and type(metadata.status) == "table" then return metadata.status end
  end
end
-- The status view renders action hints into its own buffer rows after the status text.
local function hints()
  local view = state.presentation and state.presentation.status_view
  if not view or not vim.api.nvim_buf_is_valid(view.buffer) then return "" end
  return table.concat(vim.api.nvim_buf_get_lines(view.buffer, 0, -1, false), "\n")
end
local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
-- The mock provider requests approval only for scripted host-tool calls, and host tools are
-- allowed unless a saved rule says otherwise. A saved Ask rule for harness_config, a read-only
-- tool offered on every turn, therefore produces one approval request per scripted call.
local approved_tool = "harness_config"
vim.fn.mkdir(vim.fs.joinpath(data, "forge"), "p")
vim.fn.writefile({ vim.json.encode({ permission = {
  read = { ["*"] = "allow" }, edit = { ["."] = "allow" }, webfetch = { ["*"] = "allow" },
  mcp = { ["*"] = "ask" }, tool = { [approved_tool] = "ask" },
} }) }, vim.fs.joinpath(data, "forge", "permissions.json"))
local function script_tool_call()
  request("session.provider.mock_response", { operation = "script", script = {
    steps = { { kind = "tool_call", name = approved_tool, arguments = vim.empty_dict() } }, terminal = "completed",
  } })
end
local function transcript_text()
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end
-- Completed exchanges keep tool output collapsed, so denial feedback is read from the durable
-- tool outcomes of the latest exchange rather than from the visible transcript.
local function latest_tool_reasons()
  local history = request("session.exchange.list", {})
  local reasons = {}
  for _, entry in ipairs(history[#history].entry or {}) do
    for _, tool in ipairs((entry.commentary or {}).tool or {}) do
      for _, execution in pairs(tool.store.execution or {}) do
        if execution.outcome and execution.outcome.reason then reasons[#reasons + 1] = execution.outcome.reason end
      end
    end
  end
  return table.concat(reasons, "\n")
end
---`session.prompt.submit` responds at admission, so turn completion is observed through activity.
local function await_idle(message)
  assert(vim.wait(10000, function()
    local current = status()
    return state.status and state.status.kind == "idle" and not require("forge.views.harness.activity").busy(state)
      and (not current or not current.animated)
  end, 10), message .. ": " .. vim.inspect(status()))
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10),
    "startup timeout: " .. vim.inspect(errors))
  local picker = require("forge.views.picker")
  local controller = require("forge.views.harness.controller")
  local permission_path = vim.fs.joinpath(data, "forge", "permissions.json")
  local permission_before = vim.fn.readfile(permission_path)
  -- The real clock never reaches this deadline; the mock control below expires the approval.
  request("session.input.timeout.configure", {
    approval_timeout = { amount = 60, unit = "m" },
  })
  -- Exchange counts per serialized phase prove that no expired approval schedules a second
  -- continuation: a duplicate would land before a later phase settles and change its count.
  local exchanges_per_timeout
  for iteration = 1, 2 do
    local before = approval_requests
    local exchanges_before = #request("session.exchange.list", {})
    script_tool_call()
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Continue after approval expires " .. iteration })
    controller.submit()
    -- The session owner publishes activity asynchronously, so busy follows submission by an event.
    assert(vim.wait(5000, function() return require("forge.views.harness.activity").busy(state) end, 10),
      "composer submission did not enter busy state")
    local seen = {}
    assert(vim.wait(10000, function()
      local current = status()
      seen[#seen + 1] = vim.inspect({ #state.approval, current and current.hint, picker.is_open() }, { newline = "" })
      if seen[#seen] == seen[#seen - 1] then seen[#seen] = nil end
      return #state.approval == 1 and current and current.hint == "permission" and picker.is_open()
    end, 10), "timed approval popup never appeared: " .. vim.inspect({ seen = seen, errors = errors, requests = approval_requests,
      approval = state.approval, status = status(), picker = picker.is_open(), transcript = transcript_text() }))
    local approval_id = state.approval[1].request_id
    assert(mock.expire_approval(state.session.id) == 1, "mock control did not expire exactly one approval")
    -- The scripted mock turn completes as soon as the tool result returns, so the resumed Working
    -- interval is not reliably observable; the completed exchange below proves the turn resumed.
    assert(vim.wait(5000, function()
      return #state.approval == 0 and not picker.is_open() and #resolution_list > 0
        and resolution_list[#resolution_list].request_id == approval_id
    end, 10), "timeout did not clear the popup: " .. vim.inspect(status()))
    local resolution = resolution_list[#resolution_list]
    assert(resolution.request_id == approval_id and resolution.targets[1].source == "Timeout" and resolution.targets[1].decision == "TimedOut",
      "expiry was not recorded as an automatic timeout: " .. vim.inspect(resolution))
    await_idle("completed timeout recovery retained Working")
    assert(not hints():find("review permission", 1, true), "expired permission hint remained visible")
    local text = latest_tool_reasons()
    assert(text:find("Automatically rejected because the approval timeout expired", 1, true), text)
    assert(text:find("The requested operation was not executed", 1, true), text)
    assert(text:find("Continue with permitted actions", 1, true), text)
    local snapshot = request("session.state.get", {})
    assert(snapshot.status.kind == "idle" and #snapshot.approval == 0)
    local history = request("session.exchange.list", {})
    local exchange_count = #history
    assert(history[exchange_count].state == "complete", "expired approval did not settle")
    local added = exchange_count - exchanges_before
    exchanges_per_timeout = exchanges_per_timeout or added
    assert(added >= 1 and added == exchanges_per_timeout,
      "answered timeout scheduled a duplicate continuation: " .. vim.inspect({ iteration = iteration, added = added,
        expected = exchanges_per_timeout }))
    assert(approval_requests == before + 1, "timeout retried the rejected operation")
    assert(vim.deep_equal(vim.fn.readfile(permission_path), permission_before), "timeout saved a permission rule")
  end
  request("session.input.timeout.configure", {
    approval_timeout = { amount = 0, unit = "s" },
  })
  for iteration, choice in ipairs({ "allow_once", "deny_once", "allow_once" }) do
    local exchanges_before = #request("session.exchange.list", {})
    script_tool_call()
    request("session.prompt.submit", { text = "Resume after approval " .. iteration })
    assert(vim.wait(10000, function()
      local current = status()
      return #(state.approval or {}) == 1 and current and current.hint == "permission"
    end, 10), "approval never reached the timeline: " .. vim.inspect(errors))
    assert(vim.wait(1000, function() return hints():find("review permission", 1, true) ~= nil end, 10),
      "permission action was not rendered: " .. hints())
    local approval = state.approval[1]
    local answers = {}
    for _, item in ipairs(approval.targets) do answers[#answers + 1] = { item_id = item.target_id, choice_id = choice } end
    request("session.approval.resolve", { approval_id = approval.request_id, answer_list = answers })
    assert(vim.wait(1500, function() return #(state.approval or {}) == 0 end, 10),
      "approval resolution did not clear the request: " .. vim.inspect(status()))
    await_idle("resolved approval left stale Working status")
    assert(not hints():find("review permission", 1, true), "stale permission action remained visible")
    local history = request("session.exchange.list", {})
    assert(history[#history].state == "complete", "approved exchange did not complete")
    -- The first resolved turn also bounds the final expired approval: a late duplicate
    -- continuation would add an exchange here.
    assert(#history == exchanges_before + 1, "resolved approval phase observed an extra exchange: "
      .. vim.inspect({ iteration = iteration, before = exchanges_before, after = #history }))
    if choice == "deny_once" then
      assert(resolution_list[#resolution_list].targets[1].source == "User")
      assert(latest_tool_reasons():find("Rejected by the user", 1, true), "user denial lost its origin: " .. latest_tool_reasons())
      assert(vim.deep_equal(vim.fn.readfile(permission_path), permission_before), "deny-once saved a rule")
    end
  end
  for _, mode in ipairs({ "deny", "allow" }) do
    local transition_settled
    controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = mode } }, nil,
      function() transition_settled = true end)
    assert(vim.wait(10000, function()
      return state.session.approval_mode == mode and transition_settled
    end, 10), "approval selection did not settle")
    local held = mock.hold(state.session.id, "chat")
    script_tool_call()
    request("session.prompt.submit", { text = "Automatic approval decision: " .. mode })
    held:wait()
    local before = approval_requests
    held:release()
    await_idle("automatic approval decision left stale Working status")
    assert(approval_requests == before and #state.approval == 0, "automatic approval opened a popup")
    local history = request("session.exchange.list", {})
    assert(history[#history].state == "complete", "automatic approval exchange did not complete")
    if mode == "deny" then
      assert(latest_tool_reasons():find("current approval mode denies requests that need confirmation", 1, true),
        "automatic denial lost its origin: " .. latest_tool_reasons())
    end
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
