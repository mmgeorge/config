vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local root = vim.fn.getcwd()
local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) return function() end end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local operation, starts, current_session = {}, 0, nil
local model_frames, original_refresh = {}, nil
client.session_events_subscribe(function(event, payload)
  if event == "task_operation" then operation[payload.id] = payload end
  if event == "backend_event" and payload.kind == "execution_state" then starts = starts + 1 end
  if event == "session_configured" then current_session = payload.session or payload end
end)
local function await(predicate, message)
  local settled = vim.wait(10000, predicate, 10)
  local state = require("forge.session").harness
  assert(settled, message .. " " .. vim.inspect({
    operation = operation, access = state.access_abbreviation,
    approval_mode = state.session and state.session.approval_mode, task_operation = state.task_operation,
  }))
end
local function request(session_id, method, params)
  local done, result, failure
  client.request({ session_id = session_id, method = method, params = params }, function(value, problem) done, result, failure = true, value, problem end)
  await(function() return done end, method)
  assert(not failure, failure)
  return result
end
local function open(session_id)
  local result, failure
  client.session_initialize(function(value, problem) result, failure = value, problem end, { session_id = session_id })
  await(function() return result or failure end, "initialization")
  assert(not failure, failure)
  return result
end
local function assert_configuration_presentation(state)
  local text = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
    { use_winbar = true, maxwidth = 240 }).str
  assert(not text:find("Task ", 1, true), "configuration was presented as task submission: " .. text)
  local suffix = ({ deny = "dx", allow = "ax" })[state.session.approval_mode] or ""
  assert(text:find(state.access_abbreviation .. suffix .. " · ", 1, true), "policy label disagrees with confirmed state: " .. text)
  for _, approval in ipairs({ "ask", "deny", "allow" }) do
    assert(not text:find(" · " .. approval .. " · ", 1, true), "approval retained a separate field: " .. text)
  end
end
local function assert_model_frames(deferred)
  assert(#model_frames > 0, "model configuration did not render")
  local pending_seen = false
  for _, text in ipairs(model_frames) do
    local _, markers = text:gsub("%*", "")
    assert(markers <= 1, "model configuration rendered multiple markers: " .. text)
    pending_seen = pending_seen or markers == 1
  end
  assert(pending_seen == deferred, "model pending marker disagrees with interrupted work: " .. vim.inspect(model_frames))
  assert(not model_frames[#model_frames]:find("*", 1, true), "applied model retained a pending marker")
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "blocking" } } } } })
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local startup_frames = {}
  original_refresh = controller.refresh_winbar
  controller.refresh_winbar = function(...)
    original_refresh(...)
    model_frames[#model_frames + 1] = vim.api.nvim_eval_statusline(vim.wo[state.transcript_win].winbar,
      { use_winbar = true, maxwidth = 240 }).str
    startup_frames[#startup_frames + 1] = { pending = state.initial_snapshot_pending,
      text = model_frames[#model_frames], session = state.session and state.session.id }
  end
  require("forge").open_harness()
  await(function() return state.ready and state.session end, "open Harness")
  local session_id = state.session.id
  local pending_seen = false
  for _, frame in ipairs(startup_frames) do
    if frame.pending or not frame.session then
      pending_seen = true
      assert(not frame.text:find("rxw", 1, true) and not frame.text:find("model", 1, true),
        "startup rendered unconfirmed session settings: " .. frame.text)
    end
  end
  assert(pending_seen, "host startup did not capture the first frame")
  await(function() return state.session.resolved_model == "mock-model" end, "initial model cache seed")
  for _, approval in ipairs({ "allow", "ask", "allow", "ask" }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/yolo" })
    controller.submit()
    await(function() return not state.task_operation and not state.busy end, "idle YOLO toggle")
    assert(state.access_abbreviation == "rxwx" and state.session.approval_mode == approval)
    assert(state.session.access.sandbox == false, "unsupported provider enabled sandbox on YOLO exit")
    assert_configuration_presentation(state)
    assert(request(session_id, "session.task.get", {}) == nil and starts == 0, "YOLO toggle created work")
  end
  for _, command in ipairs({ "/yolo", "/write full", "/ask" }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { command })
    controller.submit()
    assert_configuration_presentation(state)
    await(function() return not state.task_operation and not state.busy end, command)
    assert(request(session_id, "session.task.get", {}) == nil, "configuration created a workflow")
    assert(starts == 0, "idle configuration started provider work")
  end
  model_frames = {}
  local idle_applied
  controller.configure({ effort = "high" }, false, function(value) idle_applied = value end)
  await(function() return idle_applied and not state.task_operation and not state.busy end, "idle model configuration")
  assert_model_frames(false)
  local initial_response = mock.hold(session_id)
  require("forge.views.harness.controller").task_transition({ action = "goal", text = "Continue until interrupted" })
  local pending = initial_response:wait()
  assert(pending.prompt:find("Continue until interrupted", 1, true), "held a different goal")
  await(function() return starts == 1 end, "goal start")
  local access = { sandbox = false, read_access = "workspace", write_access = "workspace",
    readable_directory = { data }, writable_directory = { workspace }, windows_sandbox = "elevated" }
  local applied
  model_frames = {}
  require("forge.views.harness.controller").configure({ access = access, effort = "low" }, false, function(value) applied = value end)
  assert_configuration_presentation(state)
  await(function() return starts == 2 end, "configuration did not resume goal")
  assert(applied == true and state.session.access.sandbox == false, "configuration waited for goal completion")
  assert_model_frames(true)
  assert(current_session.access.sandbox == false and #current_session.access.writable_directory == 1)
  assert(current_session.access.read_access == "workspace" and #current_session.access.readable_directory == 1)
  assert(current_session.approval_mode == "ask")
  local task_id = current_session.active_task.revision
  require("forge.views.harness.controller").toggle_write()
  await(function() return starts == 3 end, "Read approval mode paused the task")
  assert(current_session.approval_mode == "ask" and current_session.access.write_access == "off" and current_session.access.sandbox == false)
  assert(current_session.active_task.revision == task_id)
  for index, step in ipairs({
    { "/allow", "r", "allow" },
    { "/write full", "rwx", "allow" },
    { "/read full", "rxwx", "allow" },
    { "/deny", "rxwx", "deny" },
    { "/yolo", "rxwx", "allow" },
    { "/yolo", "rxwx", "ask" },
    { "/yolo", "rxwx", "allow" },
    { "/write", "rx", "allow" },
  }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { step[1] })
    controller.submit()
    assert_configuration_presentation(state)
    await(function() return starts == index + 3 and state.access_abbreviation == step[2] end, step[1])
    assert(state.session.approval_mode == step[3] and state.session.active_task.revision == task_id)
    assert(state.session.access.sandbox == false, "policy command changed sandbox selection")
  end
  request(session_id, "session.command.apply", { operation_id = "pause", action = "pause" })
  await(function() return operation.pause and operation.pause.state == "completed" end, "pause")
  request(session_id, "session.turn.cancel", {})
  await(function() return not state.busy end, "paused provider cleanup")
  local paused_task = request(session_id, "session.task.get", {})
  assert(paused_task.revision ~= task_id and paused_task.goal.state == "paused", "pause did not renew the task revision")
  task_id = paused_task.revision
  local function submit(text)
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
    controller.submit()
  end
  local previous_operations = {}
  for id in pairs(operation) do previous_operations[id] = true end
  submit("/write full")
  submit("/ask")
  await(function()
    local completed = 0
    for id, current in pairs(operation) do
      if not previous_operations[id] and current.state == "completed" then completed = completed + 1 end
    end
    return completed == 2
  end, "rapid permission operations did not settle")
  local observed = request(session_id, "session.state.get", {})
  assert(state.access_abbreviation == "rxwx" and state.session.approval_mode == "ask",
    "rapid independent permission changes: " .. vim.inspect({
      displayed = state.access_abbreviation, persisted = observed.access_abbreviation,
      access = observed.session.access, approval = observed.session.approval_mode,
    }))
  for _ = 1, 4 do submit("/write") end
  await(function() return state.access_abbreviation == "rxw" and not state.task_operation end,
    "rapid write toggles were lost or restored full access")
  local paused = require("forge.views.harness.task").current(state)
  assert(paused.revision == task_id and require("forge.views.harness.task").status(paused) == "paused",
    "permission burst changed paused work: " .. vim.inspect({ before = task_id, current = paused,
      persisted = request(session_id, "session.task.get", {}) }))
  model_frames = {}
  local paused_applied
  controller.configure({ effort = "high" }, false, function(value) paused_applied = value end)
  await(function() return paused_applied and not state.task_operation and not state.busy end, "paused model configuration")
  assert_model_frames(false)
  submit("/yolo")
  submit("/yolo")
  await(function() return state.access_abbreviation == "rxwx" and state.session.approval_mode == "ask"
    and not state.task_operation end, "paused YOLO toggles")
  assert(state.session.access.sandbox == false)
  assert(require("forge.views.harness.task").status(require("forge.views.harness.task").current(state)) == "paused", "YOLO exit resumed paused work")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Queued after the current task" })
  controller.queue_submit()
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Unsubmitted draft" })
  controller.toggle_write()
  await(function() return state.access_abbreviation == "rx" and not state.task_operation end, "draft-safe write toggle")
  assert(#state.queue == 1 and state.queue[1].text == "Queued after the current task")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "Unsubmitted draft")
  local resumed_response = mock.hold(session_id)
  controller.task_transition({ action = "resume" })
  resumed_response:wait()
  assert(state.session.active_task.revision ~= task_id and state.access_abbreviation == "rx")
  assert(#state.queue == 1, "resuming consumed a follow-up during active work")
  controller.task_transition({ action = "pause" })
  await(function()
    local current = require("forge.views.harness.task").current(state)
    return current and require("forge.views.harness.task").status(current) == "paused"
  end, "pause held task")
  request(session_id, "session.turn.cancel", {})
  await(function()
    local current = require("forge.views.harness.task").current(state)
    return not state.task_operation and not state.busy and current and require("forge.views.harness.task").status(current) == "paused"
  end, "pause held response")
  assert(require("forge.views.harness.task").status(require("forge.views.harness.task").current(state)) == "paused")
  task_id = state.session.active_task.revision
  local before_restart = request(session_id, "session.task.get", {})
  assert(#state.queue == 1, "pausing discarded queued input")
  client._client.process:kill(9)
  await(function() return client._client.process == nil end, "shutdown")
  local recovered = open(session_id)
  assert(recovered.session.approval_mode == "ask" and recovered.access_abbreviation == "rx" and recovered.session.access.write_access == "off")
  assert(recovered.session.access.sandbox == false and #recovered.session.access.writable_directory == 1)
  assert(recovered.session.access.read_access == "full" and #recovered.session.access.readable_directory == 1)
  local recovered_task = recovered.session.active_task
  assert(recovered_task.revision ~= task_id and recovered_task.created_at_ms == before_restart.created_at_ms
    and recovered_task.goal.objective == before_restart.goal.objective and recovered_task.goal.state == "paused",
    "recovery did not preserve and fence the paused goal: " .. vim.inspect(recovered_task))
  local created = request(session_id, "session.new", { name = "Cached default" })
  assert(created.session.id ~= session_id and created.session.model == "default")
  assert(created.session.resolved_model == "mock-model", "new conversation waited for another model catalog lookup: "
    .. vim.inspect({ recovered = recovered.session.resolved_model, created = created.session.resolved_model }))
end, debug.traceback)
if original_refresh then require("forge.views.harness.controller").refresh_winbar = original_refresh end
local state = require("forge.session").harness
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
if not success then error(failure) end
print("harness_access_host: passed")
vim.cmd("qa!")
