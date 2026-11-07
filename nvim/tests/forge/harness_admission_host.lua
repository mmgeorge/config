vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
vim.opt.runtimepath:prepend(original_directory .. "/nvim")
local fixture_evidence = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/evidence_directory.lua")("harness-admission")
local evidence = fixture_evidence.path
local workspace, data = evidence .. "/workspace", evidence .. "/data"
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local function git(...)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, { ... })
  local result = vim.system(command, { text = true }):wait()
  assert(result.code == 0, result.stderr)
end
git("init", "--quiet")
vim.fn.writefile({ "Admission fixture" }, workspace .. "/README.md")
git("add", "README.md")
git("-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid", "commit", "--quiet", "-m", "Fixture baseline")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local diagnostics, last_snapshot = {}, nil
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
notifications.error = function(message, title)
  diagnostics[#diagnostics + 1] = { title = title, message = tostring(message) }
  return original_error(message, title)
end
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
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then last_snapshot = state_snapshot.get() return last_snapshot end
  local completion = finish(start(method, params))
  assert(not completion.failure, tostring(completion.failure))
  if method == "session.state.get" then last_snapshot = completion.value end
  return completion.value
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = true,
    harness = { provider = "mock" } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session ~= nil end, 10), "startup timeout")
  local function script(steps)
    request("session.provider.mock_response", { operation = "script", script = {
      steps = steps, terminal = "completed",
    } })
  end
  script({ { kind = "tool_call", name = "question_ask", arguments = { wait = false, questions = {
    { header = "Fixture choice", question = "Which fixture should be used?", allow_freeform = true,
      options = { { label = "Local", description = "Use the local fixture" } } },
    { header = "Optional detail", question = "Add an optional detail?", options = {} },
  } } } })
  request("session.prompt.submit", { text = "Ask the fixture questions." })
  local before
  assert(vim.wait(10000, function()
    before = request("session.state.get")
    return before.questions and #before.questions.questions == 2 and before.status.kind == "idle"
  end, 25), "prompt dispatch did not publish and settle the fixture questions")
  local question, optional = before.questions.questions[1], before.questions.questions[2]
  local question_id = question.id
  local target = { owner = "question", question_id = question_id, revision = question.content.revision }
  local function current_question(snapshot)
    for _, item in ipairs(snapshot.questions.questions) do
      if item.id == question_id then return item end
    end
    error("original question disappeared")
  end
  local function clarification(id, text)
    local input = request("session.input.get")
    return start("session.input.apply", { id = id, control_revision = input.control_revision,
      after_control = input.control_operation_id, target = target,
      action = { kind = "submit", message = { id = id .. "-input", revision = 0, text = text,
        purpose = { kind = "user" }, intent = "auto", attachments = {} } } })
  end
  script({ { kind = "tool_call", name = "question_reply", arguments = {
    question_id = question_id, revision = question.content.revision, message_id = "clarify-first-input",
    text = "The fixture choices are local and leave this question unanswered.",
  } } })
  local active = clarification("clarify-first", "Explain the available choices.")
  local queued = clarification("clarify-second", "A second clarification.")
  local first_result, second_result = finish(active), finish(queued)
  assert(not first_result.failure, tostring(first_result.failure))
  assert(not second_result.failure, "FIFO clarification was rejected as busy: " .. tostring(second_result.failure))
  local after
  assert(vim.wait(10000, function()
    after = request("session.state.get")
    return #current_question(after).discussion == 3 and after.status.kind == "idle"
  end, 25), "clarification replies did not settle against the original question")
  local discussion = current_question(after).discussion
  local by_id, reply = {}, nil
  for _, message in ipairs(discussion) do
    by_id[message.id] = message
    if message.kind == "reply" then reply = message end
  end
  assert(by_id["clarify-first-input"] and by_id["clarify-first-input"].kind == "clarification"
    and by_id["clarify-first-input"].text == "Explain the available choices.", "first clarification was lost")
  assert(by_id["clarify-second-input"] and by_id["clarify-second-input"].kind == "clarification"
    and by_id["clarify-second-input"].text == "A second clarification.", "queued clarification was lost")
  assert(reply and reply.reply_to == "clarify-first-input"
    and reply.text == "The fixture choices are local and leave this question unanswered.",
    "scripted reply did not attach to the first clarification")
  assert(current_question(after).resolution.kind == "pending", "clarification answered the question")
  local controller = require("forge.views.harness.controller")
  local picker = require("forge.views.picker")
  assert(vim.wait(10000, function()
    for _, current in ipairs((state.questions or {}).questions or {}) do
      if current.id == question_id and current.resolution.kind == "pending" then return true end
    end
    return false
  end, 10), "question publication did not reach the view")
  picker.close(false)
  controller.reopen_question()
  assert(picker.is_open("questions"), "pending question could not reopen after clarification")
  picker.close(false)
  assert(not (state.execution_notice or ""):find("Stopped", 1, true))
  for _, change in ipairs({
    { kind = "toggle_write_enabled" },
    { kind = "set_approval", mode = "deny" },
    { kind = "set_approval", mode = "allow" },
  }) do
    -- Wait for the command response itself; Lua keeps no in-flight task-operation flag.
    local applied
    controller.task_transition({ action = "permission", change = change }, nil, function(ok) applied = ok end)
    assert(vim.wait(10000, function() return applied ~= nil and not require("forge.views.harness.activity").busy(state) end, 10),
      "permission change did not settle while a question waited")
    assert(applied, "permission change failed while a question waited")
    local waiting = request("session.state.get")
    assert(current_question(waiting).resolution.kind == "pending", "permission change answered the question")
    assert(waiting.session.access.write_access == "off", "approval change restored writes")
  end
  assert(vim.wait(10000, function()
    local settled = request("session.state.get")
    for _, input in ipairs(request("session.input.get").messages) do
      if input.id == "clarify-second-input" then return false end
    end
    return settled.status.kind == "idle"
  end, 25), "reconfiguration did not deliver the interrupted clarification")
  assert(#diagnostics == 0, vim.inspect(diagnostics))
  request("session.question.answer", { command = { operation_id = "dismiss-optional", question_id = optional.id,
    revision = optional.content.revision, action = { kind = "dismiss" } } })
  local dismissed = request("session.state.get")
  assert(dismissed.questions.questions[2].resolution.kind == "dismissed", "optional question did not dismiss")
  request("session.question.answer", { command = { operation_id = "answer-fixture", question_id = question_id,
    revision = question.content.revision, action = { kind = "answer", answer = { kind = "other", text = "Use the local fixture" } } } })
  request("session.question.answer", { command = { operation_id = "correct-fixture", question_id = question_id,
    revision = question.content.revision, action = { kind = "correct", text = "Keep the local fixture" } } })
  local answered = current_question(request("session.state.get"))
  assert(answered.resolution.kind == "answered", "answer and correction did not retain resolution")
  assert(#answered.discussion == 5, "answer or correction lost its question discussion")
  assert(vim.wait(10000, function()
    local input = request("session.input.get")
    local final = request("session.state.get")
    return #input.messages == 0 and final.status.kind == "idle"
  end, 25), "answer and correction delivery did not settle")
  assert(#diagnostics == 0, vim.inspect(diagnostics))
end, debug.traceback)
if vim.g.forge_admission_inspect and success then
  print("harness_admission_host: passed, inspection ready")
  return
end
if not success then
  local diagnostic = { workspace = workspace, data = data, snapshot = last_snapshot, errors = diagnostics,
    host_stderr = client._client.stderr,
    failure = failure, transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false) or nil }
  vim.fn.writefile(vim.split(vim.inspect(diagnostic), "\n", { plain = true }), evidence .. "/failure.txt")
  failure = tostring(failure) .. "\nRetained diagnostics: " .. evidence .. "/failure.txt"
end
notifications.error = original_error
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath = original_stdpath
-- Remove passing evidence only after the host exits and releases its files.
local release_failure = fixture_evidence.release(success and collected)
assert(success and collected, failure or "host was not collected")
assert(not release_failure, release_failure)
print("harness_admission_host: passed")
