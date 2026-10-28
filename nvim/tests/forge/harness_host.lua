vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local executable = data .. "/" .. name
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors = {}
local cleaning = false
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return #errors > 0 or (state.presentation and state.presentation.ready) end, 10), "native Harness did not open")
  assert(#errors == 0, table.concat(errors, "\n"))
  require("forge.views.harness.settings").log(state.session.id, "on")
  assert(vim.wait(3000, function() return require("forge.infra.perf").enabled("harness") end, 10))
  local prompt = "literal ** prompt with native ownership"
  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
  local held_response = mock.hold(state.session.id, "chat")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { prompt })
  controller.submit()
  local pending_response = held_response:wait()
  assert(pending_response.prompt == prompt and pending_response.mode == "chat",
    "mock response hold captured a different request")
  assert(not table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    :find("Mock response:", 1, true), "mock provider responded before explicit release")
  held_response:release()
  assert(vim.wait(10000, function()
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return #errors > 0 or (not state.busy and text:find("Mock response:", 1, true) ~= nil)
  end, 10), "native Harness response did not settle")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "durable native admission did not clear composer")
  assert(vim.wait(10000, function()
    for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line:match("^● Thought %d+s$") then return true end
    end
  end, 10), "fresh host did not render the current metrics header:\n"
    .. table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"))
  local response_marker = false
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.transcript_buf,
    state.presentation.transcript.namespace, 0, -1, { details = true })) do
    local decoration = mark[4]
    if decoration.sign_hl_group == "ForgeHarnessResponse" then
      assert(vim.trim(decoration.sign_text) == "●", "final response used a fold marker")
      response_marker = true
    end
  end
  assert(response_marker, "final response omitted its large dot marker")
  local function compose_and_press(text, key)
    vim.api.nvim_set_current_win(state.composer_win)
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
    local binding = vim.fn.maparg(key, "n", false, true)
    assert(binding.buffer == 1 and type(binding.callback) == "function", "missing composer binding " .. key)
    binding.callback()
  end
  local function pending_message(text)
    for _, message in ipairs(state.input.confirmed) do
      if message.text == text then return message end
    end
  end
  local function input_settled()
    return state.input.sending == nil and #state.input.pending == 0
  end
  local function request(method, params, expected_error)
    local done, value, failure = false
    client.request_for(state.session.id, method, params, function(result, error)
      value, failure, done = result, error, true
    end)
    assert(vim.wait(3000, function() return done end, 10), method .. " waited for execution")
    if expected_error then
      assert(failure and tostring(failure):find(expected_error, 1, true), tostring(failure))
    else
      assert(not failure, tostring(failure))
    end
    return value
  end
  local active_prompt = "Keep this response active while sending follow-ups"
  local first_queued = "First explicitly queued follow-up"
  local second_queued = "Second explicitly queued follow-up"
  local first_steering = "Immediate steering behind one queued follow-up"
  local second_steering = "Immediate steering behind two queued follow-ups"
  local active_response = mock.hold(state.session.id, "chat")
  local idle_session = request("session.new", { name = "Idle cached deletion regression" }).session.id
  compose_and_press(active_prompt, "<C-s>")
  assert(active_response:wait().prompt == active_prompt)
  assert(request("session.resume", { session_id = state.session.id }).session.id == state.session.id,
    "reopening an active session waited for provider completion")
  request("session.delete", { session_id = idle_session })
  for _, saved in ipairs(request("session.list", { scope = "all" })) do
    assert(saved.id ~= idle_session, "deleted cached session remained in the picker")
  end
  local renamed = "Renamed while the provider is active"
  compose_and_press("/rename " .. renamed, "<C-s>")
  assert(vim.wait(3000, function()
    return #errors > 0 or state.session.name == renamed
  end, 10), "active session rename did not finish")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(active_response:wait().prompt == active_prompt, "rename interrupted the provider response")
  request("session.rename", { session_id = state.session.id, name = "stale generated name",
    expected_name = "old name" }, "changed during generation")
  request("session.delete", { session_id = state.session.id }, "cannot delete the active session")
  local exchanges = request("exchange.list", {})
  local reviewed = assert(exchanges[1], "no exchange available for comment regression")
  local comment = request("exchange.comment.save", { id = "active-review-comment",
    exchange_id = reviewed.id, file_path = "example.rs", new_line = 1,
    body = "Comment while the provider is active", created_at_ms = 0 })
  assert(comment.body == "Comment while the provider is active")
  request("exchange.comment.save", { id = "foreign-comment", exchange_id = "missing-exchange",
    file_path = "example.rs", new_line = 1, body = "invalid", created_at_ms = 0 },
    "does not belong to the active session")
  request("plan.declaration", { plan_id = "missing-plan", path = "plan.json", revision = 1 },
    "plan artifact not found")
  request("exchange.rollback.preview", {}, "preview_id is required")
  local snapshot = request("state.get", {})
  assert(snapshot.session.name == renamed, "durable snapshot lost active rename")
  compose_and_press(first_queued, "<C-q>")
  assert(vim.wait(5000, function()
    local queued = pending_message(first_queued)
    return input_settled() and queued and queued.intent == "queue" and queued.delivery == "queued"
  end, 10), "Ctrl-Q did not retain the explicit follow-up")
  compose_and_press(first_steering, "<C-s>")
  assert(vim.wait(5000, function()
    return #errors > 0 or (input_settled() and not pending_message(first_steering))
  end, 10), "Ctrl-S waited behind an explicitly queued message instead of steering")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(active_response:wait().prompt == active_prompt, "steering completed the active response")
  assert(pending_message(first_queued).delivery == "queued", "steering consumed the queued follow-up")
  compose_and_press(second_queued, "<C-q>")
  assert(vim.wait(5000, function() return input_settled() and pending_message(second_queued) end, 10))
  compose_and_press(second_steering, "<C-s>")
  assert(vim.wait(5000, function()
    return #errors > 0 or (input_settled() and not pending_message(second_steering))
  end, 10), "Ctrl-S waited behind multiple explicit follow-ups")
  assert(#errors == 0, table.concat(errors, "\n"))
  for _, text in ipairs({ first_queued, second_queued }) do
    local queued = pending_message(text)
    assert(queued and queued.intent == "queue" and queued.delivery == "queued", "Ctrl-S changed the explicit queue")
  end
  local first_queued_response = mock.hold(state.session.id, "chat")
  local second_queued_response = mock.hold(state.session.id, "chat")
  active_response:release()
  assert(first_queued_response:wait().prompt == first_queued, "steering changed queued FIFO delivery")
  first_queued_response:release()
  assert(second_queued_response:wait().prompt == second_queued, "explicit follow-ups were reordered")
  second_queued_response:release()
  assert(vim.wait(10000, function()
    return #errors > 0 or (not state.busy and input_settled() and #state.input.confirmed == 0)
  end, 10), "queued follow-ups did not finish after the active response")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(state.session.name == renamed, "provider completion overwrote the active rename")
  dofile(root .. "/nvim/tests/forge/support/submission_races.lua")({
    state = state, controller = controller, client = client, mock = mock, errors = errors,
    request = request, compose_and_press = compose_and_press,
  })
  local log_status
  client.request("trace.status", {}, function(result, error)
    assert(not error, error)
    log_status = result
  end)
  assert(vim.wait(3000, function() return log_status ~= nil end, 10))
  assert(log_status.enabled and log_status.path:find(state.session.id, 1, true))
  local prompt_record
  for _, line in ipairs(vim.fn.readfile(log_status.path)) do
    if line:find(prompt, 1, true) then prompt_record = line break end
  end
  assert(prompt_record, "session trace lost prompt body")
  local cleared_status
  client.request("trace.session.clear", {}, function(result, error)
    assert(not error, error)
    cleared_status = result
  end)
  assert(vim.wait(3000, function() return cleared_status ~= nil end, 10))
  assert(cleared_status.enabled and cleared_status.path == log_status.path)
  assert(not vim.tbl_contains(vim.fn.readfile(log_status.path), prompt_record), "prior session trace survived clear")
  local compaction_status = {}
  local unsubscribe_compaction = client.subscribe(function(event, payload, session_id)
    if event == "document_changed" and session_id == state.session.id and payload.status then
      compaction_status[#compaction_status + 1] = payload.status.kind
    end
  end)
  controller.compact()
  assert(vim.wait(5000, function()
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return #errors > 0 or (text:find("Context compacted", 1, true) and not state.task_operation)
  end, 10), "manual compaction did not render in the native timeline")
  unsubscribe_compaction()
  assert(vim.tbl_contains(compaction_status, "compacting"), "native host never published Compacting status")
  assert(compaction_status[#compaction_status] == "idle", "native host retained Compacting after completion")
  assert(#errors == 0, table.concat(errors, "\n"))
  local compaction_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("Context compacted", 1, true) then
      assert(not compaction_row, "manual compaction created duplicate rows")
      assert(not line:find("manual", 1, true) and not line:find("automatic", 1, true),
        "compaction row exposed the request origin")
      compaction_row = row
    end
  end
  assert(compaction_row, "compaction row missing")
  local collapsed = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(not collapsed:find("Mock compacted context", 1, true), "compaction details did not default to collapsed")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { compaction_row, 0 })
  controller.toggle_activity()
  assert(vim.wait(3000, function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      :find("Mock compacted context preserves the conversation constraints.", 1, true) ~= nil
  end, 10), "expanding compaction did not expose its generated summary")
  assert(vim.bo[state.composer_buf].modifiable and not vim.bo[state.transcript_buf].modifiable)
  local previous = state.transcript_win
  vim.api.nvim_set_current_win(previous)
  vim.cmd("vsplit")
  local secondary = vim.api.nvim_get_current_win()
  state.presentation.refresh_views()
  vim.api.nvim_win_close(previous, true)
  assert(vim.wait(3000, function() return state.presentation.views[previous] == nil and state.presentation.views[secondary] ~= nil end, 10))
  controller.render()
  assert(vim.wait(3000, function() return not state.presentation.syncing end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  local restart_hold = mock.hold(state.session.id, "chat")
  compose_and_press("Active before host replacement", "<C-s>")
  restart_hold:wait()
  compose_and_press("Recover queued text after host replacement", "<C-q>")
  assert(vim.wait(3000, function()
    return input_settled() and pending_message("Recover queued text after host replacement") ~= nil
  end, 10), "restart fixture did not retain the queued message")
  local restart_message_id = pending_message("Recover queued text after host replacement").id
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft survives collected host" })
  local state_settled, state_error = false, nil
  client.request("state.get", {}, function(_, request_error)
    state_error = request_error
    state_settled = true
  end)
  assert(vim.wait(3000, function() return state_settled and not state.state_sync_pending end, 10),
    "Harness state did not settle before host collection")
  assert(not state_error, state_error)
  local old_generation = client.host_generation()
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "old Harness host was not collected")
  local restarted = false
  client.start_harness(function(snapshot, restart_error)
    if restart_error then if not cleaning then errors[#errors + 1] = restart_error end return end
    controller.activate_snapshot(snapshot)
    restarted = true
  end)
  assert(vim.wait(10000, function() return #errors > 0 or (restarted and state.presentation and state.presentation.ready and state.presentation.host_generation ~= old_generation) end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(state.session.name == renamed, "active rename did not survive host restart")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft survives collected host")
  local recovered_queue = vim.wait(3000, function()
    return not state.input.refreshing and #state.input.rejected == 1
  end, 10)
  assert(recovered_queue, "host replacement lost the recoverable queue: " .. vim.inspect({
    confirmed = state.input.confirmed, rejected = state.input.rejected, queue = state.queue,
    epoch = state.runtime_epoch, input_epoch = state.input.runtime_epoch,
  }))
  local recovery = state.input.rejected[1]
  assert(recovery.id == restart_message_id and recovery.delivery == "unknown" and recovery.local_only,
    "host replacement made unresolved queued text executable")
  for _, exchange in ipairs(request("exchange.list", {})) do
    assert(exchange.prompt ~= recovery.text, "host replacement automatically replayed queued text")
  end
  require("forge.views.harness.input_state").discard_rejected(state, recovery.id)
  assert(require("forge.infra.perf").enabled("harness"), "saved logging preference lost on restart")
  local compaction_count = 0
  for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("Context compacted", 1, true) then compaction_count = compaction_count + 1 end
  end
  assert(compaction_count == 1, "compaction timeline history did not survive host restart")
  local initialized = vim.system({ "git", "init", workspace }, { text = true }):wait(10000)
  assert(initialized.code == 0, initialized.stderr)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan build the feature" })
  controller.submit()
  assert(vim.wait(10000, function() return #errors > 0 or (not state.busy and state.active_plan) end, 10),
    "complete-text planning submission did not settle")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "planning admission did not clear the draft")
end, debug.traceback)
cleaning = true
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("harness_host: passed")
