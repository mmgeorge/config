-- Unit tests for `forge.git.write` (write.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["native_git_write"] = function()
  local client = require("forge.client")
  local writer = require("forge.git.write")
  local perf = require("forge.infra.perf")
  local event_list = {}
  perf.event = function(_, event, payload)
    event_list[#event_list + 1] = { event = event, payload = vim.deepcopy(payload) }
  end
  local acknowledged, operation_list, chunks = false, {}, {}
  client.request = function(request_specification, callback, progress)
  local method = request_specification.method
  local params = request_specification.params or {}

    eq(method, "repository.write")
    operation_list[#operation_list + 1] = params.operation
    if params.operation == "prepare" then
      assert(params.workspace == "fixture" and params.action.kind == "push")
      callback({ intent = "prepared", timing_us = { write_queue = 1250000 } }, nil)
    elseif params.operation == "submit" then
      eq(params.intent, "prepared")
      progress({ stream = "stderr", sequence = 0, bytes = vim.base64.encode("Push") })
      eq(#chunks, 0, { fail_reason = "partial progress line was published" })
      progress({ stream = "stderr", sequence = 1, bytes = vim.base64.encode("ing\r") })
      progress({ stream = "stderr", sequence = 2, bytes = vim.base64.encode("\ndone") })
      callback({ operation_id = "1", target = { { completion = "completed", exit_code = 0 } } }, nil)
    elseif params.operation == "acknowledge" then
      eq(params.operation_id, "1")
      acknowledged = true
      callback({}, nil)
    else error("unexpected native writer operation") end
  end
  local result
  writer.execute("fixture", { kind = "push" }, function(value) result = value end, function(text, stream)
    eq(stream, "stderr")
    chunks[#chunks + 1] = text
  end, "test-write")
  assert(result.ok and result.stderr == "Pushing\r\ndone")
  assert(table.concat(chunks) == "Pushing\ndone" and acknowledged)
  eq(table.concat(operation_list, ","), "prepare,submit,acknowledge")
  local expected = { "git.write.prepare.start", "git.write.prepare.complete", "git.write.prepare.native.write_queue", "git.write.prepared", "git.write.submit.start",
    "git.write.progress.first", "git.write.complete", "git.write.acknowledge.start", "git.write.acknowledge.complete" }
  assert(#event_list == #expected, "writer timing count differs")
  for index, event in ipairs(event_list) do
    assert(event.event == expected[index], "writer timing order differs")
    eq(event.payload.request_id, "test-write", { fail_reason = "writer timing lost action correlation" })
    assert(event.payload.elapsed_ms >= 0, "writer timing omitted elapsed time")
  end
  eq(event_list[3].payload.ms, 1250, { fail_reason = "native timing lost microsecond conversion" })
  assert(event_list[7].payload.operation_id == "1" and event_list[7].payload.code == 0)

  client.request = function(request_specification, callback)
  local params = request_specification.params or {}

    if params.operation == "prepare" then callback({ intent = "uncertain" }, nil)
    elseif params.operation == "submit" then callback({ operation_id = "2", target = { { completion = "outcome_unknown", diagnostic = "connection closed" } } }, nil)
    else error("uncertain result was automatically acknowledged") end
  end
  writer.execute("fixture", { kind = "push" }, function(value)
    assert(not value.ok and value.output:find("connection closed", 1, true))
  end)

  client.request = function(request_specification, callback)
  local params = request_specification.params or {}

    if params.operation == "prepare" then callback({ intent = "incomplete" }, nil)
    elseif params.operation == "submit" then callback({ operation_id = "3", target = {} }, nil)
    else error("incomplete result was automatically acknowledged") end
  end
  writer.execute("fixture", { kind = "push" }, function(value)
    assert(not value.ok and value.output:find("no verifiable target receipt", 1, true))
  end)

  local cancelled = false
  client.request = function(request_specification, callback, progress)
  local params = request_specification.params or {}

    if params.operation == "prepare" then callback({ intent = "misordered" }, nil)
    elseif params.operation == "submit" then
      progress({ stream = "stdout", sequence = 2, bytes = vim.base64.encode("bad order") })
      callback({ operation_id = "4", target = { { completion = "completed", exit_code = 0 } } }, nil)
    elseif params.operation == "cancel" then cancelled = true callback({}, nil)
    else error("invalid progress receipt was automatically acknowledged") end
  end
  writer.execute("fixture", { kind = "push" }, function(value)
    assert(not value.ok and value.output:find("sequence differs", 1, true))
  end)
  assert(cancelled, "invalid progress did not cancel its native owner")

  operation_list = {}
  client.request = function(request_specification, callback)
  local params = request_specification.params or {}

    operation_list[#operation_list + 1] = params.operation
    if params.operation == "prepare" then callback({ intent = "completed" }, nil)
    elseif params.operation == "submit" then
      callback({ operation_id = "5", target = { { completion = "completed", exit_code = 0 } } }, nil)
    elseif params.operation == "acknowledge" then callback({}, nil)
    else error("completed write should not receive a cancellation request") end
  end
  local completed
  local cancel = writer.execute("fixture", { kind = "push" }, function(value) completed = value end)
  assert(completed and completed.ok, "completed write did not report success")
  cancel()
  eq(table.concat(operation_list, ","), "prepare,submit,acknowledge", { fail_reason = "completed write accepted a late cancellation" })

  operation_list = {}
  local prepared_callback
  client.request = function(request_specification, callback)
  local params = request_specification.params or {}

    operation_list[#operation_list + 1] = params.operation
    if params.operation == "prepare" then prepared_callback = callback
    elseif params.operation == "cancel" then callback({}, nil)
    else error("cancelled write submitted after cancellation") end
  end
  local cancelled_before_prepare
  local cancel_before_prepare = writer.execute("fixture", { kind = "push" }, function(value)
    cancelled_before_prepare = value
  end)
  cancel_before_prepare()
  eq(cancelled_before_prepare, nil, { fail_reason = "cancellation settled before prepare returned an intent" })
  prepared_callback({ intent = "cancel-before-submit" }, nil)
  assert(not cancelled_before_prepare.ok and cancelled_before_prepare.output:find("cancelled before submission", 1, true))
  eq(table.concat(operation_list, ","), "prepare,cancel", { fail_reason = "cancelled write reached native submission" })

  operation_list = {}
  local submitted_callback
  client.request = function(request_specification, callback)
  local params = request_specification.params or {}

    operation_list[#operation_list + 1] = params.operation
    if params.operation == "prepare" then callback({ intent = "cancel-during-submit" }, nil)
    elseif params.operation == "submit" then submitted_callback = callback
    elseif params.operation == "cancel" then callback({}, nil)
    else error("uncertain cancelled write was automatically acknowledged") end
  end
  local cancelled_during_submit
  local cancel_during_submit = writer.execute("fixture", { kind = "push" }, function(value)
    cancelled_during_submit = value
  end)
  cancel_during_submit()
  submitted_callback({ operation_id = "6", target = {
    { completion = "completed", exit_code = 0 },
    { completion = "outcome_unknown", diagnostic = "cancel raced with process completion" },
    { completion = "not_started", exit_code = 1 },
  } }, nil)
  assert(not cancelled_during_submit.ok and cancelled_during_submit.output:find("cancel raced with process completion", 1, true))
  eq(table.concat(operation_list, ","), "prepare,submit,cancel", { fail_reason = "uncertain cancellation acknowledged a recoverable outcome" })
end

return T
