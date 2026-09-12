vim.loader.enable(false)
local client = require("forge.client")
local writer = require("forge.git.write")
local original = client.request_host
local ok, failure = xpcall(function()
  local acknowledged, operation_list, chunks = false, {}, {}
  client.request_host = function(method, params, callback, progress)
    assert(method == "repository.write")
    operation_list[#operation_list + 1] = params.operation
    if params.operation == "prepare" then
      assert(params.workspace == "fixture" and params.action.kind == "push")
      callback({ intent = "prepared" }, nil)
    elseif params.operation == "submit" then
      assert(params.intent == "prepared")
      progress({ stream = "stderr", sequence = 0, bytes = vim.base64.encode("Push") })
      assert(#chunks == 0, "partial progress line was published")
      progress({ stream = "stderr", sequence = 1, bytes = vim.base64.encode("ing\r") })
      progress({ stream = "stderr", sequence = 2, bytes = vim.base64.encode("\ndone") })
      callback({ operation_id = "1", target = { { completion = "completed", exit_code = 0 } } }, nil)
    elseif params.operation == "acknowledge" then
      assert(params.operation_id == "1")
      acknowledged = true
      callback({}, nil)
    else error("unexpected native writer operation") end
  end
  local result
  writer.execute("fixture", { kind = "push" }, function(value) result = value end, function(text, stream)
    assert(stream == "stderr")
    chunks[#chunks + 1] = text
  end)
  assert(result.ok and result.stderr == "Pushing\r\ndone")
  assert(table.concat(chunks) == "Pushing\ndone" and acknowledged)
  assert(table.concat(operation_list, ",") == "prepare,submit,acknowledge")

  client.request_host = function(_, params, callback)
    if params.operation == "prepare" then callback({ intent = "uncertain" }, nil)
    elseif params.operation == "submit" then callback({ operation_id = "2", target = { { completion = "outcome_unknown", diagnostic = "connection closed" } } }, nil)
    else error("uncertain result was automatically acknowledged") end
  end
  writer.execute("fixture", { kind = "push" }, function(value)
    assert(not value.ok and value.output:find("connection closed", 1, true))
  end)

  client.request_host = function(_, params, callback)
    if params.operation == "prepare" then callback({ intent = "incomplete" }, nil)
    elseif params.operation == "submit" then callback({ operation_id = "3", target = {} }, nil)
    else error("incomplete result was automatically acknowledged") end
  end
  writer.execute("fixture", { kind = "push" }, function(value)
    assert(not value.ok and value.output:find("no verifiable target receipt", 1, true))
  end)

  local cancelled = false
  client.request_host = function(_, params, callback, progress)
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
  client.request_host = function(_, params, callback)
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
  assert(table.concat(operation_list, ",") == "prepare,submit,acknowledge", "completed write accepted a late cancellation")

  operation_list = {}
  local prepared_callback
  client.request_host = function(_, params, callback)
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
  assert(cancelled_before_prepare == nil, "cancellation settled before prepare returned an intent")
  prepared_callback({ intent = "cancel-before-submit" }, nil)
  assert(not cancelled_before_prepare.ok and cancelled_before_prepare.output:find("cancelled before submission", 1, true))
  assert(table.concat(operation_list, ",") == "prepare,cancel", "cancelled write reached native submission")

  operation_list = {}
  local submitted_callback
  client.request_host = function(_, params, callback)
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
  assert(table.concat(operation_list, ",") == "prepare,submit,cancel", "uncertain cancellation acknowledged a recoverable outcome")
end, debug.traceback)
client.request_host = original
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
