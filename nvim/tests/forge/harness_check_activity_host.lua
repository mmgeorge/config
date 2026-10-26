vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
if not vim.g.forge_check_activity_grid and not vim.g.forge_check_activity_hold then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 120, 42, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_check_activity_grid = true
      dofile('nvim/tests/forge/harness_check_activity_host.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("harness_check_activity_host: passed at 120x42")
  vim.cmd("qa!")
  return
end
local original_directory = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local hold = vim.g.forge_check_activity_hold or 0
vim.fn.writefile({ vim.json.encode({ checks = {
  ["activity"] = { trigger = { auto = false }, action = { command = { "nu",  "--no-config-file", "-c",
      ("sleep %dms; print 'retained check output'"):format(hold + 1200)  }, label = "Activity fixture", directory = ".", category = "test", after = {} } },
  ["unused"] = { trigger = { auto = false }, action = { command = { "nu",  "--no-config-file", "-c", "exit 9"  }, label = "Unused fixture", directory = ".", category = "test", after = {} } }
} }) }, workspace .. "/.forge.json")
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

local function assert_status_region()
  local height = vim.api.nvim_win_get_height(state.transcript_win)
    - (vim.wo[state.transcript_win].winbar ~= "" and 1 or 0)
  local used = vim.api.nvim_win_text_height(state.transcript_win, { start_row = 0 }).all
  assert(used == height, ("status occupied %d rows in a %d-row viewport"):format(used, height))
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
  local run_id = request("checks.run", { check = { "activity" } }).run_id
  assert(vim.wait(3000, function()
    answer_permissions()
    return transcript():find("Activity fixture", 1, true) ~= nil
      and transcript():find("Checks running", 1, true) ~= nil
  end, 10), "live check did not reach the status area")
  local snapshot = request("state.get")
  assert(snapshot.checks_running and snapshot.active_check_run == run_id)
  assert(snapshot.status.kind == "idle", "check replaced the primary phase")
  local running = transcript()
  local phase_row, checks_row = running:find("Checking", 1, true), running:find("Checks running", 1, true)
  assert(not running:find("User requested", 1, true) and not running:find("Agent requested", 1, true),
    "check progress must not expose requester labels")
  assert(phase_row and checks_row and phase_row < checks_row, running)
  assert(not running:find("retained check output", 1, true), "running status exposed check output")
  assert_status_region()
  if hold > 0 then vim.wait(hold, function() vim.cmd("redraw") return false end, 20) end
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
  local _, check_row = state.presentation.transcript.sequence:position(run_id .. ":checks")
  assert(check_row ~= nil, "check result heading is missing")
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { check_row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "check results did not expand")
  assert(vim.wait(3000, function()
    return transcript():find("Activity fixture", 1, true) ~= nil
  end, 10), "executed check is missing from expanded results")
  assert(not transcript():find("Unused fixture", 1, true), "skipped check appeared in expanded results")
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, -2, -1, false)[1] == "Idle",
    "idle retained unused rows below the phase")
  assert_status_region()
  assert(#errors == 0, table.concat(errors, "\n"))
  if hold > 0 then vim.wait(hold, function() vim.cmd("redraw") return false end, 20) end

  local session_id = state.session.id
  local exchange = request("exchange.list")[1]
  assert(exchange and exchange.completed_at_ms, "fixture exchange did not settle")
  require("forge.views.harness.workspace").release(state)
  state.presentation.close()
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "first host did not stop")

  -- Seed the provider-owned persisted form of this actual run before the next host opens it.
  local record_path = data .. "/forge/harness/" .. session_id .. "/checks/" .. run_id .. "/record.json"
  local record = vim.json.decode(table.concat(vim.fn.readfile(record_path), "\n"))
  record.run.owner = { kind = "exchange", session_id = session_id, exchange_id = exchange.id, node_count = 0 }
  vim.fn.writefile({ vim.json.encode(record) }, record_path)
  local restored, restore_error
  client.start_harness(function(value, problem) restored, restore_error = value, problem end, { session_id = session_id })
  assert(vim.wait(10000, function() return restored or restore_error end, 10), "history host did not reopen")
  assert(not restore_error, restore_error)
  require("forge.views.harness.session_navigation").activate(restored, {
    interaction_mode = "reconcile", open_mode = "tab",
  })
  state = require("forge.session").harness
  assert(vim.wait(5000, function() return transcript():find("Checks finished", 1, true) ~= nil end, 10),
    "restored exchange hid its check summary inside collapsed activity: " .. transcript()
      .. "\nhistory=" .. vim.inspect(vim.tbl_map(function(entry)
        return {id = entry.id, state = entry.state, owner = entry.owner}
      end, request("checks.list").history))
      .. "\nerrors=" .. vim.inspect(errors))
  local sequence = state.presentation.transcript.sequence
  local function assert_visible_check()
    local summary_id = exchange.id .. ":results:checks"
    local _, row = sequence:position(summary_id)
    assert(row, "check summary disappeared")
    local block = state.presentation.transcript.block[summary_id]
    assert(not block.metadata.node.parent or block.metadata.node.parent == vim.NIL,
      "check summary belongs to the activity fold")
    vim.api.nvim_set_current_win(state.transcript_win)
    vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
    vim.cmd("normal! zz")
    vim.cmd("redraw!")
    local position = vim.fn.screenpos(state.transcript_win, row + 1, 1)
    local rendered = ""
    for column = 1, 120 do rendered = rendered .. vim.fn.screenstring(position.row, column) end
    assert(rendered:find("Checks finished", 1, true), "screen is missing check summary: " .. rendered)
    assert(not transcript():find("retained check output", 1, true), "restored output was expanded")
  end
  assert_visible_check()
  for _, expanded in ipairs({ true, false }) do
    local _, row = sequence:position(exchange.id .. ":summary")
    assert(row, "exchange activity heading is missing")
    vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
    assert(state.presentation.toggle_heading(state.transcript_win))
    assert(vim.wait(3000, function()
      local block = state.presentation.transcript.block[exchange.id .. ":summary"]
      return (block.metadata.node.display == "full") == expanded
    end, 10), "activity toggle did not settle")
    assert_visible_check()
  end
  local _, restored_count = transcript():gsub("Checks finished", "")
  assert(restored_count == 1, "restored check summary was duplicated")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "check activity host was not collected")
print("harness_check_activity_host: passed")
if not vim.g.forge_check_activity_grid then vim.cmd("qa!") end
