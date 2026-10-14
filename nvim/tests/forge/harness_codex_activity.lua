vim.loader.enable(false)
local repository = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h:h:h")
vim.opt.runtimepath:prepend(repository .. "/nvim")
local data, workspace = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(data, "p")
vim.fn.mkdir(workspace, "p")
assert(vim.system({ "git", "init", "--quiet", workspace }):wait(10000).code == 0)
vim.fn.writefile({ "# Activity fixture", "The counter starts at 7 and adds 5 on each update." }, workspace .. "/README.md")
local original_stdpath = vim.fn.stdpath
local binary = vim.g.forge_codex_activity_binary or require("forge.builder").binary_path()
local private_binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(binary, private_binary))
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = private_binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
---@type string[]
local errors = {}
---@type table<string, boolean>
local resolved_approval = {}
---@type string?
local working_summary
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
---@param method string
---@param parameter table<string, any>?
---@return any
local function request(method, parameter)
  local settled, result, failure
  client.request(method, parameter or {}, function(value, error)
    result, failure, settled = value, error, true
  end)
  assert(vim.wait(method == "prompt.submit" and 90000 or 15000, function()
    for _, approval in ipairs(state.approval or {}) do
      if not resolved_approval[approval.id] then
        resolved_approval[approval.id] = true
        ---@type { item_id: string, choice_id: string }[]
        local answer = {}
        for _, item in ipairs(approval.item_list or {}) do
          answer[#answer + 1] = { item_id = item.id, choice_id = "allow_once" }
        end
        client.request("approval.resolve", { approval_id = approval.id, answer_list = answer }, function(_, error)
          assert(not error, error)
        end)
      end
    end
    if state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) then
      for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
        if line:match("^Working .+ · .+") then working_summary = line end
      end
    end
    if vim.g.forge_codex_activity_visual then vim.cmd.redraw() end
    return settled
  end, 10),
    method .. " timed out: " .. vim.inspect(errors))
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local function transcript()
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { config_dir = data, backend = "codex" } })
  require("forge.views.harness").open()
  assert(vim.wait(15000, function()
    return (state.session and state.ready and state.presentation and state.presentation.ready) or #errors > 0
  end, 10), "startup timeout")
  assert(state.session, table.concat(errors, "\n"))
  request("session.configure", { model = "gpt-5.6-terra", effort = "high", service_tier = "fast" })
  request("trace.configure", { enabled = true })
  request("prompt.submit", { text = "Verify this isolated fixture. First call harness_repository_status with offset 0. "
    .. "Then read README.md with a shell tool. Do not edit files or call other tools. "
    .. "Determine the counter after 23 updates and whether the result is odd. Explain the arithmetic briefly "
    .. "and finish with CODEX_ACTIVITY_VERIFIED." })
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.wait(5000, function()
    return not state.busy and transcript():find("Thought", 1, true)
      and transcript():find("CODEX_ACTIVITY_VERIFIED", 1, true)
  end, 10), "completed turn did not publish its settled transcript\n" .. transcript())
  local trace_path = request("trace.status").path
  local summary, arguments, command = false, false, false
  for _, line in ipairs(vim.fn.readfile(trace_path)) do
    local record = vim.json.decode(line)
    local payload = record.payload or {}
    if record.event == "model.sent" and payload.method == "turn/start" then
      assert(payload.params.summary == "auto", "turn did not enable reasoning summaries")
    end
    if record.event == "model.received" then
      local parameter = payload.params or {}
      if payload.method == "item/reasoning/summaryTextDelta" and parameter.delta and #parameter.delta > 0 then
        summary = true
      end
      local item = parameter.item or {}
      if item.type == "dynamicToolCall" and item.tool == "harness_repository_status" then arguments = true end
      if item.type == "commandExecution" then command = true end
    end
  end
  if summary then
    assert(working_summary, "live status did not render the provider's readable reasoning summary")
  end
  assert(arguments and command, "real Codex did not execute both requested tool types: " .. trace_path)
  local controller = require("forge.views.harness.controller")
  vim.api.nvim_set_current_win(state.transcript_win)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("Thought", 1, true) then
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      controller.toggle_activity()
      break
    end
  end
  assert(vim.wait(3000, function() return transcript():find("Ran 2 tools", 1, true) end, 10),
    "expanded exchange lost its tool group\n" .. transcript())
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("Ran 2 tools", 1, true) then
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      controller.toggle_activity()
      break
    end
  end
  assert(vim.wait(3000, function() return transcript():find("harness_repository_status(", 1, true) end, 10),
    "expanded transcript lost the dynamic call\n" .. transcript())
  assert(transcript():find('"offset":0', 1, true), "dynamic arguments missing from transcript\n" .. transcript())
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("harness_repository_status(", 1, true) then
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      controller.toggle_activity()
      break
    end
  end
  assert(vim.wait(3000, function() return transcript():find("Call: harness_repository_status(", 1, true) end, 10),
    "expanded tool lost its complete call\n" .. transcript())
  assert(transcript():find("CODEX_ACTIVITY_VERIFIED", 1, true), "final answer missing")
  vim.g.forge_codex_activity_result = "pass"
  vim.g.forge_codex_activity_trace = trace_path
  local output = vim.g.forge_codex_activity_output
  if output then
    vim.fn.mkdir(output, "p")
    assert(vim.uv.fs_copyfile(trace_path, output .. "/trace.jsonl"))
    vim.fn.writefile(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), output .. "/transcript.txt")
    vim.fn.writefile({ vim.json.encode({ result = "pass", columns = vim.o.columns, rows = vim.o.lines,
      summary_emitted = summary, working_summary = working_summary,
      trace = output .. "/trace.jsonl" }) }, output .. "/result.json")
  end
  if not vim.g.forge_codex_activity_visual then print("HARNESS_CODEX_ACTIVITY_PASS trace=" .. trace_path) end
end, debug.traceback)
if succeeded and vim.g.forge_codex_activity_visual then return end
local stopped = false
client.stop(nil, function() stopped = true end)
vim.wait(10000, function() return stopped end, 10)
vim.fn.stdpath = original_stdpath
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
vim.cmd("qa!")
