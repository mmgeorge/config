vim.loader.enable(false)
if not vim.g.forge_acp_fixture_grid then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 120, 42, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      local kind, lost = ...
      vim.g.forge_acp_fixture_grid = true
      vim.g.forge_acp_fixture_kind = kind
      vim.g.forge_acp_fixture_lost_ack = lost
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
      dofile('nvim/tests/forge/harness_acp_submission_host.lua')
    ]], { vim.g.forge_acp_fixture_kind or "claude", vim.g.forge_acp_fixture_lost_ack == true })
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("harness_acp_submission_host: passed at 120x42")
  vim.cmd("qa!")
  return
end
local kind = vim.g.forge_acp_fixture_kind or "claude"
local lost = vim.g.forge_acp_fixture_lost_ack == true
local root, workspace, data = vim.fn.getcwd(), vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local skill = workspace .. "/.rulesync/skills/fixture/SKILL.md"
vim.fn.mkdir(vim.fn.fnamemodify(skill, ":h"), "p")
vim.fn.writefile({ "---", "name: fixture", "description: Timeline fixture", "targets: ['*']", "---", "Fixture instructions." }, skill)
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
assert(vim.fn.executable(executable) == 1, "build the development Forge host first")
local peer, sent = data .. "/peer.cjs", data .. "/sent.jsonl"
vim.fn.writefile(vim.split([[
const fs = require('node:fs');
const readline = require('node:readline');
const write = value => process.stdout.write(JSON.stringify({jsonrpc:'2.0', ...value}) + '\n');
readline.createInterface({input:process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = {protocolVersion:1,agentCapabilities:{loadSession:true}};
  if (request.method === 'session/new' || request.method === 'session/load') result = {sessionId:'native'};
  if (request.method === 'session/prompt') {
    fs.appendFileSync(process.argv[2], JSON.stringify(request.params) + '\n');
    if (process.argv[3] === 'lost') {
      process.exit(0);
      return;
    }
    // Isolate projection from provider-specific skill detection with normalized read evidence.
    write({method:'session/update',params:{sessionId:'native',update:{sessionUpdate:'tool_call',
      toolCallId:'skill-read',kind:'read',title:'Read fixture',status:'completed',rawInput:{path:process.argv[4]},
      skill_reads:[{resource_id:'fixture',name:'fixture',path:process.argv[4],tool_id:'skill-read'}]}}});
    write({method:'session/update',params:{sessionId:'native',update:{sessionUpdate:'tool_call',
      toolCallId:'edit',kind:'edit',title:'File changes',status:'interrupted'}}});
    write({method:'session/update',params:{sessionId:'native',update:{sessionUpdate:'agent_message_chunk',
      content:{type:'text',text:'ACP_SUBMISSION_ACCEPTED'}}}});
    result = {stopReason:'end_turn'};
  }
  write({id:request.id,result});
});
]], "\n", { plain = true }), peer)
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(name)
  return (name == "data" or name == "config") and data or original_stdpath(name)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
require("forge.views.harness.controller").resolve_runtime_model = function() end
---@type string[]
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function request(method, params, expected_failure)
  local value, failure, finished
  client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, request_error)
    value, failure, finished = result, request_error, true
  end)
  assert(vim.wait(10000, function() return finished end, 10), method .. " timed out")
  if not expected_failure then assert(not failure, method .. ": " .. tostring(failure)) end
  return value, failure
end
local function settle(count)
  local exchanges
  assert(vim.wait(10000, function()
    exchanges = request("session.exchange.list")
    return #exchanges == count and (exchanges[count].state == "complete" or exchanges[count].state == "failed")
  end, 10), "exchange did not settle: " .. vim.inspect(exchanges) .. " " .. table.concat(errors, " | "))
  return exchanges[count]
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = kind, config_dir = data,
    backends = { [kind] = { command = { "node", peer, sent, lost and "lost" or "ok", skill }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session end, 10), table.concat(errors, " | "))
  request("session.configure", { access = { sandbox = false, write_access = "workspace", writable_directory = {}, windows_sandbox = "elevated" } })
  request("session.permission.set", { change = { kind = "enable_yolo" } })
  request("session.prompt.submit", { text = "FIRST_SUBMISSION_MARKER" }, lost)
  local first = settle(1)
  assert(first.state == (lost and "failed" or "complete"), vim.inspect(first))
  local _, second_error = request("session.prompt.submit", { text = "SECOND_SUBMISSION_MARKER" }, lost)
  local second = settle(2)
  assert(second.state == (lost and "failed" or "complete"), vim.inspect(second))
  local native = vim.fn.readfile(sent)
  assert(#native == (lost and 1 or 2), "unexpected native dispatch count: " .. #native)
  if lost then
    local failure_text = tostring(second_error) .. table.concat(errors, " | ")
    assert(failure_text:find("uncertain delivery", 1, true), failure_text)
    assert(failure_text:find("ACP prompt failed:", 1, true), failure_text)
  else
    assert(not native[2]:find("FIRST_SUBMISSION_MARKER", 1, true), "accepted input was replayed as missing context")
    assert(native[2]:find("SECOND_SUBMISSION_MARKER", 1, true), "follow-up input was lost")
    local transcript = state.presentation.transcript
    local function toggle_activity()
      local row
      assert(vim.wait(5000, function()
        local count = 0
        for index = 0, transcript.sequence:count() - 1 do
          local node = transcript.sequence:at(index)
          local descriptor = node.entry.metadata.node
          if descriptor and descriptor ~= vim.NIL and descriptor.kind == "exchange" then
            count = count + 1
            local _, position = transcript.sequence:position(node.id)
            row = position
          end
        end
        return count == 2
      end, 10), "activity heading did not arrive")
      vim.api.nvim_set_current_win(state.transcript_win)
      vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
      assert(state.presentation.toggle_heading(state.transcript_win), "activity did not accept expansion")
    end
    toggle_activity()
    local function assert_activity()
      local found = {}
      for row, text in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
        for _, label in ipairs({ "Read skill · fixture", "File changes interrupted" }) do
          if text:find(label, 1, true) then
            local location = require("forge.buffer").locate(transcript, row - 1, 0)
            local entry = transcript.sequence.node[location.block].entry
            assert(entry.metadata.layout.indent == 4, label .. " escaped activity indentation")
            found[label] = true
          end
        end
      end
      return found["Read skill · fixture"] and found["File changes interrupted"]
    end
    assert(vim.wait(5000, assert_activity, 10), "activity events did not render:\n"
      .. table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      .. "\n" .. table.concat(errors, "\n"))
    local visible = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    assert(visible:find("Read skill · fixture", 1, true) and visible:find("File changes interrupted", 1, true))
    vim.api.nvim_set_current_win(state.transcript_win)
    for row, text in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if text:find("Read skill · fixture", 1, true) or text:find("File changes interrupted", 1, true) then
        vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
        vim.cmd("normal! zz")
        vim.cmd("redraw!")
        local screen = vim.fn.screenpos(state.transcript_win, row, 1)
        local rendered = ""
        for column = 1, 120 do rendered = rendered .. vim.fn.screenstring(screen.row, column) end
        assert(rendered:find("  ◇ ", 1, true) == 1, "event marker escaped the activity column: " .. rendered)
      end
    end
    toggle_activity()
    assert(vim.wait(5000, function()
      visible = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      return not visible:find("Read skill · fixture", 1, true) and not visible:find("File changes interrupted", 1, true)
    end, 10), "collapsed activity left its events outside the fold")
    assert(visible:find("ACP_SUBMISSION_ACCEPTED", 1, true), "activity collapse hid the final response")
  end
end, debug.traceback)
client.host_stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "host was not collected")
print("harness_acp_submission_host: passed " .. kind .. (lost and " lost acknowledgment" or " acceptance"))
if not vim.g.forge_acp_fixture_grid then vim.cmd("qa!") end
