if vim.g.forge_acp_fixture_grid then
  local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
  local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
  local kind = vim.g.forge_acp_fixture_kind or "claude"
  local lost = vim.g.forge_acp_fixture_lost_ack == true
  local runtime_version = kind == "antigravity" and "1.4.5" or "0.99.1"
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
let promptCount = 0;
readline.createInterface({input:process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = {protocolVersion:1,agentCapabilities:{loadSession:true},
    agentInfo:{name:'fixture',version:process.argv[5]}};
  if (request.method === 'session/new' || request.method === 'session/load') result = {sessionId:'native'};
  if (request.method === 'session/prompt') {
    const prompt = ++promptCount;
    fs.appendFileSync(process.argv[2], JSON.stringify(request.params) + '\n');
    if (process.argv[3] === 'lost') {
      process.exit(0);
      return;
    }
    // Exercise native ACP read and failed-edit updates before the final response.
    write({method:'session/update',params:{sessionId:'native',update:{sessionUpdate:'tool_call',
      toolCallId:`skill-read-${prompt}`,kind:'read',title:'Read fixture',status:'completed',rawInput:{path:process.argv[4]}}}});
    write({method:'session/update',params:{sessionId:'native',update:{sessionUpdate:'tool_call',
      toolCallId:`edit-${prompt}`,kind:'edit',title:'File changes',status:'interrupted'}}});
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
    local value, failure, detail, finished
    local deadline = vim.uv.hrtime() + 10000000000
    repeat
      value, failure, detail, finished = nil, nil, nil, false
      client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(result, request_error, request_detail)
        value, failure, detail, finished = result, request_error, request_detail, true
      end)
      wait_until(function() return finished end, method .. " timed out", { timeout_ms = 4000 })
      -- Native initialization can still own preparation. Only its explicit pre-dispatch
      -- rejection permits retry; uncertain delivery must reach the assertions below.
      if method ~= "session.prompt.submit" or not detail or detail.code ~= "not_admitted" then break end
      vim.wait(20, function() return false end, 10)
    until vim.uv.hrtime() >= deadline
    if not expected_failure then assert(not failure, method .. ": " .. tostring(failure)) end
    return value, failure
  end
  local function settle(count)
    local exchanges
    wait_until(function()
      exchanges = request("session.exchange.list")
      return #exchanges == count and (exchanges[count].state == "complete" or exchanges[count].state == "failed")
    end, "exchange did not settle", { timeout_ms = 4000,
      context = function() return vim.inspect(exchanges) .. " " .. table.concat(errors, " | ") end })
    return exchanges[count]
  end
  local success, failure = xpcall(function()
    vim.fn.chdir(workspace)
    -- These peers advertise no native effort option, and an explicit effort without one is rejected.
    require("forge").setup({ harness = { provider = kind, effort = "default", config_dir = data,
      providers = { [kind] = { command = { "node", peer, sent, lost and "lost" or "ok", skill, runtime_version }, selectable = true } } } })
    require("forge.views.harness").open()
    wait_until(function() return state.ready and state.session end, "Harness did not start",
      { timeout_ms = 4000, context = function() return table.concat(errors, " | ") end })
    request("session.configure", { access = { sandbox = false, write_access = "workspace", writable_directory = {} } })
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
      assert(not failure_text:find("session/prompt", 1, true), "native protocol detail escaped normalization: " .. failure_text)
    else
      assert(not native[2]:find("FIRST_SUBMISSION_MARKER", 1, true), "accepted input was replayed as missing context")
      assert(native[2]:find("SECOND_SUBMISSION_MARKER", 1, true), "follow-up input was lost")
      local transcript = state.presentation.transcript
      local function set_activity(expanded)
        local row
        wait_until(function()
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
        end, "activity heading did not arrive", { timeout_ms = 4000 })
        vim.api.nvim_set_current_win(state.transcript_win)
        vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
        local location = require("forge.buffer").locate(transcript, row, 0)
        local descriptor = transcript.sequence.node[location.block].entry.metadata.node
        state.presentation.set_section(descriptor.id, expanded, false)
      end
      set_activity(true)
      local expanded = {}
      local function assert_activity()
        for index = 0, transcript.sequence:count() - 1 do
          local node = transcript.sequence:at(index).entry.metadata.node
          if node and node ~= vim.NIL and (node.kind == "thought" or node.kind == "tool_group")
            and node.display ~= "full" and not expanded[node.id] then
            expanded[node.id] = true
            state.presentation.set_section(node.id, true, false)
          end
        end
        local found = {}
        for row, text in ipairs(buffer.lines(state.transcript_buf)) do
          for _, label in ipairs({ "Read fixture", "Reported edits" }) do
            if text:find(label, 1, true) then
              local location = require("forge.buffer").locate(transcript, row - 1, 0)
              local entry = transcript.sequence.node[location.block].entry
              assert(entry.metadata.layout.indent == 6, label .. " escaped activity indentation")
              found[label] = true
            end
          end
        end
        return found["Read fixture"] and found["Reported edits"]
      end
      wait_until(assert_activity, "activity events did not render", { timeout_ms = 4000,
        context = function() return buffer.text(state.transcript_buf) .. "\n" .. table.concat(errors, "\n") end })
      local visible = buffer.text(state.transcript_buf)
      assert(visible:find("Read fixture", 1, true) and visible:find("Reported edits", 1, true))
      assert(visible:find("1 unsuccessful", 1, true), "failed ACP edit lost its outcome")
      vim.api.nvim_set_current_win(state.transcript_win)
      for row, text in ipairs(buffer.lines(state.transcript_buf)) do
        if text:find("Read fixture", 1, true) or text:find("Reported edits", 1, true) then
          vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
          vim.cmd("normal! zz")
          vim.cmd("redraw!")
          local screen = vim.fn.screenpos(state.transcript_win, row, 1)
          local rendered = ""
          for column = 1, 120 do rendered = rendered .. vim.fn.screenstring(screen.row, column) end
          assert(rendered:find(text, 1, true), "activity text is absent from the attached editor grid: " .. rendered)
        end
      end
      set_activity(false)
      wait_until(function()
        visible = buffer.text(state.transcript_buf)
        return not visible:find("Read fixture", 1, true) and not visible:find("Reported edits", 1, true)
      end, "collapsed activity left its events outside the fold", { timeout_ms = 4000 })
      assert(visible:find("ACP_SUBMISSION_ACCEPTED", 1, true), "activity collapse hid the final response")
    end
  end, debug.traceback)
  client.host_stop()
  local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
  vim.fn.chdir(root)
  vim.fn.stdpath = original_stdpath
  pcall(vim.fn.delete, workspace, "rf")
  pcall(vim.fn.delete, data, "rf")
  assert(success and collected, (failure or "host was not collected") .. "\n" .. table.concat(errors, "\n"))
  return
end

local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["submission"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(120, 42, { rgb = true })
  child.lua([[
    local kind, lost = ...
    vim.g.forge_acp_fixture_grid = true
    vim.g.forge_acp_fixture_kind = kind
    vim.g.forge_acp_fixture_lost_ack = lost
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
    dofile('nvim/lua/forge/tests/integration/views/harness/submission.test.lua')
  ]], { vim.g.forge_acp_fixture_kind or "claude", vim.g.forge_acp_fixture_lost_ack == true })
end

return T
