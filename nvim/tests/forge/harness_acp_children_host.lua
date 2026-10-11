vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.g.forge_acp_test_binary or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the release Forge host first")
local peer = data .. "/acp-peer.cjs"
vim.fn.writefile(vim.split([[
const readline = require('node:readline');
const write = value => process.stdout.write(JSON.stringify({ jsonrpc: '2.0', ...value }) + '\n');
const update = (sessionId, update) => write({ method: 'session/update', params: { sessionId, update } });
readline.createInterface({ input: process.stdin }).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = { protocolVersion: 1,
    agentCapabilities: { loadSession: true, sessionCapabilities: { subagents: {} } },
    agentInfo: { name: 'fixture', version: '1.0.0' }, authMethods: [] };
  if (request.method === 'session/new' || request.method === 'session/load') result = {
    sessionId: 'root', models: { currentModelId: 'haiku', availableModels: [{ modelId: 'haiku', name: 'Fixture' }] } };
  if (request.method === 'session/load') {
    write({ id: request.id, error: { code: -32002, message: 'Empty provider history cannot be loaded' } });
    return;
  }
  if (request.method === 'session/prompt') {
    update('root', { sessionUpdate: 'subagent_spawned', subagentSessionId: 'child', name: 'Reviewer', task: 'Review fixture' });
    update('child', { sessionUpdate: 'agent_message_chunk', content: { type: 'text', text: 'CHILD_CONTENT_VERIFIED' } });
    update('child', { sessionUpdate: 'tool_call', toolCallId: 'shared-tool-id', kind: 'execute', title: 'Child check', status: 'in_progress' });
    update('child', { sessionUpdate: 'tool_call_update', toolCallId: 'shared-tool-id', status: 'completed', rawOutput: { combinedOutput: 'CHILD_COMMAND_OUTPUT', exitCode: 0 } });
    update('child', { sessionUpdate: 'subagent_spawned', subagentSessionId: 'nested', name: 'Nested', task: 'Nested fixture' });
    update('nested', { sessionUpdate: 'agent_message_chunk', content: { type: 'text', text: 'NESTED_CONTENT_VERIFIED' } });
    update('child', { sessionUpdate: 'subagent_state_update', subagentSessionId: 'nested', state: 'completed' });
    update('root', { sessionUpdate: 'subagent_state_update', subagentSessionId: 'child', state: 'completed' });
    update('root', { sessionUpdate: 'subagent_spawned', subagentSessionId: 'unfinished', task: 'Unfinished fixture' });
    update('unfinished', { sessionUpdate: 'subagent_spawned', subagentSessionId: 'unfinished-nested', task: 'Unfinished nested fixture' });
    update('child', { sessionUpdate: 'agent_message_chunk', content: { type: 'text', text: 'LATE_CHILD_CONTENT' } });
    update('unannounced', { sessionUpdate: 'agent_message_chunk', content: { type: 'text', text: 'UNOWNED_CONTENT' } });
    update('root', { sessionUpdate: 'tool_call', toolCallId: 'shared-tool-id', kind: 'execute', title: 'Root check', status: 'in_progress' });
    update('root', { sessionUpdate: 'tool_call_update', toolCallId: 'shared-tool-id', status: 'completed', rawOutput: 'ROOT_COMMAND_OUTPUT' });
    update('root', { sessionUpdate: 'agent_message_chunk', content: { type: 'text', text: 'ROOT_CONTENT_VERIFIED' } });
    result = { stopReason: 'end_turn' };
  }
  write({ id: request.id, result });
});
]], "\n", { plain = true }), peer)
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
require("forge.views.harness.controller").resolve_runtime_model = function() end
---@type string[]
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function request(method, params)
  local result, failure, completed
  client.request(method, params or {}, function(value, request_error)
    result, failure, completed = value, request_error, true
  end)
  assert(vim.wait(10000, function() return completed end, 10), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "claude", config_dir = data,
    backends = { claude = { command = { "node", peer }, label = "Fixture ACP", selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session end, 10))
  request("session.configure", { access = { sandbox = false, write_access = "workspace", writable_directory = {}, windows_sandbox = "elevated" } })
  request("backend.models")
  request("session.execution_mode", { mode = "yolo" })
  request("prompt.submit", { text = "Read the simulated ACP child updates" })
  local snapshot = request("state.get")
  local child, nested, unfinished, unfinished_nested
  for _, agent in pairs(snapshot.agent.run or {}) do
    if agent.provider_thread_id == "child" then child = agent end
    if agent.provider_thread_id == "nested" then nested = agent end
    if agent.provider_thread_id == "unfinished" then unfinished = agent end
    if agent.provider_thread_id == "unfinished-nested" then unfinished_nested = agent end
  end
  assert(child and nested, "ACP child identities were not retained: " .. vim.inspect(snapshot.agent))
  assert(snapshot.agent.summary[child.id].state == "complete" and snapshot.agent.summary[child.id].tool_count == 1)
  assert(snapshot.agent.summary[nested.id].state == "complete" and snapshot.agent.summary[nested.id].tool_count == 0)
  assert(unfinished and unfinished_nested, "unfinished child identities were not retained")
  assert(snapshot.agent.summary[unfinished.id].state == "interrupted")
  assert(snapshot.agent.summary[unfinished_nested.id].state == "interrupted")
  local main = request("exchange.list")
  assert(#main == 1 and main[1].state == "complete")
  assert(main[1].turn[1].tool.item["shared-tool-id"].title == "Root check", "child tool fields leaked across session identity")
  local root_text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(vim.wait(5000, function()
    root_text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return root_text:find("ROOT_CONTENT_VERIFIED", 1, true)
  end, 10), root_text)
  assert(not root_text:find("CHILD_CONTENT_VERIFIED", 1, true) and not root_text:find("NESTED_CONTENT_VERIFIED", 1, true)
    and not root_text:find("LATE_CHILD_CONTENT", 1, true) and not root_text:find("UNOWNED_CONTENT", 1, true), root_text)
  local function select(run, marker)
    assert(vim.wait(5000, function() return state.presentation and state.presentation.ready end, 10))
    state.presentation.select_agent(run.id)
    local text
    assert(vim.wait(5000, function()
      text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      return state.presentation.timeline_key == run.id and text:find(marker, 1, true)
    end, 10), tostring(text))
    assert(not text:find("ROOT_CONTENT_VERIFIED", 1, true) and not text:find("LATE_CHILD_CONTENT", 1, true))
    return text
  end
  local child_text = select(child, "CHILD_CONTENT_VERIFIED")
  assert(not child_text:find("NESTED_CONTENT_VERIFIED", 1, true))
  vim.api.nvim_set_current_win(state.transcript_win)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find("Ran 1 tool", 1, true) then
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      assert(state.presentation.toggle_heading(state.transcript_win))
      break
    end
  end
  assert(vim.wait(5000, function()
    child_text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return child_text:find("Child check", 1, true)
  end, 10), child_text)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    local column = line:find("Child check", 1, true)
    if column then vim.api.nvim_win_set_cursor(state.transcript_win, { row, column - 1 }) break end
  end
  local activated
  state.presentation.activate(function(action, captured)
    assert(action.kind == "tool", vim.inspect(action))
    activated = state.presentation.open_output(action, captured)
  end)
  assert(vim.wait(5000, function() return activated end, 10), "child tool action was unavailable: " .. child_text .. "\n" .. table.concat(errors, "\n"))
  local output = state.presentation.output[#state.presentation.output]
  assert(vim.wait(5000, function() return output.ready end, 10), "child tool output was unavailable")
  local output_text
  assert(vim.wait(5000, function()
    output_text = table.concat(vim.api.nvim_buf_get_lines(output.replica.buffer, 0, -1, false), "\n")
    return output_text:find("CHILD_COMMAND_OUTPUT", 1, true)
  end, 10), tostring(output_text))
  assert(not output_text:find("ROOT_COMMAND_OUTPUT", 1, true), output_text)
  output.close()
  local nested_text = select(nested, "NESTED_CONTENT_VERIFIED")
  assert(not nested_text:find("CHILD_CONTENT_VERIFIED", 1, true))
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
client.stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_acp_children_host: passed (simulated ACP peer, real host, nested ownership, completion, output persistence)")
vim.cmd("qa!")
