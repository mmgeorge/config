vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.g.forge_acp_test_binary or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the development Forge host first")
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
    agentInfo: { name: 'fixture', version: '0.99.1' }, authMethods: [] };
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
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
require("forge.views.harness.controller").resolve_runtime_model = function() end
---@type string[]
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function request(method, params)
  local result, failure, detail
  local deadline = vim.uv.hrtime() + 10e9
  repeat
    local completed = false
    client.request({ method = method, params = params or {}, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(state.session).id or nil }, function(value, request_error, request_detail)
      result, failure, detail, completed = value, request_error, request_detail, true
    end)
    assert(vim.wait(10000, function() return completed end, 10), method .. " timed out")
    if method ~= "session.prompt.submit" or not detail or detail.code ~= "not_admitted" then break end
    vim.wait(20, function() return false end, 10)
  until vim.uv.hrtime() >= deadline
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { provider = "claude", config_dir = data,
    providers = { claude = { command = { "node", peer }, label = "Fixture ACP", selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session end, 10))
  request("session.configure", { access = { sandbox = false, write_access = "workspace", writable_directory = {}, isolation_mode = "elevated" } })
  request("session.provider.model.list")
  request("session.permission.set", { change = { kind = "enable_yolo" } })
  request("session.prompt.submit", { text = "Read the simulated ACP child updates" })
  assert(vim.wait(10000, function()
    local exchanges = request("session.exchange.list")
    return #exchanges == 1 and exchanges[1].state == "complete"
  end, 10), "simulated ACP execution did not settle after command admission")
  local snapshot = request("session.state.get")
  assert(vim.tbl_isempty(snapshot.agent.run or {}), "native notifications created Harness-owned agents")
  local main = request("session.exchange.list")
  assert(#main == 1 and main[1].state == "complete")
  local function stored_tool(value)
    if type(value) ~= "table" then return nil end
    if value.id == "shared-tool-id" and value.title then return value end
    for _, child in pairs(value) do
      local found = stored_tool(child)
      if found then return found end
    end
  end
  assert(assert(stored_tool(main[1]), "root tool was not retained").title == "Root check", "child tool fields leaked across session identity")
  local root_text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(vim.wait(5000, function()
    root_text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return root_text:find("ROOT_CONTENT_VERIFIED", 1, true)
  end, 10), root_text)
  assert(not root_text:find("CHILD_CONTENT_VERIFIED", 1, true) and not root_text:find("NESTED_CONTENT_VERIFIED", 1, true)
    and not root_text:find("LATE_CHILD_CONTENT", 1, true) and not root_text:find("UNOWNED_CONTENT", 1, true), root_text)
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
client.host_stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n", table.concat(errors, "\n"), "\n")
  vim.cmd("cquit 1")
end
print("harness_acp_children_host: passed (simulated ACP peer, real host, native child isolation, root completion, tool ownership)")
vim.cmd("qa!")
