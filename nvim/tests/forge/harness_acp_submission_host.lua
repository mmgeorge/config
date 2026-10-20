vim.loader.enable(false)
local kind = vim.g.forge_acp_fixture_kind or "claude"
local lost = vim.g.forge_acp_fixture_lost_ack == true
local root, workspace, data = vim.fn.getcwd(), vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
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
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
require("forge.views.harness.controller").resolve_runtime_model = function() end
---@type string[]
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function request(method, params, expected_failure)
  local value, failure, finished
  client.request(method, params or {}, function(result, request_error)
    value, failure, finished = result, request_error, true
  end)
  assert(vim.wait(10000, function() return finished end, 10), method .. " timed out")
  if not expected_failure then assert(not failure, method .. ": " .. tostring(failure)) end
  return value, failure
end
local function settle(count)
  local exchanges
  assert(vim.wait(10000, function()
    exchanges = request("exchange.list")
    return #exchanges == count and (exchanges[count].state == "complete" or exchanges[count].state == "failed")
  end, 10), "exchange did not settle: " .. vim.inspect(exchanges) .. " " .. table.concat(errors, " | "))
  return exchanges[count]
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = kind, config_dir = data,
    backends = { [kind] = { command = { "node", peer, sent, lost and "lost" or "ok" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session end, 10), table.concat(errors, " | "))
  request("session.configure", { access = { sandbox = false, write_access = "workspace", writable_directory = {}, windows_sandbox = "elevated" } })
  request("session.execution_mode", { mode = "yolo" })
  request("prompt.submit", { text = "FIRST_SUBMISSION_MARKER" }, lost)
  local first = settle(1)
  assert(first.state == (lost and "failed" or "complete"), vim.inspect(first))
  local _, second_error = request("prompt.submit", { text = "SECOND_SUBMISSION_MARKER" }, lost)
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
  end
end, debug.traceback)
client.stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_acp_submission_host: passed " .. kind .. (lost and " lost acknowledgment" or " acceptance"))
vim.cmd("qa!")
