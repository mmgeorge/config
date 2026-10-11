vim.loader.enable(false)
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local backend = assert(vim.g.forge_acp_test_backend, "set forge_acp_test_backend")
local command = assert(vim.g.forge_acp_test_command, "set forge_acp_test_command")
local workspace = vim.fn.tempname()
local data = vim.fn.tempname()
local mcp_call_log = data .. "/mcp-calls.log"
local import_call_log = data .. "/import-calls.log"
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
if vim.g.forge_acp_test_plan then
  vim.fn.writefile({ '{"message":"before"}' }, workspace .. "/example.json")
end
if vim.g.forge_acp_test_native_import then
  vim.fn.mkdir(data .. "/native-skills/imported-fixture", "p")
  vim.fn.writefile({ "---", "name: imported-fixture", "description: Use for the imported skill verification phrase.",
    "---", "For the imported skill verification phrase, reply exactly FORGE_IMPORTED_SKILL_C724." },
    data .. "/native-skills/imported-fixture/SKILL.md")
  vim.fn.writefile(vim.split([[
const readline = require('node:readline');
readline.createInterface({ input: process.stdin }).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = { protocolVersion: '2024-11-05', capabilities: { tools: {} }, serverInfo: { name: 'imported-fixture', version: '1.0.0' } };
  if (request.method === 'tools/list') result = { tools: [{ name: 'imported_phrase', description: 'Return the imported MCP verification phrase.', inputSchema: { type: 'object', properties: {}, additionalProperties: false } }] };
  if (request.method === 'tools/call') {
    require('node:fs').appendFileSync(process.argv[2], 'imported_phrase\n');
    result = { content: [{ type: 'text', text: process.env.FORGE_IMPORT_FIXTURE === 'literal$value' ? 'FORGE_IMPORTED_MCP_C724' : 'IMPORT_ENVIRONMENT_FAILED' }] };
  }
  if (request.method === 'resources/list') result = { resources: [] };
  if (request.method === 'prompts/list') result = { prompts: [] };
  process.stdout.write(JSON.stringify({ jsonrpc: '2.0', id: request.id, result }) + '\n');
});
]], "\n", { plain = true }), data .. "/native-mcp.cjs")
  vim.fn.writefile({ vim.json.encode({ mcpServers = { ["imported-fixture"] = {
    command = "node", args = { data .. "/native-mcp.cjs", import_call_log }, env = { FORGE_IMPORT_FIXTURE = "literal$value" },
  } }, apiKey = "NATIVE_AUTHENTICATION_FIXTURE" }) }, data .. "/native-mcp.json")
end
if vim.g.forge_acp_test_resources then
  local root = data .. "/rulesync-global/.rulesync"
  vim.fn.mkdir(root .. "/skills/forge-fixture-skill", "p")
  vim.fn.writefile({ "---", "name: forge-fixture-skill", "description: Use when asked for the Forge skill verification phrase.",
    "---", "When asked for the Forge skill verification phrase, reply with exactly FORGE_SKILL_8C46B2." },
    root .. "/skills/forge-fixture-skill/SKILL.md")
  local server = { ["forge-fixture-missing-credential"] = {
    command = "forge-fixture-must-not-launch", envVars = { "FORGE_ACP_MISSING_CREDENTIAL_8C46B2" },
  } }
  if vim.g.forge_acp_test_live_mcp then
    local fixture = data .. "/mcp-fixture.cjs"
    vim.fn.writefile(vim.split([[
const readline = require('node:readline');
readline.createInterface({ input: process.stdin }).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  let result = {};
  if (request.method === 'initialize') result = { protocolVersion: '2024-11-05', capabilities: { tools: {} }, serverInfo: { name: 'forge-fixture', version: '1.0.0' } };
  if (request.method === 'tools/list') result = { tools: [{ name: 'verification_phrase', description: 'Return the Forge MCP verification phrase.', inputSchema: { type: 'object', properties: {}, additionalProperties: false } }] };
  if (request.method === 'tools/call') {
    require('node:fs').appendFileSync(process.argv[2], 'verification_phrase\n');
    result = { content: [{ type: 'text', text: 'FORGE_MCP_53F128' }] };
  }
  if (request.method === 'resources/list') result = { resources: [] };
  if (request.method === 'prompts/list') result = { prompts: [] };
  process.stdout.write(JSON.stringify({ jsonrpc: '2.0', id: request.id, result }) + '\n');
});
]], "\n", { plain = true }), fixture)
    server["forge-fixture-live"] = { command = "node", args = { fixture, mcp_call_log } }
  end
  vim.fn.writefile({ vim.json.encode({ mcpServers = server }) }, root .. "/mcp.json")
end
local original_stdpath = vim.fn.stdpath
local runtime_binary = vim.g.forge_acp_test_binary or require("forge.builder").binary_path()
local test_binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(runtime_binary,test_binary), "copy rebuilt Forge executable: " .. runtime_binary)
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = test_binary }) end)
  return function() end
end }
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local notifications = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notifications[#notifications + 1] = tostring(message) end
end
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")

local function request(method, parameter, target_session)
  if method ~= "task.operation" then print("ACP request: " .. backend .. " " .. method) end
  local settled, value, failure
  local function receive(result, error)
    value, failure, settled = result, error, true
  end
  if target_session then client.request_for(target_session, method, parameter or {}, receive)
  else client.request(method, parameter or {}, receive) end
  local timeout = method == "backend.setup" and 120000
    or ((method == "prompt.submit" or method == "goal.set" or method == "session.compact" or method == "question.continue") and 90000 or 30000)
  local answered = {}
  assert(vim.wait(timeout, function()
    if method == "goal.set" or (method == "prompt.submit" and (vim.g.forge_acp_test_resources or vim.g.forge_acp_test_questions)) then
      for _, approval in ipairs(state.approval or {}) do
        if not answered[approval.id] then
          answered[approval.id] = true
          local answer_list = {}
          for _, item in ipairs(approval.item_list or {}) do
            answer_list[#answer_list + 1] = { item_id = item.id, choice_id = "allow_once" }
          end
          client.request("approval.resolve", { approval_id = approval.id, answer_list = answer_list }, function(_, approval_error)
            assert(not approval_error, approval_error)
          end)
        end
      end
    end
    return settled
  end, 10), method .. " timeout: " .. vim.inspect({
    notification = notifications,
    transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false),
    approval = state.approval,
    session = state.session,
    data_root = data,
  }))
  assert(not failure, method .. ": " .. tostring(failure))
  if method ~= "task.operation" then print("ACP response: " .. backend .. " " .. method) end
  return value
end

local function settle(operation)
  local observed
  assert(vim.wait(30000, function()
    observed = request("task.operation", { operation_id = operation.id })
    return observed.state == "completed" or observed.state == "failed"
  end, 100), "configuration did not settle")
  assert(observed.state == "completed", vim.inspect(observed))
end

local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  local catalog_root = data
  if vim.g.forge_acp_test_global_mcp then catalog_root = nil end
  require("forge").setup({ harness = { config_dir = catalog_root,
    backend = backend, backends = { [backend] = {
    command = command, label = backend, selectable = true,
  } } } })
  if vim.g.forge_acp_test_switch then
    local other = backend == "claude" and "antigravity" or "claude"
    require("forge.infra.config").options.harness.backends[other].command = assert(vim.g.forge_acp_test_other_command)
  end
  require("forge.views.harness").open()
  assert(vim.wait(30000, function() return state.session ~= nil or #notifications > 0 end, 10),
    "Harness startup timeout: " .. vim.inspect({ errors = notifications, session = require("forge.session").harness.session,
      ready = require("forge.session").harness.ready, host_error = require("forge.session").harness.host_error }))
  assert(state.session,table.concat(notifications,"\n"))
  if vim.g.forge_acp_test_setup then
    local installed = request("backend.setup", { install = true })
    assert(installed.available and installed.managed, "managed ACP installation did not become available")
    assert(#installed.auth_methods > 0, "provider omitted sign-in methods")
    local oauth = false
    for _, method in ipairs(installed.auth_methods) do
      if method.id == "claude-ai-login" or method.id == "oauth-personal" then oauth = true end
    end
    assert(oauth, "provider omitted the expected OAuth sign-in method")
    print("ACP managed runtime and OAuth discovery passed: " .. backend .. " " .. installed.supported_version)
  end
  if vim.g.forge_acp_test_trace then
    print("ACP trace: " .. vim.json.encode(request("trace.configure", { enabled = true })))
    print("ACP data root: " .. data)
  end
  request("session.configure", { access = { sandbox = false, write_access = "workspace",
    writable_directory = {}, windows_sandbox = "elevated" } })
  if vim.g.forge_acp_test_auth then
    request("backend.authenticate", { method = vim.g.forge_acp_test_auth })
  end
  local models = request("backend.models")
  local desired = vim.g.forge_acp_test_model or (backend == "claude" and "haiku" or "flash")
  local model
  for _, candidate in ipairs(models) do
    if candidate.id:lower():find(desired, 1, true) then model = candidate.id break end
  end
  assert(model, desired .. " model is not advertised: " .. vim.json.encode(models))
  request("session.configure", { model = model, validate = true })
  local manifest = request("backend.capabilities")
  assert(manifest.backend == backend)
  local response = request("prompt.submit", { text = "Reply with exactly ACP_VERIFIED. Do not call tools." })
  assert(response, "prompt response absent")
  local snapshot = request("state.get")
  assert(snapshot.session.backend_session_id, "provider conversation identity missing")
  local function verification_count()
    local count = 0
    for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line == "ACP_VERIFIED" then count = count + 1 end
    end
    return count
  end
  assert(vim.wait(5000, function() return verification_count() == 1 end, 10), "assistant verification response absent: " ..
    table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"))
  if vim.g.forge_acp_test_native_import then
    for _, resource in ipairs({ { kind = "mcp", source = data .. "/native-mcp.json" },
      { kind = "skills", source = data .. "/native-skills" } }) do
      local preview = request("settings.import.preview", { kind = resource.kind, source = resource.source, scope = "workspace" })
      assert(#preview.candidate == 1 and (preview.candidate[1].blocked == nil or preview.candidate[1].blocked == vim.NIL),
        vim.inspect(preview))
      local definition = preview.candidate[1]
      settle(request("settings.import.apply", { kind = resource.kind, source = preview.source,
        scope = "workspace", fingerprint = preview.fingerprint, expected_revision = preview.revision, selected = { definition.id } }))
      local saved = request("settings.get")
      local field = resource.kind == "skills" and "skill" or "mcp"
      assert(saved.desired[field][definition.id] == false, "native import enabled a resource implicitly")
      local disabled
      for _, entry in ipairs(request("backend." .. resource.kind)) do
        if entry.resource_id == definition.id then disabled = entry break end
      end
      assert(disabled and disabled.enabled == false, "imported resource is absent from its disabled catalog row")
      settle(request("backend." .. resource.kind .. ".set_enabled", { name = definition.name,
        resource_id = definition.id, enabled = true, scope = "workspace" }))
      local enabled
      for _, entry in ipairs(request("backend." .. resource.kind)) do
        if entry.resource_id == definition.id then enabled = entry break end
      end
      assert(enabled and enabled.enabled == true, "imported resource catalog did not refresh its enabled selection")
      assert(request("state.get").session.backend_session_id == snapshot.session.backend_session_id,
        "native resource activation replaced provider history")
    end
    request("session.execution_mode", { mode = "yolo" })
    request("prompt.submit", { text = "Use only the imported-fixture skill from the configured skill catalog to give its skill verification phrase. Follow its SKILL.md instructions exactly. Read only its listed SKILL.md if needed. Do not call imported_phrase or any MCP tools, search directories, or inspect unrelated files. The skill and MCP have different verification phrases." })
    request("prompt.submit", { text = "Call imported_phrase from the imported-fixture MCP server. Reply with exactly its returned text. Do not inspect files, run shell commands, write configuration, or repair missing tools. If unavailable, reply IMPORT_TOOL_UNAVAILABLE." })
    local text
    assert(vim.wait(5000, function()
      text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      return text:find("FORGE_IMPORTED_SKILL_C724", 1, true) and text:find("FORGE_IMPORTED_MCP_C724", 1, true)
    end, 10), "imported skill or MCP did not produce its expected phrase: " .. tostring(text))
    assert(vim.fn.filereadable(import_call_log) == 1 and #vim.fn.readfile(import_call_log) > 0,
      "imported MCP phrase did not come from a tool invocation")
    assert(request("state.get").session.backend_session_id == snapshot.session.backend_session_id,
      "imported resources lost conversation continuity")
    print("ACP native MCP and skill import, disabled defaults, credential reference, activation, and continuity passed: " .. backend)
  end
  if vim.g.forge_acp_test_switch then
    assert(vim.g.forge_acp_test_native_import, "provider switch verification requires imported resources")
    local original = request("state.get").session
    local preferences = request("settings.get").desired
    local other = backend == "claude" and "antigravity" or "claude"
    local function switch(provider, destination)
      assert(require("forge.views.harness").backend_switch_available())
      require("forge.views.harness").switch_backend(provider, destination)
      assert(vim.wait(60000, function()
        local current = require("forge.session").harness
        return current.ready and not current.switching_backend and current.session and current.session.backend == provider
          and require("forge.views.harness").backend_switch_available()
      end, 10), "live provider switch did not settle")
      state = require("forge.session").harness
    end
    switch(other, { kind = "new" })
    local inherited = request("settings.get").desired
    assert(vim.deep_equal(inherited.skill, preferences.skill) and vim.deep_equal(inherited.mcp, preferences.mcp),
      "provider switch lost canonical resource selections")
    for _, resource in ipairs({ { method = "backend.skills", field = "skill" }, { method = "backend.mcp", field = "mcp" } }) do
      local selected = {}
      for _, entry in ipairs(request(resource.method)) do selected[entry.resource_id] = entry.enabled end
      for id, enabled in pairs(preferences[resource.field]) do
        assert(selected[id] == enabled, "destination catalog lost a transferred resource selection: " .. id)
      end
    end
    local other_model
    for _, candidate in ipairs(request("backend.models")) do
      if candidate.id:lower():find(other == "claude" and "haiku" or "flash", 1, true) then other_model = candidate.id break end
    end
    assert(other_model, "destination provider did not advertise its test model")
    request("session.configure", { model = other_model, validate = true })
    request("session.execution_mode", { mode = "yolo" })
    local previous_calls = #vim.fn.readfile(import_call_log)
    request("prompt.submit", { text = "Use the imported-fixture skill to give its verification phrase. Then call imported_phrase from the imported-fixture MCP server. Do not read files or run commands. Reply with both exact phrases." })
    local encoded = vim.json.encode(request("exchange.list"))
    assert(encoded:find("FORGE_IMPORTED_SKILL_C724", 1, true) and encoded:find("FORGE_IMPORTED_MCP_C724", 1, true), encoded)
    assert(#vim.fn.readfile(import_call_log) > previous_calls, "destination provider did not invoke the transferred MCP")
    switch(backend, { kind = "resume", session_id = original.id })
    local restored = request("state.get").session
    assert(restored.id == original.id and restored.model == model and restored.backend_session_id == original.backend_session_id,
      "returning to the source lost provider-local tuning or conversation identity")
    request("prompt.submit", { text = "Reply with exactly PROVIDER_SWITCH_RETURNED. Do not call tools." })
    local returned = request("exchange.list")
    assert(returned[#returned].state == "complete"
      and vim.json.encode(returned[#returned]):find("PROVIDER_SWITCH_RETURNED", 1, true), vim.inspect(returned[#returned]))
    print("ACP live provider switch transferred resources and restored model and history: " .. backend .. " -> " .. other .. " -> " .. backend)
  end
  if vim.g.forge_acp_test_fork then
    assert(backend == "claude" and vim.g.forge_acp_test_native_import, "fork verification requires Claude with imported resources")
    local original = request("state.get").session
    local local_settings = request("settings.get")
    local desired = vim.deepcopy(local_settings.layers.session)
    desired.plan_auto_approve_revisions = false
    settle(request("settings.update", { scope = "session", expected_revision = local_settings.revision, desired = desired }))
    local fork = request("session.fork", { name = "ACP fork verification" }).session
    assert(fork.id ~= original.id)
    local inherited = request("settings.get", {}, fork.id)
    assert(inherited.desired.plan_auto_approve_revisions == false, "fork lost conversation-scoped preferences")
    request("session.execution_mode", { mode = "yolo" }, fork.id)
    local previous_calls = #vim.fn.readfile(import_call_log)
    request("prompt.submit", { text = "Use the imported-fixture skill to give its verification phrase. Then call imported_phrase from the imported-fixture MCP server. Do not read files or run commands. Reply with both exact phrases." }, fork.id)
    local fork_history = request("exchange.list", {}, fork.id)
    local encoded = vim.json.encode(fork_history)
    assert(encoded:find("FORGE_IMPORTED_SKILL_C724", 1, true) and encoded:find("FORGE_IMPORTED_MCP_C724", 1, true), encoded)
    local forked = request("state.get", {}, fork.id).session
    assert(forked.backend_session_id ~= original.backend_session_id, "native fork reused the source conversation")
    assert(request("state.get", {}, original.id).session.backend_session_id == original.backend_session_id, "fork replaced the source conversation")
    assert(#vim.fn.readfile(import_call_log) > previous_calls, "fork did not invoke the inherited MCP binding")
    print("ACP native fork preserved selected resources, private preference scope, and independent history: " .. backend)
  end
  if vim.g.forge_acp_test_raw_output then
    request("session.execution_mode", { mode = "yolo" })
    local command_text = "echo ACP_RAW_OUTPUT_VERIFIED"
    request("prompt.submit", { text = "Run exactly one shell command: " .. command_text ..
      ". Do not create files, repair configuration, or start agents. Reply SHELL_OUTPUT_DONE after it finishes." })
    local exchanges = request("exchange.list")
    local reported = false
    for _, turn in ipairs(exchanges[#exchanges].turn) do
      for _, tool in pairs(turn.tool.item) do
        if tool.kind == "command" and tool.output:find("ACP_RAW_OUTPUT_VERIFIED", 1, true) then
          assert(tool.status == "completed", vim.inspect(tool))
          reported = true
        end
      end
    end
    assert(reported, "completed command output did not enter durable tool history: " .. vim.inspect(exchanges[#exchanges]))
    print("ACP completed shell output persistence passed: " .. backend)
  end
  if vim.g.forge_acp_test_setup then
    local refreshed = request("backend.setup", { install = false })
    assert(refreshed.available and refreshed.managed, "setup refresh lost the managed runtime")
    request("prompt.submit", { text = "Reply with exactly AFTER_SETUP_REFRESH. Do not call tools." })
    local resumed = request("state.get")
    assert(resumed.session.backend_session_id == snapshot.session.backend_session_id,
      "provider setup refresh replaced conversation history")
    assert(vim.wait(5000, function()
      for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
        if line == "AFTER_SETUP_REFRESH" then return true end
      end
    end, 10), "provider setup refresh did not restore prompt execution")
    print("ACP setup refresh and conversation continuity passed: " .. backend)
  end
  if vim.g.forge_acp_test_diffs then
    request("session.execution_mode", { mode = "yolo" })
    request("prompt.submit", { text = "Create acp-diff-test.txt containing ACP_DIFF_VERIFIED using a file-edit tool. Do not use a shell." })
    assert(vim.fn.readfile(workspace .. "/acp-diff-test.txt")[1] == "ACP_DIFF_VERIFIED", "diff fixture file was not created")
    local exchange = request("exchange.list")
    local reported = false
    for _, turn in ipairs(exchange[#exchange].turn) do
      for _, tool in pairs(turn.tool.item) do
        for _, change in ipairs(tool.change.file) do
          if change.path:gsub("\\", "/"):find("acp-diff-test.txt", 1, true) then
            assert(change.kind == "add" and change.diff:find("+ACP_DIFF_VERIFIED", 1, true), vim.inspect(change))
            reported = true
          end
        end
      end
    end
    local file_diff
    for _, capability in ipairs(manifest.capability) do
      if capability.id == "file_diff" then file_diff = capability break end
    end
    assert(file_diff, "file diff capability was not reported")
    if backend == "antigravity" then
      assert(file_diff.support == "limited", "Antigravity hid its structured diff limitation")
      local file_tool = false
      for _, turn in ipairs(exchange[#exchange].turn) do
        for _, tool in pairs(turn.tool.item) do
          file_tool = file_tool or (tool.kind == "file_change" and tool.status == "completed")
        end
      end
      assert(file_tool, "Antigravity file completion was absent from durable tool history")
      print("ACP file completion and declared diff limitation passed: " .. backend)
    else
      assert(file_diff.support == "supported")
      assert(reported, "ACP file diff was absent from durable tool history: " .. vim.inspect(exchange[#exchange]))
      print("ACP structured file diff persistence passed: " .. backend)
    end
  end
  if vim.g.forge_acp_test_resources then
    local settings = request("settings.get")
    assert(#settings.catalog.skill == 1 and #settings.catalog.mcp == (vim.g.forge_acp_test_live_mcp and 2 or 1), "global Rulesync fixture was not discovered")
    request("prompt.submit", { text = "Use the forge-fixture-skill skill to give the Forge skill verification phrase. Follow its instructions exactly." })
    assert(vim.wait(5000, function()
      for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
        if line == "FORGE_SKILL_8C46B2" then return true end
      end
    end, 10), "managed skill content was not loaded from the global Rulesync source")
    local function verify_mcp_ready()
      if backend == "claude" or backend == "antigravity" then return end
      local runtime_catalog
      assert(vim.wait(15000, function()
        runtime_catalog = request("backend.mcp")
        for _, server in ipairs(runtime_catalog) do
          if server.name == "forge-fixture-live" and server.status == "connected" then
            for _, tool in ipairs(server.tools or {}) do if tool.name == "verification_phrase" then return true end end
          end
        end
        return false
      end, 250), "shared MCP tool did not become ready: " .. vim.json.encode(runtime_catalog))
    end
    if vim.g.forge_acp_test_live_mcp then
      verify_mcp_ready()
      request("prompt.submit", { text = "Call the verification_phrase tool from forge-fixture-live and reply with only the phrase it returns." })
      assert(vim.wait(5000, function()
        for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
          if line == "FORGE_MCP_53F128" then return true end
        end
      end, 10), "shared MCP tool was not invoked: " ..
        table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"))
      assert(vim.fn.filereadable(mcp_call_log) == 1 and #vim.fn.readfile(mcp_call_log) > 0,
        "MCP response did not come from a real tool invocation")
    end
    local resources = { { kind = "skills", definition = settings.catalog.skill[1] } }
    for _, definition in ipairs(settings.catalog.mcp) do resources[#resources + 1] = { kind = "mcp", definition = definition } end
    local completed_skill_operation = 0
    local function set_resource(resource, enabled, timeout)
      local changed = request("backend." .. resource.kind .. ".set_enabled", {
        name = resource.definition.name, resource_id = resource.definition.id, enabled = enabled,
      })
      local operation
      assert(vim.wait(timeout or 30000, function()
        local settled
        client.request("task.operation", { operation_id = changed.id }, function(value, error)
          assert(not error, error)
          operation, settled = value, true
        end)
        vim.wait(1000, function() return settled end, 10)
        return operation and (operation.state == "completed" or operation.state == "failed")
      end, 100), "resource selection did not settle")
      assert(operation.state == "completed", vim.inspect(operation))
      if resource.kind == "skills" then completed_skill_operation = completed_skill_operation + 1 end
      return changed.id
    end
    for _, resource in ipairs(resources) do set_resource(resource, false) end
    local disabled = request("settings.get")
    assert(disabled.desired.skill[settings.catalog.skill[1].id] == false)
    for _, definition in ipairs(settings.catalog.mcp) do assert(disabled.desired.mcp[definition.id] == false) end
    assert(disabled.application.state == "applied")
    assert(request("state.get").session.backend_session_id == snapshot.session.backend_session_id,
      "resource restart replaced provider history")
    if vim.g.forge_acp_test_toggle_back then
      for _, resource in ipairs(resources) do
        if resource.kind == "skills" or resource.definition.name == "forge-fixture-live" then set_resource(resource, true) end
      end
      verify_mcp_ready()
      local calls_before = #vim.fn.readfile(mcp_call_log)
      request("prompt.submit", { text = "Call verification_phrase from forge-fixture-live again and reply with only the phrase returned by the tool." })
      assert(vim.wait(5000, function()
        local count = 0
        for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
          if line == "FORGE_MCP_53F128" then count = count + 1 end
        end
        return count >= 2
      end, 10), "re-enabled MCP tool did not execute")
      assert(#vim.fn.readfile(mcp_call_log) > calls_before, "re-enabled MCP response reused prior text without a tool call")
      assert(request("state.get").session.backend_session_id == snapshot.session.backend_session_id,
        "re-enabling resources replaced provider history")
    end
    if vim.g.forge_acp_test_active_settings then
      if not vim.g.forge_acp_test_toggle_back then set_resource(resources[1], true) end
      local before = request("state.get")
      local before_exchange = request("exchange.list")
      local function completed_settings_count()
        local count = 0
        for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
          if line:find("Settings completed: skill", 1, true) then count = count + 1 end
        end
        return count
      end
      assert(vim.wait(5000, function() return completed_settings_count() == completed_skill_operation end, 10),
        "earlier settings timeline rows did not render: expected " .. completed_skill_operation ..
        ", observed " .. completed_settings_count())
      local before_settings = completed_settings_count()
      local settled, failure, detail = false, nil, nil
      local phase = {}
      local unsubscribe = client.subscribe(function(event, payload)
        if event == "task_operation" then
          phase[payload.id] = phase[payload.id] or {}
          phase[payload.id][payload.state] = true
        end
      end)
      client.request("prompt.submit", { text = "Begin your reply with ACTIVE_SETTINGS_STARTED on its own plain line, then write a detailed 3000-word explanation of asynchronous state machines. Do not use tools. If Forge interrupts this response to change settings and asks you to continue, reply with exactly SETTINGS_RESUMED instead of continuing the explanation." },
        function(_, request_error, request_detail) failure, detail, settled = request_error, request_detail, true end)
      assert(vim.wait(30000, function()
        for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
          if line:match("^ACTIVE_SETTINGS_STARTED") then return true end
        end
        return settled
      end, 10), "active-settings prompt did not stream its start marker")
      assert(not settled, "active-settings prompt completed before the configuration change: " .. tostring(failure))
      local operation_id = set_resource(resources[1], false, 90000)
      assert(vim.wait(10000, function() return settled end, 10), "interrupted request did not settle")
      assert(not failure or detail and (detail.code == "turn_cancelled" or detail.code == "turn_retracted"), tostring(failure))
      local after = request("state.get")
      assert(after.session.backend_session_id == before.session.backend_session_id, "active configuration replaced provider history")
      local resumed_exchange = request("exchange.list")
      assert(#resumed_exchange == #before_exchange + 1, "active configuration replaced the interrupted exchange")
      local resumed = resumed_exchange[#resumed_exchange]
      assert(resumed.state == "complete" and #resumed.turn >= 2,
        "active configuration did not complete a continuation in the same exchange: " .. vim.inspect(resumed))
      assert(resumed.turn[1].state.outcome == "interrupted", "settings restart did not preserve the interrupted turn")
      local continued_text = false
      for _, message in ipairs(resumed.turn[#resumed.turn].message) do
        if message.kind == "assistant" and message.text ~= "" then continued_text = true end
      end
      assert(continued_text, "settings restart completed without a continued assistant response")
      for _, state_name in ipairs({ "applying", "verifying", "resuming" }) do
        assert(phase[operation_id] and phase[operation_id][state_name], "configuration phase missing: " .. state_name)
      end
      assert(vim.wait(5000, function() return completed_settings_count() == before_settings + 1 end, 10),
        "configuration timeline operation was lost or duplicated: before " .. before_settings ..
        ", after " .. completed_settings_count())
      unsubscribe()
      print("ACP active settings interruption, timeline, and exchange continuity passed: " .. backend)
    end
    print("ACP global Rulesync skill and MCP selection passed: " .. backend)
  end
  if vim.g.forge_acp_test_matrix then
    local before = request("settings.get")
    local configured = request("settings.update", { expected_revision = before.revision, scope = "global", desired = {
      access = { sandbox = false, write_access = "workspace", writable_directory = {}, windows_sandbox = "elevated" },
      shell = "system", default_write_permission = "write", plan_compact = true,
    } })
    local operation
    assert(vim.wait(30000, function()
      local settled
      client.request("task.operation", { operation_id = configured.id }, function(value, request_error)
        assert(not request_error, request_error)
        operation, settled = value, true
      end)
      vim.wait(1000, function() return settled end, 10)
      return operation and (operation.state == "completed" or operation.state == "failed")
    end, 100), "configuration operation did not settle")
    assert(operation.state == "completed", vim.inspect(operation))
    local applied = request("settings.get")
    assert(applied.application.state == "applied", vim.inspect(applied))
    assert(applied.application.applied_revision == applied.revision)
    assert(applied.application.effective.plan_compact == (backend == "claude"))
    local restarted = request("state.get")
    assert(restarted.session.backend_session_id == snapshot.session.backend_session_id, "restart changed provider conversation")
    request("prompt.submit", { text = "What was your very first assistant reply at the start of this conversation, before any skill requests? Reply with that string only. Do not call tools." })
    assert(vim.wait(5000, function() return verification_count() == 2 end, 10), "provider did not retain the first verification response: " ..
      table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"))
    local continued = request("state.get")
    assert(continued.session.backend_session_id == snapshot.session.backend_session_id, "continuation replaced provider history")
    request("session.execution_mode", { mode = "read" })
    assert(request("state.get").session.execution_mode == "read", "approval fixture must use Read permission")
    print("ACP settings restart and history continuity passed: " .. backend)
    local settled, prompt_error, answered = false, nil, {}
    client.request("prompt.submit", { text = "Create approval-test.txt containing APPROVED_ONCE using a file-edit tool. Do not use a shell. Then reply APPROVAL_VERIFIED." }, function(_, request_error)
      prompt_error, settled = request_error, true
    end)
    assert(vim.wait(90000, function()
      for _, approval in ipairs(state.approval or {}) do
        if not answered[approval.id] then
          answered[approval.id] = true
          local answer_list = {}
          for _, item in ipairs(approval.item_list or {}) do
            answer_list[#answer_list + 1] = { item_id = item.id, choice_id = "allow_once" }
          end
          client.request("approval.resolve", { approval_id = approval.id, answer_list = answer_list }, function(_, approval_error)
            assert(not approval_error, approval_error)
          end)
        end
      end
      return settled
    end, 20), "approved file edit did not finish")
    assert(not prompt_error, prompt_error)
    assert(next(answered), "provider did not request Read-mode approval: " .. vim.inspect({
      file_exists = vim.fn.filereadable(workspace .. "/approval-test.txt") == 1,
      transcript = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false),
    }))
    assert(vim.fn.readfile(workspace .. "/approval-test.txt")[1] == "APPROVED_ONCE", "approved file edit was not applied")
    print("ACP once-only approval workflow passed: " .. backend)
    request("session.execution_mode", { mode = "yolo" })
    request("prompt.submit", { text = "Create yolo-test.txt containing YOLO_VERIFIED using a file-edit tool. Do not use a shell." })
    assert(vim.fn.readfile(workspace .. "/yolo-test.txt")[1] == "YOLO_VERIFIED", "Yolo file edit did not finish")
    assert(not next(state.approval or {}), "ordinary Yolo work unexpectedly requested approval")
    print("ACP Yolo file workflow passed: " .. backend)
  end
  if vim.g.forge_acp_test_lifecycle then
    local settled, failure_detail = false, nil
    client.request("prompt.submit", { text = "Write a detailed 2000-word explanation of asynchronous state machines. Do not use tools." }, function(_, error, detail)
      failure_detail, settled = error and detail, true
    end)
    vim.wait(1000, function() return settled end, 10)
    assert(not settled, "cancellation fixture completed before interruption")
    request("turn.cancel")
    assert(vim.wait(10000, function() return settled end, 10), "cancelled provider turn did not settle")
    assert(failure_detail and failure_detail.code == "turn_cancelled", vim.inspect(failure_detail))
    request("prompt.submit", { text = "Reply with exactly AFTER_CANCEL. Do not call tools." })
    local resumed = request("state.get")
    assert(resumed.session.backend_session_id == snapshot.session.backend_session_id, "cancellation replaced provider history")
    if backend == "claude" then
      request("session.compact")
      local compacted = request("state.get")
      assert(vim.wait(5000, function()
        for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
          if line:find("Context compacted", 1, true) then return true end
        end
      end, 10), "completed compaction timeline row missing: " ..
        table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"))
      assert(compacted.session.backend_session_id == snapshot.session.backend_session_id, "compaction replaced provider history")
    end
    print("ACP cancellation and compaction lifecycle passed: " .. backend)
  end
  if vim.g.forge_acp_test_control then
    request("goal.set", { objective = "Verify the Harness control connection by calling harness_goal_complete with a short summary. No files or shell commands are needed. Complete this goal only through that tool." })
    local completed = request("state.get")
    assert(completed.goal and completed.goal.state == "complete", "control tool did not complete the Goal task: " .. vim.inspect(completed.goal))
    print("ACP Harness control workflow passed: " .. backend)
  end
  if vim.g.forge_acp_test_questions then
    request("prompt.submit", { text = "Call harness_question_ask with one question, id fixture_choice, header Choice, question 'Choose the verification option', and options Alpha / Verify Alpha, Beta / Verify Beta. Set allow_freeform false. Do not call any other tools or start agents. End the turn after the tool. When the user answers later, reply with exactly QUESTION_ANSWER_ALPHA and do not ask another question." })
    local pending = request("state.get").active_elicitation
    assert(pending and pending.owner == "interaction", vim.inspect(pending))
    local history = request("exchange.list")
    local exchange_id = history[#history].id
    assert(history[#history].completed_at_ms == nil, "question completed its exchange before an answer")
    local question = pending.elicitation.question_set.questions[1]
    request("question.answer", { question_id = question.id, response = { kind = "selected", option = "Alpha" } })
    request("question.continue")
    require("forge.views.picker").close(false)
    local completed = request("exchange.list")
    assert(completed[#completed].id == exchange_id and completed[#completed].state == "complete", vim.inspect(completed[#completed]))
    assert(vim.json.encode(completed[#completed]):find("QUESTION_ANSWER_ALPHA", 1, true), "question continuation lost the supplied answer")
    print("ACP structured question and answer completed the same exchange: " .. backend)
  end
  if vim.g.forge_acp_test_queue then
    controller.activate_snapshot(request("state.get"))
    assert(vim.wait(5000, function() return not state.busy and not state.configuring and not state.task_operation end, 10))
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Reply with exactly QUEUE_FIRST_VERIFIED. Do not call tools." })
    controller.queue_submit()
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Reply with exactly QUEUE_SECOND_VERIFIED. Do not call tools." })
    controller.queue_submit()
    assert(vim.wait(90000, function()
      local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      return not state.busy and #state.queue == 0 and text:find("QUEUE_FIRST_VERIFIED", 1, true)
        and text:find("QUEUE_SECOND_VERIFIED", 1, true)
    end, 10), "provider prompt queue did not drain: " .. vim.inspect({ queue = state.queue, busy = state.busy,
      task_operation = state.task_operation, execution_notice = state.execution_notice, active_elicitation = state.active_elicitation,
      state_sync_pending = state.state_sync_pending, configuring = state.configuring, connection_error = state.connection_error,
      status = state.status, queue_suspended = state.queue_suspended, host_error = state.host_error,
      sync_error = state.sync_error, pending_steer = state.pending_steer, switching_backend = state.switching_backend,
      configuration_debounce = state.configuration_debounce, aborting_plan = state.aborting_plan,
      transcript = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false) }))
    local history = request("exchange.list")
    assert(vim.json.encode(history[#history - 1]):find("QUEUE_FIRST_VERIFIED", 1, true))
    assert(vim.json.encode(history[#history]):find("QUEUE_SECOND_VERIFIED", 1, true))
    assert(history[#history - 1].state == "complete" and history[#history].state == "complete")
    print("ACP queued prompts completed in separate ordered exchanges: " .. backend)
  end
  if vim.g.forge_acp_test_plan then
    request("prompt.submit", { text = "/plan Change example.json so message becomes ACP_PLAN_VERIFIED. Keep the complete JSON proposal. Only create a plan and submit it through harness_plan_submit. Do not implement the change, run builds, tests, shell commands, or start agents. No questions are needed. Keep verification and checks empty for this fixture." })
    local planned = request("state.get")
    assert(planned.active_plan and planned.active_plan.state == "awaiting_review", vim.inspect(planned.active_plan))
    assert(vim.fn.readfile(workspace .. "/example.json")[1] == '{"message":"before"}', "planning changed the workspace")
    assert(planned.active_plan.working_path and vim.fn.filereadable(planned.active_plan.working_path) == 1,
      "ACP planning did not create a physical review artifact")
    print("ACP planning stopped at a physical review artifact without workspace changes: " .. backend)
  end
  print("ACP real workflow passed: " .. backend .. " " .. model .. " " .. snapshot.session.backend_session_id)
end, debug.traceback)

if succeeded and vim.g.forge_acp_test_setup_inspect then
  require("forge.views.harness.provider_setup").open({
    host = { window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win },
    session_id = state.session.id,
  })
  return
end
if succeeded and vim.g.forge_acp_test_inspect then
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/capabilities" })
  controller.submit()
  return
end

local stopped = false
client.stop(nil, function() stopped = true end)
vim.wait(10000, function() return stopped end, 10)
vim.fn.stdpath = original_stdpath
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
vim.cmd("qa!")
