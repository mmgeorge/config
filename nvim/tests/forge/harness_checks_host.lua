vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
local answered = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function()
    for _, approval in ipairs(state.approval or {}) do
      if not answered[approval.id] then
        answered[approval.id] = true
        local answers = {}
        for _, item in ipairs(approval.item_list) do answers[#answers + 1] = {item_id = item.id, choice_id = "allow_once"} end
        client.request("approval.resolve", {approval_id = approval.id, answer_list = answers}, function(_, error)
          assert(not error, tostring(error))
        end)
      end
    end
    return settled
  end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local function save(definitions)
  local checks = {}
  for _, definition in ipairs(definitions) do
    local value = vim.deepcopy(definition)
    value.id = nil
    checks[definition.id] = value
  end
  vim.fn.writefile({ vim.json.encode({ branch_prefix = "fixture", checks = checks }) }, workspace .. "/.forge.json")
end
local function definition(id, command, dependency)
  return { id = id, trigger = {auto = true}, action = { command = command, category = "test", after = dependency or {} } }
end
local function setup(params)
  local admitted = request("checks.setup", params)
  assert(vim.wait(1000, function()
    return state.active_elicitation and state.active_elicitation.elicitation
      and state.status.kind == "awaiting_input"
  end, 10), "setup completed without publishing its question to the UI: " .. vim.inspect({
    question = state.active_elicitation, status = state.status, busy = state.busy,
  }))
  local question = state.active_elicitation.elicitation.question_set.questions[1]
  local controller = require("forge.views.harness.controller")
  require("forge.views.harness.plan_question").close()
  state.plan_question_open = false
  controller.reopen_question()
  assert(state.plan_question_open, "oe did not reopen the pending setup question")
  require("forge.views.harness.plan_question").close()
  state.plan_question_open = false
  request("question.answer", { question_id = question.id, response = { kind = "other", text = "Use targeted checks for this fixture." } })
  request("question.continue")
  return admitted
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "retained-control" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout: " .. table.concat(errors, "\n"))
  assert(request("checks.list").configured == false, "empty directories must remain unconfigured")
  local controller = require("forge.views.harness.controller")
  assert(state.check_editing == false, "check editing must default off")
  local function toggle_editing(enabled)
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/checks setup " .. (enabled and "on" or "off") })
    controller.submit()
    assert(vim.wait(3000, function() return state.check_editing == enabled end, 10), "editing toggle did not settle")
    local winbar = vim.wo[state.transcript_win].winbar
    assert((winbar:find("check editing on", 1, true) ~= nil) == enabled,
      "winbar must show check editing only when enabled")
    assert(not winbar:find("check editing off", 1, true), "winbar must omit disabled check editing")
  end
  toggle_editing(true)
  toggle_editing(false)
  assert(#request("exchange.list") == 0, "permission toggles sent a provider prompt")
  local first_setup = setup()
  assert(state.check_editing == true, "guided setup must enable editing")
  assert(request("checks.list").configured, "setup did not publish after the user answered")
  local repeated_setup = setup({ check_id = "unit" })
  assert(first_setup.exchange.id ~= repeated_setup.exchange.id, "each setup request must own a new exchange")
  request("session.configure", { shell = "nushell" })
  save({ definition("failure", "exit 7"), definition("dependent", "'unexpected' | save unexpected.txt", { "failure" }), definition("independent", "sleep 200ms; print 'independent output'") })
  assert(not pcall(request, "checks.run"), "manual runs must name their checks")
  local id = request("checks.run", { check = {"failure", "dependent", "independent"}, all = true }).run_id
  local run
  assert(vim.wait(10000, function()
    run = request("checks.status", { run_id = id })
    return run.state ~= "running"
  end, 20), "run did not settle")
  assert(run.commands[1].state == "failed" and run.commands[2].state == "skipped" and run.commands[3].state == "passed", vim.inspect(run))
  assert(vim.fn.filereadable(workspace .. "/unexpected.txt") == 0, "failed prerequisites must skip dependents")
  assert(table.concat(vim.fn.readfile(run.commands[3].output), "\n"):find("independent output", 1, true))
  local event_count = #run.lifecycle
  request("state.get")
  local groups = 0
  for _, entry in ipairs(request("checks.list").history) do if entry.id == id then groups = groups + 1 end end
  assert(groups == 1, "one run must own one timeline group")
  assert(vim.wait(3000, function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"):find("Checks", 1, true) ~= nil
  end, 10), "check events did not reach the native transcript")
  local scoped = definition("scoped", "exit 0")
  vim.fn.mkdir(workspace .. "/.forge/checks", "p")
  vim.fn.writefile({ "def main [] {", "  let request = ($in | from json)", "  {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path", "}" }, workspace .. "/.forge/checks/receipt.nu")
  assert(vim.fn.filereadable(workspace .. "/.forge/checks/receipt.nu") == 1)
  local validation = vim.system({ "nu", "--no-config-file", "--stdin", "-c", "nu-check ($in | str trim)" }, { text = true, cwd = workspace, stdin = workspace .. "/.forge/checks/receipt.nu" }):wait(10000)
  assert(validation.code == 0 and validation.stdout:find("true", 1, true), validation.stderr)
  scoped.action.command = "nu --no-config-file --stdin .forge/checks/receipt.nu"
  scoped.action.input = true
  save({ scoped })
  request("session.configure", { shell = "system" })
  local scoped_id = request("checks.run", { check = "scoped", files = { "src/name $value.rs" } }).run_id
  assert(vim.wait(10000, function() return request("checks.status", { run_id = scoped_id }).state ~= "running" end, 20))
  local resolved = request("checks.status", { run_id = scoped_id })
  assert(resolved.state == "passed", table.concat(vim.fn.readfile(resolved.commands[1].output), "\n") .. "\n" .. vim.inspect(resolved))
  assert(resolved.commands[1].context.resolved.files[1] == "src/name $value.rs", "system shell changed scoped selectors")
  request("session.configure", { shell = "nushell" })
  scoped.action.command = "print 'missing receipt'"
  save({ scoped })
  local bad_id = request("checks.run", { check = "scoped", files = { "src/example.rs" } }).run_id
  assert(vim.wait(10000, function() return request("checks.status", { run_id = bad_id }).state ~= "running" end, 20))
  assert(request("checks.status", { run_id = bad_id }).state == "configuration_error")
  assert(vim.wait(3000, function() return #errors > 0 end, 10), "configuration errors must notify the user")
  assert(errors[#errors]:find("receipt", 1, true), table.concat(errors, "\n"))
  errors = {}
  save({ definition("shutdown", "sleep 2sec; 'orphan' | save shutdown-orphan.txt") })
  local shutdown_id = request("checks.run", { check = "shutdown", all = true }).run_id
  assert(request("checks.status", { run_id = shutdown_id }).state == "running")
  local session_id = state.session.id
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "host was not collected")
  local reopened, reopen_failure
  client.start_harness(function(value, problem) reopened, reopen_failure = value, problem end, { session_id = session_id })
  assert(vim.wait(10000, function() return reopened ~= nil or reopen_failure ~= nil end, 10), "reopen timeout")
  assert(not reopen_failure, reopen_failure)
  require("forge.views.harness").open()
  state = require("forge.session").harness
  assert(state.check_editing == true, "editing permission was not restored after host restart")
  local restored = request("checks.status", { run_id = id })
  assert(#restored.lifecycle == event_count, "reopening must preserve event identities without replay")
  assert(restored.commands[3].state == "passed")
  assert(request("checks.status", { run_id = shutdown_id }).state == "interrupted", "shutdown must retain interrupted checks")
  vim.wait(2300, function() return false end, 50)
  assert(vim.fn.filereadable(workspace .. "/shutdown-orphan.txt") == 0, "shutdown left a check process running")
  vim.fn.delete(workspace .. "/.forge.json")
  request("prompt.submit", { text = "/plan add test coverage" })
  local bootstrap_state = request("state.get")
  local bootstrap = bootstrap_state.active_elicitation
  assert(type(bootstrap) == "table", "planning bootstrap did not ask about missing checks")
  assert(not pcall(request, "checks.setup"), "separate setup must not take ownership of pending plan questions")
  local rejected_setup = request("state.get")
  assert(rejected_setup.active_elicitation.elicitation.question_set.id == bootstrap.elicitation.question_set.id,
    "rejected setup changed the pending plan questions")
  assert(vim.deep_equal(rejected_setup.task, bootstrap_state.task),
    "rejected setup changed the waiting plan task: " .. vim.inspect(rejected_setup.task))
  request("question.answer", {question_id = bootstrap.elicitation.question_set.questions[1].id,
    response = {kind = "other", text = "Use targeted checks for the proposed project."}})
  request("question.continue")
  assert(request("checks.list").configured, "planning bootstrap did not save after the user's answer")
  local planned = request("state.get").active_plan
  assert(type(planned) == "table", "plan fixture did not create a review")
  local configured = setup()
  local after_setup = request("state.get").active_plan
  assert(after_setup.id == planned.id and after_setup.state == planned.state, "check setup changed the active plan lifecycle")
  assert(type(configured.exchange.id) == "string", "check setup did not create an exchange during plan review")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "check host was not collected")
print("harness_checks_host: passed")
