-- Integration tests for the `status` area. Each scenario row runs in its own headless Neovim:
-- `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`. Tests that launch the Forge host
-- assume `rust-build` has produced a current executable.
local scenarios = {
  { name = "commit", run = function()
vim.loader.enable(false)

dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
local commit = require("forge.integrations.commit")
local ai_commit = require("forge.integrations.ai_commit")
local git_write = require("forge.git.write")
local perf = require("forge.infra.perf")
local original_event = perf.event
local event_list = {}
perf.event = function(scope, event, payload)
  if event:match("^commit%.") then event_list[#event_list + 1] = { event = event, payload = vim.deepcopy(payload) } end
end

local repository = vim.fn.tempname()
assert(vim.fn.mkdir(repository, "p") == 1)

local original_execute = git_write.execute
local original_provider = ai_commit._provider
local background_settings = require("forge.integrations.background_settings")
local original_settings_request = background_settings.request
background_settings.request = function(_, _, callback)
  callback({ settings = { models = {
    fixture_alpha = { model = "fixture-model", thinking = "low" },
    fixture_beta = { model = "fixture-model", thinking = "low" },
  }, auto_compaction = { enabled = true, threshold_tokens = 128000 } } })
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments), { text = true, timeout = 30000 }):wait()
  assert_true(result.code == 0, result.stderr)
  return result.stdout
end

local function run()
  local request_list, receipt_list = {}, {}
  ai_commit.reset_provider()
  ai_commit.set_provider({ request_async = function(params, callback)
    request_list[#request_list + 1] = { params = params, callback = callback }
  end })

  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "before" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  git({ "commit", "--quiet", "-m", "initial" })
  vim.fn.writefile({ "after" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  vim.fn.writefile({ "#!/bin/sh", "echo forge-pre-commit-stdout", "echo forge-pre-commit-stderr >&2" }, repository .. "/.git/hooks/pre-commit")
  assert_true(vim.uv.fs_chmod(repository .. "/.git/hooks/pre-commit", 493), "pre-commit hook could not become executable")

  ai_commit.ensure(repository, { ref = "HEAD" })
  assert_true(request_list[1].params.operation == "generate" and request_list[1].params.comparison == "status",
    "cold Status draft did not generate directly")

  git_write.execute = function(workspace, action, callback, progress)
    assert_true(workspace == repository, "commit host used the wrong repository")
    assert_true(action.kind == "commit_editor", "commit host bypassed fake-editor admission")
    assert_true(type(action.command) == "string" and action.command ~= "", "commit host omitted GIT_EDITOR")
    assert_true(type(action.nvim_server) == "string" and action.nvim_server ~= "", "commit host omitted parent RPC address")
    local command = { "git", "-C", workspace, "commit" }
    if action.amend then vim.list_extend(command, { "--amend", "--only" }) end
    vim.system(command, {
      text = true,
      timeout = 30000,
      stderr = function(error, data)
        if error then receipt_list[#receipt_list + 1] = { stream = "stderr", text = tostring(error) } end
        if data then receipt_list[#receipt_list + 1] = { stream = "stderr", text = data } end
      end,
      env = { GIT_EDITOR = action.command, NVIM = action.nvim_server },
    }, function(result)
      vim.schedule(function()
        if result.stdout ~= "" then receipt_list[#receipt_list + 1] = { stream = "stdout", text = result.stdout } progress(result.stdout, "stdout") end
        if result.stderr and result.stderr ~= "" then receipt_list[#receipt_list + 1] = { stream = "stderr", text = result.stderr } progress(result.stderr, "stderr") end
        callback({ ok = result.code == 0, code = result.code, output = (result.stdout or "") .. (result.stderr or "") })
      end)
    end)
    return function() end
  end

  local window = vim.api.nvim_get_current_win()
  commit.commit({ win = window, workspace = repository })
  assert_true(vim.wait(15000, function()
    return commit._active and vim.b[vim.api.nvim_win_get_buf(window)].forge_commit_buffer
  end, 10), "real GIT_EDITOR did not open")
  assert_true(#request_list == 1, "commit editor requested validation or duplicated generation")

  local editor = vim.api.nvim_win_get_buf(window)
  assert_true(vim.b[editor].forge_commit_buffer, "fake editor callback did not open the real COMMIT_EDITMSG buffer")
  vim.api.nvim_buf_set_lines(editor, 0, -1, false, { "user authored commit" })
  request_list[1].callback({ state = "ready", fingerprint = "same-content", message = "generated commit" })
  assert_true(vim.api.nvim_buf_get_lines(editor, 0, -1, false)[1] == "user authored commit",
    "delayed staged generation overwrote commit editor text")
  local submit = vim.fn.maparg("<C-c><C-c>", "n", false, true)
  assert_true(type(submit.callback) == "function", "commit editor did not install submit mapping")
  submit.callback()

  assert_true(vim.wait(30000, function() return commit._active == nil end, 10),
    "real git commit did not settle: " .. vim.inspect({ receipts = receipt_list, console = commit._active
      and vim.api.nvim_buf_get_lines(commit._active.console, 0, -1, false) or {} }))
  assert_true(git({ "log", "-1", "--format=%B" }):find("user authored commit", 1, true) ~= nil,
    "real git commit did not receive fake-editor text")
  local receipt = table.concat(vim.tbl_map(function(item) return item.text end, receipt_list), "\n")
  assert_true(receipt:find("forge%-pre%-commit%-stdout") and receipt:find("forge%-pre%-commit%-stderr"),
    "pre-commit stdout/stderr did not reach the commit receipt path: " .. receipt)

  local original_tree = vim.trim(git({ "rev-parse", "HEAD^{tree}" }))
  vim.fn.writefile({ "staged but not amended" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  commit.commit({ win = window, workspace = repository, amend = true })
  assert_true(vim.wait(15000, function()
    return commit._active and vim.b[vim.api.nvim_win_get_buf(window)].forge_commit_buffer
  end, 10), "amend GIT_EDITOR did not open")
  editor = vim.api.nvim_win_get_buf(window)
  assert_true(vim.api.nvim_buf_get_lines(editor, 0, 1, false)[1] == "user authored commit",
    "amend did not preload the latest commit message")
  local regenerate = vim.fn.maparg("<C-a>", "n", false, true)
  assert_true(type(regenerate.callback) == "function", "amend lost AI regeneration mapping")
  regenerate.callback()
  assert_true(request_list[2].params.comparison == "latest_commit",
    "amend regeneration did not select the latest commit")
  request_list[2].callback({ state = "ready", message = "AI amended message" })
  assert_true(vim.api.nvim_buf_get_lines(editor, 0, 1, false)[1] == "AI amended message",
    "amend regeneration did not replace the message")
  vim.api.nvim_buf_set_lines(editor, 0, 1, false, { "updated commit message" })
  regenerate.callback()
  assert_true(vim.api.nvim_buf_get_lines(editor, 0, 1, false)[1]:find("Generating with ", 1, true) ~= nil,
    "pending amend omitted generation progress")
  vim.fn.maparg("<C-c><C-c>", "n", false, true).callback()
  assert_true(vim.wait(30000, function() return commit._active == nil end, 10),
    "real git amend did not settle")
  assert_true(vim.trim(git({ "log", "-1", "--format=%s" })) == "updated commit message",
    "amend did not replace the latest message")
  assert_true(vim.trim(git({ "rev-parse", "HEAD^{tree}" })) == original_tree,
    "amend changed the committed tree")
  assert_true(git({ "diff", "--cached", "--name-only" }):find("tracked.txt", 1, true) ~= nil,
    "amend consumed staged changes")

  local expected = { "commit.started", "commit.root.resolved", "commit.write.dispatch", "commit.editor.requested",
    "commit.editor.opened", "commit.editor.submit", "commit.editor.write", "commit.editor.signal", "commit.finished",
    "commit.cleanup", "commit.preview.restored" }
  assert_true(#event_list == #expected * 2, "commit timing event count differs: " .. vim.inspect(event_list))
  for index, event in ipairs(event_list) do
    local position = (index - 1) % #expected + 1
    assert_true(event.event == expected[position], "commit timing order differs: " .. event.event)
    local start = event_list[index - position + 1].payload
    assert_true(event.payload.request_id == start.request_id, "commit timing lost correlation")
    assert_true(event.payload.elapsed_ms >= 0, "commit timing missing elapsed time")
    if event.event == "commit.finished" then
      assert_true(type(event.payload.ms) == "number" and event.payload.ms <= event.payload.elapsed_ms,
        "post-submit timing includes pre-submit time")
    end
  end
  assert_true(event_list[1].payload.request_id ~= event_list[#expected + 1].payload.request_id,
    "amend reused the prior commit trace")
end

local ok, failure = xpcall(run, debug.traceback)
perf.event = original_event
git_write.execute = original_execute
ai_commit.reset_provider()
ai_commit.set_provider(original_provider)
background_settings.request = original_settings_request
if commit._active then commit._active.aborted = true end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_host OK")
vim.cmd("qa!")
  end },
  { name = "commit_large_output", run = function()
vim.loader.enable(false)

dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
local commit = require("forge.integrations.commit")
local ai_commit = require("forge.integrations.ai_commit")
local git_write = require("forge.git.write")

local repository = vim.fn.tempname()
assert(vim.fn.mkdir(repository, "p") == 1)

local original_execute = git_write.execute
local original_provider = ai_commit._provider
local background_settings = require("forge.integrations.background_settings")
local original_settings_request = background_settings.request
background_settings.request = function(_, _, callback)
  callback({ settings = { models = {
    fixture_alpha = { model = "fixture-model", thinking = "low" },
    fixture_beta = { model = "fixture-model", thinking = "low" },
  }, auto_compaction = { enabled = true, threshold_tokens = 128000 } } })
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments), { text = true, timeout = 30000 }):wait()
  assert_true(result.code == 0, result.stderr)
  return result.stdout
end

local function run()
  local request_list, receipt_list = {}, {}
  ai_commit.reset_provider()
  ai_commit.set_provider({ request_async = function(params, callback)
    request_list[#request_list + 1] = { params = params, callback = callback }
  end })

  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "before" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  git({ "commit", "--quiet", "-m", "initial" })
  vim.fn.writefile({ "after" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  vim.fn.writefile({ string.rep("hook output ", 9000) }, repository .. "/.git/forge-hook-output")
  vim.fn.writefile({ "#!/bin/sh", "cat .git/forge-hook-output >&2" }, repository .. "/.git/hooks/pre-commit")
  assert_true(vim.uv.fs_chmod(repository .. "/.git/hooks/pre-commit", 493), "pre-commit hook could not become executable")

  ai_commit.ensure(repository, { ref = "HEAD" })
  assert_true(request_list[1].params.operation == "generate" and request_list[1].params.comparison == "status",
    "cold Status draft did not generate directly")

  local outcome
  git_write.execute = function(workspace, action, callback, progress)
    return original_execute(workspace, action, function(result)
      outcome = result
      callback(result)
    end, function(text, stream)
      receipt_list[#receipt_list + 1] = { stream = stream, text = text }
      progress(text, stream)
    end)
  end

  local window = vim.api.nvim_get_current_win()
  commit.commit({ win = window, workspace = repository })
  assert_true(vim.wait(15000, function()
    return commit._active and vim.b[vim.api.nvim_win_get_buf(window)].forge_commit_buffer
  end, 10), "real GIT_EDITOR did not open: " .. vim.inspect(outcome) .. "\n"
    .. table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(window), 0, -1, false), "\n"):sub(-2000))
  assert_true(#request_list == 1, "commit editor requested validation or duplicated generation")

  local editor = vim.api.nvim_win_get_buf(window)
  assert_true(vim.b[editor].forge_commit_buffer, "fake editor callback did not open the real COMMIT_EDITMSG buffer")
  local message = "test: " .. string.rep("large summary ", 9000)
  vim.api.nvim_buf_set_lines(editor, 0, -1, false, { message })
  request_list[1].callback({ state = "ready", fingerprint = "same-content", message = "generated commit" })
  assert_true(vim.api.nvim_buf_get_lines(editor, 0, -1, false)[1] == message,
    "delayed staged generation overwrote commit editor text")
  local submit = vim.fn.maparg("<C-c><C-c>", "n", false, true)
  assert_true(type(submit.callback) == "function", "commit editor did not install submit mapping")
  submit.callback()

  assert_true(vim.wait(30000, function() return commit._active == nil end, 10),
    "real git commit did not settle")
  assert_true(outcome and outcome.ok, "native commit reported failure")
  assert_true(vim.trim(git({ "log", "-1", "--format=%s" })) == vim.trim(message), "Git did not commit editor text")
  assert_true(vim.trim(git({ "rev-list", "--count", "HEAD" })) == "2", "commit was lost or duplicated")
  for _, stream in ipairs({ "stdout", "stderr" }) do
    local chunks = {}
    for _, receipt in ipairs(receipt_list) do
      if receipt.stream == stream then chunks[#chunks + 1] = receipt.text end
    end
    local output = table.concat(chunks)
    assert_true(#output <= 65536, stream .. " exceeded Lua progress budget")
    assert_true(output:find("Command output truncated", 1, true), stream .. " omitted truncation notice")
  end
  vim.fn.writefile({ "next change" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  vim.fn.writefile({ "#!/bin/sh", "cat .git/forge-hook-output >&2", "echo fixture-hook-rejected >&2", "exit 7" },
    repository .. "/.git/hooks/pre-commit")
  outcome = nil
  commit.commit({ win = window, workspace = repository })
  assert_true(vim.wait(15000, function() return outcome ~= nil end, 10), "failing hook did not settle")
  assert_true(not outcome.ok and outcome.code ~= 0, "noisy failing hook reported success")
  assert_true(outcome.output:find("fixture-hook-rejected", 1, true), "failure lost final hook diagnostic: " .. outcome.output:sub(-1000))
  assert_true(vim.trim(git({ "rev-list", "--count", "HEAD" })) == "2", "failing hook created a commit")
  local console = table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(window), 0, -1, false), "\n")
  assert_true(console:find("fixture-hook-rejected", 1, true), "commit console omitted hook rejection")
end

local ok, failure = xpcall(run, debug.traceback)
git_write.execute = original_execute
ai_commit.reset_provider()
ai_commit.set_provider(original_provider)
background_settings.request = original_settings_request
if commit._active then commit._active.aborted = true end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_large_output_host OK")
vim.cmd("qa!")
  end },
  { name = "commit_parent_exit", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")

local repository = vim.fn.tempname()
vim.fn.mkdir(repository, "p")
local config_root = vim.fn.getcwd() .. "/nvim"
local parent_process, git_process, parent_channel

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments),
    { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end

local function run()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Test" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "committed" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  git({ "commit", "--quiet", "-m", "original" })
  local original_head = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "staged" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  local original_index = git({ "write-tree" })

  for ordinal, shutdown in ipairs({ "quit", "terminate", "abort" }) do
    local server = vim.fn.has("win32") == 1
      and ("\\\\.\\pipe\\forge-commit-parent-%d-%d"):format(vim.fn.getpid(), ordinal)
      or vim.fn.tempname()
    local setup = ([[
      require("forge.git.write").execute = function(_, action)
        vim.g.commit_parent_test_action = action
        return function() end
      end
      require("forge.integrations.commit").commit({
        win = vim.api.nvim_get_current_win(), workspace = %q, amend = true,
      })
    ]]):format(repository)
    parent_process = vim.system({ vim.v.progpath, "--headless", "--clean", "--noplugin",
      "--listen", server, "-c", "set runtimepath^=" .. vim.fn.fnameescape(config_root),
      "-c", "lua " .. setup }, { text = true, stdout = true, stderr = true })
    parent_channel = nil
    assert(vim.wait(5000, function()
      local connected, channel = pcall(vim.fn.sockconnect, "pipe", server, { rpc = true })
      if connected and channel ~= 0 then parent_channel = channel return true end
      return false
    end, 50), "parent editor server did not start")
    local action
    assert(vim.wait(5000, function()
      action = vim.rpcrequest(parent_channel, "nvim_exec_lua", "return vim.g.commit_parent_test_action", {})
      return type(action) == "table"
    end, 50), "parent did not prepare commit editor")

    local result
    git_process = vim.system({ "git", "-C", repository, "commit", "--amend", "--only" }, {
      text = true, stdout = true, stderr = true, timeout = 15000,
      env = { GIT_EDITOR = action.command, NVIM = action.nvim_server },
    }, function(outcome) result = outcome end)
    assert(vim.wait(5000, function()
      return vim.rpcrequest(parent_channel, "nvim_exec_lua",
        "return vim.b.forge_commit_buffer == true", {})
    end, 50), "commit editor did not open")
    assert(vim.uv.fs_stat(repository .. "/.git/index.lock"), "amend did not hold index lock")

    local started = vim.uv.hrtime()
    if shutdown == "terminate" then
      parent_process:kill(9)
    elseif shutdown == "quit" then
      vim.rpcnotify(parent_channel, "nvim_command", "qa!")
    else
      vim.rpcrequest(parent_channel, "nvim_exec_lua",
        "vim.fn.maparg('<C-q>', 'n', false, true).callback()", {})
    end
    assert(vim.wait(5000, function() return result ~= nil end, 20), shutdown .. " left Git waiting")
    assert(result.code ~= 0 and result.code ~= 124, shutdown .. " did not abort Git promptly")
    assert(not vim.uv.fs_stat(repository .. "/.git/index.lock"), shutdown .. " retained index.lock")
    assert(git({ "rev-parse", "HEAD" }) == original_head, shutdown .. " changed HEAD")
    assert(git({ "write-tree" }) == original_index, shutdown .. " changed staged content")
    print(("commit parent %s released Git and lock in %.0f ms"):format(shutdown,
      (vim.uv.hrtime() - started) / 1e6))
    if shutdown == "abort" then vim.rpcnotify(parent_channel, "nvim_command", "qa!") end
    parent_process:wait(2000)
    pcall(vim.fn.chanclose, parent_channel)
    parent_process, git_process, parent_channel = nil, nil, nil
  end
end

local ok, failure = xpcall(run, debug.traceback)
if parent_process then parent_process:kill(9) parent_process:wait(2000) end
if git_process then git_process:kill(9) git_process:wait(2000) end
if parent_channel then pcall(vim.fn.chanclose, parent_channel) end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_parent_exit OK")
vim.cmd("qa!")
  end },
  { name = "commit_scope", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local generator = require("forge.integrations.ai_commit")
local builder = require("forge.builder")
local fixture, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local binary = data .. "/forge-scope-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile("nvim/lua/forge/tests/support/host_executable.lua")()
builder.ensure = function(callback) callback({ ok = true, path = binary }) return function() end end
client._host_set_launcher_for_test(vim.system)
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local requests = 0
local generation_request = client.generation_request
client.generation_request = function(...)
  requests = requests + 1
  return generation_request(...)
end
local function await(predicate, message)
  assert(vim.wait(10000, predicate, 10), message)
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true }):wait()
  assert(result.code == 0, result.stderr)
end
local function generate()
  generator.ensure(fixture, { force = true })
  await(function() return generator.state(fixture, "HEAD").state ~= "generating" end, "About did not settle")
  local state = generator.state(fixture, "HEAD")
  assert(state.state == "ready", vim.inspect(state))
  return state.message
end
local success, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.invalid" })
  vim.fn.writefile({ "scope-before" }, fixture .. "/tracked.txt")
  vim.fn.writefile({ "ignored.txt" }, fixture .. "/.gitignore")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "initial" })
  vim.fn.writefile({ "scope-staged" }, fixture .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  vim.fn.writefile({ "scope-unstaged" }, fixture .. "/tracked.txt")
  vim.fn.writefile({ "scope-untracked" }, fixture .. "/untracked.txt")
  vim.fn.writefile({ "scope-ignored" }, fixture .. "/ignored.txt")
  require("forge.infra.config").setup({ harness = { provider = "mock" },
    background_generation = { mock = { model = "mock-model", reasoning_effort = "low",
      auto_compaction = { enabled = false, threshold_tokens = 128000 } } } })
  local about = generate()
  assert(about:find("+ scope-staged", 1, true), about)
  assert(not about:find("scope-unstaged", 1, true) and not about:find("scope-untracked", 1, true)
    and not about:find("scope-ignored", 1, true), about)
  local buffer = vim.api.nvim_create_buf(false, true)
  generator.populate_commit_buffer_when_ready(buffer, fixture)
  assert(requests == 1, "commit editor regenerated instead of reusing About")
  assert(vim.trim(table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n")) == vim.trim(about))
  generator.populate_commit_buffer_when_ready(buffer, fixture, true)
  await(function() return generator.state(fixture, "staged").state ~= "generating" end, "Ctrl-A did not settle")
  local staged = generator.state(fixture, "staged")
  assert(staged.state == "ready" and staged.message:find("+ scope-staged", 1, true), vim.inspect(staged))
  assert(not staged.message:find("scope-unstaged", 1, true) and not staged.message:find("scope-untracked", 1, true))
  git({ "reset", "--quiet" })
  about = generate()
  assert(about:find("+ scope-unstaged", 1, true) and about:find("+ scope-untracked", 1, true), about)
  assert(not about:find("scope-ignored", 1, true) and not about:find("scope-staged", 1, true), about)
  local previous = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  generator.populate_commit_buffer_when_ready(buffer, fixture, true)
  await(function() return generator.state(fixture, "staged").state ~= "generating" end, "empty staged request did not settle")
  assert(generator.state(fixture, "staged").state == "none", "Ctrl-A fell back to unstaged changes")
  assert(vim.deep_equal(previous, vim.api.nvim_buf_get_lines(buffer, 0, -1, false)), "empty staged request changed the draft")
  vim.api.nvim_buf_delete(buffer, { force = true })
end, debug.traceback)
generator.reset_provider()
client.host_stop()
await(function() return client._client.process == nil end, "scope test host was not collected")
vim.fn.stdpath = original_stdpath
assert(success, failure)
print("commit_scope_host: staged About, unstaged fallback, reuse, and staged-only regeneration passed")
  end },
  { name = "commit_status_reuse", run = function()
local root = vim.fn.getcwd()
dofile(root .. "/nvim/lua/forge/tests/fixtures/commit_reuse_manual.lua")
local client = require("forge.client")
local ai_commit = require("forge.integrations.ai_commit")
local commit = require("forge.integrations.commit")
local state = forge_reuse.state
local function request(params, callback)
  forge_reuse.model_calls = forge_reuse.model_calls + 1
  vim.schedule(function()
    callback({ state = "ready", message = params.comparison == "staged"
      and "refactor: describe staged fixture" or forge_reuse.message })
  end)
end
local editor = commit.editor
local held = {}
local generation_calls = 0
local comparison_calls = 0
local hold_regeneration = false
local first_editor_line
ai_commit._provider.request_async = function(params, callback)
  if params.operation == "generate" then generation_calls = generation_calls + 1 end
  if params.operation == "fingerprint" then comparison_calls = comparison_calls + 1 end
  request(params, function(result, failure)
    if hold_regeneration and params.comparison == "staged" then
      held[#held + 1] = function() callback(result, failure) end
    else callback(result, failure) end
  end)
end
commit.editor = function(target, address)
  editor(target, address)
  first_editor_line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]
end
local ok, failure = xpcall(function()
  assert(vim.wait(15000, function()
    return state.replica.status == "Applied" and forge_reuse.model_calls == 1
      and not state.request_active and next(client._client.pending) == nil
  end, 10), "Status About did not finish")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n")
  assert(rendered:find("About:  " .. forge_reuse.message, 1, true), "Status did not display generated About")
  assert(generation_calls == 1, "initial Status generated more than once")
  assert(comparison_calls == 0, "cold About requested an unnecessary comparison")
  local selected
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == "tracked.txt" then selected = file end
  end
  assert(selected, "fixture tracked file missing")
  local _, row = state.replica.sequence:position(string.format("file:%.0f", selected.id))
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  vim.api.nvim_feedkeys("S", "xt", false)
  assert(vim.wait(15000, function()
    if state.request_active or next(client._client.pending) ~= nil then return false end
    local staged, remaining = false, false
    for _, file in ipairs(state.replica.inventory.file) do
      staged = staged or (file.path == "tracked.txt" and file.section == "staged")
      remaining = remaining or file.section == "unstaged"
    end
    return staged and remaining and #(state.replica.inventory.pending or {}) == 0
  end, 10), "partial staging did not settle with unstaged changes remaining")
  assert(generation_calls == 1, "staging generated another About message")
  hold_regeneration = true
  vim.api.nvim_feedkeys("cc", "xt", false)
  assert(vim.wait(15000, function() return first_editor_line ~= nil end, 10), "real commit editor did not open")
  assert(vim.b.forge_commit_buffer and vim.api.nvim_buf_get_name(0):match("COMMIT_EDITMSG$"), "cc did not open the real Git editor")
  assert(first_editor_line == forge_reuse.message, "first editor presentation omitted cached About")
  assert(generation_calls == 1 and comparison_calls == 0 and #held == 0, "opening editor performed generation or validation")
  assert(vim.wo.winbar:find("<C-a>", 1, true) and vim.wo.winbar:find("regenerate staged", 1, true), "regeneration help is missing")
  for _, mode in ipairs({ "n", "i" }) do
    assert(type(vim.fn.maparg("<C-a>", mode, false, true).callback) == "function", "Ctrl-A is missing in " .. mode)
  end
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-a>", true, false, true), "xt", false)
  assert(vim.wait(10000, function() return #held == 1 end, 10), "Ctrl-A did not generate staged context")
  assert(generation_calls == 2 and comparison_calls == 0, "Ctrl-A did not issue exactly one generation")
  assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]:find("Generating with ", 1, true), "regeneration omitted progress")
  held[1]()
  assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == "refactor: describe staged fixture", "Ctrl-A did not replace the message")
  local comments = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(comments:find("#", 1, true), "regeneration removed Git comments")
  print("Status -> cc reuses About without requests; Ctrl-A generates staged text and preserves comments")
end, debug.traceback)
commit.editor = editor
if commit._active then
  local abort = vim.fn.maparg("q", "n", false, true)
  if type(abort.callback) == "function" then
    vim.defer_fn(abort.callback, 100)
    assert(vim.wait(10000, function() return commit._active == nil end, 10), "fixture commit did not abort")
  end
end
require("forge.status").close(state)
client.host_stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "fixture host did not stop")
vim.fn.chdir(root)
vim.fn.delete(forge_reuse.fixture, "rf")
assert(ok, failure)
  end },
  { name = "empty_diff_cursor", run = function()
vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local executable_name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the Forge host first")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = data_directory .. "/" .. executable_name
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
executable = dofile("nvim/lua/forge/tests/support/host_executable.lua")()
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local status = require("forge.status")
local state, notices
notices = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local function git(arguments)
  local command = { "git", "-C", fixture }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end
local function settled()
  return state.replica.status == "Applied" and not state.pending and not state.scheduled
    and next(client._client.pending) == nil
end
local gutter = require("forge.gutter")
local clipboard
vim.g.clipboard = { name = "Forge fixture", copy = {
  ["+"] = function(lines, kind) clipboard = { lines = lines, kind = kind } end,
  ["*"] = function() end,
}, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }
local function mapping(key, mode)
  local value = vim.fn.maparg(key, mode or "n", false, true)
  return value.buffer == 1 and value or nil
end
local function moved(row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.replica.buffer })
  return vim.fn.getcurpos()
end
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "function foo()", "", "  return 1", "end" }, fixture .. "/sample.lua")
  git({ "add", "sample.lua" })
  git({ "commit", "--quiet", "-m", "Empty row fixture" })
  vim.fn.writefile({ "function foo()", "  return 2", "end" }, fixture .. "/sample.lua")
  vim.fn.chdir(fixture)
  require("forge").setup({ about_auto_generate = false })
  state = require("forge.views.commands").open()
  assert(vim.wait(10000, settled, 10), "native Status did not settle: " .. table.concat(notices, "\n"))
  local buffer = state.replica.buffer
  assert(vim.bo[buffer].filetype == "ForgeStatus")
  local file_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, true)) do
    if line:match("^Modified sample%.lua ") then file_row = row break end
  end
  assert(file_row, "native Status omitted file header: " .. table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, true), "\n"))
  moved(file_row)
  mapping("<Tab>").callback()
  assert(vim.wait(10000, function()
    return settled() and table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, true), "\n"):find("@@", 1, true)
  end, 10), "native diff demand did not settle")
  local empty_row, code_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, true)) do
    if gutter.bounds(state.replica, row) then
      if line == "" then empty_row = row end
      if line == "  return 2" then code_row = row end
    end
  end
  assert(empty_row and code_row, "native diff omitted empty or changed source row")
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  local empty_bounds = gutter.bounds(state.replica, empty_row)
  local position = moved(empty_row)
  assert(position[2] == empty_row and position[3] == 1 and position[4] == empty_bounds.width,
    "empty row cursor lost gutter offset: " .. vim.inspect(position))
  assert(vim.api.nvim_buf_get_lines(buffer, empty_row - 1, empty_row, true)[1] == "", "empty row gained padding")
  local code = vim.api.nvim_buf_get_lines(buffer, code_row - 1, code_row, true)[1]
  local bounds = gutter.bounds(state.replica, code_row)
  position = moved(code_row)
  assert(position[2] == code_row and position[3] == bounds.column + 1 and position[4] == bounds.width,
    "code row cursor lost gutter offset: " .. vim.inspect(position))
  assert(not mapping("<Space>l", "x"), "clipboard mapping installed before W")
  mapping("W").callback()
  assert(mapping("<Space>l", "x"), "W omitted temporary clipboard mapping")
  position = vim.fn.getcurpos()
  assert(vim.api.nvim_get_mode().mode == "V" and position[3] == 1 and position[4] == 0,
    "W did not select the actual gutter edge")
  assert(state.replica.gutter_selection.first == code_row and state.replica.gutter_selection.last == code_row,
    "gutter highlight range differs from native selection")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  position = vim.fn.getcurpos()
  assert(position[3] == 1 and position[4] == 0 and state.replica.gutter_selection,
    "cursor normalization disturbed active gutter selection")
  local prefix = {}
  for _, entry in ipairs(bounds.insertion) do
    for _, chunk in ipairs(entry.chunk) do prefix[#prefix + 1] = chunk.text end
  end
  mapping("<Space>l", "x").callback()
  assert(clipboard and clipboard.kind == "V"
    and vim.deep_equal(clipboard.lines, { table.concat(prefix) .. code, "" }),
    "clipboard differs from visible source and gutter: " .. vim.inspect(clipboard))
  assert(vim.api.nvim_get_mode().mode == "n" and not mapping("<Space>l", "x")
    and state.replica.gutter_selection == nil, "clipboard completion retained selection ownership")
  mapping("W").callback()
  assert(mapping("<Space>l", "x"), "second W omitted clipboard mapping")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  vim.api.nvim_exec_autocmds("ModeChanged", {})
  assert(not mapping("<Space>l", "x") and state.replica.gutter_selection == nil,
    "Escape retained clipboard or gutter highlight ownership")
  position = vim.fn.getcurpos()
  assert(position[3] == bounds.column + 1 and position[4] == bounds.width,
    "normal cursor restriction did not resume")
  vim.fn.setpos(".", { 0, code_row, #code + 20, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  position = vim.fn.getcurpos()
  assert(position[2] == code_row and position[3] == #code and position[4] == 0,
    "EOL clamp retained virtual text: " .. vim.inspect(position))
  assert(vim.api.nvim_buf_get_changedtick(buffer) == tick, "cursor or selection changed source bytes")
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
if state then status.close(state) end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then vim.api.nvim_err_writeln(failure or "host was not collected") vim.cmd("cquit 1") end
print("empty_diff_cursor OK: native source, cursor, gutter selection, clipboard and collected close")
vim.cmd("qa!")
  end },
  { name = "file_revision", run = function()
local fixture = dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/support/status_fixture.lua")
vim.loader.enable(false)

local forge = require("forge")
local gh = require("forge.integrations.gh")
local status = require("forge.status")

local root = "D:/diffreview-file-revision-root"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local original_notify = vim.notify
local captured_notifications = {}
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = {
    message = tostring(message),
    level = level,
    opts = opts,
  }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

--- Unified diff with one modified line ("old" -> "NEW") at line 2.
---@param relpath string
---@return string
local function modified_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1,3 +1,3 @@",
    " alpha " .. relpath,
    "-old " .. relpath,
    "+NEW " .. relpath,
    " omega " .. relpath,
  }, "\n")
end

---@param relpath string
---@return string[]
local function original_lines(relpath)
  return { "alpha " .. relpath, "old " .. relpath, "omega " .. relpath }
end

local function status_snapshot_text()
  return table.concat({
    "1 .M N... 100644 100644 100644 1111111 2222222 a.txt\0",
    "1 .M N... 100644 100644 100644 1111111 2222222 b.txt\0",
    "1 M. N... 100644 100644 100644 1111111 2222222 c.txt\0",
  })
end

---@param target_list {id: string, start_row: integer, end_row: integer}[]
---@return table
local function metadata(target_list)
  local target = {}
  for _, entry in ipairs(target_list) do
    target[#target + 1] = {
      id = entry.id,
      range = { start = { row = entry.start_row, column = 0 }, ["end"] = { row = entry.end_row, column = 0 } },
    }
  end
  return { target = target, decoration = {}, editable_region = {}, visible_decoration = {}, fold = {}, gutter = {} }
end

---@param document string
---@return table
local function native_status_snapshot(document)
  local snapshot = fixture.snapshot(document)
  snapshot.file = { fixture.file("a.txt", 1), fixture.file("b.txt", 2), fixture.file("c.txt", 3) }
  snapshot.file[3].section = "staged"
  for _, file in ipairs(snapshot.file) do
    file.stats = { state = "exact", added = 1, deleted = 1 }
    file.header = dofile("nvim/lua/forge/tests/support/status_fixture.lua").header(file.path, 1, 1)
  end
  snapshot.section = { { kind = "unstaged", file = { 1, 2 } }, { kind = "staged", file = { 3 } } }
  return snapshot
end

---@type ForgeGhProvider
local gh_provider = {}

function gh_provider.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

---@type ForgeGitProvider
local provider = {}

function provider.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { "abc1234" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { "file revision test" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return { "M\ta.txt", "M\tb.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return { "M\tc.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(modified_diff("a.txt") .. "\n" .. modified_diff("b.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return vim.split(modified_diff("c.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:a.txt" then
    return original_lines("a.txt"), 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\tHEAD:c.txt" then
    return original_lines("c.txt"), 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tfeature" then
    return { "fff5678" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\tfeature:a.txt" then
    return original_lines("a.txt"), 0
  end
  return {}, 1
end

function provider.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = provider.systemlist(command)
    cb(output, code)
  end, 3)
end

function provider.system(command)
  local key = command_key(command)
  local status_key = "git\t--no-optional-locks\t-C\t" .. root .. "\tstatus\t--porcelain=v2\t-z\t--untracked-files=all"
  local diff_key = "git\t--no-optional-locks\t-C\t" .. root
    .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0"
  local unstaged_key = diff_key .. "\t--diff-filter=MRC"
  if key == status_key then return status_snapshot_text(), 0 end
  if key == unstaged_key then return modified_diff("a.txt") .. "\n" .. modified_diff("b.txt"), 0 end
  if key == diff_key .. "\t--cached\t--diff-filter=MRC" then return modified_diff("c.txt"), 0 end
  if key:find("\tdiff\t", 1, true) and key:find("\t--numstat\t", 1, true) then return "", 0 end
  return "unexpected command: " .. command_key(command), 1
end

function provider.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = provider.system(command)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 3)
end

function provider.delete()
  return 0
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function trigger_buf_mapping(buf, key, row)
  vim.api.nvim_win_set_buf(0, buf)
  if row then
    vim.api.nvim_win_set_cursor(0, { row, 0 })
  end
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

---@param expected_name string
---@param expected_cursor_text string
---@return integer buf
local function assert_revision_buffer(expected_name, expected_cursor_text)
  local buf = vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(buf)
  assert_true(name:find(expected_name, 1, true) ~= nil, "wrong revision buffer name: " .. name)
  assert_true(vim.bo[buf].buftype == "nowrite", "revision buffer must be nowrite")
  assert_true(vim.bo[buf].readonly, "revision buffer must be readonly")
  assert_true(not vim.bo[buf].modifiable, "revision buffer must not be modifiable")
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  assert_true(line == expected_cursor_text, ("cursor on %q, expected %q"):format(line, expected_cursor_text))
  return buf
end

local function run()
  vim.notify = capture_notify
  forge.set_git_provider(provider)
  forge.setup({ about_auto_generate = false })
  status._set_runner_for_test(function(method, params, callback)
    assert_true(method == "repository.status.document", "file revision status must use the native route")
    if params.operation == "open" then
      callback(native_status_snapshot(params.document))
    elseif params.operation == "demand" then
      local id = params.input.location.id
      local name = ({ "a", "b", "c" })[id]
      local delivery = fixture.body(params.input.document, { "old " .. name .. ".txt", "NEW " .. name .. ".txt" })
      delivery.file, delivery.snapshot.document = id, "body:" .. id .. ":1"
      delivery.snapshot.block[1].id = name .. "-hunk"
      delivery.snapshot.block[1].metadata = metadata({ { id = name .. "-deleted", start_row = 0, end_row = 1 },
        { id = name .. "-added", start_row = 1, end_row = 2 } })
      callback(delivery)
    elseif params.operation == "close_view" or params.operation == "refresh" then
      callback(nil)
    elseif params.operation == "open_target" then
      local target = params.input.location.target
      if target == "a-deleted" or target == "c-deleted" then
        local path = target == "a-deleted" and "a.txt" or "c.txt"
        callback({
          id = "open-source-" .. target, kind = "open_source", document = params.input.document,
          revision = params.input.revision, view = params.input.view, sequence = params.input.sequence,
          workspace = root, path = vim.base64.encode(path), source_revision = "abc1234", row = 1,
        })
      elseif target == "a-added" or target == "b-deleted" then
        local path = target == "a-added" and "a.txt" or "b.txt"
        callback({
          id = "open-file-" .. target, kind = "open_file", document = params.input.document,
          revision = params.input.revision, view = params.input.view, sequence = params.input.sequence,
          path = root .. "/" .. path, row = 1, column = 0,
        })
      else
        callback(nil, "unexpected status target: " .. tostring(target))
      end
    elseif params.operation == "close" then
      callback({ closed = true })
    else
      error("unexpected native status operation: " .. params.operation)
    end
  end)
  require("forge.source_document")._set_runner_for_test(function(method, params, callback)
    assert_true(method == "repository.source.document", "revision must use shared native source route")
    if params.operation ~= "open" then callback({ closed = true }) return end
    local path = vim.base64.decode(params.path)
    if path == "b.txt" or params.revision == "missingrev" then
      callback(nil, "revision unavailable: " .. params.revision)
      return
    end
    local object = params.revision == "feature" and "fff5678" or "abc1234"
    callback({ object = object, title = path .. " @ " .. object, state = { state = "ready" }, more = false,
      snapshot = { document = params.document, revision = 1, block = { { id = "historical", text = original_lines(path),
        metadata = { target = {}, decoration = {}, editable_region = {}, visible_decoration = {} } } } } })
  end)

  local status_state = forge.open()
  wait_for(function()
    return status_state.replica and buffer_contains(status_state.replica.buffer, "Unstaged changes (2)")
  end, "status did not render")
  local status_buf = status_state.replica.buffer
  -- ── open on an unstaged deleted line lands in the index revision ───────────
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "a.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old a.txt") end, "a.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old a.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("ForgeFileRevision", 1, true) ~= nil
  end, "open on unstaged deleted line did not open a revision buffer")
  local index_buf = assert_revision_buffer("ForgeFileRevision://a.txt@abc1234", "old a.txt")
  assert_true(
    vim.wo.winbar:find("a.txt @ abc1234", 1, true) ~= nil,
    "revision winbar header missing: " .. vim.wo.winbar
  )
  -- ── the buffer stays alive, loaded, and listed once hidden ─────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  assert_true(
    vim.wo.winbar:find("read-only revision", 1, true) == nil,
    "winbar header must clear when leaving the revision buffer, got: " .. vim.wo.winbar
  )
  assert_true(vim.api.nvim_buf_is_valid(index_buf), "revision buffer must stay alive when hidden")
  assert_true(vim.api.nvim_buf_is_loaded(index_buf), "revision buffer must stay loaded when hidden")
  assert_true(vim.bo[index_buf].buflisted, "revision buffer must stay listed when hidden")
  -- ── re-opening reuses the same buffer ──────────────────────────────────────
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old a.txt"))
  wait_for(function() return vim.api.nvim_get_current_buf() == index_buf end, "revision buffer was not reused")
  -- ── open on a staged deleted line lands in the HEAD revision ───────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  if not buffer_contains(status_buf, "c.txt +1 -1") then
    trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "Staged changes (1)"))
  end
  wait_for(function() return buffer_contains(status_buf, "c.txt +1 -1") end, "staged section did not expand")
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "c.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old c.txt") end, "c.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old c.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("c.txt@", 1, true) ~= nil
  end, "open on staged deleted line did not open the HEAD revision")
  assert_revision_buffer("ForgeFileRevision://c.txt@abc1234", "old c.txt")
  -- ── added lines still jump to the working-tree file ────────────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "NEW a.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("a.txt", 1, true) ~= nil
      and vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("ForgeFileRevision", 1, true) == nil
  end, "open on added line did not edit the working-tree file")
  -- ── failed base lookup falls back to the working-tree jump ─────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "b.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old b.txt") end, "b.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old b.txt"))
  wait_for(function()
    local name = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
    return name:find("b.txt", 1, true) ~= nil and name:find("ForgeFileRevision", 1, true) == nil
  end, "failed base lookup did not fall back to the working-tree file")
  -- ── :ForgeFileRevision entry point names the buffer with the short sha ───────
  forge.open_file_revision(root .. "/a.txt", "feature")
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("@fff5678", 1, true) ~= nil
  end, "open_file_revision did not open the requested revision")
  assert_revision_buffer("ForgeFileRevision://a.txt@fff5678", "alpha a.txt")
  -- ── q closes the revision buffer ───────────────────────────────────────────
  local q_buf = vim.api.nvim_get_current_buf()
  trigger_buf_mapping(q_buf, "q")
  assert_true(not vim.api.nvim_buf_is_valid(q_buf), "q did not close the revision buffer")
  -- ── command errors are reported ────────────────────────────────────────────
  captured_notifications = {}
  forge.open_file_revision(root .. "/a.txt", "missingrev")
  wait_for(function() return saw_notification_containing("missingrev") end, "missing revision error not notified")

  captured_notifications = {}
  forge.open_file_revision("", "")
  wait_for(function() return saw_notification_containing("requires a file and a revision") end, "empty args not rejected")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
forge.reset_git_provider()
gh.reset_provider()
status._set_runner_for_test(nil)
require("forge.source_document")._set_runner_for_test(nil)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
  end },
  { name = "revision_commands", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local original_systemlist = vim.fn.systemlist
local host_count, harness_count, page_count = 0, 0, 0
local expected = {}
for index = 1, 20000 do expected[index] = string.format("branch-%05d-with-a-long-name-for-paging", index) end
local packed = table.concat(expected, "\0") .. "\0"
local initialized = false

local ok, failure = xpcall(function()
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  plugin.config(nil, { harness = { provider = "mock" }, harness_logging = false, diff_logging = false })
  builder.ensure = function(done) done({ ok = true, path = "injected-forge" }) end
  client._host_set_launcher_for_test(function(_, options)
    host_count = host_count + 1
    local function emit(id, result)
      options.stdout(nil, vim.json.encode({ id = id, result = result }) .. "\n")
    end
    return {
      write = function(_, bytes)
        local request = vim.json.decode(bytes)
        if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
        if request.method == "host.initialize" then
          assert(request.params.protocol_version == require("forge.protocol").VERSION)
          assert(request.params.provider == nil and request.params.data_root == nil)
          emit(request.id, { protocol_version = require("forge.protocol").VERSION })
        elseif request.method == "repository.revision.list" then
          page_count = page_count + 1
          local offset = request.params.offset or 0
          local next_offset = math.min(offset + 128 * 1024, #packed)
          emit(request.id, {
            repository = "repository-fixture", reference_digest = string.rep("1", 64),
            revision = 1, offset = offset, next_offset = next_offset, total_bytes = #packed,
            count = #expected, truncated = false, data = vim.base64.encode(packed:sub(offset + 1, next_offset)),
          })
        elseif request.method == "host.trace.configure" then
          emit(request.id, { enabled = false })
        elseif request.method == "provider.registry.list" then
          emit(request.id, dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry())
        elseif request.method == "session.initialize" then
          harness_count = harness_count + 1
          emit(request.id, { session = { id = "shared-host" } })
        else error("unexpected shared-host method " .. request.method) end
      end,
      kill = function() end,
    }
  end)
  vim.fn.systemlist = function() error("completion executed synchronous Git") end
  local first = vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline")
  assert(#first == 0 and host_count == 0, "cold command callback performed I/O")
  assert(vim.wait(2000, function()
    local values = vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline")
    return #values == #expected
  end, 1))
  assert(host_count == 1 and harness_count == 0 and page_count > 1)
  local requests = page_count
  local values = vim.fn.getcompletion("ForgeFileRevision file branch-04999", "cmdline")
  assert(#values == 1 and values[1] == expected[4999])
  local file_values = vim.fn.getcompletion("ForgeBranchDiffFile file branch-05000", "cmdline")
  assert(#file_values == 1 and file_values[1] == expected[5000])
  assert(page_count == requests)
  local sample = {}
  for index = 1, 40 do
    local started = vim.uv.hrtime()
    assert(#vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline") == #expected)
    sample[index] = (vim.uv.hrtime() - started) / 1000000
  end
  table.sort(sample)
  print(vim.json.encode({ completion_values = #expected, samples = #sample, p95_ms = sample[38], maximum_ms = sample[40] }))
  client.session_initialize(function(_, start_error)
    assert(not start_error, start_error)
    initialized = true
  end)
  assert(vim.wait(1000, function() return initialized end, 1))
  assert(host_count == 1 and harness_count == 1, "Harness launched a second host")
end, debug.traceback)

vim.fn.systemlist = original_systemlist
builder.ensure = original_ensure
client._host_reset_for_test()
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
  end },
  { name = "revision_commands_real", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local original_notify = vim.notify
local original_directory = vim.fn.getcwd()
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local owned_root = vim.uv.fs_realpath(root)
local binary = vim.fs.joinpath(original_directory, "nvim", "rust", "forge", "target", "debug", "forge.exe")
if vim.fn.has("win32") == 0 then binary = binary:sub(1, -5) end
local notifications = {}

---@param arguments string[]
local function git(arguments)
  local command = { "git", "-C", root }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, stdout = true, stderr = true }):wait(5000)
  assert(result.code == 0, result.stderr)
end

local ok, failure = xpcall(function()
  assert(vim.fn.executable(binary) == 1, "build the Forge debug executable before native host tests")
  git({ "init", "--quiet", "--initial-branch=main" })
  vim.fn.writefile({ "content" }, vim.fs.joinpath(root, "file"))
  git({ "add", "file" })
  git({ "-c", "user.name=Forge Test", "-c", "user.email=forge@example.test", "commit", "--quiet", "-m", "base" })
  git({ "branch", "feature-é" })
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  vim.cmd.cd(vim.fn.fnameescape(root))
  plugin.config(nil, { harness = { provider = "mock" }, harness_logging = false, diff_logging = false })
  builder._set_artifact_root_for_test(vim.fs.joinpath(root, "forge-sidecar"))
  builder.ensure = function(done) done({ ok = true, path = binary }) end
  vim.notify = function(message, level, options)
    notifications[#notifications + 1] = { message = tostring(message), level = level, options = options }
  end
  assert(#vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline") == 0)
  assert(vim.wait(10000, function()
    local values = vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline")
    return #values == 1 and values[1] == "feature-é"
  end, 5), "native host did not publish branch candidates: " .. vim.inspect({
    notifications = notifications,
    client = client._client and {
      ready = client._client.ready,
      starting = client._client.starting,
      draining = client._client.draining,
      stderr = client._client.stderr,
      pending = vim.tbl_keys(client._client.pending),
    },
    values = vim.fn.getcompletion("ForgeBranchDiff feature-", "cmdline"),
  }))
  assert(client._client.ready and not client._client.harness_ready)
  local process = client._client.process
  client.host_stop()
  local exit = process:wait(2000)
  assert(exit.code == 0, "shared host did not shut down cleanly")
end, debug.traceback)

builder.ensure = original_ensure
builder._host_reset_for_test()
vim.notify = original_notify
client._host_reset_for_test()
vim.cmd.cd(vim.fn.fnameescape(original_directory))
if owned_root and vim.uv.fs_realpath(root) == owned_root then vim.fn.delete(root, "rf") end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
  end },
  { name = "diff_controls", run = function()
vim.o.columns = 160
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local status = require("forge.status")
local render = require("forge.status_render")
local nodes = require("forge.nodes")
local artifact = root .. "/nvim/rust/forge/target/status-semantic-fixture.json"
local fixture = vim.json.decode(table.concat(vim.fn.readfile(artifact), "\n"))
local original_document = fixture.initial.document
local notices, controls, sections = {}, {}, {}
local full_delivered = {}
local delayed
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local function replace_document(value, document)
  if type(value) == "string" then return value:gsub(vim.pesc(original_document), document) end
  if type(value) ~= "table" then return value end
  local result = {}
  for key, entry in pairs(value) do result[key] = replace_document(entry, document) end
  return result
end
local function wait_for(predicate)
  assert(vim.wait(3000, predicate, 5), table.concat(notices, "\n"))
end
status._set_runner_for_test(function(method, params, callback)
  assert(method == "repository.status.document", method)
  if params.operation == "open" then
    fixture = replace_document(fixture, params.document)
    callback(fixture.initial)
  elseif params.operation == "demand" then
    for _, delivery in ipairs(fixture.full) do
      if delivery.document == params.input.document and delivery.file == params.input.location.id then
        if not full_delivered[delivery.document] then
          full_delivered[delivery.document] = true
          callback(delivery)
        else callback({ document = delivery.document, file = delivery.file, generation = delivery.generation, more = false, state = "ready" }) end
        return
      end
    end
    error("unexpected source demand: " .. vim.inspect(params.input))
  elseif params.operation == "diff_view" then
    controls[#controls + 1] = params
    assert(#params.related == 1 and params.related[1] == fixture.child_initial.document)
    local group = ({ fixture.semantic, fixture.public, fixture.public_full, fixture.all_full })[#controls]
    assert(group, "unexpected diff control request")
    if #controls == 1 then delayed = function() callback(group) end else callback(group) end
  elseif params.operation == "source_section" then
    sections[#sections + 1] = params.input
    assert(params.input.action == "source_expand")
    callback(fixture.expanded)
  elseif params.operation == "refresh" or params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then callback({ closed = true })
  else error("unexpected status request: " .. params.operation) end
end)
local state = status.open({ workspace = root })
wait_for(function() return state.ready and not state.request_active end)
local commit = fixture.child_initial.document:match(":commit:(.+)$")
local owner = require("forge.status_history").new(state.document, commit)
owner.snapshot = fixture.child_initial
state.replica.commit[commit] = owner
assert(render.apply_snapshot(state.replica, state.replica.inventory).kind == "Applied")
local parent_file = fixture.full[1].file
for _, delivery in ipairs(fixture.full) do
  assert(render.apply_body(state.replica, delivery).kind == "Applied", table.concat(notices, "\n"))
  full_delivered[delivery.document] = true
end
local child_file = require("forge.status_history").file_key(owner, fixture.full[2].file)
assert(nodes.set_open(state.replica, 0, "status:context:recent:" .. commit, true))
assert(nodes.set_open(state.replica, 0, "file:" .. parent_file, true))
assert(nodes.set_open(state.replica, 0, "file:" .. child_file, true))
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
local first_window = vim.fn.win_findbuf(state.replica.buffer)[1]
status.demand(state)
assert(vim.wait(3000, function() return state.view[first_window] and state.view[second_window] end, 5), vim.inspect({ view = state.view, windows = vim.fn.win_findbuf(state.replica.buffer), first = first_window, second = second_window, ready = state.ready, active = state.active, pending = state.pending, request = state.request_active, status = state.replica.status, notices = notices }))
assert(vim.fn.maparg("ov", "n", false, true).callback)
assert(vim.fn.maparg("ox", "n", false, true).callback)
vim.fn.maparg("ox", "n", false, true).callback()
wait_for(function() return delayed ~= nil end)
status.toggle_diff(state, "toggle_visibility")
status.toggle_diff(state, "toggle_visibility")
assert(state.replica.inventory.diff_view.presentation == "full", "uncommitted control changed the visible settings")
delayed()
wait_for(function() return not state.request_active and not state.diff_active end)
assert(#controls == 1, "cancelled rapid visibility toggles issued work")
assert(state.replica.inventory.diff_view.presentation == "semantic")
assert(owner.snapshot.diff_view.presentation == "semantic")
local parent_heading
for id, entry in pairs(state.replica.source_entry) do
  if state.replica.body_owner[id] == parent_file and entry.metadata.section and #entry.metadata.section > 0
    and entry.text[1]:find("pub fn visible()", 1, true) then parent_heading = id break end
end
assert(parent_heading, "semantic parent heading missing")
assert(nodes.closed(state.replica, parent_heading), "semantic body started open")
local _, row = state.replica.sequence:position(parent_heading)
vim.api.nvim_win_set_cursor(second_window, { row + 1, 0 })
vim.fn.maparg("<Tab>", "n", false, true).callback()
wait_for(function() return #sections == 1 and not state.request_active end)
assert(not nodes.closed(state.replica, parent_heading))
local text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(text:find("before()", 1, true) and text:find("after()", 1, true), text)
local source_block
for id, entry in pairs(state.replica.source_entry) do
  if state.replica.body_owner[id] == parent_file and entry.text and entry.text[1] and entry.text[1]:find("after()", 1, true) then source_block = id break end
end
assert(source_block)
local _, source_row = state.replica.sequence:position(source_block)
vim.api.nvim_win_set_cursor(second_window, { source_row + 1, 0 })
vim.fn.maparg("ov", "n", false, true).callback()
wait_for(function() return #controls == 2 and not state.request_active end)
assert(state.replica.inventory.diff_view.visibility == "public")
assert(owner.snapshot.diff_view.visibility == "public")
text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(not text:find("hidden", 1, true) and not text:find("private_", 1, true), text)
local cursor = vim.api.nvim_win_get_cursor(second_window)
assert(render.locate(state.replica, cursor[1] - 1, cursor[2]).block == source_block, "visibility toggle moved the surviving source cursor")
vim.fn.maparg("ox", "n", false, true).callback()
wait_for(function() return #controls == 3 and not state.request_active end)
assert(state.replica.inventory.diff_view.presentation == "full" and state.replica.inventory.diff_view.visibility == "public")
assert(owner.snapshot.diff_view.presentation == "full")
vim.fn.maparg("ov", "n", false, true).callback()
wait_for(function() return #controls == 4 and not state.request_active end)
text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(text:find("private_after", 1, true) and text:find("private_before", 1, true), text)
assert(#notices == 0, table.concat(notices, "\n"))
status.close(state)
wait_for(function() return not state.request_active end)
status._set_runner_for_test(nil)
local replica = render.open(fixture.initial.document, { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_win_set_buf(0, replica.buffer)
local child = require("forge.status_history").new(replica.document, commit)
child.snapshot = fixture.child_initial
replica.commit[commit] = child
assert(render.apply_snapshot(replica, fixture.initial).kind == "Applied")
for _, delivery in ipairs(fixture.full) do assert(render.apply_body(replica, delivery).kind == "Applied") end
local retained_parent = replica.file[parent_file].body
local retained_child = replica.file[child_file].body
local parent_revision, child_revision = retained_parent.revision, retained_child.revision
local before = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
local invalid = vim.deepcopy(fixture.semantic)
invalid[2].body[1].generation = invalid[2].body[1].generation + 1
assert(render.apply_updates(replica, invalid).kind == "Desynchronized")
assert(replica.file[parent_file].body == retained_parent and retained_parent.revision == parent_revision)
assert(replica.file[child_file].body == retained_child and retained_child.revision == child_revision)
assert(child.snapshot == fixture.child_initial, "failed transaction published a child snapshot")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), before), "failed transaction changed the usable buffer")
assert(#notices == 1)
render.close(replica)
print("shared status diff controls: passed")
  end },
  { name = "status", run = function()
vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the Forge host first")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = build
local phase_file = vim.fn.tempname() .. ".status-host-phase.log"
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local status = require("forge.status")
local state, notices
local phase = "setup"
notices = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local function git(arguments)
  local command = { "git", "-C", fixture }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end
local function settled()
  return state.replica.status == "Applied" and not state.pending and not state.scheduled
    and #(state.replica.inventory.pending or {}) == 0
    and next(client._client.pending) == nil
end
local function pending_diagnostic()
  local pending = {}
  for id, request in pairs(client._client.pending) do pending[id] = request.method end
  return vim.inspect({
    phase = phase,
    replica = state and state.replica.status or nil,
    status_pending = state and state.pending or nil,
    status_scheduled = state and state.scheduled or nil,
    status_request_active = state and state.request_active or nil,
    queued_status_requests = state and #(state.request_queue or {}) or nil,
    client_pending = pending,
    client_stderr = client._client.stderr,
    notices = notices,
  })
end
---@param name string
local function record_phase(name)
  phase = name
  vim.fn.writefile({ string.format("%d %s", vim.uv.hrtime(), name) }, phase_file, "a")
end
---@return string
local function failure_diagnostic()
  return pending_diagnostic() .. "\nphase file: " .. phase_file
end
local function await_phase(name, timeout, predicate)
  record_phase(name)
  assert(vim.wait(timeout, predicate, 200), "status host phase timed out: " .. failure_diagnostic())
end
local ok, failure = xpcall(function()
  record_phase("setup repository")
  git({ "init", "--quiet" })
  git({ "config", "core.autocrlf", "false" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "local value = 1", "return value" }, fixture .. "/tracked file.lua")
  git({ "add", "tracked file.lua" })
  git({ "commit", "--quiet", "-m", "Status fixture" })
  vim.fn.writefile({ "local value = 2", "return value" }, fixture .. "/tracked file.lua")
  vim.fn.chdir(fixture)
  record_phase("open native status")
  require("forge").setup({ about_auto_generate = false })
  state = require("forge.views.commands").open()
  await_phase("initial document application", 15000, settled)
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(vim.b[state.replica.buffer].forge_native_document and not client._client.harness_ready)
  assert(not vim.wo.foldenable, "Status enabled native folds")
  local file = assert(state.replica.inventory.file[1], "status file is missing")
  local _, file_row = state.replica.sequence:position("file:" .. file.id)
  vim.api.nvim_win_set_cursor(0, { file_row + 1, 0 })
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "xt", false)
  await_phase("file and hunk expansion", 15000, function()
    return settled() and next(state.done) ~= nil
      and table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n"):find("@@", 1, true)
  end)
  status.navigate(state, true)
  await_phase("native hunk navigation", 10000, function() return not state.navigation and settled() end)
  local hunk_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
    if line:find("@@", 1, true) then hunk_row = row break end
  end
  assert(hunk_row, "status hunk is missing")
  vim.api.nvim_win_set_cursor(0, { hunk_row, 0 })
  local nodes = require("forge.nodes")
  local hunk = assert(nodes.at(state.replica, hunk_row - 1))
  assert(nodes.set_open(state.replica, 0, hunk.fold.id, false))
  assert(nodes.closed(state.replica, hunk.fold.id), "status hunk did not collapse")
  assert(nodes.set_open(state.replica, 0, hunk.fold.id, true))
  _, file_row = state.replica.sequence:position("file:" .. file.id)
  vim.api.nvim_win_set_cursor(0, { file_row + 1, 0 })
  assert(nodes.set_open(state.replica, 0, "file:" .. file.id, false))
  assert(not state.replica.sequence.node[hunk.owner], "closed file left hunk visible")
  local closed = false
  record_phase("close native status")
  status.close(state, function() closed = true end)
  await_phase("close native status", 6000, function() return closed end)
  state = nil
end, debug.traceback)
if state then status.close(state) end
client.host_stop()
record_phase("collect native host")
local collected = vim.wait(6000, function() return client._client.process == nil end, 200)
record_phase("cleanup")
vim.fn.chdir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then
  vim.api.nvim_err_writeln((failure or "host was not collected") .. "\nphase file: " .. phase_file)
  vim.cmd("cquit 1")
end
vim.fn.delete(phase_file)
print("status_host OK: public native Status lifecycle collected")
vim.cmd("qa!")
  end },
  { name = "popup", run = function()
local root = vim.fn.getcwd()
dofile(root .. "/nvim/lua/forge/tests/fixtures/commit_reuse_manual.lua")
local state = forge_reuse.state
local status = require("forge.status")
local client = require("forge.client")
local notices = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then notices[#notices + 1] = message end end
vim.ui.select = function() error("Status invoked a Snacks picker") end
vim.ui.input = function() error("Status invoked a Snacks input") end
local function idle()
  return state.ready and not state.request_active and next(client._client.pending) == nil
end
local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
end
local function file_row(path)
  for id, model in pairs(state.replica.file) do
    if model.record.path == path then return select(2, state.replica.sequence:position("file:" .. id)) + 1 end
  end
  error("Missing fixture file " .. path)
end
local function read(path) return vim.fn.readfile(forge_reuse.fixture .. "/" .. path) end
local ok, failure = xpcall(function()
  assert(vim.wait(15000, idle, 10), "native Status did not open")
  local origin = vim.api.nvim_get_current_win()
  vim.v.errmsg = ""
  press("bc")
  assert(vim.bo.filetype == "ForgeBranchPrompt" and vim.api.nvim_win_get_height(0) == 1)
  assert(vim.wo.winbar == "" and vim.v.errmsg == "", "branch popup inherited a Status winbar: " .. vim.v.errmsg)
  press("q")
  assert(vim.api.nvim_get_current_win() == origin)
  local revision = state.replica.revision
  local first, last = file_row("tracked.txt"), file_row("untracked.txt")
  vim.api.nvim_win_set_cursor(origin, { first, 0 })
  vim.cmd("normal! V" .. last .. "G")
  status.action(state, "discard", true)
  assert(vim.bo.filetype == "ForgeConfirm" and vim.api.nvim_get_current_line() == "Discard changes in 2 file(s)?")
  assert(vim.wait(5000, idle, 10), "Status did not drain while the confirmation was open")
  press("n")
  assert(vim.wait(1000, idle, 10))
  assert(state.replica.revision == revision, "returning from confirmation refreshed and invalidated the captured selection")
  assert(read("tracked.txt")[1] == "after" and read("untracked.txt")[1] == "new content")
  vim.api.nvim_win_set_cursor(origin, { file_row("tracked.txt"), 0 })
  press("j")
  assert(vim.bo.filetype == "ForgeConfirm", "discard mapping did not open the old confirmation")
  assert(vim.api.nvim_get_current_line() == "Discard ALL changes to file?")
  press("n")
  assert(read("tracked.txt")[1] == "after", "cancel discarded the tracked file")
  press("j")
  press("y")
  assert(vim.wait(5000, function() return idle() and read("tracked.txt")[1] == "before" end, 10), table.concat(notices, "\n"))
  vim.api.nvim_win_set_cursor(origin, { file_row("untracked.txt"), 0 })
  press("j")
  assert(vim.api.nvim_get_current_line() == "Delete untracked file?")
  press("<Esc>")
  assert(read("untracked.txt")[1] == "new content")
  press("j")
  press("y")
  assert(vim.wait(5000, function() return idle() and vim.fn.filereadable(forge_reuse.fixture .. "/untracked.txt") == 0 end, 10))
  first, last = file_row("deleted.txt"), file_row("old-name.txt")
  vim.api.nvim_win_set_cursor(origin, { first, 0 })
  vim.cmd("normal! V" .. last .. "G")
  status.action(state, "discard", true)
  assert(vim.api.nvim_get_current_line() == "Discard changes in 3 file(s)?")
  assert(vim.wait(5000, idle, 10), "Status did not drain while the confirmation was open")
  press("y")
  assert(vim.wait(5000, function() return idle() and #state.replica.inventory.file == 0 end, 10), table.concat(notices, "\n"))
  assert(read("deleted.txt")[1] == "removed" and read("old-name.txt")[1] == "original renamed content")
  assert(vim.fn.filereadable(forge_reuse.fixture .. "/new-name.txt") == 0)
  local _, issue_row = state.replica.root:position("status:context:issues")
  vim.api.nvim_win_set_cursor(origin, { issue_row + 1, 8 })
  state.replica.issues_editor.sync()
  assert(vim.bo.modifiable, "Issues is not editable inline")
  vim.api.nvim_buf_set_lines(0, issue_row, issue_row + 1, false, { "Issues: #42 #7" })
  vim.api.nvim_win_set_cursor(origin, { issue_row + 2, 0 })
  state.replica.issues_editor.sync()
  vim.cmd("write")
  assert(vim.wait(5000, function() return idle() and not state.replica.issues_editor.dirty end, 10), table.concat(notices, "\n"))
  local persisted = vim.json.decode(table.concat(read(".forge.json"), "\n"))
  assert(vim.deep_equal(persisted.issues, { 7, 42 }), "inline Issues did not reach native persistence")
  assert(state.replica.source_entry["status:context:issues"].text[1] == "Issues: #7 #42")
  assert(#notices == 0, table.concat(notices, "\n"))
  status.close(state)
  client.host_stop()
end, debug.traceback)
if not ok then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("native discard confirmations and inline Issues persistence passed")
vim.cmd("qa!")
  end },
  { name = "push", run = function()
require("forge.infra.config").defaults.about_auto_generate = false
dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/fixtures/commit_reuse_manual.lua")
local state = forge_reuse.state
local repository = forge_reuse.fixture
local remote = vim.fn.tempname() .. "-forge-push-remote"
local client = require("forge.client")
local writer = require("forge.git.write")
local original_execute = writer.execute
local errors = {}
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = message end
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
local function idle() return state.ready and not state.request_active and next(client._client.pending) == nil end
assert(vim.wait(15000, idle, 10), "status did not open")
git({ "init", "--bare", remote })
git({ "remote", "add", "origin", remote })
git({ "push", "--set-upstream", "origin", "HEAD" })
git({ "add", "tracked.txt" })
git({ "commit", "--quiet", "-m", "test: push fixture" })
local expected = git({ "rev-parse", "HEAD" })
local branch = git({ "symbolic-ref", "--short", "HEAD" })
local function remote_head()
  local result = vim.system({ "git", "--git-dir", remote, "rev-parse", "refs/heads/" .. branch }, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
assert(remote_head() ~= expected, "fixture remote already has the commit")
require("forge.status").refresh(state)
assert(vim.wait(10000, idle, 10), "status did not refresh")
local header
for row, text in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
  if text:find("Unstaged changes", 1, true) then header = row break end
end
assert(header, "missing section header")
vim.api.nvim_win_set_cursor(0, { header, 0 })
local outcome, writes = nil, 0
local left_status, progress_seen = false, false
vim.api.nvim_create_autocmd("BufWinLeave", { buffer = state.replica.buffer, callback = function() left_status = true end })
writer.execute = function(workspace, action, callback, progress)
  assert(workspace == repository and action.kind == "push", "unexpected write")
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "push switched buffers before starting")
  assert(state.replica.presentation.remote_action.status:find(" Pushing…", 1, true), "push did not present initial progress")
  writes = writes + 1
  return original_execute(workspace, action, function(result) outcome = result callback(result) end, function(text, stream)
    progress(text, stream)
    progress_seen = true
    assert(not left_status and vim.api.nvim_get_current_buf() == state.replica.buffer, "push progress replaced Status")
    local status = state.replica.presentation.remote_action.status
    local rows = table.concat(state.replica.source_entry["status:context:push"].text, " ")
    assert(rows:find(status, 1, true) or rows:find(status:sub(1, 20), 1, true), "Git progress did not reach the Push header")
  end)
end
_G.forge_push_check = function()
  assert(outcome and outcome.ok, "push failed: " .. vim.inspect(outcome))
  assert(writes == 1, "push was not dispatched exactly once")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(remote_head() == expected, "remote did not receive commit")
  assert(not left_status and progress_seen, "Status did not stay visible throughout streamed progress")
  assert(state.replica.presentation.remote_action == nil, "finished push retained temporary progress")
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "push did not restore Status")
  print("status_push_host OK: inline Git progress, no buffer switch, local remote updated")
end
if vim.g.forge_manual_push then return end
vim.o.timeout, vim.o.timeoutlen = true, 300
local index = 0
local timer = assert(vim.uv.new_timer())
timer:start(50, 50, function()
  index = index + 1
  vim.api.nvim_input(("opp"):sub(index, index))
  if index == 3 then timer:stop() timer:close() end
end)
local started = vim.uv.hrtime()
local checking_rejection = false
local function finish()
  if not outcome and vim.uv.hrtime() - started < 10e9 then vim.defer_fn(finish, 50) return end
  local ok, failure = xpcall(function()
    if not checking_rejection then
      forge_push_check()
      vim.fn.writefile({ "#!/bin/sh", "echo fixture-push-rejected >&2", "exit 1" }, remote .. "/hooks/pre-receive")
      assert(vim.uv.fs_chmod(remote .. "/hooks/pre-receive", 493))
      vim.fn.writefile({ "next rejected change" }, repository .. "/tracked.txt")
      git({ "add", "tracked.txt" })
      git({ "commit", "--quiet", "-m", "test: rejected push" })
      outcome, errors, progress_seen = nil, {}, false
      checking_rejection, started = true, vim.uv.hrtime()
      vim.api.nvim_input("opp")
      return false
    end
    assert(outcome and not outcome.ok and writes == 2, "rejecting hook did not fail exactly one push")
    assert(table.concat(errors, "\n"):find("fixture-push-rejected", 1, true), "rejection omitted its error notification")
    assert(remote_head() == expected, "rejected push changed the remote")
    assert(not left_status and progress_seen and vim.api.nvim_get_current_buf() == state.replica.buffer,
      "failed push replaced Status")
    assert(state.replica.presentation.remote_action == nil, "failed push retained temporary progress")
    print("status_push_host rejection OK: Status retained, progress cleared, error notified, remote unchanged")
    return true
  end, debug.traceback)
  if ok and failure == false then vim.defer_fn(finish, 50) return end
  writer.execute, vim.notify = original_execute, original_notify
  require("forge.status").close(state)
  if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") return end
  vim.cmd("qa!")
end
vim.defer_fn(finish, 250)
  end },
  { name = "recent_commits", run = function()
vim.loader.enable(false)
local fixture = dofile("nvim/lua/forge/tests/support/status_fixture.lua")
local status = require("forge.status")
local render = require("forge.status_render")
local history = require("forge.status_history")
local nodes = require("forge.nodes")
package.loaded["forge.views.status.status_context"] = { attach = function()
  return { refresh = function() end, close = function() end, producer_handlers = function() return {} end }
end, producer_handlers = function() return {} end }
local notices, requests, closed, demand = {}, {}, {}, {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local first, second, empty, failing = string.rep("a", 40), string.rep("b", 40), string.rep("c", 40), string.rep("d", 40)
local context = { workspace = vim.fn.getcwd(), branch = "main", issues = {}, recent = {
  { oid = first, reference = "main", subject = "Latest commit" },
  { oid = second, reference = "", subject = "Earlier commit" },
  { oid = empty, reference = "", subject = "Empty commit" },
  { oid = failing, reference = "", subject = "Failed comparison" },
} }
local pending
local state
status._set_runner_for_test(function(method, params, callback)
  requests[#requests + 1] = params.operation or method
  if params.operation == "open" then callback(fixture.snapshot(params.document, { context = context }))
  elseif params.operation == "comparison" then
    assert(params.worktree == false and params.reference:match("^%x+$"))
    local snapshot = fixture.snapshot(params.document, { view = { kind = "comparison", title = params.reference, worktree = false } })
    if params.reference == empty then snapshot.file, snapshot.section = {}, {} end
    if params.reference == failing then callback(nil, "comparison fixture failure")
    else pending = function() callback(snapshot) end end
  elseif params.operation == "demand" then
    demand[#demand + 1] = params.input
    assert(params.input.location.id == 1, "UI identity leaked to native comparison")
    callback(fixture.body(params.input.document, { "@@ -1 +1 @@", "old", "new" }, { block = "body:2", target = "hunk:3" }))
  elseif params.operation == "open_target" then
    assert(params.input.document == state.replica.commit[first].document)
    assert(params.input.location.block == "body:2" and params.input.location.target == "hunk:3")
    callback({ id = "effect", document = params.input.document, revision = params.input.revision,
      view = params.input.view, sequence = params.input.sequence, kind = "notify", message = "Historical source selected", level = 2 })
  elseif params.operation == "refresh" or params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then closed[params.document] = true callback({ closed = true })
  else error("unexpected request " .. vim.inspect(params)) end
end)
local function settle()
  assert(vim.wait(2000, function() return state.ready and not state.request_active and not state.scheduled end, 5), table.concat(notices, "\n"))
end
local function row(id) return select(2, state.replica.sequence:position(id)) + 1 end
local function toggle(id)
  vim.api.nvim_win_set_cursor(0, { row(id) + (id == "status:context:recent-title" and 1 or 0), 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
end
local function open_commit(oid)
  toggle("status:context:recent:" .. oid)
  assert(vim.wait(1000, function() return pending ~= nil end, 5), table.concat(notices, "\n"))
  local complete = pending
  pending = nil
  if oid == first then toggle("status:context:recent:" .. oid) end
  complete()
  settle()
  if oid == first then
    assert(nodes.closed(state.replica, "status:context:recent:" .. oid), "late files reopened a collapsed commit")
    toggle("status:context:recent:" .. oid)
    settle()
  end
end
state = status.open({ workspace = vim.fn.getcwd() })
settle()
assert(#requests == 1 and #demand == 0, "collapsed history loaded eagerly")
toggle("status:context:recent-title")
settle()
assert(#requests == 1, "opening history loaded all commits")
open_commit(first)
local first_key = history.file_key(state.replica.commit[first], 1)
assert(nodes.closed(state.replica, "file:" .. first_key), "commit file did not start closed")
assert(#demand == 0, "commit expansion eagerly loaded file source")
toggle("file:" .. first_key)
settle()
assert(#demand == 1 and state.replica.file[first_key].body)
local first_body = state.replica.file[first_key].body
local first_owner = state.replica.commit[first]
local patch = { document = "body:1:1", base = 1, next = 2, base_rows = 3, next_rows = 4, base_blocks = 1, next_blocks = 2,
  text_edit = { { start_row = 3, removed_rows = 0, text = { "additional context" } } },
  block_edit = { { start_block = 1, removed_blocks = 0, inserted = { "body:4" } } },
  metadata_edit = { { block = "body:4", row_count = 1, metadata = fixture.metadata("hunk:3", 1) } }, removed_block = {} }
assert(render.apply_body(state.replica, { document = first_owner.document, file = 1, generation = 1,
  patch = patch, more = false }).kind == "Applied", table.concat(notices, "\n"))
assert(state.replica.source_entry[first_owner.prefix .. "body:4"].text[1] == "additional context")
assert(first_body.revision == 2, "incremental commit body did not advance")
open_commit(second)
local second_key = history.file_key(state.replica.commit[second], 1)
toggle("file:" .. second_key)
settle()
assert(#demand == 2 and state.replica.file[first_key].body == first_body, "another commit replaced the first diff")
assert(state.replica.file[1].body == nil, "historical body replaced working-tree body")
local first_block = state.replica.commit[first].prefix .. "body:2"
vim.api.nvim_win_set_cursor(0, { row(first_block), 0 })
status.action(state, "stage")
assert(notices[#notices] == "Historical commit diffs are read-only")
notices = {}
status.action(state, "open")
settle()
assert(notices[1] == "Historical source selected", "historical effect was discarded")
notices = {}
local selection_ok = pcall(render.selection, state.replica, row("file:1") - 1, row(first_block))
assert(not selection_ok, "mixed selection admitted historic mutation")
assert(render.apply_snapshot(state.replica, state.replica.inventory).kind == "Applied")
assert(state.replica.file[first_key].body == first_body)
assert(not nodes.closed(state.replica, "file:" .. first_key), "refresh closed expanded diff")
local updated_record = vim.deepcopy(first_owner.snapshot.file[1])
updated_record.generation = 2
local updated_body = fixture.body(first_owner.document, { "syntax-enriched historical diff" })
updated_body.generation, updated_body.snapshot.document = 2, "body:1:2"
local revision = state.replica.revision
status.apply_update(state, { document = first_owner.document, operation_id = 0, phase = "context",
  delta = { document = first_owner.document, base = 0, next = 1, removed = {}, section = {}, file = { updated_record }, pending = {} },
  body = { updated_body } })
settle()
assert(first_owner.snapshot.revision == 1 and state.replica.revision == revision, "child context update changed root revision")
assert(state.replica.file[first_key].record.generation == 2 and state.replica.file[first_key].body)
assert(not nodes.closed(state.replica, "file:" .. first_key), "context update collapsed historical file")
vim.api.nvim_win_set_cursor(0, { row("file:" .. first_key), 0 })
local captured = assert(render.capture(state.replica, state.view[vim.api.nvim_get_current_win()], "open"))
assert(captured.document == first_owner.document and captured.revision == 1 and captured.location.id == 1)
open_commit(empty)
assert(state.replica.source_entry["status:context:recent:" .. empty .. ":placeholder"].text[1] == "No changed files")
toggle("status:context:recent:" .. failing)
settle()
assert(#notices == 1 and notices[1] == "comparison fixture failure")
assert(state.replica.source_entry["status:context:recent:" .. failing .. ":placeholder"].text[1]:find("Unable to load commit", 1, true))
local request_count = #requests
status.demand(state)
settle()
assert(#requests == request_count, "failed comparison retried without user action")
toggle("status:context:recent:" .. failing)
toggle("status:context:recent:" .. failing)
settle()
assert(#notices == 2, "reopening failed comparison did not retry")
notices = {}
local old_document = state.replica.commit[second].document
local snapshot = vim.deepcopy(state.replica.inventory)
table.remove(snapshot.context.recent, 2)
assert(render.apply_snapshot(state.replica, snapshot).kind == "Applied")
status.demand(state)
settle()
assert(closed[old_document] and not state.replica.commit[second], "removed recent commit retained native document")
assert(render.apply_body(state.replica, fixture.body(old_document, { "stale" })).kind == "Discarded")
local remaining = state.replica.commit[first].document
status.close(state)
assert(vim.wait(2000, function() return not state.request_active end, 5))
assert(closed[remaining] and closed[state.document], "status close leaked comparison")
assert(#notices == 0, table.concat(notices, "\n"))
state = status.open({ workspace = vim.fn.getcwd() })
settle()
toggle("status:context:recent-title")
toggle("status:context:recent:" .. first)
assert(vim.wait(1000, function() return pending ~= nil end, 5))
local pending_document = state.replica.commit[first].document
status.close(state)
pending()
assert(vim.wait(2000, function() return not state.request_active end, 5))
assert(closed[pending_document], "closing during comparison open leaked native document")
assert(#notices == 0, table.concat(notices, "\n"))
status._set_runner_for_test(nil)
print("status recent commits: passed")
  end },
  { name = "status_recent_commits", run = function()
vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data_directory = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data_directory, "p")
local executable = vim.g.forge_recent_executable or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host first")
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data_directory or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
package.loaded["forge.views.status.status_context"] = { attach = function()
  return { refresh = function() end, close = function() end }
end, producer_handlers = function() return {} end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local status, history = require("forge.status"), require("forge.status_history")
local state, notices = nil, {}
local request_host = client.request
local demand_count = 0
client.request = function(request_specification, callback, progress)
  local method = request_specification.method
  local params = request_specification.params or {}

  if method == "repository.status.document" and params.operation == "demand" then
    demand_count = demand_count + 1
    if demand_count > 64 then
      callback(nil, "Repeated demand: " .. vim.inspect(params))
      return
    end
  end
  return request_host({ method = method, params = params }, callback, progress)
end
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
local function settle()
  assert(vim.wait(10000, function()
    return #notices > 0 or state.ready and not state.pending and not state.scheduled
      and not state.request_active and next(client._client.pending) == nil
  end, 10), "native commit expansion timed out")
  assert(#notices == 0, table.concat(notices, "\n"))
end
local function row(id) return select(2, state.replica.sequence:position(id)) + 1 end
local function toggle(id)
  vim.api.nvim_win_set_cursor(0, { row(id) + (id == "status:context:recent-title" and 1 or 0), 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  settle()
end
local function open_commit(oid)
  toggle("status:context:recent:" .. oid)
  return assert(state.replica.commit[oid], "commit was not loaded")
end
local latest
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "local value = 1", "return value" }, workspace .. "/first.lua")
  vim.fn.writefile({ "initial" }, workspace .. "/second.txt")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "Initial files" })
  local initial = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "local value = 2", "return value" }, workspace .. "/first.lua")
  git({ "mv", "second.txt", "renamed.txt" })
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "Update value and rename file" })
  latest = git({ "rev-parse", "HEAD" })
  git({ "commit", "--quiet", "--allow-empty", "-m", "Empty commit" })
  local empty = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "local value = 3", "return value" }, workspace .. "/first.lua")
  state = status.open({ workspace = workspace })
  settle()
  assert(next(state.replica.commit) == nil)
  toggle("status:context:recent-title")
  local owner = open_commit(latest)
  assert(#owner.snapshot.file == 2, "commit inventory differs from Git")
  local selected
  for _, file in ipairs(owner.snapshot.file) do
    local key = history.file_key(owner, file.id)
    assert(state.replica.file[key].body == nil, "collapsed file loaded eagerly")
    if file.path == "first.lua" then selected = key end
  end
  assert(selected)
  toggle("file:" .. selected)
  local body = assert(state.replica.file[selected].body)
  local text = {}
  for index = 0, body.sequence:count() - 1 do vim.list_extend(text, body.sequence:at(index).entry.text) end
  local rendered = table.concat(text, "\n")
  assert(rendered:find("value = 1", 1, true) and rendered:find("value = 2", 1, true), rendered)
  assert(not rendered:find("value = 3", 1, true), "commit diff read the working tree")
  assert(next(body.fold.record), "shared diff hunk folds missing")
  vim.api.nvim_win_set_cursor(0, { row("file:" .. selected), 0 })
  status.navigate(state, true)
  settle()
  local cursor = vim.api.nvim_win_get_cursor(0)
  assert(cursor[1] > row("file:" .. selected), "native hunk navigation did not enter commit diff")
  status.refresh(state)
  settle()
  assert(state.replica.file[selected].body, "refresh lost historical diff")
  assert(not require("forge.nodes").closed(state.replica, "file:" .. selected), "refresh closed historical file")
  local root_owner = open_commit(initial)
  assert(#root_owner.snapshot.file == 2, "root commit omitted added files")
  local root_key = history.file_key(root_owner, root_owner.snapshot.file[1].id)
  toggle("file:" .. root_key)
  assert(state.replica.file[root_key].body and state.replica.file[selected].body)
  local empty_owner = open_commit(empty)
  assert(#empty_owner.snapshot.file == 0)
  assert(state.replica.source_entry["status:context:recent:" .. empty .. ":placeholder"].text[1] == "No changed files")
  vim.fn.maparg("zR", "n", false, true).callback()
  settle()
  for _, model in pairs(state.replica.file) do assert(model.body, "open-all left a file unloaded") end
  assert(#notices == 0)
end, debug.traceback)
local function cleanup()
  if state then status.close(state) end
  vim.wait(3000, function() return not state or not state.request_active end, 10)
  client.host_stop()
  vim.wait(3000, function() return client._client.process == nil end, 10)
  vim.fn.stdpath, vim.notify = original_stdpath, original_notify
  client.request = request_host
  vim.fn.chdir(root)
  vim.fn.delete(workspace, "rf")
  vim.fn.delete(data_directory, "rf")
end
if ok and vim.g.forge_recent_manual then
  vim.fn.maparg("zM", "n", false, true).callback()
  toggle("status:context:recent-title")
  vim.api.nvim_win_set_cursor(0, { row("status:context:recent:" .. latest), 0 })
  vim.api.nvim_create_autocmd("VimLeavePre", { once = true, callback = cleanup })
  return
end
cleanup()
assert(ok, failure)
assert(#notices == 0, table.concat(notices, "\n"))
print("native recent commits: passed (lazy files, historical diff, root, empty, navigation, refresh, cleanup)")
  end },
  { name = "word_wrap", run = function()
local root = vim.fn.getcwd()
dofile(root .. "/nvim/lua/forge/tests/fixtures/commit_reuse_manual.lua")
local status = require("forge.status")
local client = require("forge.client")
local state = forge_reuse.state
local notices = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then notices[#notices + 1] = message end end
local function idle()
  return state.ready and not state.request_active and next(client._client.pending) == nil
end
local text = "The default profile is `release` for development, runtime verification, and profiling."
local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
end
local success, failure = xpcall(function()
  vim.o.columns, vim.o.lines = 80, 40
  assert(vim.wait(15000, idle, 10))
  vim.fn.writefile({ text }, forge_reuse.fixture .. "/tracked.txt")
  status.refresh(state)
  assert(vim.wait(10000, idle, 10))
  local file
  for _, candidate in ipairs(state.replica.inventory.file) do if candidate.path == "tracked.txt" then file = candidate end end
  local _, first = state.replica.sequence:position("file:" .. assert(file).id)
  vim.api.nvim_win_set_cursor(0, { first + 1, 0 })
  press("<Tab>")
  local row
  assert(vim.wait(10000, function()
    for index, line in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
      if line == text then row = index break end
    end
    return row and idle()
  end, 10))
  local function audit()
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.cmd("normal! zt")
    vim.cmd.redraw()
    local previous_row, continuation_count = nil, 0
    for column, word in text:gmatch("()(%S+)") do
      local first_cell = vim.fn.screenpos(0, row, column)
      local last_cell = vim.fn.screenpos(0, row, column + #word - 1)
      assert(first_cell.row > 0 and last_cell.row > 0, "wrapped source word was clipped: " .. word)
      assert(first_cell.row == last_cell.row, "native wrapping split a word: " .. word)
      if previous_row and first_cell.row ~= previous_row then
        continuation_count = continuation_count + 1
        assert(first_cell.col == vim.api.nvim_win_get_position(0)[2] + 1, "wrapped source retained a continuation-line margin")
      end
      previous_row = last_cell.row
    end
    assert(continuation_count > 0, "fixture did not wrap")
    assert(vim.api.nvim_buf_get_lines(state.replica.buffer, row - 1, row, false)[1] == text, "wrapping changed source text")
  end
  audit()
  vim.cmd("enew")
  vim.api.nvim_set_current_buf(state.replica.buffer)
  assert(vim.wait(10000, idle, 10))
  audit()
  vim.cmd("vsplit")
  vim.cmd("vertical resize 48")
  assert(vim.api.nvim_win_get_width(0) == 48)
  audit()
  vim.cmd("close")
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
status.close(state)
client.host_stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20))
assert(success, failure)
print("expanded Status diff word wrapping and buffer return passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
