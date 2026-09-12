vim.loader.enable(false)

local commit = require("forge.integrations.commit")
local ai_commit = require("forge.integrations.ai_commit")
local git_write = require("forge.git.write")

local repository = vim.fn.tempname()
assert(vim.fn.mkdir(repository, "p") == 1)

local original_execute = git_write.execute
local original_backend = ai_commit._backend
local original_ai = package.loaded.ai
local original_adapters = package.loaded["ai.adapters"]

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
  package.loaded.ai = { resolve = function() return { provider = { name = "fixture" }, model = "fixture-model" } end }
  package.loaded["ai.adapters"] = { get = function() return { commit = "fixture-model" } end }
  ai_commit.reset_backend()
  ai_commit.set_backend({ request_async = function(params, callback)
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
  assert_true(request_list[1].params.operation == "generate" and request_list[1].params.comparison == "head",
    "cold Status draft did not generate directly")

  git_write.execute = function(workspace, action, callback, progress)
    assert_true(workspace == repository, "commit host used the wrong repository")
    assert_true(action.kind == "commit_editor", "commit host bypassed fake-editor admission")
    assert_true(type(action.command) == "string" and action.command ~= "", "commit host omitted GIT_EDITOR")
    assert_true(type(action.nvim_server) == "string" and action.nvim_server ~= "", "commit host omitted parent RPC address")
    vim.system({ "git", "-C", workspace, "commit" }, {
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
end

local ok, failure = xpcall(run, debug.traceback)
git_write.execute = original_execute
ai_commit.reset_backend()
ai_commit.set_backend(original_backend)
package.loaded.ai = original_ai
package.loaded["ai.adapters"] = original_adapters
if commit._active then commit._active.aborted = true end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_host OK")
vim.cmd("qa!")
