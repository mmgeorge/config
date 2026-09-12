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
  vim.fn.writefile({ string.rep("hook output ", 9000) }, repository .. "/.git/forge-hook-output")
  vim.fn.writefile({ "#!/bin/sh", "cat .git/forge-hook-output >&2" }, repository .. "/.git/hooks/pre-commit")
  assert_true(vim.uv.fs_chmod(repository .. "/.git/hooks/pre-commit", 493), "pre-commit hook could not become executable")

  ai_commit.ensure(repository, { ref = "HEAD" })
  assert_true(request_list[1].params.operation == "generate" and request_list[1].params.comparison == "head",
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
ai_commit.reset_backend()
ai_commit.set_backend(original_backend)
package.loaded.ai = original_ai
package.loaded["ai.adapters"] = original_adapters
if commit._active then commit._active.aborted = true end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_large_output_host OK")
vim.cmd("qa!")
