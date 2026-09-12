vim.loader.enable(false)

local ai_commit = require("forge.integrations.ai_commit")
local original_ai = package.loaded.ai
local original_adapters = package.loaded["ai.adapters"]
local original_backend = ai_commit._backend

local function assert_equal(actual, expected, message)
  if actual ~= expected then error((message or "values differ") .. ": " .. vim.inspect(actual) .. " ~= " .. vim.inspect(expected), 2) end
end

local function run()
  local requests, generated = {}, 0
  package.loaded.ai = { resolve = function() return { provider = { name = "fixture" }, model = "fixture-model" } end }
  package.loaded["ai.adapters"] = { get = function() return { commit = "fixture-model" } end }
  ai_commit.reset_backend()
  ai_commit.set_backend({ request_async = function(params, callback)
    if params.operation == "generate" then generated = generated + 1 end
    requests[#requests + 1] = { params = params, callback = callback }
  end })
  local function take(operation, comparison)
    local request = table.remove(requests, 1)
    assert(request, "missing " .. operation)
    assert_equal(request.params.operation, operation)
    assert_equal(request.params.comparison, comparison)
    return request.callback
  end
  local cwd = "D:/ai-commit-fixture"
  ai_commit.ensure(cwd, { ref = "HEAD", ignored_paths = { "excluded.txt" } })
  assert_equal(requests[1].params.ignored_paths[1], "excluded.txt")
  local finish_draft = take("generate", "head")
  local buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
  assert_equal(#requests, 0, "pending draft performed validation or generation")
  finish_draft({ state = "ready", message = "reused draft" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft")
  vim.api.nvim_buf_delete(buffer, { force = true })

  buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "", "# Git comment" })
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft", "ready draft was not immediate")
  assert_equal(#requests, 0, "ready draft performed validation")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, nil, true)
  assert_equal(requests[1].params.ignored_paths[1], nil, "staged regeneration excluded a staged path")
  local first = take("generate", "staged")
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft", "pending generation cleared draft")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, nil, true)
  local second = take("generate", "staged")
  first({ state = "ready", message = "obsolete request" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft")
  second({ state = "ready", message = "staged subject\n\nstaged body" })
  local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  assert_equal(lines[1], "staged subject")
  assert_equal(lines[#lines], "# Git comment", "regeneration removed Git comments")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, nil, true)
  local edited = take("generate", "staged")
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "user edited while generating" })
  edited({ state = "ready", message = "late replacement" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "user edited while generating")
  local notices = {}
  local function notify(message) notices[#notices + 1] = message end
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, notify, true)
  take("generate", "staged")(nil, "provider failed")
  assert_equal(notices[#notices], "provider failed")
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "user edited while generating")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, notify, true)
  take("generate", "staged")({ state = "none" })
  assert_equal(notices[#notices], "No staged changes to describe")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, nil, true)
  local closed = take("generate", "staged")
  vim.api.nvim_buf_delete(buffer, { force = true })
  closed({ state = "ready", message = "closed" })

  buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status")
  assert_equal(#requests, 0, "opening without Status silently generated a message")
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status", nil, true)
  take("generate", "staged")({ state = "ready", message = "explicit draft" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "explicit draft")
  vim.api.nvim_buf_delete(buffer, { force = true })

  ai_commit.ensure("D:/pending", { ref = "HEAD" })
  local pending = take("generate", "head")
  buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending")
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending", nil, true)
  local explicit = take("generate", "staged")
  explicit({ state = "ready", message = "explicit wins" })
  pending({ state = "ready", message = "late About" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "explicit wins")
  vim.api.nvim_buf_delete(buffer, { force = true })
  assert_equal(#requests, 0)
end

local ok, failure = xpcall(run, debug.traceback)
ai_commit.reset_backend()
ai_commit.set_backend(original_backend)
package.loaded.ai = original_ai
package.loaded["ai.adapters"] = original_adapters
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("ai_commit OK")
vim.cmd("qa!")
