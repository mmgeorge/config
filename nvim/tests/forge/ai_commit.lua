vim.loader.enable(false)

local ai_commit = require("forge.integrations.ai_commit")
local original_backend = ai_commit._backend
local original_client = package.loaded["forge.client"]
local original_notify = vim.notify
local original_settings = package.loaded["forge.integrations.background_settings"]
local function generation_get_config() return { backend = { kind = "codex" } } end
package.loaded["forge.integrations.background_settings"] = {
  request = function(_, _, callback)
    callback({ settings = { auto_compaction = { enabled = true, threshold_tokens = 128000 } } })
  end,
  model = function() return { model = "fixture-model", thinking = "low", service_tier = "fast" } end,
}
local notices = {}
vim.notify = function(message, level, options)
  notices[#notices + 1] = { message = message, level = level, options = options }
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then error((message or "values differ") .. ": " .. vim.inspect(actual) .. " ~= " .. vim.inspect(expected), 2) end
end

local function assert_progress(buffer)
  assert(vim.wait(1000, function()
    return vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1]:find("Generating with fixture-model · thinking low · fast", 1, true)
  end, 10), "missing model generation progress")
end

local function run()
  local requests, generated = {}, 0
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
  assert_equal(#notices, 0, "starting generation displayed a notification")
  vim.wait(250, function() return false end)
  assert_equal(#notices, 0, "waiting for generation displayed a notification")
  local finish_draft = take("generate", "head")
  local buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
  assert_equal(#requests, 0, "pending draft performed validation or generation")
  finish_draft({ state = "ready", message = "reused draft" })
  assert_equal(#notices, 0, "completed generation displayed a notification")
  local completed_count = #notices
  vim.wait(250, function() return false end)
  assert_equal(#notices, completed_count, "completed generation timer kept notifying")
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft")
  vim.api.nvim_buf_delete(buffer, { force = true })

  buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "", "# Git comment" })
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "reused draft", "ready draft was not immediate")
  assert_equal(#requests, 0, "ready draft performed validation")
  vim.api.nvim_set_current_buf(buffer)
  vim.api.nvim_win_set_cursor(0, { 3, 4 })
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  assert_equal(requests[1].params.ignored_paths[1], nil, "staged regeneration excluded a staged path")
  local first = take("generate", "staged")
  assert_progress(buffer)
  local progress_line = vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1]
  assert(vim.wait(1000, function() return vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1] ~= progress_line end, 10),
    "generation spinner did not animate")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), { 3, 4 }), "spinner moved the cursor")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  local second = take("generate", "staged")
  first({ state = "ready", message = "obsolete request" })
  assert_progress(buffer)
  second({ state = "ready", message = "staged subject\n\nstaged body" })
  local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  assert_equal(lines[1], "staged subject")
  assert_equal(lines[#lines], "# Git comment", "regeneration removed Git comments")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  local edited = take("generate", "staged")
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "user edited while generating" })
  edited({ state = "ready", message = "late replacement" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "user edited while generating")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "edited before provider failure" })
  take("generate", "staged")(nil, "provider failed")
  assert_equal(notices[#notices].message, "Commit generation failed: provider failed")
  assert_equal(notices[#notices].level, vim.log.levels.ERROR)
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "edited before provider failure")
  local error_count = #notices
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  take("generate", "staged")({ state = "none" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "edited before provider failure", "empty result retained progress")
  assert_equal(#notices, error_count, "empty generation displayed a notification")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  local failed = take("generate", "staged")
  assert_progress(buffer)
  failed(nil, "generation failed")
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "edited before provider failure", "failure retained progress")
  local restored_tick = vim.api.nvim_buf_get_changedtick(buffer)
  vim.wait(150, function() return false end)
  assert_equal(vim.api.nvim_buf_get_changedtick(buffer), restored_tick, "failed generation kept animating")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  local abandoned = take("generate", "staged")
  assert_progress(buffer)
  vim.api.nvim_exec_autocmds("BufWritePre", { buffer = buffer })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "edited before provider failure", "write retained progress")
  abandoned({ state = "ready", message = "must not replace saved message" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "edited before provider failure")
  ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
  local closed = take("generate", "staged")
  vim.api.nvim_buf_delete(buffer, { force = true })
  closed({ state = "ready", message = "closed" })

  buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status")
  assert_equal(#requests, 0, "opening without Status silently generated a message")
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status", true)
  take("generate", "staged")({ state = "ready", message = "explicit draft" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "explicit draft")
  vim.api.nvim_buf_delete(buffer, { force = true })

  ai_commit.ensure("D:/pending", { ref = "HEAD" })
  local pending = take("generate", "head")
  buffer = vim.api.nvim_create_buf(false, true)
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending")
  ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending", true)
  local explicit = take("generate", "staged")
  explicit({ state = "ready", message = "explicit wins" })
  pending({ state = "ready", message = "late About" })
  assert_equal(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1], "explicit wins")
  vim.api.nvim_buf_delete(buffer, { force = true })
  ai_commit.ensure(cwd, { ref = "HEAD", force = true })
  local obsolete = take("generate", "head")
  local invalidated
  ai_commit.ensure(cwd, { ref = "HEAD" }, function(state) invalidated = state end)
  local cancelled_count = #notices
  ai_commit.clear(cwd)
  assert_equal(invalidated.state, "none", "pending About waiter was not released")
  assert_equal(#notices, cancelled_count, "cancelled generation displayed a notification")
  vim.wait(150, function() return false end)
  assert_equal(#notices, cancelled_count, "cancelled generation timer kept notifying")
  assert_equal(ai_commit.state(cwd, "HEAD"), nil, "cleared About remained reusable")
  obsolete({ state = "ready", message = "late consumed draft" })
  assert_equal(ai_commit.state(cwd, "HEAD"), nil, "late completion restored a consumed draft")
  ai_commit.ensure(cwd, { ref = "HEAD", force = true })
  take("generate", "head")({ state = "ready", message = "new draft" })
  assert_equal(ai_commit.state(cwd, "HEAD").message, "new draft")
  ai_commit.ensure("D:/another-workspace", { ref = "HEAD" })
  take("generate", "head")({ state = "ready", message = "other draft" })
  ai_commit.clear(cwd)
  assert_equal(ai_commit.state(cwd, "HEAD"), nil)
  assert_equal(ai_commit.state().message, "other draft", "clearing one workspace removed another draft")
  assert_equal(#requests, 0)

  ai_commit.ensure(cwd, { ref = "HEAD" })
  take("generate", "head")({ state = "ready", message = "new draft" })
  for _, result in ipairs({ {}, { state = "ready", message = "  " }, { state = "error", error = "upstream rejection" }, "invalid", true }) do
    ai_commit.ensure(cwd, { ref = "HEAD", force = true })
    take("generate", "head")(result)
    assert_equal(ai_commit.state(cwd, "HEAD").state, "error", "invalid response was accepted")
    assert_equal(ai_commit.state(cwd, "HEAD").message, "new draft", "failure erased the previous draft")
    assert_equal(notices[#notices].level, vim.log.levels.ERROR, "invalid response failed silently")
  end
  local failed_count = #notices
  vim.wait(150, function() return false end)
  assert_equal(#notices, failed_count, "failed generation timer kept notifying")
  ai_commit.set_backend({ request_async = function() error("request startup failed") end })
  ai_commit.ensure(cwd, { ref = "HEAD", force = true })
  assert(ai_commit.state(cwd, "HEAD").error:find("request startup failed", 1, true))
  assert_equal(notices[#notices].level, vim.log.levels.ERROR)

  ai_commit.reset_backend()
  package.loaded["forge.client"] = {
    request = function() error("Commit generation initialized a conversation") end,
    generation_get_config = generation_get_config,
    generation_request = function(params, callback, on_model)
      assert_equal(params.model, nil, "fallback hardcoded a provider or model")
      on_model("selected-model (codex)")
      callback({ state = "ready", message = "fix: use recent Harness\n\nPreserve the body." })
    end,
  }
  ai_commit.ensure("D:/harness-fallback", { ref = "HEAD" })
  assert_equal(ai_commit.state("D:/harness-fallback", "HEAD").message, "fix: use recent Harness\n\nPreserve the body.")
  assert_equal(ai_commit.state("D:/harness-fallback", "HEAD").model, "selected-model (codex) · fast")
  package.loaded["forge.client"].generation_request = function(_, callback)
    callback(nil, "claude commit_message generation failed: Unsupported provider capability: text_generation")
  end
  ai_commit.ensure("D:/unsupported-harness", { ref = "HEAD" })
  assert(ai_commit.state("D:/unsupported-harness", "HEAD").error:find("text_generation", 1, true))
  for _, notice in ipairs(notices) do
    assert_equal(notice.level, vim.log.levels.ERROR, "generation displayed a non-error notification")
  end
end

local ok, failure = xpcall(run, debug.traceback)
ai_commit.reset_backend()
ai_commit.set_backend(original_backend)
package.loaded["forge.client"] = original_client
package.loaded["forge.integrations.background_settings"] = original_settings
vim.notify = original_notify
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("ai_commit OK")
vim.cmd("qa!")
