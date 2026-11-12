-- Unit tests for `forge.integrations.ai_commit` (ai_commit.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local ai_commit = package.loaded["forge.integrations.ai_commit"]
      if ai_commit then ai_commit.reset_provider() end
    end,
  }),
})

T["ai_commit"] = function()
  local ai_commit = require("forge.integrations.ai_commit")
  local function generation_get_config() return { provider = { kind = "fixture_alpha" } } end
  package.loaded["forge.client"] = { generation_get_config = generation_get_config }
  package.loaded["forge.integrations.commit_settings"] = {
    request = function(_, _, callback)
      callback({ settings = { auto_compaction = { enabled = true, threshold_tokens = 128000 } } })
    end,
    selection = function(snapshot)
      local choice = { model = "fixture-model", thinking = "low", service_tier = "fast" }
      return { provider = "fixture_beta", model = choice.model, reasoning_effort = choice.thinking,
        service_tier = choice.service_tier, auto_compaction = snapshot.settings.auto_compaction }, choice
    end,
  }
  local notices = notify.capture()

  local function assert_progress(buffer)
    wait_until(function()
      return buffer_support.line(buffer, 1):find("Generating with fixture-model · thinking low · fast", 1, true)
    end, "missing model generation progress")
  end

  local function run()
    local requests, generated = {}, 0
    ai_commit.reset_provider()
    ai_commit.set_provider({ request_async = function(params, callback)
      if params.operation == "generate" then generated = generated + 1 end
      local request = { params = params, callback = callback, cancelled = false }
      requests[#requests + 1] = request
      return function() request.cancelled = true end
    end })
    local function take(operation, comparison)
      local request = table.remove(requests, 1)
      assert(request, "missing " .. operation)
      eq(request.params.operation, operation)
      eq(request.params.comparison, comparison)
      return request.callback, request
    end
    local release_first = ai_commit.ensure("D:/consumer-fixture", {}, function() end)
    local release_second = ai_commit.ensure("D:/consumer-fixture", {}, function() end)
    local shared = table.remove(requests, 1)
    release_first()
    eq(shared.cancelled, false)
    release_second()
    eq(shared.cancelled, true)
    eq(ai_commit.state("D:/consumer-fixture", "HEAD"), nil)
    generated = 0
    local cwd = "D:/ai-commit-fixture"
    ai_commit.ensure(cwd, { ref = "HEAD", ignored_paths = { "excluded.txt" } })
    eq(requests[1].params.ignored_paths[1], "excluded.txt")
    eq(#notices, 0, { fail_reason = "starting generation displayed a notification" })
    vim.wait(250, function() return false end, 5)
    eq(#notices, 0, { fail_reason = "waiting for generation displayed a notification" })
    local finish_draft = take("generate", "status")
    local buffer = buffer_support.scratch()
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
    eq(#requests, 0, { fail_reason = "pending draft performed validation or generation" })
    finish_draft({ state = "ready", message = "reused draft" })
    eq(#notices, 0, { fail_reason = "completed generation displayed a notification" })
    local completed_count = #notices
    vim.wait(250, function() return false end, 5)
    eq(#notices, completed_count, { fail_reason = "completed generation timer kept notifying" })
    eq(buffer_support.line(buffer, 1), "reused draft")
    vim.api.nvim_buf_delete(buffer, { force = true })

    local source = { head = "fixture-head", file = { { id = 1, generation = 1, section = "staged" } } }
    local function open_context(document)
      return require("forge.views.status.status_context").attach({
        document_id = document, workspace = cwd,
        is_alive = function() return true end,
        capture_input = function() error("unexpected input") end,
        is_input_current = function() return true end,
        get_about_source = function() return vim.deepcopy(source) end,
        present = function() end,
      })
    end
    local context = open_context("first-status")
    context.generate_about()
    eq(#requests, 0, { fail_reason = "opening Status bypassed the cached draft" })
    eq(context.about.message, "reused draft")
    context.close()
    source.file[#source.file + 1] = { id = 2, generation = 1, section = "staged" }
    context = open_context("reopened-status")
    context.generate_about()
    eq(#requests, 0, { fail_reason = "reopening Status regenerated after staging another file" })
    eq(context.about.message, "reused draft")
    local replica = require("forge.buffer").open("about-command")
    replica.status = "Applied"
    local command_owner = require("forge.document_commands").attach(replica, {
      view = "status",
      handler = require("forge.views.status.status_context").producer_handlers({
        workspace = cwd, is_alive = function() return true end, context = function() return context end,
      }),
    })
    local mapping = keymap.get("<C-a>", { buffer = replica.buffer })
    eq(type(mapping.callback), "function", { fail_reason = "Status has no manual About regeneration mapping" })
    local regenerate_about = mapping.callback
    regenerate_about()
    local superseded_about, superseded_request = take("generate", "status")
    local waiting_commit = buffer_support.scratch()
    ai_commit.populate_commit_buffer_when_ready(waiting_commit, cwd)
    regenerate_about()
    assert(superseded_request.cancelled, "Status regeneration did not cancel its previous request")
    superseded_about({ state = "ready", message = "obsolete About" })
    eq(context.about.state, "generating", { fail_reason = "superseded About replaced the active generation" })
    assert(buffer_support.line(waiting_commit, 1) ~= "obsolete About",
      "superseded About reached the waiting commit editor")
    take("generate", "status")({ state = "ready", message = "reused draft" })
    eq(context.about.state, "ready", { fail_reason = "explicit Status generation did not complete" })
    eq(buffer_support.line(waiting_commit, 1), "reused draft", { fail_reason = "waiting commit editor did not receive the replacement About" })
    vim.api.nvim_buf_delete(waiting_commit, { force = true })
    assert(vim.iter(command_owner.binding):any(function(binding)
      return binding.spec.id == "regenerate_about" and binding.keys[1] == "<C-a>"
    end), "About regeneration is absent from Status help bindings")
    command_owner.close()
    require("forge.buffer").close(replica)
    context.close()

    buffer = buffer_support.scratch({ "", "# Git comment" })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd)
    eq(buffer_support.line(buffer, 1), "reused draft", { fail_reason = "ready draft was not immediate" })
    eq(#requests, 0, { fail_reason = "ready draft performed validation" })
    vim.api.nvim_set_current_buf(buffer)
    vim.api.nvim_win_set_cursor(0, { 3, 4 })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    eq(requests[1].params.ignored_paths[1], nil, { fail_reason = "staged regeneration excluded a staged path" })
    local first, first_request = take("generate", "staged")
    assert_progress(buffer)
    local progress_line = buffer_support.line(buffer, 1)
    wait_until(function() return buffer_support.line(buffer, 1) ~= progress_line end, "generation spinner did not animate")
    eq(vim.api.nvim_win_get_cursor(0), { 3, 4 }, { fail_reason = "spinner moved the cursor" })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    assert(first_request.cancelled, "regeneration did not cancel the preceding staged request")
    local second = take("generate", "staged")
    first({ state = "ready", message = "obsolete request" })
    assert_progress(buffer)
    second({ state = "ready", message = "staged subject\n\nstaged body" })
    local lines = buffer_support.lines(buffer)
    eq(lines[1], "staged subject")
    eq(lines[#lines], "# Git comment", { fail_reason = "regeneration removed Git comments" })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    local edited = take("generate", "staged")
    vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "user edited while generating" })
    edited({ state = "ready", message = "late replacement" })
    eq(buffer_support.line(buffer, 1), "user edited while generating")
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "edited before provider failure" })
    take("generate", "staged")(nil, "provider failed")
    eq(notices[#notices].message, "Commit generation failed: provider failed")
    eq(notices[#notices].level, vim.log.levels.ERROR)
    eq(buffer_support.line(buffer, 1), "edited before provider failure")
    local error_count = #notices
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    take("generate", "staged")({ state = "none" })
    eq(buffer_support.line(buffer, 1), "edited before provider failure", { fail_reason = "empty result retained progress" })
    eq(#notices, error_count, { fail_reason = "empty generation displayed a notification" })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    local failed = take("generate", "staged")
    assert_progress(buffer)
    failed(nil, "generation failed")
    eq(buffer_support.line(buffer, 1), "edited before provider failure", { fail_reason = "failure retained progress" })
    local restored_tick = vim.api.nvim_buf_get_changedtick(buffer)
    vim.wait(150, function() return false end, 5)
    eq(vim.api.nvim_buf_get_changedtick(buffer), restored_tick, { fail_reason = "failed generation kept animating" })
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    local abandoned = take("generate", "staged")
    assert_progress(buffer)
    vim.api.nvim_exec_autocmds("BufWritePre", { buffer = buffer })
    eq(buffer_support.line(buffer, 1), "edited before provider failure", { fail_reason = "write retained progress" })
    abandoned({ state = "ready", message = "must not replace saved message" })
    eq(buffer_support.line(buffer, 1), "edited before provider failure")
    ai_commit.populate_commit_buffer_when_ready(buffer, cwd, true)
    local closed = take("generate", "staged")
    vim.api.nvim_buf_delete(buffer, { force = true })
    closed({ state = "ready", message = "closed" })

    buffer = buffer_support.scratch()
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status")
    eq(#requests, 0, { fail_reason = "opening without Status silently generated a message" })
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/without-status", true)
    take("generate", "staged")({ state = "ready", message = "explicit draft" })
    eq(buffer_support.line(buffer, 1), "explicit draft")
    vim.api.nvim_buf_delete(buffer, { force = true })

    ai_commit.ensure("D:/pending", { ref = "HEAD" })
    local pending, pending_request = take("generate", "status")
    buffer = buffer_support.scratch()
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending")
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/pending", true)
    assert(pending_request.cancelled, "regeneration did not cancel the pending About request")
    local explicit = take("generate", "staged")
    explicit({ state = "ready", message = "explicit wins" })
    pending({ state = "ready", message = "late About" })
    eq(buffer_support.line(buffer, 1), "explicit wins")
    vim.api.nvim_buf_delete(buffer, { force = true })

    for _, comparison in ipairs({ "status", "staged", "latest_commit" }) do
      local workspace = "D:/manual-" .. comparison
      local subject = comparison == "status" and "" or "existing subject"
      buffer = buffer_support.scratch({ subject, "", "existing body", "# Git comment" })
      local finish, request
      if comparison == "status" then
        ai_commit.ensure(workspace, { ref = "HEAD" })
        finish, request = take("generate", comparison)
        ai_commit.populate_commit_buffer_when_ready(buffer, workspace)
      else
        ai_commit.populate_commit_buffer_when_ready(buffer, workspace, true,
          comparison == "latest_commit" and "latest_commit" or nil)
        finish, request = take("generate", comparison)
      end
      assert_progress(buffer)
      local unrelated = buffer_support.scratch()
      vim.api.nvim_exec_autocmds("InsertEnter", { buffer = unrelated })
      assert(not request.cancelled, "editing another buffer cancelled commit generation")
      vim.api.nvim_buf_delete(unrelated, { force = true })
      vim.api.nvim_set_current_buf(buffer)
      eq(vim.fn.maparg("i", "n"), "", { fail_reason = "fixture requires the native Insert-mode command" })
      keymap.feed("i<Esc>")
      assert(request.cancelled, "InsertEnter did not cancel " .. comparison .. " generation")
      eq(buffer_support.line(buffer, 1), subject, { fail_reason = "InsertEnter retained the spinner" })
      eq(buffer_support.line(buffer, 3), "existing body")
      local manual = { "manual subject", "", "manual body", "# Git comment" }
      buffer_support.set(buffer, manual)
      local notice_count = #notices
      finish({ state = "ready", message = "obsolete generated text" })
      eq(buffer_support.lines(buffer), manual, { fail_reason = "late result replaced manual input" })
      eq(#notices, notice_count, { fail_reason = "manual cancellation reported a generation failure" })
      vim.api.nvim_buf_delete(buffer, { force = true })
    end

    local settings = package.loaded["forge.integrations.commit_settings"]
    local original_request, resolution = settings.request, {}
    settings.request = function(_, _, callback) resolution[#resolution + 1] = callback end
    ai_commit.ensure("D:/delayed-settings", { ref = "HEAD" })
    buffer = buffer_support.scratch()
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/delayed-settings")
    ai_commit.populate_commit_buffer_when_ready(buffer, "D:/delayed-settings", true)
    original_request(nil, nil, resolution[1])
    eq(#requests, 0, { fail_reason = "cancelled settings resolution started a provider request" })
    original_request(nil, nil, resolution[2])
    take("generate", "staged")({ state = "ready", message = "resolved replacement" })
    eq(buffer_support.line(buffer, 1), "resolved replacement")
    settings.request = original_request
    vim.api.nvim_buf_delete(buffer, { force = true })
    ai_commit.ensure(cwd, { ref = "HEAD", force = true })
    local obsolete = take("generate", "status")
    local invalidated
    ai_commit.ensure(cwd, { ref = "HEAD" }, function(state) invalidated = state end)
    local cancelled_count = #notices
    ai_commit.clear(cwd)
    eq(invalidated.state, "none", { fail_reason = "pending About waiter was not released" })
    eq(#notices, cancelled_count, { fail_reason = "cancelled generation displayed a notification" })
    vim.wait(150, function() return false end, 5)
    eq(#notices, cancelled_count, { fail_reason = "cancelled generation timer kept notifying" })
    eq(ai_commit.state(cwd, "HEAD"), nil, { fail_reason = "cleared About remained reusable" })
    obsolete({ state = "ready", message = "late consumed draft" })
    eq(ai_commit.state(cwd, "HEAD"), nil, { fail_reason = "late completion restored a consumed draft" })
    ai_commit.ensure(cwd, { ref = "HEAD", force = true })
    take("generate", "status")({ state = "ready", message = "new draft" })
    eq(ai_commit.state(cwd, "HEAD").message, "new draft")
    ai_commit.ensure("D:/another-workspace", { ref = "HEAD" })
    take("generate", "status")({ state = "ready", message = "other draft" })
    ai_commit.clear(cwd)
    eq(ai_commit.state(cwd, "HEAD"), nil)
    eq(ai_commit.state().message, "other draft", { fail_reason = "clearing one workspace removed another draft" })
    eq(#requests, 0)

    ai_commit.ensure(cwd, { ref = "HEAD" })
    take("generate", "status")({ state = "ready", message = "new draft" })
    for _, result in ipairs({ {}, { state = "ready", message = "  " }, { state = "error", error = "upstream rejection" }, "invalid", true }) do
      ai_commit.ensure(cwd, { ref = "HEAD", force = true })
      take("generate", "status")(result)
      eq(ai_commit.state(cwd, "HEAD").state, "error", { fail_reason = "invalid response was accepted" })
      eq(ai_commit.state(cwd, "HEAD").message, "new draft", { fail_reason = "failure erased the previous draft" })
      eq(notices[#notices].level, vim.log.levels.ERROR, { fail_reason = "invalid response failed silently" })
    end
    local failed_count = #notices
    vim.wait(150, function() return false end, 5)
    eq(#notices, failed_count, { fail_reason = "failed generation timer kept notifying" })
    ai_commit.set_provider({ request_async = function() error("request startup failed") end })
    ai_commit.ensure(cwd, { ref = "HEAD", force = true })
    assert(ai_commit.state(cwd, "HEAD").error:find("request startup failed", 1, true))
    eq(notices[#notices].level, vim.log.levels.ERROR)

    ai_commit.reset_provider()
    package.loaded["forge.client"] = {
      request = function() error("Commit generation initialized a conversation") end,
      generation_get_config = generation_get_config,
      generation_request = function(params, callback, on_model, selection)
        eq(params.model, nil, { fail_reason = "fallback hardcoded a provider or model" })
        eq(selection.provider, "fixture_beta", { fail_reason = "generation ignored the saved Background provider" })
        on_model("selected-model (fixture_alpha)")
        callback({ state = "ready", message = "fix: use recent Harness\n\nPreserve the body." })
      end,
    }
    ai_commit.ensure("D:/harness-fallback", { ref = "HEAD" })
    eq(ai_commit.state("D:/harness-fallback", "HEAD").message, "fix: use recent Harness\n\nPreserve the body.")
    eq(ai_commit.state("D:/harness-fallback", "HEAD").model, "selected-model (fixture_alpha) · fast")
    package.loaded["forge.client"].generation_request = function(_, callback)
      callback(nil, "fixture_gamma commit_message generation failed: Unsupported provider capability: text_generation")
    end
    ai_commit.ensure("D:/unsupported-harness", { ref = "HEAD" })
    assert(ai_commit.state("D:/unsupported-harness", "HEAD").error:find("text_generation", 1, true))
    for _, notice in ipairs(notices) do
      eq(notice.level, vim.log.levels.ERROR, { fail_reason = "generation displayed a non-error notification" })
    end
  end

  run()
end

return T
