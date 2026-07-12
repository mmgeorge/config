vim.loader.enable(false)

local diff_review = require("diff_review")
local builder = require("diff_review.harness.builder")
local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local transcript_renderer = require("diff_review.render.harness.transcript")
local session = require("diff_review.session")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2) end
end

local function has_buffer_keymap(buf, mode, lhs)
  for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(buf, mode)) do
    if keymap.lhs == lhs then return true end
  end
  return false
end

local function line_number(line_list, expected)
  for index, line in ipairs(line_list) do
    if line == expected then return index end
  end
  return nil
end

local test_root = vim.fn.tempname()
local crate_root = vim.fs.joinpath(test_root, "crate")
local plan_path = vim.fs.joinpath(test_root, "working.md")
vim.fn.mkdir(vim.fs.joinpath(crate_root, "src"), "p")
vim.fn.writefile({ "[package]", 'name = "diff-review-harness"', 'version = "0.1.0"' }, vim.fs.joinpath(crate_root, "Cargo.toml"))
vim.fn.writefile({ "fn main() {}" }, vim.fs.joinpath(crate_root, "src", "main.rs"))
vim.fn.writefile({ "# Plan", "", "1. Review the implementation." }, plan_path)

local active_session = {
  id = "session-one",
  name = "Harness test",
  workspace = vim.fn.getcwd(),
  backend = "mock",
  backend_session_id = "provider-one",
  model = "mock-model",
  provider_label = "Mock CLI",
  resolved_model = "mock-model-resolved",
  effort = "low",
  trust_profile = "workspace",
  write_mode = "read",
  native_fork = false,
}

local interaction = {
  id = "interaction-one",
  session_id = active_session.id,
  ordinal = 1,
  prompt = "Change the greeting",
  state = "complete",
  checkpoint_before = "before",
  checkpoint_after = "after",
  diff_text = table.concat({
    "diff --git a/demo.lua b/demo.lua",
    "--- a/demo.lua",
    "+++ b/demo.lua",
    "@@ -1,1 +1,1 @@",
    "-return 'before'",
    "+return 'after'",
  }, "\n"),
  comment = {
    {
      id = "comment-one",
      interaction_id = "interaction-one",
      file_path = "demo.lua",
      new_line = 1,
      body = "Persisted review note",
      created_at_ms = 1,
    },
  },
}

local request_by_method = {}
local goal_continue_count = 0

local function emit(options, message)
  vim.schedule(function() options.stdout(nil, vim.json.encode(message) .. "\n") end)
end

local function fake_launcher(_, options, _)
  local process = {}
  function process:write(payload)
    local request = vim.json.decode(vim.trim(payload))
    request_by_method[request.method] = request
    if request.method == "initialize" then
      vim.defer_fn(function()
        emit(options, { id = request.id, result = {
          session = active_session,
          transcript = {},
          capability = { native_fork = false, model_selection = true },
          no_checkpoint = false,
        } })
      end, 80)
    elseif request.method == "prompt.submit" then
      emit(options, { event = "backend_event", payload = { kind = "user_message", text = request.params.text } })
      if request.params.text == "/goal fail" then
        emit(options, { id = request.id, error = { message = "synthetic goal failure" } })
        return
      end
      if request.params.text == "hello harness" then
        local output = table.concat({ "# Rendering", "line two", "line three", "line four", "last line" }, "\n")
        emit(options, { event = "backend_event", payload = {
          kind = "tool",
          activity = {
            id = "command-one",
            kind = "command",
            title = "Get-Content README.md",
            status = "inProgress",
            output_delta = false,
          },
        } })
        emit(options, { event = "backend_event", payload = {
          kind = "tool",
          activity = {
            id = "command-one",
            kind = "command",
            title = "command",
            output = output,
            output_delta = true,
          },
        } })
        emit(options, { event = "backend_event", payload = {
          kind = "tool",
          activity = {
            id = "command-completed-with-different-provider-id",
            kind = "command",
            title = "Get-Content README.md",
            output = output,
            status = "completed",
            output_delta = false,
          },
        } })
      end
      emit(options, { event = "backend_event", payload = { kind = "assistant_message", text = "Mock " } })
      emit(options, { event = "backend_event", payload = { kind = "assistant_message", text = "reply" } })
      emit(options, { event = "backend_event", payload = {
        kind = "assistant_summary",
        summary = { duration_ms = 2000, token_count = 2700 },
      } })
      if request.params.text:match("^/plan ") then
        emit(options, { event = "plan_review", payload = {
          id = "plan-one",
          session_id = active_session.id,
          state = "awaiting_review",
          working_path = plan_path,
          review_digest = "digest",
        } })
      end
      if request.params.text == "hello harness" then
        vim.defer_fn(function()
          emit(options, { id = request.id, result = { session = active_session, capability = { native_fork = false } } })
        end, 1200)
      else
        emit(options, { id = request.id, result = { session = active_session, capability = { native_fork = false } } })
      end
    elseif request.method == "interaction.list" then
      emit(options, { id = request.id, result = { interaction } })
    elseif request.method == "session.list" then
      emit(options, { id = request.id, result = { active_session } })
    elseif request.method == "session.rename" then
      active_session.name = vim.trim(request.params.name or "")
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "backend.models" then
      emit(options, { id = request.id, result = { { id = "mock-model", label = "Mock model", effort = { "low" } } } })
    elseif request.method == "mode.set" then
      active_session.write_mode = request.params.mode
      emit(options, { event = "mode_changed", payload = active_session })
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "session.configure" then
      active_session = vim.tbl_extend("force", active_session, request.params)
      emit(options, { event = "session_configured", payload = active_session })
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "session.new" then
      active_session = vim.tbl_extend("force", active_session, { id = "session-two", write_mode = "read" })
      emit(options, { event = "session_changed", payload = { session = active_session, transcript = {} } })
      emit(options, { id = request.id, result = { session = active_session, transcript = {} } })
    elseif request.method == "goal.continue" then
      goal_continue_count = goal_continue_count + 1
      if goal_continue_count == 2 then
        emit(options, { event = "goal_changed", payload = { objective = "finish", state = "stalled" } })
      end
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "state.get" then
      emit(options, { id = request.id, result = {
        session = active_session,
        transcript = session.harness.transcript,
        capability = { native_fork = false },
        no_checkpoint = false,
        goal = { objective = "fail", state = "paused" },
      } })
    elseif request.method == "shutdown" then
      emit(options, { id = request.id, result = { shutdown = true } })
    else
      emit(options, { id = request.id, result = active_session })
    end
  end
  function process:kill() end
  return process
end

local ok, failure = pcall(function()
  diff_review.setup({
    harness = { backend = "mock" },
    keymaps = { status = { open = { "x" } } },
  })
  assert_equals(require("diff_review.infra.config").defaults.harness.backends.acp.command[1], "copilot",
    "default ACP backend should have a runnable launch descriptor")
  assert_equals(require("diff_review.infra.config").defaults.harness.backend, "codex",
    "Harness should default to the direct Codex backend")
  assert_equals(#require("diff_review.infra.config").options.keymaps.status.open, 1, "configured key lists should replace defaults atomically")
  assert_true(vim.uv.fs_stat(builder.manifest_path()) ~= nil, "Harness builder should resolve its crate from the plugin runtime")

  builder._set_crate_dir_for_test(crate_root)
  builder._set_runner_for_test(function(_, _, callback)
    vim.fn.mkdir(vim.fs.dirname(builder.release_binary_path()), "p")
    vim.fn.writefile({ "fake" }, builder.release_binary_path())
    callback({ code = 0, stdout = "", stderr = "" })
  end)
  client._set_launcher_for_test(fake_launcher)

  local render_frame_list = {}
  controller._set_render_observer_for_test(function(lines) render_frame_list[#render_frame_list + 1] = lines end)
  require("diff_review.views.harness").open()
  assert_equals(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, 1, false)[1], "",
    "new Harness transcripts should open empty")
  local original_get_parser = vim.treesitter.get_parser
  local original_treesitter_start = vim.treesitter.start
  local original_render_markdown = package.loaded["render-markdown"]
  local parser_region_list = nil
  local highlighted_language = nil
  local render_markdown_options = nil
  package.loaded["render-markdown"] = { render = function(options) render_markdown_options = options end }
  vim.treesitter.get_parser = function(buf, language)
    assert_equals(buf, session.harness.transcript_buf, "markdown should parse the Harness transcript")
    assert_equals(language, "markdown", "Harness responses should use the markdown parser")
    return {
      set_included_regions = function(_, region_list) parser_region_list = region_list end,
      invalidate = function() end,
    }
  end
  vim.treesitter.start = function(buf, language)
    assert_equals(buf, session.harness.transcript_buf, "markdown highlighting should target the Harness transcript")
    highlighted_language = language
  end
  require("diff_review.render.harness.markdown").render(
    session.harness.transcript_buf,
    session.harness.transcript_win,
    { { first0 = 2, after0 = 4 } }
  )
  vim.treesitter.get_parser = original_get_parser
  vim.treesitter.start = original_treesitter_start
  package.loaded["render-markdown"] = original_render_markdown
  assert_equals(highlighted_language, "markdown",
    "Harness markdown regions should activate inline Tree-sitter highlights")
  assert_equals(parser_region_list[1][1][1], 2,
    "Harness markdown highlighting should begin at the response range")
  assert_equals(parser_region_list[1][1][3], 4,
    "Harness markdown highlighting should stop before unrelated transcript rows")
  assert_equals(render_markdown_options.config.win_options.conceallevel.default, 0,
    "Harness markdown should restore the transcript's original conceal level in raw mode")
  assert_equals(render_markdown_options.config.win_options.conceallevel.rendered, 3,
    "Harness markdown should conceal syntax markers in rendered mode")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "what is this repo?" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    if not session.harness.ready or session.harness.busy then return false end
    for _, event in ipairs(session.harness.transcript) do
      if event.kind == "assistant_message" and event.text == "Mock reply" then return true end
    end
    return false
  end, 10), "Harness did not complete the prompt queued during initialization")
  local prompt_seen = false
  for _, frame in ipairs(render_frame_list) do
    local frame_text = table.concat(frame, "\n")
    if frame_text:find("what is this repo?", 1, true) then prompt_seen = true end
    if prompt_seen then
      assert_true(frame_text:find("what is this repo?", 1, true) ~= nil,
        "rendered transcript must not regress to a stale pre-prompt snapshot: " .. frame_text)
      assert_true(frame_text:find("Harness is ready.", 1, true) == nil,
        "ready placeholder must not flash after the first prompt renders")
    end
  end
  assert_true(prompt_seen, "mock frame log should observe the first prompt")
  local partial_response_seen = false
  local complete_response_seen = false
  local summary_seen = false
  for _, frame in ipairs(render_frame_list) do
    local frame_text = table.concat(frame, "\n")
    if frame_text:find("  Mock ", 1, true) and not frame_text:find("  Mock reply", 1, true) then
      partial_response_seen = true
    end
    if frame_text:find("  Mock reply", 1, true) then complete_response_seen = true end
    if frame_text:find("▸ Thought for 2s, 2.7k tokens", 1, true) then summary_seen = true end
  end
  assert_true(partial_response_seen, "each assistant delta should produce a visible intermediate frame")
  assert_true(complete_response_seen, "assistant deltas should compose into the complete response")
  assert_true(summary_seen, "assistant completion should produce a final summary frame")
  local markdown_render = require("diff_review.render.harness.transcript").build({ {
    kind = "assistant_message",
    text = "# Heading\n\n- **item**",
    summary = { duration_ms = 42000, token_count = 28500 },
  } })
  assert_equals(markdown_render.markdown_range[1].first0, 1,
    "assistant markdown should begin after its thought summary")
  assert_equals(markdown_render.markdown_range[1].after0, 4,
    "assistant markdown should end with its response body")
  assert_equals(markdown_render.highlights[1].group, "DiffReviewHarnessThought",
    "thought summaries should use the Harness green highlight")
  local commentary_render = transcript_renderer.build({
    {
      kind = "assistant_message",
      text = "I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather\nthan a directory listing.",
    },
    { kind = "tool", activity = { id = "trace", kind = "command", title = "rg --files", output = "README.md", status = "completed" } },
    { kind = "assistant_message", text = "# Repository\n\nFinal response." },
  })
  assert_equals(commentary_render.lines[1],
    "  I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather",
    "intermediate commentary should carry the response indent")
  assert_equals(commentary_render.lines[2], "  than a directory listing.",
    "explicit commentary continuations should preserve the response indent")
  assert_equals(commentary_render.highlights[1].group, "DiffReviewHarnessCommentary",
    "intermediate commentary should render in italics")
  assert_equals(#commentary_render.markdown_range, 1,
    "only the final assistant response should enter the markdown region")
  local streaming_response = transcript_renderer.build({ {
    kind = "assistant_message",
    text = "Inspecting the repository.",
  } }, { working_seconds = 2 })
  assert_true(not vim.tbl_contains(streaming_response.lines, "▸ Thinking…"),
    "streaming should rely on actual commentary instead of a transient Thinking placeholder")
  local settled_transcript = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(settled_transcript, "▸ what is this repo?"), "user prompts should use the Harness prompt marker")
  assert_true(vim.tbl_contains(settled_transcript, "▸ Thought for 2s, 2.7k tokens"),
    "completed responses should expose timing and token usage")
  assert_true(vim.tbl_contains(settled_transcript, "  Mock reply"), "assistant response content should be indented")
  assert_true(not vim.tbl_contains(settled_transcript, "You"), "transcript should not render a You label")
  assert_true(not vim.tbl_contains(settled_transcript, "Assistant"), "transcript should not render an Assistant label")
  local multiline_prompt = transcript_renderer.build({ {
    kind = "user_message",
    text = "What is this repo?\nis it cool?",
  } })
  assert_equals(multiline_prompt.lines[1], "▸ What is this repo?", "prompt should mark its first line")
  assert_equals(multiline_prompt.lines[2], "  is it cool?", "prompt continuation should align under quoted input")
  assert_equals(multiline_prompt.highlights[1].group, "DiffReviewHarnessPrompt",
    "user prompts should use the Harness yellow highlight")
  local rejected_command = transcript_renderer.build({ {
    kind = "tool",
    activity = {
      id = "rejected-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "`pwsh -Command Get-ChildItem` rejected: Generated from Rulesync permission.bash: pwsh",
      status = "failed",
    },
  } }, { activity_view = { ["rejected-command"] = "full" } })
  local consecutive_command = transcript_renderer.build({
    { kind = "tool", activity = { id = "one", kind = "command", title = "first", output = "hidden one", status = "completed" } },
    { kind = "tool", activity = { id = "two", kind = "command", title = "second", output = "hidden two", status = "completed" } },
  })
  assert_equals(consecutive_command.lines[2], "• Ran second",
    "consecutive commands should render without a blank separator")
  assert_equals(rejected_command.lines[1], "• Ran Get-ChildItem -Force",
    "rejected commands should communicate failure without adding transcript text")
  assert_equals(#rejected_command.lines, 1,
    "rejected commands should suppress redundant permission output")
  assert_true(vim.iter(rejected_command.highlights):any(function(highlight)
    return highlight.line == 1
      and highlight.first == 0
      and highlight.last == #"•"
      and highlight.group == "DiffReviewHarnessToolFailure"
  end), "rejected command bullets should use the failure highlight")
  local ansi_command = transcript_renderer.build({ {
    kind = "tool",
    activity = {
      id = "ansi-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "\27[32;1mMode\27[0m  \27[32;1mName\27[0m",
      status = "completed",
    },
  } }, { activity_view = { ["ansi-command"] = "full" } })
  assert_equals(ansi_command.lines[2], "  └ Mode  Name",
    "command output should never expose terminal color controls")
  assert_equals(vim.bo[session.harness.transcript_buf].filetype, "Harness", "transcript filetype mismatch")
  assert_equals(vim.bo[session.harness.composer_buf].filetype, "HarnessInput", "composer filetype mismatch")
  for _, harness_win in ipairs({ session.harness.transcript_win, session.harness.composer_win }) do
    assert_true(not vim.wo[harness_win].number, "Harness windows should hide absolute line numbers")
    assert_true(not vim.wo[harness_win].relativenumber, "Harness windows should hide relative line numbers")
    assert_equals(vim.wo[harness_win].signcolumn, "no", "Harness windows should hide the sign gutter")
    assert_equals(vim.wo[harness_win].statuscolumn, "", "Harness windows should hide custom status columns")
  end
  assert_equals(vim.wo[session.harness.transcript_win].foldcolumn, "0",
    "Harness transcripts should hide the fold gutter")
  assert_equals(vim.wo[session.harness.composer_win].foldcolumn, "1",
    "Harness input should reserve a one-character gutter")
  local wrapped_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(wrapped_buf, 0, -1, false, {
    "header",
    string.rep("wrapped response content ", 12),
    "final response",
  })
  local wrapped_win = vim.api.nvim_open_win(wrapped_buf, false, {
    relative = "editor",
    row = 0,
    col = 0,
    width = 20,
    height = 5,
    style = "minimal",
  })
  vim.wo[wrapped_win].wrap = true
  assert_true(require("diff_review.views.harness.layout").maximum_content_topline(wrapped_buf, wrapped_win) > 1,
    "scroll boundaries should count wrapped screen rows instead of only physical lines")
  vim.api.nvim_win_close(wrapped_win, true)
  vim.api.nvim_buf_delete(wrapped_buf, { force = true })
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Read", 1, true) ~= nil, "winbar should expose Read mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessRead#Read%*", 1, true) ~= nil,
    "winbar should render Read with the white mode highlight")
  assert_true(vim.wo[session.harness.transcript_win].breakindent,
    "Harness soft-wrapped commentary should preserve its leading indent")
  assert_equals(vim.wo[session.harness.transcript_win].breakindentopt, "",
    "Harness soft wraps should not add an extra continuation shift")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Mock CLI", 1, true) ~= nil, "winbar should expose the underlying CLI")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("mock-model-resolved", 1, true) ~= nil, "winbar should expose the resolved model")
  local plain_winbar = vim.wo[session.harness.transcript_win].winbar:gsub("%%#.-#", ""):gsub("%%%*", "")
  assert_true(plain_winbar:find("Harness", 1, true) == nil, "winbar should omit the redundant view name")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("@workspace", 1, true) == nil, "winbar should omit the trust profile")
  assert_equals(vim.wo[session.harness.composer_win].winbar, "", "composer should not duplicate the Harness winbar")
  session.harness.goal = { objective = "Complete the plan", state = "active" }
  controller.refresh_winbar()
  assert_true(vim.wo[session.harness.transcript_win].winbar:find(
    "%#DiffReviewHarnessGoal# • Goal: Complete the plan%*", 1, true
  ) ~= nil, "winbar should render the active goal with the green goal highlight")
  session.harness.goal = nil
  controller.refresh_winbar()
  assert_true(vim.fn.maparg("<C-s>", "i", false, true).callback ~= nil, "composer should map submit in insert mode")
  assert_true(vim.fn.maparg("<S-Tab>", "n", false, true).callback ~= nil, "Harness should map Shift-Tab in normal mode")
  assert_true(vim.fn.maparg("<S-Tab>", "i", false, true).callback ~= nil, "Harness should map Shift-Tab in insert mode")
  assert_true(vim.fn.maparg("<M-s>", "i", false, true).callback ~= nil,
    "Harness should map Alt-s to edit the newest queued prompt")
  assert_true(vim.fn.maparg("<C-y>", "n", false, true).callback ~= nil,
    "Harness should map Ctrl-y to the previous user prompt")
  assert_true(vim.fn.maparg("<C-z>", "n", false, true).callback ~= nil,
    "Harness should map Ctrl-z to the next user prompt")
  assert_true(vim.fn.maparg("<C-n>", "n", false, true).callback == nil,
    "Harness should not retain the old Ctrl-n prompt mapping")
  assert_true(vim.fn.maparg("<Tab>", "n", false, true).callback ~= nil,
    "Harness should map Tab to toggle complete command output")
  assert_true(has_buffer_keymap(session.harness.transcript_buf, "n", "q"),
    "Harness transcript should map q to close")
  assert_true(not has_buffer_keymap(session.harness.composer_buf, "n", "q"),
    "HarnessInput should leave q available for normal-mode editing")
  local original_ui_select = vim.ui.select
  local effort_picker_options = nil
  vim.ui.select = function(_, options, callback)
    effort_picker_options = options
    callback(nil)
  end
  controller.select_effort()
  vim.ui.select = original_ui_select
  assert_true(effort_picker_options.snacks.layout.preview == false,
    "choice-only Harness pickers should disable the Snacks preview pane")
  assert_true(effort_picker_options.snacks.main.current == false,
    "choice-only Harness pickers should not treat HarnessInput as their main window")
  assert_equals(effort_picker_options.snacks.layout.preset, "select",
    "choice-only Harness pickers should use the floating select preset")
  assert_equals(effort_picker_options.snacks.layout.layout.relative, "editor",
    "choice-only Harness pickers should float relative to the full editor")
  assert_equals(effort_picker_options.snacks.layout.layout.row, 2,
    "choice-only Harness pickers should float over the transcript area")
  local command_source = require("diff_review.views.harness.completion.command_source").new()
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/" })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 1 })
  assert_true(command_source:enabled(), "slash completion should activate at the start of a Harness prompt")
  local command_completion = nil
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(command_completion ~= nil and #command_completion.items >= 9, "slash completion should list Harness commands")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/rename" end),
    "slash completion should expose session rename")
  assert_equals(command_completion.items[1].label, "/plan", "slash completion should prioritize plan creation")

  local file_source = require("diff_review.views.harness.completion.file_source").new()
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "inspect @nvim/lua/diff_review" })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 29 })
  assert_true(file_source:enabled(), "file completion should activate after @")
  local file_completion = nil
  file_source:get_completions({}, function(result) file_completion = result end)
  assert_true(vim.wait(3000, function() return file_completion ~= nil end, 10), "workspace file completion timed out")
  assert_true(vim.iter(file_completion.items):any(function(item)
    return item.label == "@nvim/lua/diff_review/init.lua"
  end), "workspace file completion should include Git-visible files")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.write_mode == "write" end, 10), "Shift-Tab should enable Write mode")
  assert_equals(request_by_method["mode.set"].params.mode, "write", "Shift-Tab should route Write through the broker")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Write", 1, true) ~= nil, "winbar should show Write mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessWrite#Write%*", 1, true) ~= nil,
    "winbar should render Write with the yellow mode highlight")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.write_mode == "read" end, 10), "second Shift-Tab should restore Read mode")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "hello harness" })
  session.harness.activity_view["command-one"] = "full"
  local tool_frame_start = #render_frame_list + 1
  controller.submit()
  assert_true(vim.wait(2000, function()
    for _, event in ipairs(session.harness.transcript) do
      if event.activity and event.activity.id == "command-one" and event.activity.status == "completed" then return true end
    end
    return false
  end, 10), "Harness did not render the backend event")
  local tool_started_seen = false
  local tool_delta_seen = false
  local tool_completed_seen = false
  for frame_index = tool_frame_start, #render_frame_list do
    local frame_text = table.concat(render_frame_list[frame_index], "\n")
    if frame_text:find("• Running Get-Content README.md", 1, true)
      and frame_text:find("  └ working…", 1, true)
    then
      tool_started_seen = true
    end
    if frame_text:find("• Running Get-Content README.md", 1, true)
      and frame_text:find("  └ # Rendering", 1, true)
    then
      tool_delta_seen = true
    end
    if frame_text:find("• Ran Get-Content README.md", 1, true)
      and frame_text:find("  └ # Rendering", 1, true)
    then
      tool_completed_seen = true
    end
  end
  assert_true(tool_started_seen, "tool start should render before output arrives")
  assert_true(tool_delta_seen, "tool output delta should render before completion")
  assert_true(tool_completed_seen, "tool completion should replace the running state")
  local command_count = 0
  for _, event in ipairs(session.harness.transcript) do
    if event.activity and event.activity.id == "command-one" then command_count = command_count + 1 end
  end
  assert_equals(command_count, 1, "command lifecycle updates should render as one activity")
  session.harness.activity_view["command-one"] = nil
  controller.render()
  local transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(transcript_line, "• Ran Get-Content README.md"), "collapsed tools should name the command")
  assert_true(not vim.tbl_contains(transcript_line, "  └ # Rendering"), "collapsed tools should hide output content")
  local collapsed_running = transcript_renderer.build({ {
    kind = "tool",
    activity = {
      id = "running-command",
      kind = "command",
      title = "Get-Content rulesync.jsonc -TotalCount 220",
      status = "inProgress",
    },
  } })
  assert_equals(#collapsed_running.lines, 1,
    "collapsed running commands should hide the working status row")
  assert_equals(collapsed_running.activity_range[1].first, 1,
    "collapsed commands without output should remain expandable")
  local long_command_event = { kind = "tool", activity = {
    id = "long-command",
    kind = "command",
    title = "Get-Content large.txt",
    status = "completed",
    output = table.concat({ "one", "two", "three", "four", "five", "six" }, "\n"),
  } }
  local collapsed_long_command = transcript_renderer.build({ long_command_event })
  assert_equals(#collapsed_long_command.lines, 1,
    "collapsed commands should contain only their command row")
  assert_equals(collapsed_long_command.lines[1], "• Ran Get-Content large.txt",
    "collapsed commands should preserve their command text")
  local expanded_long_command = transcript_renderer.build({ long_command_event }, {
    activity_view = { ["long-command"] = "full" },
  })
  assert_equals(#expanded_long_command.lines, 7,
    "one expansion should render every command output line")
  assert_equals(expanded_long_command.lines[2], "  └ one",
    "expanded command output should attach its first row to the command")
  assert_equals(expanded_long_command.lines[7], "    six",
    "expanded command output should retain its final row without a preview stage")
  assert_equals(expanded_long_command.activity_range[1].last, 7,
    "expanded command targeting should include every output row")
  local hidden_control_tool = transcript_renderer.build({
    { kind = "user_message", text = "/goal finish the change" },
    {
      kind = "tool",
      activity = {
        id = "goal-complete",
        kind = "tool_call",
        title = "harness_goal_complete",
        status = "completed",
      },
    },
  })
  assert_equals(#hidden_control_tool.lines, 1,
    "Harness control tools should never enter the visible transcript")
  local plan_event = {
    kind = "plan",
    plan_progress = {
      id = "turn-plan",
      status = "inProgress",
      step_list = {
        { text = "Inventory the repository", status = "completed" },
        { text = "Trace configuration boundaries", status = "inProgress" },
        { text = "Synthesize the analysis", status = "pending" },
      },
    },
  }
  local active_plan = transcript_renderer.build({
    { kind = "plan", data = { method = "turn/plan/updated" } },
    plan_event,
  }, { working_seconds = 1 })
  assert_equals(active_plan.lines[1], "• Plan",
    "native plan updates should render as one structured status row")
  assert_equals(active_plan.lines[2], "  [x] Inventory the repository",
    "active plans should show their current checklist by default")
  assert_equals(active_plan.lines[3], "  [-] Trace configuration boundaries",
    "active plans should update the in-progress checklist row")
  assert_equals(active_plan.lines[4], "  [ ] Synthesize the analysis",
    "active plans should show pending checklist rows")
  assert_equals(active_plan.lines[6], "  Working (1s)",
    "live plan status should remain immediately above elapsed work status")
  assert_equals(active_plan.activity_range[1].id, "plan:turn-plan",
    "native plan rows should expose a stable expansion target")
  assert_equals(active_plan.activity_range[1].default_view, "full",
    "active plan expansion should drive the default Tab state")
  local expanded_plan = transcript_renderer.build({ plan_event }, {
    activity_view = { ["plan:turn-plan"] = "full" },
  })
  assert_equals(expanded_plan.lines[2], "  [x] Inventory the repository",
    "expanded plans should check completed steps")
  assert_equals(expanded_plan.lines[3], "  [-] Trace configuration boundaries",
    "expanded plans should mark the active step")
  assert_equals(expanded_plan.lines[4], "  [ ] Synthesize the analysis",
    "expanded plans should retain pending steps")
  plan_event.plan_progress.status = "completed"
  plan_event.plan_progress.name = "Repository analysis"
  local completed_plan = transcript_renderer.build({ plan_event })
  assert_equals(completed_plan.lines[1], "• Plan Repository analysis Complete",
    "completed named plans should emit a final completion row")
  assert_equals(#completed_plan.lines, 1,
    "completed plans should collapse their checklist by default")
  assert_equals(completed_plan.activity_range[1].default_view, "collapsed",
    "completed plan expansion should drive the default Tab state")
  local expanded_completed_plan = transcript_renderer.build({ plan_event }, {
    activity_view = { ["plan:turn-plan"] = "full" },
  })
  assert_equals(expanded_completed_plan.lines[4], "  [x] Synthesize the analysis",
    "completed plans should retain the final checklist for Tab expansion")
  local wrapped_command = require("diff_review.render.harness.transcript").build({ {
    kind = "tool",
    activity = {
      id = "wrapped-command",
      kind = "command",
      status = "completed",
      title = [["C:\Program Files\PowerShell\7\pwsh.exe" -Command 'Get-ChildItem -Force | Select-Object Mode,Name']],
      output = "",
    },
  } }, { activity_view = { ["wrapped-command"] = "full" } })
  assert_equals(wrapped_command.lines[1], "• Ran Get-ChildItem -Force | Select-Object Mode,Name",
    "command rows should remove the PowerShell launcher wrapper")
  assert_equals(wrapped_command.lines[2], "  └ no output", "empty commands should render an explicit muted result")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.line == 1 and highlight.group == "DiffReviewHarnessOption"
  end), "command options should use their semantic highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.line == 2 and highlight.group == "DiffReviewHarnessOutput"
  end), "command output should use the muted output highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.line == 1
      and highlight.first == 0
      and highlight.last == #"•"
      and highlight.group == "DiffReviewHarnessToolSuccess"
  end), "successful command bullets should use the success highlight")
  assert_true(vim.tbl_contains(transcript_line, "  Working (0s)"), "active request should render indented elapsed work status")
  assert_true(vim.wait(1600, function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false), "  Working (1s)")
  end, 20), "elapsed work status should update once per second")
  local activity_range = session.harness.activity_range[1]
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { activity_range.first, 0 })
  controller.toggle_activity()
  transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(transcript_line, "  └ # Rendering"),
    "one activity toggle should reveal the first output line")
  assert_true(vim.tbl_contains(transcript_line, "    last line"),
    "one activity toggle should reveal the complete command output")
  assert_equals(session.harness.activity_view["command-one"], "full",
    "one activity toggle should enter the full state directly")
  activity_range = session.harness.activity_range[1]
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { activity_range.last, 0 })
  controller.toggle_activity()
  transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(not vim.tbl_contains(transcript_line, "    last line"),
    "Tab from any expanded output row should collapse the command")
  assert_equals(session.harness.activity_view["command-one"], "collapsed",
    "the second activity toggle should return directly to collapsed")
  activity_range = session.harness.activity_range[1]
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], activity_range.first,
    "collapsing command output should return the cursor to its command row")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(transcript_line, "    last line"),
    "the installed Tab mapping should reveal complete command output in one press")
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], session.harness.activity_range[1].first,
    "expanding command output should keep the cursor on its command row")
  controller.render()
  assert_true(vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false), "    last line"),
    "full command output should survive a streaming rerender")

  local original_transcript = session.harness.transcript
  local original_activity_view = session.harness.activity_view
  session.harness.transcript = {
    { kind = "tool", activity = {
      id = "first-command",
      kind = "command",
      title = "first",
      output = "first output",
      status = "completed",
    } },
    { kind = "assistant_message", text = "between commands" },
    { kind = "tool", activity = {
      id = "second-command",
      kind = "command",
      title = "second",
      output = "second output\nsecond final",
      status = "completed",
    } },
  }
  session.harness.activity_view = {}
  controller.render()
  local second_range = session.harness.activity_range[2]
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { second_range.first, 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  local targeted_lines = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(not vim.tbl_contains(targeted_lines, "  └ first output"),
    "expanding the second command should leave the first command collapsed")
  assert_true(vim.tbl_contains(targeted_lines, "  └ second output"),
    "expanding the second command should reveal its first output row")
  assert_true(vim.tbl_contains(targeted_lines, "    second final"),
    "expanding the second command should reveal its final output row")
  assert_equals(session.harness.activity_view["second-command"], "full",
    "cursor targeting should update only the selected command identity")
  assert_true(session.harness.activity_view["first-command"] == nil,
    "cursor targeting should not mutate another command identity")
  local prose_line = line_number(targeted_lines, "  between commands")
  assert_true(prose_line ~= nil, "synthetic command transcript should retain its prose separator")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { prose_line, 0 })
  controller.toggle_activity()
  assert_equals(session.harness.activity_view["second-command"], "full",
    "Tab on prose should not toggle the nearest command")
  session.harness.transcript = original_transcript
  session.harness.activity_view = original_activity_view
  controller.render()
  assert_true(#session.harness.prompt_line >= 2, "Harness should index each user prompt for cursor-relative navigation")
  local first_prompt_line = session.harness.prompt_line[1]
  local second_prompt_line = session.harness.prompt_line[2]
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { second_prompt_line, 0 })
  vim.fn.maparg("<C-y>", "n", false, true).callback()
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], first_prompt_line,
    "Ctrl-y should find the previous prompt from the current cursor row")
  vim.fn.maparg("<C-z>", "n", false, true).callback()
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], second_prompt_line,
    "Ctrl-z should find the next prompt from the current cursor row")
  local hello_count = 0
  for _, event in ipairs(session.harness.transcript) do
    if event.kind == "user_message" and event.text == "hello harness" then hello_count = hello_count + 1 end
  end
  assert_equals(hello_count, 1, "streamed user admission should replace the optimistic duplicate")

  session.harness.busy = true
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "queued prompt" })
  controller.submit()
  local queue_preview = false
  for _, extmark in ipairs(vim.api.nvim_buf_get_extmarks(session.harness.composer_buf, -1, 0, -1, { details = true })) do
    local virtual_line_list = extmark[4].virt_lines or {}
    if virtual_line_list[1] and virtual_line_list[1][1]
      and virtual_line_list[1][1][1] == "• Queued follow-up inputs"
      and virtual_line_list[2] and virtual_line_list[2][1]
      and virtual_line_list[2][1][1] == "└ queued prompt"
    then
      queue_preview = true
    end
  end
  assert_true(queue_preview, "queued prompts should render above the composer")
  vim.api.nvim_win_call(session.harness.composer_win, function()
    assert_equals(vim.fn.winsaveview().topfill, 2,
      "queued prompt virtual lines should be included in the visible composer viewport")
  end)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "displaced draft" })
  controller.edit_last_queued()
  assert_equals(table.concat(vim.api.nvim_buf_get_lines(session.harness.composer_buf, 0, -1, false), "\n"), "queued prompt", "edit queued should restore newest prompt")
  assert_equals(session.harness.queue[#session.harness.queue], "displaced draft", "edit queued should preserve an existing draft")
  session.harness.queue = {}
  session.harness.busy = false

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan inspect the feature" })
  controller.submit()
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10), "PlanReview did not open the physical plan")
  assert_equals(vim.bo.filetype, "markdown", "PlanReview should use markdown")
  assert_true(vim.fn.maparg("oY", "n", false, true).callback ~= nil, "PlanReview accept mapping missing")
  assert_true(vim.fn.maparg("oN", "n", false, true).callback ~= nil, "PlanReview request-changes mapping missing")
  vim.fn.maparg("oY", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return request_by_method["plan.accept"] ~= nil end, 10), "PlanReview did not submit acceptance")
  assert_equals(#request_by_method["plan.accept"].params.digest, 64, "PlanReview should submit the exact saved digest")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan revise the feature" })
  controller.submit()
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10), "second PlanReview did not open")
  local original_input = vim.ui.input
  vim.ui.input = function(_, callback) callback("Name the exact module") end
  vim.fn.maparg("C", "n", false, true).callback()
  vim.ui.input = original_input
  vim.fn.maparg("oN", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return request_by_method["plan.request_changes"] ~= nil end, 10), "PlanReview did not request changes")
  assert_equals(request_by_method["plan.request_changes"].params.annotations[1].body, "Name the exact module",
    "PlanReview should serialize anchored annotations")
  vim.cmd("tabclose")

  require("diff_review.views.interactions").open()
  assert_true(vim.wait(2000, function() return vim.bo.filetype == "DiffReviewInteractions" end, 10), "Interactions did not open")
  local interaction_text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert_true(interaction_text:find("Interaction 1", 1, true) ~= nil, "Interactions should render interaction headers")
  assert_true(interaction_text:find("return 'after'", 1, true) ~= nil, "Interactions should render parsed diff lines")
  local comment_rendered = false
  for _, extmark in ipairs(vim.api.nvim_buf_get_extmarks(0, -1, 0, -1, { details = true })) do
    for _, virtual_line in ipairs(extmark[4].virt_lines or {}) do
      for _, segment in ipairs(virtual_line) do
        if segment[1]:find("Persisted review note", 1, true) then comment_rendered = true end
      end
    end
  end
  assert_true(comment_rendered, "Interactions should restore persisted review comments")
  assert_true(vim.fn.maparg("R", "n", false, true).callback ~= nil, "checkpointed sessions should expose rollback")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert_equals(vim.fn.foldclosed(1), 1, "interaction folds should begin on their visible header")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  vim.cmd("tabclose")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/rename Architecture review" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.rename"] ~= nil and session.harness.session.name == "Architecture review"
  end, 10), "/rename should persist the active session name")
  require("diff_review.views.sessions").open()
  assert_true(vim.wait(2000, function() return vim.bo.filetype == "DiffReviewSessions" end, 10), "Sessions did not open")
  assert_true(vim.fn.maparg("F", "n", false, true).callback == nil, "unsupported native fork should be absent")
  assert_true(vim.api.nvim_get_current_line():find("Current Repo", 1, true) ~= nil, "Sessions should show repository and global tabs")
  local named_session_line = vim.iter(vim.api.nvim_buf_get_lines(0, 0, -1, false)):find(function(line)
    return line:find("Architecture review", 1, true) ~= nil
  end)
  assert_true(named_session_line ~= nil and named_session_line:find("Architecture review", 1, true) == 1,
    "Sessions should render the session name as its first column")

  vim.cmd("tabclose")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/rename" })
  controller.submit()
  assert_true(vim.wait(2000, function() return session.harness.session.name == "" end, 10),
    "/rename without a name should clear the active session name")
  require("diff_review.views.sessions").open()
  assert_true(vim.wait(2000, function() return vim.bo.filetype == "DiffReviewSessions" end, 10),
    "Sessions did not reopen for unnamed-session verification")
  local unnamed_session_line = vim.iter(vim.api.nvim_buf_get_lines(0, 0, -1, false)):find(function(line)
    return line:find("[unnamed]", 1, true) ~= nil
  end)
  assert_true(unnamed_session_line ~= nil and unnamed_session_line:find("[unnamed]", 1, true) == 1,
    "Sessions should render empty names as [unnamed] in the first column")
  vim.cmd("tabclose")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/model selected-model" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.configure"] ~= nil and session.harness.session.model == "selected-model"
  end, 10), "/model should configure the active session")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/effort high" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.configure"] ~= nil and session.harness.session.effort == "high"
  end, 10), "/effort should configure the active session")
  require("diff_review.views.harness").new_session()
  assert_true(vim.wait(2000, function()
    return session.harness.session.id == "session-two"
  end, 10), "new session should complete")
  assert_equals(session.harness.session.model, "selected-model", "new sessions should preserve the selected model")
  assert_equals(session.harness.session.effort, "high", "new sessions should preserve the selected effort")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/goal fail" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return session.harness.goal and session.harness.goal.state == "paused" and not session.harness.busy
  end, 10), "failed goals should reconcile to paused durable state")
  assert_equals(goal_continue_count, 0, "failed goals must not enter an automatic retry loop")

  session.harness.active_plan = nil
  session.harness.goal = { objective = "finish", state = "active" }
  controller.drain()
  assert_true(vim.wait(2000, function() return goal_continue_count == 2 and not session.harness.busy end, 10), "goal continuation should drain until a terminal guard state")
  assert_equals(session.harness.goal.state, "stalled", "goal guard should stop automatic continuation")
end)

controller._set_render_observer_for_test(nil)
client._reset_for_test()
builder._reset_for_test()
pcall(vim.fn.delete, test_root, "rf")

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
