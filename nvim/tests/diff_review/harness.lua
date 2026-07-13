vim.loader.enable(false)

local diff_review = require("diff_review")
local builder = require("diff_review.harness.builder")
local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local interaction_renderer = require("diff_review.render.harness.interaction_tree")
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
local prompt_count = 0
local launched_binary = nil
local active_plan = nil

local function emit(options, message)
  vim.schedule(function() options.stdout(nil, vim.json.encode(message) .. "\n") end)
end

local function fake_launcher(command, options, _)
  launched_binary = command[1]
  local process = {}
  function process:write(payload)
    local request = vim.json.decode(vim.trim(payload))
    request_by_method[request.method] = request
    if request.method == "initialize" then
      vim.defer_fn(function()
        emit(options, { id = request.id, result = {
          session = active_session,
          interaction = {},
          capability = { native_fork = false, model_selection = true },
          no_checkpoint = false,
        } })
      end, 80)
    elseif request.method == "prompt.submit" then
      if request.params.text == "/goal fail" then
        emit(options, { id = request.id, error = { message = "synthetic goal failure" } })
        return
      end
      prompt_count = prompt_count + 1
      local interaction_id = "live-interaction-" .. prompt_count
      local thought_id = interaction_id .. ":thought:1"
      local completed_tool = nil
      if request.params.text == "hello harness" then
        local output = table.concat({ "# Rendering", "line two", "line three", "line four", "last line" }, "\n")
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_active",
          data = {
            interaction_id = interaction_id,
            thought_id = thought_id,
            text = "Working",
            synthetic = true,
            tool_count = 1,
            failed_count = 0,
            revision = 1,
          },
        } })
        completed_tool = {
          id = "command-one",
          kind = "command",
          title = "Get-Content README.md",
          status = "completed",
          output = output,
        }
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_thought_completed",
          data = {
            id = thought_id,
            text = "Working",
            synthetic = true,
            tool = { completed_tool },
            started_at_ms = 0,
            completed_at_ms = 1000,
          },
        } })
      end
      emit(options, { event = "backend_event", payload = {
        kind = "timeline_active",
        data = {
          interaction_id = interaction_id,
          thought_id = interaction_id .. ":thought:2",
          text = "Mock ",
          synthetic = false,
          tool_count = 0,
          failed_count = 0,
          revision = 2,
        },
      } })
      emit(options, { event = "backend_event", payload = {
        kind = "timeline_active",
        data = {
          interaction_id = interaction_id,
          thought_id = interaction_id .. ":thought:2",
          text = "Mock reply",
          synthetic = false,
          tool_count = 0,
          failed_count = 0,
          revision = 3,
        },
      } })
      if request.params.text:match("^/plan ") then
        active_plan = {
          id = "plan-one",
          session_id = active_session.id,
          title = "Inspect the feature",
          state = "awaiting_review",
          working_path = plan_path,
          review_digest = "digest",
        }
        emit(options, { event = "plan_created", payload = {
          plan = active_plan,
          lifecycle = { id = "lifecycle-one", kind = "created" },
          content = "# Plan\n\n1. Review the implementation.",
        } })
      end
      if request.params.text == "hello harness" then
        vim.defer_fn(function()
          local completed = {
            id = interaction_id,
            session_id = active_session.id,
            ordinal = prompt_count,
            prompt = request.params.text,
            state = "complete",
            thought = completed_tool and { {
              id = thought_id,
              text = "Working",
              synthetic = true,
              tool = { completed_tool },
              started_at_ms = 0,
              completed_at_ms = 1000,
            } } or {},
            response = "Mock reply",
            duration_ms = 2000,
            token_count = 2700,
            attribution_complete = true,
          }
          emit(options, { event = "interaction_complete", payload = completed })
          emit(options, { id = request.id, result = {
            interaction = completed,
            session = active_session,
            capability = { native_fork = false },
          } })
        end, 1200)
      else
        local completed = {
          id = interaction_id,
          session_id = active_session.id,
          ordinal = prompt_count,
          prompt = request.params.text,
          state = "complete",
          thought = {},
          response = "Mock reply",
          duration_ms = 2000,
          token_count = 2700,
          attribution_complete = true,
        }
        emit(options, { event = "interaction_complete", payload = completed })
        emit(options, { id = request.id, result = {
          interaction = completed,
          session = active_session,
          capability = { native_fork = false },
        } })
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
      emit(options, { event = "session_changed", payload = { session = active_session, interaction = {} } })
      emit(options, { id = request.id, result = { session = active_session, interaction = {} } })
    elseif request.method == "session.fork" then
      active_session = vim.tbl_extend("force", active_session, {
        id = "session-fork",
        native_fork = true,
        write_mode = "read",
      })
      emit(options, { event = "session_changed", payload = { session = active_session } })
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = { interaction },
        capability = { native_fork = true },
      } })
    elseif request.method == "goal.continue" then
      goal_continue_count = goal_continue_count + 1
      if goal_continue_count == 2 then
        emit(options, { event = "goal_changed", payload = { objective = "finish", state = "stalled" } })
      end
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "state.get" then
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = session.harness.interaction,
        capability = { native_fork = false },
        no_checkpoint = false,
        goal = { objective = "fail", state = "paused" },
        active_plan = active_plan,
        artifact = active_plan and { active_plan } or {},
      } })
    elseif request.method == "plan.activate" then
      emit(options, { id = request.id, result = active_plan })
    elseif request.method == "plan.accept" then
      emit(options, { event = "goal_changed", payload = { objective = "Complete the plan", state = "complete" } })
      emit(options, { id = request.id, result = { session = active_session } })
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
  builder._set_artifact_root_for_test(vim.fs.joinpath(test_root, "artifacts"))
  builder._set_runner_for_test(function(_, _, callback)
    vim.fn.mkdir(vim.fs.dirname(builder.release_binary_path()), "p")
    vim.fn.writefile({ "fake" }, builder.release_binary_path())
    callback({ code = 0, stdout = "", stderr = "" })
  end)
  client._set_launcher_for_test(fake_launcher)

  local render_frame_list = {}
  local render_transaction_list = {}
  controller._set_render_observer_for_test(function(lines, transaction)
    render_frame_list[#render_frame_list + 1] = lines
    render_transaction_list[#render_transaction_list + 1] = transaction
  end)
  require("diff_review.views.harness").open()
  assert_true(vim.wait(1000, function() return launched_binary ~= nil end, 10), "Harness broker did not launch")
  assert_true(launched_binary ~= builder.release_binary_path(), "Harness must not execute Cargo's replaceable output")
  assert_equals(launched_binary, builder.deployed_binary_path(), "Harness should launch the immutable staged executable")
  assert_equals(vim.wo[session.harness.transcript_win].breakindentopt, "shift:0",
    "wrapped Harness rows should preserve their existing indent without adding another shift")
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
    for _, item in ipairs(session.harness.interaction) do
      if item.prompt == "what is this repo?" and item.response == "Mock reply" then return true end
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
    if frame_text:find("↳ Mock ", 1, true) and not frame_text:find("↳ Mock reply", 1, true) then
      partial_response_seen = true
    end
    if frame_text:find("Mock reply", 1, true) then complete_response_seen = true end
    if frame_text:find("▸ Thought for 2s, 2.7k tokens", 1, true) then summary_seen = true end
  end
  assert_true(partial_response_seen, "each assistant delta should produce a visible intermediate frame")
  assert_true(complete_response_seen, "assistant deltas should compose into the complete response")
  assert_true(summary_seen, "assistant completion should produce a final summary frame")
  local markdown_render = interaction_renderer.build({ {
    id = "markdown",
    prompt = "Explain the result",
    state = "complete",
    duration_ms = 42000,
    token_count = 28500,
    thought = {},
    response = "# Heading\n\n- **item**",
  } })
  assert_equals(markdown_render.markdown_ranges[1].first0, 3,
    "assistant markdown should begin after its thought summary")
  assert_equals(markdown_render.markdown_ranges[1].after0, 6,
    "assistant markdown should end with its response body")
  assert_true(vim.iter(markdown_render.highlights):any(function(highlight)
    return highlight.line == 2 and highlight.group == "DiffReviewHarnessThought"
  end),
    "thought summaries should use the Harness green highlight")
  assert_equals(interaction_renderer.foldtext("▸ Thought for 42s, 28.5k tokens")[1][2],
    "DiffReviewHarnessThought",
    "closed thought folds should retain the Harness green highlight")
  assert_true(vim.iter(markdown_render.highlights):any(function(highlight)
    return highlight.line == 3 and highlight.group == "DiffReviewHarnessResponse"
  end),
    "response headings should use the Harness white highlight")
  local completed_thought_render = interaction_renderer.build({ {
    id = "completed-thought",
    prompt = "Inspect the repository",
    state = "complete",
    thought = { {
      id = "completed-thought:1",
      text = "I’m tracing the repository’s ownership boundaries\nand generated configuration.",
      tool = { {
        id = "completed-thought:tool:1",
        kind = "command",
        title = "rg --files",
        status = "completed",
        output = "README.md",
      } },
    } },
  } }, { expanded = {
    ["interaction:completed-thought:thoughts"] = true,
    ["interaction:completed-thought:thought:completed-thought:1"] = true,
  } })
  assert_equals(completed_thought_render.lines[3],
    "↳ I’m tracing the repository’s ownership boundaries",
    "completed thoughts should retain their first text row")
  assert_equals(completed_thought_render.lines[4], "  and generated configuration.",
    "completed thoughts should retain every continuation row")
  assert_true(vim.iter(completed_thought_render.highlights):all(function(highlight)
    if highlight.line ~= 3 and highlight.line ~= 4 then return true end
    return highlight.group == "DiffReviewHarnessCommentary"
  end), "completed thought rows should retain the original italic commentary highlight")
  assert_equals(completed_thought_render.rows[3].expand_key,
    "interaction:completed-thought:thought:completed-thought:1",
    "completed thought rows should own their semantic expansion state")
  assert_equals(completed_thought_render.rows[5].expand_key,
    "interaction:completed-thought:thought:completed-thought:1:tools",
    "completed tool summaries should own the tool-list expansion state")
  assert_equals(completed_thought_render.lines[5], "  ▸ Ran 1 tool",
    "thought tool summaries should render as nested disclosure nodes")
  assert_equals(interaction_renderer.foldtext("  ▸ Ran 3 tools (2 failed)")[1][2], "Normal",
    "collapsed tool summaries should not inherit the dark Folded highlight")
  local folded_command_chunks = require("diff_review.render.harness.tool").foldtext_chunks({
    kind = "command",
    title = "rg --files",
    status = "completed",
  }, "  ")
  assert_equals(folded_command_chunks[1][2], "DiffReviewHarnessToolSuccess",
    "collapsed command bullets should retain their completion highlight")
  assert_true(vim.iter(folded_command_chunks):any(function(chunk)
    return chunk[1] == "rg" and chunk[2] == "DiffReviewHarnessCommand"
  end), "collapsed commands should retain their command-name highlight")
  local commentary_render = interaction_renderer.build({ {
    id = "commentary",
    prompt = "Map the repo",
    state = "running",
    thought = {},
    active = {
      text = "I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather\nthan a directory listing.",
      tool_count = 0,
      failed_count = 0,
    },
  } }, { working_seconds = 2 })
  assert_equals(commentary_render.lines[3],
    "↳ I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather",
    "intermediate commentary should carry the response indent")
  assert_equals(commentary_render.lines[4], "  than a directory listing.",
    "explicit commentary continuations should preserve the response indent")
  assert_true(vim.iter(commentary_render.highlights):any(function(highlight)
    return highlight.line == 3 and highlight.group == "DiffReviewHarnessCommentary"
  end),
    "intermediate commentary should render in italics")
  assert_equals(#commentary_render.markdown_ranges, 0,
    "streaming commentary should stay outside the markdown region")
  assert_true(not vim.tbl_contains(commentary_render.lines, "▸ Thinking…"),
    "streaming should rely on actual commentary instead of a transient Thinking placeholder")
  local settled_transcript = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(settled_transcript, "▸ what is this repo?"), "user prompts should use the Harness prompt marker")
  assert_true(vim.tbl_contains(settled_transcript, "▸ Thought for 2s, 2.7k tokens"),
    "completed responses should expose timing and token usage")
  assert_true(vim.tbl_contains(settled_transcript, "Mock reply"),
    "assistant response content should remain raw Markdown in the buffer")
  local response_body_line = line_number(settled_transcript, "Mock reply")
  assert_true(vim.iter(vim.api.nvim_buf_get_extmarks(
    session.harness.transcript_buf,
    session.harness.render_namespace,
    { response_body_line - 1, 0 },
    { response_body_line - 1, -1 },
    { details = true }
  )):any(function(extmark)
    local virtual_text = extmark[4].virt_text
    return virtual_text and virtual_text[1] and virtual_text[1][1] == "  "
  end), "response rows should use a visual two-column prefix so soft wraps stay aligned")
  assert_true(not vim.tbl_contains(settled_transcript, "You"), "transcript should not render a You label")
  assert_true(not vim.tbl_contains(settled_transcript, "Assistant"), "transcript should not render an Assistant label")
  local multiline_prompt = interaction_renderer.build({ {
    id = "multiline",
    prompt = "What is this repo?\nis it cool?",
    state = "running",
    thought = {},
  } })
  assert_equals(multiline_prompt.lines[1], "▸ What is this repo?", "prompt should mark its first line")
  assert_equals(multiline_prompt.lines[2], "  is it cool?", "prompt continuation should align under quoted input")
  assert_equals(multiline_prompt.highlights[1].group, "DiffReviewHarnessPrompt",
    "user prompts should use the Harness yellow highlight")
  local command_render = interaction_renderer.build({ {
    id = "commands",
    prompt = "Run commands",
    state = "complete",
    duration_ms = 1,
    thought = { {
      id = "commands:thought:1",
      text = "Checking the workspace.",
      tool = { {
      id = "rejected-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "`pwsh -Command Get-ChildItem` rejected: Generated from Rulesync permission.bash: pwsh",
      status = "failed",
      failed = true,
    }, {
      id = "ansi-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "\27[32;1mMode\27[0m  \27[32;1mName\27[0m",
      status = "completed",
      failed = false,
    } },
      started_at_ms = 0,
      completed_at_ms = 1,
    } },
  } })
  assert_true(vim.tbl_contains(command_render.lines, "  • Ran Get-ChildItem -Force"),
    "completed commands should publish one stable command row")
  assert_true(vim.tbl_contains(command_render.lines, "    └ Mode  Name"),
    "command output should never expose terminal color controls")
  assert_true(vim.iter(command_render.highlights):any(function(highlight)
    return command_render.lines[highlight.line] == "  • Ran Get-ChildItem -Force"
      and highlight.first == 2
      and highlight.last == 2 + #"•"
      and highlight.group == "DiffReviewHarnessToolFailure"
  end), "rejected command bullets should use the failure highlight")
  local long_command_render = interaction_renderer.build({ {
    id = "long-command",
    prompt = "Inspect manifests",
    state = "complete",
    thought = { {
      id = "long-command:thought:1",
      text = "Inspecting manifests.",
      tool = { {
        id = "long-command:tool:1",
        kind = "command",
        title = "rg -n --glob 'README*' --glob 'rulesync.jsonc' --glob 'package.json' --glob 'Cargo.toml' --glob 'init.lua' --glob 'config.toml' 'Rulesync|dotfiles|Neovim|Nushell|WezTerm|Codex' .",
        output = "README.md:1:# Config",
        status = "completed",
      } },
    } },
  } }, { content_width = 72 })
  local command_line_list = {}
  for line, row in pairs(long_command_render.rows) do
    if row.tool and row.tool.id == "long-command:tool:1" and row.kind ~= "tool_output" then
      command_line_list[#command_line_list + 1] = long_command_render.lines[line]
    end
  end
  assert_true(#command_line_list > 1, "long folded commands should render as multiple real buffer lines")
  for _, line in ipairs(command_line_list) do
    assert_true(vim.fn.strdisplaywidth(line) <= 72, "wrapped command row exceeded the transcript width: " .. line)
  end
  local output_line = line_number(long_command_render.lines, "    └ README.md:1:# Config")
  assert_true(output_line ~= nil, "long command fixture should retain its folded output")
  assert_equals(long_command_render.rows[output_line - #command_line_list].expand_key,
    "interaction:long-command:thought:long-command:thought:1:tool:1",
    "wrapped command rows should share one semantic output expansion key")
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
  assert_equals(vim.wo[session.harness.transcript_win].breakindentopt, "shift:0",
    "Harness soft wraps should preserve the row indent without adding another two columns")
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
  local tool_frame_start = #render_frame_list + 1
  controller.submit()
  assert_true(vim.wait(2000, function()
    for _, item in ipairs(session.harness.interaction) do
      if item.prompt == "hello harness" and item.state == "complete" then return true end
    end
    return false
  end, 10), "Harness did not complete the interaction timeline")
  local active_counter_seen = false
  local active_tool_leak_seen = false
  local mixed_transition_seen = false
  local tool_completed_seen = false
  for frame_index = tool_frame_start, #render_frame_list do
    local frame_text = table.concat(render_frame_list[frame_index], "\n")
    if frame_text:find("Running 1 tool", 1, true) then
      active_counter_seen = true
      if frame_text:find("Get-Content README.md", 1, true) then active_tool_leak_seen = true end
      if frame_text:find("Ran 1 tool", 1, true) then mixed_transition_seen = true end
    end
    if frame_text:find("▸ Thought for 2s, 2.7k tokens, 1 tool called", 1, true)
      and not frame_text:find("• Ran Get-Content README.md", 1, true) then tool_completed_seen = true end
  end
  assert_true(active_counter_seen, "active thoughts should stream an updating tool counter")
  assert_true(not active_tool_leak_seen, "active thoughts must not expose expandable tool rows")
  assert_true(not mixed_transition_seen, "Running-to-Ran transitions must publish one coherent frame")
  assert_true(tool_completed_seen, "completed thoughts should atomically publish their collapsed summary")
  for transaction_index = tool_frame_start, #render_transaction_list do
    assert_true((render_transaction_list[transaction_index].first0 or 0) > 0,
      "streaming a later interaction should never rewrite the settled transcript prefix")
  end
  local transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(not vim.tbl_contains(transcript_line, "  • Ran Get-Content README.md"),
    "completed summaries should initially hide their thought tree")
  local summary_line = line_number(transcript_line, "▸ Thought for 2s, 2.7k tokens, 1 tool called")
  assert_true(summary_line ~= nil, "completed interaction should render its summary node")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { summary_line, 0 })
  controller.toggle_activity()
  local thought_line = line_number(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "↳ Working")
  assert_true(thought_line ~= nil, "expanding a summary should reveal its completed thought")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { thought_line, 0 })
  controller.toggle_activity()
  local tool_summary_line = line_number(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "  ▸ Ran 1 tool")
  assert_true(tool_summary_line ~= nil, "expanding a thought should reveal its tool summary")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { tool_summary_line, 0 })
  controller.toggle_activity()
  transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(transcript_line, "  • Ran Get-Content README.md"),
    "expanding a tool summary should reveal command headers")
  assert_true(not vim.tbl_contains(transcript_line, "    └ # Rendering"),
    "command headers should initially hide their output")
  local task_snapshot = {
    scope_id = "turn-plan",
    revision = 1,
    current = {
      { id = "task-1", title = "Inventory the repository", status = "completed" },
      { id = "task-2", title = "Trace configuration boundaries", status = "in_progress" },
      { id = "task-3", title = "Synthesize the analysis", status = "pending" },
    },
  }
  local active_plan = interaction_renderer.build({ {
    id = "plan-interaction",
    prompt = "/plan analyze the repo",
    kind = "plan_draft",
    state = "running",
    thought = {},
    active = { text = "Working", tool_count = 0, failed_count = 0 },
    task = task_snapshot,
  } }, {
    working_seconds = 1,
  })
  assert_true(vim.tbl_contains(active_plan.lines, "▸ Planning for 1s"),
    "Harness planning should use a dedicated inline summary")
  assert_true(vim.tbl_contains(active_plan.lines, "● Inventory the repository"),
    "active planning should show completed provider tasks inline")
  assert_true(vim.tbl_contains(active_plan.lines, "◐ Trace configuration boundaries"),
    "active planning should show the provider's current task inline")
  assert_true(vim.tbl_contains(active_plan.lines, "○ Synthesize the analysis"),
    "active planning should show pending provider tasks inline")
  assert_true(not vim.tbl_contains(active_plan.lines, "• Plan"),
    "provider tasks should never reappear as a tail-only plan block")
  for _, task in ipairs(task_snapshot.current) do task.status = "completed" end
  local completed_plan = interaction_renderer.build({ {
    id = "plan-interaction",
    prompt = "/plan analyze the repo",
    kind = "plan_draft",
    state = "complete",
    duration_ms = 1000,
    thought = {},
    task = task_snapshot,
  } })
  assert_true(vim.tbl_contains(completed_plan.lines, "▸ Planned for 1s"),
    "completed planning should retain its dedicated summary")
  assert_true(vim.tbl_contains(completed_plan.lines, "● Synthesize the analysis"),
    "completed provider tasks should remain inline with their interaction")
  assert_equals(#completed_plan.folds, 0,
    "provider task state should use semantic expansion instead of native folds")
  local lifecycle_collapsed = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "lifecycle-created",
    lifecycle = { id = "lifecycle-created", kind = "created" },
    content = "# Streaming plan\n\nUse one timeline projection.",
  } }, { expanded = {} })
  assert_equals(lifecycle_collapsed.lines[1], "▸ Plan created",
    "plan creation should render as one inline lifecycle node")
  assert_true(not vim.tbl_contains(lifecycle_collapsed.lines, "  ▸ Content"),
    "collapsed lifecycle nodes should hide artifact content")
  local lifecycle_expanded = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "lifecycle-created",
    lifecycle = { id = "lifecycle-created", kind = "created" },
    content = "# Streaming plan\n\nUse one timeline projection.",
  } }, { expanded = { ["plan_lifecycle:lifecycle-created"] = true } })
  assert_true(vim.tbl_contains(lifecycle_expanded.lines, "  ▸ Content"),
    "expanded lifecycle nodes should reveal artifact content")
  assert_equals(#lifecycle_expanded.markdown_ranges, 1,
    "expanded plan content should register one markdown region")
  local attributed_execution = interaction_renderer.build({ {
    kind = "plan_execution",
    id = "execution-one",
    execution = { state = "complete" },
    interaction = { {
      id = "execution-interaction",
      state = "complete",
      duration_ms = 1000,
      task = { current = { { id = "task-one", title = "Rewrite core", status = "completed" } } },
      thought = { {
        id = "thought-one",
        text = "Rewriting core.",
        task_id = "task-one",
        tool = {},
      } },
    } },
  } }, { expanded = { ["interaction:execution-one:task:task-one"] = true } })
  assert_true(vim.tbl_contains(attributed_execution.lines, "▸ Executed plan (1/1) for 1s"),
    "accepted plan execution should render as one grouped timeline node")
  assert_true(vim.tbl_contains(attributed_execution.lines, "● Rewrite core"),
    "execution should render the provider's final task presentation")
  assert_true(vim.tbl_contains(attributed_execution.lines, "↳ Rewriting core."),
    "completed tasks should expand their frozen attributed thoughts")

  local wrapped_tool = {
    id = "wrapped-command",
    kind = "command",
    status = "completed",
    title = [["C:\Program Files\PowerShell\7\pwsh.exe" -Command 'Get-ChildItem -Force | Select-Object Mode,Name']],
    output = "",
    failed = false,
  }
  local wrapped_command = interaction_renderer.build({ {
    id = "wrapped",
    prompt = "list files",
    state = "complete",
    thought = { {
      id = "wrapped:thought:1",
      text = "Listing files.",
      tool = { wrapped_tool },
      started_at_ms = 0,
      completed_at_ms = 1,
    } },
  } })
  assert_true(vim.tbl_contains(wrapped_command.lines, "  • Ran Get-ChildItem -Force | Select-Object Mode,Name"),
    "command rows should remove the PowerShell launcher wrapper")
  assert_true(vim.tbl_contains(wrapped_command.lines, "    └ no output"),
    "empty commands should retain an explicit muted result inside their fold")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.group == "DiffReviewHarnessOption"
  end), "command options should use their semantic highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.group == "DiffReviewHarnessOutput"
  end), "command output should use the muted output highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.first == 2
      and highlight.last == 2 + #"•"
      and highlight.group == "DiffReviewHarnessToolSuccess"
  end), "successful command bullets should use the success highlight")
  local diff_text = table.concat({
    "diff --git a/nvim/lua/diff_review/render/diff_tree.lua b/nvim/lua/diff_review/render/diff_tree.lua",
    "--- a/nvim/lua/diff_review/render/diff_tree.lua",
    "+++ b/nvim/lua/diff_review/render/diff_tree.lua",
    "@@ -1,2 +1,2 @@",
    " local M = {}",
    "-return M",
    "+return { M }",
  }, "\n")
  local thought_diff_render = interaction_renderer.build({ {
    id = "thought-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = { {
      id = "thought-diff:thought:1",
      text = "Updating the renderer.",
      tool = {},
      diff_text = diff_text,
    } },
  } })
  local thought_change_line = line_number(thought_diff_render.lines, "  ▸ Changed 1 file +1 -1")
  assert_true(thought_change_line ~= nil, "thought change summaries should render as nested disclosure nodes")
  assert_true(thought_diff_render.lines[thought_change_line + 1]:match("^    Modified") ~= nil,
    "thought-level file rows should nest beneath their change summary")
  assert_true(thought_diff_render.lines[thought_change_line + 2]:match("^    @@") ~= nil,
    "thought-level hunk rows should align with their nested file header")
  local aggregate_diff_render = interaction_renderer.build({ {
    id = "aggregate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = {},
  } }, { expanded = {
    ["interaction:aggregate-diff:aggregate"] = true,
    ["interaction:aggregate-diff:aggregate:file:1"] = true,
    ["interaction:aggregate-diff:aggregate:file:1:hunk:1"] = true,
  } })
  local aggregate_change_line = line_number(aggregate_diff_render.lines, "▸ Changed 1 file +1 -1")
  assert_true(aggregate_change_line ~= nil, "aggregate change summaries should remain top-level nodes")
  assert_true(aggregate_diff_render.lines[aggregate_change_line + 1]:match("^  Modified") ~= nil,
    "aggregate file rows should nest one level below their change summary")
  assert_true(aggregate_diff_render.lines[aggregate_change_line + 2]:match("^  @@") ~= nil,
    "aggregate hunk rows should align with their nested file header")
  local collapsed_aggregate_render = interaction_renderer.build({ {
    id = "aggregate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = {},
    response = "Done",
  } }, { expanded = {} })
  local transaction_buf = vim.api.nvim_create_buf(false, true)
  local transaction_namespace = vim.api.nvim_create_namespace("diff_review_harness_collapse_test")
  local transaction_state = {
    transcript_buf = transaction_buf,
    transcript_win = nil,
    rendered_lines = {},
    render_rows = {},
    render_namespace = transaction_namespace,
    render_initialized = false,
    fold_installed = {},
  }
  require("diff_review.render.harness.transaction").apply(transaction_state, aggregate_diff_render, { reset = true })
  assert_true(vim.iter(vim.api.nvim_buf_get_extmarks(transaction_buf, transaction_namespace, 0, -1, { details = true }))
    :any(function(extmark) return type(extmark[4].virt_text) == "table" end),
    "expanded Harness diffs should install persistent shared gutters")
  require("diff_review.render.harness.transaction").apply(transaction_state, collapsed_aggregate_render)
  local collapsed_extmark_list = vim.api.nvim_buf_get_extmarks(
    transaction_buf, transaction_namespace, 0, -1, { details = true }
  )
  assert_true(not vim.iter(collapsed_extmark_list)
    :any(function(extmark)
      local virtual_text = extmark[4].virt_text
      return type(virtual_text) == "table" and #virtual_text > 1
    end),
    "collapsing a Harness diff should remove old gutters before deleted rows can relocate them")
  assert_true(not vim.iter(collapsed_extmark_list):any(function(extmark)
    local details = extmark[4]
    local group = tostring(details.hl_group or details.line_hl_group or "")
    return group:find("DiffReviewAdd", 1, true) or group:find("DiffReviewDelete", 1, true)
  end), "collapsing a Harness diff should remove old diff colors before suffix rows relocate")
  assert_equals(vim.api.nvim_buf_get_lines(transaction_buf, -2, -1, false)[1], "Done",
    "collapsing a Harness diff should leave the response row uncorrupted")
  local duplicate_thought_key = "interaction:duplicate-diff:thought:duplicate-thought"
  local duplicate_aggregate_key = "interaction:duplicate-diff:aggregate"
  local duplicate_expanded = {
    ["interaction:duplicate-diff:thoughts"] = true,
    [duplicate_thought_key] = true,
    [duplicate_thought_key .. ":changes"] = true,
    [duplicate_thought_key .. ":changes:file:1"] = true,
    [duplicate_thought_key .. ":changes:file:1:hunk:1"] = true,
    [duplicate_aggregate_key] = true,
    [duplicate_aggregate_key .. ":file:1"] = true,
    [duplicate_aggregate_key .. ":file:1:hunk:1"] = true,
  }
  local duplicate_interaction = { {
    id = "duplicate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = { {
      id = "duplicate-thought",
      text = "Updating the renderer.",
      tool = {},
      diff_text = diff_text,
    } },
  } }
  local duplicate_render = interaction_renderer.build(duplicate_interaction, { expanded = duplicate_expanded })
  local duplicate_buf = vim.api.nvim_create_buf(false, true)
  local duplicate_win = vim.api.nvim_open_win(duplicate_buf, false, {
    relative = "editor",
    row = 1,
    col = 1,
    width = 60,
    height = 12,
    style = "minimal",
  })
  local duplicate_state = {
    transcript_buf = duplicate_buf,
    transcript_win = duplicate_win,
    rendered_lines = {},
    render_rows = {},
    render_namespace = vim.api.nvim_create_namespace("diff_review_harness_duplicate_cursor_test"),
    render_initialized = false,
    fold_installed = {},
  }
  require("diff_review.render.harness.transaction").apply(duplicate_state, duplicate_render, { reset = true })
  local aggregate_hunk_line = line_number(duplicate_render.lines, "  @@ +1 -1")
  assert_true(aggregate_hunk_line ~= nil, "duplicate diff fixture should expose the aggregate hunk header")
  vim.api.nvim_win_set_cursor(duplicate_win, { aggregate_hunk_line, 0 })
  duplicate_expanded[duplicate_aggregate_key .. ":file:1:hunk:1"] = false
  local collapsed_duplicate_render = interaction_renderer.build(duplicate_interaction, { expanded = duplicate_expanded })
  require("diff_review.render.harness.transaction").apply(duplicate_state, collapsed_duplicate_render)
  local restored_hunk_line = vim.api.nvim_win_get_cursor(duplicate_win)[1]
  assert_equals(collapsed_duplicate_render.lines[restored_hunk_line], "  @@ +1 -1",
    "collapsing one repeated diff should keep the cursor on that tree's hunk header")
  vim.api.nvim_win_close(duplicate_win, true)
  vim.api.nvim_buf_delete(duplicate_buf, { force = true })
  local diff_key = "projection:file:1"
  local hunk_key = diff_key .. ":hunk:1"
  local collapsed_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    expanded = {},
  })
  assert_equals(#collapsed_diff.lines, 1, "collapsed changed-file nodes should materialize only the file header")
  assert_equals(collapsed_diff.rows[1].expand_key, diff_key,
    "changed-file headers should own stable semantic expansion keys")
  local file_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    expanded = { [diff_key] = true },
  })
  assert_equals(#file_diff.lines, 2, "expanded files should materialize only collapsed hunk headers")
  assert_equals(file_diff.rows[2].expand_key, hunk_key,
    "hunk headers should own stable semantic expansion keys")
  local indented_file_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    indent = 2,
    expanded = { [diff_key] = true },
  })
  assert_true(indented_file_diff.lines[1]:match("^  Modified") ~= nil,
    "embedded file headers should apply only the caller-selected indent")
  assert_true(indented_file_diff.lines[2]:match("^  @@") ~= nil,
    "embedded hunk headers should align exactly with their file header")
  local background_extmark = require("diff_review.render.row_emitter").extmark_list(4, {
    bg = { hl_group = "DiffReviewAddBg", priority = 60 },
    highlights = {},
  }, false)[1]
  assert_equals(background_extmark.options.end_row, 5,
    "the shared row emitter should cover the complete diff row instead of a zero-length gutter span")
  assert_true(background_extmark.options.hl_eol == true,
    "the shared row emitter should extend diff backgrounds to the window edge")
  local syntax_updated = false
  local expanded_diff_options = {
    key_prefix = "projection",
    cwd = vim.uv.cwd(),
    expanded = { [diff_key] = true, [hunk_key] = true },
    on_update = function() syntax_updated = true end,
  }
  local expanded_diff = require("diff_review.render.diff_tree").build(diff_text, expanded_diff_options)
  vim.wait(1000, function() return syntax_updated end, 10)
  assert_true(syntax_updated, "Harness diff syntax should schedule an asynchronous shared-renderer upgrade")
  vim.wait(2000, function()
    expanded_diff = require("diff_review.render.diff_tree").build(diff_text, expanded_diff_options)
    for _, spans in pairs(expanded_diff.diff_row_spans or {}) do
      if vim.iter(spans.highlights or {}):any(function(highlight)
        return tostring(highlight.hl_group):match("^@") ~= nil
      end) then return true end
    end
    return false
  end, 10)
  assert_true(#expanded_diff.lines > 2, "expanded hunks should materialize their shared fancy diff rows")
  local syntax_group_list = {}
  for _, spans in pairs(expanded_diff.diff_row_spans or {}) do
    for _, highlight in ipairs(spans.highlights or {}) do
      syntax_group_list[#syntax_group_list + 1] = tostring(highlight.hl_group)
    end
  end
  assert_true(vim.iter(syntax_group_list):any(function(group)
    return group:match("^@") ~= nil
  end), "Harness diffs should retain the shared Tree-sitter syntax highlights: " .. table.concat(syntax_group_list, ","))
  local active_tree = require("diff_review.render.harness.interaction_tree").build({ {
    id = "active-only",
    prompt = "stream",
    state = "running",
    thought = {},
    active = {
      text = "Working",
      tool_count = 3,
      failed_count = 1,
      latest_tool = {
        id = "latest-tool",
        kind = "command",
        title = "rg -n -C 3 pattern nvim/lua/plugins/highlight.lua",
        output = "line one\nline two\nline three\nline four\nline five",
        status = "completed",
        failed = false,
      },
    },
  } }, { working_seconds = 1 })
  assert_true(vim.tbl_contains(active_tree.lines, "  ▸ Running 3 tools (1 failed)"),
    "active thoughts should expose their updating tool counter")
  assert_equals(#active_tree.folds, 0, "active thoughts must not expose expandable ranges")
  assert_true(vim.tbl_contains(active_tree.lines,
    "  • Ran rg -n -C 3 pattern nvim/lua/plugins/highlight.lua"),
    "active thoughts should render the latest tool identity")
  assert_true(vim.tbl_contains(active_tree.lines, "    └ line one"),
    "active tool previews should render their first output line")
  assert_true(vim.tbl_contains(active_tree.lines, "      line four"),
    "active tool previews should render up to four output lines")
  assert_true(not vim.tbl_contains(active_tree.lines, "      line five"),
    "active tool previews should truncate output after four lines")

  vim.api.nvim_set_current_win(session.harness.transcript_win)
  local tool_line = nil
  for line, row in pairs(session.harness.render_rows) do
    if row.kind == "tool" and row.tool.id == "command-one" then tool_line = line end
  end
  assert_true(tool_line ~= nil, "completed thought should index its tool row")
  assert_true(session.harness.render_rows[tool_line].expand_key ~= nil,
    "completed tool rows should own one output expansion key")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { tool_line, 0 })
  controller.toggle_activity()
  assert_true(vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "one activity toggle should reveal the complete command output")
  controller.toggle_activity()
  assert_true(not vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "the second activity toggle should hide the complete command output")
  controller.render()
  assert_true(not vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "collapsed completed output should survive later render transactions")

  vim.api.nvim_set_current_win(session.harness.transcript_win)
  assert_equals(#session.harness.activity_range, 0,
    "interaction nodes should avoid overlapping native fold ranges")
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
  for _, item in ipairs(session.harness.interaction) do
    if item.prompt == "hello harness" then hello_count = hello_count + 1 end
  end
  assert_equals(hello_count, 1, "broker completion should replace the optimistic interaction")

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
  assert_true(vim.wait(2000, function() return #(session.harness.artifact or {}) == 1 end, 10),
    "completed planning should publish a session artifact")
  assert_true(vim.api.nvim_buf_get_name(0) ~= plan_path,
    "completed planning should not open PlanReview automatically")
  local original_select = vim.ui.select
  vim.ui.select = function(items, _, callback) callback(items[1]) end
  controller.open_artifact_picker()
  vim.ui.select = original_select
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10),
    "artifact selection did not open the physical plan")
  assert_equals(vim.bo.filetype, "markdown", "PlanReview should use markdown")
  assert_true(vim.fn.maparg("oY", "n", false, true).callback ~= nil, "PlanReview accept mapping missing")
  assert_true(vim.fn.maparg("oN", "n", false, true).callback ~= nil, "PlanReview request-changes mapping missing")
  vim.fn.maparg("oY", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return request_by_method["plan.accept"] ~= nil end, 10), "PlanReview did not submit acceptance")
  assert_equals(#request_by_method["plan.accept"].params.digest, 64, "PlanReview should submit the exact saved digest")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan revise the feature" })
  controller.submit()
  assert_true(vim.wait(2000, function() return not session.harness.busy end, 10), "second planning turn did not complete")
  vim.ui.select = function(items, _, callback) callback(items[1]) end
  controller.open_artifact_picker()
  vim.ui.select = original_select
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10),
    "second artifact selection did not open PlanReview")
  local original_input = vim.ui.input
  vim.ui.input = function(options, callback)
    if options.prompt == "Plan comment: " then callback("Name the exact module")
    else callback("Include explicit integration coverage") end
  end
  vim.fn.maparg("C", "n", false, true).callback()
  vim.fn.maparg("oN", "n", false, true).callback()
  vim.ui.input = original_input
  assert_true(vim.wait(2000, function() return request_by_method["plan.request_changes"] ~= nil end, 10), "PlanReview did not request changes")
  assert_equals(request_by_method["plan.request_changes"].params.annotations[1].body, "Name the exact module",
    "PlanReview should serialize anchored annotations")
  assert_equals(request_by_method["plan.request_changes"].params.comment, "Include explicit integration coverage",
    "PlanReview should serialize the optional overall review comment")
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

  local new_session_result
  client.resolve_lease_conflict("new", { session_id = "busy-session" }, function(result, resolve_error)
    assert_true(resolve_error == nil, resolve_error)
    new_session_result = result
  end)
  assert_true(vim.wait(2000, function() return new_session_result ~= nil end, 10),
    "lease conflict should allow an independently initialized new session")
  assert_equals(request_by_method.initialize.params.lease_conflict_action, "new",
    "lease recovery should bypass the busy latest-session lease")

  local fork_result
  client.resolve_lease_conflict("fork", { session_id = "busy-session" }, function(result, resolve_error)
    assert_true(resolve_error == nil, resolve_error)
    fork_result = result
  end)
  assert_true(vim.wait(2000, function() return fork_result ~= nil end, 10),
    "lease conflict should fork through an independently leased broker")
  assert_equals(request_by_method["session.fork"].params.session_id, "busy-session",
    "lease recovery should fork the conflicted source session")
  assert_equals(fork_result.session.id, "session-fork", "lease recovery should return the fork snapshot")
  local harness_view = require("diff_review.views.harness")
  assert_true(not vim.iter(harness_view.lease_conflict_options({ native_fork = false })):any(function(option)
    return option.value == "fork"
  end), "lease recovery should hide fork when the backend cannot fork")
  assert_true(vim.iter(harness_view.lease_conflict_options({ native_fork = true })):any(function(option)
    return option.value == "fork"
  end), "lease recovery should expose fork when the persisted backend capability allows it")
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
