vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local ok, failure = pcall(function()
  require("diff_review").setup({ harness = { backend = "mock" } })
  local interaction_tree = require("diff_review.render.harness.interaction_tree")
  local status_render = require("diff_review.render.harness.timeline_status")
  local status_view = require("diff_review.views.harness.timeline_status")
  local presentation = require("diff_review.views.harness.question_presentation")

  assert_equals(status_render.frame_at(0), "⠋", "spinner should start at its first deterministic frame")
  assert_equals(status_render.frame_at(120), "⠙", "spinner should advance every 120ms")

  local rendered = interaction_tree.build({}, {
    timeline_status = { id = "subagents", text = "Waiting for 1 subagent" },
  })
  assert_equals(rendered.lines, { "", "  Waiting for 1 subagent" },
    "Timeline Status should be separated from history and omit prompt chrome")
  assert_equals(rendered.timeline_status_line, 2, "renderer should publish the status decoration row")

  local state = {
    status = { kind = "waiting_for_agent", agent_count = 2 },
    active_elicitation = nil,
  }
  assert_equals(status_view.resolve(state).text, "Waiting for 2 subagents",
    "subagent status should pluralize without elapsed time")
  state.status = { kind = "awaiting_plan_review", plan_id = "plan-1", revision = 1 }
  assert_equals(status_view.resolve(state).text, "Waiting for plan review (press op)",
    "submitted plans should expose the configured artifact-review key")
  state.status = { kind = "awaiting_input", owner = "interaction", interaction_id = "interaction-1" }
  state.active_elicitation = {
    owner = "interaction",
    interaction_id = "interaction-1",
    elicitation = { revision = 1, question_set = { id = "questions-1" } },
  }
  assert_equals(status_view.resolve(state).text, "Waiting for input (press oe)",
    "idle elicitation should expose the configured feedback key")
  state.busy = true
  state.status = { kind = "working", started_at_ms = os.time() * 1000, activity = "planning" }
  assert_equals(status_view.resolve(state).text, "Working for 0s",
    "Rust working state should use one request-lifecycle label during planning")
  state.status = {
    kind = "retrying_plan_generation",
    plan_id = "plan-1",
    turn = 2,
    max_turn = 20,
    started_at_ms = os.time() * 1000,
  }
  assert_equals(status_view.resolve(state).text, "Retrying plan generation 2/20",
    "Rust retry state should expose its bounded continuation progress")
  state.status = { kind = "planning_failed", plan_id = "plan-1", turn_count = 2 }
  assert_equals(status_view.resolve(state).text, "Plan generation stopped. Run /plan retry or /plan cancel",
    "failed planning should expose only the Rust-owned recovery commands")
  state.status = { kind = "idle" }
  assert_equals(status_view.resolve(state), nil, "structural idle status should remain invisible")
  state.busy = false

  local original_writefile = vim.fn.writefile
  local status_trace = {}
  local pending_elicitation = state.active_elicitation
  vim.fn.writefile = function(line_list, path, flags)
    status_trace[#status_trace + 1] = { line_list = line_list, path = path, flags = flags }
    return 0
  end
  state.active_elicitation = nil
  state.status = { kind = "waiting_for_agent", agent_count = 2 }
  local visible_status = status_view.resolve(state)
  status_view.record(state, visible_status)
  status_view.record(state, visible_status)
  state.busy = true
  status_view.record(state, status_view.resolve(state))
  vim.fn.writefile = original_writefile
  assert_equals(#status_trace, 2, "status logging should retain transitions and suppress duplicate frames")
  assert_equals(vim.json.decode(status_trace[1].line_list[1]).status_text, "Waiting for 2 subagents",
    "status trace should record the rendered text")
  assert_true(vim.json.decode(status_trace[2].line_list[1]).busy,
    "status trace should record the busy state that selected the rendered text")
  state.busy = false
  state.active_elicitation = pending_elicitation

  assert_true(presentation.should_present(state), "new question revisions should auto-present")
  presentation.mark_presented(state)
  assert_true(not presentation.should_present(state), "an unchanged revision should remain dismissed")
  state.active_elicitation.elicitation.revision = 2
  assert_true(presentation.should_present(state), "a replaced question revision should auto-present")

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, rendered.lines)
  state.transcript_buf = buf
  state.status = { kind = "planning_failed", plan_id = "plan-1", turn_count = 2 }
  status_view.synchronize(state, rendered.timeline_status_line)
  assert_equals(state.timeline_status_timer, nil, "terminal planning failure should not retain a spinner timer")
  state.status = { kind = "awaiting_input", owner = "interaction", interaction_id = "interaction-1" }
  status_view.synchronize(state, rendered.timeline_status_line)
  local before = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.wait(260, function() return false end, 20)
  assert_equals(vim.api.nvim_buf_get_lines(buf, 0, -1, false), before,
    "spinner animation should not mutate timeline buffer lines")
  status_view.stop(state)
  assert_equals(state.timeline_status_timer, nil, "status teardown should release its timer")
  vim.api.nvim_buf_delete(buf, { force = true })
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
