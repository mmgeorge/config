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
    active_wait = { agent_count = 2 },
    active_elicitation = nil,
  }
  assert_equals(status_view.resolve(state).text, "Waiting for 2 subagents",
    "subagent status should pluralize without elapsed time")
  state.active_elicitation = {
    owner = "interaction",
    interaction_id = "interaction-1",
    elicitation = { revision = 1, question_set = { id = "questions-1" } },
  }
  assert_true(status_view.resolve(state).text:find("Waiting for input", 1, true) == 1,
    "input status should take precedence over subagent waiting")

  assert_true(presentation.should_present(state), "new question revisions should auto-present")
  presentation.mark_presented(state)
  assert_true(not presentation.should_present(state), "an unchanged revision should remain dismissed")
  state.active_elicitation.elicitation.revision = 2
  assert_true(presentation.should_present(state), "a replaced question revision should auto-present")

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, rendered.lines)
  state.transcript_buf = buf
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
