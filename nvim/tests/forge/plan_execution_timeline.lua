vim.loader.enable(false)

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function turn_content(id, text, duration_ms)
  local message_id = id .. ":message"
  return {
    node = { kind = "turn_content", id = id .. ":content", turn_id = id, item = { kind = "message", id = message_id } },
    turn = {
      id = id,
      state = { kind = "finished", outcome = "completed" },
      started_at_ms = 0,
      completed_at_ms = duration_ms,
      usage = { input = 100, cached_input = 0, reasoning = 0, output = 0 },
      tool = { order = {}, item = {} },
      message = { { id = message_id, kind = "assistant", delivery = "commentary", text = text } },
      item = { { kind = "message", id = message_id } },
    },
  }
end

local ok, failure = pcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  local renderer = require("forge.render.harness.interaction_tree")
  local result = renderer.build({ {
    kind = "plan_execution",
    id = "execution-one",
    execution = { state = "complete" },
    item = {
      {
        kind = "task_started",
        task_path = "/tasks/0",
        ordinal = 1,
        total = 2,
        title = "Add durable task state",
      },
      {
        kind = "exchange",
        exchange = {
          id = "interaction-one",
          kind = "plan_execution",
          state = "complete",
          duration_ms = 1000,
          node_list = { turn_content("turn-one", "First continuation", 1000).node },
          turn = { turn_content("turn-one", "First continuation", 1000).turn },
          task = { current = { { status = "completed" }, { status = "pending" } } },
        },
      },
      {
        kind = "deviation_recorded",
        deviation_id = "deviation-one",
        summary = "Add the missing input path",
      },
      {
        kind = "task_completed",
        task_path = "/tasks/0",
        ordinal = 1,
        total = 2,
        title = "Add durable task state",
        elapsed_ms = 34000,
      },
      {
        kind = "task_started",
        task_path = "/tasks/1",
        ordinal = 2,
        total = 2,
        title = "Render scheduler progress",
      },
      {
        kind = "exchange",
        exchange = {
          id = "interaction-two",
          kind = "plan_execution",
          state = "complete",
          duration_ms = 2000,
          node_list = { turn_content("turn-two", "Second continuation", 2000).node },
          turn = { turn_content("turn-two", "Second continuation", 2000).turn },
        },
      },
    },
  } })

  local summary_count = 0
  local text = table.concat(result.lines, "\n")
  for _, line in ipairs(result.lines) do
    if line:find("▾ Implementation stopped", 1, true) then summary_count = summary_count + 1 end
  end
  assert_equals(summary_count, 2, "each expanded exchange should retain its own summary")
  assert_equals(text:find("First continuation", 1, true) ~= nil, true,
    "execution timeline should retain the first continuation")
  assert_equals(text:find("Second continuation", 1, true) ~= nil, true,
    "execution timeline should retain the second continuation")
  assert_equals(text:find("▸ Task 1/2 started: Add durable task state", 1, true) ~= nil, true,
    "task start should render the canonical task title")
  assert_equals(text:find("✓ Task 1/2 completed in 34s", 1, true) ~= nil, true,
    "task completion should render elapsed scheduler time")
  assert_equals(text:find("▸ Task 2/2 started: Render scheduler progress", 1, true) ~= nil, true,
    "the next canonical task should render once")
  assert_equals(text:find("! Plan deviation recorded: Add the missing input path", 1, true) ~= nil, true,
    "persisted deviations should render immediately")
  assert_equals(text:find("Implementation stopped (", 1, true), nil,
    "provider task snapshots should not drive accepted-plan progress")

  for _, phase in ipairs({ "verify", "resolve" }) do
    for _, scenario in ipairs({
      { state = "running", label = phase == "verify" and "Verifying" or "Resolving" },
      { state = "complete", label = phase == "verify" and "Verification stopped" or "Resolution stopped" },
      { state = "complete", outcome = "passed", label = phase == "verify" and "Verified" or "Resolved" },
    }) do
      local rendered = renderer.build({ {
        kind = "exchange",
        exchange = {
          id = "phase-label", kind = "plan_execution", state = scenario.state,
          execution_phase = { phase = phase, outcome = scenario.outcome },
          duration_ms = 1000, execution_started_at_ms = 0, node_list = {}, turn = {},
        },
      } })
      assert_equals(table.concat(rendered.lines, "\n"):find(scenario.label .. " 1s", 1, true) ~= nil,
        true, "phase summaries should preserve the phase and outcome: " .. vim.inspect(rendered.lines))
    end
  end

  local exchange = {
    id = "verification", kind = "plan_execution", state = "complete", duration_ms = 18000,
    completed_at_ms = 18000, node_list = {}, turn = {},
    execution_phase = { phase = "verify", outcome = "failed", summary = "Ran eight tests.",
      findings = { "restart_resets_round: expected score 0, got 3", "pause_freezes_timer: time changed" } },
  }
  local collapsed = renderer.build({ { kind = "exchange", exchange = exchange } }, { expanded = {} })
  local collapsed_text = table.concat(collapsed.lines, "\n")
  assert(collapsed_text:find("▸ Verified 18s", 1, true))
  assert(collapsed_text:find("▸ Verification failed · restart_resets_round: expected score 0, got 3 (+1 more)", 1, true))
  assert(not collapsed_text:find("pause_freezes_timer", 1, true))
  local expanded = renderer.build({ { kind = "exchange", exchange = exchange } }, {
    expanded = { ["exchange:verification:verification-result"] = true },
  })
  assert(table.concat(expanded.lines, "\n"):find("pause_freezes_timer: time changed", 1, true))
  exchange.execution_phase = { phase = "verify", outcome = "passed", summary = "Eight tests passed.", findings = {} }
  local passed = renderer.build({ { kind = "exchange", exchange = exchange } }, { expanded = {} })
  assert(table.concat(passed.lines, "\n"):find("◇ Verification passed · Eight tests passed.", 1, true))
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
