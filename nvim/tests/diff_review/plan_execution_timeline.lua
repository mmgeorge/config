vim.loader.enable(false)

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function segment(id, text, duration_ms)
  return {
    kind = "main_segment",
    segment = {
      id = id,
      state = "complete",
      started_at_ms = 0,
      completed_at_ms = duration_ms,
      duration_ms = duration_ms,
      token_count = 100,
      spawned_agent_count = 0,
      thought = { {
        id = id .. ":thought",
        text = text,
        tool = {},
      } },
    },
  }
end

local ok, failure = pcall(function()
  require("diff_review").setup({ harness = { backend = "mock" } })
  local renderer = require("diff_review.render.harness.interaction_tree")
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
        kind = "interaction",
        interaction = {
          id = "interaction-one",
          kind = "plan_execution",
          state = "complete",
          node_list = { segment("turn-one", "First continuation", 1000) },
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
        kind = "interaction",
        interaction = {
          id = "interaction-two",
          kind = "plan_execution",
          state = "complete",
          node_list = { segment("turn-two", "Second continuation", 2000) },
        },
      },
    },
  } })

  local summary_count = 0
  local text = table.concat(result.lines, "\n")
  for _, line in ipairs(result.lines) do
    if line:find("▸ Executed plan", 1, true) then summary_count = summary_count + 1 end
  end
  assert_equals(summary_count, 2, "each execution interaction should retain its own turn summary")
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
  assert_equals(text:find("Executed plan (", 1, true), nil,
    "provider task snapshots should not drive accepted-plan progress")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
