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
  local cache = require("forge.views.harness.timeline_cache")
  local state = {
    session = { id = "session-one" },
  }
  cache.replace(state, {
    {
      kind = "exchange",
      id = "interaction-one",
      created_at_ms = 1,
      exchange = { id = "interaction-one", prompt = "inspect", state = "running" },
      agent_by_id = {},
    },
    {
      kind = "status",
      id = "session-one:status",
      created_at_ms = 0,
      status = { kind = "working", started_at_ms = 1 },
    },
  }, 4)

  local applied, patch_error = cache.apply(state, {
    session_id = "session-one",
    base_revision = 4,
    revision = 5,
    operation = { {
      kind = "replace",
      index = 1,
      entry = {
        kind = "status",
        id = "session-one:status",
        created_at_ms = 0,
        status = { kind = "awaiting_plan_review", plan_id = "plan-one", revision = 1 },
      },
    } },
  })
  assert_true(applied, patch_error)
  assert_equals(state.timeline_revision, 5, "patch application should advance exactly one session revision")
  assert_equals(state.status.kind, "awaiting_plan_review", "final status entry should own visible workflow status")
  assert_equals(#cache.history(state), 1, "status should remain synthetic and outside rendered history")

  local before = vim.deepcopy(state.timeline)
  applied, patch_error = cache.apply(state, {
    session_id = "session-one",
    base_revision = 3,
    revision = 6,
    operation = {},
  })
  assert_true(not applied and patch_error:find("revision gap", 1, true) ~= nil,
    "a stale base revision should request snapshot recovery")
  assert_equals(state.timeline, before, "a rejected patch should not partially mutate the local cache")

  applied = cache.apply(state, {
    session_id = "session-two",
    base_revision = 5,
    revision = 6,
    operation = {},
  })
  assert_true(not applied, "one session must reject another session's patch stream")

  state.timeline[1].exchange.turn = { { id = "turn-one", tool = { item = {
    tool = { output = "first", status = "inProgress" }, untouched = { output = "settled" },
  } } } }
  local previous_entry, previous_tool = state.timeline[1], state.timeline[1].exchange.turn[1].tool.item.tool
  local status_entry = state.timeline[2]
  applied, patch_error = cache.apply(state, {
    session_id = "session-one", base_revision = 5, revision = 6,
    operation = { { kind = "tool_output", index = 0, entry_id = "interaction-one",
      call_id = "turn-one:tool", delta = "\nλ" } },
  })
  assert_true(applied, patch_error)
  assert_equals(state.timeline[1].exchange.turn[1].tool.item.tool.output, "first\nλ")
  assert_true(state.timeline[2] == status_entry, "streaming replaced unrelated history")
  assert_true(state.timeline[1].exchange.turn[1].tool.item.untouched
    == previous_entry.exchange.turn[1].tool.item.untouched, "streaming copied an unrelated tool")
  assert_equals(previous_tool.output, "first", "streaming mutated the prior canonical owner")
  before = state.timeline[1]
  applied = cache.apply(state, {
    session_id = "session-one", base_revision = 6, revision = 7,
    operation = {
      { kind = "tool_output", index = 0, entry_id = "interaction-one", call_id = "turn-one:tool", delta = "discard" },
      { kind = "tool_output", index = 0, entry_id = "interaction-one", call_id = "missing", delta = "bad" },
    },
  })
  assert_true(not applied, "missing tool owner was admitted")
  assert_true(state.timeline[1] == before and state.timeline_revision == 6,
    "a rejected streaming batch partially changed history")
  state.timeline[1].exchange.turn[1].message = { { id = "message-one", text = "before", kind = "assistant" } }
  local retained_tool = state.timeline[1].exchange.turn[1].tool
  applied, patch_error = cache.apply(state, {
    session_id = "session-one", base_revision = 6, revision = 7,
    operation = { { kind = "message", index = 0, entry_id = "interaction-one",
      exchange_id = "interaction-one", turn_id = "turn-one",
      message = { id = "message-one", text = "after", kind = "assistant" } } },
  })
  assert_true(applied, patch_error)
  assert_equals(state.timeline[1].exchange.turn[1].message[1].text, "after")
  assert_true(state.timeline[1].exchange.turn[1].tool == retained_tool,
    "message streaming copied tool output")
  local tool = state.timeline[1].exchange.turn[1].tool.item.tool
  local previous_chunk = rawget(tool, "output_chunk")
  local expected = tool.output
  for revision = 8, 107 do
    applied, patch_error = cache.apply(state, {
      session_id = "session-one", base_revision = revision - 1, revision = revision,
      operation = { { kind = "tool_output", index = 0, entry_id = "interaction-one", call_id = "turn-one:tool", delta = " 🦀" } },
    })
    assert_true(applied, patch_error)
    tool = state.timeline[1].exchange.turn[1].tool.item.tool
    assert_true(rawget(tool, "output") == nil and rawget(tool, "output_materialized") == nil,
      "streaming eagerly rebuilt accumulated output")
    assert_true(rawget(tool, "output_chunk").previous == previous_chunk,
      "streaming copied retained output chunks")
    previous_chunk = rawget(tool, "output_chunk")
  end
  assert_equals(tool.output, expected .. string.rep(" 🦀", 100))
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
