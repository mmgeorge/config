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
  local cache = require("diff_review.views.harness.timeline_cache")
  local state = {
    session = { id = "session-one" },
  }
  cache.replace(state, {
    {
      kind = "interaction",
      id = "interaction-one",
      created_at_ms = 1,
      interaction = { id = "interaction-one", prompt = "inspect", state = "running" },
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
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
