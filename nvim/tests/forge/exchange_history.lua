vim.loader.enable(false)

local ok, failure = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  local renderer = require("forge.render.harness.interaction_tree")
  local earlier = {
    id = "earlier", state = "complete", completed_at_ms = 1000, duration_ms = 1000, turn = {},
  }
  local later = { id = "later", state = "running", execution_started_at_ms = 2000, turn = {} }
  local child = {
    kind = "agent_lifecycle", id = "delegation",
    run = { id = "child", definition = "reviewer", state = "ready" },
    exchange = { earlier, later },
  }
  local parent = {
    id = "parent", prompt = "Review", state = "complete", duration_ms = 1000, turn = {},
    node_list = { { kind = "agent_reference", agent = {
      id = "delegation", child_agent_id = "child", child_exchange_id = "earlier", task = "Inspect",
    } } },
  }
  for _, lookup in ipairs({ { delegation = child }, { child = child } }) do
    local tree = renderer.build({ { kind = "exchange", exchange = parent, agent_by_id = lookup } }, {
      expanded = { ["exchange:parent"] = true }, content_width = 80, now_ms = 9000,
    })
    local rows = vim.tbl_filter(function(row) return row.kind == "agent_lifecycle" end, tree.rows)
    assert(vim.tbl_count(rows) == 1, "canonical delegation must render exactly one child row")
    local text = table.concat(tree.lines, "\n")
    assert(not text:find("running", 1, true), "later child reuse must not restart the earlier delegation")
    assert(#child.exchange == 2, "preview must not mutate source history")
  end

  local collapsed = renderer.build({ { kind = "exchange", exchange = parent, agent_by_id = { child = child } } }, {
    expanded = {}, content_width = 80, now_ms = 9000,
  })
  assert(#collapsed.lines == 2, "collapsed exchange must hide delegated activity")
  local waiting = vim.deepcopy(parent)
  waiting.id, waiting.state, waiting.awaiting_input = "waiting", "running", true
  waiting.execution_started_at_ms = nil
  local frozen = renderer.build({ waiting }, { expanded = {}, working_seconds = 90, now_ms = 90000 })
  assert(frozen.lines[2]:find("for 1s", 1, true), "another request's clock must not advance a waiting exchange")

  local state = require("forge.session").harness
  state.busy, state.no_checkpoint = false, false
  local history = {}
  for _, spec in ipairs({
    { "complete", "current" }, { "interrupted", "current" },
    { "complete", "rolled_back" }, { "failed", "superseded" }, { "running", "current" },
  }) do
    history[#history + 1] = {
      id = tostring(#history + 1), ordinal = #history + 1, prompt = "work",
      state = spec[1], disposition = spec[2], checkpoint_before = "checkpoint",
    }
  end
  require("forge.client").request = function(method, _, callback)
    assert(method == "exchange.list")
    callback(history)
  end
  local options
  require("forge.views.picker").open = function(value) options = value.page_list[1].option_list end
  require("forge.views.harness.controller").open_undo_picker()
  assert(options and #options == 2, "undo must include only current settled history")
  assert(options[1].id == "2" and options[2].id == "1", "interrupted history must remain undoable")
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("exchange_history: passed")
  vim.cmd("qa!")
end
