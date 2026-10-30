vim.opt.runtimepath:append("nvim")
local commit_fixture = vim.g.forge_commit_semantic_fixture == true
local fixture_path = "nvim/rust/forge/target/" .. (commit_fixture and "review-commit-semantic-fixture.json" or "review-semantic-fixture.json")
assert(vim.fn.filereadable(fixture_path) == 1,
  "run file_diff_controls_share_visibility_and_preserve_lazy_source_actions first")
local fixture = vim.json.decode(table.concat(vim.fn.readfile(fixture_path), "\n"))
local adapter = require("forge.review_document")
local nodes = require("forge.nodes")
local stage = 0
local current = fixture.initial
local errors = {}
adapter._set_runner_for_test(function(method, params, callback)
  if method == "github.review.document.open" then
    callback({ document = current.snapshot.document, diff_view = current.diff_view })
  elseif method == "github.actor.get" then callback({ login = "viewer" })
  elseif method == "github.review.document.header.load" then callback({ ready = true })
  elseif method == "github.review.document.render" then callback(current)
  elseif method == "github.review.document.view.set" then callback(vim.NIL)
  elseif method == "github.review.document.load" then callback({ diagnostic = {} })
  elseif method == "github.review.document.input.apply" then
    local transition = assert(fixture.transition[stage + 1], "unexpected source action")
    assert(params.input.action == transition.action, params.input.action .. " differs from " .. transition.action)
    stage, current = stage + 1, transition.expected
    callback(transition.delivery)
  elseif method == "github.review.document.close" then callback({ closed = true })
  else error("unexpected route " .. method) end
end)
local state = adapter.open({ directory = vim.fn.getcwd(), target = { number = 7 },
  on_error = function(message) errors[#errors + 1] = message end })
assert(vim.wait(1000, function() return state.shown and state.view_ready end))
for identity, record in pairs(state.replica.fold.record) do
  if record.owner:match("section:Files:.*:title$") or (commit_fixture and record.owner == "section:Commits:heading") then
    assert(nodes.set_open(state.replica, state.window, identity, true))
  end
end
assert(vim.wait(1000, function() return not state.replica.update_pending end))
local function source_cursor()
  local fallback
  for row, text in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
    local location = require("forge.buffer").locate(state.replica, row - 1, 0)
    local desired = not commit_fixture or (location and location.block:find("commit:" .. string.rep("f", 40) .. ":", 1, true) == 1)
    if desired and text:find("pub fn visible() {", 1, true) then
      vim.api.nvim_win_set_cursor(state.window, { row, 0 })
      return
    end
    if desired and text:find(commit_fixture and "first_commit()" or "updated()", 1, true) then fallback = row end
  end
  if fallback then vim.api.nvim_win_set_cursor(state.window, { fallback, 0 }) return end
  error("public declaration is absent " .. vim.inspect({ folds = state.replica.fold.record,
    choices = state.replica.projection.choice,
    rendered = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false) }))
end
local function invoke(key, expected)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.buffer == 1 and mapping.callback, "missing " .. key)
  mapping.callback()
  assert(vim.wait(1000, function() return stage == expected and not state.action_running end))
  assert(state.replica.status == "Applied" and #errors == 0, table.concat(errors, "\n"))
  assert(vim.deep_equal(state.diff_view, current.diff_view))
end
if vim.g.forge_semantic_interactive then
  _G.ForgeSemanticFixture = { state = state, source_cursor = source_cursor, stage = function() return stage end }
  source_cursor()
  vim.cmd("redraw")
  return
end
source_cursor()
invoke("ox", 1)
assert(table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n"):find("{ … 1 change }", 1, true),
  "comment projection omitted the shared semantic summary")
local visible_heading
for identity, record in pairs(state.replica.fold.record) do
  local entry = state.replica.block[record.owner]
  if entry and entry.metadata.content_node == identity and nodes.closed(state.replica, identity) then
    visible_heading = true
  end
end
assert(visible_heading, "semantic declarations did not start closed")
local first_window = state.window
vim.cmd("vsplit")
assert(vim.wait(1000, function() return state.window == vim.api.nvim_get_current_win() and state.view_ready end))
assert(state.window ~= first_window and vim.tbl_count(state.window_view) == 2,
  "a second window did not acquire its own native input view")
source_cursor()
invoke("<Tab>", 2)
assert(vim.api.nvim_get_current_line():find("pub fn visible() {", 1, true),
  "source expansion moved the cursor away from its retained declaration")
invoke("ov", 3)
for _, window in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do
  assert(vim.wo[window].winbar:find("Public", 1, true), "visibility did not update both document windows")
end
assert(not table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n"):find("hidden", 1, true))
invoke("ox", 4)
invoke("ov", 5)
local rendered = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n")
assert(rendered:find(commit_fixture and "first_commit()" or "updated()", 1, true)
  and rendered:find(commit_fixture and "first_private()" or "new_private()", 1, true))
if commit_fixture then
  assert(rendered:find("second_commit()", 1, true) and rendered:find("pr_current()", 1, true))
end
adapter.close(state)
assert(state.hidden and not vim.bo[state.replica.buffer].modified)
vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
assert(vim.wait(1000, function() return not state.active end))
print("review_semantic: native patches, lazy source folds, independent controls, and retained source actions passed")
