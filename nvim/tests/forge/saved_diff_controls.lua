vim.opt.runtimepath:append("nvim")
local interactive = vim.g.forge_saved_diff_interactive == true
if not interactive then vim.o.columns = 160 end
local fixture_path = "nvim/rust/forge/target/saved-diff-controls-fixture.json"
assert(vim.fn.filereadable(fixture_path) == 1, "run saved_diff_controls_preserve_unclassified_patch_and_independent_options first")
local fixture = vim.json.decode(table.concat(vim.fn.readfile(fixture_path), "\n"))
local adapter = require("forge.source_document")
local stage, pending, current = 0, nil, fixture.initial
local recovery = vim.g.forge_saved_diff_recovery == true
local snapshots = 0
local errors = {}
local function bind_document(value, native, document)
  for key, child in pairs(value) do
    if child == native then value[key] = document
    elseif type(child) == "table" then bind_document(child, native, document) end
  end
end
adapter._set_runner_for_test(function(method, params, callback)
  if params.operation == "open_diff" then
    assert(method == "session.document")
    bind_document(fixture, fixture.initial.snapshot.document, params.document)
    callback(fixture.initial)
  elseif params.operation == "demand" then
    assert(method == "repository.source.document")
    stage = stage + 1
    local transition = assert(fixture.transition[stage], "unexpected saved diff control")
    assert(params.input.action == transition.action)
    assert(params.input.revision == current.snapshot.revision)
    current = { snapshot = transition.expected, diff_view = transition.delivery.diff_view }
    if stage == 1 and not interactive then
      local delivery = vim.deepcopy(transition.delivery)
      if recovery then delivery.patch.base = delivery.patch.base + 100 end
      pending = function() callback(delivery) end
    else callback(transition.delivery) end
  elseif params.operation == "close_view" then callback({})
  elseif params.operation == "close" then callback({ closed = true })
  elseif params.operation == "snapshot" then
    snapshots = snapshots + 1
    callback(vim.tbl_extend("force", current.snapshot, { diff_view = current.diff_view, more = false }))
  else error("unexpected source request " .. params.operation) end
end)
local state = adapter.open_harness_diff({ session_id = "saved-fixture", input = {},
  on_error = function(failure) errors[#errors + 1] = failure end })
assert(vim.wait(1000, function() return state.replica.status == "Applied" and state.commands ~= nil end))
if interactive then
  _G.ForgeSavedDiffFixture = { state = state, stage = function() return stage end, errors = errors }
  vim.cmd("redraw")
  return
end
local first_window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_cursor(first_window, {3, 0})
vim.cmd("split")
local second_window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_cursor(second_window, {4, 0})
assert(vim.wait(1000, function() return vim.tbl_count(state.view) == 2 end))
local expected_line = {}
for _, window in ipairs({first_window, second_window}) do
  expected_line[window] = vim.api.nvim_buf_get_lines(state.replica.buffer,
    vim.api.nvim_win_get_cursor(window)[1] - 1, vim.api.nvim_win_get_cursor(window)[1], false)[1]
end
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.buffer == 1 and mapping.callback, "missing " .. key)
  mapping.callback()
end
invoke("ox")
assert(vim.wait(1000, function() return stage == 1 and pending ~= nil end))
invoke("ov")
invoke("ov")
vim.api.nvim_win_set_cursor(second_window, {5, 0})
expected_line[second_window] = "+after();"
pending()
assert(vim.wait(1000, function() return not state.pending and state.replica.status == "Applied" and state.diff_view.presentation == "semantic" end))
assert(stage == 1 and state.diff_view.presentation == "semantic" and state.diff_view.visibility == "all")
for _, key in ipairs({"ov", "ox", "ov"}) do
  local expected = stage + 1
  invoke(key)
  assert(vim.wait(1000, function() return stage == expected and not state.pending end))
  assert(vim.deep_equal(state.diff_view, current.diff_view))
  for _, window in ipairs({first_window, second_window}) do
    local header = vim.wo[window].winbar
    assert(header:find(state.diff_view.visibility == "public" and "Public" or "All", 1, true), "visibility header: " .. header)
    assert(header:find(state.diff_view.presentation == "semantic" and "Semantic" or "Full", 1, true), "presentation header: " .. header)
    local row = vim.api.nvim_win_get_cursor(window)[1]
    assert(vim.api.nvim_buf_get_lines(state.replica.buffer, row - 1, row, false)[1] == expected_line[window], "control moved a surviving source row")
  end
end
local raw = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n")
assert(raw:find("@@ -20 +20 @@", 1, true) and raw:find("+after();", 1, true))
assert(not raw:find("Unclassified", 1, true))
assert(#errors == (recovery and 1 or 0), table.concat(errors, "\n"))
assert(snapshots == (recovery and 1 or 0), "unexpected recovery request count")
adapter.close(state)
print("saved_diff_controls: native fallback, independent controls, parity, split headers, and source positions passed")
