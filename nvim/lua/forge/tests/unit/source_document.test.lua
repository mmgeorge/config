-- Unit tests for `forge.source_document` (source_document.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "saved_diff_controls", run = function()
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
  end },
  { name = "stale", run = function()
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local source = require("forge.source_document")
local pending, closed = {}, {}
source._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    pending[#pending + 1] = { params = params, callback = callback }
  else
    if params.operation == "close" then closed[params.document] = true end
    callback({})
  end
end)
local function deliver()
  local request = table.remove(pending, 1)
  request.callback({ title = request.params.revision, object = request.params.revision, revision = request.params.revision,
    more = false, state = { state = "ready" }, snapshot = { document = request.params.document, revision = 1,
      block = { { id = "source", text = { "captured source" }, metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  return request.params.document
end
local ok, failure = xpcall(function()
  local origin = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(origin, 0, -1, false, { "first", "second" })
  local owner = source.open({ workspace = root, revision = "cached", path = "sample.txt" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == owner.replica.buffer end))
  vim.api.nvim_win_set_buf(0, origin)
  for _, revision in ipairs({ "cached", "new" }) do
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    local state = source.open({ workspace = root, revision = revision, path = "sample.txt", is_current = function()
      return vim.api.nvim_get_current_buf() == origin and vim.api.nvim_win_get_cursor(0)[1] == 1
    end })
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local document = deliver()
    assert(vim.wait(1000, function() return closed[document] end), "stale native document was not closed")
    assert(vim.api.nvim_get_current_buf() == origin, "stale response replaced the origin buffer")
    assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "stale response moved the cursor")
    assert(not state.active, "stale source owner remained active")
  end
  source.close(owner)

  vim.bo[origin].buflisted = false
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  local revision = source.open({ workspace = root, revision = "return", path = "sample.txt" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == revision.replica.buffer end))
  vim.fn.maparg("q", "n", false, true).callback()
  assert(vim.api.nvim_get_current_buf() == origin, "closing revision lost its unlisted origin")
  assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "closing revision lost its origin cursor")

  local cached = source.open({ workspace = root, revision = "return-cached", path = "sample.txt" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == cached.replica.buffer end))
  local first_window = vim.api.nvim_get_current_win()
  vim.cmd("vnew")
  local second_window, second_origin = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_buf()
  vim.bo[second_origin].buflisted = false
  local reused
  source.open({ workspace = root, revision = "return-cached", path = "sample.txt",
    on_ready = function(state) reused = state end })
  deliver()
  assert(vim.wait(1000, function() return reused ~= nil end))
  assert(reused == cached, "source reopen did not reuse the cached revision")
  vim.fn.maparg("q", "n", false, true).callback()
  assert(vim.api.nvim_win_get_buf(first_window) == origin, "cached close lost its first window origin")
  assert(vim.api.nvim_win_get_buf(second_window) == second_origin, "cached close lost its second window origin")
  vim.api.nvim_win_close(second_window, true)

  local hidden = source.open({ workspace = root, revision = "return-hidden", path = "sample.txt" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == hidden.replica.buffer end))
  local unrelated = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_win_set_buf(0, unrelated)
  source.close(hidden)
  assert(vim.api.nvim_get_current_buf() == unrelated, "hidden source close replaced an unrelated buffer")

  local native_filetype_event = false
  local filetype_group = vim.api.nvim_create_augroup("ForgeSourceFiletypeTest", { clear = true })
  vim.api.nvim_create_autocmd("FileType", { group = filetype_group, pattern = "rust", callback = function(event)
    assert(vim.b[event.buf].forge_native_document == true, "source filetype ran before native ownership")
    assert(not require("forge.native_syntax").attach_global_parser(event.buf), "source filetype admitted a global parser")
    native_filetype_event = true
  end })
  local typed = source.open({ workspace = root, revision = "return-typed", path = "sample.rs" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == typed.replica.buffer end))
  assert(native_filetype_event and vim.bo[typed.replica.buffer].filetype == "rust", "revision lost its source filetype")
  assert(vim.bo[typed.replica.buffer].buflisted, "revision must remain listed in buffer pickers")
  assert(vim.bo[typed.replica.buffer].buftype == "nowrite", "revision must preserve nowrite semantics")
  assert(vim.bo[typed.replica.buffer].readonly and not vim.bo[typed.replica.buffer].modifiable, "revision permitted native writes")
  assert(vim.bo[typed.replica.buffer].bufhidden == "hide", "revision must persist while hidden")
  vim.api.nvim_win_set_buf(0, unrelated)
  assert(vim.api.nvim_buf_is_loaded(typed.replica.buffer) and typed.active, "hiding revision closed its source document")
  source.close(typed)
  local highlighter = vim.treesitter.highlighter
  local attached_buffer
  vim.api.nvim_create_autocmd("FileType", { group = filetype_group, pattern = "markdown", callback = function(event)
    attached_buffer = event.buf
    highlighter.active[event.buf] = { destroy = function() highlighter.active[event.buf] = nil end }
  end })
  local markdown = source.open({ workspace = root, revision = "return-markdown", path = "sample.md" })
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == markdown.replica.buffer end))
  assert(attached_buffer == markdown.replica.buffer, "upstream markdown filetype attachment did not run")
  assert(highlighter.active[markdown.replica.buffer] == nil, "native source retained an upstream filetype highlighter")
  assert(vim.bo[markdown.replica.buffer].filetype == "markdown", "native source lost markdown filetype")
  source.close(markdown)
  vim.api.nvim_del_augroup_by_id(filetype_group)
end, debug.traceback)
source._set_runner_for_test(nil)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit") end
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
