-- Unit tests for `forge.source_document` (source_document.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["saved_diff_controls"] = function()
  vim.o.columns = 160
  local fixture_path = "nvim/lua/forge/tests/fixtures/protocol/saved-diff-controls.json"
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
      if stage == 1 then
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
  wait_until(function() return state.replica.status == "Applied" and state.commands ~= nil end,
    "saved diff did not apply its initial snapshot")
  local first_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_cursor(first_window, {3, 0})
  vim.cmd("split")
  local second_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_cursor(second_window, {4, 0})
  wait_until(function() return vim.tbl_count(state.view) == 2 end, "split did not register a second saved diff view")
  local expected_line = {}
  for _, window in ipairs({first_window, second_window}) do
    expected_line[window] = buffer_support.line(state.replica.buffer, vim.api.nvim_win_get_cursor(window)[1])
  end
  local function invoke(key)
    assert(keymap.get(key).buffer == 1, "missing " .. key)
    keymap.invoke(key)
  end
  invoke("ox")
  wait_until(function() return stage == 1 and pending ~= nil end, "ox did not request the first saved diff control")
  invoke("ov")
  invoke("ov")
  vim.api.nvim_win_set_cursor(second_window, {5, 0})
  expected_line[second_window] = "+after();"
  pending()
  wait_until(function() return not state.pending and state.replica.status == "Applied" and state.diff_view.presentation == "semantic" end,
    "held saved diff control did not apply its semantic presentation")
  assert(stage == 1 and state.diff_view.presentation == "semantic" and state.diff_view.visibility == "all")
  for _, key in ipairs({"ov", "ox", "ov"}) do
    local expected = stage + 1
    invoke(key)
    wait_until(function() return stage == expected and not state.pending end, key .. " did not settle its saved diff control")
    eq(state.diff_view, current.diff_view)
    for _, window in ipairs({first_window, second_window}) do
      local row = vim.api.nvim_win_get_cursor(window)[1]
      assert(buffer_support.line(state.replica.buffer, row) == expected_line[window], "control moved a surviving source row")
    end
  end
  local raw = buffer_support.text(state.replica.buffer)
  assert(raw:find("@@ -20 +20 @@", 1, true) and raw:find("+after();", 1, true))
  assert(not raw:find("Unclassified", 1, true))
  assert(#errors == (recovery and 1 or 0), table.concat(errors, "\n"))
  assert(snapshots == (recovery and 1 or 0), "unexpected recovery request count")
  adapter.close(state)
end

T["stale"] = function()
  local root = vim.fn.getcwd()
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
  local origin = vim.api.nvim_get_current_buf()
  buffer_support.set(origin, { "first", "second" })
  local owner = source.open({ workspace = root, revision = "cached", path = "sample.txt" })
  deliver()
  wait_until(function() return vim.api.nvim_get_current_buf() == owner.replica.buffer end, "cached source did not become current")
  vim.api.nvim_win_set_buf(0, origin)
  for _, revision in ipairs({ "cached", "new" }) do
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    local state = source.open({ workspace = root, revision = revision, path = "sample.txt", is_current = function()
      return vim.api.nvim_get_current_buf() == origin and vim.api.nvim_win_get_cursor(0)[1] == 1
    end })
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local document = deliver()
    wait_until(function() return closed[document] end, "stale native document was not closed")
    assert(vim.api.nvim_get_current_buf() == origin, "stale response replaced the origin buffer")
    assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "stale response moved the cursor")
    assert(not state.active, "stale source owner remained active")
  end
  source.close(owner)

  vim.bo[origin].buflisted = false
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  local revision = source.open({ workspace = root, revision = "return", path = "sample.txt" })
  deliver()
  wait_until(function() return vim.api.nvim_get_current_buf() == revision.replica.buffer end, "returning revision did not become current")
  keymap.invoke("q")
  assert(vim.api.nvim_get_current_buf() == origin, "closing revision lost its unlisted origin")
  assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "closing revision lost its origin cursor")

  local cached = source.open({ workspace = root, revision = "return-cached", path = "sample.txt" })
  deliver()
  wait_until(function() return vim.api.nvim_get_current_buf() == cached.replica.buffer end, "cached revision did not become current")
  local first_window = vim.api.nvim_get_current_win()
  vim.cmd("vnew")
  local second_window, second_origin = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_buf()
  vim.bo[second_origin].buflisted = false
  local reused
  source.open({ workspace = root, revision = "return-cached", path = "sample.txt",
    on_ready = function(state) reused = state end })
  deliver()
  wait_until(function() return reused ~= nil end, "source reopen did not report readiness")
  assert(reused == cached, "source reopen did not reuse the cached revision")
  keymap.invoke("q")
  assert(vim.api.nvim_win_get_buf(first_window) == origin, "cached close lost its first window origin")
  assert(vim.api.nvim_win_get_buf(second_window) == second_origin, "cached close lost its second window origin")
  vim.api.nvim_win_close(second_window, true)

  local hidden = source.open({ workspace = root, revision = "return-hidden", path = "sample.txt" })
  deliver()
  wait_until(function() return vim.api.nvim_get_current_buf() == hidden.replica.buffer end, "hidden revision did not become current")
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
  wait_until(function() return vim.api.nvim_get_current_buf() == typed.replica.buffer end, "typed revision did not become current")
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
  wait_until(function() return vim.api.nvim_get_current_buf() == markdown.replica.buffer end, "markdown revision did not become current")
  assert(attached_buffer == markdown.replica.buffer, "upstream markdown filetype attachment did not run")
  assert(highlighter.active[markdown.replica.buffer] == nil, "native source retained an upstream filetype highlighter")
  assert(vim.bo[markdown.replica.buffer].filetype == "markdown", "native source lost markdown filetype")
  source.close(markdown)
end

return T
