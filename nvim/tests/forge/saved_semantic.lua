vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local interactive = vim.g.forge_saved_semantic_interactive == true
local fixture = vim.json.decode(table.concat(vim.fn.readfile("nvim/rust/forge/target/saved-semantic-fixture.json"), "\n"))
local adapter = require("forge.source_document")
local stage, state = 0, nil
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
    stage = stage + 1
    local transition = assert(fixture.transition[stage], "unexpected source action " .. params.input.action)
    assert(params.input.action == transition.action, params.input.action .. " instead of " .. transition.action)
    local previous = stage == 1 and fixture.initial.snapshot or fixture.transition[stage - 1].expected
    assert(params.input.revision == previous.revision)
    local block
    for _, candidate in ipairs(previous.block) do if candidate.id == params.input.block then block = candidate break end end
    assert(block and params.input.position.row >= 0 and params.input.position.row < #block.text)
    if params.input.action == "demand" then assert(params.input.block:match(":file:1:.*:more$")) end
    if params.input.action == "source_expand" or params.input.action == "source_collapse" then
      assert(block.text[1] == "pub fn visible() {")
    end
    callback(transition.delivery)
  elseif params.operation == "close" or params.operation == "close_view" then callback({})
  else error("unexpected source request " .. params.operation) end
end)
state = adapter.open_harness_diff({ session_id = "saved-semantic", input = {}, on_error = function(failure) errors[#errors + 1] = failure end })
assert(vim.wait(2000, function() return state.replica.status == "Applied" and stage == 1 and not state.pending end))
if interactive then
  _G.ForgeSavedSemanticFixture = { state = state, stage = function() return stage end, errors = errors }
  vim.cmd("redraw")
  return
end
local function invoke(key, expected)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback and mapping.buffer == 1, "missing " .. key)
  mapping.callback()
  assert(vim.wait(2000, function() return stage == expected and not state.pending end), table.concat(errors, "\n"))
  assert(state.replica.status == "Applied")
end
local function heading()
  local rows = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)
  for row, text in ipairs(rows) do
    if text:find("pub fn visible() {", 1, true) then
      vim.api.nvim_win_set_cursor(0, {row, 0})
      local location = require("forge.buffer").locate(state.replica, row - 1, 0)
      return state.replica.sequence.node[location.block].entry.metadata.content_node
    end
  end
  error("missing semantic heading")
end
invoke("ox", 2)
local nodes = require("forge.nodes")
assert(nodes.closed(state.replica, heading()))
invoke("<Tab>", 3)
assert(not nodes.closed(state.replica, heading()))
vim.cmd("split")
assert(vim.wait(1000, function() return vim.tbl_count(state.view) == 2 end))
invoke("ov", 4)
for window in pairs(state.view) do
  assert(vim.wo[window].winbar:find("Public", 1, true))
  assert(vim.wo[window].winbar:find("Semantic", 1, true))
end
invoke("ox", 5)
assert(state.diff_view.visibility == "public" and state.diff_view.presentation == "full")
invoke("ox", 6)
assert(not nodes.closed(state.replica, heading()))
invoke("<Tab>", 7)
assert(nodes.closed(state.replica, heading()))
assert(#errors == 0, table.concat(errors, "\n"))
adapter.close(state)
print("saved_semantic: native viewport demand, folds, cached body, independent controls, and split headers passed")
