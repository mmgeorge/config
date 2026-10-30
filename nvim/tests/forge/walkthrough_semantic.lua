vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local interactive = vim.g.forge_walkthrough_semantic_interactive == true
local fixture = vim.json.decode(table.concat(vim.fn.readfile("nvim/rust/forge/target/walkthrough-semantic-fixture.json"), "\n"))
local source = require("forge.source_document")
local stage, pending = 0, nil
local notices = {}
local function bind(value, native, document)
  for key, child in pairs(value) do
    if child == native then value[key] = document
    elseif type(child) == "table" then bind(child, native, document) end
  end
end
package.loaded["forge.client"] = { request = function(specification, callback)
  local params = specification.params
  if params.operation == "open" then
    callback({ inventory_state = "disabled", snapshot = { document = params.document, revision = 1, block = {
      { id = "change", text = { "Annotated change" }, metadata = { decoration = {}, editable_region = {}, target = {
        { id = "change", range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } },
      } } },
    } } })
  elseif params.operation == "review_input" then
    stage = stage + 1
    local transition = assert(fixture.transition[stage])
    assert(params.input.action == transition.action)
    local previous = stage == 1 and fixture.initial or fixture.transition[stage - 1].expected
    assert(params.input.revision == previous.revision)
    if stage == 1 and not interactive then pending = function() callback(transition.delivery) end else callback(transition.delivery) end
  else callback(vim.NIL) end
end }
source._set_runner_for_test(function(_, params, callback)
  if params.operation == "change" then
    bind(fixture, fixture.initial.document, params.annotation_document)
    callback({ title = "source.rs", object = "captured", revision = "captured", source_row = 0, more = false, review = true,
      review_diff_view = fixture.initial.diff_view,
      state = { state = "ready" }, snapshot = { document = params.document, revision = 0, block = {
        { id = "source", text = { "pub fn visible() {", "    updated();", "}" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
      } }, annotation = fixture.initial })
  else callback({}) end
end)
local walkthrough = require("forge.walkthrough")
local state = walkthrough.open({ on_error = function(message) notices[#notices + 1] = message end })
local function settled()
  assert(vim.wait(2000, function() return not state.opening and not state.pending and #state.queue == 0 end), table.concat(notices, "\n"))
end
settled()
walkthrough.open_change(state)
settled()
local owner = assert(state.source[1])
if interactive then
  _G.ForgeWalkthroughSemanticFixture = { state = state, owner = owner, stage = function() return stage end, notices = notices }
  vim.cmd("redraw")
  return
end
local function key(value)
  local mapping = vim.fn.maparg(value, "n", false, true)
  assert(mapping.callback and mapping.buffer == 1, "missing " .. value)
  mapping.callback()
end
for row, text in ipairs(vim.api.nvim_buf_get_lines(owner.annotation.buffer, 0, -1, false)) do
  if text:find("updated();", 1, true) then vim.api.nvim_win_set_cursor(0, {row, 0}) break end
end
key("ox")
assert(vim.wait(1000, function() return pending ~= nil end))
key("ov")
key("ov")
pending()
settled()
assert(stage == 1 and owner.diff_view.presentation == "semantic" and owner.diff_view.visibility == "all")
assert(vim.api.nvim_get_current_line():find("pub fn visible() {", 1, true),
  "collapsing a source body moved the cursor into its annotation")
local function signature()
  for row, text in ipairs(vim.api.nvim_buf_get_lines(owner.annotation.buffer, 0, -1, false)) do
    if text:find("pub fn visible() {", 1, true) then vim.api.nvim_win_set_cursor(0, {row, 0}) return end
  end
  error("semantic signature disappeared")
end
signature()
for _, value in ipairs({ "<Tab>", "ov", "ox", "ov" }) do
  local expected = stage + 1
  key(value)
  settled()
  assert(stage == expected)
  local rows = table.concat(vim.api.nvim_buf_get_lines(owner.annotation.buffer, 0, -1, false), "\n")
  assert(rows:find("Captured contract", 1, true))
  assert(rows:find("updated();", 1, true))
end
assert(owner.diff_view.presentation == "full" and owner.diff_view.visibility == "all")
assert(#notices == 0, table.concat(notices, "\n"))
walkthrough.close(state)
source._set_runner_for_test(nil)
print("walkthrough_semantic: native controls, body expansion, parity, annotation preservation, and cached Full restoration passed")
