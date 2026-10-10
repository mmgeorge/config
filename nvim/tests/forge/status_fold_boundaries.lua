vim.opt.runtimepath:prepend("nvim")
local render = require("forge.status_render")
local input = require("forge.input")
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
local notices = {}
local replica = render.open("status-fold-boundaries", { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_set_current_buf(replica.buffer)
local function record(id)
  return { id = id, generation = 1, section = "unstaged", change = "modified", path = id .. ".lua",
    header = fixture.header(id .. ".lua"), untracked = false, stats = { state = "unknown" } }
end
assert(render.apply_snapshot(replica, { document = replica.document, revision = 0, view = { kind = "status" },
  head = { state = "attached", reference = "refs/heads/main", object = "abc123" }, context = vim.NIL,
  section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1), record(2) } }).kind == "Applied")
local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
vim.cmd("4foldopen")
local function metadata(definitions)
  return { target = {}, decoration = {}, editable_region = {}, fold = definitions or {} }
end
assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
  snapshot = { document = "body:1:1", revision = 1, block = {
    { id = "hunk", text = { "@@ hunk" }, metadata = metadata({ { id = "hunk", start = { row = 0, column = 0 },
      ["end"] = { block = "end", position = { row = 0, column = 0 } }, closed = false } }) },
    { id = "content", text = { "first", "second" }, metadata = metadata() },
    { id = "end", text = {}, metadata = metadata() },
  } } }).kind == "Applied", table.concat(notices, "\n"))

local previous = 2
for _, count in ipairs({ 5, 1, 0, 4 }) do
  local text = {}
  for row = 1, count do text[row] = "line " .. row end
  local body = replica.file[1].body
  assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
    patch = { document = body.document, base = body.revision, next = body.revision + 1,
      base_rows = previous + 1, next_rows = count + 1, base_blocks = 3, next_blocks = 3,
      text_edit = { { start_row = 1, removed_rows = previous, text = text } }, block_edit = {}, removed_block = {},
      metadata_edit = { { block = "content", row_count = count, metadata = metadata() } } },
  }).kind == "Applied", table.concat(notices, "\n"))
  for row = 5, 5 + count do
    assert(vim.fn.foldlevel(row) == 3, "body update lost a section, file, or hunk fold at " .. row)
  end
  assert(vim.fn.foldlevel(4) == 2 and vim.fn.foldclosed(4) == -1, "body update changed its file state")
  assert(vim.fn.foldclosed(6 + count) == 6 + count, "body update opened the next file")
  previous = count
end
assert(#notices == 0, table.concat(notices, "\n"))
local body = replica.file[1].body
local revision, text = body.revision, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
local invalid = render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
  patch = { document = body.document, base = revision, next = revision + 1,
    base_rows = 5, next_rows = 6, base_blocks = 3, next_blocks = 3,
    text_edit = { { start_row = 1, removed_rows = 4, text = { "replacement" } } }, block_edit = {}, removed_block = {},
    metadata_edit = { { block = "content", row_count = 5, metadata = metadata() } } },
})
assert(invalid.kind == "Desynchronized" and #notices == 1, "invalid patch did not report failure")
assert(body.revision == revision and vim.deep_equal(text, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)),
  "invalid patch changed the committed body")
assert(vim.fn.foldlevel(9) == 3 and vim.fn.foldclosed(10) == 10, "invalid patch changed native folds")
input.close(view)
render.close(replica)
print("status fold boundaries: body resizing, empty output, and invalid patch preservation passed")
vim.cmd("qa!")
