local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local render = require("forge.status_render")
local input = require("forge.input")
local notices = {}
local replica = render.open("semantic-test", { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_win_set_buf(0, replica.buffer)
local function record(id, path)
  return { id = id, generation = 1, section = "unstaged", change = "modified", path = path,
    untracked = false, stats = { state = "unknown" } }
end
local snapshot = { document = replica.document, revision = 0, view = { kind = "status" },
  head = { state = "attached", reference = "refs/heads/main", object = "abc123" }, context = vim.NIL,
  section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1, "a.lua"), record(2, "雪.lua") } }
assert(render.apply_snapshot(replica, snapshot).kind == "Applied", table.concat(notices, "\n"))
local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
local function lines() return vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true) end
assert(vim.deep_equal(lines(), { "refs/heads/main abc123", "", "Unstaged changes (2):", "Modified a.lua", "", "Modified 雪.lua", "" }))
assert(vim.fn.foldclosed(4) == 4, "file does not start collapsed")
assert(vim.fn.foldclosed(3) == -1, "section starts collapsed")
vim.api.nvim_win_set_cursor(0, { 4, 0 })
local captured = assert(input.capture(replica, view, "demand"))
assert(captured.location.kind == "file" and captured.location.id == 1 and captured.block == nil)
vim.cmd("4foldopen")
local function metadata(target, count)
  return { target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = count, column = 0 } } } },
    decoration = {}, editable_region = {} }
end
local header_metadata = metadata("group:10", 1)
header_metadata.fold = { { id = "hunk-header:11", start = { row = 0, column = 0 },
  ["end"] = { block = "body:12", position = { row = 2, column = 0 } }, closed = false } }
local delivery = { document = replica.document, file = 1, generation = 1, more = true, state = { state = "partial" },
  snapshot = { document = "body:1:1", revision = 1, block = {
    { id = "hunk-header:11", text = { "@@ +1 -1" }, metadata = header_metadata },
    { id = "body:12", text = { "old", "new" }, metadata = metadata("hunk:13", 2) },
  } } }
assert(render.apply_body(replica, delivery).kind == "Applied", table.concat(notices, "\n"))
assert(vim.deep_equal(lines(), { "refs/heads/main abc123", "", "Unstaged changes (2):", "Modified a.lua", "@@ +1 -1", "old", "new", "Modified 雪.lua", "" }))
assert(replica.revision == 0, "body changed inventory revision")
assert(vim.fn.foldclosed(4) == -1, "body expansion closed its file")
local location = render.locate(replica, 6, 1)
assert(location.location.kind == "body" and location.location.file == 1 and location.location.position.row == 1)
assert(location.target == "hunk:13")
assert(select(2, replica.sequence:position("file:2")) == 7)
local selected = render.selection(replica, 4, 6)
assert(#selected.target == 2 and selected.target[1].target == "group:10" and selected.target[2].target == "hunk:13")
local body = replica.file[1].body
local changed = record(1, "a.lua")
changed.stats = { state = "exact", added = 2, deleted = 1 }
assert(render.apply_patch(replica, { document = replica.document, base = 0, next = 1, removed = {}, section = {}, file = { changed } }).kind == "Applied")
assert(replica.file[1].body == body and lines()[4] == "Modified a.lua +2 -1", "count update lost body")
local patch = { document = "body:1:1", base = 1, next = 2, base_rows = 3, next_rows = 4, base_blocks = 2, next_blocks = 3,
  text_edit = { { start_row = 3, removed_rows = 0, text = { "tail" } } },
  block_edit = { { start_block = 2, removed_blocks = 0, inserted = { "body:14" } } },
  metadata_edit = { { block = "body:14", row_count = 1, metadata = metadata("hunk:13", 1) } }, removed_block = {} }
assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1, patch = patch }).kind == "Applied", table.concat(notices, "\n"))
assert(lines()[8] == "tail" and lines()[9] == "Modified 雪.lua")
changed = vim.deepcopy(changed)
changed.generation = 2
assert(render.apply_patch(replica, { document = replica.document, base = 1, next = 2, removed = {}, section = {}, file = { changed } }).kind == "Applied")
assert(replica.file[1].body == nil and #lines() == 7, "changed generation retained stale body")
assert(render.apply_body(replica, delivery).kind == "Discarded", "late body crossed generation")
vim.api.nvim_win_set_cursor(0, { 1, 0 })
assert(input.capture(replica, view, "navigate").location.kind == "boundary")
assert(render.locate(replica, 4, 0).target == nil, "placeholder selected its file")
local restored = vim.deepcopy(replica.inventory)
vim.bo[replica.buffer].modifiable = true
vim.api.nvim_buf_set_lines(replica.buffer, 0, -1, false, { "external change" })
vim.bo[replica.buffer].modifiable = false
assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 2 }).kind == "Desynchronized")
assert(render.apply_snapshot(replica, restored).kind == "Applied", "inventory recovery rejected external text replacement")
assert(#lines() == 7 and lines()[4] == "Modified a.lua +2 -1")
assert(#notices == 1 and notices[1]:find("externally", 1, true))
notices = {}
assert(#notices == 0, table.concat(notices, "\n"))
input.close(view)
render.close(replica)

local context = { workspace = root, branch = "main", head = { oid = string.rep("a", 40), reference = "main", subject = "雪の修正 é" },
  recent = { { oid = string.rep("a", 40), reference = "main", subject = "雪の修正 é" } }, issues = { 12 } }
replica = render.open("context-test", { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_win_set_buf(0, replica.buffer)
replica.width = 22
snapshot = vim.deepcopy(snapshot)
snapshot.document, snapshot.context = replica.document, context
assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
local root_sequence, file_header = replica.root, replica.file[1].header
local original_revision = replica.revision
render.present_context(replica, { pr = { state = "ready", text = "日本語を含む長い題名 é é é" }, about = { state = "ready", text = "summary\nbody" } })
assert(replica.root == root_sequence and replica.file[1].header == file_header, "context rebuilt file inventory")
assert(replica.revision == original_revision, "local presentation changed native revision")
local pr = replica.block["status:context:pr"]
assert(pr.row_count > 1)
for index, text in ipairs(pr.text) do
  assert(vim.fn.strdisplaywidth(text) <= replica.width, "context wrapping exceeded display width")
  local rendered = {}
  for _, chunk in ipairs(pr.chunk[index]) do rendered[#rendered + 1] = chunk[1] end
  assert(table.concat(rendered) == text, "highlight byte ranges lost wrapped Unicode text")
end
replica.width = 80
render.present_context(replica, replica.presentation)
assert(replica.block["status:context:pr"].row_count == 1)
assert(replica.root == root_sequence and #notices == 0, table.concat(notices, "\n"))
render.close(replica)
print("semantic status rendering: passed")
