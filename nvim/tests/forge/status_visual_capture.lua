vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local render = require("forge.status_render")
local input = require("forge.input")
local replica = render.open("visual-fold-end", {})
vim.api.nvim_win_set_buf(0, replica.buffer)
local file = {}
for id = 1, 3 do
  file[id] = { id = id, generation = 1, section = "unstaged", change = "modified",
    path = "file_" .. id .. ".rs", untracked = false, stats = { state = "exact", added = 1, deleted = 1 } }
end
assert(render.apply_snapshot(replica, { document = replica.document, revision = 0, view = { kind = "status" },
  head = { state = "attached", reference = "main", object = "abc" }, pending = {},
  section = { { kind = "unstaged", file = { 1, 2, 3 } } }, file = file }).kind == "Applied")
local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
local _, first = replica.sequence:position("file:1")
local _, last = replica.sequence:position("file:3")
vim.api.nvim_win_set_cursor(0, { last + 2, 0 })
assert(render.capture(replica, view, "stage") == nil, "blank fold endpoint became a whole-file target")
local selection = render.selection(replica, first, last + 1)
local captured, failure = render.capture(replica, view, "stage", selection)
assert(captured, failure)
assert(captured.location.kind == "file" and captured.location.id == 1)
assert(#selection.target == 3)
for ordinal, target in ipairs(selection.target) do assert(target.kind == "file" and target.id == ordinal) end
input.close(view)
render.close(replica)
print("visual selection captures file targets when its cursor ends inside a collapsed fold")
