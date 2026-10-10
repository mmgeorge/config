vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.nodes")
local replica = buffer.open("saved-change-folds", {})
local rows = {
  "▸ Changed 2 files +4 -4", "Modified first.rs +3 -3", "@@ -1,2 +1,2 @@",
  "old", "new", "@@ -10 +10 @@", "before", "after",
  "Modified second.rs +1 -1", "@@ -1 +1 @@", "before", "after",
}
local metadata = { target = {}, decoration = {}, editable_region = {}, fold = {}, gutter = {} }
for _, range in ipairs({ { "summary", 0, 12 }, { "first", 1, 8 }, { "first-hunk", 2, 5 },
  { "next-hunk", 5, 8 }, { "second", 8, 12 }, { "second-hunk", 9, 12 } }) do
  metadata.fold[#metadata.fold + 1] = { id = range[1], start = { row = range[2], column = 0 },
    ["end"] = { block = "changes", position = { row = range[3], column = 0 } }, closed = true,
    collapse_children = range[1] == "summary", expand_children = range[1] == "first" or range[1] == "second" }
end
local snapshot = { document = replica.document, revision = 0,
  block = { { id = "changes", text = rows, metadata = metadata } } }
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, replica.buffer)
folds.attach(replica, window)
local function expand(id, value)
  assert(buffer.set_expansion(replica, id, value))
end
local function lines() return vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true) end
assert(#lines() == 1)
expand("summary", true)
assert(#lines() == 3 and folds.closed(replica, "first") and folds.closed(replica, "second"))
expand("first", true)
assert(#lines() == 9 and not folds.closed(replica, "first-hunk"))
assert(folds.closed(replica, "second"), "opening a file changed its sibling")
expand("first-hunk", false)
assert(#lines() == 7)
expand("first", false)
expand("first", true)
assert(#lines() == 9, "file expansion did not apply its child policy")
expand("summary", false)
snapshot.revision = 1
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
expand("summary", true)
assert(#lines() == 3, "summary collapse did not reset its descendants")
expand("second", true)
assert(#lines() == 6 and lines()[5] == "before" and lines()[6] == "after")
buffer.close(replica)
print("change node expansion policies: passed")
