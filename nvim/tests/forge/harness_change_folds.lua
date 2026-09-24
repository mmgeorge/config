vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.folds")
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
    collapse_children = range[1] == "summary" }
end
local snapshot = { document = replica.document, revision = 0,
  block = { { id = "changes", text = rows, metadata = metadata } } }
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, replica.buffer)
folds.attach(replica, window)
local function tab(row)
  vim.api.nvim_win_set_cursor(window, { row, 0 })
  assert(folds.toggle_heading(replica, window))
end
assert(vim.fn.foldclosed(1) == 1)
tab(1)
assert(vim.fn.foldclosed(2) == 2, "opening Changed exposed the first file's hunks")
assert(vim.fn.foldclosed(9) == 9, "opening Changed exposed the second file's hunks")
tab(2)
assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosed(6) == 6, "opening file exposed hunk bodies")
assert(vim.fn.foldtextresult(3) == "@@ -1,2 +1,2 @@", "hunk indentation differs from file")
tab(1)
local saved = folds.capture(replica)
snapshot.revision = 1
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
folds.restore(replica, saved)
tab(1)
assert(vim.fn.foldclosed(2) == 2, "reopening Changed restored an expanded file")
tab(2)
assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosed(9) == 9, "refresh lost child fold choices")
tab(2)
tab(1)
tab(1)
assert(vim.fn.foldclosed(2) == 2 and vim.fn.foldclosed(9) == 9, "parent toggle reset file folds")
tab(2)
tab(3)
assert(vim.fn.foldclosed(4) == -1, "hunk did not expose its body")
tab(1)
tab(1)
assert(vim.fn.foldclosed(2) == 2, "Changed restored an expanded hunk")
tab(2)
assert(vim.fn.foldclosed(3) == 3, "Changed did not reset descendant hunk folds")
tab(2)
vim.api.nvim_win_set_cursor(window, { 1, 0 })
vim.keymap.set("n", "<Tab>", function() folds.toggle_heading(replica, window) end, { buffer = replica.buffer })
print("harness_change_folds: passed")
