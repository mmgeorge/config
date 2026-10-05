vim.loader.enable(false)
local comments = require("forge.draft_comments")
local mapping = require("forge.draft_source")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
local source = {
  { id = "first", text = "First", source_line = 1, canonical_row = 0,
    block = "first-block", position = { row = 0, column = 0 }, target = "first-target" },
  { id = "last", text = "Last", source_line = 44, canonical_row = 43,
    block = "last-block", position = { row = 3, column = 0 }, target = "last-target" },
}
local state = comments.attach(buffer, window, { "First", "Last" }, {}, {
  source_provider = function() return source end,
})
local replica = {}
mapping.attach(replica, state, source)
assert(replica.physical_row(43) == 1)
assert(replica.physical_row(12) == 1, "omitted canonical rows must resolve to the following source boundary")
comments.add(buffer, { id = "draft-comment/client/body", source_line = 1, end_source_line = 1, body = "Body" }, false)
assert(replica.physical_row(43) == 4, "comment insertion must shift canonical source positions")
assert(replica.decoration_location(2) == nil, "comment rows must not borrow source syntax")
local canonical, boundary = replica.fold_location(2)
assert(canonical == 0 and not boundary, "comment rows must inherit source fold depth without opening duplicate folds")
vim.api.nvim_win_set_cursor(window, { 1, 0 })
local located = replica.locate(4, 2)
assert(located.block == "last-block" and located.position.row == 3 and located.target == "last-target",
  "explicit row lookups must not use the current cursor's annotation")
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
assert(replica.physical_row(43) > 1, "compact comments must retain source positions")
assert(replica.physical_row(44) == vim.api.nvim_buf_line_count(buffer), "final source boundaries must include comments")
comments.detach(buffer)
vim.api.nvim_buf_delete(buffer, { force = true })
print("draft_source: source navigation, sparse boundaries, syntax, and folds survive local comments")
