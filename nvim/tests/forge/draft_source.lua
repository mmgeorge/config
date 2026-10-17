vim.loader.enable(false)
local comments = require("forge.draft_comments")
local mapping = require("forge.draft_source")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
local source = {
  { id = "first", text = "First", source_line = 1, canonical_row = 0,
    block = "first-block", position = { row = 0, column = 0 }, target = "first-target" },
  { id = "last", text = "Last WaveDirector Weapon", source_line = 44, canonical_row = 43,
    block = "last-block", position = { row = 3, column = 0 }, target = "last-target",
    metadata = { target = {
      { id = "weapon", range = { start = { row = 3, column = 18 }, ["end"] = { row = 3, column = 24 } } },
      { id = "director", range = { start = { row = 3, column = 5 }, ["end"] = { row = 3, column = 17 } } },
      { id = "last-target", range = { start = { row = 3, column = 0 }, ["end"] = { row = 4, column = 0 } } },
    } } },
}
local state = comments.attach(buffer, window, { "First", "Last WaveDirector Weapon" }, {}, {
  source_provider = function() return source end,
})
local replica = { sequence = { count = function() return 0 end } }
mapping.attach(replica, state, source)
assert(replica.physical_row(43) == 1)
assert(replica.physical_row(12) == 1, "omitted canonical rows must resolve to the following source boundary")
comments.add(buffer, { id = "draft-comment/client/body", source_line = 1, end_source_line = 1, body = "Body" }, false)
assert(replica.physical_row(43) == 4, "comment insertion must shift canonical source positions")
assert(replica.decoration_location(2) == nil, "comment rows must not borrow source syntax")
local comment_location = replica.locate(2, 0)
assert(comment_location.block == "first-block" and comment_location.target == "first-target",
  "comment rows must retain their owning source identity")
vim.api.nvim_win_set_cursor(window, { 1, 0 })
local located = replica.locate(4, 2)
assert(located.block == "last-block" and located.position.row == 3 and located.target == "last-target",
  "explicit row lookups must not use the current cursor's annotation")
assert(replica.locate(4, 5).target == "director" and replica.locate(4, 18).target == "weapon",
  "comment-adjusted rows must resolve the symbol under the cursor rather than the row's first target")
assert(replica.locate(4, 17).target == "last-target", "symbol endpoints must remain exclusive")
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
assert(replica.physical_row(43) > 1, "compact comments must retain source positions")
assert(replica.physical_row(44) == vim.api.nvim_buf_line_count(buffer), "final source boundaries must include comments")
comments.detach(buffer)
vim.api.nvim_buf_delete(buffer, { force = true })
print("draft_source: exact symbol navigation and sparse source boundaries survive local comments")
