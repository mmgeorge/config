vim.loader.enable(false)

local comments = require("forge.draft_comments")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)

local annotation = {
  id = "remote-comment",
  source_line = 44,
  end_source_line = 44,
  body = "Retain the immutable source identity",
  readonly = true,
}
local state = comments.attach(buffer, window, { "Title", "Diff" }, { annotation }, {
  heading = "Comment",
  source_label = function(comment) return "line " .. comment.source_line end,
  editable_source = function(row) return row == 0 end,
  source_provider = function()
    return {
      { id = "title", text = "Title", source_line = 1 },
      { id = "diff", text = "+ pub mod assets;", source_line = 44 },
    }
  end,
})

assert(annotation.source_line == 44, "sparse source identities must survive projection")
assert(comments.display_line_for_source_line(buffer, 44) == 2,
  "source identity must map to its physical row")
assert(vim.bo[buffer].modifiable, "editable source fields must retain native editing")

local compact = state.range_list[1]
vim.api.nvim_win_set_cursor(window, { compact.first_row + 2, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
assert(not annotation.focused, "another author's comment must stay compact")
assert(not vim.bo[buffer].modifiable, "another author's comment must stay read-only")
comments.delete_at_cursor(buffer)
assert(#state.annotation_list == 1, "local deletion must reject read-only comments")

annotation.readonly = false
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
assert(annotation.focused, "viewer-authored comments must expand locally")
assert(vim.bo[buffer].modifiable, "focused viewer-authored comments must permit native editing")
assert(comments.capture(buffer)[1].source.start_line == 44,
  "explicit capture must retain the immutable source identity")

comments.detach(buffer)
local duplicate_annotation = { id = "draft", source_line = 44, end_source_line = 44, body = "" }
local duplicate_state = comments.attach(buffer, window, { "Declaration", "Separator", "Untargeted row" },
  { duplicate_annotation }, { source_provider = function()
    return {
      { id = "declaration", text = "pub struct Example;", source_line = 44 },
      { id = "separator", text = "", source_line = 45 },
      { id = "untargeted", text = "", source_line = 44, annotation_anchor = false },
    }
  end })
assert(#duplicate_state.range_list == 1, "one draft must not have multiple body editors")
assert(comments.focus(buffer, "draft"))
local body_row = vim.api.nvim_win_get_cursor(window)[1] - 1
vim.api.nvim_buf_set_text(buffer, body_row, 0, body_row, 0, { "Retain typed body" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
vim.api.nvim_win_set_cursor(window, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
assert(comments.capture(buffer)[1].source.body == "Retain typed body",
  "an untargeted duplicate source line erased the typed draft")
assert(table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n"):find("Retain typed body", 1, true),
  "collapsed comment omitted its typed body")
comments.detach(buffer)
vim.api.nvim_buf_delete(buffer, { force = true })
print("draft_comments: sparse source identities, editable fields, read-only ownership, and local focus passed")
