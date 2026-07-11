vim.loader.enable(false)

local comment_box = require("diff_review.render.comment_box")
local comment_box_rows = require("diff_review.views.status.comment_box_rows")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function virtual_line_text(virtual_line)
  local value = ""
  for _, chunk in ipairs(virtual_line or {}) do value = value .. tostring(chunk[1] or "") end
  return value
end

local function assert_lines_fit(virtual_lines, width, label)
  for line_index, virtual_line in ipairs(virtual_lines or {}) do
    local text = virtual_line_text(virtual_line)
    assert_true(
      vim.fn.strdisplaywidth(text) <= width,
      ("%s line %d exceeded width %d: %s"):format(label, line_index, width, vim.inspect(text))
    )
  end
end

local descriptor = {
  id = "annotation:primary",
  heading = " An excessively long heading with a date, author, and line number that must truncate ",
  body_lines = {
    "SupercalifragilisticexpialidociousSupercalifragilisticexpialidocious",
    "",
    "Unicode 界界界 stays display-width safe.",
  },
  replies = {
    {
      heading = "a reply heading that also wraps safely",
      body_lines = { "another_unbroken_reply_token_that_must_split" },
    },
  },
}

local narrow_lines = comment_box.build_box_lines(descriptor, 24)
assert_lines_fit(narrow_lines, 24, "narrow box")
assert_true(#narrow_lines > 6, "narrow box did not wrap long content")
local narrow_text = table.concat(vim.tbl_map(virtual_line_text, narrow_lines), "\n")
assert_true(narrow_text:find("├─", 1, true) ~= nil, "reply did not render as an internal box divider")
assert_true(select(2, narrow_text:gsub("╭", "")) == 1, "reply created another top-level box")
assert_true(select(2, narrow_text:gsub("╰", "")) == 1, "reply created another bottom-level box")

local group_lines = comment_box.build_box_group_lines({
  { desc = descriptor },
  { desc = { heading = " second ", body_lines = { "second body" } } },
}, 32)
assert_lines_fit(group_lines, 32, "stacked box")
local group_text = table.concat(vim.tbl_map(virtual_line_text, group_lines), "\n")
assert_true(group_text:find("second body", 1, true) ~= nil, "stacked box dropped the second annotation")
assert_true(
  group_text:find("Super", 1, true) < group_text:find("second body", 1, true),
  "stacked boxes changed descriptor order"
)

local buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "anchor" })
local wide_win = vim.api.nvim_open_win(buf, false, {
  relative = "editor",
  row = 0,
  col = 0,
  width = 60,
  height = 8,
  style = "minimal",
})
local state = {
  buf = buf,
  lines = { "anchor" },
  entries = { { id = "anchor:1", kind = "diff" } },
  extmarks = {},
  highlights = {},
  line_highlights = {},
  comment_box_record_by_anchor = {},
}
comment_box_rows.render_box(state, descriptor, 1)
comment_box_rows.render_box(state, { heading = " second ", body_lines = { "second body" } }, 1)
assert_true(#state.extmarks == 0, "interactive comment boxes should not use virtual-line extmarks")
assert_true(#state.lines > 3, "interactive comment boxes did not emit real buffer rows")
assert_true(state.entries[2].kind == "comment_box", "first compact row lacks comment-box identity")
assert_true(state.comment_box_record_by_id[descriptor.id].start_line == 2, "stable comment-box identity did not index its rows")
assert_true(state.entries[2].comment_box_boundary == "header", "first compact row is not the box header")
assert_true(state.entries[#state.lines].comment_box_boundary == "footer", "last compact row is not the box footer")
local rendered_text = table.concat(state.lines, "\n")
assert_true(rendered_text:find("second body", 1, true) ~= nil, "real compact rows dropped the second annotation")

local narrow_win = vim.api.nvim_open_win(buf, false, {
  relative = "editor",
  row = 9,
  col = 0,
  width = 24,
  height = 8,
  style = "minimal",
})
assert_true(comment_box_rows.refresh_buffer(state), "width change did not request a compact-box layout render")
assert_true(not comment_box_rows.refresh_buffer(state), "unchanged width requested a duplicate layout render")

vim.api.nvim_win_close(narrow_win, true)
vim.api.nvim_win_close(wide_win, true)
vim.api.nvim_buf_delete(buf, { force = true })
vim.cmd("qa!")
