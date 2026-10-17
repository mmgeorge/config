vim.loader.enable(false)
local buffer = require("forge.buffer")
local comments = require("forge.draft_comments")
local replica = buffer.open("projected-diff-gutters", { source_projected = true })
vim.api.nvim_win_set_buf(0, replica.buffer)
local source, blocks = {}, {}
for index, text in ipairs({ "@@ first", "...", "@@ second", "...", "changed" }) do
  local metadata = { target = {}, decoration = {}, editable_region = {}, gutter = {} }
  if text == "..." or text == "changed" then
    metadata.gutter = { { position = { row = 0, column = 0 }, priority = 100,
      chunk = { { text = "gutter:" .. index, capture = "Normal" } } } }
  end
  local id = "row:" .. index
  blocks[index] = { id = id, text = { text }, metadata = metadata }
  source[index] = { id = id, block = id, position = { row = 0, column = 0 },
    canonical_row = index - 1, source_line = index, text = text, metadata = metadata,
    annotation_anchor = text == "changed" }
end
assert(buffer.apply_snapshot(replica, { document = replica.document, revision = 0, block = blocks }).kind == "Applied")
local state = comments.attach(replica.buffer, vim.api.nvim_get_current_win(),
  vim.tbl_map(function(row) return row.text end, source), {}, {
    source_provider = function() return source end, guard_source = true,
  })
require("forge.draft_source").attach(replica, state, source)
local function audit()
  local rows = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)
  local expected = {}
  for _, record in ipairs(state.source_mark) do
    local row = source[record.source_index]
    local position = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, state.namespace, record.mark, {})
    if #row.metadata.gutter > 0 then expected[position[1]] = row.metadata.gutter[1].chunk[1].text end
  end
  local actual = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, { details = true })) do
    local chunks = mark[4].virt_text
    if chunks then
      local text = table.concat(vim.tbl_map(function(chunk) return chunk[1] end, chunks))
      if text:match("^gutter:") then
        assert(not actual[mark[2]], "duplicate diff gutter")
        actual[mark[2]] = text
        assert(not rows[mark[2] + 1]:match("^@@"), "diff gutter moved onto a hunk header")
      end
    end
  end
  assert(vim.deep_equal(actual, expected), "diff gutters differ from source identities")
end
audit()
for _ = 1, 2 do
  source[2].hidden = true
  comments.update(replica.buffer, {})
  audit()
  source[2].hidden = false
  comments.update(replica.buffer, {})
  audit()
end
comments.add(replica.buffer, { id = "comment", source_line = 5, end_source_line = 5, body = "first\nsecond" }, false)
audit()
comments.lock(replica.buffer)
audit()
comments.detach(replica.buffer, false)
buffer.close(replica)
print("projected_diff_gutters: passed")
