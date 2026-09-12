vim.opt.runtimepath:append("nvim")

local namespace = vim.api.nvim_create_namespace("render-markdown.nvim")
package.loaded["render-markdown.core.ui"] = { ns = namespace }
package.loaded["render-markdown"] = {
  render = function(options)
    for row = 0, vim.api.nvim_buf_line_count(options.buf) - 1 do
      vim.api.nvim_buf_set_extmark(options.buf, namespace, row, 0, {
        end_col = #(vim.api.nvim_buf_get_lines(options.buf, row, row + 1, false)[1] or ""),
        hl_group = "Normal",
      })
    end
    options.config.on.render()
  end,
}

local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, {
  "Title: source",
  "Description:",
  "- first",
  "**second**",
  "Comments (0):",
})
vim.api.nvim_win_set_buf(0, buffer)
local replica = {
  buffer = buffer,
  block = { ["region:body"] = { row_count = 2 } },
  sequence = { position = function(_, id)
    assert(id == "region:body")
    return 2, 2
  end },
}
local source = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
local markdown = require("github.issue_markdown")
assert(markdown.render(replica, vim.api.nvim_get_current_win()))
local marks = vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {})
assert(#marks == 2 and marks[1][2] == 2 and marks[2][2] == 3)
assert(vim.deep_equal(source, vim.api.nvim_buf_get_lines(buffer, 0, -1, false)))

vim.api.nvim_buf_set_lines(buffer, 3, 3, false, { "`third`" })
replica.block["region:body"].row_count = 3
assert(markdown.render(replica, vim.api.nvim_get_current_win()))
marks = vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {})
assert(#marks == 3 and marks[1][2] == 2 and marks[3][2] == 4)
markdown.clear(replica)
assert(#vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {}) == 0)
print("issue_markdown: range pruning, rerender, source preservation, and detach cleanup passed")
