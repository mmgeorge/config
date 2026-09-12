vim.opt.runtimepath:append("nvim")

local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "title", "summary", "stay ...", "body" })
local replica = {
  buffer = buffer,
  block = {
    ["issue:title"] = { row_count = 1 },
    ["issue:comment:0:label"] = { row_count = 2 },
  },
  sequence = { position = function(_, id) return 0, id == "issue:title" and 0 or 1 end },
}
local gutter = require("github.issue_gutter")
gutter.refresh(replica)
local namespace = vim.api.nvim_get_namespaces().ForgeIssueContinuationGutter
local marks = vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, { details = true })
assert(#marks == 1 and marks[1][2] == 2)
assert(marks[1][4].number_hl_group == "ForgeIssueContinuationNumber")
assert(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)[3] == "stay ...")
gutter.clear(replica)
assert(#vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {}) == 0)
print("issue_gutter: continuation number marker, source preservation, and cleanup passed")
