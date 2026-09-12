vim.opt.runtimepath:append("nvim")

local markdown_namespace = vim.api.nvim_create_namespace("render-markdown.nvim")
local render_calls = 0
package.loaded["render-markdown.core.ui"] = { ns = markdown_namespace }
package.loaded["render-markdown"] = {
  render = function(context)
    render_calls = render_calls + 1
    vim.api.nvim_buf_set_extmark(context.buf, markdown_namespace, 0, 0, { virt_text = { { "body", "Comment" } } })
    local lines = vim.api.nvim_buf_get_lines(context.buf, 0, -1, false)
    for row, line in ipairs(lines) do
      if line == "## Heading" then
        vim.api.nvim_buf_set_extmark(context.buf, markdown_namespace, row - 1, 0, { virt_text = { { "heading", "Comment" } } })
      end
    end
    context.config.on.render()
  end,
}

local preview = require("github.issue_preview")
local rendered = preview.render({
  kind = "issue", number = 12, title = "Native preview", author = "alice", state = "OPEN",
  created_at = "2026-06-01T00:00:00Z", updated_at = "2026-06-10T12:00:00Z",
  body = "## Heading\nBody", labels = { "bug" }, assignees = { "bob" }, projects = { "Roadmap" },
  milestone = "v1", subscription = "Subscribed", comments_count = 1,
  comments = { { author = "carol", body = "Comment body", created_at = "2026-06-09T00:00:00Z", url = "https://example.test/comment" } },
}, { folds = {} })

assert(table.concat(rendered.lines, "\n"):find("Title:  Native preview", 1, true))
assert(table.concat(rendered.lines, "\n"):find("Comments (1):", 1, true))
assert(table.concat(rendered.lines, "\n"):find("@bob", 1, true))
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
preview.present(buffer, rendered, { markdown = true, win = vim.api.nvim_get_current_win() })
assert(render_calls == 1, "picker preview did not render markdown")
assert(#vim.api.nvim_buf_get_extmarks(buffer, markdown_namespace, 0, -1, {}) == 1,
  "picker preview did not prune markdown outside the issue body")
local decoration_namespace = vim.api.nvim_get_namespaces()["github.issue_preview.decorations"]
assert(decoration_namespace, "picker preview did not install its dedicated decoration namespace")
assert(#vim.api.nvim_buf_get_extmarks(buffer, decoration_namespace, 0, -1, {}) > 0,
  "picker preview did not decorate rendered metadata")
local header = vim.api.nvim_get_hl(0, { name = "ForgeStatusHeader", link = false })
assert(header.fg == 0xf8f8f2 and header.bold, "picker preview lost the legacy section heading color")
local heading_count = 0
for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, decoration_namespace, 0, -1, { details = true })) do
  if mark[4].hl_group == "ForgeStatusHeader" then heading_count = heading_count + 1 end
end
assert(heading_count == 2, "picker preview did not decorate both legacy section headings")
assert(package.loaded["github.issue_view"] == nil, "picker preview retained the removed interactive issue module")
print("issue_preview: readonly picker rendering, markdown pruning, and decorations passed")
vim.cmd("qa!")
