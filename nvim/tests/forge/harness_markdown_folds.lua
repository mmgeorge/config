vim.loader.enable(false)
local data = vim.fn.stdpath("data")
vim.opt.runtimepath:append(data .. "/site")
vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
require("render-markdown").setup(require("plugins.markdown")[1].opts())

local markdown = require("forge.render.harness.markdown")
local buffer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
vim.bo[buffer].filetype = "ForgeHarness"
vim.b[buffer].forge_native_document = true
local lines = {
  "Prompt", "Planned", "Proposed changes", "Plan overview", "Plan submitted",
  "Submitted the plan for review with three gameplay systems:", "",
  "- Rechargeable directional dash with swept collision.",
  "- Shield rewards every third orb.", "- Escalating chaser waves at 15 and 30 seconds.", "",
  "The plan includes checks.", "", "Waiting for plan review",
}
local namespace = require("render-markdown.core.ui").ns
local function bullet_count()
  local count = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, { details = true })) do
    for _, chunk in ipairs(mark[4].virt_text or {}) do
      if chunk[1] == "●" then count = count + 1 end
    end
  end
  return count
end
local function render(after, expected)
  markdown.render(buffer, window, { { first0 = 5, after0 = after } })
  assert(vim.wait(3000, function() return bullet_count() == expected end, 10),
    ("expected %d visible bullets, got %d"):format(expected, bullet_count()))
end
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
render(12, 3)
for _ = 1, 3 do
  vim.api.nvim_buf_set_lines(buffer, 8, 12, false, {})
  render(8, 1)
  require("render-markdown.core.ui").update(buffer, window, "CursorMoved", false)
  vim.wait(30)
  assert(bullet_count() == 1, "cursor refresh restored hidden list markers")
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, {})) do
    assert(mark[2] >= 5 and mark[2] < 8, "Markdown marker escaped the folded response")
  end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  render(12, 3)
end
print("harness_markdown_folds: passed")
vim.cmd("qa!")
