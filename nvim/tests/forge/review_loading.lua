vim.opt.runtimepath:append("nvim")
require("forge.infra.config").options.about_auto_generate = false
local lookup, opened
require("forge.integrations.gh").current_pr_async = function(_, callback) lookup = callback end
require("forge.views.commands").open_pr = function(pr) opened = pr end
local window = vim.api.nvim_get_current_win()
local origin = vim.api.nvim_get_current_buf()
local owner = require("forge.views.status.status_context").attach({
  document_id = "loading-test", workspace = vim.fn.getcwd(),
  present = function() end, is_alive = function() return true end,
  capture_input = function() return {} end, is_input_current = function() return true end,
  get_info = function() return {} end,
})
owner.refresh()
owner.open_pull_request(window)
assert(owner.pending_pr and vim.api.nvim_get_current_buf() == origin,
  "early ogp must retain the current buffer while awaiting metadata")
lookup({ ok = true, pr = { number = 2, title = "PR title", repo = "owner/repo" } })
assert(opened and opened.number == 2, "pending ogp was not continued after lookup")
opened = nil
owner.refresh()
owner.open_pull_request(window)
local replacement = vim.api.nvim_create_buf(false, true)
vim.api.nvim_win_set_buf(window, replacement)
lookup({ ok = true, pr = { number = 2 } })
assert(not opened, "late lookup replaced a newer buffer")
owner.close()
vim.api.nvim_win_set_buf(window, origin)
vim.api.nvim_buf_delete(replacement, { force = true })
print("review_loading: deferred lookup and stale-window protection passed")
