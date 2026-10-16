local info = require("forge.views.plan_review.entity_info")
local navigation = require("forge.views.plan_review.entity_navigation")
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "External operation" })
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, buffer)
local requests = 0
local model = {
  entity_at_position = function() return nil end,
  described_item_at_position = function() return nil end,
  workspace_target_at_position = function() return nil end,
  rustdoc_target_at_position = function() error("external Rust documentation must not be acquired") end,
}
local options = { source_line = 1, workspace_root = vim.fn.getcwd(), request = function() requests = requests + 1 end }
assert(not info.show_context(model, buffer, window, options))
assert(not navigation.jump(model, buffer, window, options))
assert(requests == 0, "external plan reference triggered dependency acquisition")
print("plan_review_rustdoc: external acquisition disabled")
