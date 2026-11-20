-- Unit tests for `forge.shared.keymaps` (shared/keymaps.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["view_help_lists_commands_bound_in_another_keymap_group"] = function()
  local keymaps = require("forge.shared.keymaps")
  -- PlanReview borrows the diff group's visibility key, as its mapping and winbar do.
  local command_set = { order = { "toggle_visibility" },
    action_by_id = { toggle_visibility = { run = function() end } } }
  keymaps.show_view_help("plan_review", command_set, "PlanReview")
  local help = vim.tbl_filter(function(buf) return vim.bo[buf].filetype == "ForgeHelp" end, vim.api.nvim_list_bufs())
  MiniTest.expect.equality(#help, 1)
  local text = buffer.text(help[1])
  assert(text:find("ov", 1, true) and text:find("Toggle All/Public diff visibility", 1, true),
    "PlanReview help omitted its visibility key:\n" .. text)
end

return T
