vim.loader.enable(false)
require("forge").setup({ harness = { backend = "mock" } })
local picker = require("forge.views.picker")
local datetime = require("forge.integrations.datetime")
local now = 1700000000
datetime.now_override = function() return now end
local selected
require("forge.views.harness.plan_picker").open({
  host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() },
  plan_list = {
    { id = "first", title = "Migrate cloud diagnostics to Rust", session_name = "Main", created_at_ms = (now - 2 * 86400) * 1000, revision_count = 3, state = "cancelled", implemented = false },
    { id = "second", title = "Improve diagnostics", session_name = "Earlier session", created_at_ms = (now - 2 * 3600) * 1000, revision_count = 2, state = "accepted", implemented = true },
  },
  on_confirm = function(selection) selected = selection end,
})
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback, "missing picker binding: " .. key)
  mapping.callback()
  vim.wait(25, function() return false end, 5)
end
local function detail(index)
  return picker._state_for_test().spec.page_list[1].option_list[index].detail
end
local function label(index)
  return picker._state_for_test().spec.page_list[1].option_list[index].label
end
assert(label(1):find("2 days ago", 1, true) == 1)
assert(label(2):find("2 hours ago", 1, true) == 1)
assert(detail(1) == "← 3/3 →  · cancelled · Main")
assert(detail(2) == "  2/2    · accepted · Earlier session")
invoke("<Left>")
assert(detail(1):find("2/3", 1, true))
invoke("<Down>")
invoke("<Left>")
assert(detail(2):find("1/2", 1, true))
invoke("<Up>")
assert(detail(1):find("2/3", 1, true), "moving between plans lost the selected revision")
invoke("<Right>")
invoke("<Right>")
assert(detail(1):find("1/3", 1, true), "revision selector did not wrap")
invoke("<CR>")
assert(selected.plan_id == "first" and selected.revision == 1)
assert(not picker.is_open())

require("forge.views.harness.plan_picker").open({
  host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() },
  plan_list = {},
  on_confirm = function() error("empty plan picker cannot confirm") end,
})
assert(picker._state_for_test().spec.page_list[1].empty_text == "This workspace has no submitted plans.")
picker.close(false)
datetime.now_override = nil
print("harness_plan_picker: passed")
