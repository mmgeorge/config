vim.loader.enable(false)
vim.o.columns, vim.o.lines = 110, 38
require("forge").setup({ harness = { backend = "mock" } })
local picker = require("forge.views.picker")
local state = require("forge.views.picker.state")
local layout = require("forge.views.picker.layout")
local changes, messages = {}, {}
local notifications = require("forge.infra.notifications")
notifications.warn = function(message) messages[#messages + 1] = message end
local spec
spec = {
  owner = "grouped-fields-test", page_list = { { id = "fields", field_navigation = true, highlight_selected_text = false,
    option_list = {
      { id = "first", section = "Models", label = "Main", fields = {
        { id = "model", kind = "choice", value = "model", description = "Selected model help" },
        { id = "enabled", kind = "toggle", value = false, label = "Automatic", description = "Toggle help" },
      } },
      { id = "second", section = "Checks", label = "Checks", fields = {
        { id = "blocked", kind = "toggle", value = false, label = "Editing", disabled_reason = "Request is pending" },
      } },
    },
  } },
  on_field_action = function(context, action, direction)
    changes[#changes + 1] = { context.field.id, action, direction }
    if action == "toggle" then context.field.value = not context.field.value end
    picker.update(spec)
  end,
}
local function press(key)
  local instance = picker._state_for_test()
  local mapping = vim.api.nvim_buf_call(instance.buf, function() return vim.fn.maparg(key, "n", false, true) end)
  assert(mapping.callback, "missing field key: " .. key)
  mapping.callback()
end
local function selected()
  local instance = picker._state_for_test()
  return state.selected_field(instance.state, instance.spec)
end
picker.open(spec)
assert(selected().id == "model")
press("<Right>")
assert(changes[1][1] == "model" and changes[1][2] == "change")
press("<Space>")
assert(#changes == 1, "Space modified a nonboolean")
press("<Tab>")
assert(selected().id == "enabled")
press("<Right>")
press("<CR>")
assert(#changes == 1, "boolean changed outside Space")
press("<Space>")
assert(selected().value == true and selected().id == "enabled", "focus did not survive rebuilding")
press("<Down>")
assert(selected().id == "blocked", "navigation selected a section heading")
press("<Space>")
assert(messages[1] == "Request is pending" and #changes == 2)
press("<Up>")
assert(selected().id == "enabled", "row focus was not retained")
press("<S-Tab>")
assert(selected().id == "model")
local instance = picker._state_for_test()
local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
assert(text:find("Selected model help", 1, true) and not text:find("Toggle help", 1, true))
local joined = layout.build({ field_navigation = true, option_list = {
  { id = "duration", label = "Timeout", fields = {
    { id = "amount", kind = "input", label = "Approval", value = "10" },
    { id = "unit", kind = "choice", value = "m", join_previous = true },
  } },
} }, 1, 80, { field_by_option = { duration = "unit" } })
assert(joined.lines[joined.option_range[1].first]:find("Approval: 10 ← m →", 1, true), "joined unit lost adjacent focus")
local flat = layout.build({ field_navigation = true, option_list = {
  { id = "tool", section = "Tools", label = "LSP", inline_fields = true, fields = {
    { id = "lsp", kind = "toggle", value = true, label = "LSP" },
  } },
} }, 1, 80)
assert(flat.lines[flat.option_range[1].first] == "  ● LSP", "flat toggles must lead with the enablement icon")
local retained = string.rep("界", 1200) .. "complete-tail"
spec.page_list[1].option_list[1].fields[1].value = retained
local config = require("forge.infra.config")
local previous_height = config.options.picker.max_height
config.options.picker.max_height = 8
picker.update(spec)
assert(vim.api.nvim_win_get_height(picker._state_for_test().win) == 8, "ordinary picker lost its configured height limit")
spec.fit_content, spec.height_ratio = true, 0.9
picker.update(spec)
assert(vim.api.nvim_win_get_height(picker._state_for_test().win) > 8, "content-sized settings ignored available host space")
config.options.picker.max_height = previous_height
local frame = layout.build(spec.page_list[1], 1, 46, { field_by_option = { first = "model" } })
local range = frame.option_range[1]
local reconstructed = table.concat(vim.list_slice(frame.lines, range.first, range.last), "\n")
assert(select(2, reconstructed:gsub("界", "")) == 1200, "wrapped field lost Unicode characters")
assert(reconstructed:find("complete", 1, true), "narrow layout clipped a valid field value")
assert(reconstructed:find("tail", 1, true), "narrow layout lost the field tail")
for _, row in ipairs(frame.lines) do assert(vim.fn.strdisplaywidth(row) <= 46, "field row overflow") end
local buffer = vim.api.nvim_create_buf(false, true)
local projected = layout.viewport(frame, 14)
require("forge.views.picker.render").apply(buffer, projected)
assert(projected.option_scroll_max > 0, "large fields must remain scrollable")
vim.api.nvim_buf_delete(buffer, { force = true })
picker.close()
print("picker_fields: focus, toggles, disabled controls, hints, and narrow wrapping passed")
