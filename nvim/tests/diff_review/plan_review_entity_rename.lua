local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
  end
end

local function assert_true(value, message)
  if not value then error(message) end
end

local entity_rename = require("diff_review.views.plan_review.entity_rename")

local review_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, { "GeoParquetInspector" })
vim.api.nvim_win_set_buf(0, review_buf)
local review_win = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_cursor(review_win, { 1, 0 })

local entity = {
  action = "add",
  entity_id = "geo_inspector",
  name = "GeoParquetInspector",
}
local model = {
  entity_at_position = function() return entity end,
}
local original_input = vim.ui.input
local input_options = nil
vim.ui.input = function(options, callback)
  input_options = options
  callback("LocalFileInspector")
end
local renamed_entity = nil
local renamed_name = nil
entity_rename.prompt(model, review_buf, review_win, function(candidate, new_name)
  renamed_entity = candidate
  renamed_name = new_name
end)
vim.ui.input = original_input

assert_equals(input_options.default, "GeoParquetInspector", "Rename input should start with the current entity name")
assert_equals(renamed_entity, entity, "Rename should preserve the canonical entity identity")
assert_equals(renamed_name, "LocalFileInspector", "Rename should return the replacement name")

local original_notify = vim.notify
local notification = nil
vim.notify = function(message, level, options)
  notification = { message = message, level = level, title = options and options.title }
end
entity.action = "modify"
local input_opened = false
vim.ui.input = function()
  input_opened = true
end
entity_rename.prompt(model, review_buf, review_win, function()
  error("Existing entities must not reach the rename callback")
end)
vim.ui.input = original_input
vim.notify = original_notify

assert_true(not input_opened, "Existing entities should be rejected before opening rename input")
assert_equals(notification, {
  message = "Only newly added plan entities can be renamed",
  level = vim.log.levels.ERROR,
  title = "PlanReview",
}, "Existing entity rename should produce an error notification")

vim.cmd("qa!")
