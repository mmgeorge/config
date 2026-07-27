local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
  end
end

local function assert_true(value, message)
  if not value then error(message) end
end

local task_model = require("diff_review.views.plan_review.task_model")
local entity_info = require("diff_review.views.plan_review.entity_info")
local comment_view = require("diff_review.views.plan_review.comment")

vim.o.columns = math.max(vim.o.columns, 160)

local document = {
  entity_changes = {
    {
      name = "GeoParquetInspector",
      description = "Coordinates one local GeoParquet inspection.",
    },
  },
}

local resolved_entity =
  task_model.entity_at_position(document, "Use GeoParquetInspector at the command boundary.", 8)
assert_equals(
  resolved_entity and resolved_entity.name,
  "GeoParquetInspector",
  "PlanReview should resolve the entity whose rendered name contains the cursor"
)
assert_equals(
  task_model.entity_at_position(document, "GeoParquetInspectorFactory", 8),
  nil,
  "PlanReview should not resolve an entity name embedded inside a longer identifier"
)

local review_buf = vim.api.nvim_create_buf(false, true)
local review_line_list = {
  "*struct GeoParquetInspector [hello/src/inspection.rs]",
  "Use GeoParquetInspector at the command boundary.",
  "Move here to close the popup.",
}
vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, review_line_list)
vim.api.nvim_win_set_buf(0, review_buf)
local review_win = vim.api.nvim_get_current_win()
comment_view.attach(review_buf, review_win, review_line_list, {})
vim.api.nvim_win_set_cursor(review_win, { 2, 8 })
local model = {
  entity_at_position = function(_, line, byte_col)
    return task_model.entity_at_position(document, line, byte_col)
  end,
  entity_declaration_source_line = function() return 1 end,
}

entity_info.show(model, review_buf, review_win)

local entity_info_win = nil
for _, candidate_win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
  if candidate_win ~= review_win and vim.api.nvim_win_get_config(candidate_win).relative ~= "" then
    entity_info_win = candidate_win
  end
end
assert_true(entity_info_win ~= nil, "PlanReview entity info should open an LSP-style floating window")
local entity_info_config = vim.api.nvim_win_get_config(entity_info_win)
assert_equals(entity_info_config.width, 40, "PlanReview entity info should use a fixed 40-column width")
assert_true(
  entity_info_config.border == nil
    or entity_info_config.border == "none"
    or #entity_info_config.border == 0
    or (
      type(entity_info_config.border) == "table"
      and vim.iter(entity_info_config.border):all(function(segment) return segment == "" end)
    ),
  "PlanReview entity info should render without an outline"
)
assert_equals(
  vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(entity_info_win), 0, -1, false),
  { "Coordinates one local GeoParquet", "inspection." },
  "PlanReview entity info should render only the canonical entity description"
)
assert_equals(
  vim.api.nvim_get_current_win(),
  review_win,
  "PlanReview entity info should preserve focus in the review window"
)
vim.api.nvim_win_set_cursor(review_win, { 3, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review_buf })
assert_true(
  vim.wait(1000, function() return not vim.api.nvim_win_is_valid(entity_info_win) end, 10),
  "PlanReview entity info should close when the review cursor moves"
)

vim.api.nvim_win_set_cursor(review_win, { 2, 8 })
entity_info.jump(model, review_buf, review_win)
assert_equals(
  vim.api.nvim_win_get_cursor(review_win),
  { 1, 8 },
  "PlanReview entity navigation should land on the matching UML declaration name"
)
assert_equals(
  vim.fn.getpos("''")[2],
  2,
  "PlanReview entity navigation should preserve the origin in the previous-context mark"
)
vim.cmd("normal! \15")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win)[1],
  2,
  "PlanReview entity navigation should push its origin onto the native jumplist"
)
entity_info.jump(model, review_buf, review_win)
vim.cmd("normal! ''")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win)[1],
  2,
  "The previous-context mark should return to the entity reference"
)

vim.cmd("qa!")
