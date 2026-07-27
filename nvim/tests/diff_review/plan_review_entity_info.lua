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
local entity_navigation = require("diff_review.views.plan_review.entity_navigation")
local comment_view = require("diff_review.views.plan_review.comment")

vim.o.columns = math.max(vim.o.columns, 160)

local document = {
  entity_changes = {
    {
      entity_id = "entity_geo_parquet_inspector",
      name = "GeoParquetInspector",
      description = "Coordinates one local GeoParquet inspection.",
      members = {
        {
          member_id = "entity_geo_parquet_inspector_member_geometry_column",
          name = "geometry_column",
          description = "Stores the primary geometry column declared by GeoParquet metadata.",
        },
      },
      variants = {},
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
local member_anchor = {
  target = {
    target_type = "entity_member",
    entity_id = "entity_geo_parquet_inspector",
    member_id = "entity_geo_parquet_inspector_member_geometry_column",
  },
}
local resolved_member = task_model.described_item_at_position(
  document,
  member_anchor,
  "    + geometry_column: String",
  10
)
assert_equals(
  resolved_member and resolved_member.name,
  "geometry_column",
  "PlanReview should resolve a described member through its canonical navigation anchor"
)

local review_buf = vim.api.nvim_create_buf(false, true)
local review_line_list = {
  "*struct GeoParquetInspector [hello/src/inspection.rs]",
  "    + geometry_column: String",
  "Use GeoParquetInspector at the command boundary.",
  "Move here to close the popup.",
}
vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, review_line_list)
vim.api.nvim_win_set_buf(0, review_buf)
local review_win = vim.api.nvim_get_current_win()
comment_view.attach(review_buf, review_win, review_line_list, {})
vim.api.nvim_win_set_cursor(review_win, { 3, 8 })
local model = {
  entity_at_position = function(_, line, byte_col)
    return task_model.entity_at_position(document, line, byte_col)
  end,
  described_item_at_position = function(_, source_line, line, byte_col)
    local anchor = source_line == 2 and member_anchor or nil
    return task_model.described_item_at_position(document, anchor, line, byte_col)
  end,
  entity_declaration_source_line = function() return 1 end,
  workspace_target_at_position = function() return nil end,
}

assert_equals(
  entity_info.show(model, review_buf, review_win, { source_line = 3 }),
  true,
  "PlanReview should handle a matching planned entity"
)

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
vim.api.nvim_win_set_cursor(review_win, { 4, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review_buf })
assert_true(
  vim.wait(1000, function() return not vim.api.nvim_win_is_valid(entity_info_win) end, 10),
  "PlanReview entity info should close when the review cursor moves"
)

vim.api.nvim_win_set_cursor(review_win, { 2, 10 })
local member_info_handled = false
vim.keymap.set("n", "ol", function()
  member_info_handled = entity_info.show(model, review_buf, review_win, { source_line = 2 })
end, { buffer = review_buf, silent = true, nowait = true })
vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("ol", true, false, true), "xt", false)
assert_equals(
  member_info_handled,
  true,
  "PlanReview ol should handle a matching planned member"
)
local member_info_win = nil
for _, candidate_win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
  if candidate_win ~= review_win and vim.api.nvim_win_get_config(candidate_win).relative ~= "" then
    member_info_win = candidate_win
  end
end
assert_true(member_info_win ~= nil, "PlanReview member info should open a floating window")
assert_equals(
  vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(member_info_win), 0, -1, false),
  { "Stores the primary geometry column", "declared by GeoParquet metadata." },
  "PlanReview member info should render the canonical member description"
)
vim.api.nvim_win_set_cursor(review_win, { 4, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review_buf })
assert_true(
  vim.wait(1000, function() return not vim.api.nvim_win_is_valid(member_info_win) end, 10),
  "PlanReview member info should close when the review cursor moves"
)

vim.api.nvim_win_set_cursor(review_win, { 3, 8 })
entity_navigation.jump(model, review_buf, review_win, {
  source_line = 3,
  plan_id = "plan",
  expected_version = 1,
  request = function() error("Planned entity navigation must not request Rust source") end,
})
assert_equals(
  vim.api.nvim_win_get_cursor(review_win),
  { 1, 8 },
  "PlanReview entity navigation should land on the matching UML declaration name"
)
assert_equals(
  vim.fn.getpos("''")[2],
  3,
  "PlanReview entity navigation should preserve the origin in the previous-context mark"
)
vim.cmd("normal! \15")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win)[1],
  3,
  "PlanReview entity navigation should push its origin onto the native jumplist"
)
entity_navigation.jump(model, review_buf, review_win, {
  source_line = 3,
  plan_id = "plan",
  expected_version = 1,
  request = function() error("Planned entity navigation must not request Rust source") end,
})
vim.cmd("normal! ''")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win)[1],
  3,
  "The previous-context mark should return to the entity reference"
)

vim.cmd("qa!")
