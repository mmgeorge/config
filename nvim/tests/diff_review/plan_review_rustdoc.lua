local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
  end
end

local entity_info = require("diff_review.views.plan_review.entity_info")
local dependency_browser = require("diff_review.views.plan_review.dependency_browser")

vim.o.columns = math.max(vim.o.columns, 120)
vim.o.lines = math.max(vim.o.lines, 40)

local source_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(
  source_buf,
  0,
  -1,
  false,
  { "  ├─ Read geoparquet_metadata() from ParquetRecordBatchReaderBuilder" }
)
local source_win = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(source_win, source_buf)
vim.api.nvim_win_set_cursor(source_win, { 1, 12 })

local captured_params = nil
local rustdoc_callback = nil
local model = {
  entity_at_position = function() return nil end,
  described_item_at_position = function() return nil end,
  rustdoc_target_at_position = function(_, source_line, _, _)
    assert_equals(source_line, 22, "Rustdoc hover should preserve the canonical source line")
    return {
      flow_id = "metadata",
      step_id = "read",
      edge_id = "geoparquet_metadata",
      selection = "callable",
    }
  end,
}
local function show_rustdoc()
  return entity_info.show_context(model, source_buf, source_win, {
    source_line = 22,
    plan_id = "plan",
    expected_version = 7,
    request = function(params, callback)
      captured_params = params
      rustdoc_callback = callback
    end,
  })
end
vim.keymap.set("n", "ol", function()
  assert_equals(show_rustdoc(), true, "Rustdoc hover should claim a callable token")
end, { buffer = source_buf, silent = true, nowait = true })
vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("ol", true, false, true), "xt", false)
vim.api.nvim_win_set_cursor(source_win, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = source_buf })
vim.wait(60)
assert_equals(captured_params.plan_id, "plan", "Rustdoc hover should send the canonical plan identity")
assert_equals(captured_params.expected_version, 7, "Rustdoc hover should guard the plan version")

local function rustdoc_popup()
  return vim.iter(vim.api.nvim_list_wins()):find(function(win)
    return win ~= source_win and vim.api.nvim_win_get_config(win).relative ~= ""
  end)
end
assert_equals(
  rustdoc_popup(),
  nil,
  "Rustdoc hover should not open a loading popup while documentation is pending"
)
rustdoc_callback({
  signature = "fn geoparquet_metadata(&self) -> GeoParquetMetadata",
  docs = "Returns decoded GeoParquet metadata.",
})
assert_equals(
  vim.wait(1000, function() return rustdoc_popup() ~= nil end, 10),
  true,
  "Rustdoc hover should finish its deferred cursor-settling phase"
)
local popup_win = rustdoc_popup()
if not popup_win then error("Rustdoc hover should open a popup after documentation loads") end
local popup_lines = vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(popup_win), 0, -1, false)
assert_equals(popup_lines, {
  "```rust",
  "fn geoparquet_metadata(&self) -> GeoParquetMetadata",
  "```",
  "",
  "Returns decoded GeoParquet metadata.",
}, "Rustdoc hover should open with the exact signature and full docs")

vim.wait(60)
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = source_buf })
assert_equals(
  vim.api.nvim_win_is_valid(popup_win),
  true,
  "Rustdoc hover should anchor to the settled cursor position"
)
vim.api.nvim_win_set_cursor(source_win, { 1, 13 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = source_buf })
assert_equals(
  vim.api.nvim_win_is_valid(popup_win),
  false,
  "Rustdoc hover should close after the cursor leaves its original position"
)

local stale_callback = nil
vim.api.nvim_win_set_cursor(source_win, { 1, 12 })
entity_info.show_context(model, source_buf, source_win, {
  source_line = 22,
  plan_id = "plan",
  expected_version = 7,
  request = function(_, callback) stale_callback = callback end,
})
vim.wait(60)
vim.api.nvim_win_set_cursor(source_win, { 1, 13 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = source_buf })
stale_callback({
  signature = "fn geoparquet_metadata(&self) -> GeoParquetMetadata",
  docs = "Returns decoded GeoParquet metadata.",
})
assert_equals(
  vim.iter(vim.api.nvim_list_wins()):find(function(win)
  return win ~= source_win and vim.api.nvim_win_get_config(win).relative ~= ""
  end),
  nil,
  "Rustdoc hover should discard documentation that arrives after cursor movement"
)

vim.api.nvim_buf_set_lines(source_buf, 0, -1, false, {
  "  ├─ Construct GeoParquetInspector",
})
vim.api.nvim_win_set_cursor(source_win, { 1, 18 })
local rustdoc_request_count = 0
local planned_model = {
  entity_at_position = function()
    return {
      name = "GeoParquetInspector",
      description = "Coordinates one local GeoParquet inspection.",
    }
  end,
  described_item_at_position = function()
    return {
      name = "GeoParquetInspector",
      description = "Coordinates one local GeoParquet inspection.",
    }
  end,
  rustdoc_target_at_position = function()
    error("Planned entities must resolve before Rustdoc targets")
  end,
}
assert_equals(
  entity_info.show_context(planned_model, source_buf, source_win, {
    source_line = 22,
    plan_id = "plan",
    expected_version = 7,
    request = function()
      rustdoc_request_count = rustdoc_request_count + 1
    end,
  }),
  true,
  "Entity information should resolve a planned flow receiver before Rustdoc"
)
assert_equals(rustdoc_request_count, 0, "Planned flow receivers should not issue Rustdoc requests")
local planned_popup_win = vim.iter(vim.api.nvim_list_wins()):find(function(win)
  return win ~= source_win and vim.api.nvim_win_get_config(win).relative ~= ""
end)
if not planned_popup_win then error("Planned entity information should open a popup") end
assert_equals(
  vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(planned_popup_win), 0, -1, false),
  { "Coordinates one local GeoParquet", "inspection." },
  "Planned flow receivers should show their canonical plan description"
)
vim.wait(60)
vim.api.nvim_win_set_cursor(source_win, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = source_buf })
assert_equals(
  vim.api.nvim_win_is_valid(planned_popup_win),
  false,
  "Planned entity information should close before the test exits"
)

local opened_url = nil
dependency_browser.opener = function(url)
  opened_url = url
  return true
end
assert_equals(dependency_browser.open("datafusion"), true, "Dependency browsing should open valid package names")
assert_equals(opened_url, "https://crates.io/crates/datafusion", "Dependency browsing should use crates.io")

vim.cmd("qa!")
