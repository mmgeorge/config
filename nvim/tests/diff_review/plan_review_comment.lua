vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function find_row(buf, text)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(text, 1, true) then return row end
  end
  error("missing row containing " .. vim.inspect(text), 2)
end

local ok, failure = pcall(function()
  require("diff_review").setup({ harness = { backend = "mock" } })
  local comment_view = require("diff_review.views.plan_review.comment")
  local source_lines = { "# Plan", "Inspect this boundary", "Finish" }
  local annotation_list = {}
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, source_lines)

  comment_view.attach(buf, win, source_lines, annotation_list)
  assert_true(vim.wo[win].number, "PlanReview windows should show absolute line numbers")
  assert_true(not vim.wo[win].relativenumber, "PlanReview windows should not show relative line numbers")
  vim.api.nvim_win_set_cursor(win, { 2, 0 })
  comment_view.add_at_cursor(buf)
  vim.wait(20)
  vim.cmd("stopinsert")

  assert_true(vim.bo[buf].modifiable, "new inline plan comment should expose an editable body")
  local header_row = find_row(buf, " Plan comment ")
  local footer_row = header_row + 2
  local full_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(full_lines[footer_row]:match("^%-+$") ~= nil, "focused comment should use the shared full-width footer")

  vim.api.nvim_buf_set_lines(buf, header_row, header_row + 1, false, {
    "Name the exact module",
    "Cover the integration boundary",
  })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  vim.api.nvim_win_set_cursor(win, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })

  assert_true(not vim.bo[buf].modifiable, "leaving the plan comment should restore the read-only projection")
  local compact_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(table.concat(compact_lines, "\n"):find("╭─", 1, true) ~= nil,
    "unfocused plan comment should use the compact comment-box renderer")
  local compact_body_row = find_row(buf, "Name the exact module")
  assert_equals(annotation_list[1].body, "Name the exact module\nCover the integration boundary",
    "collapse should retain the multiline inline body")

  vim.api.nvim_win_set_cursor(win, { compact_body_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "moving onto a compact comment should expand its editable body")
  assert_true(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"):find("╭─", 1, true) == nil,
    "focused plan comment should replace compact box chrome with full inline rules")

  assert_equals(comment_view.serialize(buf), {
    { line = 2, body = "Name the exact module\nCover the integration boundary" },
  }, "serialized annotations should retain source-line identity across display expansion")

  comment_view.detach(buf)
  assert_equals(vim.api.nvim_buf_get_lines(buf, 0, -1, false), source_lines,
    "detaching PlanReview comments should restore the physical plan projection")
  assert_true(not vim.bo[buf].modifiable, "detached plan projection should remain read-only")
  vim.api.nvim_buf_delete(buf, { force = true })
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
