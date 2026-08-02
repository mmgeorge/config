vim.loader.enable(false)

local session = require("diff_review.session")
local status_render = require("diff_review.views.status.status_render")

---@param message string
local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

---@param actual unknown
---@param expected unknown
---@param message string
local function assert_equal(actual, expected, message)
  if vim.deep_equal(actual, expected) then return end
  fail(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
end

---@class DiffReviewStatusLineWriteTestCall
---@field start_row integer
---@field end_row integer
---@field replacement string[]

local buf = vim.api.nvim_create_buf(false, true)
local old_lines = { "one", "anchor-a", "delete-me", "anchor-b", "five", "anchor-c", "seven" }
local new_lines = { "ONE", "anchor-a", "insert-a", "insert-b", "anchor-b", "FIVE", "anchor-c", "seven", "" }
vim.api.nvim_buf_set_lines(buf, 0, -1, false, old_lines)

local original_status = session.status
local original_set_lines = vim.api.nvim_buf_set_lines
local original_get_cursor = vim.api.nvim_win_get_cursor
local original_set_cursor = vim.api.nvim_win_set_cursor
local original_schedule = vim.schedule
local original_diff = vim.diff
local original_notify = vim.notify

---@type DiffReviewStatusLineWriteTestCall[]
local write_call_list = {}
local notification

vim.api.nvim_buf_set_lines = function(target_buf, start_row, end_row, strict_indexing, replacement)
  if target_buf == buf then
    write_call_list[#write_call_list + 1] = {
      start_row = start_row,
      end_row = end_row,
      replacement = vim.deepcopy(replacement),
    }
  end
  return original_set_lines(target_buf, start_row, end_row, strict_indexing, replacement)
end
vim.api.nvim_win_get_cursor = function() error("line reconciliation must not read cursor state") end
vim.api.nvim_win_set_cursor = function() error("line reconciliation must not move the cursor") end
vim.schedule = function() error("line reconciliation must apply every hunk in the current main-loop turn") end

local ok, error_message = xpcall(function()
  session.status = { lines = new_lines }
  status_render.status_write_rendered_buffer(buf)

  assert_equal(write_call_list, {
    { start_row = 7, end_row = 7, replacement = { "" } },
    { start_row = 4, end_row = 5, replacement = { "FIVE" } },
    { start_row = 2, end_row = 3, replacement = { "insert-a", "insert-b" } },
    { start_row = 0, end_row = 1, replacement = { "ONE" } },
  }, "disjoint line edits must apply from the bottom hunk upward")
  assert_equal(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false),
    new_lines,
    "bottom-up line reconciliation produced the wrong buffer"
  )

  vim.bo[buf].modifiable = true
  original_set_lines(buf, 0, -1, false, { "old", "keep", "tail" })
  vim.bo[buf].modifiable = false
  write_call_list = {}
  session.status = { lines = { "new", "keep", "tail" } }
  vim.diff = function() error("forced vim.diff failure") end
  vim.notify = function(message) notification = message end

  status_render.status_write_rendered_buffer(buf)

  assert_equal(write_call_list, {
    { start_row = 0, end_row = 1, replacement = { "new" } },
  }, "vim.diff errors must fall back to conservative single-span reconciliation")
  if not (notification and notification:find("single%-span fallback")) then
    fail("vim.diff fallback must notify the user")
  end
end, debug.traceback)

vim.api.nvim_buf_set_lines = original_set_lines
vim.api.nvim_win_get_cursor = original_get_cursor
vim.api.nvim_win_set_cursor = original_set_cursor
vim.schedule = original_schedule
vim.diff = original_diff
vim.notify = original_notify
session.status = original_status

if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
if not ok then fail(error_message) end

io.write("status_line_reconcile OK\n")
vim.cmd("qa!")
