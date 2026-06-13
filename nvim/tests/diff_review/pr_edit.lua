vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local original_notify = vim.notify
local captured_notifications = {}
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = {
    message = tostring(message),
    level = level,
    opts = opts,
  }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

local pr_diff_text = table.concat({
  "diff --git a/src/a.txt b/src/a.txt",
  "index 1111111..2222222 100644",
  "--- a/src/a.txt",
  "+++ b/src/a.txt",
  "@@ -1,3 +1,3 @@",
  " alpha",
  "-old line",
  "+NEW LINE",
  " omega",
}, "\n")

---@type { command: string[], input: string? }[]
local edit_calls = {}
local edit_should_fail = false

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  vim.defer_fn(function()
    if key:find("gh pr diff", 1, true) then
      cb({ code = 0, stdout = pr_diff_text, stderr = "", output = pr_diff_text })
      return
    end
    if key:find("gh pr edit", 1, true) then
      edit_calls[#edit_calls + 1] = { command = command, input = input }
      if edit_should_fail then
        cb({ code = 1, stdout = "", stderr = "mock edit failure", output = "mock edit failure" })
      else
        cb({ code = 0, stdout = "", stderr = "", output = "" })
      end
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
  end, 3)
end

---@type DiffReviewGitBackend
local git_backend = {}

function git_backend.systemlist(command)
  return {}, 1
end

function git_backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    cb({}, 1)
  end, 3)
end

function git_backend.system(command)
  return "unexpected git command: " .. table.concat(command, " "), 1
end

function git_backend.system_async(command, input, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "", output = "" })
  end, 3)
end

function git_backend.delete()
  return 0
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local pr_edit_ns = vim.api.nvim_create_namespace("diff_review_pr_edit")

--- 1-based rows that carry the "*" out-of-sync marker.
---@param buf integer
---@return integer[]
local function marker_rows(buf)
  local rows = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, pr_edit_ns, 0, -1, { details = true })) do
    local details = mark[4] or {}
    local virt = details.virt_text
    if virt and virt[1] and virt[1][1] == "*" then rows[#rows + 1] = mark[2] + 1 end
  end
  table.sort(rows)
  return rows
end

--- Move the cursor and fire the CursorMoved autocmd that re-syncs
--- 'modifiable' (nvim_win_set_cursor does not fire it on its own).
local function move_cursor(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
end

--- Edit one buffer line the way a gated insert would, then leave insert.
local function edit_line(buf, row, text)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, row - 1, row, false, { text })
  vim.api.nvim_exec_autocmds("InsertLeave", { buffer = buf })
end

local function trigger_buf_mapping(buf, key)
  vim.api.nvim_win_set_buf(0, buf)
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

local pr = {
  number = 7,
  title = "Old title",
  body = "Line one\nLine two",
  url = "https://github.com/owner/repo/pull/7",
  headRefName = "feature",
  headRefOid = "abc1234def5678abc1234def5678abc1234def56",
  commits = {},
  files = { { path = "src/a.txt", additions = 1, deletions = 1 } },
  changedFiles = 1,
  additions = 1,
  deletions = 1,
}

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })

  local buf = diff_review.open_pr(pr, { cwd = "D:/diffreview-pr-edit-root" })
  assert_true(buf ~= nil, "open_pr did not return a buffer")
  wait_for(function() return buffer_contains(buf, "Title:  Old title") end, "PR view did not render the title")
  assert_true(buffer_contains(buf, "Line one"), "PR body did not render")
  assert_true(vim.bo[buf].buftype == "acwrite", "PR buffer must be acwrite")
  assert_true(not vim.bo[buf].modifiable, "PR buffer must start nomodifiable")

  local title_row = find_row(buf, "Title:  Old title")
  local body_row = find_row(buf, "Line one")

  -- ── the buffer unlocks exactly on the editable regions ─────────────────────
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not vim.bo[buf].modifiable, "buffer must stay locked outside the editable regions")
  move_cursor(buf, title_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on the title row")
  move_cursor(buf, find_row(buf, "Description:"))
  assert_true(not vim.bo[buf].modifiable, "the Description: label itself must stay locked")
  move_cursor(buf, body_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on description rows")
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not vim.bo[buf].modifiable, "buffer must relock after leaving the regions")

  -- ── edits flag the field with a "*" marker ──────────────────────────────────
  edit_line(buf, title_row, "Title:  New title")
  local rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == title_row, "title edit did not add exactly one marker on the title row")
  assert_true(vim.bo[buf].modified, "title edit must mark the buffer modified")

  edit_line(buf, body_row, "Line one EDITED")
  rows = marker_rows(buf)
  assert_true(#rows == 2, "description edit did not add a second marker")
  assert_true(rows[2] == find_row(buf, "Description:"), "description marker is not on the Description: label")

  -- ── re-renders are blocked while dirty ──────────────────────────────────────
  captured_notifications = {}
  trigger_buf_mapping(buf, "R")
  vim.wait(120, function() return false end, 10)
  assert_true(buffer_contains(buf, "New title"), "refresh clobbered unsynced edits")
  assert_true(saw_notification_containing("Unsynced PR edits"), "blocked refresh did not explain itself")

  -- ── :w clears markers immediately and syncs through the queue ──────────────
  captured_notifications = {}
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  assert_true(#marker_rows(buf) == 0, "markers must clear immediately on save")
  assert_true(not vim.bo[buf].modified, "save must clear the modified flag")
  wait_for(function() return #edit_calls == 1 end, "gh pr edit was not invoked")
  local call = edit_calls[1]
  local call_key = table.concat(call.command, " ")
  assert_true(call_key:find("gh pr edit 7", 1, true) ~= nil, "wrong gh edit command: " .. call_key)
  assert_true(call_key:find("--title New title", 1, true) ~= nil, "title missing from gh edit: " .. call_key)
  assert_true(call_key:find("--body-file -", 1, true) ~= nil, "body flag missing from gh edit: " .. call_key)
  assert_true(call.input == "Line one EDITED\nLine two", "wrong body sent: " .. tostring(call.input))
  wait_for(function() return saw_notification_containing("PR #7 updated") end, "successful sync was not notified")
  assert_true(pr.title == "New title", "PR cache title was not updated")
  assert_true(pr.body == "Line one EDITED\nLine two", "PR cache body was not updated")
  assert_true(#marker_rows(buf) == 0, "markers must stay clear after a successful sync")
  assert_true(buffer_contains(buf, "Title:  New title"), "post-sync re-render lost the new title")

  -- ── unchanged save is a no-op ───────────────────────────────────────────────
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  vim.wait(120, function() return false end, 10)
  assert_true(#edit_calls == 1, "no-op save must not call gh")

  -- ── failed sync restores the markers and notifies ───────────────────────────
  edit_should_fail = true
  captured_notifications = {}
  edit_line(buf, find_row(buf, "Title:  New title"), "Title:  Broken title")
  assert_true(#marker_rows(buf) == 1, "second title edit did not mark dirty")
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  assert_true(#marker_rows(buf) == 0, "markers must clear immediately on save even when sync later fails")
  wait_for(function() return saw_notification_containing("GitHub PR update failed") end, "failed sync was not notified")
  assert_true(#marker_rows(buf) == 1, "failed sync must restore the dirty marker")
  assert_true(vim.bo[buf].modified, "failed sync must restore the modified flag")
  assert_true(pr.title == "New title", "failed sync must not update the PR cache")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review.reset_git_backend()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
