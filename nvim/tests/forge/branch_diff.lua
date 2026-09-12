local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)

local forge = require("forge")
local status = require("forge.status")
local source_document = require("forge.source_document")

local root = "D:/diffreview-flow-root"

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

local branch_diff_requests = 0

---@param target string
---@return table
local function metadata(target)
  return {
    target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } },
    decoration = {}, editable_region = {}, visible_decoration = {}, fold = {}, gutter = {},
  }
end

---@param document string
---@param file? string
---@return table
local function comparison_snapshot(document, file)
  local snapshot = fixture.snapshot(document, { path = "a.txt", view = { kind = "comparison", title = "somebranch", path = file, worktree = true } })
  snapshot.file[1].stats = { state = "exact", added = 1, deleted = 1 }
  return snapshot
end

status._set_runner_for_test(function(method, params, callback)
  assert(method == "status", "BranchDiff must use the native status route")
  if params.operation == "comparison" then
    if params.reference == "missingbranch" then
      callback(nil, "Git diff failed: unknown revision or path not in the working tree")
    else
      branch_diff_requests = branch_diff_requests + 1
      callback(comparison_snapshot(params.document, params.path and vim.base64.decode(params.path) or nil))
    end
  elseif params.operation == "demand" then
    callback(fixture.body(params.input.document, { "old line", "NEW LINE" }))
  elseif params.operation == "close_view" then
    callback(nil)
  elseif params.operation == "open_target" then
    callback({
      id = "comparison-open-source", kind = "open_source", document = params.input.document,
      revision = params.input.revision, view = params.input.view, sequence = params.input.sequence,
      workspace = root, path = vim.base64.encode("src/a.txt"), source_revision = "somebranch", row = 1,
    })
  elseif params.operation == "refresh" then
    branch_diff_requests = branch_diff_requests + 1
    callback(vim.NIL)
  elseif params.operation == "close" then
    callback({ closed = true })
  else
    error("unexpected BranchDiff native status operation: " .. params.operation)
  end
end)

source_document._set_runner_for_test(function(method, params, callback)
  assert(method == "source.document" and params.operation == "open", "BranchDiff must use the native source route")
  callback({
    title = "src/a.txt", object = "bbb2222", revision = "bbb2222", source_row = 1, more = false,
    snapshot = { document = params.document, revision = 0,
      block = { { id = "source", text = { "alpha", "old line", "omega" }, metadata = metadata("source") } } },
  })
end)

local function wait_for(condition, message)
  assert_true(vim.wait(2000, condition, 10), message)
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

local function buf_mapping(buf, key)
  return vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
end

local function trigger_buf_mapping(buf, key, row)
  if row then
    vim.api.nvim_win_set_cursor(vim.fn.bufwinid(buf), { row, 0 })
  end
  local mapping = buf_mapping(buf, key)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

local function run()
  vim.notify = capture_notify
  forge.setup({ about_auto_generate = false })

  -- ── open and render the branch diff ────────────────────────────────────────
  forge.open_branch_diff("somebranch", { cwd = root })
  wait_for(function()
    local buf = vim.api.nvim_get_current_buf()
    return vim.bo[buf].filetype == "ForgeStatus" and buffer_contains(buf, "Changes vs somebranch")
  end, "branch diff did not render")
  local buf = vim.api.nvim_get_current_buf()
  assert_true(vim.api.nvim_buf_get_name(buf):find("ForgeBranchDiff", 1, true) ~= nil, "buffer name missing ForgeBranchDiff")
  assert_true(buffer_contains(buf, "somebranch -> working tree"), "head line missing")
  assert_true(buffer_contains(buf, "Changes vs somebranch (1)"), "section title missing file count")
  assert_true(buffer_contains(buf, "a.txt +1 -1"), "file row missing")

  -- ── diff content expands via toggle ────────────────────────────────────────
  trigger_buf_mapping(buf, "<Tab>", find_row(buf, "a.txt +1 -1"))
  wait_for(function() return buffer_contains(buf, "NEW LINE") end, "toggle did not expand the hunk")

  -- ── status-only actions are absent in the diff view ────────────────────────
  for _, key in ipairs({ "S", "U", "cc", "opp", "ow" }) do
    local mapping = buf_mapping(buf, key)
    assert_true(mapping.buffer ~= 1, ("status-only key %q must not be mapped in the diff view"):format(key))
  end
  assert_true(not buffer_contains(buf, "stage"), "hint row must not advertise staging")

  -- ── open on a deleted line shows the branch revision of the file ───────────
  trigger_buf_mapping(buf, "<CR>", find_row(buf, "old line"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("ForgeFileRevision", 1, true) ~= nil
  end, "open on deleted line did not open a revision buffer")
  local revision_buf = vim.api.nvim_get_current_buf()
  local revision_name = vim.api.nvim_buf_get_name(revision_buf)
  assert_true(
    revision_name:find("ForgeFileRevision://src/a.txt@bbb2222", 1, true) ~= nil,
    "wrong revision buffer name: " .. revision_name
  )
  assert_true(vim.bo[revision_buf].readonly, "revision buffer must be readonly")
  assert_true(not vim.bo[revision_buf].modifiable, "revision buffer must not be modifiable")
  local revision_row = vim.api.nvim_win_get_cursor(0)[1]
  local revision_line = vim.api.nvim_buf_get_lines(revision_buf, revision_row - 1, revision_row, false)[1]
  assert_true(revision_line == "old line", "cursor not on the deleted line: " .. tostring(revision_line))
  vim.api.nvim_win_set_buf(0, buf)

  -- ── refresh re-runs the branch diff ────────────────────────────────────────
  local requests_before = branch_diff_requests
  trigger_buf_mapping(buf, "R")
  wait_for(function() return branch_diff_requests > requests_before end, "refresh did not re-run the diff")
  wait_for(function() return buffer_contains(buf, "Changes vs somebranch (1)") end, "refresh did not re-render")

  -- ── q closes the diff buffer ───────────────────────────────────────────────
  trigger_buf_mapping(buf, "q")
  assert_true(not vim.api.nvim_buf_is_valid(buf), "q did not close the branch diff buffer")

  -- ── single-file diff ───────────────────────────────────────────────────────
  forge.open_branch_diff("somebranch", { cwd = root, file = "src/a.txt" })
  wait_for(function()
    local file_buf = vim.api.nvim_get_current_buf()
    return buffer_contains(file_buf, "Changes vs somebranch (1)")
  end, "single-file branch diff did not render")
  local file_buf = vim.api.nvim_get_current_buf()
  assert_true(buffer_contains(file_buf, "File:"), "single-file head line missing")
  assert_true(buffer_contains(file_buf, "src/a.txt"), "single-file path missing from head lines")
  trigger_buf_mapping(file_buf, "q")

  -- ── unknown branch surfaces the git error ──────────────────────────────────
  captured_notifications = {}
  forge.open_branch_diff("missingbranch", { cwd = root })
  wait_for(function() return saw_notification_containing("Git diff failed") end, "missing branch error not notified")

  -- ── empty branch argument is rejected ──────────────────────────────────────
  captured_notifications = {}
  forge.open_branch_diff("  ")
  wait_for(function() return saw_notification_containing("requires a branch") end, "empty branch not rejected")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
status._set_runner_for_test(nil)
source_document._set_runner_for_test(nil)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
