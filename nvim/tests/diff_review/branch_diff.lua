vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local root = "D:/diffreview-flow-root"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
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

local branch_diff_text = table.concat({
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

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\tsomebranch" then
    branch_diff_requests = branch_diff_requests + 1
    return vim.split(branch_diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\tsomebranch\t--\tsrc/a.txt" then
    return vim.split(branch_diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\tmissingbranch" then
    return {}, 128
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tsomebranch" then
    return { "bbb2222" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\tsomebranch:src/a.txt" then
    return { "alpha", "old line", "omega" }, 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code, code ~= 0 and "unknown revision or path not in the working tree" or "")
  end, 3)
end

function backend.system(command)
  return "unexpected command: " .. command_key(command), 1
end

function backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = backend.system(command)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 3)
end

function backend.delete()
  return 0
end

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
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })

  -- ── open and render the branch diff ────────────────────────────────────────
  diff_review.open_branch_diff("somebranch")
  wait_for(function()
    local buf = vim.api.nvim_get_current_buf()
    return vim.bo[buf].filetype == "GitStatus" and buffer_contains(buf, "Changes vs somebranch")
  end, "branch diff did not render")
  local buf = vim.api.nvim_get_current_buf()
  assert_true(vim.api.nvim_buf_get_name(buf):find("GitBranchDiff", 1, true) ~= nil, "buffer name missing GitBranchDiff")
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
  trigger_buf_mapping(buf, "o", find_row(buf, "old line"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("GitFileRevision", 1, true) ~= nil
  end, "open on deleted line did not open a revision buffer")
  local revision_buf = vim.api.nvim_get_current_buf()
  local revision_name = vim.api.nvim_buf_get_name(revision_buf)
  assert_true(
    revision_name:find("GitFileRevision://src/a.txt@bbb2222", 1, true) ~= nil,
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
  trigger_buf_mapping(buf, "r")
  wait_for(function() return branch_diff_requests > requests_before end, "refresh did not re-run the diff")
  wait_for(function() return buffer_contains(buf, "Changes vs somebranch (1)") end, "refresh did not re-render")

  -- ── q closes the diff buffer ───────────────────────────────────────────────
  trigger_buf_mapping(buf, "q")
  assert_true(not vim.api.nvim_buf_is_valid(buf), "q did not close the branch diff buffer")

  -- ── single-file diff ───────────────────────────────────────────────────────
  diff_review.open_branch_diff("somebranch", { file = "src/a.txt" })
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
  diff_review.open_branch_diff("missingbranch")
  wait_for(function() return saw_notification_containing("Git diff failed") end, "missing branch error not notified")

  -- ── empty branch argument is rejected ──────────────────────────────────────
  captured_notifications = {}
  diff_review.open_branch_diff("  ")
  wait_for(function() return saw_notification_containing("requires a branch") end, "empty branch not rejected")
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
