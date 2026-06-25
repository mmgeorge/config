vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.integrations.gh")

local root = "D:/diffreview-file-revision-root"

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

--- Unified diff with one modified line ("old" -> "NEW") at line 2.
---@param relpath string
---@return string
local function modified_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1,3 +1,3 @@",
    " alpha " .. relpath,
    "-old " .. relpath,
    "+NEW " .. relpath,
    " omega " .. relpath,
  }, "\n")
end

---@param relpath string
---@return string[]
local function original_lines(relpath)
  return { "alpha " .. relpath, "old " .. relpath, "omega " .. relpath }
end

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
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { "abc1234" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { "file revision test" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return { "M\ta.txt", "M\tb.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return { "M\tc.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(modified_diff("a.txt") .. "\n" .. modified_diff("b.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return vim.split(modified_diff("c.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:a.txt" then
    return original_lines("a.txt"), 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\tHEAD:c.txt" then
    return original_lines("c.txt"), 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tfeature" then
    return { "fff5678" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\tfeature:a.txt" then
    return original_lines("a.txt"), 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
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

local function trigger_buf_mapping(buf, key, row)
  vim.api.nvim_win_set_buf(0, buf)
  if row then
    vim.api.nvim_win_set_cursor(0, { row, 0 })
  end
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

---@param expected_name string
---@param expected_cursor_text string
---@return integer buf
local function assert_revision_buffer(expected_name, expected_cursor_text)
  local buf = vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(buf)
  assert_true(name:find(expected_name, 1, true) ~= nil, "wrong revision buffer name: " .. name)
  assert_true(vim.bo[buf].buftype == "nowrite", "revision buffer must be nowrite")
  assert_true(vim.bo[buf].readonly, "revision buffer must be readonly")
  assert_true(not vim.bo[buf].modifiable, "revision buffer must not be modifiable")
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  assert_true(line == expected_cursor_text, ("cursor on %q, expected %q"):format(line, expected_cursor_text))
  return buf
end

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })

  diff_review.open()
  local status_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(status_buf, "Unstaged changes (2)") end, "status did not render")
  -- ── open on an unstaged deleted line lands in the index revision ───────────
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "a.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old a.txt") end, "a.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old a.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("GitFileRevision", 1, true) ~= nil
  end, "open on unstaged deleted line did not open a revision buffer")
  local index_buf = assert_revision_buffer("GitFileRevision://a.txt@abc1234", "old a.txt")
  assert_true(
    vim.wo.winbar:find("a.txt @ abc1234", 1, true) ~= nil,
    "revision winbar header missing: " .. vim.wo.winbar
  )
  -- ── the buffer stays alive, loaded, and listed once hidden ─────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  assert_true(
    vim.wo.winbar:find("read-only revision", 1, true) == nil,
    "winbar header must clear when leaving the revision buffer, got: " .. vim.wo.winbar
  )
  assert_true(vim.api.nvim_buf_is_valid(index_buf), "revision buffer must stay alive when hidden")
  assert_true(vim.api.nvim_buf_is_loaded(index_buf), "revision buffer must stay loaded when hidden")
  assert_true(vim.bo[index_buf].buflisted, "revision buffer must stay listed when hidden")
  -- ── re-opening reuses the same buffer ──────────────────────────────────────
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old a.txt"))
  wait_for(function() return vim.api.nvim_get_current_buf() == index_buf end, "revision buffer was not reused")
  -- ── open on a staged deleted line lands in the HEAD revision ───────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  if not buffer_contains(status_buf, "c.txt +1 -1") then
    trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "Staged changes (1)"))
  end
  wait_for(function() return buffer_contains(status_buf, "c.txt +1 -1") end, "staged section did not expand")
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "c.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old c.txt") end, "c.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old c.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("c.txt@", 1, true) ~= nil
  end, "open on staged deleted line did not open the HEAD revision")
  assert_revision_buffer("GitFileRevision://c.txt@abc1234", "old c.txt")
  -- ── added lines still jump to the working-tree file ────────────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "NEW a.txt"))
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("a.txt", 1, true) ~= nil
      and vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("GitFileRevision", 1, true) == nil
  end, "open on added line did not edit the working-tree file")
  -- ── failed base lookup falls back to the working-tree jump ─────────────────
  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_buf_mapping(status_buf, "<Tab>", find_row(status_buf, "b.txt +1 -1"))
  wait_for(function() return buffer_contains(status_buf, "old b.txt") end, "b.txt hunk did not expand")
  trigger_buf_mapping(status_buf, "<CR>", find_row(status_buf, "old b.txt"))
  wait_for(function()
    local name = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
    return name:find("b.txt", 1, true) ~= nil and name:find("GitFileRevision", 1, true) == nil
  end, "failed base lookup did not fall back to the working-tree file")
  -- ── :GitFileRevision entry point names the buffer with the short sha ───────
  diff_review.open_file_revision(root .. "/a.txt", "feature")
  wait_for(function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()):find("@fff5678", 1, true) ~= nil
  end, "open_file_revision did not open the requested revision")
  assert_revision_buffer("GitFileRevision://a.txt@fff5678", "alpha a.txt")
  -- ── q closes the revision buffer ───────────────────────────────────────────
  local q_buf = vim.api.nvim_get_current_buf()
  trigger_buf_mapping(q_buf, "q")
  assert_true(not vim.api.nvim_buf_is_valid(q_buf), "q did not close the revision buffer")
  -- ── command errors are reported ────────────────────────────────────────────
  captured_notifications = {}
  diff_review.open_file_revision(root .. "/a.txt", "missingrev")
  wait_for(function() return saw_notification_containing("missingrev") end, "missing revision error not notified")

  captured_notifications = {}
  diff_review.open_file_revision("", "")
  wait_for(function() return saw_notification_containing("requires a file and a revision") end, "empty args not rejected")
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
