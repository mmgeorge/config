vim.loader.enable(false)

local diff_review = require("diff_review")
local render_orchestrator = require("diff_review.views.status.render_orchestrator")
local gh = require("diff_review.integrations.gh")
local original_notify = vim.notify

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local test_root = vim.fs.normalize(original_cwd .. "/.diffreview-visual-selection-test")
local captured_notification_list = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function comparable_path(path)
  return vim.fs.normalize(path):gsub("\\", "/")
end

local function run_git(arguments)
  local command = { "git", "-C", test_root }
  vim.list_extend(command, arguments)
  local output = vim.fn.systemlist(command)
  local exit_code = vim.v.shell_error
  if exit_code ~= 0 then
    error(("git %s failed: %s"):format(table.concat(arguments, " "), table.concat(output, "\n")), 2)
  end
  return output
end

local function write_lines(path, lines)
  assert_true(vim.fn.writefile(lines, path) == 0, "writefile failed: " .. path)
end

local function contains_line(lines, needle)
  for _, line in ipairs(lines) do
    if line == needle then return true end
  end
  return false
end

local function contains_text(lines, needle)
  for _, line in ipairs(lines) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function find_named_file_rows(first_name, second_name)
  local buffer_lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local first_row = nil
  local second_row = nil
  for line_number, line in ipairs(buffer_lines) do
    if line:find(first_name, 1, true) then
      first_row = line_number
    elseif line:find(second_name, 1, true) then
      second_row = line_number
    end
  end
  assert_true(first_row ~= nil, "missing " .. first_name .. " status row:\n" .. table.concat(buffer_lines, "\n"))
  assert_true(second_row ~= nil, "missing " .. second_name .. " status row:\n" .. table.concat(buffer_lines, "\n"))
  return first_row, second_row
end

local function find_file_rows()
  return find_named_file_rows("a.txt", "b.txt")
end

local function trigger_visual_mapping_rows(key, first_row, second_row)
  vim.fn.setpos("'<", { 0, first_row, 1, 0 })
  vim.fn.setpos("'>", { 0, second_row, 1, 0 })
  local mapping = vim.fn.maparg(key, "x", false, true)
  assert_true(type(mapping.callback) == "function", "missing visual mapping for " .. key)
  mapping.callback()
end

local function trigger_visual_mapping(key)
  local first_row, second_row = find_file_rows()
  trigger_visual_mapping_rows(key, first_row, second_row)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function find_hunk_row_after(file_row)
  local buffer_lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for line_number = file_row + 1, #buffer_lines do
    if buffer_lines[line_number]:find("@@", 1, true) then return line_number end
    if buffer_lines[line_number]:find(".txt", 1, true) then break end
  end
  return nil
end

local function confirm_yes()
  local mapping = vim.fn.maparg("y", "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing discard confirmation mapping")
  mapping.callback()
end

local function notification_contains(needle)
  for _, notification in ipairs(captured_notification_list) do
    if notification:find(needle, 1, true) then return true end
  end
  return false
end

local function cleanup()
  pcall(vim.fn.chdir, original_cwd)
  vim.fn.delete(test_root, "rf")
  vim.notify = original_notify
end

local function run()
  vim.fn.delete(test_root, "rf")
  assert_true(vim.fn.mkdir(test_root, "p") == 1, "mkdir failed: " .. test_root)

  write_lines(test_root .. "/a.txt", { "one" })
  write_lines(test_root .. "/b.txt", { "one" })
  run_git({ "init" })
  run_git({ "config", "user.email", "test@example.com" })
  run_git({ "config", "user.name", "Test User" })
  run_git({ "add", "." })
  run_git({ "commit", "-m", "init" })

  write_lines(test_root .. "/a.txt", { "two" })
  write_lines(test_root .. "/b.txt", { "two" })

  local chdir_ok = pcall(vim.fn.chdir, test_root)
  assert_true(chdir_ok and comparable_path(vim.fn.getcwd()) == comparable_path(test_root), "chdir failed: " .. test_root)
  local root_output = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })
  assert_true(vim.v.shell_error == 0, "rev-parse failed: " .. table.concat(root_output, "\n"))
  assert_true(comparable_path(root_output[1]) == comparable_path(test_root), "wrong git root: " .. tostring(root_output[1]))

  gh.set_backend(gh_backend)
  vim.notify = function(message)
    captured_notification_list[#captured_notification_list + 1] = tostring(message)
  end
  diff_review.open()
  assert_true(vim.bo.filetype == "GitStatus", "GitStatus buffer did not open")
  local status_buf = vim.api.nvim_get_current_buf()
  render_orchestrator.render_status(status_buf)
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(status_buf, 0, -1, false)
    for _, line in ipairs(lines) do
      if line:find("a.txt", 1, true) and line:find("b.txt", 1, true) == nil then
        return true
      end
    end
    return false
  end, "DiffReview status did not render changed files")

  trigger_visual_mapping("S")
  local staged_after_stage = {}
  wait_for(function()
    staged_after_stage = run_git({ "diff", "--cached", "--name-only" })
    return contains_line(staged_after_stage, "a.txt") and contains_line(staged_after_stage, "b.txt")
  end, "visual S did not stage both selected files")
  assert_true(contains_line(staged_after_stage, "a.txt"), "a.txt was not staged")
  assert_true(contains_line(staged_after_stage, "b.txt"), "b.txt was not staged")
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(status_buf, 0, -1, false)
    for _, line in ipairs(lines) do
      if line:find("Staged changes", 1, true) then return true end
    end
    return false
  end, "DiffReview status did not refresh to staged section")

  trigger_visual_mapping("U")
  local staged_after_unstage = {}
  wait_for(function()
    staged_after_unstage = run_git({ "diff", "--cached", "--name-only" })
    return #staged_after_unstage == 0
  end, "visual U did not clear staged diff")
  assert_true(#staged_after_unstage == 0, "staged diff was not cleared by visual U")
  local unstaged_after_unstage = run_git({ "diff", "--name-only" })
  assert_true(contains_line(unstaged_after_unstage, "a.txt"), "a.txt was not unstaged")
  assert_true(contains_line(unstaged_after_unstage, "b.txt"), "b.txt was not unstaged")

  local discard_mapping = vim.fn.maparg("j", "x", false, true)
  assert_true(type(discard_mapping.callback) == "function", "missing visual mapping for j")

  local first_file_row, second_file_row = find_file_rows()
  trigger_normal_mapping("<Tab>", first_file_row)
  wait_for(function()
    local refreshed_first_row = find_file_rows()
    return find_hunk_row_after(refreshed_first_row) ~= nil
  end, "first file hunk did not render")
  _, second_file_row = find_file_rows()
  trigger_normal_mapping("<Tab>", second_file_row)
  local second_hunk_row = nil
  wait_for(function()
    _, second_file_row = find_file_rows()
    second_hunk_row = find_hunk_row_after(second_file_row)
    return second_hunk_row ~= nil
  end, "second file hunk did not render")

  first_file_row = find_file_rows()
  trigger_visual_mapping_rows("j", first_file_row, assert(second_hunk_row))
  confirm_yes()
  wait_for(function()
    return #run_git({ "diff", "--name-only" }) == 0
  end, "visual discard did not restore both selected files")
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(status_buf, 0, -1, false)
    return not contains_text(lines, "a.txt") and not contains_text(lines, "b.txt")
  end, "visual discard did not refresh the status view")
  assert_true(not notification_contains("Discard failed"), "visual discard executed covered hunk targets")
  assert_true(vim.fn.readfile(test_root .. "/a.txt")[1] == "one", "visual discard did not restore a.txt")
  assert_true(vim.fn.readfile(test_root .. "/b.txt")[1] == "one", "visual discard did not restore b.txt")

  captured_notification_list = {}
  write_lines(test_root .. "/untracked-a.txt", { "new" })
  write_lines(test_root .. "/untracked-b.txt", { "new" })
  render_orchestrator.render_status(status_buf)
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(status_buf, 0, -1, false)
    return contains_text(lines, "untracked-a.txt") and contains_text(lines, "untracked-b.txt")
  end, "DiffReview status did not render untracked files")

  local first_untracked_row, second_untracked_row = find_named_file_rows("untracked-a.txt", "untracked-b.txt")
  trigger_normal_mapping("<Tab>", first_untracked_row)
  wait_for(function()
    first_untracked_row = find_named_file_rows("untracked-a.txt", "untracked-b.txt")
    return find_hunk_row_after(first_untracked_row) ~= nil
  end, "first untracked hunk did not render")
  _, second_untracked_row = find_named_file_rows("untracked-a.txt", "untracked-b.txt")
  trigger_normal_mapping("<Tab>", second_untracked_row)
  local second_untracked_hunk_row = nil
  wait_for(function()
    _, second_untracked_row = find_named_file_rows("untracked-a.txt", "untracked-b.txt")
    second_untracked_hunk_row = find_hunk_row_after(second_untracked_row)
    return second_untracked_hunk_row ~= nil
  end, "second untracked hunk did not render")

  first_untracked_row = find_named_file_rows("untracked-a.txt", "untracked-b.txt")
  trigger_visual_mapping_rows("j", first_untracked_row, assert(second_untracked_hunk_row))
  confirm_yes()
  wait_for(function()
    return vim.fn.filereadable(test_root .. "/untracked-a.txt") == 0
      and vim.fn.filereadable(test_root .. "/untracked-b.txt") == 0
  end, "visual discard did not delete both untracked files")
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(status_buf, 0, -1, false)
    return not contains_text(lines, "untracked-a.txt") and not contains_text(lines, "untracked-b.txt")
  end, "visual untracked discard did not refresh the status view")
  assert_true(not notification_contains("Discard failed"), "visual untracked discard executed covered hunks")
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
