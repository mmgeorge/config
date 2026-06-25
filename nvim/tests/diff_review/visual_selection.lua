vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.integrations.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local test_root = vim.fs.normalize(original_cwd .. "/.diffreview-visual-selection-test")

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

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function find_file_rows()
  local buffer_lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local first_row = nil
  local second_row = nil
  for line_number, line in ipairs(buffer_lines) do
    if line:find("a.txt", 1, true) then
      first_row = line_number
    elseif line:find("b.txt", 1, true) then
      second_row = line_number
    end
  end
  assert_true(first_row ~= nil, "missing a.txt status row:\n" .. table.concat(buffer_lines, "\n"))
  assert_true(second_row ~= nil, "missing b.txt status row:\n" .. table.concat(buffer_lines, "\n"))
  return first_row, second_row
end

local function trigger_visual_mapping(key)
  local first_row, second_row = find_file_rows()
  vim.fn.setpos("'<", { 0, first_row, 1, 0 })
  vim.fn.setpos("'>", { 0, second_row, 1, 0 })
  local mapping = vim.fn.maparg(key, "x", false, true)
  assert_true(type(mapping.callback) == "function", "missing visual mapping for " .. key)
  mapping.callback()
end

local function cleanup()
  pcall(vim.fn.chdir, original_cwd)
  vim.fn.delete(test_root, "rf")
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
  diff_review.open()
  assert_true(vim.bo.filetype == "GitStatus", "GitStatus buffer did not open")
  local status_buf = vim.api.nvim_get_current_buf()
  diff_review.render_status(status_buf)
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
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
