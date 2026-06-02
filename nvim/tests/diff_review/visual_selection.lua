vim.loader.enable(false)

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local test_root = vim.fs.normalize(original_cwd .. "/.diffreview-visual-selection-test")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
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
  assert_true(chdir_ok and vim.fs.normalize(vim.fn.getcwd()) == test_root, "chdir failed: " .. test_root)
  local root_output = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })
  assert_true(vim.v.shell_error == 0, "rev-parse failed: " .. table.concat(root_output, "\n"))
  assert_true(vim.fs.normalize(root_output[1]) == test_root, "wrong git root: " .. tostring(root_output[1]))

  vim.cmd("DiffReview")
  assert_true(vim.bo.filetype == "DiffReviewStatus", "DiffReviewStatus buffer did not open")
  require("diff_review").render_status(vim.api.nvim_get_current_buf())

  trigger_visual_mapping("S")
  local staged_after_stage = run_git({ "diff", "--cached", "--name-only" })
  assert_true(contains_line(staged_after_stage, "a.txt"), "a.txt was not staged")
  assert_true(contains_line(staged_after_stage, "b.txt"), "b.txt was not staged")

  trigger_visual_mapping("U")
  local staged_after_unstage = run_git({ "diff", "--cached", "--name-only" })
  assert_true(#staged_after_unstage == 0, "staged diff was not cleared by visual U")
  local unstaged_after_unstage = run_git({ "diff", "--name-only" })
  assert_true(contains_line(unstaged_after_unstage, "a.txt"), "a.txt was not unstaged")
  assert_true(contains_line(unstaged_after_unstage, "b.txt"), "b.txt was not unstaged")

  local discard_mapping = vim.fn.maparg("j", "x", false, true)
  assert_true(type(discard_mapping.callback) == "function", "missing visual mapping for j")
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
