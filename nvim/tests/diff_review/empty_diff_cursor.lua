vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local test_root = vim.fs.normalize(original_cwd .. "/.diffreview-empty-diff-cursor-test")

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

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function find_file_row(filename)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for row, line in ipairs(lines) do
    if line:find(filename, 1, true) then return row end
  end
  error("missing " .. filename .. " status row:\n" .. table.concat(lines, "\n"), 2)
end

local function maybe_file_row(filename)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for row, line in ipairs(lines) do
    if line:find(filename, 1, true) then return row end
  end
  return nil
end

local function find_empty_diff_row(status_buf)
  local status = diff_review._status_states and diff_review._status_states[status_buf] or diff_review._status
  for row, entry in pairs(status and status.entries or {}) do
    if entry.diff_line and entry.diff_line.code == "" then return row end
  end
  return nil
end

local function find_non_empty_diff_row(status_buf)
  local status = diff_review._status_states and diff_review._status_states[status_buf] or diff_review._status
  for row, entry in pairs(status and status.entries or {}) do
    if entry.diff_line and entry.diff_line.code and entry.diff_line.code ~= "" then return row, entry.diff_line end
  end
  return nil, nil
end

local function cleanup()
  pcall(vim.fn.chdir, original_cwd)
  vim.fn.delete(test_root, "rf")
end

local function run()
  vim.fn.delete(test_root, "rf")
  assert_true(vim.fn.mkdir(test_root, "p") == 1, "mkdir failed: " .. test_root)

  local sample = test_root .. "/sample.lua"
  write_lines(sample, {
    "function foo()",
    "",
    "  return 1",
    "end",
  })
  run_git({ "init" })
  run_git({ "config", "user.email", "test@example.com" })
  run_git({ "config", "user.name", "Test User" })
  run_git({ "add", "." })
  run_git({ "commit", "-m", "init" })
  write_lines(sample, {
    "function foo()",
    "",
    "  return 2",
    "end",
  })

  local chdir_ok = pcall(vim.fn.chdir, test_root)
  assert_true(chdir_ok and comparable_path(vim.fn.getcwd()) == comparable_path(test_root), "chdir failed: " .. test_root)

  gh.set_backend(gh_backend)
  diff_review.open()
  assert_true(vim.bo.filetype == "GitStatus", "GitStatus buffer did not open")
  local status_buf = vim.api.nvim_get_current_buf()
  diff_review.render_status(status_buf)
  wait_for(function()
    local row = maybe_file_row("sample.lua")
    return row ~= nil
  end, "DiffReview status did not render sample.lua")

  vim.api.nvim_win_set_cursor(0, { find_file_row("sample.lua"), 0 })
  local toggle_mapping = vim.fn.maparg("<Tab>", "n", false, true)
  assert_true(type(toggle_mapping.callback) == "function", "missing toggle mapping")
  toggle_mapping.callback()

  local empty_row
  wait_for(function()
    empty_row = find_empty_diff_row(status_buf)
    return empty_row ~= nil
  end, "expanded diff did not render an empty diff row")

  local before_line = vim.api.nvim_buf_get_lines(status_buf, empty_row - 1, empty_row, false)[1]
  assert_true(before_line == "", "empty diff row should stay empty buffer text, got: " .. vim.inspect(before_line))

  vim.api.nvim_win_set_cursor(0, { empty_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = status_buf })
  local cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == empty_row, "cursor moved to wrong row: " .. vim.inspect(cursor))
  assert_true(cursor[3] == 1, "empty diff row should stay at buffer column 1: " .. vim.inspect(cursor))
  assert_true(cursor[4] > 0, "empty diff row did not get a virtual gutter offset: " .. vim.inspect(cursor))

  local after_line = vim.api.nvim_buf_get_lines(status_buf, empty_row - 1, empty_row, false)[1]
  assert_true(after_line == "", "cursor correction mutated empty diff row: " .. vim.inspect(after_line))

  local code_row, diff_line = find_non_empty_diff_row(status_buf)
  assert_true(code_row ~= nil and diff_line ~= nil, "expanded diff did not render a non-empty diff row")
  local code_line = vim.api.nvim_buf_get_lines(status_buf, code_row - 1, code_row, false)[1]
  local code_start = code_line:find(diff_line.code, 1, true)
  assert_true(code_start ~= nil, "diff code text missing from status row: " .. vim.inspect({ code_line, diff_line.code }))
  local code_bounds = diff_review._diff_gutter_cursor_bounds(status_buf, code_row, diff_review._status_ns)
  assert_true(code_bounds ~= nil, "non-empty diff row is missing inline gutter bounds")

  vim.api.nvim_win_set_cursor(0, { code_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = status_buf })
  cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == code_row, "cursor moved off non-empty diff row: " .. vim.inspect(cursor))
  assert_true(
    cursor[3] == code_bounds.gutter_col + 1,
    "cursor was not clamped to diff gutter column: " .. vim.inspect({ cursor = cursor, bounds = code_bounds, code_start = code_start })
  )
  assert_true(
    cursor[4] == code_bounds.gutter_width,
    "non-empty diff row did not get a virtual gutter offset: " .. vim.inspect({ cursor = cursor, bounds = code_bounds })
  )

  vim.fn.setpos(".", { 0, code_row, #code_line, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = status_buf })
  cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == code_row, "cursor moved off non-empty diff row after EOL clamp: " .. vim.inspect(cursor))
  assert_true(cursor[3] == #code_line, "cursor was not clamped to final real text column: " .. vim.inspect({ cursor = cursor, line = code_line }))
  assert_true(cursor[4] == 0, "cursor kept virtual columns past the diff row text: " .. vim.inspect(cursor))
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
