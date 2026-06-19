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

local function visual_gutter_mark(status_buf, row)
  local marks = vim.api.nvim_buf_get_extmarks(
    status_buf,
    diff_review._diff_gutter_visual_ns,
    { row - 1, 0 },
    { row - 1, -1 },
    { details = true }
  )
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.virt_text and details.virt_text_pos == "win_col" then return details end
  end
  return nil
end

local function has_local_visual_clipboard_yank()
  local mapping = vim.fn.maparg("<Space>l", "x", false, true)
  return mapping and mapping.buffer == 1 and type(mapping.callback) == "function"
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
  assert_true(before_line:match("^%s*$") ~= nil, "empty diff row should contain only visual padding, got: " .. vim.inspect(before_line))
  local content_lengths = diff_review._diff_line_content_lengths and diff_review._diff_line_content_lengths[status_buf] or nil
  assert_true(content_lengths and content_lengths[empty_row] == 0, "empty diff row should keep real content length 0")

  vim.api.nvim_win_set_cursor(0, { empty_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = status_buf })
  local cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == empty_row, "cursor moved to wrong row: " .. vim.inspect(cursor))
  assert_true(cursor[3] == 1, "empty diff row should stay at buffer column 1: " .. vim.inspect(cursor))
  assert_true(cursor[4] > 0, "empty diff row did not get a virtual gutter offset: " .. vim.inspect(cursor))

  local after_line = vim.api.nvim_buf_get_lines(status_buf, empty_row - 1, empty_row, false)[1]
  assert_true(after_line == before_line, "cursor correction mutated empty diff row: " .. vim.inspect(after_line))

  local code_row, diff_line = find_non_empty_diff_row(status_buf)
  assert_true(code_row ~= nil and diff_line ~= nil, "expanded diff did not render a non-empty diff row")
  local code_line = vim.api.nvim_buf_get_lines(status_buf, code_row - 1, code_row, false)[1]
  local code_start = code_line:find(diff_line.code, 1, true)
  assert_true(code_start ~= nil, "diff code text missing from status row: " .. vim.inspect({ code_line, diff_line.code }))
  local code_bounds = diff_review._diff_gutter_cursor_bounds(status_buf, code_row, diff_review._status_ns)
  assert_true(code_bounds ~= nil, "non-empty diff row is missing inline gutter bounds")
  assert_true(code_bounds.content_length == #diff_line.code, "non-empty diff row should track real code length: " .. vim.inspect(code_bounds))

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

  local visual_gutter_mapping = vim.fn.maparg("W", "n", false, true)
  assert_true(type(visual_gutter_mapping.callback) == "function", "missing visual line gutter mapping")
  assert_true(not has_local_visual_clipboard_yank(), "visual clipboard yank mapping should not be installed before W selection")
  vim.api.nvim_win_set_cursor(0, { code_row, 0 })
  visual_gutter_mapping.callback()
  assert_true(has_local_visual_clipboard_yank(), "visual clipboard yank mapping was not installed for W selection")
  cursor = vim.fn.getcurpos()
  assert_true(diff_review._is_visual_mode(), "W did not enter visual mode: " .. vim.inspect(vim.api.nvim_get_mode()))
  assert_true(cursor[2] == code_row, "W moved off non-empty diff row: " .. vim.inspect(cursor))
  assert_true(cursor[3] == 1, "W should keep the cursor at the real gutter edge: " .. vim.inspect(cursor))
  assert_true(cursor[4] == 0, "W should not use a virtual gutter offset: " .. vim.inspect(cursor))
  local selected_gutter = visual_gutter_mark(status_buf, code_row)
  assert_true(selected_gutter ~= nil, "W did not render a Visual gutter overlay")
  assert_true(selected_gutter.virt_text_win_col == code_bounds.gutter_col, "Visual gutter overlay started in the wrong column: " .. vim.inspect(selected_gutter))
  assert_true(#selected_gutter.virt_text > 0, "Visual gutter overlay had no chunks: " .. vim.inspect(selected_gutter))
  for _, chunk in ipairs(selected_gutter.virt_text) do
    assert_true(chunk[2] == "Visual", "Visual gutter overlay did not select a gutter chunk: " .. vim.inspect(selected_gutter.virt_text))
  end

  diff_review._normalize_status_cursor(status_buf)
  cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == code_row, "visual gutter normalization moved off row: " .. vim.inspect(cursor))
  assert_true(cursor[3] == 1, "visual gutter normalization clamped into code text: " .. vim.inspect(cursor))
  assert_true(cursor[4] == 0, "visual gutter normalization added virtual gutter offset: " .. vim.inspect(cursor))
  assert_true(visual_gutter_mark(status_buf, code_row) ~= nil, "visual gutter overlay disappeared during active selection")

  local expected_yank = diff_review._diff_gutter_text(code_bounds.virt_text) .. code_line:sub(1, code_bounds.content_length)
  local clipboard_yank_mapping = vim.fn.maparg("<Space>l", "x", false, true)
  assert_true(type(clipboard_yank_mapping.callback) == "function", "missing visual clipboard yank mapping")
  vim.fn.setreg("+", "", "V")
  clipboard_yank_mapping.callback()
  wait_for(function()
    return not diff_review._is_visual_mode()
  end, "visual gutter clipboard yank did not leave visual mode")
  assert_true(vim.fn.getregtype("+") == "V", "W clipboard yank should write a linewise register: " .. vim.inspect(vim.fn.getregtype("+")))
  assert_true(vim.fn.getreg("+") == expected_yank .. "\n", "W clipboard yank did not include gutter text: " .. vim.inspect({ expected = expected_yank, actual = vim.fn.getreg("+") }))
  assert_true(visual_gutter_mark(status_buf, code_row) == nil, "visual gutter overlay was not cleared after clipboard yank")
  assert_true(not has_local_visual_clipboard_yank(), "visual clipboard yank mapping was not removed after clipboard yank")

  vim.api.nvim_win_set_cursor(0, { code_row, 0 })
  visual_gutter_mapping.callback()
  assert_true(has_local_visual_clipboard_yank(), "visual clipboard yank mapping was not installed for second W selection")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  wait_for(function()
    return not diff_review._is_visual_mode()
  end, "visual gutter selection did not exit")
  diff_review._normalize_status_cursor(status_buf)
  assert_true(visual_gutter_mark(status_buf, code_row) == nil, "visual gutter overlay was not cleared after selection")
  assert_true(not has_local_visual_clipboard_yank(), "visual clipboard yank mapping was not removed after selection")
  cursor = vim.fn.getcurpos()
  assert_true(
    cursor[3] == code_bounds.gutter_col + 1 and cursor[4] == code_bounds.gutter_width,
    "normal cursor restriction did not resume after visual gutter selection: " .. vim.inspect({ cursor = cursor, bounds = code_bounds })
  )

  vim.fn.setpos(".", { 0, code_row, #code_line, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = status_buf })
  cursor = vim.fn.getcurpos()
  assert_true(cursor[2] == code_row, "cursor moved off non-empty diff row after EOL clamp: " .. vim.inspect(cursor))
  assert_true(
    cursor[3] == code_bounds.content_length,
    "cursor was not clamped to final real text column: " .. vim.inspect({ cursor = cursor, line = code_line, bounds = code_bounds })
  )
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
