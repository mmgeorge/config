vim.loader.enable(false)

local diff_review = require("diff_review")
local diff_buffer = require("diff_review.views.diff_buffer")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")
local session = require("diff_review.session")
local status_sync = require("diff_review.views.status.status_sync")

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

---@param command string[]
---@param argument string
---@return boolean
local function command_has(command, argument)
  for _, command_argument in ipairs(command) do
    if command_argument == argument then return true end
  end
  return false
end

---@param command string[]
---@return integer?
local function path_separator_index(command)
  for command_index, command_argument in ipairs(command) do
    if command_argument == "--" then return command_index end
  end
  return nil
end

local root = "D:/diffreview-action-cursor"
local relative_path = "example.txt"
local filename = root .. "/" .. relative_path
local object_id = string.rep("1", 40)
local diff_text = table.concat({
  "diff --git a/example.txt b/example.txt",
  "--- a/example.txt",
  "+++ b/example.txt",
  "@@ -1 +1 @@",
  "-before one",
  "+after one",
}, "\n")

local authoritative_staged = false
local mutation_command_list = {}
local snapshot_command_list = {}

---@type DiffReviewGitBackend
local backend = {}

---@param command string[]
---@param input string?
---@param callback fun(result: DiffReviewGitCommandResult)
function backend.system_async(command, input, callback)
  local stdout = ""
  if command_has(command, "apply") then
    mutation_command_list[#mutation_command_list + 1] = {
      command = vim.deepcopy(command),
      input = input,
    }
    authoritative_staged = not command_has(command, "--reverse")
  elseif command_has(command, "status") then
    snapshot_command_list[#snapshot_command_list + 1] = vim.deepcopy(command)
    local xy = authoritative_staged and "M." or ".M"
    stdout = ("1 %s N... 100644 100644 100644 %s %s %s\0"):format(
      xy,
      object_id,
      object_id,
      relative_path
    )
  elseif command_has(command, "diff") then
    snapshot_command_list[#snapshot_command_list + 1] = vim.deepcopy(command)
    local requests_staged = command_has(command, "--cached")
    if not command_has(command, "--numstat") and requests_staged == authoritative_staged then stdout = diff_text end
  else
    error("unexpected Git command: " .. table.concat(command, " "))
  end

  vim.schedule(function()
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)
end

---@param first_snapshot_index integer
local function assert_path_snapshot(first_snapshot_index)
  local status_count = 0
  local unstaged_diff_count = 0
  local staged_diff_count = 0
  local numstat_count = 0
  for snapshot_index = first_snapshot_index, first_snapshot_index + 4 do
    local command = snapshot_command_list[snapshot_index]
    assert_true(command ~= nil, "stage/unstage must collect all five snapshot commands")
    local separator_index = path_separator_index(command)
    assert_true(separator_index ~= nil, "snapshot command must contain a literal pathspec separator")
    assert_equals(command[separator_index + 1], relative_path, "snapshot command must scope Git to the affected path")
    assert_equals(command[separator_index + 2], nil, "snapshot command must contain only the affected path")

    if command_has(command, "status") then
      status_count = status_count + 1
      assert_true(command_has(command, "--porcelain=v2"), "snapshot status must use porcelain v2")
    elseif command_has(command, "--numstat") then
      numstat_count = numstat_count + 1
    elseif command_has(command, "--cached") then
      staged_diff_count = staged_diff_count + 1
    else
      unstaged_diff_count = unstaged_diff_count + 1
    end
  end
  assert_equals(status_count, 1, "snapshot must issue one status command")
  assert_equals(unstaged_diff_count, 1, "snapshot must issue one unstaged diff command")
  assert_equals(staged_diff_count, 1, "snapshot must issue one staged diff command")
  assert_equals(numstat_count, 2, "snapshot must issue two added-file numstat commands")
end

---@param mapping table
---@param expected_staged boolean
---@param action_label string
local function run_cursor_action(mapping, expected_staged, action_label)
  local mutation_count_before = #mutation_command_list
  local snapshot_count_before = #snapshot_command_list
  local cursor_line_before = vim.api.nvim_win_get_cursor(0)[1]
  local cursor_move_count = 0
  local original_set_cursor = vim.api.nvim_win_set_cursor
  vim.api.nvim_win_set_cursor = function(...)
    cursor_move_count = cursor_move_count + 1
    return original_set_cursor(...)
  end

  local action_ok, action_error = xpcall(function()
    mapping.callback()
    assert_true(vim.wait(3000, function()
      return #snapshot_command_list == snapshot_count_before + 5 and not mutation_coordinator.pending(root)
    end, 10), action_label .. " synchronization timed out")
  end, debug.traceback)
  vim.api.nvim_win_set_cursor = original_set_cursor
  if not action_ok then error(action_error, 2) end

  assert_equals(cursor_move_count, 0, action_label .. " must not call cursor movement APIs")
  assert_equals(vim.api.nvim_win_get_cursor(0)[1], cursor_line_before, action_label .. " must leave the cursor in place")
  assert_equals(#mutation_command_list, mutation_count_before + 1, action_label .. " must issue one index mutation")
  assert_equals(#snapshot_command_list, snapshot_count_before + 5, action_label .. " must settle with exactly five Git reads")
  assert_path_snapshot(snapshot_count_before + 1)

  local staged_flag_list = session.file_hunk_staged[filename]
  assert_equals(staged_flag_list and staged_flag_list[1] or false, expected_staged, action_label .. " must retain authoritative cache state")
end

local function run()
  mutation_coordinator.reset_for_test()
  status_sync.reset_for_test()
  diff_review.set_git_backend(backend)

  session.states = {}
  session.main_status = nil
  session.status = { cwd = root }
  session.file_diffs = { [filename] = diff_text }
  session.file_hunk_staged = { [filename] = { false } }
  session.untracked = {}
  session.buf_last_rendered = {}
  session.diff_line_content_lengths = {}
  session.empty_diff_rows = {}

  status_sync.configure_root(root)
  mutation_coordinator.set_quiet_delay_for_test(root, 0)

  local buf = diff_buffer.open_diff_buffer(filename)
  vim.api.nvim_win_set_buf(0, buf)
  diff_buffer._refresh_diff_buffer(buf, filename)
  vim.wait(50)

  local first_hunk_line = nil
  for line_number, line_text in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line_text:match("^@@") then
      first_hunk_line = line_number
      break
    end
  end
  assert_true(first_hunk_line ~= nil, "rendered diff should contain a hunk header")

  local stage_mapping = vim.fn.maparg("S", "n", false, true)
  local unstage_mapping = vim.fn.maparg("U", "n", false, true)
  assert_true(type(stage_mapping.callback) == "function", "diff buffer must install the stage mapping")
  assert_true(type(unstage_mapping.callback) == "function", "diff buffer must install the unstage mapping")

  vim.api.nvim_win_set_cursor(0, { first_hunk_line, 0 })
  run_cursor_action(stage_mapping, true, "staging from a diff buffer")
  assert_true(authoritative_staged, "stage mutation must update the fake index")
  assert_true(not command_has(mutation_command_list[1].command, "--reverse"), "stage mutation must apply the patch forward")

  vim.api.nvim_win_set_cursor(0, { first_hunk_line, 0 })
  run_cursor_action(unstage_mapping, false, "unstaging from a diff buffer")
  assert_true(not authoritative_staged, "unstage mutation must update the fake index")
  assert_true(command_has(mutation_command_list[2].command, "--reverse"), "unstage mutation must reverse the patch")
end

local run_ok, run_error = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
mutation_coordinator.reset_for_test()
status_sync.reset_for_test()
if not run_ok then error(run_error) end

vim.cmd("qa!")
