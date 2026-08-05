vim.loader.enable(false)

local diff_review = require("diff_review")
local render_orchestrator = require("diff_review.views.status.render_orchestrator")
local status_render = require("diff_review.views.status.status_render")
local entry_nav = require("diff_review.views.status.entry_nav")
local git_data = require("diff_review.git.git_data")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")
local paths = require("diff_review.infra.paths")
local session = require("diff_review.session")
local syntax_engine = require("diff_review.render.syntax_engine")
local gh = require("diff_review.integrations.gh")
local repo_cache = require("github.repo_cache")
local ui = require("diff_review.infra.ui")
local original_notify = vim.notify

local root = "D:/diffreview-flow-root"
local repo_cache_dir = vim.fn.tempname()
local calls = {}
local deletes = {}
local state = {}
local held_systemlist_async = nil
local held_gh_async = nil
local captured_notifications = {}
local forced_mutation_failure_path = nil
local forced_snapshot_failure_command_count = 0
local gh_calls = 0
local repo_metadata_calls = 0

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, _, cb)
  local key = table.concat(command, "\t")
  if key == "gh\trepo\tview\t--json\tnameWithOwner" then
    repo_metadata_calls = repo_metadata_calls + 1
    vim.defer_fn(function()
      cb({ code = 0, stdout = vim.json.encode({ nameWithOwner = "owner/repo" }), stderr = "", output = "" })
    end, 5)
    return
  end
  if key == "gh\tapi\t/repos/owner/repo/contributors\t--paginate\t--slurp" then
    repo_metadata_calls = repo_metadata_calls + 1
    local stdout = vim.json.encode({
      {
        { login = "alice-dev" },
        { login = "bobtown" },
      },
    })
    vim.defer_fn(function()
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    end, 5)
    return
  end
  if key == "gh\tapi\t/repos/owner/repo/collaborators\t--paginate\t--slurp" then
    repo_metadata_calls = repo_metadata_calls + 1
    local stdout = vim.json.encode({
      {
        { login = "mgeorge-esri" },
      },
    })
    vim.defer_fn(function()
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    end, 5)
    return
  end
  gh_calls = gh_calls + 1
  if held_gh_async then
    held_gh_async[#held_gh_async + 1] = function()
      cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
    end
    return
  end
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(kind, command, input)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

local function reset_calls()
  calls = {}
  deletes = {}
end

local function reset_notifications()
  captured_notifications = {}
end

local function reset_gh_calls()
  gh_calls = 0
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

local function reset_state(next_state)
  assert_true(
    vim.wait(3000, function()
      return not mutation_coordinator.pending(root) and not mutation_coordinator.recovering(root)
    end, 10),
    "previous status mutation did not settle before fixture reset"
  )
  state = {
    modified = next_state.modified or {},
    staged_modified = next_state.staged_modified or {},
    untracked = next_state.untracked or {},
    staged_added = next_state.staged_added or {},
    unstaged_added = next_state.unstaged_added or {},
    staged_deleted = next_state.staged_deleted or {},
    staged_renamed = next_state.staged_renamed or {},
    staged_copied = next_state.staged_copied or {},
    ignored = next_state.ignored or {},
  }
  forced_mutation_failure_path = nil
  forced_snapshot_failure_command_count = 0
  reset_calls()
end

local function numbered_files(prefix, count)
  local files = {}
  for i = 1, count do
    files[("%s-%02d.txt"):format(prefix, i)] = true
  end
  return files
end

local function sorted_keys(map)
  local keys = {}
  for key in pairs(map) do
    keys[#keys + 1] = key
  end
  table.sort(keys)
  return keys
end

local function modified_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1 +1 @@",
    "-old",
    "+new",
  }, "\n")
end

local function added_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "index 0000000..3333333",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1 @@",
    "+new",
  }, "\n")
end

local function joined_diff(files, builder)
  local diffs = {}
  for _, relpath in ipairs(sorted_keys(files)) do
    diffs[#diffs + 1] = builder(relpath)
  end
  return table.concat(diffs, "\n")
end

local function name_status(files, status)
  local lines = {}
  for _, relpath in ipairs(sorted_keys(files)) do
    lines[#lines + 1] = status .. "\t" .. relpath
  end
  return lines
end

local function command_path_set(command)
  for command_index, argument in ipairs(command) do
    if argument == "--" then
      local path_set = {}
      for path_index = command_index + 1, #command do path_set[command[path_index]] = true end
      return path_set
    end
  end
  return nil
end

local function path_selected(path_set, path)
  return path_set == nil or path_set[path] == true
end

local function filtered_file_map(file_map, path_set)
  local filtered = {}
  for path, value in pairs(file_map or {}) do
    if path_selected(path_set, path) then filtered[path] = value end
  end
  return filtered
end

local function porcelain_status(command)
  local path_set = command_path_set(command)
  local status_by_path = {}
  local function set_status(file_map, side, status)
    for path in pairs(file_map or {}) do
      if path_selected(path_set, path) then
        local current = status_by_path[path] or { index = ".", worktree = "." }
        current[side] = status
        status_by_path[path] = current
      end
    end
  end
  set_status(state.modified, "worktree", "M")
  set_status(state.unstaged_added, "worktree", "A")
  set_status(state.staged_modified, "index", "M")
  set_status(state.staged_added, "index", "A")
  set_status(state.staged_deleted, "index", "D")

  local record_list = {}
  for _, path in ipairs(sorted_keys(status_by_path)) do
    local status = status_by_path[path]
    record_list[#record_list + 1] = (
      "1 %s%s N... 100644 100644 100644 1111111 2222222 %s\0"
    ):format(status.index, status.worktree, path)
  end
  for _, path in ipairs(sorted_keys(state.untracked or {})) do
    if path_selected(path_set, path) then record_list[#record_list + 1] = "? " .. path .. "\0" end
  end
  for new_path, old_path in pairs(state.staged_renamed or {}) do
    if path_selected(path_set, new_path) or path_selected(path_set, old_path) then
      record_list[#record_list + 1] = (
        "2 R. N... 100644 100644 100644 1111111 2222222 R100 %s\0%s\0"
      ):format(new_path, old_path)
    end
  end
  for new_path, old_path in pairs(state.staged_copied or {}) do
    if path_selected(path_set, new_path) then
      record_list[#record_list + 1] = (
        "2 C. N... 100644 100644 100644 1111111 2222222 C100 %s\0%s\0"
      ):format(new_path, old_path)
    end
  end
  table.sort(record_list)
  return table.concat(record_list)
end

local function snapshot_diff(command, staged)
  local path_set = command_path_set(command)
  local modified_file_map = filtered_file_map(staged and state.staged_modified or state.modified, path_set)
  local copied_file_map = {}
  if staged then
    for copied_path in pairs(filtered_file_map(state.staged_copied, path_set)) do copied_file_map[copied_path] = true end
  end
  local text = joined_diff(modified_file_map, modified_diff)
  local copied = joined_diff(copied_file_map, added_diff)
  if text ~= "" and copied ~= "" then return text .. "\n" .. copied end
  return text .. copied
end

local function snapshot_added_numstat(command, staged)
  local path_set = command_path_set(command)
  local added_file_map = filtered_file_map(staged and state.staged_added or state.unstaged_added, path_set)
  local record_list = {}
  for _, path in ipairs(sorted_keys(added_file_map)) do
    record_list[#record_list + 1] = "1\t0\t" .. path .. "\0"
  end
  return table.concat(record_list)
end

local function input_relpath(input)
  return tostring(input or ""):match("diff %-%-git a/([^%s]+) b/")
end

local function output_lines(text)
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  record("systemlist", command)
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
    return { "status flow test" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return sorted_keys(state.untracked), 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    local lines = name_status(state.modified, "M")
    vim.list_extend(lines, name_status(state.unstaged_added, "A"))
    table.sort(lines)
    return lines, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    local lines = name_status(state.staged_modified, "M")
    vim.list_extend(lines, name_status(state.staged_added, "A"))
    vim.list_extend(lines, name_status(state.staged_deleted, "D"))
    for new_path, old_path in pairs(state.staged_renamed) do
      lines[#lines + 1] = "R100\t" .. old_path .. "\t" .. new_path
    end
    table.sort(lines)
    return lines, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    local text = joined_diff(state.modified, modified_diff)
    local added = joined_diff(state.unstaged_added, added_diff)
    if text ~= "" and added ~= "" then text = text .. "\n" .. added else text = text .. added end
    return output_lines(text), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    local text = joined_diff(state.staged_modified, modified_diff)
    local added = joined_diff(state.staged_added, added_diff)
    if text ~= "" and added ~= "" then text = text .. "\n" .. added else text = text .. added end
    return output_lines(text), 0
  end

  return {}, 1
end

function backend.systemlist_async(command, cb)
  record("systemlist_async", command)
  if held_systemlist_async then
    held_systemlist_async[#held_systemlist_async + 1] = function()
      local output, code = backend.systemlist(command)
      cb(output, code)
    end
    return
  end
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system(command, input)
  record("system", command, input)
  local key = command_key(command)
  local relpath = command[#command]

  local snapshot_status_command = key:find(
    "git\t--no-optional-locks\t-C\t" .. root .. "\tstatus\t--porcelain=v2\t-z",
    1,
    true
  ) ~= nil
  local snapshot_diff_command = key:find(
    "git\t--no-optional-locks\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff",
    1,
    true
  ) ~= nil
  if (snapshot_status_command or snapshot_diff_command) and forced_snapshot_failure_command_count > 0 then
    forced_snapshot_failure_command_count = forced_snapshot_failure_command_count - 1
    return "forced snapshot failure", 1
  end
  if snapshot_status_command then
    return porcelain_status(command), 0
  end
  if snapshot_diff_command then
    local staged = key:find("\t--cached", 1, true) ~= nil
    if key:find("\t--numstat\t", 1, true) then return snapshot_added_numstat(command, staged), 0 end
    return snapshot_diff(command, staged), 0
  end

  local mutation_path = key:find("\tapply\t", 1, true) and input_relpath(input) or relpath
  if forced_mutation_failure_path == mutation_path
    and (key:find("\tadd\t", 1, true)
      or key:find("\trestore\t--staged\t", 1, true)
      or key:find("\trm\t--cached\t", 1, true)
      or key:find("\tapply\t", 1, true))
  then
    return "forced mutation failure for " .. mutation_path, 1
  end

  if key:find("\tadd\t-u\t--\t", 1, true) then
    if state.modified[relpath] then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
      return "", 0
    end
    return "add -u failed for " .. relpath, 1
  end

  if key:find("\tadd\t--\t", 1, true) then
    if state.ignored[relpath] then
      return "The following paths are ignored by one of your .gitignore files: " .. relpath, 1
    end
    if state.untracked[relpath] then
      state.untracked[relpath] = nil
      state.staged_added[relpath] = true
      return "", 0
    end
    if state.modified[relpath] then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
      return "", 0
    end
    return "add failed for " .. relpath, 1
  end

  if key:find("\trestore\t--staged\t--\t", 1, true) then
    if #command >= 7 and state.staged_renamed[command[#command - 1]] == command[#command] then
      local new_path = command[#command - 1]
      state.staged_renamed[new_path] = nil
      state.untracked[new_path] = true
      state.modified[command[#command]] = true
      return "", 0
    end
    if state.staged_modified[relpath] then
      state.staged_modified[relpath] = nil
      state.modified[relpath] = true
      return "", 0
    end
    if state.unstaged_added[relpath] then
      state.unstaged_added[relpath] = nil
      state.untracked[relpath] = true
      return "", 0
    end
    if state.staged_deleted[relpath] then
      state.staged_deleted[relpath] = nil
      state.modified[relpath] = true
      return "", 0
    end
    if state.staged_added[relpath] then
      return "error: pathspec '" .. relpath .. "' did not match any file(s) known to git", 1
    end
    return "restore --staged failed for " .. relpath, 1
  end

  if key:find("\trm\t--cached\t--ignore-unmatch\t--\t", 1, true) then
    if state.staged_added[relpath] then
      state.staged_added[relpath] = nil
      state.untracked[relpath] = true
    end
    if state.staged_copied[relpath] then
      state.staged_copied[relpath] = nil
      state.untracked[relpath] = true
    end
    return "", 0
  end

  if key:find("\tcheckout\tHEAD\t--\t", 1, true) then
    if state.modified[relpath] then
      state.modified[relpath] = nil
      return "", 0
    end
    if state.staged_modified[relpath] then
      state.staged_modified[relpath] = nil
      return "", 0
    end
    return "checkout failed for " .. relpath, 1
  end

  if key:find("\tcheckout\t--\t", 1, true) then
    if state.modified[relpath] then
      state.modified[relpath] = nil
      return "", 0
    end
    if relpath ~= command[#command] then
      return "unexpected checkout relpath mismatch", 1
    end
    return "worktree checkout failed for " .. relpath, 1
  end

  if key:find("\tapply\t", 1, true) then
    relpath = input_relpath(input)
    if not relpath then return "missing patch file header", 1 end
    local cached = key:find("\t--cached", 1, true) ~= nil
    local reverse = key:find("\t--reverse", 1, true) ~= nil
    local index = key:find("\t--index", 1, true) ~= nil
    if cached and reverse then
      state.staged_modified[relpath] = nil
      state.modified[relpath] = true
    elseif cached then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
    elseif reverse and index then
      state.staged_modified[relpath] = nil
    elseif reverse then
      state.modified[relpath] = nil
    end
    return "", 0
  end

  return "unexpected command: " .. key, 1
end

function backend.system_async(command, input, cb)
  record("system_async", command, input)
  vim.defer_fn(function()
    local output, code = backend.system(command, input)
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 10)
end

function backend.delete(path)
  deletes[#deletes + 1] = path
  local relpath = path:gsub("\\", "/"):gsub("^" .. vim.pesc(root) .. "/", "")
  state.untracked[relpath] = nil
  return 0
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function plain_winbar()
  return (vim.wo.winbar or ""):gsub("%%#[^#]+#", ""):gsub("%%%*", ""):gsub("%%=", " "):gsub("%%%%", "%%")
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function row_is_folded(buf, row)
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "buffer window missing for fold check")
  local previous_win = vim.api.nvim_get_current_win()
  vim.api.nvim_set_current_win(win)
  local folded = vim.fn.foldclosed(row) ~= -1
  if vim.api.nvim_win_is_valid(previous_win) then
    pcall(vim.api.nvim_set_current_win, previous_win)
  end
  return folded
end

local function row_has_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, ui.status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group then return true end
  end
  return false
end

local function count_lines_containing(buf, needle)
  local count = 0
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then count = count + 1 end
  end
  return count
end

local function find_row(buf, needle)
  for index, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row " .. needle .. "\n" .. table.concat(status_lines(buf), "\n"), 2)
end

local function find_row_after(buf, needle, start_row)
  local lines = status_lines(buf)
  for index = start_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. start_row .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function find_hunk_row_after_file(buf, file)
  local lines = status_lines(buf)
  local file_row = find_row(buf, file)
  for index = file_row + 1, #lines do
    if lines[index]:find("@@", 1, true) then return index end
    if lines[index]:find("%.txt ", 1) or lines[index]:find("%.rs ", 1) then break end
  end
  error("missing hunk row after " .. file .. "\n" .. table.concat(lines, "\n"), 2)
end

local function cursor_is_on_hunk_after_file(buf, file)
  local ok_hunk, hunk_row = pcall(find_hunk_row_after_file, buf, file)
  if not ok_hunk then return false end
  return vim.api.nvim_win_get_cursor(0)[1] == hunk_row
end

local function cursor_line_text(buf)
  return status_lines(buf)[vim.api.nvim_win_get_cursor(0)[1]] or ""
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function trigger_visual_mapping(key, first_row, second_row)
  vim.api.nvim_win_set_cursor(0, { second_row, 0 })
  vim.fn.setpos("'<", { 0, first_row, 1, 0 })
  vim.fn.setpos("'>", { 0, second_row, 1, 0 })
  local mapping = vim.fn.maparg(key, "x", false, true)
  assert_true(type(mapping.callback) == "function", "missing visual mapping for " .. key)
  mapping.callback()
end

local function mode_is_visual(mode)
  return mode == "v" or mode == "V" or mode:byte() == 22
end

local function assert_visual_callback_exits_mode(key, first_row, second_row)
  local original_get_mode = vim.api.nvim_get_mode
  local original_feedkeys = vim.api.nvim_feedkeys
  local exit_count = 0
  vim.api.nvim_get_mode = function()
    return { mode = "V", blocking = false }
  end
  vim.api.nvim_feedkeys = function()
    exit_count = exit_count + 1
  end
  local ok, err = pcall(function()
    trigger_visual_mapping(key, first_row, second_row)
  end)
  vim.api.nvim_get_mode = original_get_mode
  vim.api.nvim_feedkeys = original_feedkeys
  assert_true(ok, tostring(err))
  assert_true(exit_count == 1, ("visual %s did not request visual-mode exit"):format(key))
end

local function confirm_yes()
  local mapping = vim.fn.maparg("y", "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing confirm yes mapping")
  mapping.callback()
end

local function saw_system_call(expected_key)
  for _, call in ipairs(calls) do
    if call.kind == "system" and call.key == expected_key then return true end
  end
  return false
end

local function saw_systemlist_call(expected_key)
  for _, call in ipairs(calls) do
    if call.kind == "systemlist_async" and call.key == expected_key then return true end
  end
  return false
end

local function saw_system_call_containing(needle)
  for _, call in ipairs(calls) do
    if call.kind == "system" and call.key:find(needle, 1, true) then return true end
  end
  return false
end

local function count_calls(kind, needle)
  local count = 0
  for _, call in ipairs(calls) do
    if call.kind == kind and (not needle or call.key:find(needle, 1, true)) then
      count = count + 1
    end
  end
  return count
end

local function count_calls_with_input(kind, needle)
  local count = 0
  for _, call in ipairs(calls) do
    if call.kind == kind and tostring(call.input or ""):find(needle, 1, true) then count = count + 1 end
  end
  return count
end

local function count_snapshot_diff_calls()
  return count_calls("system_async", "\tdiff")
end

local function calls_text()
  local lines = {}
  for _, call in ipairs(calls) do
    lines[#lines + 1] = ("%s: %s"):format(call.kind, call.key)
  end
  return table.concat(lines, "\n")
end

local function hold_systemlist_async()
  held_systemlist_async = {}
end

local function release_systemlist_async()
  local callbacks = held_systemlist_async or {}
  held_systemlist_async = nil
  for _, callback in ipairs(callbacks) do
    vim.defer_fn(callback, 0)
  end
end

local function hold_gh_async()
  held_gh_async = {}
end

local function release_gh_async()
  local callbacks = held_gh_async or {}
  held_gh_async = nil
  for _, callback in ipairs(callbacks) do
    vim.defer_fn(callback, 0)
  end
end

local function render_and_wait(buf, needle)
  render_orchestrator.render_status(buf)
  wait_for(function() return buffer_contains(buf, needle) end, "status did not render " .. needle)
end

local function open_compact_preview_and_wait(opts, expected)
  local previous_buf = vim.api.nvim_get_current_buf()
  diff_review.open_compact_preview(opts)
  wait_for(function()
    local buf = vim.api.nvim_get_current_buf()
    return buf ~= previous_buf and buffer_contains(buf, expected)
  end, "compact preview did not open with " .. expected)
  return vim.api.nvim_get_current_buf()
end

local function assert_path_helpers()
  local relpath, err = paths.repo_relative_for_test("D:\\Repo\\App\\src\\main.rs", "d:/repo/app", true)
  assert_true(relpath == "src/main.rs", "windows drive/backslash path failed: " .. tostring(relpath or err))

  relpath, err = paths.repo_relative_for_test("D:/Repo/App/src/main.rs", "D:/Repo/App", true)
  assert_true(relpath == "src/main.rs", "windows slash path failed: " .. tostring(relpath or err))

  relpath, err = paths.repo_relative_for_test("/home/matt/project/src/lib.lua", "/home/matt/project", false)
  assert_true(relpath == "src/lib.lua", "linux path failed: " .. tostring(relpath or err))

  relpath, err = paths.repo_relative_for_test("/Users/matt/Project/src/init.lua", "/Users/matt/Project", false)
  assert_true(relpath == "src/init.lua", "macos path failed: " .. tostring(relpath or err))

  relpath = paths.repo_relative_for_test("/Users/matt/Project/src/init.lua", "/Users/matt/project", false)
  assert_true(relpath == nil, "case-sensitive unix-style path should reject mismatched root")
end

local function run()
  assert_path_helpers()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  repo_cache.set_data_dir_for_test(repo_cache_dir)
  vim.notify = function(message, level, opts)
    captured_notifications[#captured_notifications + 1] = {
      message = tostring(message),
      level = level,
      opts = opts,
    }
  end
  diff_review.setup({ about_auto_generate = false })
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()

  reset_state({ modified = { ["mod.txt"] = true } })
  render_and_wait(buf, "mod.txt +1 -1")
  wait_for(function() return gh_calls > 0 end, "initial PR lookup did not run")
  wait_for(function()
    return repo_metadata_calls >= 3 and #repo_cache.contributors("owner/repo") == 3
  end, "GitStatus did not load repo contributor metadata")

  local original_debug_log_path = diff_review._gitstatus_debug_log_path
  local original_gitstatus_debug = vim.g.diff_review_gitstatus_debug
  local debug_log_path = vim.fn.tempname()
  vim.fn.delete(debug_log_path)
  diff_review._gitstatus_debug_log_path = function() return debug_log_path end
  diff_review._gitstatus_debug_enabled = nil
  diff_review._gitstatus_debug_force = nil
  vim.g.diff_review_gitstatus_debug = nil
  require("diff_review.views.status.status_debug").dump(buf, "disabled-test")
  vim.wait(350)
  assert_true(vim.fn.filereadable(debug_log_path) == 0, "GitStatus debug dump should be disabled by default")
  diff_review._gitstatus_debug_force = true
  require("diff_review.views.status.status_debug").dump(buf, "enabled-test")
  wait_for(function() return vim.fn.filereadable(debug_log_path) == 1 end, "GitStatus debug dump did not write when enabled")
  diff_review._gitstatus_debug_force = nil
  vim.g.diff_review_gitstatus_debug = original_gitstatus_debug
  diff_review._gitstatus_debug_log_path = original_debug_log_path
  vim.fn.delete(debug_log_path)

  reset_state({ modified = numbered_files("preview-unstaged", 31) })
  reset_calls()
  local preview_buf = open_compact_preview_and_wait({ cwd = root }, "Compact diff: 31 hunks, +31 -31 changed lines")
  assert_true(vim.bo[preview_buf].filetype == "diff", "compact preview buffer should use diff filetype")
  assert_true(vim.bo[preview_buf].buftype == "nofile", "compact preview buffer should be nofile")
  assert_true(vim.b[preview_buf].git_diff_compacted == true, "compact preview should mark compacted output")
  assert_true(vim.b[preview_buf].git_diff_compact_metrics.hunks == 31, "compact preview metrics missing hunk count")
  assert_true(
    saw_systemlist_call("git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0"),
    "compact preview did not read unstaged diff"
  )
  assert_true(buffer_contains(preview_buf, "No hunks have at least 8 changed lines."), "compact preview missing no-large-hunks message")
  assert_true(buffer_contains(preview_buf, "Skipped 31 small hunks (62 changed lines total)"), "compact preview missing skipped summary")
  vim.api.nvim_win_set_buf(0, buf)
  if vim.api.nvim_buf_is_valid(preview_buf) then vim.api.nvim_buf_delete(preview_buf, { force = true }) end

  reset_state({ staged_modified = numbered_files("preview-staged", 31) })
  reset_calls()
  local staged_preview_buf = open_compact_preview_and_wait({ cwd = root, staged = true }, "Compact diff: 31 hunks, +31 -31 changed lines")
  assert_true(
    saw_systemlist_call("git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached"),
    "compact preview did not read staged diff"
  )
  vim.api.nvim_win_set_buf(0, buf)
  if vim.api.nvim_buf_is_valid(staged_preview_buf) then vim.api.nvim_buf_delete(staged_preview_buf, { force = true }) end

  reset_state({ modified = { ["startup-pr-delay.txt"] = true } })
  reset_gh_calls()
  render_orchestrator.render_status(buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(buf, "startup-pr-delay.txt +1 -1") end, "status did not render before PR lookup")
  assert_true(gh_calls == 0, "PR lookup started before initial status render")
  wait_for(function() return gh_calls > 0 end, "deferred PR lookup did not run after status render")

  reset_state({ modified = { ["pr-header-only.txt"] = true } })
  reset_gh_calls()
  hold_gh_async()
  render_orchestrator.render_status(buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(buf, "pr-header-only.txt +1 -1") end, "status did not render before held PR lookup")
  wait_for(function() return gh_calls > 0 end, "held PR lookup did not start")
  local original_render_status = render_orchestrator.render_status
  local pr_completion_rerendered = false
  render_orchestrator.render_status = function(...)
    pr_completion_rerendered = true
    return original_render_status(...)
  end
  release_gh_async()
  wait_for(function() return buffer_contains(buf, "PR:     none") end, "PR completion did not patch header line")
  render_orchestrator.render_status = original_render_status
  assert_true(not pr_completion_rerendered, "PR completion rerendered the full status buffer")

  reset_state({ modified = { ["mock-pr-delay.txt"] = true } })
  reset_gh_calls()
  diff_review.setup({ pr_lookup_mode = "mock-delay", pr_mock_delay_ms = 20, about_auto_generate = false })
  render_orchestrator.render_status(buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(buf, "mock-pr-delay.txt +1 -1") end, "mock PR delay status did not render")
  assert_true(gh_calls == 0, "mock PR delay spawned gh before timer")
  wait_for(function() return buffer_contains(buf, "PR:     none") end, "mock PR delay did not finish as no PR")
  assert_true(gh_calls == 0, "mock PR delay spawned gh")
  diff_review.setup({ about_auto_generate = false })

  reset_state({ modified = { ["mod.txt"] = true } })
  render_and_wait(buf, "mod.txt +1 -1")
  local status_hint = plain_winbar()
  assert_true(
    status_hint:find("GitStatus", 1, true) ~= nil,
    "status hint winbar did not include the buffer name\n" .. status_hint
  )
  assert_true(
    status_hint:find("S stage | U unstage | I ignore | j discard | cc commit | o open | R refresh | q close | ? help", 1, true) ~= nil,
    "status hint winbar did not use the compact binding list\n" .. status_hint
  )
  assert_true(
    status_hint:find("opp push", 1, true) == nil and status_hint:find("opP pull", 1, true) == nil and status_hint:find("ogp pr", 1, true) == nil,
    "status hint winbar included non-compact bindings\n" .. status_hint
  )
  assert_true(not buffer_contains(buf, "Hint:"), "status hint should be a sticky winbar, not buffer text")
  vim.api.nvim_win_set_cursor(0, { vim.api.nvim_buf_line_count(buf), 0 })
  assert_true(plain_winbar() == status_hint, "status hint winbar changed after scrolling")
  syntax_engine.clear_diff_syntax_cache()
  local original_compute_diff_syntax_async = git_data.compute_diff_syntax_async
  local prewarm_count = 0
  git_data.compute_diff_syntax_async = function(_, _, cb)
    prewarm_count = prewarm_count + 1
    cb(nil)
  end
  vim.api.nvim_win_set_cursor(0, { find_row(buf, "mod.txt"), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(prewarm_count == 0, "CursorMoved started diff syntax prewarm synchronously")
  wait_for(function() return prewarm_count > 0 end, "deferred cursor prewarm did not run")
  git_data.compute_diff_syntax_async = original_compute_diff_syntax_async

  local syntax_engine = require("diff_review.render.syntax_engine")
  local original_prewarm = syntax_engine.prewarm_diff_syntax
  local decorate_prewarm_count = 0
  syntax_engine.prewarm_diff_syntax = function(...)
    decorate_prewarm_count = decorate_prewarm_count + 1
    return original_prewarm(...)
  end
  status_render.status_decorate_visible(buf, 1, vim.api.nvim_buf_line_count(buf))
  assert_true(decorate_prewarm_count == 0, "decoration provider prewarmed collapsed file rows")
  trigger_normal_mapping("<Tab>", find_row(buf, "mod.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "visible prewarm test hunk did not render")
  status_render.status_decorate_visible(buf, 1, vim.api.nvim_buf_line_count(buf))
  syntax_engine.prewarm_diff_syntax = original_prewarm
  assert_true(decorate_prewarm_count > 0, "decoration provider did not prewarm visible expanded hunk syntax")

  reset_notifications()
  trigger_normal_mapping("S", find_row(buf, "mod.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tmod.txt")
  end, "tracked stage did not run git add -u")
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "tracked stage did not reconcile")
  assert_true(
    not saw_notification_containing("Staged"),
    "stage action emitted a debug notification with debug_notifications=false"
  )
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "mod.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tmod.txt")
  end, "tracked unstage did not run restore --staged")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (1)") end, "tracked unstage did not reconcile")

  diff_review.setup({ debug_notifications = true, about_auto_generate = false })
  reset_state({ modified = { ["debug-notify.txt"] = true } })
  render_and_wait(buf, "debug-notify.txt +1 -1")
  reset_notifications()
  trigger_normal_mapping("S", find_row(buf, "debug-notify.txt"))
  wait_for(function() return saw_notification_containing("Staged") end, "debug stage notification was not emitted")
  diff_review.setup({ debug_notifications = false, about_auto_generate = false })

  reset_state({
    modified = {
      ["folded-stage-a.txt"] = true,
      ["folded-stage-b.txt"] = true,
      ["folded-stage-c.txt"] = true,
    },
  })
  render_and_wait(buf, "folded-stage-a.txt +1 -1")
  reset_calls()
  local folded_stage_row = find_row(buf, "folded-stage-b.txt")
  trigger_normal_mapping("S", folded_stage_row)
  wait_for(function()
    return state.staged_modified["folded-stage-b.txt"] == true
  end, "folded file stage did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "folded file stage did not reconcile")

  reset_state({
    modified = {
      ["visual-stage-a.txt"] = true,
      ["visual-stage-b.txt"] = true,
      ["visual-stage-c.txt"] = true,
      ["visual-stage-d.txt"] = true,
    },
  })
  render_and_wait(buf, "visual-stage-a.txt +1 -1")
  reset_calls()
  local single_file_stage_row = find_row(buf, "visual-stage-d.txt")
  trigger_normal_mapping("S", single_file_stage_row)
  local visual_first_row = find_row(buf, "visual-stage-a.txt")
  local visual_second_row = find_row(buf, "visual-stage-b.txt")
  assert_visual_callback_exits_mode("S", visual_first_row, visual_second_row)
  assert_true(not mode_is_visual(vim.api.nvim_get_mode().mode), "visual stage left test in visual mode")
  assert_true(
    vim.api.nvim_get_current_line():find("visual-stage-c.txt", 1, true) ~= nil,
    "visual stage did not move the cursor to the next surviving file"
  )
  wait_for(function()
    return state.staged_modified["visual-stage-a.txt"]
      and state.staged_modified["visual-stage-b.txt"]
      and state.staged_modified["visual-stage-d.txt"]
  end, "visual stage queue did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "visual stage queue did not reconcile")
  wait_for(function() return not mutation_coordinator.pending(root) end, "visual stage queue did not settle")

  reset_state({
    modified = {
      ["visual-tail-a.txt"] = true,
      ["visual-tail-b.txt"] = true,
      ["visual-tail-c.txt"] = true,
    },
  })
  render_and_wait(buf, "visual-tail-a.txt +1 -1")
  assert_visual_callback_exits_mode(
    "S",
    find_row(buf, "visual-tail-b.txt"),
    find_row(buf, "visual-tail-c.txt")
  )
  assert_true(
    vim.api.nvim_get_current_line():find("visual-tail-a.txt", 1, true) ~= nil,
    "visual stage at the end did not move the cursor to the previous surviving file"
  )

  reset_state({ modified = { ["hunk-stage.txt"] = true } })
  render_and_wait(buf, "hunk-stage.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "hunk-stage.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "hunk row did not render")
  local original_render_current_model = status_render.status_render_current_model
  local original_restore_cursor = entry_nav._status_restore_cursor
  local optimistic_action_cursor_disabled = false
  local current_model_render_count = 0
  local action_restore_cursor_count = 0
  entry_nav._status_restore_cursor = function(...)
    action_restore_cursor_count = action_restore_cursor_count + 1
    return original_restore_cursor(...)
  end
  status_render.status_render_current_model = function(target_id, opts)
    current_model_render_count = current_model_render_count + 1
    if opts and opts.restore_cursor == false then
      optimistic_action_cursor_disabled = target_id == nil
    end
    return original_render_current_model(target_id, opts)
  end
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "@@ +1 -1"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "hunk stage did not run cached apply")
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "hunk stage did not reconcile")
  wait_for(function() return count_snapshot_diff_calls() > 0 end, "hunk stage did not reload Git state")
  wait_for(function() return not mutation_coordinator.pending(root) end, "hunk stage did not finish synchronization")
  assert_true(action_restore_cursor_count == 0, "hunk stage invoked cursor restoration")
  reset_calls()
  trigger_normal_mapping("<Tab>", find_row(buf, "hunk-stage.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "staged hunk row did not render")
  action_restore_cursor_count = 0
  trigger_normal_mapping("U", find_row(buf, "@@ +1 -1"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--reverse\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "hunk unstage did not run reverse cached apply")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (1)") end, "hunk unstage did not reconcile")
  wait_for(function() return count_snapshot_diff_calls() > 0 end, "hunk unstage did not reload Git state")
  wait_for(function() return not mutation_coordinator.pending(root) end, "hunk unstage did not finish synchronization")
  status_render.status_render_current_model = original_render_current_model
  entry_nav._status_restore_cursor = original_restore_cursor
  assert_true(optimistic_action_cursor_disabled, "stage/unstage optimistic render still controlled the cursor")
  assert_true(action_restore_cursor_count == 0, "hunk unstage invoked cursor restoration")
  assert_true(
    current_model_render_count == 2,
    ("matching stage/unstage sync triggered a redundant status render (%d)"):format(current_model_render_count)
  )

  reset_state({ modified = { ["transient-snapshot.txt"] = true } })
  render_and_wait(buf, "transient-snapshot.txt +1 -1")
  local transient_original_render_current_model = status_render.status_render_current_model
  local transient_render_count = 0
  status_render.status_render_current_model = function(...)
    transient_render_count = transient_render_count + 1
    return transient_original_render_current_model(...)
  end
  forced_snapshot_failure_command_count = 5
  reset_calls()
  reset_notifications()
  trigger_normal_mapping("S", find_row(buf, "transient-snapshot.txt"))
  assert_true(buffer_contains(buf, "Staged changes (1)"), "transient snapshot stage did not project immediately")
  wait_for(function()
    return count_calls("system_async", "git\t--no-optional-locks\t") >= 3
  end, "transient snapshot stage did not run its first authoritative read")
  assert_true(
    not saw_notification_containing("Git status snapshot failed"),
    "transient snapshot failure notified before the bounded retry"
  )
  wait_for(function() return not mutation_coordinator.pending(root) end, "transient snapshot retry did not settle")
  status_render.status_render_current_model = transient_original_render_current_model
  assert_true(state.staged_modified["transient-snapshot.txt"], "transient snapshot retry lost the successful Git write")
  assert_true(buffer_contains(buf, "Staged changes (1)"), "transient snapshot retry changed the matching projection")
  assert_true(transient_render_count == 1, "matching snapshot retry rendered after the optimistic action")
  assert_true(count_snapshot_diff_calls() == 8, "transient snapshot failure did not run exactly one retry")
  assert_true(
    count_calls("system_async", "git\t--no-optional-locks\t") == 10,
    "transient snapshot retry did not use two five-command attempts\n" .. calls_text()
  )
  assert_true(
    not saw_notification_containing("Git status snapshot failed"),
    "successful snapshot retry emitted a synchronization failure"
  )

  reset_state({ modified = { ["fold-highlight.txt"] = true } })
  render_and_wait(buf, "fold-highlight.txt +1 -1")
  local highlighted_file_row = find_row(buf, "fold-highlight.txt")
  assert_true(
    row_has_highlight(buf, highlighted_file_row, "DiffReviewStatusPath"),
    "file row path highlight missing before fold toggle\n" .. table.concat(status_lines(buf), "\n")
  )
  trigger_normal_mapping("<Tab>", highlighted_file_row)
  wait_for(function() return pcall(find_hunk_row_after_file, buf, "fold-highlight.txt") end, "fold-highlight hunk row did not render")
  trigger_normal_mapping("<Tab>", find_row(buf, "fold-highlight.txt"))
  wait_for(function()
    local found, row = pcall(find_hunk_row_after_file, buf, "fold-highlight.txt")
    return found and row_is_folded(buf, row)
  end, "fold-highlight collapse did not retain hunk rows in a native fold\n" .. table.concat(status_lines(buf), "\n"))
  highlighted_file_row = find_row(buf, "fold-highlight.txt")
  assert_true(
    row_is_folded(buf, highlighted_file_row),
    "fold-highlight file row did not close into native foldtext\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    row_has_highlight(buf, highlighted_file_row, "DiffReviewStatusPath"),
    "file row path highlight missing after fold toggle collapse\n" .. table.concat(status_lines(buf), "\n")
  )

  -- A fold re-render must not disturb extmarks on rows it leaves unchanged. A full
  -- nvim_buf_set_lines relocates and collapses every overlapping mark (the render-markdown
  -- conceal flicker on the PR description and comments), so toggling a later file must keep
  -- a mark on an earlier, untouched row byte-for-byte intact.
  reset_state({ modified = { ["preserve-mark-a.txt"] = true, ["preserve-mark-b.txt"] = true } })
  render_and_wait(buf, "preserve-mark-a.txt +1 -1")
  local sentinel_host, fold_target = "preserve-mark-a.txt", "preserve-mark-b.txt"
  if find_row(buf, sentinel_host) > find_row(buf, fold_target) then
    sentinel_host, fold_target = fold_target, sentinel_host
  end
  local sentinel_ns = vim.api.nvim_create_namespace("diff_review_flow_sentinel")
  local sentinel_row = find_row(buf, sentinel_host)
  local sentinel_end_col = #status_lines(buf)[sentinel_row]
  local sentinel_id = vim.api.nvim_buf_set_extmark(buf, sentinel_ns, sentinel_row - 1, 0, {
    end_row = sentinel_row - 1,
    end_col = sentinel_end_col,
    hl_group = "Comment",
  })
  local function assert_sentinel_intact(stage)
    local pos = vim.api.nvim_buf_get_extmark_by_id(buf, sentinel_ns, sentinel_id, { details = true })
    assert_true(#pos > 0, "sentinel extmark lost after " .. stage)
    local details = pos[3] or {}
    assert_true(
      pos[1] == sentinel_row - 1 and details.end_col == sentinel_end_col,
      string.format(
        "fold re-render corrupted an unchanged-row extmark after %s (row %d->%d, end_col %d->%s) -- full set_lines instead of reconcile\n%s",
        stage, sentinel_row - 1, pos[1], sentinel_end_col, tostring(details.end_col),
        table.concat(status_lines(buf), "\n")
      )
    )
  end
  trigger_normal_mapping("<Tab>", find_row(buf, fold_target))
  wait_for(function() return pcall(find_hunk_row_after_file, buf, fold_target) end, "preserve-mark fold target did not expand")
  assert_sentinel_intact("expanding the later file")
  trigger_normal_mapping("<Tab>", find_row(buf, fold_target))
  wait_for(function()
    local found, row = pcall(find_hunk_row_after_file, buf, fold_target)
    return found and row_is_folded(buf, row)
  end, "preserve-mark fold target did not collapse natively")
  assert_sentinel_intact("collapsing the later file")
  vim.api.nvim_buf_clear_namespace(buf, sentinel_ns, 0, -1)

  reset_state({ modified = { ["collapse-parent-a.txt"] = true, ["collapse-parent-b.txt"] = true } })
  render_and_wait(buf, "collapse-parent-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "collapse-parent-a.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "collapse parent hunk row did not render")
  trigger_normal_mapping("N", find_hunk_row_after_file(buf, "collapse-parent-a.txt"))
  assert_true(
    cursor_line_text(buf):find("collapse-parent-a.txt +1 -1", 1, true) ~= nil,
    "Collapse Parent from hunk did not move to file row\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    row_is_folded(buf, find_hunk_row_after_file(buf, "collapse-parent-a.txt")),
    "Collapse Parent from hunk did not keep the file body in a native fold\n" .. table.concat(status_lines(buf), "\n")
  )
  trigger_normal_mapping("N", find_row(buf, "collapse-parent-a.txt"))
  assert_true(
    cursor_line_text(buf):find("Unstaged changes (2)", 1, true) ~= nil,
    "Collapse Parent from file did not move to section row\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    row_is_folded(buf, find_row(buf, "collapse-parent-a.txt +1 -1")),
    "Collapse Parent from file did not fold the section\n" .. table.concat(status_lines(buf), "\n")
  )
  session.status.folds = {}

  reset_state({ modified = { ["refresh-collapse-a.txt"] = true, ["refresh-collapse-b.txt"] = true } })
  render_and_wait(buf, "refresh-collapse-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "refresh-collapse-a.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "refresh collapse hunk row did not render")
  trigger_normal_mapping("R", find_hunk_row_after_file(buf, "refresh-collapse-a.txt"))
  wait_for(function()
    local has_hunk_row, hunk_row = pcall(find_hunk_row_after_file, buf, "refresh-collapse-a.txt")
    return cursor_line_text(buf):find("Unstaged changes (2)", 1, true) ~= nil
      and buffer_contains(buf, "refresh-collapse-a.txt +1 -1")
      and has_hunk_row
      and row_is_folded(buf, hunk_row)
  end, "refresh did not preserve the materialized file in its native fold\n" .. table.concat(status_lines(buf), "\n"))
  session.status.folds = {}

  reset_state({ modified = { ["rapid-stage-a.txt"] = true, ["rapid-stage-b.txt"] = true } })
  render_and_wait(buf, "rapid-stage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-stage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-stage-b.txt"))
  local rapid_original_render_current_model = status_render.status_render_current_model
  local rapid_render_count = 0
  status_render.status_render_current_model = function(...)
    rapid_render_count = rapid_render_count + 1
    return rapid_original_render_current_model(...)
  end
  reset_calls()
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-stage-a.txt"))
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-stage-b.txt"))
  render_orchestrator.render_status(buf)
  assert_true(buffer_contains(buf, "Staged changes (2)"), "rapid hunk stages did not project immediately")
  assert_true(
    not state.staged_modified["rapid-stage-a.txt"] and not state.staged_modified["rapid-stage-b.txt"],
    "rapid optimistic projection waited for Git"
  )
  wait_for(function()
    return state.staged_modified["rapid-stage-a.txt"] == true
      and state.staged_modified["rapid-stage-b.txt"] == true
  end, "rapid hunk stages did not finish")
  vim.wait(20)
  assert_true(
    count_snapshot_diff_calls() == 0,
    "rapid queued hunk stages reconciled before debounce\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "rapid hunk stages did not reconcile after debounce")
  wait_for(function() return not mutation_coordinator.pending(root) end, "rapid hunk stage sync did not finish")
  status_render.status_render_current_model = rapid_original_render_current_model
  assert_true(rapid_render_count == 2, "matching rapid hunk sync rendered more than the two optimistic actions")
  assert_true(count_snapshot_diff_calls() == 4, "rapid hunk stage burst did not use one five-command snapshot")
  assert_true(
    count_calls("system_async", "\t--\trapid-stage-a.txt\trapid-stage-b.txt") == 5,
    "rapid hunk stage snapshot did not union both pathspecs\n" .. calls_text()
  )

  reset_state({ modified = { ["rapid-fail-a.txt"] = true, ["rapid-fail-b.txt"] = true } })
  render_and_wait(buf, "rapid-fail-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-fail-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-fail-b.txt"))
  local recovery_original_render_current_model = status_render.status_render_current_model
  local recovery_original_cancel_pending_enrichment = status_render.status_cancel_pending_enrichment
  local recovery_render_count = 0
  local recovery_cancel_pending_enrichment_count = 0
  status_render.status_render_current_model = function(...)
    recovery_render_count = recovery_render_count + 1
    return recovery_original_render_current_model(...)
  end
  status_render.status_cancel_pending_enrichment = function(...)
    recovery_cancel_pending_enrichment_count = recovery_cancel_pending_enrichment_count + 1
    return recovery_original_cancel_pending_enrichment(...)
  end
  forced_mutation_failure_path = "rapid-fail-a.txt"
  forced_snapshot_failure_command_count = 3
  reset_calls()
  reset_notifications()
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-fail-a.txt"))
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-fail-b.txt"))
  assert_true(buffer_contains(buf, "Staged changes (2)"), "failed rapid burst did not project both actions immediately")
  wait_for(function()
    return saw_notification_containing("Cancelled 1 queued action(s)")
  end, "failed rapid burst did not report the cancelled queued action")
  wait_for(function() return not mutation_coordinator.pending(root) end, "failed rapid burst did not recover")
  status_render.status_render_current_model = recovery_original_render_current_model
  status_render.status_cancel_pending_enrichment = recovery_original_cancel_pending_enrichment
  assert_true(
    state.modified["rapid-fail-a.txt"] and state.modified["rapid-fail-b.txt"],
    "failed rapid burst changed backend state for a failed or cancelled hunk"
  )
  assert_true(
    buffer_contains(buf, "Unstaged changes (2)") and not buffer_contains(buf, "Staged changes"),
    "failed rapid burst did not render authoritative Git state once\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(recovery_render_count == 3, "failed rapid burst did not use two projections and one recovery render")
  assert_true(
    recovery_cancel_pending_enrichment_count == 3,
    "failed rapid burst did not cancel enrichment before each optimistic and recovery render"
  )
  assert_true(count_calls_with_input("system", "rapid-fail-a.txt") == 1, "failed rapid hunk did not execute exactly once")
  assert_true(count_calls_with_input("system", "rapid-fail-b.txt") == 0, "cancelled rapid hunk still reached Git")
  assert_true(count_snapshot_diff_calls() == 8, "failed rapid burst did not retry the transient recovery snapshot once")
  assert_true(
    count_calls("system_async", "\t--\trapid-fail-a.txt\trapid-fail-b.txt") == 10,
    "failed rapid burst recovery did not include every optimistic path\n" .. calls_text()
  )
  assert_true(
    not saw_notification_containing("Git mutation recovery failed"),
    "successful recovery snapshot retry emitted a recovery failure"
  )

  reset_state({ modified = { ["partial-stage-a.txt"] = true, ["partial-stage-b.txt"] = true } })
  render_and_wait(buf, "partial-stage-a.txt +1 -1")
  local partial_original_render_current_model = status_render.status_render_current_model
  local partial_render_count = 0
  status_render.status_render_current_model = function(...)
    partial_render_count = partial_render_count + 1
    return partial_original_render_current_model(...)
  end
  forced_mutation_failure_path = "partial-stage-b.txt"
  reset_calls()
  reset_notifications()
  trigger_normal_mapping("S", find_row(buf, "Unstaged changes (2)"))
  assert_true(buffer_contains(buf, "Staged changes (2)"), "partial stage did not project the batch immediately")
  wait_for(function() return saw_notification_containing("partial-stage-b.txt") end, "partial stage failure was not reported")
  wait_for(function() return not mutation_coordinator.pending(root) end, "partial stage failure did not recover")
  status_render.status_render_current_model = partial_original_render_current_model
  assert_true(
    state.staged_modified["partial-stage-a.txt"] and state.modified["partial-stage-b.txt"],
    "partial stage recovery discarded the successful Git write"
  )
  assert_true(
    buffer_contains(buf, "Staged changes (1)") and buffer_contains(buf, "Unstaged changes (1)"),
    "partial stage recovery did not render mixed authoritative state\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(partial_render_count == 2, "partial stage failure did not use one projection and one recovery render")
  assert_true(count_snapshot_diff_calls() == 4, "partial stage failure did not use one recovery snapshot")

  reset_state({ modified = { ["stale-partial-a.txt"] = true, ["stale-partial-b.txt"] = true } })
  render_and_wait(buf, "stale-partial-a.txt +1 -1")
  local stale_partial_original_render_current_model = status_render.status_render_current_model
  local stale_partial_render_count = 0
  status_render.status_render_current_model = function(...)
    stale_partial_render_count = stale_partial_render_count + 1
    return stale_partial_original_render_current_model(...)
  end
  forced_mutation_failure_path = "stale-partial-b.txt"
  forced_snapshot_failure_command_count = 6
  reset_calls()
  reset_notifications()
  trigger_normal_mapping("S", find_row(buf, "Unstaged changes (2)"))
  wait_for(function()
    return saw_notification_containing("Git mutation recovery failed")
  end, "terminal partial recovery failure was not reported")
  wait_for(function() return not mutation_coordinator.pending(root) end, "terminal partial recovery did not finish")
  status_render.status_render_current_model = stale_partial_original_render_current_model
  assert_true(
    state.staged_modified["stale-partial-a.txt"] and state.modified["stale-partial-b.txt"],
    "terminal partial recovery changed actual Git state"
  )
  assert_true(
    buffer_contains(buf, "Staged changes (1)") and buffer_contains(buf, "Unstaged changes (1)"),
    "terminal partial recovery hid the known successful write\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    vim.deep_equal(session.file_hunk_staged[root .. "/stale-partial-a.txt"], { true }),
    "terminal partial recovery lost the successful cache projection"
  )
  assert_true(
    vim.deep_equal(session.file_hunk_staged[root .. "/stale-partial-b.txt"], { false }),
    "terminal partial recovery retained the failed cache projection"
  )
  assert_true(stale_partial_render_count == 2, "terminal partial recovery did not use one projection and one fallback render")
  assert_true(count_snapshot_diff_calls() == 8, "terminal partial recovery did not stop after two snapshot attempts")

  local pending_context_callbacks = {}
  local original_compute_hunk_context_async = git_data.compute_hunk_context_async
  git_data.compute_hunk_context_async = function(filename, line, cb)
    pending_context_callbacks[#pending_context_callbacks + 1] = {
      filename = filename,
      line = line,
      cb = cb,
    }
  end
  syntax_engine.clear_context_cache()
  reset_state({ modified = { ["context-stage-a.txt"] = true, ["context-stage-b.txt"] = true } })
  render_and_wait(buf, "context-stage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "context-stage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "context-stage-b.txt"))
  wait_for(function() return #pending_context_callbacks > 0 end, "status render did not request hunk context")
  assert_true(
    not pcall(find_hunk_row_after_file, buf, "context-stage-a.txt"),
    "pending hunk context should keep file folded until context is ready\n" .. table.concat(status_lines(buf), "\n")
  )
  for _, request in ipairs(pending_context_callbacks) do
    request.cb("DelayedContext")
  end
  wait_for(function()
    return pcall(find_hunk_row_after_file, buf, "context-stage-a.txt")
      and pcall(find_hunk_row_after_file, buf, "context-stage-b.txt")
  end, "files did not open after delayed hunk context callbacks\n" .. table.concat(status_lines(buf), "\n"))
  reset_calls()
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "context-stage-a.txt"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "context cursor hunk stage did not run cached apply")
  git_data.compute_hunk_context_async = original_compute_hunk_context_async
  syntax_engine.clear_context_cache()

  reset_state({ modified = { ["refresh-cursor-a.txt"] = true, ["refresh-cursor-b.txt"] = true } })
  render_and_wait(buf, "refresh-cursor-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "refresh-cursor-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "refresh-cursor-b.txt"))
  vim.api.nvim_win_set_cursor(0, { find_hunk_row_after_file(buf, "refresh-cursor-b.txt"), 0 })
  render_orchestrator.render_status(buf)
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "refresh-cursor-b.txt")
  end, "untargeted status refresh did not preserve cursor\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["late-cursor-a.txt"] = true, ["late-cursor-b.txt"] = true } })
  render_and_wait(buf, "late-cursor-a.txt +1 -1")
  local late_cursor_a_row = find_row(buf, "late-cursor-a.txt")
  local late_cursor_b_row = find_row(buf, "late-cursor-b.txt")
  vim.api.nvim_win_set_cursor(0, { late_cursor_a_row, 0 })
  hold_systemlist_async()
  render_orchestrator.render_status(buf)
  wait_for(function()
    return held_systemlist_async and #held_systemlist_async > 0
  end, "status refresh did not start held async git calls")
  vim.api.nvim_win_set_cursor(0, { late_cursor_b_row, 0 })
  release_systemlist_async()
  wait_for(function()
    return cursor_line_text(buf):find("late-cursor-b.txt", 1, true) ~= nil
  end, "late async status refresh restored the old cursor instead of the latest cursor\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["header-stage-a.txt"] = true, ["header-stage-b.txt"] = true } })
  render_and_wait(buf, "header-stage-a.txt +1 -1")
  reset_calls()
  local only_unstaged_header_row = find_row(buf, "Unstaged changes (2)")
  trigger_normal_mapping("S", only_unstaged_header_row)
  wait_for(function()
    return state.staged_modified["header-stage-a.txt"] and state.staged_modified["header-stage-b.txt"]
  end, "stage-all from section header did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "stage-all from section header did not reconcile")

  reset_state({
    modified = { ["header-stage-with-untracked-a.txt"] = true, ["header-stage-with-untracked-b.txt"] = true },
    untracked = { ["header-stage-untracked-next.txt"] = true },
  })
  render_and_wait(buf, "header-stage-with-untracked-a.txt +1 -1")
  reset_calls()
  local unstaged_with_untracked_header_row = find_row(buf, "Unstaged changes (3)")
  trigger_normal_mapping("S", unstaged_with_untracked_header_row)
  wait_for(function()
    return state.staged_modified["header-stage-with-untracked-a.txt"]
      and state.staged_modified["header-stage-with-untracked-b.txt"]
      and state.staged_added["header-stage-untracked-next.txt"]
  end, "stage-all from unstaged section with untracked did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "stage-all from unstaged section with untracked did not reconcile")

  reset_state({
    modified = { ["header-untracked-stage-remaining.txt"] = true },
    untracked = { ["header-untracked-stage-a.txt"] = true, ["header-untracked-stage-b.txt"] = true },
  })
  render_and_wait(buf, "header-untracked-stage-a.txt new")
  reset_calls()
  local untracked_with_unstaged_header_row = find_row(buf, "Unstaged changes (3)")
  trigger_normal_mapping("S", untracked_with_unstaged_header_row)
  wait_for(function()
    return state.staged_modified["header-untracked-stage-remaining.txt"]
      and state.staged_added["header-untracked-stage-a.txt"]
      and state.staged_added["header-untracked-stage-b.txt"]
  end, "stage-all from merged unstaged section did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "stage-all from merged unstaged section did not reconcile")

  reset_state({ untracked = { ["header-untracked-stage-only.txt"] = true } })
  render_and_wait(buf, "header-untracked-stage-only.txt new")
  reset_calls()
  local only_untracked_header_row = find_row(buf, "Unstaged changes (1)")
  trigger_normal_mapping("S", only_untracked_header_row)

  reset_state({ modified = { ["codex/config.toml"] = true }, ignored = { ["codex/config.toml"] = true } })
  render_and_wait(buf, "codex")
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "codex"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tcodex/config.toml")
  end, "ignored tracked file did not stage with git add -u")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tadd\t--\tcodex/config.toml"),
    "ignored tracked file used plain git add\n" .. calls_text()
  )
  wait_for(function()
    return state.staged_modified["codex/config.toml"] == true
  end, "ignored tracked file did not stage")

  reset_state({ staged_modified = { ["header-unstage-a.txt"] = true, ["header-unstage-b.txt"] = true } })
  render_and_wait(buf, "header-unstage-a.txt +1 -1")
  reset_calls()
  local only_staged_header_row = find_row(buf, "Staged changes (2)")
  trigger_normal_mapping("U", only_staged_header_row)
  assert_true(vim.wait(3000, function()
    return state.modified["header-unstage-a.txt"] and state.modified["header-unstage-b.txt"]
  end, 10), "unstage-all from section header did not finish\ncalls:\n" .. calls_text())
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "unstage-all from section header did not reconcile")

  reset_state({
    modified = { ["header-section-existing.txt"] = true },
    staged_modified = { ["header-section-a.txt"] = true, ["header-section-b.txt"] = true },
  })
  render_and_wait(buf, "header-section-a.txt +1 -1")
  reset_calls()
  local staged_with_unstaged_header_row = find_row(buf, "Staged changes (2)")
  trigger_normal_mapping("U", staged_with_unstaged_header_row)
  wait_for(function()
    return state.modified["header-section-a.txt"] and state.modified["header-section-b.txt"]
  end, "unstage-all from section header with existing destination did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "unstage-all from section header with existing destination did not reconcile")

  reset_state({ staged_added = { ["header-staged-added.txt"] = true } })
  render_and_wait(buf, "header-staged-added.txt +1 -0")
  reset_calls()
  local staged_added_header_row = find_row(buf, "Staged changes (1)")
  trigger_normal_mapping("U", staged_added_header_row)
  wait_for(function()
    return state.untracked["header-staged-added.txt"] == true
  end, "unstage-all from staged added section did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "unstage-all from staged added section did not reconcile")

  reset_state({ staged_renamed = { ["rename-new.txt"] = "rename-old.txt" } })
  render_and_wait(buf, "rename-new.txt")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "rename-new.txt"))
  wait_for(function()
    return state.untracked["rename-new.txt"] and state.modified["rename-old.txt"]
  end, "renamed-file unstage did not restore both paths")
  wait_for(function() return not mutation_coordinator.pending(root) end, "renamed-file unstage did not synchronize")
  assert_true(
    saw_system_call(
      "git\t-C\t" .. root .. "\trestore\t--staged\t--\trename-new.txt\trename-old.txt"
    ),
    "renamed-file unstage did not pass both pathspecs\n" .. calls_text()
  )
  assert_true(
    buffer_contains(buf, "rename-new.txt") and buffer_contains(buf, "rename-old.txt"),
    "renamed-file unstage did not render the authoritative split paths"
  )

  reset_state({
    staged_modified = { ["copy-source.txt"] = true },
    staged_copied = { ["copy-destination.txt"] = "copy-source.txt" },
  })
  render_and_wait(buf, "copy-destination.txt")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "copy-destination.txt"))
  wait_for(function() return state.untracked["copy-destination.txt"] end, "copied-file unstage did not finish")
  wait_for(function() return not mutation_coordinator.pending(root) end, "copied-file unstage did not synchronize")
  assert_true(state.staged_modified["copy-source.txt"], "copied-file unstage changed the staged source")
  assert_true(
    saw_system_call(
      "git\t-C\t" .. root .. "\trm\t--cached\t--ignore-unmatch\t--\tcopy-destination.txt"
    ),
    "copied-file unstage did not limit the pathspec to the destination\n" .. calls_text()
  )
  assert_true(
    not calls_text():find("copy-destination.txt\tcopy-source.txt", 1, true),
    "copied-file unstage included the source path\n" .. calls_text()
  )

  reset_state({ staged_modified = { ["header-file-unstage.txt"] = true } })
  render_and_wait(buf, "header-file-unstage.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "header-file-unstage.txt +1 -1"))
  wait_for(function()
    return state.modified["header-file-unstage.txt"] == true
  end, "unstage from file header did not finish")
  wait_for(function()
    return count_snapshot_diff_calls() > 0
  end, "unstage from file header did not reconcile")

  reset_state({ modified = { ["merge-file.txt"] = true }, staged_modified = { ["merge-file.txt"] = true } })
  render_and_wait(buf, "merge-file.txt +1 -1")
  trigger_normal_mapping("U", find_row_after(buf, "merge-file.txt", find_row(buf, "merge-file.txt")))
  assert_true(
    count_lines_containing(buf, "merge-file.txt") == 1,
    "optimistic file unstage rendered duplicate file headings\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tmerge-file.txt")
  end, "merge file unstage did not run restore --staged")

  reset_state({ modified = { ["merge-hunk.txt"] = true }, staged_modified = { ["merge-hunk.txt"] = true } })
  render_and_wait(buf, "merge-hunk.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "merge-hunk.txt"))
  local staged_merge_hunk_file_row = find_row_after(buf, "merge-hunk.txt", find_row(buf, "merge-hunk.txt"))
  trigger_normal_mapping("<Tab>", staged_merge_hunk_file_row)
  reset_calls()
  trigger_normal_mapping("U", find_row_after(buf, "@@ +1 -1", staged_merge_hunk_file_row))
  assert_true(
    count_lines_containing(buf, "merge-hunk.txt") == 1,
    "optimistic hunk unstage rendered duplicate file headings\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    count_lines_containing(buf, "@@ +1 -1") == 1,
    "optimistic hunk unstage rendered duplicate identical hunks\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--reverse\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "merge hunk unstage did not run reverse cached apply")

  reset_state({ untracked = { ["new.txt"] = true } })
  render_and_wait(buf, "new.txt new")
  trigger_normal_mapping("S", find_row(buf, "new.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t--\tnew.txt")
  end, "untracked stage did not use plain git add")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tnew.txt"),
    "untracked stage used git add -u"
  )
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "untracked optimistic stage did not render")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "new.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trm\t--cached\t--ignore-unmatch\t--\tnew.txt")
  end, "staged addition unstage did not run rm --cached")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tnew.txt"),
    "staged addition unstage used restore --staged"
  )
  wait_for(function() return buffer_contains(buf, "Unstaged changes (1)") end, "staged addition did not return to unstaged")

  reset_state({ untracked = { ["ignore-u.txt"] = true } })
  render_and_wait(buf, "ignore-u.txt new")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "ignore-u.txt"))
  vim.wait(50)
  assert_true(#calls == 0, "U on untracked file should not run git")

  reset_state({ untracked = { ["delete-untracked.txt"] = true } })
  render_and_wait(buf, "delete-untracked.txt new")
  trigger_normal_mapping("j", find_row(buf, "delete-untracked.txt"))
  confirm_yes()
  wait_for(function() return #deletes == 1 end, "discard untracked did not delete the file")
  wait_for(function() return not buffer_contains(buf, "delete-untracked.txt") end, "discard untracked did not refresh")

  reset_state({ modified = { ["discard-modified.txt"] = true } })
  render_and_wait(buf, "discard-modified.txt +1 -1")
  trigger_normal_mapping("j", find_row(buf, "discard-modified.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tcheckout\t--\tdiscard-modified.txt")
  end, "discard unstaged tracked file did not run worktree-only checkout")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-modified.txt"),
    "discard unstaged tracked file used HEAD checkout"
  )
  wait_for(function() return not buffer_contains(buf, "discard-modified.txt") end, "discard tracked file did not refresh")

  reset_state({ modified = { ["discard-mixed.txt"] = true }, staged_modified = { ["discard-mixed.txt"] = true } })
  render_and_wait(buf, "discard-mixed.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("j", find_row_after(buf, "discard-mixed.txt", find_row(buf, "Unstaged changes")))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tcheckout\t--\tdiscard-mixed.txt")
  end, "discard unstaged file did not run worktree-only checkout")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-mixed.txt"),
    "discard unstaged file reset the index through HEAD checkout"
  )
  wait_for(function()
    return state.modified["discard-mixed.txt"] == nil and state.staged_modified["discard-mixed.txt"] == true
  end, "discard unstaged file did not preserve staged changes")
  wait_for(function()
    return buffer_contains(buf, "Staged changes (1)") and not buffer_contains(buf, "Unstaged changes")
  end, "discard unstaged file did not leave only staged changes")

  reset_state({ unstaged_added = { ["discard-intent-added.txt"] = true } })
  render_and_wait(buf, "discard-intent-added.txt +1 -0")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-intent-added.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-intent-added.txt") and #deletes == 1
  end, "discard unstaged added file did not unstage then delete")
  wait_for(function() return not buffer_contains(buf, "discard-intent-added.txt") end, "discard unstaged added file did not refresh")

  reset_state({ modified = { ["discard-hunk.txt"] = true } })
  render_and_wait(buf, "discard-hunk.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "discard-hunk.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "discard hunk row did not render")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "@@ +1 -1"))
  confirm_yes()
  wait_for(function()
    return saw_system_call_containing("\tapply\t--reverse\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "discard hunk did not run reverse apply")
  wait_for(function() return not buffer_contains(buf, "discard-hunk.txt") end, "discard hunk did not refresh")

  reset_state({ modified = { ["cursor-discard-a.txt"] = true, ["cursor-discard-b.txt"] = true } })
  render_and_wait(buf, "cursor-discard-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-discard-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-discard-b.txt"))
  reset_calls()
  trigger_normal_mapping("j", find_row_after(buf, "@@ +1 -1", find_row(buf, "cursor-discard-a.txt")))
  confirm_yes()
  wait_for(function()
    return saw_system_call_containing("\tapply\t--reverse\t--whitespace=nowarn\t--unidiff-zero\t-")
  end, "cursor hunk discard did not run reverse apply")
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "cursor-discard-b.txt")
  end, "cursor did not move to next hunk after discarding\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["discard-header-a.txt"] = true, ["discard-header-b.txt"] = true } })
  render_and_wait(buf, "discard-header-a.txt +1 -1")
  reset_calls()
  local discard_header_row = find_row(buf, "Unstaged changes (2)")
  trigger_normal_mapping("j", discard_header_row)
  confirm_yes()
  wait_for(function()
    return not buffer_contains(buf, "discard-header-a.txt") and not buffer_contains(buf, "discard-header-b.txt")
  end, "discard from section header did not remove files")
  assert_true(
    vim.api.nvim_win_get_cursor(0)[1] == discard_header_row and cursor_line_text(buf):find("@@", 1, true) == nil,
    "discard from section header moved cursor after refresh\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ modified = { ["discard-file-header-a.txt"] = true, ["discard-file-header-b.txt"] = true } })
  render_and_wait(buf, "discard-file-header-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-file-header-a.txt +1 -1"))
  confirm_yes()
  wait_for(function()
    return not buffer_contains(buf, "discard-file-header-a.txt") and buffer_contains(buf, "discard-file-header-b.txt")
  end, "discard from file header did not remove only that file")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "discard from file header jumped to a hunk after refresh\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ staged_added = { ["discard-added.txt"] = true } })
  render_and_wait(buf, "discard-added.txt +1 -0")
  trigger_normal_mapping("j", find_row(buf, "discard-added.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trm\t--cached\t--ignore-unmatch\t--\tdiscard-added.txt")
      and #deletes == 1
  end, "discard staged addition did not unstage with rm --cached and delete")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-added.txt"),
    "discard staged addition used restore --staged"
  )
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-added.txt"),
    "discard staged addition used HEAD checkout"
  )

  reset_state({ staged_modified = { ["discard-staged-modified.txt"] = true } })
  render_and_wait(buf, "discard-staged-modified.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-staged-modified.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-staged-modified.txt")
      and saw_system_call("git\t-C\t" .. root .. "\tcheckout\t--\tdiscard-staged-modified.txt")
  end, "discard staged modified file did not reset index then checkout worktree")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-staged-modified.txt"),
    "discard staged modified file used HEAD checkout"
  )

  reset_state({ staged_deleted = { ["discard-staged-deleted.txt"] = true } })
  render_and_wait(buf, "discard-staged-deleted.txt +0 -0")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-staged-deleted.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-staged-deleted.txt")
      and saw_system_call("git\t-C\t" .. root .. "\tcheckout\t--\tdiscard-staged-deleted.txt")
  end, "discard staged deleted file did not reset index then restore worktree")

  reset_state({ staged_renamed = { ["discard-staged-new.txt"] = "discard-staged-old.txt" } })
  render_and_wait(buf, "discard-staged-new.txt +0 -0")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-staged-new.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-staged-new.txt\tdiscard-staged-old.txt")
      and saw_system_call("git\t-C\t" .. root .. "\tcheckout\t--\tdiscard-staged-old.txt")
      and #deletes == 1
  end, "discard staged rename did not reset both paths, restore original, and delete new path")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
repo_cache.set_data_dir_for_test(nil)
vim.fn.delete(repo_cache_dir, "rf")
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
