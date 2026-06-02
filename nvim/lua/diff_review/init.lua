--- DiffReview standalone git review UI.
--- Usage: :DiffReview

---@alias DiffReviewGitCommand string[]

---@class DiffReviewGitCommandResult
---@field ok boolean
---@field code integer
---@field output string
---@field stdout? string
---@field stderr? string
---@field root? string
---@field args DiffReviewGitCommand

---@class DiffReviewGitAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias DiffReviewGitTextCallback fun(result: DiffReviewGitAsyncResult)
---@alias DiffReviewGitListCallback fun(output: string[], code: integer, stderr?: string)

---@class DiffReviewGitFailure
---@field file? string
---@field path? string
---@field message? string
---@field output? string
---@field stderr? string
---@field stdout? string
---@field code? integer

---@class DiffReviewGitBackend
---@field systemlist? fun(command: DiffReviewGitCommand): string[]|string, integer?
---@field system? fun(command: DiffReviewGitCommand, input?: string): string, integer?
---@field systemlist_async? fun(command: DiffReviewGitCommand, cb: DiffReviewGitListCallback)
---@field system_async? fun(command: DiffReviewGitCommand, input: string?, cb: DiffReviewGitTextCallback)
---@field delete? fun(path: string): integer

---@class DiffReviewHunk
---@field file string
---@field filename? string
---@field section_name? string
---@field pos integer
---@field context? string
---@field context_text? string
---@field diff string
---@field staged boolean
---@field added integer
---@field removed integer
---@field git_status? string

---@class DiffReviewStatusFile
---@field filename string
---@field relpath string
---@field section_name string
---@field added integer
---@field removed integer
---@field hunks DiffReviewHunk[]
---@field untracked boolean
---@field status string
---@field git_status? string

---@class DiffReviewStatusSection
---@field name string
---@field title string
---@field default_folded boolean
---@field files DiffReviewStatusFile[]
---@field files_by_name table<string, DiffReviewStatusFile>

---@class DiffReviewStatusEntry
---@field id? string
---@field kind "section"|"file"|"hunk"
---@field section? DiffReviewStatusSection
---@field file? DiffReviewStatusFile
---@field hunk? DiffReviewHunk
---@field diff_line? table

---@alias DiffReviewStatusSectionName "unstaged"|"staged"|"untracked"

---@class DiffReviewTreeSitterContextPending
---@field pending true
---@field callbacks table<string, fun(context?: DiffReviewHunkTreeSitterContext|string)>

---@class DiffReviewHunkTreeSitterContext
---@field label string
---@field start_row integer 0-based row
---@field end_row integer 0-based row
---@field start_text string
---@field end_text string
---@field start_segments DiffReviewHighlightSegment[]
---@field end_segments DiffReviewHighlightSegment[]

---@class DiffReviewHighlightSegment
---@field text string
---@field hl_group? string

---@class DiffReviewModule
---@field config DiffReviewConfig?
---@field _git_backend DiffReviewGitBackend?
---@field _status table?
---@field _untracked table<string, string>?
---@field _file_diffs table<string, string>?
---@field _file_hunk_staged table<string, boolean[]>?
---@field _diff_bufs table<string, integer>?
---@field _buf_hunks table<integer, DiffReviewHunk[]>?
---@field _buf_filename table<integer, string>?
---@field _buf_saved_cursor table<integer, integer[]>?
---@field _buf_last_rendered table<integer, string>?
---@field _ts_context_cache table<string, DiffReviewHunkTreeSitterContext|string|false|DiffReviewTreeSitterContextPending>?
---@field _ts_source_bufs table<string, integer>?
---@field _main_win integer?
---@field _saved_wo table?
---@field suspend_preview boolean?

---@type DiffReviewModule
local M = {}

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

local config = require("diff_review.config")
local highlights = require("diff_review.highlights")
local notifications = require("diff_review.notifications")

local function setup_bg_highlights()
  highlights.setup()
end

setup_bg_highlights()

--- Compute added/removed line counts from diff text
---@param diff_text string
---@return number added, number removed
local function count_stats(diff_text)
  local added, removed = 0, 0
  for line in diff_text:gmatch("[^\n]+") do
    local first = line:sub(1, 1)
    if first == "+" and not line:find("^%+%+%+") then
      added = added + 1
    elseif first == "-" and not line:find("^%-%-%-") then
      removed = removed + 1
    end
  end
  return added, removed
end

local detect_filetype

---@param filename string
---@return number?
local function loaded_file_buffer(filename)
  local buf = vim.fn.bufnr(filename)
  if buf == -1 or not vim.api.nvim_buf_is_loaded(buf) then return nil end
  return buf
end

---@param filename string
---@return integer?
local function treesitter_source_buffer(filename)
  local loaded = loaded_file_buffer(filename)
  if loaded then return loaded end

  M._ts_source_bufs = M._ts_source_bufs or {}
  local cached = M._ts_source_bufs[filename]
  if cached and vim.api.nvim_buf_is_valid(cached) then return cached end

  local read_ok, lines = pcall(vim.fn.readfile, filename)
  if not read_ok or type(lines) ~= "table" then return nil end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].filetype = detect_filetype(filename, lines)
  M._ts_source_bufs[filename] = buf
  return buf
end

local function clear_treesitter_source_buffers()
  for _, buf in pairs(M._ts_source_bufs or {}) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
  M._ts_source_bufs = {}
end

---@param buf integer
---@param tree TSTree
---@param query vim.treesitter.Query?
---@param row integer 0-based row
---@param text string
---@return DiffReviewHighlightSegment[]
local function treesitter_line_segments(buf, tree, query, row, text)
  local segments = {}
  if text == "" then return segments end
  if not query then
    return { { text = text } }
  end

  local ranges = {}
  local ok = pcall(function()
    for id, node in query:iter_captures(tree:root(), buf, row, row + 1) do
      local capture = query.captures[id]
      if capture and capture ~= "" then
        local start_row, start_col, end_row, end_col = node:range()
        if start_row <= row and end_row >= row then
          local range_start = start_row == row and start_col or 0
          local range_end = end_row == row and end_col or #text
          if range_end > range_start then
            ranges[#ranges + 1] = {
              start_col = math.max(range_start, 0),
              end_col = math.min(range_end, #text),
              hl_group = "@" .. capture,
            }
          end
        end
      end
    end
  end)
  if not ok or #ranges == 0 then
    return { { text = text } }
  end

  local segment_start = 1
  local current_hl = nil
  for col = 0, #text - 1 do
    local char = text:sub(col + 1, col + 1)
    local hl_group = nil
    if not char:match("%s") then
      for _, range in ipairs(ranges) do
        if col >= range.start_col and col < range.end_col then
          hl_group = range.hl_group
        end
      end
    end
    if col == 0 then
      current_hl = hl_group
    elseif hl_group ~= current_hl then
      segments[#segments + 1] = {
        text = text:sub(segment_start, col),
        hl_group = current_hl,
      }
      segment_start = col + 1
      current_hl = hl_group
    end
  end

  segments[#segments + 1] = {
    text = text:sub(segment_start),
    hl_group = current_hl,
  }
  return segments
end

---@param filename string
---@param contents? string[]
---@return string
function detect_filetype(filename, contents)
  local args = {
    filename = filename,
  }
  local buf = loaded_file_buffer(filename)
  if buf then args.buf = buf end
  if contents then
    args.contents = contents
  end
  return vim.filetype.match(args) or ""
end

---@param message string
---@param title? string
local function notify_error(message, title)
  notifications.error(message, title)
end

---@param backend DiffReviewGitBackend?
function M.set_git_backend(backend)
  M._git_backend = backend
end

function M.reset_git_backend()
  M._git_backend = nil
end

---@param command DiffReviewGitCommand
---@param input? string
---@param cb DiffReviewGitTextCallback
local function system_text_async(command, input, cb)
  local backend = M._git_backend
  if backend and backend.system_async then
    backend.system_async(command, input, cb)
    return
  end
  if backend and backend.system then
    vim.schedule(function()
      local output, code = backend.system(command, input)
      local text = tostring(output or "")
      cb({
        code = code or 0,
        stdout = text,
        stderr = "",
        output = text,
      })
    end)
    return
  end

  local ok, process = pcall(vim.system, command, { text = true, stdin = input }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      cb({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = stdout ~= "" and stdout or stderr,
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process)
      cb({
        code = -1,
        stdout = "",
        stderr = message,
        output = message,
      })
    end)
  end
end

---@param text string
---@return string[]
local function text_to_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text:sub(-1) == "\n" then
    text = text:sub(1, -2)
  end
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end

---@param command DiffReviewGitCommand
---@param cb DiffReviewGitListCallback
local function systemlist_async(command, cb)
  local backend = M._git_backend
  if backend and backend.systemlist_async then
    backend.systemlist_async(command, cb)
    return
  end
  if backend and backend.systemlist then
    vim.schedule(function()
      local output, code = backend.systemlist(command)
      if type(output) == "string" then
        output = text_to_lines(output)
      end
      cb(output or {}, code or 0, "")
    end)
    return
  end

  system_text_async(command, nil, function(result)
    cb(text_to_lines(result.stdout), result.code, result.stderr)
  end)
end

---@param path string
---@return integer code
local function delete_path(path)
  local backend = M._git_backend
  if backend and backend.delete then
    return backend.delete(path)
  end
  return vim.fn.delete(path)
end

---@param cb fun(root?: string, err?: string)
local function git_root_async(cb)
  systemlist_async({ "git", "rev-parse", "--show-toplevel" }, function(output, code)
    local root = output[1]
    if code ~= 0 or not root or root == "" then
      cb(nil, "Not a git repository")
      return
    end
    cb(vim.trim(root), nil)
  end)
end

---@return string? root
---@return string? err
local function git_root_sync_for_test_backend()
  local backend = M._git_backend
  if not (backend and backend.systemlist) then
    return nil, "Synchronous git root is unavailable"
  end
  local output, code = backend.systemlist({ "git", "rev-parse", "--show-toplevel" })
  if type(output) == "string" then output = text_to_lines(output) end
  local root = output and output[1]
  if code ~= 0 or not root or root == "" then
    return nil, "Not a git repository"
  end
  return vim.trim(root), nil
end

---@param path string
---@return string
local function normalize_path(path)
  return tostring(vim.fs.normalize(vim.fn.fnamemodify(path, ":p"))):gsub("\\", "/"):gsub("/+$", "")
end

---@param path string
---@return string
local function normalize_path_text(path)
  return tostring(path or ""):gsub("\\", "/"):gsub("/+$", "")
end

---@param absolute string
---@param root_path string
---@param case_insensitive boolean
---@param original_filename string
---@return string? relpath
---@return string? err
local function repo_relative_normalized(absolute, root_path, case_insensitive, original_filename)
  local compare_absolute = absolute
  local compare_root = root_path
  if case_insensitive then
    compare_absolute = compare_absolute:lower()
    compare_root = compare_root:lower()
  end
  if compare_absolute == compare_root then
    return ".", nil
  end
  local prefix = compare_root .. "/"
  if compare_absolute:sub(1, #prefix) ~= prefix then
    return nil, ("Path is outside the git root: %s"):format(original_filename)
  end
  return absolute:sub(#root_path + 2), nil
end

---@param filename string
---@param root string
---@return string? relpath
---@return string? err
local function repo_relative(filename, root)
  return repo_relative_normalized(
    normalize_path(filename),
    normalize_path(root),
    package.config:sub(1, 1) == "\\",
    filename
  )
end

---@param filename string
---@param root string
---@param case_insensitive boolean?
---@return string? relpath
---@return string? err
function M._repo_relative_for_test(filename, root, case_insensitive)
  return repo_relative_normalized(normalize_path_text(filename), normalize_path_text(root), case_insensitive == true, filename)
end

---@param root string
---@param args string[]
---@param input? string
---@param cb fun(result: DiffReviewGitCommandResult)
local function run_git_at_root_async(root, args, input, cb)
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  system_text_async(command, input, function(result)
    cb({
      ok = result.code == 0,
      code = result.code,
      output = vim.trim(result.output or ""),
      stdout = result.stdout,
      stderr = result.stderr,
      root = root,
      args = args,
    })
  end)
end

---@param args string[]
---@param input? string
---@param cb fun(result: DiffReviewGitCommandResult)
local function run_git_async(args, input, cb)
  git_root_async(function(root, root_err)
    if not root then
      cb({
        ok = false,
        code = -1,
        output = root_err or "Unable to find git root",
        args = args,
      })
      return
    end
    run_git_at_root_async(root, args, input, cb)
  end)
end

---@param args string[]
---@param input? string
---@return DiffReviewGitCommandResult
local function run_git_sync_for_test_backend(args, input)
  local root, root_err = git_root_sync_for_test_backend()
  if not root then
    return {
      ok = false,
      code = -1,
      output = root_err or "Unable to find git root",
      args = args,
    }
  end
  local backend = M._git_backend
  if not (backend and backend.system) then
    return {
      ok = false,
      code = -1,
      output = "Synchronous git is unavailable",
      root = root,
      args = args,
    }
  end
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  local output, code = backend.system(command, input)
  return {
    ok = (code or 0) == 0,
    code = code or 0,
    output = vim.trim(tostring(output or "")),
    root = root,
    args = args,
  }
end

---@param title string
---@param failures DiffReviewGitFailure[]
function M.notify_git_failures(title, failures)
  notifications.git_failures(title, failures)
end

---@param files string[]
---@param args_for_file fun(relpath: string): string[]
---@param title string
---@param cb fun(result: { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] })
local function run_file_batch_async(files, args_for_file, title, cb)
  git_root_async(function(root, root_err)
  if not root then
    local failures = { { message = root_err or "Unable to find git root" } }
    M.notify_git_failures(title, failures)
    cb({ ok = false, successes = {}, failures = failures })
    return
  end

  local successes = {}
  local failures = {}
  local seen = {}
  local tasks = {}

  local function finish_all()
    if #failures > 0 then
      M.notify_git_failures(title, failures)
    end
    cb({ ok = #failures == 0, successes = successes, failures = failures })
  end

  local function run_next(index)
    local task = tasks[index]
    if not task then
      finish_all()
      return
    end
    run_git_at_root_async(root, task.args, nil, function(result)
      if result.ok then
        successes[#successes + 1] = task.filename
      else
        result.file = task.filename
        failures[#failures + 1] = result
      end
      run_next(index + 1)
    end)
  end

  for _, filename in ipairs(files) do
    if filename and filename ~= "" and not seen[filename] then
      seen[filename] = true
      local relpath, rel_err = repo_relative(filename, root)
      if not relpath then
        failures[#failures + 1] = { file = filename, message = rel_err }
      else
        tasks[#tasks + 1] = { filename = filename, args = args_for_file(relpath) }
      end
    end
  end

  run_next(1)
  end)
end

---@param files string[]
---@param args_for_file fun(relpath: string): string[]
---@param title string
---@return { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] }
local function run_file_batch_sync_for_test_backend(files, args_for_file, title)
  local root, root_err = git_root_sync_for_test_backend()
  if not root then
    local failures = { { message = root_err or "Unable to find git root" } }
    M.notify_git_failures(title, failures)
    return { ok = false, successes = {}, failures = failures }
  end

  local successes = {}
  local failures = {}
  local seen = {}
  for _, filename in ipairs(files) do
    if filename and filename ~= "" and not seen[filename] then
      seen[filename] = true
      local relpath, rel_err = repo_relative(filename, root)
      if not relpath then
        failures[#failures + 1] = { file = filename, message = rel_err }
      else
        local result = run_git_sync_for_test_backend(args_for_file(relpath))
        if result.ok then
          successes[#successes + 1] = filename
        else
          result.file = filename
          failures[#failures + 1] = result
        end
      end
    end
  end
  if #failures > 0 then
    M.notify_git_failures(title, failures)
  end
  return { ok = #failures == 0, successes = successes, failures = failures }
end

---@param diff string?
---@param cb fun(ok: boolean)
function M.stage_patch_async(diff, cb)
  if not diff or diff == "" then
    notify_error("No patch to stage")
    cb(false)
    return
  end
  run_git_async({ "apply", "--cached", "--whitespace=nowarn", "-" }, diff .. "\n", function(result)
    if not result.ok then
      notify_error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
      cb(false)
      return
    end
    cb(true)
  end)
end

---@param diff string?
---@param cb fun(ok: boolean)
function M.unstage_patch_async(diff, cb)
  if not diff or diff == "" then
    notify_error("No patch to unstage")
    cb(false)
    return
  end
  run_git_async({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "-" }, diff .. "\n", function(result)
    if not result.ok then
      notify_error("Unstage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
      cb(false)
      return
    end
    cb(true)
  end)
end

---@param diff string?
---@return boolean
function M.stage_patch(diff)
  if not diff or diff == "" then
    notify_error("No patch to stage")
    return false
  end
  local result = run_git_sync_for_test_backend({ "apply", "--cached", "--whitespace=nowarn", "-" }, diff .. "\n")
  if not result.ok then
    notify_error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return false
  end
  return true
end

---@param diff string?
---@return boolean
function M.unstage_patch(diff)
  if not diff or diff == "" then
    notify_error("No patch to unstage")
    return false
  end
  local result = run_git_sync_for_test_backend({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "-" }, diff .. "\n")
  if not result.ok then
    notify_error("Unstage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return false
  end
  return true
end

---@param files string[]
---@param cb fun(result: { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] })
function M.stage_files_async(files, cb)
  run_file_batch_async(files, function(relpath)
    return { "add", "--", relpath }
  end, "Stage failed", cb)
end

---@param files string[]
---@param cb fun(result: { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] })
function M.unstage_files_async(files, cb)
  run_file_batch_async(files, function(relpath)
    return { "restore", "--staged", "--", relpath }
  end, "Unstage failed", cb)
end

---@param files string[]
---@param cb fun(result: { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] })
function M.unstage_added_files_async(files, cb)
  run_file_batch_async(files, function(relpath)
    return { "rm", "--cached", "--ignore-unmatch", "--", relpath }
  end, "Unstage failed", cb)
end

---@param files string[]
---@return { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] }
function M.stage_files(files)
  return run_file_batch_sync_for_test_backend(files, function(relpath)
    return { "add", "--", relpath }
  end, "Stage failed")
end

---@param files string[]
---@return { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] }
function M.unstage_files(files)
  return run_file_batch_sync_for_test_backend(files, function(relpath)
    return { "restore", "--staged", "--", relpath }
  end, "Unstage failed")
end

---@param files string[]
---@return { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] }
function M.unstage_added_files(files)
  return run_file_batch_sync_for_test_backend(files, function(relpath)
    return { "rm", "--cached", "--ignore-unmatch", "--", relpath }
  end, "Unstage failed")
end

---@param line string
---@return string? status
---@return string? file
local function parse_name_status_line(line)
  local parts = vim.split(line, "\t", { plain = true })
  local status = parts[1]
  local file = parts[2]
  if status and (status:sub(1, 1) == "R" or status:sub(1, 1) == "C") then
    file = parts[3] or file
  end
  if status and file then
    return status, file
  end
  status, file = line:match("^(%S+)%s+(.+)$")
  return status, file
end

---@param status string?
---@return boolean
local function git_status_is_added(status)
  return type(status) == "string" and status:sub(1, 1) == "A"
end

--- Parse unified diff output into structured file/hunk data
---@param diff_output string
---@param staged boolean
---@return DiffReviewHunk[] hunks
local function parse_diff(diff_output, staged)
  local hunks = {}
  local lines = vim.split(diff_output, "\n", { plain = true })
  local file_header = {} ---@type string[]
  local current_file = nil
  local current_hunk_lines = nil ---@type string[]?
  local current_hunk_start = nil ---@type number?
  local current_hunk_context = nil ---@type string?

  local function flush_hunk()
    if current_file and current_hunk_lines and current_hunk_start then
      local full_diff = vim.list_extend(vim.deepcopy(file_header), current_hunk_lines)
      local a, r = count_stats(table.concat(current_hunk_lines, "\n"))
      -- Find the first actual changed line (skip leading context lines)
      -- to jump to the change, not the context before it
      local change_offset = 0
      for i = 2, #current_hunk_lines do -- skip the @@ header at index 1
        local first = current_hunk_lines[i]:sub(1, 1)
        if first == "+" or first == "-" then
          break
        end
        change_offset = change_offset + 1
      end
      hunks[#hunks + 1] = {
        file = current_file,
        pos = current_hunk_start + change_offset,
        context = current_hunk_context,
        diff = table.concat(full_diff, "\n"),
        staged = staged,
        added = a,
        removed = r,
      }
    end
    current_hunk_lines = nil
    current_hunk_start = nil
    current_hunk_context = nil
  end

  for _, line in ipairs(lines) do
    if line:find("^diff ") then
      flush_hunk()
      file_header = { line }
      current_file = nil
    elseif line:find("^%-%-%- ") or line:find("^%+%+%+ ") or line:find("^index ")
        or line:find("^new file") or line:find("^deleted file")
        or line:find("^rename ") or line:find("^similarity")
        or line:find("^old mode") or line:find("^new mode") then
      file_header[#file_header + 1] = line
      if line:find("^%+%+%+ b/") then
        current_file = line:sub(7) -- strip "+++ b/"
      elseif line:find("^%+%+%+ /dev/null") and current_file == nil then
        -- deleted file: get name from --- a/
        -- will be set from the --- line
      elseif line:find("^%-%-%- a/") and current_file == nil then
        current_file = line:sub(7) -- strip "--- a/"
      end
    elseif line:find("^@@") then
      flush_hunk()
      -- Parse @@ -old,count +new,count @@ context
      local new_start, context = line:match("^@@ %-%d+,?%d* %+(%d+),?%d* @@ ?(.*)")
      if not new_start then
        new_start = line:match("^@@ %-%d+,?%d* %+(%d+),?%d* @@")
      end
      current_hunk_start = tonumber(new_start) or 1
      current_hunk_context = (context and context ~= "") and context or nil
      current_hunk_lines = { line }
    elseif current_hunk_lines then
      current_hunk_lines[#current_hunk_lines + 1] = line
    end
  end
  flush_hunk()

  return hunks
end

--- Run git diff and return parsed hunks
---@param cwd string
---@param staged boolean
---@param cb fun(hunks: DiffReviewHunk[])
local function get_hunks_async(cwd, staged, cb)
  local args = { "git", "-C", cwd, "-c", "core.quotepath=false",
    "diff", "--no-color", "--no-ext-diff" }
  if staged then
    args[#args + 1] = "--cached"
  end
  systemlist_async(args, function(result, code)
    if code ~= 0 then
      cb({})
      return
    end
    cb(parse_diff(table.concat(result, "\n"), staged))
  end)
end

--- Order a single file's hunks by line position and split out the diff
--- patches and their matching staged flags. Keeping line order means a hunk
--- stays put when staged/unstaged — it only folds, never jumps to the end of
--- the file (unstaged and staged hunks would otherwise group separately).
---@param hunks DiffReviewHunk[] hunks for one file, each with .pos, .staged, .diff
---@return string[] diffs, boolean[] staged_flags
local function order_file_hunks(hunks)
  table.sort(hunks, function(a, b)
    if a.pos ~= b.pos then
      return a.pos < b.pos
    end
    -- Same position: show the unstaged hunk before the staged one
    return not a.staged and b.staged
  end)
  local diffs, flags = {}, {}
  for _, hunk in ipairs(hunks) do
    if hunk.diff then
      diffs[#diffs + 1] = hunk.diff
      flags[#flags + 1] = hunk.staged
    end
  end
  return diffs, flags
end

--- Fetch one file's hunks (unstaged + staged) from git, ordered by line
--- position. Returns the combined diff text and per-hunk staged flags, or
--- (nil, nil) when the file has no hunks.
---@param cwd string git root
---@param filename string absolute path
---@param cb fun(diff_text?: string, staged_flags?: boolean[])
local function file_diff_and_flags_async(cwd, filename, cb)
  local norm = vim.fs.normalize(filename)
  local hunks = {}
  local pending = 2
  for _, staged in ipairs({ false, true }) do
    get_hunks_async(cwd, staged, function(result)
      for _, hunk in ipairs(result) do
      if vim.fs.normalize(vim.fn.fnamemodify(cwd .. "/" .. hunk.file, ":p")) == norm then
        hunks[#hunks + 1] = hunk
      end
    end
      pending = pending - 1
      if pending > 0 then return end
      local diffs, flags = order_file_hunks(hunks)
      if #diffs == 0 then
        cb(nil, nil)
        return
      end
      cb(table.concat(diffs, "\n"), flags)
    end)
  end
end

--- Build a synthetic "new file" diff (every line an addition) for an untracked
--- file, so it previews as a single hunk straight from disk — no git, which is
--- both faster and the only way to show content git doesn't track yet.
---@param filename string absolute path
---@param relpath string path relative to the git root (forward slashes)
---@return string? diff_text
local function build_untracked_diff(filename, relpath)
  local ok, lines = pcall(vim.fn.readfile, filename)
  if not ok or type(lines) ~= "table" or #lines == 0 then
    return nil
  end
  -- Skip binary files (a NUL byte in any line).
  for _, line in ipairs(lines) do
    if line:find("\0", 1, true) then
      return nil
    end
  end
  local out = {
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1," .. #lines .. " @@",
  }
  for _, line in ipairs(lines) do
    out[#out + 1] = "+" .. line
  end
  return table.concat(out, "\n")
end

---@param buf integer
---@param query vim.treesitter.Query
---@param highlight_query vim.treesitter.Query?
---@param trees TSTree[]
---@param target integer 0-based row
---@return DiffReviewHunkTreeSitterContext?
local function hunk_context_from_trees(buf, query, highlight_query, trees, target)
  if not trees or #trees == 0 then return nil end

  local scopes = {}
  for id, node in query:iter_captures(trees[1]:root(), buf) do
    if query.captures[id] == "scope" then
      local sr, _, er, _ = node:range()
      if sr <= target and er >= target then
        scopes[#scopes + 1] = { node = node, sr = sr, er = er }
      end
    end
  end

  if #scopes == 0 then return nil end

  -- Sort by start row descending (innermost scope first)
  table.sort(scopes, function(a, b) return a.sr > b.sr end)

  -- Limit to 3 levels max
  local max_depth = math.min(#scopes, 3)

  -- Collect names from outermost to innermost
  local names = {}
  for i = max_depth, 1, -1 do
    local scope = scopes[i]
    -- Find @scope.name within this scope's node
    for cid, cnode in query:iter_captures(scope.node, buf) do
      if query.captures[cid] == "scope.name" then
        local name_text = vim.treesitter.get_node_text(cnode, buf)
        if name_text and name_text ~= "" then
          names[#names + 1] = name_text
        end
        break
      end
    end
  end

  if #names == 0 then return nil end
  local selected_scope = scopes[1]
  local lines = vim.api.nvim_buf_get_lines(buf, selected_scope.sr, selected_scope.er + 1, false)
  local start_text = lines[1] or ""
  local end_text = lines[#lines] or start_text
  return {
    label = table.concat(names, "."),
    start_row = selected_scope.sr,
    end_row = selected_scope.er,
    start_text = start_text,
    end_text = end_text,
    start_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.sr, start_text),
    end_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.er, end_text),
  }
end

--- Compute Tree-sitter scope context for a hunk without blocking UI render.
---@param filename string absolute path
---@param line number 1-based line number
---@param cb fun(context?: DiffReviewHunkTreeSitterContext|string)
function M.compute_hunk_context_async(filename, line, cb)
  local buf = treesitter_source_buffer(filename)
  if not buf then
    cb(nil)
    return
  end

  local ft = vim.bo[buf].filetype
  if ft == "" then
    ft = detect_filetype(filename)
  end
  local lang = vim.treesitter.language.get_lang(ft)
  if not lang then
    cb(nil)
    return
  end

  local ok, query = pcall(vim.treesitter.query.get, lang, "diff_context")
  if not ok or not query then
    cb(nil)
    return
  end
  local highlight_ok, highlight_query = pcall(vim.treesitter.query.get, lang, "highlights")
  if not highlight_ok then highlight_query = nil end

  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not parser_ok or not parser then
    cb(nil)
    return
  end

  local target = math.max(line - 1, 0)
  local done = false
  local function finish(trees)
    if done then return end
    done = true
    local context = hunk_context_from_trees(buf, query, highlight_query, trees, target)
    cb(context)
  end

  local parse_ok, parsed = pcall(function()
    return parser:parse({ target, 0, target + 1, 0 }, function(first, second)
      local trees = type(first) == "table" and first or second
      vim.schedule(function()
        finish(trees)
      end)
    end)
  end)
  if not parse_ok then
    cb(nil)
  elseif parsed then
    finish(parsed)
  end
end

--- Return cached Tree-sitter context and kick off an async request if needed.
---@param filename string
---@param line number
---@param callback_key string
---@param on_update? fun(context?: DiffReviewHunkTreeSitterContext|string)
---@return DiffReviewHunkTreeSitterContext|string?
local function cached_hunk_context(filename, line, callback_key, on_update)
  M._ts_context_cache = M._ts_context_cache or {}
  local cache_key = filename .. ":" .. line
  local cached = M._ts_context_cache[cache_key]
  if cached == false then return nil end
  if type(cached) == "string" then return cached end
  if type(cached) == "table" and not cached.pending then return cached end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil
  end

  M._ts_context_cache[cache_key] = {
    pending = true,
    callbacks = on_update and { [callback_key] = on_update } or {},
  }
  M.compute_hunk_context_async(filename, line, function(context)
    local pending = M._ts_context_cache and M._ts_context_cache[cache_key]
    local callbacks = type(pending) == "table" and pending.callbacks or {}
    M._ts_context_cache[cache_key] = context or false
    for _, callback in pairs(callbacks) do
      local ok, err = pcall(callback, context)
      if not ok then notify_error("Tree-sitter context update failed: " .. tostring(err)) end
    end
  end)
  return nil
end

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?
local function hunk_context_label(context)
  if type(context) == "string" then return context end
  if type(context) == "table" then return context.label end
  return nil
end

---@param text string?
---@return string
local function line_indent(text)
  return tostring(text or ""):match("^%s*") or ""
end

---@param diff_lines string|string[]?
---@return table<string, boolean>
local function hunk_visible_source_lines(diff_lines)
  local lines = type(diff_lines) == "table" and diff_lines
    or vim.split(tostring(diff_lines or ""), "\n", { plain = true })
  local visible = {}
  local in_hunk = false
  for _, diff_line in ipairs(lines) do
    if diff_line:match("^@@") then
      in_hunk = true
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == " " or prefix == "+" or prefix == "-" then
        visible[diff_line:sub(2)] = true
      end
    end
  end
  return visible
end

---@param text string
---@param segments? DiffReviewHighlightSegment[]
---@param line_number? integer
---@return table
local function hunk_boundary_row(text, segments, line_number)
  local row = { diff_review_boundary = true }
  local gutter_width = 12
  row[#row + 1] = { string.rep(" ", gutter_width) }
  if line_number then
    local line_text = tostring(line_number)
    row[#row + 1] = {
      virt_text = { { line_text, "DiffReviewContextLineNr" } },
      virt_text_pos = "overlay",
      col = 0,
    }
    row[#row + 1] = {
      virt_text = { { line_text, "DiffReviewContextLineNr" } },
      virt_text_pos = "overlay",
      col = 5,
    }
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { text, "DiffReviewHunkBoundary" }
  end
  return row
end

---@param reference_text string?
---@return table
local function hunk_boundary_ellipsis_row(reference_text)
  return hunk_boundary_row(line_indent(reference_text) .. "...")
end

---@param row table
---@return string
local function highlight_row_text(row)
  local parts = {}
  for _, chunk in ipairs(row) do
    if type(chunk[1]) == "string" then parts[#parts + 1] = chunk[1] end
  end
  return table.concat(parts)
end

-- Cache for treesitter context per file (cleared on refresh)
M._ts_context_cache = {}
M._ts_source_bufs = {}

---@param opts? DiffReviewConfig
function M.setup(opts)
  M.config = config.setup(opts)
  setup_bg_highlights()
end

--- Close and wipe all diff buffers
function M._cleanup_diff_buffers()
  M._restore_line_numbers(M._main_win)
  M._saved_wo = nil
  M._diff_bufs = M._diff_bufs or {}
  for key, buf in pairs(M._diff_bufs) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
  M._diff_bufs = {}
  M._buf_hunks = {}
  M._buf_filename = {}
  M._buf_saved_cursor = {}
  M._main_win = nil
end

---@param cwd string
---@param cb fun(items: table[])
---@param _ctx table?
local function collect_items_from_git(cwd, cb, _ctx)
  M._ts_context_cache = {} -- clear treesitter context cache on refresh
  clear_treesitter_source_buffers()
  M._untracked = {} -- map of absolute path -> repo-relative path for untracked files
  local results = {
    unstaged = {},
    staged = {},
    untracked_output = {},
    staged_name_status = {},
    unstaged_name_status = {},
    untracked_code = 1,
    staged_name_status_code = 1,
    unstaged_name_status_code = 1,
  }
  local pending = 5

  local function finish_one()
    pending = pending - 1
    if pending > 0 then return end

    local all_hunks = {}
    vim.list_extend(all_hunks, results.unstaged)
    vim.list_extend(all_hunks, results.staged)

    local untracked_files = {}
    if results.untracked_code == 0 then
      for _, filename in ipairs(results.untracked_output) do
        if filename ~= "" then
          untracked_files[#untracked_files + 1] = filename
        end
      end
    end

  -- Get all changed files (staged + unstaged) to catch files with no hunks
  -- (e.g., empty new files, binary files), and to preserve Git status for
  -- textual hunks so action routing can distinguish added files from modified
  -- tracked files.
  local staged_status_by_file = {}
  if results.staged_name_status_code == 0 then
    for _, line in ipairs(results.staged_name_status) do
      local status, file = parse_name_status_line(line)
      if status and file then staged_status_by_file[file] = status end
    end
  end

  local unstaged_status_by_file = {}
  if results.unstaged_name_status_code == 0 then
    for _, line in ipairs(results.unstaged_name_status) do
      local status, file = parse_name_status_line(line)
      if status and file then unstaged_status_by_file[file] = status end
    end
  end

  local tracked_files_with_hunks = {}
  for _, h in ipairs(all_hunks) do
    tracked_files_with_hunks[h.file] = true
    h.git_status = h.staged and staged_status_by_file[h.file] or unstaged_status_by_file[h.file]
  end
  if results.staged_name_status_code == 0 then
    for file, status in pairs(staged_status_by_file) do
      if file and not tracked_files_with_hunks[file] then
        -- File has staged changes but no hunks (empty new file, binary, etc.)
        all_hunks[#all_hunks + 1] = {
          file = file,
          pos = 1,
          context = nil,
          diff = nil,
          staged = true,
          added = 0,
          removed = 0,
          status = status,
          git_status = status,
        }
      end
    end
  end
  if results.unstaged_name_status_code == 0 then
    for file, status in pairs(unstaged_status_by_file) do
      if file and not tracked_files_with_hunks[file] then
        all_hunks[#all_hunks + 1] = {
          file = file,
          pos = 1,
          context = nil,
          diff = nil,
          staged = false,
          added = 0,
          removed = 0,
          status = status,
          git_status = status,
        }
      end
    end
  end

  -- Compute per-file aggregate stats and staging state
  local file_stats = {} ---@type table<string, { added: number, removed: number, total: number, staged: number }>
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    if not file_stats[f] then
      file_stats[f] = { added = 0, removed = 0, total = 0, staged = 0 }
    end
    local fs = file_stats[f]
    fs.added = fs.added + hunk.added
    fs.removed = fs.removed + hunk.removed
    fs.total = fs.total + 1
    if hunk.staged then
      fs.staged = fs.staged + 1
    end
  end

  local items = {}
  for _, hunk in ipairs(all_hunks) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. hunk.file, ":p")
    -- Use treesitter scope context if available, fall back to git's @@ context
    local context_text = hunk.context or ""
    if not (_ctx and _ctx.skip_ts_context) then
      local cached = cached_hunk_context(filename, hunk.pos, "items:" .. filename .. ":" .. hunk.pos)
      local cached_label = hunk_context_label(cached)
      if cached_label then
        context_text = cached_label
      end
    end
    -- Parse range numbers from @@ header
    local full_header = hunk.status or "@@"
    if hunk.diff then
      full_header = hunk.diff:match("\n(@@[^@]+@@)") or hunk.diff:match("^(@@[^@]+@@)") or "@@"
    end
    local old_range = full_header:match("%-(%d+,?%d*)") or ""
    local new_range = full_header:match("%+(%d+,?%d*)") or ""
    local range_text = "-" .. old_range .. " +" .. new_range

    local fs = file_stats[hunk.file]
    local file_check
    if fs.staged == fs.total then
      file_check = "[x]"
    elseif fs.staged > 0 then
      file_check = "[-]"
    else
      file_check = "[ ]"
    end

    items[#items + 1] = ({
      filename = filename,
      pos = { hunk.pos, 0 },
      item = {
        category = "Tracked Changes",
        check = hunk.staged and "[x]" or "[ ]",
        file_check = file_check,
        hunk_header = range_text,
        old_range = "-" .. old_range,
        new_range = "+" .. new_range,
        context_text = context_text,
        staged = hunk.staged,
        diff = hunk.diff,
        added = hunk.added,
        removed = hunk.removed,
        added_pad = hunk.added,
        removed_pad = hunk.removed,
        file_added = fs.added,
        file_removed = fs.removed,
        git_status = hunk.git_status,
      },
    })
  end

  -- Add untracked files
  for _, f in ipairs(untracked_files) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._untracked[filename] = f -- remember repo-relative path for the synthetic diff
    items[#items + 1] = ({
      filename = filename,
      pos = { 1, 0 },
      item = {
        category = "Untracked Files",
        check = "[ ]",
        file_check = "[ ]",
        hunk_header = "new file",
        context_text = "",
        staged = false,
        diff = nil,
        added = 0,
        removed = 0,
        stats = "new",
        git_status = "??",
      },
    })
  end

  -- Build per-file combined diffs + staged status for file-level preview.
  -- order_file_hunks keeps hunks in line order so staging folds a hunk in
  -- place instead of moving it to the end of the file.
  local file_hunks = {} ---@type table<string, DiffReviewHunk[]>
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    file_hunks[f] = file_hunks[f] or {}
    file_hunks[f][#file_hunks[f] + 1] = hunk
  end
  M._file_diffs = {}
  M._file_hunk_staged = {}
  for f, hunks in pairs(file_hunks) do
    local diffs, flags = order_file_hunks(hunks)
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._file_diffs[filename] = table.concat(diffs, "\n")
    M._file_hunk_staged[filename] = flags
  end

  cb(items)

  -- Pre-render all diff buffers so file switching is instant
  if not (_ctx and _ctx.skip_pre_render) then
    vim.schedule(function()
      for filename, diff_text in pairs(M._file_diffs) do
        if diff_text and diff_text ~= "" then
          local buf = M.open_diff_buffer(filename)
          M._refresh_diff_buffer(buf, filename)
        end
      end
    end)
  end
  end

  get_hunks_async(cwd, false, function(hunks)
    results.unstaged = hunks
    finish_one()
  end)
  get_hunks_async(cwd, true, function(hunks)
    results.staged = hunks
    finish_one()
  end)
  systemlist_async({ "git", "-C", cwd, "ls-files", "--others", "--exclude-standard" }, function(output, code)
    results.untracked_output = output
    results.untracked_code = code
    finish_one()
  end)
  systemlist_async({ "git", "-C", cwd, "diff", "--cached", "--name-status" }, function(output, code)
    results.staged_name_status = output
    results.staged_name_status_code = code
    finish_one()
  end)
  systemlist_async({ "git", "-C", cwd, "diff", "--name-status" }, function(output, code)
    results.unstaged_name_status = output
    results.unstaged_name_status_code = code
    finish_one()
  end)
end

---@param cb fun(items: table[])
---@param _ctx table?
function M.get(cb, _ctx)
  git_root_async(function(cwd)
    if not cwd then
      cb({})
      return
    end
    collect_items_from_git(cwd, cb, _ctx)
  end)
end

-- Cache of diff preview items keyed by filename
M._diff_items = {}

-- Namespace for Snacks diff rendering
M._ns = vim.api.nvim_create_namespace("diff_review_preview")

-- Map Snacks diff highlights to bg-only versions so treesitter fg is preserved
-- and the background covers the full line (via the existing add_eol overlays)
local hl_replacements = {
  SnacksDiffAdd = "DiffReviewAddBg",
  SnacksDiffDelete = "DiffReviewDeleteBg",
  SnacksDiffAddLineNr = "DiffReviewAddLineNr",
  SnacksDiffDeleteLineNr = "DiffReviewDeleteLineNr",
  SnacksDiffContext = "DiffReviewContextBg",
  SnacksDiffContextLineNr = "DiffReviewContextLineNr",
}

--- Recursively replace Snacks diff highlights with bg-only versions
--- in a highlight spec (string, table of strings, or nested)
local function replace_hl(h)
  if type(h) == "string" then
    return hl_replacements[h] or h
  elseif type(h) == "table" then
    local new = {}
    for i, v in ipairs(h) do
      new[i] = replace_hl(v)
    end
    return new
  end
  return h
end

--- Walk a line's highlight entries and replace diff hl groups with bg-only versions
local function rewrite_line_hls(line)
  for _, entry in ipairs(line) do
    -- Text entry: { text, hl_group, ... }
    if type(entry[1]) == "string" and entry[2] ~= nil then
      entry[2] = replace_hl(entry[2])
    end
    -- Extmark entry with virt_text: { col=N, virt_text={ {text, hl}, ... } }
    if entry.virt_text then
      for _, vt in ipairs(entry.virt_text) do
        if vt[2] then
          vt[2] = replace_hl(vt[2])
        end
      end
    end
  end
end

---@param diff_text string
---@param hunk_staged? boolean[]
---@param filename? string
---@param context_callback_key? fun(hunk_line: number): string
---@param on_context_update? fun()
---@param opts? { context_line: integer?, boundary_context: boolean? }
local function build_fancy_diff_rows(diff_text, hunk_staged, filename, context_callback_key, on_context_update, opts)
  local snacks_diff = require("snacks.picker.util.diff")
  opts = opts or {}

  local diff = snacks_diff.get_diff(diff_text)
  local snacks_opts = { hunk_header = false }
  local ctx_base = setmetatable({ diff = diff, opts = snacks_opts }, { __index = function(_, k)
    if k == "extend" then
      return function(self2, t)
        return setmetatable(t, { __index = self2 })
      end
    end
  end })

  local ret = {} ---@type snacks.picker.Highlight[][]
  local hunk_idx = 0

  for _, block in ipairs(diff.blocks) do
    local block_ctx = ctx_base:extend({ block = block })
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      -- Checkbox + @@ separator with treesitter context
      local range_line = hunk.diff[1] or "@@"
      local range_only = range_line:match("^(@@[^@]+@@)") or range_line
      -- Get the new-file start line for treesitter context lookup
      local hunk_line = opts.context_line or tonumber(range_line:match("%+(%d+)")) or 1
      local raw_context = nil
      local ts_context = nil
      if filename then
        local callback_key = context_callback_key and context_callback_key(hunk_line)
          or ("diff-row:" .. filename .. ":" .. hunk_line)
        raw_context = cached_hunk_context(filename, hunk_line, callback_key, on_context_update)
        ts_context = hunk_context_label(raw_context)
      end
      -- Parse old/new ranges: @@ -222,6 +226,34 @@
      local old_range = range_line:match("%-(%d+,?%d*)") or ""
      local new_range = range_line:match("%+(%d+,?%d*)") or ""
      -- Count added/removed lines in this hunk
      local h_added, h_removed = 0, 0
      local counting = false
      for _, dl in ipairs(hunk.diff) do
        if counting then
          local p = dl:sub(1, 1)
          if p == "+" then h_added = h_added + 1
          elseif p == "-" then h_removed = h_removed + 1 end
        elseif dl:match("^@@") then
          counting = true
        end
      end
      -- Format: @@ MyClass.method +4 -0
      local header_parts = {
        { "@@ ", "DiffReviewHunkHeader" },
      }
      if ts_context then
        header_parts[#header_parts + 1] = { ts_context, "DiffReviewHunkContext" }
        header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      end
      header_parts[#header_parts + 1] = { ("+%d"):format(h_added), "DiffReviewAddRange" }
      header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      header_parts[#header_parts + 1] = { ("-%d"):format(h_removed), "DiffReviewDeleteRange" }
      ret[#ret + 1] = header_parts

      local visible_hunk_lines = hunk_visible_source_lines(hunk.diff)
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local start_text = raw_context.start_text or ""
        if not visible_hunk_lines[start_text] then
          ret[#ret + 1] = hunk_boundary_row(raw_context.start_text, raw_context.start_segments, node_start)
          if node_start ~= raw_context.end_row + 1 then
            ret[#ret + 1] = hunk_boundary_ellipsis_row(start_text)
          end
        end
      end

      local hunk_ctx = block_ctx:extend({ hunk = hunk })
      local block_file = block.file
      local match = vim.filetype.match
      vim.filetype.match = function(args)
        if type(args) == "table" and args.buf == nil and args.filename == block_file then
          return match(vim.tbl_extend("force", args, {
            filename = filename or block_file,
            buf = loaded_file_buffer(filename or block_file) or 0,
          }))
        end
        return match(args)
      end
      local ok, hunk_lines = pcall(snacks_diff.format_hunk, hunk_ctx)
      vim.filetype.match = match
      if not ok then
        error(hunk_lines)
      end
      if hunk_lines[1] and highlight_row_text(hunk_lines[1]):match("^%s*@@") then
        table.remove(hunk_lines, 1)
      end
      vim.list_extend(ret, hunk_lines)
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local node_end = raw_context.end_row + 1
        local end_text = raw_context.end_text or ""
        if not visible_hunk_lines[end_text] then
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_ellipsis_row(end_text)
          end
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_row(end_text, raw_context.end_segments, node_end)
          end
        end
      end
    end
  end

  -- Replace Snacks diff highlights with bg-only versions in-place
  for _, line in ipairs(ret) do
    rewrite_line_hls(line)
  end

  return ret
end

--- Render a fancy diff into a buffer using Snacks' diff renderer,
--- but skip file name headers and hunk context headers for a cleaner look.
--- Replaces Snacks' diff highlight groups with bg-only versions so that
--- treesitter syntax highlighting is preserved and the full line has a
--- colored background.
--- @param buf number
--- @param diff_text string
--- @param hunk_staged? boolean[] staged status per hunk (in order)
local function render_fancy_diff(buf, diff_text, hunk_staged, filename)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {})
  vim.bo[buf].modifiable = false
  local ft = filename and detect_filetype(filename) or ""
  if ft ~= "" and vim.bo[buf].filetype ~= ft then
    vim.bo[buf].filetype = ft
  end

  local H = Snacks.picker.highlight
  H.render(buf, M._ns, build_fancy_diff_rows(
    diff_text,
    hunk_staged,
    filename,
    function(hunk_line)
      return ("diff-buffer:%d:%s:%d"):format(buf, filename or "", hunk_line)
    end,
    function()
      if not (buf and vim.api.nvim_buf_is_valid(buf) and filename) then return end
      M._buf_last_rendered[buf] = nil
      M._refresh_diff_buffer(buf, filename)
    end
  ))
end

-- Per-buffer hunk metadata: maps buffer line ranges to raw diff patches
M._buf_hunks = {}
-- Per-buffer filename mapping
M._buf_filename = {}
-- Per-buffer saved cursor position (for restoring after jumping to file)
M._buf_saved_cursor = {}

--- Find which hunk the cursor is in within a diff buffer.
--- Returns the hunk's complete diff patch (with file header) or nil.
---@param buf number
---@return string? diff_patch
---@return number? hunk_start_line
local function get_hunk_at_cursor(buf)
  local hunks = M._buf_hunks[buf]
  if not hunks then return end
  local cursor = vim.api.nvim_win_get_cursor(0)[1]
  for _, h in ipairs(hunks) do
    if cursor >= h.start_line and cursor <= h.end_line then
      return h.diff, h.start_line
    end
  end
end

--- Create (or reuse) a real diff buffer with keymaps.
--- Call _refresh_diff_buffer after setting the buffer on a window.
---@param filename string
---@return number buf
function M.open_diff_buffer(filename)
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]

  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    local short = vim.fn.fnamemodify(filename, ":t")
    -- Basenames collide across directories (E95); fall back to a unique name.
    local name = "diff://" .. short
    if not pcall(vim.api.nvim_buf_set_name, buf, name) then
      pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
    end
    M._diff_bufs[key] = buf
    M._buf_filename[buf] = filename

    -- Save cursor + invalidate caches on leaving the diff buffer
    vim.api.nvim_create_autocmd("BufLeave", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) then return end
        -- Defer cache invalidation: only invalidate if we switched to a real
        -- file buffer (not Trouble or another diff buffer). This avoids
        -- re-running git on every s/t movement in the Trouble list.
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          local cur_win_buf = vim.api.nvim_get_current_buf()
          local ft = vim.bo[cur_win_buf].filetype
          local bt = vim.bo[cur_win_buf].buftype
          if ft ~= "trouble" and bt ~= "nofile" then
            M._buf_last_rendered[buf] = nil
            if M._file_diffs then M._file_diffs[filename] = nil end
          end
        end)
        local cur = vim.api.nvim_win_get_cursor(0)
        local hunks = M._buf_hunks[buf]
        if hunks then
          for si, sh in ipairs(hunks) do
            if cur[1] >= sh.start_line and cur[1] <= sh.end_line then
              local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
              M._buf_saved_cursor[buf] = {
                hunk_index = si,
                offset = cur[1] - sh.start_line,
                col = cur[2],
                header = header,
              }
              return
            end
          end
        end
        -- Not in any hunk, save raw position as fallback
        M._buf_saved_cursor[buf] = {
          hunk_index = 1,
          offset = 0,
          col = cur[2],
          header = "",
          raw_line = cur[1],
        }
      end,
    })



    -- Refresh and restore cursor when entering the diff buffer directly
    -- (e.g., via window-switch keybind after editing the real file)
    vim.api.nvim_create_autocmd("BufEnter", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) or not M._buf_filename[buf] then return end
        -- Re-hide the number column: re-entering the diff buffer (e.g. a
        -- window switch back from the real file) restores it otherwise.
        M._hide_line_numbers(vim.api.nvim_get_current_win())
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          -- Re-fetch diff if cache was invalidated
          local function render_and_restore()
            if not vim.api.nvim_buf_is_valid(buf) then return end
            M._refresh_diff_buffer(buf, filename)
            -- Restore saved cursor
            local saved = M._buf_saved_cursor[buf]
            local new_hunks = M._buf_hunks[buf]
            if not (saved and new_hunks) then return end
            local target_hunk = nil
            if saved.header and saved.header ~= "" then
              for _, h in ipairs(new_hunks) do
                local hh = h.diff and h.diff:match("(@@[^@]+@@)") or ""
                if hh == saved.header then
                  target_hunk = h
                  break
                end
              end
            end
            if not target_hunk and saved.hunk_index and saved.hunk_index <= #new_hunks then
              target_hunk = new_hunks[saved.hunk_index]
            end
            if target_hunk then
              local line = target_hunk.start_line + (saved.offset or 0)
              local max = vim.api.nvim_buf_line_count(buf)
              line = math.min(math.max(1, line), max)
              pcall(vim.api.nvim_win_set_cursor, 0, { line, saved.col or 0 })
              pcall(vim.cmd, "normal! zv")
            elseif saved.raw_line then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, 0, { math.min(saved.raw_line, max), saved.col or 0 })
            end
            M._buf_saved_cursor[buf] = nil
          end
          if not M._file_diffs or M._file_diffs[filename] == nil then
            M._update_file_diff_cache_async(filename, render_and_restore)
          else
            render_and_restore()
          end
        end)
      end,
    })

    local kopts = { buffer = buf, silent = true }

    -- Close both diff buffer and trouble
    vim.keymap.set("n", "q", function()
      if vim.api.nvim_buf_is_valid(buf) then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
      M._cleanup_diff_buffers()
    end, vim.tbl_extend("force", kopts, { desc = "Close DiffReview", nowait = true }))


    -- Jump to next/prev hunk header
    vim.keymap.set("n", "]c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, cursor[1], -1, false)
      for i, line in ipairs(lines) do
        if line:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { cursor[1] + i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Next hunk" }))

    vim.keymap.set("n", "[c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, 0, cursor[1] - 1, false)
      for i = #lines, 1, -1 do
        if lines[i]:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Prev hunk" }))

    -- Jump to the corresponding line in the actual file
    vim.keymap.set("n", "<CR>", function()
      local target_file = M._buf_filename[buf]
      if not target_file then return end

      -- Leaving the diff preview for the real file: restore its number column.
      M._restore_line_numbers(vim.api.nvim_get_current_win())

      -- Save cursor as hunk-relative position so we can restore after re-render.
      -- Store: { hunk_index, offset_within_hunk, col, hunk_header }
      local save_cursor = vim.api.nvim_win_get_cursor(0)
      local save_hunks = M._buf_hunks[buf]
      if save_hunks then
        for si, sh in ipairs(save_hunks) do
          if save_cursor[1] >= sh.start_line and save_cursor[1] <= sh.end_line then
            local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
            M._buf_saved_cursor[buf] = {
              hunk_index = si,
              offset = save_cursor[1] - sh.start_line,
              col = save_cursor[2],
              header = header,
            }
            break
          end
        end
      end
      if not M._buf_saved_cursor[buf] or type(M._buf_saved_cursor[buf]) ~= "table" or not M._buf_saved_cursor[buf].hunk_index then
        M._buf_saved_cursor[buf] = { hunk_index = 1, offset = 0, col = 0, header = "" }
      end

      local hunks = M._buf_hunks[buf]
      local cursor = save_cursor[1]
      local raw_col = save_cursor[2] or 0
      -- Adjust column for the Snacks diff gutter.
      -- The gutter (line numbers + prefix) is rendered as virtual text
      -- overlays, but the buffer text has matching spaces as padding.
      -- The padding width = all leading spaces on a code line.
      -- To distinguish gutter padding from code indentation, check a line
      -- that has actual code (non-space after the gutter). The gutter width
      -- is the same for all lines in a hunk, so find it from any code line.
      -- Compute gutter width: the Snacks renderer pads buffer lines with spaces
      -- for the line number + prefix gutter. The gutter width can be computed from
      -- any code line by finding where the code content starts in the buffer text
      -- vs where it starts in the raw diff line (after stripping the +/-/space prefix).
      local gutter_width = 0
      if hunks then
        for _, h in ipairs(hunks) do
          if cursor >= h.start_line and cursor <= h.end_line then
            -- Parse diff lines for this hunk
            local diff_lines_list = {}
            local found_hdr = false
            for dl in h.diff:gmatch("[^\n]+") do
              if found_hdr then
                diff_lines_list[#diff_lines_list + 1] = dl
              elseif dl:match("^@@") then
                found_hdr = true
              end
            end
            -- Find a non-empty code line to measure gutter
            for l = h.start_line + 1, h.end_line do
              local buf_line = vim.api.nvim_buf_get_lines(buf, l - 1, l, false)[1] or ""
              local dl_idx = l - h.start_line
              if dl_idx >= 1 and dl_idx <= #diff_lines_list then
                local diff_line = diff_lines_list[dl_idx]
                local code = diff_line:sub(2) -- strip +/-/space prefix
                -- Find first non-space char in code
                local code_indent = code:find("%S")
                if code_indent then
                  -- Find same char in buffer line
                  local buf_indent = buf_line:find("%S")
                  if buf_indent then
                    -- Gutter = buf_indent - code_indent
                    gutter_width = buf_indent - code_indent
                    break
                  end
                end
              end
            end
            break
          end
        end
      end
      local cursor_col = math.max(0, raw_col - gutter_width)

      -- No hunks (e.g. "No changes" / empty new file): just open the file
      if not hunks or #hunks == 0 then
        vim.cmd.edit(target_file)
        vim.cmd("normal! zz")
        return
      end

      -- Find which hunk we're in and compute file line number
      for _, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          -- Parse the @@ header to get the new-file start line
          local new_start = h.diff:match("%+(%d+)")
          new_start = tonumber(new_start) or 1

          -- Walk diff lines to find file line at cursor position
          -- Cursor offset within the hunk (0 = @@ line, 1 = first code line)
          local offset_in_hunk = cursor - h.start_line
          if offset_in_hunk == 0 then
            -- On the @@ line itself, jump to hunk start
            vim.cmd.edit(target_file)
            pcall(vim.api.nvim_win_set_cursor, 0, { new_start, cursor_col })
            vim.cmd("normal! zz")
            return
          end

          -- Walk through diff code lines counting file lines
          local diff_lines = {}
          local found_header = false
          for diff_line in h.diff:gmatch("[^\n]+") do
            if found_header then
              diff_lines[#diff_lines + 1] = diff_line
            elseif diff_line:match("^@@") then
              found_header = true
            end
          end

          local file_line = new_start
          local last_valid_line = new_start
          for i = 1, math.min(offset_in_hunk, #diff_lines) do
            local prefix = diff_lines[i]:sub(1, 1)
            if prefix == " " or prefix == "+" then
              last_valid_line = file_line
              if i < offset_in_hunk then
                file_line = file_line + 1
              end
            elseif prefix == "-" then
              -- Deleted line: doesn't exist in new file
              -- last_valid_line stays as-is
            end
          end

          vim.cmd.edit(target_file)
          local max_line = vim.api.nvim_buf_line_count(0)
          local target_line = math.min(last_valid_line, max_line)
          pcall(vim.api.nvim_win_set_cursor, 0, { target_line, cursor_col })
          vim.cmd("normal! zz")
          return
        end
      end
      -- Cursor not in any hunk: just open the file
      vim.cmd.edit(target_file)
      vim.cmd("normal! zz")
    end, vim.tbl_extend("force", kopts, { desc = "Jump to file", nowait = true }))

    -- Stage the hunk under cursor, then jump to the next hunk
    vim.keymap.set("n", "S", function()
      local patch, hunk_start = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      -- Find the index of the current hunk so we can jump to the next one
      local cur_hunk_idx = nil
      local hunks = M._buf_hunks[buf]
      if hunks and hunk_start then
        for i, h in ipairs(hunks) do
          if h.start_line == hunk_start then
            cur_hunk_idx = i
            break
          end
        end
      end
      M.stage_patch_async(patch, function(ok)
        if not ok then return end
        vim.notify("Hunk staged", vim.log.levels.INFO)
        local win = vim.api.nvim_get_current_win()
        local filename = M._buf_filename[buf]
        -- Move to the next hunk. The staged hunk keeps its position (it only
        -- folds), so "next" is the following index in the rebuilt hunk map.
        local function goto_next()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = M._buf_hunks[buf]
          if new_hunks and cur_hunk_idx then
            local target = new_hunks[cur_hunk_idx + 1] or new_hunks[cur_hunk_idx]
            if target then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, win, { math.min(target.start_line, max), 0 })
            end
          end
        end
        -- Re-render this file's diff buffer so the staged hunk folds in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        goto_next()
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the next-hunk position once it settles.
        vim.defer_fn(goto_next, 60)
      end)
    end, vim.tbl_extend("force", kopts, { desc = "Stage hunk", nowait = true }))

    -- Unstage the hunk under cursor
    vim.keymap.set("n", "U", function()
      local patch, _ = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      M.unstage_patch_async(patch, function(ok)
        if not ok then return end
        vim.notify("Hunk unstaged", vim.log.levels.INFO)
        local win = vim.api.nvim_get_current_win()
        local filename = M._buf_filename[buf]
        -- Find the current hunk so we can stay on it after the re-render
        -- (it expands in place; unstaging must not jump to the buffer top).
        local cur_idx, cur_offset = nil, 0
        local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
        local hunks = M._buf_hunks[buf]
        if hunks then
          for i, h in ipairs(hunks) do
            if cursor_line >= h.start_line and cursor_line <= h.end_line then
              cur_idx = i
              cur_offset = cursor_line - h.start_line
              break
            end
          end
        end
        local function stay()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = M._buf_hunks[buf]
          if new_hunks and cur_idx and new_hunks[cur_idx] then
            local max = vim.api.nvim_buf_line_count(buf)
            local line = math.min(new_hunks[cur_idx].start_line + cur_offset, max)
            pcall(vim.api.nvim_win_set_cursor, win, { line, 0 })
          end
        end
        -- Re-render this file's diff buffer so the hunk expands in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        stay()
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the current-hunk position once it settles.
        vim.defer_fn(stay, 60)
      end)
    end, vim.tbl_extend("force", kopts, { desc = "Unstage hunk", nowait = true }))

    -- Toggle fold (collapse/expand) the hunk under cursor
    vim.keymap.set("n", "<Tab>", function()
      local hunks = M._buf_hunks[buf]
      if not hunks then
        vim.notify("No hunk map for buffer", vim.log.levels.WARN)
        return
      end
      local cursor = vim.api.nvim_win_get_cursor(0)[1]
      local found = false
      for i, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          found = true
          h.folded = not h.folded
          vim.notify("Hunk " .. i .. " folded=" .. tostring(h.folded) .. " range=" .. h.start_line .. "-" .. h.end_line, vim.log.levels.INFO)
          M._render_with_folds(buf)
          pcall(vim.api.nvim_win_set_cursor, 0, { h.start_line, 0 })
          return
        end
      end
      if not found then
        local ranges = {}
        for i, h in ipairs(hunks) do
          ranges[#ranges + 1] = i .. ":[" .. h.start_line .. "-" .. h.end_line .. "]"
        end
        vim.notify("Cursor " .. cursor .. " not in any hunk: " .. table.concat(ranges, ", "), vim.log.levels.WARN)
      end
    end, vim.tbl_extend("force", kopts, { desc = "Toggle hunk fold", nowait = true }))
  end

  return buf
end

--- Compute hunk map from diff text. Each hunk's rendered lines are:
--- 1 line for @@ separator + N code lines (from format_hunk).
--- N = number of lines in hunk.diff AFTER the @@ header, EXCLUDING
--- the file header lines (diff --git, index, ---, +++).
---@param diff_text string
---@return table[]
function M._compute_hunk_map(diff_text)
  local raw_hunks = parse_diff(diff_text, false)
  local rendered_line = 0
  local hunk_map = {}
  for _, h in ipairs(raw_hunks) do
    -- Count code lines the same way Snacks does: lines after @@,
    -- stripping trailing empty/whitespace lines (Snacks parse_hunk does this)
    local code_lines_list = {}
    local found_hunk_header = false
    for diff_line in h.diff:gmatch("[^\n]+") do
      if found_hunk_header then
        code_lines_list[#code_lines_list + 1] = diff_line
      elseif diff_line:match("^@@") then
        found_hunk_header = true
      end
    end
    -- Strip trailing empty lines (matching Snacks parse_hunk behavior)
    while #code_lines_list > 0 and code_lines_list[#code_lines_list]:match("^%s*$") do
      table.remove(code_lines_list)
    end
    local code_lines = #code_lines_list
    -- Rendered: 1 line (@@ separator) + code_lines
    -- end_line is the LAST line of this hunk (exclusive of next hunk's @@)
    local start_line = rendered_line + 1
    local end_line = start_line + code_lines
    hunk_map[#hunk_map + 1] = {
      start_line = start_line,
      end_line = end_line,
      diff = h.diff,
      folded = false,
    }
    rendered_line = end_line
  end
  return hunk_map
end

--- Re-render buffer respecting fold state (placeholder — full fold
--- support would need tracking which hunks to hide/show)
function M._render_with_folds(buf)
  local hunks = M._buf_hunks[buf]
  if not hunks then return end

  -- Find the window showing this buffer
  local win = nil
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(w) and vim.api.nvim_win_get_buf(w) == buf then
      win = w
      break
    end
  end
  if not win then return end

  local line_count = vim.api.nvim_buf_line_count(buf)
  -- Ensure fold settings are on the correct window
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = true
  vim.api.nvim_win_call(win, function()
    -- Save view to prevent jumping
    local view = vim.fn.winsaveview()
    pcall(vim.cmd, "normal! zE") -- delete all folds
    for _, h in ipairs(hunks) do
      if h.folded then
        local fold_start = h.start_line + 1
        local fold_end = math.min(h.end_line, line_count)
        if fold_end >= fold_start and fold_start <= line_count then
          pcall(vim.cmd, fold_start .. "," .. fold_end .. "fold")
        end
      end
    end
    vim.fn.winrestview(view)
  end)
end

--- Re-fetch diff data and re-render a diff buffer after staging/unstaging.
--- Staged hunks get auto-folded.
---@param buf number
---@param filename string
-- Track what was last rendered per buffer to avoid re-rendering same data
M._buf_last_rendered = {}

function M._refresh_diff_buffer(buf, filename)
  -- Use cached diff data from M.get() instead of re-running git
  local diff_text = M._file_diffs and M._file_diffs[filename]
  local staged_flags = M._file_hunk_staged and M._file_hunk_staged[filename]

  if diff_text and diff_text ~= "" then
    -- Skip re-render if already rendered with the same data, but still
    -- (re)apply folds: the initial pre-render happens off-screen, where
    -- _render_with_folds is a no-op (no window shows the buffer yet), so the
    -- staged-hunk folds must be applied once the buffer becomes visible.
    if M._buf_last_rendered[buf] == diff_text and M._buf_hunks[buf] then
      M._render_with_folds(buf)
      return
    end
    M._buf_last_rendered[buf] = diff_text

    render_fancy_diff(buf, diff_text, staged_flags, filename)
    local hunk_map = M._compute_hunk_map(diff_text)
    -- Auto-fold staged hunks
    if staged_flags then
      for i, h in ipairs(hunk_map) do
        if staged_flags[i] then
          h.folded = true
        end
      end
    end
    M._buf_hunks[buf] = hunk_map
    -- Highlight @@ header lines with subtle gray background
    vim.api.nvim_buf_clear_namespace(buf, M._hunk_header_ns, 0, -1)
    for _, h in ipairs(hunk_map) do
      pcall(vim.api.nvim_buf_set_extmark, buf, M._hunk_header_ns, h.start_line - 1, 0, {
        line_hl_group = "SnacksDiffHunkHeader",
        priority = M._hunk_header_priority,
      })
    end
    M._render_with_folds(buf)
  else
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "No changes" })
    vim.bo[buf].modifiable = false
    M._buf_hunks[buf] = {}
    vim.api.nvim_buf_clear_namespace(buf, M._hunk_header_ns, 0, -1)
    vim.api.nvim_buf_clear_namespace(buf, M._active_hunk_header_ns, 0, -1)
  end
end

---@param buf number
---@param item_diff string?
---@return table?
function M._highlight_active_hunk(buf, item_diff)
  vim.api.nvim_buf_clear_namespace(buf, M._active_hunk_header_ns, 0, -1)
  if not item_diff then return nil end

  local hunks = M._buf_hunks[buf]
  if not hunks then return nil end

  for _, hunk in ipairs(hunks) do
    if hunk.diff == item_diff then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._active_hunk_header_ns, hunk.start_line - 1, 0, {
        line_hl_group = "DiffReviewActiveHunkHeader",
        priority = M._active_hunk_header_priority,
      })
      return hunk
    end
  end
end

--- Re-fetch the diff for a single file and update the cache.
--- Called before _refresh_diff_buffer to pick up file edits.
---@param filename string
---@param cb? fun()
function M._update_file_diff_cache_async(filename, cb)
  M._file_diffs = M._file_diffs or {}
  M._file_hunk_staged = M._file_hunk_staged or {}
  -- Untracked files: build the diff from disk, never from git. Cache `false`
  -- (not nil) for empty/binary so the preview guard treats it as "checked"
  -- and doesn't re-run this on every cursor move.
  local relpath = M._untracked and M._untracked[filename]
  if relpath then
    local diff_text = build_untracked_diff(filename, relpath)
    M._file_diffs[filename] = diff_text or false
    M._file_hunk_staged[filename] = diff_text and { false } or nil
    if cb then cb() end
    return
  end
  git_root_async(function(cwd)
    if not cwd then
      if cb then cb() end
      return
    end
    file_diff_and_flags_async(cwd, filename, function(diff_text, flags)
      M._file_diffs[filename] = diff_text or false
      M._file_hunk_staged[filename] = flags
      if cb then cb() end
    end)
  end)
end

--- Refresh an open diff buffer for the given filename (if one exists).
--- Called from Trouble S/U actions to sync the diff buffer.
---@param filename string
function M.refresh_open_diff_buffer(filename)
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

  -- Re-fetch diff data for this file only (cache is stale after staging)
  M._update_file_diff_cache_async(filename, function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    M._buf_last_rendered[buf] = nil  -- force re-render
    M._refresh_diff_buffer(buf, filename)
  end)
end

local function confirm(lines, on_yes)
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = 32
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = #body,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - #body) / 2),
    style = "minimal",
    border = "rounded",
    title = " Confirm ",
    title_pos = "center",
  })
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true })
  end
end

---@class DiffReviewSectionConfig
---@field name string
---@field title string
---@field default_folded boolean

---@type DiffReviewSectionConfig[]
local status_section_order = {
  { name = "unstaged", title = "Unstaged changes", default_folded = false },
  { name = "untracked", title = "Untracked files", default_folded = false },
  { name = "staged", title = "Staged changes", default_folded = false },
}
local status_file_indent = 0
local status_hunk_indent = 0

---@type table<string, DiffReviewSectionConfig>
local status_section_by_name = {}
for _, section in ipairs(status_section_order) do
  status_section_by_name[section.name] = section
end

local function status_folded(key, default)
  M._status = M._status or {}
  M._status.folds = M._status.folds or {}
  local folded = M._status.folds[key]
  if folded == nil then return default end
  return folded
end

local function set_status_folded(key, folded)
  M._status = M._status or {}
  M._status.folds = M._status.folds or {}
  M._status.folds[key] = folded
end

---@param cwd string
---@param args string[]
---@param cb fun(line?: string)
local function status_git_line_async(cwd, args, cb)
  local command = { "git", "-C", cwd }
  vim.list_extend(command, args)
  systemlist_async(command, function(result, code)
    if code ~= 0 then
      cb(nil)
      return
    end
    cb(vim.trim(result[1] or ""))
  end)
end

local function status_head_row(name, oid, ref, ref_hl, subject)
  return {
    { ("%-8s"):format(name .. ":"), "DiffReviewStatusLabel" },
    { ("%-7s"):format(oid or ""), "DiffReviewStatusObjectId" },
    { " " },
    { ref or "", ref_hl },
    { " " .. (subject or "") },
  }
end

---@param cwd string
---@param cb fun(lines: table[])
local function status_head_lines_async(cwd, cb)
  local values = {}
  local pending = 9

  local function done(key, value)
    values[key] = value
    pending = pending - 1
    if pending > 0 then return end

    local lines = {}
    lines[#lines + 1] = status_head_row(
      "Head",
      values.head_oid or "0000000",
      values.branch or "(detached)",
      "DiffReviewStatusBranch",
      values.subject or "(no commits)"
    )

    if values.upstream then
      lines[#lines + 1] = status_head_row(
        "Merge",
        values.upstream_oid or "",
        values.upstream,
        "DiffReviewStatusRemote",
        values.upstream_subject or ""
      )
    end

    if values.push_ref then
      lines[#lines + 1] = status_head_row(
        "Push",
        values.push_oid or "",
        values.push_ref,
        "DiffReviewStatusRemote",
        values.push_subject or ""
      )
    end

    cb(lines)
  end

  status_git_line_async(cwd, { "rev-parse", "--short", "HEAD" }, function(line) done("head_oid", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "HEAD" }, function(line) done("branch", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s" }, function(line) done("subject", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "@{upstream}" }, function(line) done("upstream", line) end)
  status_git_line_async(cwd, { "rev-parse", "--short", "@{upstream}" }, function(line) done("upstream_oid", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s", "@{upstream}" }, function(line) done("upstream_subject", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "@{push}" }, function(line) done("push_ref", line) end)
  status_git_line_async(cwd, { "rev-parse", "--short", "@{push}" }, function(line) done("push_oid", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s", "@{push}" }, function(line) done("push_subject", line) end)
end

local function status_hunk_key(section_name, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("hunk:%s:%s:%s"):format(section_name, filename, hash)
end

local function status_file_key(section_name, filename)
  return ("file:%s:%s"):format(section_name, filename)
end

local function status_section_key(section_name)
  return "section:" .. section_name
end

local function status_add_highlight(line, start_col, end_col, hl_group)
  M._status.highlights[#M._status.highlights + 1] = {
    line = line,
    start_col = start_col,
    end_col = end_col,
    hl_group = hl_group,
  }
end

local function status_add_extmark(line, col, opts)
  M._status.extmarks[#M._status.extmarks + 1] = {
    line = line,
    col = col,
    opts = opts,
  }
end

local function status_add_line(text, entry, line_hl_group)
  M._status.lines[#M._status.lines + 1] = text
  local line = #M._status.lines
  if entry then M._status.entries[line] = entry end
  if line_hl_group then
    M._status.line_highlights[#M._status.line_highlights + 1] = {
      line = line,
      hl_group = line_hl_group,
    }
  end
  return line
end

local function status_add_segment_line(segments, entry)
  local parts = {}
  local col = 0
  local segment_highlights = {}
  for _, segment in ipairs(segments) do
    local text = segment[1] or ""
    parts[#parts + 1] = text
    if segment[2] and text ~= "" then
      segment_highlights[#segment_highlights + 1] = {
        start_col = col,
        end_col = col + #text,
        hl_group = segment[2],
      }
    end
    col = col + #text
  end
  local line = status_add_line(table.concat(parts), entry)
  for _, highlight in ipairs(segment_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
end

local function status_add_hint_line()
  status_add_segment_line({
    { "Hint: ", "DiffReviewStatusHint" },
    { "<tab>", "DiffReviewStatusHintKey" },
    { " toggle | ", "DiffReviewStatusHint" },
    { "S", "DiffReviewStatusHintKey" },
    { " stage | ", "DiffReviewStatusHint" },
    { "U", "DiffReviewStatusHintKey" },
    { " unstage | ", "DiffReviewStatusHint" },
    { "j", "DiffReviewStatusHintKey" },
    { " discard | ", "DiffReviewStatusHint" },
    { "c", "DiffReviewStatusHintKey" },
    { " commit | ", "DiffReviewStatusHint" },
    { "pP", "DiffReviewStatusHintKey" },
    { " push | ", "DiffReviewStatusHint" },
    { "<CR>", "DiffReviewStatusHintKey" },
    { " jump | ", "DiffReviewStatusHint" },
    { "q", "DiffReviewStatusHintKey" },
    { " close | ", "DiffReviewStatusHint" },
    { "?", "DiffReviewStatusHintKey" },
    { " help", "DiffReviewStatusHint" },
  })
end

local function replace_text_range(text, start_col, replacement)
  local required = start_col + #replacement
  if #text < required then
    text = text .. string.rep(" ", required - #text)
  end
  return text:sub(1, start_col) .. replacement .. text:sub(required + 1)
end

local function status_add_fancy_row(row, entry, indent)
  indent = indent or 0
  local text_parts = {}
  if indent > 0 then
    text_parts[#text_parts + 1] = string.rep(" ", indent)
  end

  local col = indent
  local row_highlights = {}
  local row_extmarks = {}
  local gutter_overlays = {}
  local diff_line = nil
  for _, chunk in ipairs(row) do
    if chunk.meta and chunk.meta.diff then
      diff_line = chunk.meta.diff
    end
    if type(chunk[1]) == "string" then
      local text = chunk[1]
      if text ~= "" then
        text_parts[#text_parts + 1] = text
        if chunk[2] then
          row_highlights[#row_highlights + 1] = {
            start_col = col,
            end_col = col + #text,
            hl_group = chunk[2],
          }
        end
        col = col + #text
      end
    elseif chunk.virt_text then
      local overlay_text = chunk.virt_text[1] and chunk.virt_text[1][1]
      if chunk.virt_text_pos == "overlay" and overlay_text and #overlay_text <= 8 and (chunk.col or 0) <= 8 then
        gutter_overlays[#gutter_overlays + 1] = {
          col = (chunk.col or 0) + indent,
          text = overlay_text,
          hl_group = chunk.virt_text[1][2],
        }
      else
        local opts = {}
        for key, value in pairs(chunk) do
          if key ~= "col" then
            opts[key] = value
          end
        end
        row_extmarks[#row_extmarks + 1] = {
          col = (chunk.col or 0) + indent,
          opts = opts,
        }
      end
    end
  end

  local line_text = table.concat(text_parts)
  table.sort(gutter_overlays, function(left, right)
    return left.col < right.col
  end)
  for _, overlay in ipairs(gutter_overlays) do
    line_text = replace_text_range(line_text, overlay.col, overlay.text)
    if overlay.hl_group then
      row_highlights[#row_highlights + 1] = {
        start_col = overlay.col,
        end_col = overlay.col + #overlay.text,
        hl_group = replace_hl(overlay.hl_group),
      }
    end
  end

  local line_entry = entry
  if diff_line and entry then
    line_entry = vim.tbl_extend("force", entry, { diff_line = diff_line })
  end
  local line = status_add_line(line_text, line_entry)
  for _, highlight in ipairs(row_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
  for _, extmark in ipairs(row_extmarks) do
    status_add_extmark(line, extmark.col, extmark.opts)
  end
end

---@param item table
---@return "unstaged"|"staged"|"untracked"
local function status_section_for_item(item)
  local data = item.item or {}
  if data.category == "Untracked Files" then return "untracked" end
  if data.staged then return "staged" end
  return "unstaged"
end

---@param collected_items table[]
---@return DiffReviewStatusSection[]
local function status_sections_from_items(collected_items)
  local sections = {} ---@type table<string, DiffReviewStatusSection>
  for _, section_config in ipairs(status_section_order) do
    sections[section_config.name] = {
      name = section_config.name,
      title = section_config.title,
      default_folded = section_config.default_folded,
      files = {},
      files_by_name = {},
    }
  end

  for _, item in ipairs(collected_items) do
    local filename = item.filename
    local data = item.item or {}
    if filename then
      local section_name = status_section_for_item(item)
      local section = sections[section_name]
      local file = section.files_by_name[filename]
      if not file then
        file = {
          filename = filename,
          relpath = vim.fn.fnamemodify(filename, ":."),
          section_name = section_name,
          added = 0,
          removed = 0,
          hunks = {},
          untracked = section_name == "untracked",
          status = data.stats or data.hunk_header or "",
          git_status = data.git_status,
        }
        section.files_by_name[filename] = file
        section.files[#section.files + 1] = file
      end

      file.added = file.added + (data.added or 0)
      file.removed = file.removed + (data.removed or 0)
      if data.diff then
        file.hunks[#file.hunks + 1] = {
          filename = filename,
          section_name = section_name,
          pos = item.pos and item.pos[1] or 1,
          diff = data.diff,
          staged = data.staged == true,
          context_text = data.context_text or "",
          git_status = data.git_status,
          added = data.added or 0,
          removed = data.removed or 0,
        }
      end
    end
  end

  local ordered = {} ---@type DiffReviewStatusSection[]
  for _, section_config in ipairs(status_section_order) do
    local section = sections[section_config.name]
    table.sort(section.files, function(left_file, right_file)
      return left_file.relpath < right_file.relpath
    end)
    for _, file in ipairs(section.files) do
      table.sort(file.hunks, function(left_hunk, right_hunk)
        return (left_hunk.pos or 0) < (right_hunk.pos or 0)
      end)
    end
    if #section.files > 0 then ordered[#ordered + 1] = section end
  end

  return ordered
end

---@return table<DiffReviewStatusSectionName, DiffReviewStatusSection>
local function status_empty_section_map()
  local sections = {}
  for _, section_config in ipairs(status_section_order) do
    sections[section_config.name] = {
      name = section_config.name,
      title = section_config.title,
      default_folded = section_config.default_folded,
      files = {},
      files_by_name = {},
    }
  end
  return sections
end

---@param sections DiffReviewStatusSection[]?
---@return table<DiffReviewStatusSectionName, DiffReviewStatusSection>
local function status_section_map(sections)
  local section_map = status_empty_section_map()
  for _, section in ipairs(sections or {}) do
    section_map[section.name] = section
    section.files_by_name = {}
    for _, file in ipairs(section.files or {}) do
      section.files_by_name[file.filename] = file
    end
  end
  return section_map
end

---@param section DiffReviewStatusSection
local function status_reindex_section(section)
  section.files_by_name = {}
  for _, file in ipairs(section.files or {}) do
    section.files_by_name[file.filename] = file
  end
end

---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection>
---@param section_name DiffReviewStatusSectionName
---@param filename string
---@return DiffReviewStatusFile?
local function status_remove_file_from_section(section_map, section_name, filename)
  local section = section_map[section_name]
  if not section then return nil end
  local removed_file = section.files_by_name and section.files_by_name[filename] or nil
  if not removed_file then return nil end
  for index = #section.files, 1, -1 do
    if section.files[index].filename == filename then
      table.remove(section.files, index)
      break
    end
  end
  status_reindex_section(section)
  return removed_file
end

---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection>
---@return DiffReviewStatusSection[]
local function status_order_section_map(section_map)
  local ordered = {}
  for _, section_config in ipairs(status_section_order) do
    local section = section_map[section_config.name]
    table.sort(section.files, function(left_file, right_file)
      return left_file.relpath < right_file.relpath
    end)
    for _, file in ipairs(section.files) do
      file.section_name = section.name
      file.untracked = section.name == "untracked"
      if section.name == "untracked" then
        file.git_status = "??"
      elseif section.name == "staged" and file.git_status == "??" then
        file.git_status = "A"
      end
      for _, hunk in ipairs(file.hunks or {}) do
        hunk.section_name = section.name
        hunk.staged = section.name == "staged"
        hunk.git_status = file.git_status or hunk.git_status
      end
      table.sort(file.hunks, function(left_hunk, right_hunk)
        return (left_hunk.pos or 0) < (right_hunk.pos or 0)
      end)
    end
    status_reindex_section(section)
    if #section.files > 0 then ordered[#ordered + 1] = section end
  end
  return ordered
end

---@param file DiffReviewStatusFile
---@param section_name DiffReviewStatusSectionName
---@return DiffReviewStatusFile
local function status_copy_file_for_section(file, section_name)
  local copied_file = vim.deepcopy(file)
  copied_file.section_name = section_name
  copied_file.untracked = section_name == "untracked"
  if section_name == "untracked" then
    copied_file.git_status = "??"
  elseif section_name == "staged" and file.untracked then
    copied_file.git_status = "A"
  end
  copied_file.hunks = copied_file.hunks or {}
  for _, hunk in ipairs(copied_file.hunks) do
    hunk.section_name = section_name
    hunk.staged = section_name == "staged"
    hunk.git_status = copied_file.git_status or hunk.git_status
  end
  return copied_file
end

---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection>
---@param section_name DiffReviewStatusSectionName
---@param file DiffReviewStatusFile
---@return DiffReviewStatusFile
local function status_ensure_file(section_map, section_name, file)
  local section = section_map[section_name]
  local existing_file = section.files_by_name[file.filename]
  if existing_file then return existing_file end
  existing_file = {
    filename = file.filename,
    relpath = file.relpath,
    section_name = section_name,
    added = 0,
    removed = 0,
    hunks = {},
    untracked = section_name == "untracked",
    status = file.status,
    git_status = section_name == "untracked" and "??" or file.git_status,
  }
  section.files[#section.files + 1] = existing_file
  section.files_by_name[file.filename] = existing_file
  return existing_file
end

---@param sections DiffReviewStatusSection[]?
---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@return DiffReviewStatusSection[]?
local function status_apply_optimistic_move(sections, entries, target_section)
  if not sections then return nil end
  local section_map = status_section_map(sections)
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file then
      local source_section = entry.file.section_name
      if source_section ~= target_section then
        status_remove_file_from_section(section_map, source_section, entry.file.filename)
        local moved_file = status_copy_file_for_section(entry.file, target_section)
        local section = section_map[target_section]
        section.files[#section.files + 1] = moved_file
        section.files_by_name[moved_file.filename] = moved_file
      end
    elseif entry.kind == "hunk" and entry.file and entry.hunk then
      local source_file = entry.file
      local source_section = source_file.section_name
      if source_section ~= target_section then
        local source = section_map[source_section] and section_map[source_section].files_by_name[source_file.filename]
        if source then
          for index = #source.hunks, 1, -1 do
            if source.hunks[index].diff == entry.hunk.diff then
              table.remove(source.hunks, index)
              break
            end
          end
          source.added = math.max(0, source.added - (entry.hunk.added or 0))
          source.removed = math.max(0, source.removed - (entry.hunk.removed or 0))
          if #source.hunks == 0 then
            status_remove_file_from_section(section_map, source_section, source.filename)
          end
        end
        local target_file = status_ensure_file(section_map, target_section, source_file)
        local moved_hunk = vim.deepcopy(entry.hunk)
        moved_hunk.section_name = target_section
        moved_hunk.staged = target_section == "staged"
        moved_hunk.git_status = target_file.git_status or moved_hunk.git_status
        target_file.hunks[#target_file.hunks + 1] = moved_hunk
        target_file.added = target_file.added + (moved_hunk.added or 0)
        target_file.removed = target_file.removed + (moved_hunk.removed or 0)
      end
    end
  end
  return status_order_section_map(section_map)
end

---@param cwd string
---@param cb fun(result: { head_lines: table[], sections: DiffReviewStatusSection[] })
local function status_load_async(cwd, cb)
  local head_lines = nil
  local sections = nil

  local function maybe_done()
    if not (head_lines and sections) then return end
    cb({ head_lines = head_lines, sections = sections })
  end

  status_head_lines_async(cwd, function(lines)
    head_lines = lines
    maybe_done()
  end)
  collect_items_from_git(cwd, function(items)
    sections = status_sections_from_items(items or {})
    maybe_done()
  end, { skip_pre_render = true, skip_ts_context = true })
end

---@param file DiffReviewStatusFile
---@return DiffReviewHunk[]
local function status_diff_hunks_for_file(file)
  if #file.hunks > 0 then return file.hunks end
  if not file.untracked then return {} end

  local relpath = M._untracked and M._untracked[file.filename]
  local diff_text = relpath and build_untracked_diff(file.filename, relpath) or nil
  if not diff_text then return {} end

  local hunks = {}
  for _, parsed_hunk in ipairs(parse_diff(diff_text, false)) do
    hunks[#hunks + 1] = {
      filename = file.filename,
      section_name = file.section_name,
      pos = parsed_hunk.pos,
      diff = parsed_hunk.diff,
      staged = false,
      context_text = parsed_hunk.context or "",
      added = parsed_hunk.added or 0,
      removed = parsed_hunk.removed or 0,
    }
  end
  return hunks
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
local function status_render_hunk(file, hunk)
  local hunk_key = status_hunk_key(file.section_name, file.filename, hunk.diff)
  local hunk_folded = status_folded(hunk_key, false)
  local entry = { id = hunk_key, kind = "hunk", file = file, hunk = hunk }
  M._status.fancy_rows = M._status.fancy_rows or {}
  local rows_key = ("%s:%s"):format(hunk_key, hunk.staged and "staged" or "unstaged")
  local rows = M._status.fancy_rows[rows_key]
  local function rerender_with_context()
    M._status = M._status or {}
    if M._status.fancy_rows then
      M._status.fancy_rows[rows_key] = nil
    end
    local buf = M._status.buf
    if buf and vim.api.nvim_buf_is_valid(buf) then
      local cursor = vim.api.nvim_win_get_cursor(0)[1]
      M.render_status(buf, hunk_key, cursor, { reuse_sections = true })
    end
  end
  if not rows then
    local ok, built_rows = pcall(
      build_fancy_diff_rows,
      hunk.diff,
      { hunk.staged },
      file.filename,
      function(hunk_line)
        return ("status:%s:%d"):format(rows_key, hunk_line)
      end,
      rerender_with_context,
      { context_line = hunk.pos, boundary_context = true }
    )
    if ok then
      rows = built_rows
      M._status.fancy_rows[rows_key] = rows
    end
  end
  if not rows then
    local ts_context = cached_hunk_context(
      file.filename,
      hunk.pos,
      "status-fallback:" .. hunk_key,
      rerender_with_context
    )
    local context_text = hunk_context_label(ts_context) or hunk.context_text or ""
    local context = context_text ~= "" and (context_text .. " ") or ""
    local header = ("%s@@ %s+%d -%d"):format(string.rep(" ", status_hunk_indent), context, hunk.added or 0, hunk.removed or 0)
    status_add_line(header, entry, hunk_folded and "DiffReviewActiveHunkHeader" or "DiffReviewHunkHeader")
    if not hunk_folded and type(ts_context) == "table" then
      local visible_hunk_lines = hunk_visible_source_lines(hunk.diff)
      local node_start = ts_context.start_row + 1
      local node_end = ts_context.end_row + 1
      local start_text = ts_context.start_text or ""
      local end_text = ts_context.end_text or ""
      if not visible_hunk_lines[start_text] then
        status_add_fancy_row(hunk_boundary_row(ts_context.start_text or "", ts_context.start_segments, node_start), nil, status_hunk_indent)
        if node_start ~= node_end then
          status_add_fancy_row(hunk_boundary_ellipsis_row(start_text), nil, status_hunk_indent)
        end
      end
      if not visible_hunk_lines[end_text] then
        if node_end ~= node_start then
          status_add_fancy_row(hunk_boundary_ellipsis_row(end_text), nil, status_hunk_indent)
        end
        if node_end ~= node_start then
          status_add_fancy_row(hunk_boundary_row(end_text, ts_context.end_segments, node_end), nil, status_hunk_indent)
        end
      end
    end
    return
  end

  if hunk_folded then
    status_add_fancy_row(rows[1], entry, status_hunk_indent)
    return
  end

  status_add_fancy_row(rows[1], entry, status_hunk_indent)
  for row_index = 2, #rows do
    local row = rows[row_index]
    if row then status_add_fancy_row(row, row.diff_review_boundary and nil or entry, status_hunk_indent) end
  end
end

---@param file DiffReviewStatusFile
local function status_render_file(file)
  local file_key = status_file_key(file.section_name, file.filename)
  local file_folded = status_folded(file_key, true)
  local stats = file.untracked and "new" or ("+%d -%d"):format(file.added, file.removed)
  local line = ("%s%s %s"):format(string.rep(" ", status_file_indent), file.relpath, stats)
  local entry = { id = file_key, kind = "file", file = file }
  local line_number = status_add_line(line, entry)
  local path_start = status_file_indent
  local stats_start = #line - #stats
  status_add_highlight(line_number, path_start, stats_start - 1, "DiffReviewStatusPath")
  status_add_highlight(line_number, stats_start, #line, file.untracked and "Comment" or "DiffReviewAddRange")

  if file_folded then return end

  local hunks = status_diff_hunks_for_file(file)
  if #hunks == 0 then
    status_add_line(string.rep(" ", status_hunk_indent) .. "No textual diff", entry, "Comment")
    return
  end
  for _, hunk in ipairs(hunks) do
    status_render_hunk(file, hunk)
  end
end

---@param section DiffReviewStatusSection
local function status_render_section(section)
  local section_key = status_section_key(section.name)
  local section_folded = status_folded(section_key, section.default_folded)
  local line = ("%s (%d)"):format(section.title, #section.files)
  local entry = { id = section_key, kind = "section", section = section }
  status_add_line(line, entry, "DiffReviewStatusHeader")
  if section_folded then return end
  for _, file in ipairs(section.files) do
    status_render_file(file)
  end
end

---@return DiffReviewStatusEntry?
local function status_entry_under_cursor()
  local status = M._status
  if not status then return nil end
  return status.entries[vim.api.nvim_win_get_cursor(0)[1]]
end

local status_files_from_set

---@param entry DiffReviewStatusEntry?
---@return string[]
local function status_files_for_entry(entry)
  if not entry then return {} end
  if entry.kind == "hunk" then return { entry.file.filename } end
  if entry.kind == "file" then return { entry.file.filename } end
  if entry.kind == "section" then
    local files = {}
    for _, file in ipairs(entry.section.files or {}) do
      files[#files + 1] = file.filename
    end
    return files
  end
  return {}
end

---@param start_line integer
---@param end_line integer
---@return DiffReviewStatusEntry[]
local function status_entries_for_lines(start_line, end_line)
  local status = M._status
  if not status then return {} end
  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

  local entries = {}
  local seen = {}
  for line = start_line, end_line do
    local entry = status.entries[line]
    if entry and entry.id and not seen[entry.id] then
      seen[entry.id] = true
      entries[#entries + 1] = entry
    end
  end
  return entries
end

---@return DiffReviewStatusEntry[]
local function status_entries_from_visual_selection()
  local mode = vim.fn.mode()
  local in_visual_mode = mode == "v" or mode == "V" or mode:byte() == 22
  if in_visual_mode then
    return status_entries_for_lines(vim.fn.line("v"), vim.api.nvim_win_get_cursor(0)[1])
  end

  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")
  return status_entries_for_lines(start_pos[2], end_pos[2])
end

---@param entry DiffReviewStatusEntry?
---@return DiffReviewStatusEntry[]
local function status_file_entries_for_entry(entry)
  if not entry then return {} end
  if entry.kind == "section" then
    local entries = {}
    for _, file in ipairs(entry.section.files or {}) do
      entries[#entries + 1] = { id = status_file_key(file.section_name, file.filename), kind = "file", file = file }
    end
    return entries
  end
  return { entry }
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[]
local function status_expanded_entries(entries)
  local expanded_entries = {}
  local seen = {}
  for _, selected_entry in ipairs(entries or {}) do
    for _, entry in ipairs(status_file_entries_for_entry(selected_entry)) do
      local id = entry.id or ("%s:%s"):format(entry.kind or "entry", (entry.file and entry.file.filename) or "")
      if not seen[id] then
        seen[id] = true
        expanded_entries[#expanded_entries + 1] = entry
      end
    end
  end
  return expanded_entries
end

---@param file_set table<string, boolean>
---@return string[]
function status_files_from_set(file_set)
  local files = {}
  for filename in pairs(file_set) do
    files[#files + 1] = filename
  end
  table.sort(files)
  return files
end

---@param items table<any, any>
---@return integer
local function status_count_set(items)
  local count = 0
  for _ in pairs(items) do
    count = count + 1
  end
  return count
end

---@param action string
---@param hunk_count integer
---@param file_count integer
local function status_notify_action(action, hunk_count, file_count)
  if hunk_count <= 0 and file_count <= 0 then return end
  local parts = {}
  if hunk_count > 0 then
    parts[#parts + 1] = ("%d hunk(s)"):format(hunk_count)
  end
  if file_count > 0 then
    parts[#parts + 1] = ("%d file(s)"):format(file_count)
  end
  vim.notify(("%s %s"):format(action, table.concat(parts, ", ")), vim.log.levels.INFO)
end

local function status_restore_cursor(buf, target_id, fallback_line)
  local target_line = nil
  if target_id then
    for line, entry in pairs(M._status.entries or {}) do
      if entry.id == target_id then
        target_line = line
        break
      end
    end
  end
  target_line = target_line or math.min(fallback_line or 1, vim.api.nvim_buf_line_count(buf))
  pcall(vim.api.nvim_win_set_cursor, 0, { math.max(target_line, 1), 0 })
end

---@param buf integer
---@param lines string[]
local function status_set_plain_lines(buf, lines)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
---@param head_lines table[]
---@param sections DiffReviewStatusSection[]
local function status_render_loaded(buf, target_id, fallback_line, opts, head_lines, sections)
  opts = opts or {}
  setup_bg_highlights()
  M._status = M._status or {}
  M._status.buf = buf
  M._status.lines = {}
  M._status.entries = {}
  M._status.highlights = {}
  M._status.line_highlights = {}
  M._status.extmarks = {}

  status_add_hint_line()
  status_add_line("")
  for _, head_line in ipairs(head_lines) do
    status_add_segment_line(head_line)
  end
  status_add_line("")

  if #sections == 0 then
    status_add_line("No changes", nil, "Comment")
  else
    for index, section in ipairs(sections) do
      if index > 1 then
        status_add_line("")
      end
      status_render_section(section)
    end
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, M._status.lines)
  vim.bo[buf].modifiable = false

  for _, line_hl in ipairs(M._status.line_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, line_hl.line - 1, 0, {
      line_hl_group = line_hl.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(M._status.highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  for _, extmark in ipairs(M._status.extmarks) do
    local opts = vim.tbl_extend("force", { priority = 95 }, extmark.opts)
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, extmark.line - 1, extmark.col, opts)
  end

  status_restore_cursor(buf, target_id, fallback_line)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
function M.render_status(buf, target_id, fallback_line, opts)
  opts = opts or {}
  setup_bg_highlights()
  M._status = M._status or {}
  M._status.buf = buf

  if opts.reuse_sections and M._status.head_lines and M._status.sections then
    status_render_loaded(buf, target_id, fallback_line, opts, M._status.head_lines, M._status.sections)
    return
  end

  M._status.request_id = (M._status.request_id or 0) + 1
  local request_id = M._status.request_id
  local has_existing_view = M._status.head_lines ~= nil or M._status.sections ~= nil
  if not has_existing_view then
    status_set_plain_lines(buf, { "Loading DiffReview..." })
  end

  git_root_async(function(cwd, root_err)
    if not (M._status and M._status.request_id == request_id) then return end
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      if not has_existing_view then
        status_set_plain_lines(buf, { "Not a git repository" })
      end
      return
    end

    status_load_async(cwd, function(result)
      if not (M._status and M._status.request_id == request_id) then return end
      if not vim.api.nvim_buf_is_valid(buf) then return end
      M._status.head_lines = result.head_lines
      M._status.sections = result.sections
      M._status.fancy_rows = {}
      status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
    end)
  end)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
local function render_status_or_notify(buf, target_id, fallback_line, opts)
  local ok, err = xpcall(function()
    M.render_status(buf, target_id, fallback_line, opts)
  end, debug.traceback)
  if not ok then
    notify_error("DiffReview render failed: " .. tostring(err))
  end
end

---@param target_id? string
local function status_render_current_model(target_id)
  local status = M._status
  if not (status and status.buf and vim.api.nvim_buf_is_valid(status.buf) and status.head_lines and status.sections) then
    return
  end
  status.fancy_rows = {}
  status_render_loaded(status.buf, target_id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true }, status.head_lines, status.sections)
end

---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@param target_id? string
local function status_apply_optimistic_entries(entries, target_section, target_id)
  local status = M._status
  if not (status and status.sections) then return end
  local next_sections = status_apply_optimistic_move(status.sections, entries, target_section)
  if not next_sections then return end
  status.sections = next_sections
  status_render_current_model(target_id)
end

---@param buf integer
---@param target_id? string
local function refresh_status_after_action(buf, target_id)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  render_status_or_notify(buf, target_id, vim.api.nvim_win_get_cursor(0)[1])
end

---@param buf integer?
---@param target_id? string
local function status_request_reconcile(buf, target_id)
  local status = M._status
  if not status then return end
  status.reconcile_buf = buf or status.buf
  status.reconcile_target_id = target_id or status.reconcile_target_id
  if not status.operation_running and #(status.operation_queue or {}) == 0 and status.reconcile_buf then
    local reconcile_buf = status.reconcile_buf
    local reconcile_target_id = status.reconcile_target_id
    status.reconcile_buf = nil
    status.reconcile_target_id = nil
    refresh_status_after_action(reconcile_buf, reconcile_target_id)
  end
end

---@param operation fun(done: fun())
local function status_enqueue_operation(operation)
  M._status = M._status or {}
  local status = M._status
  status.operation_queue = status.operation_queue or {}
  status.operation_queue[#status.operation_queue + 1] = operation

  local function run_next()
    if status.operation_running then return end
    local next_operation = table.remove(status.operation_queue, 1)
    if not next_operation then
      status_request_reconcile(status.reconcile_buf, status.reconcile_target_id)
      return
    end
    status.operation_running = true
    next_operation(function()
      status.operation_running = false
      run_next()
    end)
  end

  run_next()
end

---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@return DiffReviewStatusEntry[]
local function status_action_entries_for_target(entries, target_section)
  local action_entries = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file and entry.file.section_name ~= target_section then
      action_entries[#action_entries + 1] = entry
    elseif entry.kind == "hunk" and entry.hunk and entry.file and entry.hunk.staged ~= (target_section == "staged") then
      action_entries[#action_entries + 1] = entry
    end
  end
  return action_entries
end

---@param entries DiffReviewStatusEntry[]
---@return string[] hunk_diffs
---@return string[] files
local function status_split_action_entries(entries)
  local hunk_diffs = {}
  local files_to_move = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk then
      hunk_diffs[#hunk_diffs + 1] = entry.hunk.diff
    elseif entry.kind == "file" and entry.file then
      files_to_move[entry.file.filename] = true
    end
  end
  return hunk_diffs, status_files_from_set(files_to_move)
end

---@param entry DiffReviewStatusEntry
---@return boolean
local function status_entry_is_added(entry)
  return git_status_is_added((entry.file and entry.file.git_status) or (entry.hunk and entry.hunk.git_status))
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[]
local function status_unstage_action_entries(entries)
  local action_entries = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file and entry.file.section_name == "staged" then
      action_entries[#action_entries + 1] = entry
    elseif entry.kind == "hunk" and entry.hunk and entry.hunk.staged then
      action_entries[#action_entries + 1] = entry
    end
  end
  return action_entries
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[] tracked_entries
---@return DiffReviewStatusEntry[] added_entries
local function status_partition_unstage_entries(entries)
  local tracked_entries = {}
  local added_entries = {}
  for _, entry in ipairs(entries) do
    if status_entry_is_added(entry) then
      added_entries[#added_entries + 1] = entry
    else
      tracked_entries[#tracked_entries + 1] = entry
    end
  end
  return tracked_entries, added_entries
end

---@param entries DiffReviewStatusEntry[]
---@return string[] hunk_diffs
---@return string[] tracked_files
---@return string[] added_files
local function status_split_unstage_entries(entries)
  local hunk_diffs = {}
  local tracked_files = {}
  local added_files = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk then
      hunk_diffs[#hunk_diffs + 1] = entry.hunk.diff
    elseif entry.kind == "file" and entry.file then
      if status_entry_is_added(entry) then
        added_files[entry.file.filename] = true
      else
        tracked_files[entry.file.filename] = true
      end
    end
  end
  return hunk_diffs, status_files_from_set(tracked_files), status_files_from_set(added_files)
end

---@param entries DiffReviewStatusEntry[]
local function status_stage_entries(entries)
  if #entries == 0 then return end
  local expanded_entries = status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_action_entries_for_target(expanded_entries, "staged")
  if #action_entries == 0 then return end

  local first_id = action_entries[1].id
  local hunk_diffs, files = status_split_action_entries(action_entries)
  local staged_hunks = 0
  local staged_files = 0
  local status_buf = M._status and M._status.buf

  status_apply_optimistic_entries(action_entries, "staged", first_id)

  local function finish()
    if staged_hunks > 0 or staged_files > 0 then
      status_notify_action("Staged", staged_hunks, staged_files)
    end
    status_request_reconcile(status_buf, first_id)
  end

  local function stage_files_after_hunks()
    if #files == 0 then
      finish()
      return
    end
    M.stage_files_async(files, function(result)
      staged_files = #result.successes
      finish()
    end)
  end

  local function stage_hunk_at(index)
    local diff = hunk_diffs[index]
    if not diff then
      stage_files_after_hunks()
      return
    end
    M.stage_patch_async(diff, function(ok)
      if ok then staged_hunks = staged_hunks + 1 end
      stage_hunk_at(index + 1)
    end)
  end

  status_enqueue_operation(function(done)
    local original_finish = finish
    finish = function()
      original_finish()
      done()
    end
    stage_hunk_at(1)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_stage(entry)
  if not entry then return end
  status_stage_entries({ entry })
end

---@param entries DiffReviewStatusEntry[]
local function status_unstage_entries(entries)
  if #entries == 0 then return end
  local expanded_entries = status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_unstage_action_entries(expanded_entries)
  if #action_entries == 0 then return end

  local first_id = action_entries[1].id
  local tracked_entries, added_entries = status_partition_unstage_entries(action_entries)
  local hunk_diffs, files, added_files = status_split_unstage_entries(action_entries)
  local unstaged_hunks = 0
  local unstaged_files = 0
  local status_buf = M._status and M._status.buf

  status_apply_optimistic_entries(tracked_entries, "unstaged", first_id)
  status_apply_optimistic_entries(added_entries, "untracked", first_id)

  local function finish()
    if unstaged_hunks > 0 or unstaged_files > 0 then
      status_notify_action("Unstaged", unstaged_hunks, unstaged_files)
    end
    status_request_reconcile(status_buf, first_id)
  end

  local function unstage_files_after_hunks()
    if #files == 0 then
      if #added_files == 0 then
        finish()
        return
      end
      M.unstage_added_files_async(added_files, function(result)
        unstaged_files = unstaged_files + #result.successes
        finish()
      end)
      return
    end
    M.unstage_files_async(files, function(result)
      unstaged_files = unstaged_files + #result.successes
      if #added_files == 0 then
        finish()
        return
      end
      M.unstage_added_files_async(added_files, function(added_result)
        unstaged_files = unstaged_files + #added_result.successes
        finish()
      end)
    end)
  end

  local function unstage_hunk_at(index)
    local diff = hunk_diffs[index]
    if not diff then
      unstage_files_after_hunks()
      return
    end
    M.unstage_patch_async(diff, function(ok)
      if ok then unstaged_hunks = unstaged_hunks + 1 end
      unstage_hunk_at(index + 1)
    end)
  end

  status_enqueue_operation(function(done)
    local original_finish = finish
    finish = function()
      original_finish()
      done()
    end
    unstage_hunk_at(1)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_unstage(entry)
  if not entry then return end
  status_unstage_entries({ entry })
end

---@param entries DiffReviewStatusEntry[]
---@param target_id? string
local function status_discard_entries(entries, target_id)
  git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    local failures = {}
    if #entries == 0 then return end

    local function finish_all()
      if #failures > 0 then M.notify_git_failures("Discard failed", failures) end
      refresh_status_after_action(M._status.buf, target_id)
    end

    local function discard_at(index)
      local entry = entries[index]
      if not entry then
        finish_all()
        return
      end

      local function next_entry()
        discard_at(index + 1)
      end

      if entry.kind == "hunk" then
        local args = { "apply", "--reverse", "--whitespace=nowarn" }
        if entry.hunk.staged then args[#args + 1] = "--index" end
        args[#args + 1] = "-"
        run_git_at_root_async(cwd, args, entry.hunk.diff .. "\n", function(result)
          if not result.ok then
            failures[#failures + 1] = { file = entry.file.filename, output = result.output, code = result.code }
          end
          next_entry()
        end)
      elseif entry.file.untracked then
        local delete_code = delete_path(entry.file.filename)
        if delete_code ~= 0 then
          failures[#failures + 1] = {
            file = entry.file.filename,
            message = ("delete() failed with code %d"):format(delete_code),
          }
        end
        next_entry()
      else
        local relpath, rel_err = repo_relative(entry.file.filename, cwd)
        if not relpath then
          failures[#failures + 1] = { file = entry.file.filename, message = rel_err }
          next_entry()
        else
          run_git_at_root_async(cwd, { "checkout", "HEAD", "--", relpath }, nil, function(checkout_result)
            if checkout_result.ok then
              next_entry()
              return
            end
            local unstage_args = git_status_is_added(entry.file.git_status)
              and { "rm", "--cached", "--ignore-unmatch", "--", relpath }
              or { "restore", "--staged", "--", relpath }
            run_git_at_root_async(cwd, unstage_args, nil, function(restore_result)
              if restore_result.ok then
                local delete_code = delete_path(entry.file.filename)
                if delete_code ~= 0 then
                  failures[#failures + 1] = {
                    file = entry.file.filename,
                    message = ("delete() failed with code %d after unstaging"):format(delete_code),
                  }
                end
              else
                failures[#failures + 1] = {
                  file = entry.file.filename,
                  output = restore_result.output ~= "" and restore_result.output or checkout_result.output,
                  code = restore_result.code,
                }
              end
              next_entry()
            end)
          end)
        end
      end
    end

    discard_at(1)
  end)
end

---@param entries DiffReviewStatusEntry[]
---@param target_id? string
local function status_discard_entry_list(entries, target_id)
  local discard_entries = {}
  for _, entry in ipairs(status_expanded_entries(entries)) do
    if entry.kind == "hunk" or entry.kind == "file" then
      discard_entries[#discard_entries + 1] = entry
    end
  end
  if #discard_entries == 0 then return end

  local message
  if #discard_entries == 1 then
    local first_entry = discard_entries[1]
    local prompt = first_entry.kind == "hunk" and "Discard this hunk?"
      or (first_entry.file.untracked and "Delete untracked file?" or "Discard ALL changes to file?")
    message = { prompt, "  " .. first_entry.file.relpath }
  else
    local files = {}
    for _, entry in ipairs(discard_entries) do
      files[entry.file.filename] = true
    end
    message = { ("Discard changes in %d file(s)?"):format(status_count_set(files)) }
  end
  confirm(message, function()
    status_discard_entries(discard_entries, target_id)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_discard(entry)
  if not entry then return end
  status_discard_entry_list({ entry }, entry.id)
end

---@param diff_text string
---@param old_target_line integer
---@return integer?
local function closest_current_line_for_deleted_diff_line(diff_text, old_target_line)
  local old_line
  local new_line
  local in_hunk = false
  for diff_line in diff_text:gmatch("[^\n]+") do
    if diff_line:match("^@@") then
      old_line = tonumber(diff_line:match("%-(%d+)"))
      new_line = tonumber(diff_line:match("%+(%d+)"))
      in_hunk = old_line ~= nil and new_line ~= nil
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == "-" then
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
      elseif prefix == "+" then
        new_line = new_line + 1
      else
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
        new_line = new_line + 1
      end
    end
  end
end

---@param entry DiffReviewStatusEntry?
local function status_jump(entry)
  if not (entry and entry.file and entry.file.filename) then return end
  vim.cmd.edit(vim.fn.fnameescape(entry.file.filename))
  local target_line
  if entry.diff_line and entry.diff_line.line then
    if entry.diff_line.side == "left" and entry.hunk and entry.hunk.diff then
      target_line = closest_current_line_for_deleted_diff_line(entry.hunk.diff, entry.diff_line.line)
    else
      target_line = entry.diff_line.line
    end
  end
  if entry.kind == "hunk" and entry.hunk.diff then
    target_line = target_line or tonumber(entry.hunk.diff:match("@@ %-%d+,?%d* %+(%d+)"))
  end
  if target_line then
    local max_line = vim.api.nvim_buf_line_count(0)
    target_line = math.min(math.max(target_line, 1), max_line)
    pcall(vim.api.nvim_win_set_cursor, 0, { target_line, 0 })
    vim.cmd("normal! zz")
  end
end

---@param entry DiffReviewStatusEntry?
local function status_toggle(entry)
  if not entry then return end
  local default = false
  if entry.kind == "file" then
    default = true
  elseif entry.kind == "section" then
    local section_config = status_section_by_name[entry.section.name]
    default = section_config and section_config.default_folded or false
  end
  set_status_folded(entry.id, not status_folded(entry.id, default))
  render_status_or_notify(M._status.buf, entry.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
end

---@param buf integer
local function status_push(buf)
  git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    vim.notify("Pushing changes...", vim.log.levels.INFO, { title = "DiffReview" })
    system_text_async({ "git", "-C", cwd, "push" }, nil, function(result)
      local compact = {}
      for _, line in ipairs(text_to_lines((result.stdout or "") .. (result.stderr or ""))) do
        if line ~= "" then
          compact[#compact + 1] = line
        end
      end
      if result.code == 0 then
        vim.notify("Push complete", vim.log.levels.INFO, { title = "DiffReview" })
      else
        notify_error("Push failed: " .. (#compact > 0 and table.concat(compact, "\n") or ("git exited " .. result.code)))
      end
      if vim.api.nvim_buf_is_valid(buf) then
        render_status_or_notify(buf)
      end
    end)
  end)
end

---@param buf integer
local function setup_status_keymaps(buf)
  local opts = { buffer = buf, silent = true, nowait = true }
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    M._status = nil
  end, vim.tbl_extend("force", opts, { desc = "Close DiffReview" }))

  vim.keymap.set("n", "r", function()
    refresh_status_after_action(buf, (status_entry_under_cursor() or {}).id)
  end, vim.tbl_extend("force", opts, { desc = "Refresh DiffReview" }))

  vim.keymap.set("n", "<Tab>", function()
    status_toggle(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Toggle fold" }))

  vim.keymap.set("n", "S", function()
    status_stage(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Stage hunk/file" }))
  vim.keymap.set("x", "S", function()
    status_stage_entries(status_entries_from_visual_selection())
  end, vim.tbl_extend("force", opts, { desc = "Stage selection" }))

  vim.keymap.set("n", "U", function()
    status_unstage(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Unstage hunk/file" }))
  vim.keymap.set("x", "U", function()
    status_unstage_entries(status_entries_from_visual_selection())
  end, vim.tbl_extend("force", opts, { desc = "Unstage selection" }))

  vim.keymap.set("n", "j", function()
    status_discard(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Discard hunk/file" }))
  vim.keymap.set("x", "j", function()
    local entries = status_entries_from_visual_selection()
    status_discard_entry_list(entries, (entries[1] or {}).id)
  end, vim.tbl_extend("force", opts, { desc = "Discard selection" }))

  vim.keymap.set("n", "<CR>", function()
    status_jump(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Jump to file" }))

  local function commit()
    require("diff_review.commit").commit({
      win = vim.api.nvim_get_current_win(),
      on_done = function()
        if vim.api.nvim_buf_is_valid(buf) then render_status_or_notify(buf) end
      end,
    })
  end
  vim.keymap.set("n", "c", commit, vim.tbl_extend("force", opts, { desc = "Commit" }))
  vim.keymap.set("n", "cc", commit, vim.tbl_extend("force", opts, { desc = "Commit" }))

  vim.keymap.set("n", "pP", function()
    status_push(buf)
  end, vim.tbl_extend("force", opts, { desc = "Push" }))

  vim.keymap.set("n", "?", function()
    vim.notify("<Tab> toggle | S stage | U unstage | j discard | c commit | pP push | <CR> jump | r refresh | q close", vim.log.levels.INFO, {
      title = "DiffReview",
    })
  end, vim.tbl_extend("force", opts, { desc = "DiffReview help" }))
end

--- Open a standalone, Neogit-style DiffReview status buffer.
function M.open()
  setup_bg_highlights()
  local buf = M._status and M._status.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "DiffReviewStatus"
    pcall(vim.api.nvim_buf_set_name, buf, (M.config or config.options).status_buffer_name)
    M._status = {
      buf = buf,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      sections = nil,
      fancy_rows = {},
    }
    setup_status_keymaps(buf)
  end

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview open failed: " .. tostring(err))
    return
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].foldcolumn = "0"
  render_status_or_notify(buf)
end

return M
