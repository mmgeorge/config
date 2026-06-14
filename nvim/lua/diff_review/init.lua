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
---@field system_stream_async? fun(command: DiffReviewGitCommand, input: string?, on_line: fun(line: string), cb: DiffReviewGitTextCallback)
---@field delete? fun(path: string): integer

---@class DiffReviewHunk
---@field file string
---@field filename? string
---@field section_name? string
---@field pos integer
---@field context? string
---@field context_text? string
---@field diff? string
---@field staged boolean
---@field added integer
---@field removed integer
---@field git_status? string
---@field git_original_file? string

---@class DiffReviewStatusFile
---@field filename string
---@field relpath string
---@field original_relpath? string
---@field section_name string
---@field added integer
---@field removed integer
---@field hunks DiffReviewHunk[]
---@field untracked boolean
---@field status string
---@field git_status? string

---@class DiffReviewStatusCommit
---@field oid string
---@field short_oid string
---@field branch? string
---@field subject string
---@field upstream? string
---@field files? DiffReviewStatusFile[]
---@field files_loaded? boolean
---@field files_loading? boolean
---@field files_error? string

---@class DiffReviewStatusSection
---@field name string
---@field title string
---@field default_folded boolean
---@field files DiffReviewStatusFile[]
---@field files_by_name table<string, DiffReviewStatusFile>
---@field commits? DiffReviewStatusCommit[]
---@field reviews? DiffReviewGhSubmittedReview[]
---@field upstream? string
---@field file_key_prefix? string
---@field file_entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@field hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"

---@class DiffReviewStatusEntry
---@field id? string
---@field kind "section"|"file"|"hunk"|"commit"|"commit_file"|"commit_hunk"|"pr_file"|"pr_hunk"|"pr_comment"|"pr_comment_reply"|"pr_review"|"pr_review_file"|"pr_review_hunk"|"review_comment"|"pr"|"about"
---@field section? DiffReviewStatusSection
---@field file? DiffReviewStatusFile
---@field hunk? DiffReviewHunk
---@field commit? DiffReviewStatusCommit
---@field pr_review? DiffReviewGhSubmittedReview
---@field pr_comment? DiffReviewGhPendingReviewComment|DiffReviewGhIssueComment
---@field pr_comment_reply? DiffReviewGhReviewCommentReply
---@field review_comment? table
---@field review_reply? table
---@field diff_line? table
---@field pr? DiffReviewGhPR
---@field about? DiffReviewAICommitState

---@alias DiffReviewStatusViewKind "status"|"pr"|"diff"

---@class DiffReviewStatusPRState
---@field state "fetching"|"ready"|"none"|"unavailable"|"error"
---@field pr? DiffReviewGhPR
---@field message? string
---@field lookup_started? boolean

---@class DiffReviewStatusRemoteActionState
---@field action "push"|"pull"
---@field state "running"

---@class DiffReviewStatusHeadLine
---@field segments table[]
---@field entry? DiffReviewStatusEntry

---@alias DiffReviewStatusSectionName "unstaged"|"staged"|"untracked"|"unmerged"|"recent"

---@class DiffReviewTreeSitterContextPending
---@field pending true
---@field callbacks table<string, fun(context?: DiffReviewHunkTreeSitterContext|string)>

---@class DiffReviewTreeSitterSyntax
---@field buf integer
---@field tree TSTree
---@field highlight_query vim.treesitter.Query?

---@class DiffReviewTreeSitterSyntaxPending
---@field pending true
---@field callbacks table<string, fun(syntax?: DiffReviewTreeSitterSyntax)>

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
---@field _main_status table?
---@field _status_states table<integer, table>?
---@field _untracked table<string, string>?
---@field _file_diffs table<string, string>?
---@field _file_hunk_staged table<string, boolean[]>?
---@field _diff_bufs table<string, integer>?
---@field _buf_hunks table<integer, DiffReviewHunk[]>?
---@field _buf_filename table<integer, string>?
---@field _buf_saved_cursor table<integer, integer[]>?
---@field _buf_last_rendered table<integer, string>?
---@field _ts_context_cache table<string, DiffReviewHunkTreeSitterContext|string|false|DiffReviewTreeSitterContextPending>?
---@field _ts_syntax_cache table<string, DiffReviewTreeSitterSyntax|false|DiffReviewTreeSitterSyntaxPending>?
---@field _ts_diff_syntax_cache table<string, DiffReviewTreeSitterSyntax|false|DiffReviewTreeSitterSyntaxPending>?
---@field _ts_source_bufs table<string, integer>?
---@field _main_win integer?
---@field _saved_wo table<integer, { number: boolean, relativenumber: boolean }>?
---@field suspend_preview boolean?

---@type DiffReviewModule
local M = {}
local status_render_current_model
local status_diff_hunks_for_file
local status_open_pr
M._comment_icon = "󰅺"
M._reply_icon = "↳"
M._pr_overview = {}

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

local config = require("diff_review.config")
local ai_commit = require("diff_review.ai_commit")
local gh = require("diff_review.gh")
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

---@param lines string[]
---@return boolean
local function lines_contain_nul(lines)
  for _, line in ipairs(lines) do
    if line:find("\0", 1, true) then return true end
  end
  return false
end

---@param filename string
---@return boolean
local function file_contains_nul(filename)
  local uv = vim.uv or vim.loop
  local fd = uv.fs_open(filename, "r", 438)
  if not fd then return false end

  local offset = 0
  local chunk_size = 65536
  while true do
    local data = uv.fs_read(fd, chunk_size, offset)
    if not data or data == "" then break end
    if data:find("\0", 1, true) then
      uv.fs_close(fd)
      return true
    end
    offset = offset + #data
    if #data < chunk_size then break end
  end

  uv.fs_close(fd)
  return false
end

---@param filename string
---@return integer?
local function treesitter_source_buffer(filename)
  local loaded = loaded_file_buffer(filename)
  if file_contains_nul(filename) then return nil end
  if loaded then
    if vim.bo[loaded].binary then return nil end
    return loaded
  end

  M._ts_source_bufs = M._ts_source_bufs or {}
  local cached = M._ts_source_bufs[filename]
  if cached and vim.api.nvim_buf_is_valid(cached) then return cached end

  local read_ok, lines = pcall(vim.fn.readfile, filename)
  if not read_ok or type(lines) ~= "table" then return nil end
  if lines_contain_nul(lines) then return nil end

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
  for _, cached in pairs(M._ts_diff_syntax_cache or {}) do
    if type(cached) == "table" and cached.buf and vim.api.nvim_buf_is_valid(cached.buf) then
      pcall(vim.api.nvim_buf_delete, cached.buf, { force = true })
    end
  end
  M._ts_source_bufs = {}
  M._ts_syntax_cache = {}
  M._ts_diff_syntax_cache = {}
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

---@return boolean
local function debug_notifications_enabled()
  local options = M.config or config.options or config.defaults
  return options.debug_notifications == true
end

---@param message string
---@param level? integer
---@param opts? table
local function notify_debug(message, level, opts)
  if not debug_notifications_enabled() then return end
  vim.notify(message, level or vim.log.levels.INFO, opts)
end

---@param backend DiffReviewGitBackend?
function M.set_git_backend(backend)
  M._git_backend = backend
end

function M.reset_git_backend()
  M._git_backend = nil
end

---@param stdout string?
---@param stderr string?
---@return string
function M._system_output(stdout, stderr)
  stdout = tostring(stdout or "")
  stderr = tostring(stderr or "")
  if stdout == "" then return stderr end
  if stderr == "" then return stdout end
  local separator = stdout:sub(-1) == "\n" and "" or "\n"
  return stdout .. separator .. stderr
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

  local ok, process = pcall(vim.system, command, {
    text = true,
    stdin = input,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      cb({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = M._system_output(stdout, stderr),
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

---@param data string|string[]?
---@return string
local function normalize_system_chunk(data)
  if type(data) == "table" then
    return table.concat(data, "\n")
  end
  return tostring(data or "")
end

---@param command DiffReviewGitCommand
---@param input? string
---@param on_line fun(line: string)
---@param cb DiffReviewGitTextCallback
local function system_text_stream_async(command, input, on_line, cb)
  local backend = M._git_backend
  if backend and backend.system_stream_async then
    backend.system_stream_async(command, input, on_line, cb)
    return
  end
  if backend and backend.system_async then
    backend.system_async(command, input, cb, on_line)
    return
  end
  if backend and backend.system then
    vim.schedule(function()
      local output, code = backend.system(command, input)
      local text = tostring(output or "")
      text = text:gsub("\r\n", "\n")
      if text:sub(-1) == "\n" then text = text:sub(1, -2) end
      local lines = text == "" and {} or vim.split(text, "\n", { plain = true })
      for _, line in ipairs(lines) do
        if line ~= "" then on_line(line) end
      end
      cb({
        code = code or 0,
        stdout = text,
        stderr = "",
        output = text,
      })
    end)
    return
  end

  local stdout = {}
  local stderr = {}
  local pending = { stdout = "", stderr = "" }

  ---@param stream "stdout"|"stderr"
  ---@param data string|string[]?
  local function collect(stream, data)
    local text = normalize_system_chunk(data)
    if text == "" then return end
    local chunks = stream == "stdout" and stdout or stderr
    chunks[#chunks + 1] = text

    text = pending[stream] .. text:gsub("\r", "\n")
    local parts = vim.split(text, "\n", { plain = true })
    pending[stream] = table.remove(parts) or ""
    for _, line in ipairs(parts) do
      line = vim.trim(line)
      if line ~= "" then vim.schedule(function() on_line(line) end) end
    end
  end

  local ok, process = pcall(vim.system, command, {
    text = true,
    stdin = input,
    stdout = function(_, data)
      collect("stdout", data)
    end,
    stderr = function(_, data)
      collect("stderr", data)
    end,
  }, function(result)
    vim.schedule(function()
      for _, stream in ipairs({ "stdout", "stderr" }) do
        local line = vim.trim(pending[stream] or "")
        if line ~= "" then on_line(line) end
      end
      local stdout_text = table.concat(stdout)
      local stderr_text = table.concat(stderr)
      cb({
        code = result.code or 0,
        stdout = stdout_text,
        stderr = stderr_text,
        output = M._system_output(stdout_text, stderr_text),
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
    cb(text_to_lines(result.stdout), result.code, result.output)
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
  systemlist_async({ "git", "rev-parse", "--show-toplevel" }, function(output, code, stderr)
    local root = output[1]
    if code ~= 0 or not root or root == "" then
      local message = vim.trim(stderr or "")
      cb(nil, message ~= "" and message or "Not a git repository")
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

---@param cwd string
---@param relpath string
---@return string
local function repo_file_path(cwd, relpath)
  local path = cwd:gsub("[/\\]+$", "") .. "/" .. relpath
  if package.config:sub(1, 1) ~= "\\" and cwd:match("^%a:[/\\]") then
    return path
  end
  return vim.fn.fnamemodify(path, ":p")
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
function M.stage_tracked_files_async(files, cb)
  run_file_batch_async(files, function(relpath)
    return { "add", "-u", "--", relpath }
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
function M.stage_tracked_files(files)
  return run_file_batch_sync_for_test_backend(files, function(relpath)
    return { "add", "-u", "--", relpath }
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
---@return string? original_file
local function parse_name_status_line(line)
  local parts = vim.split(line, "\t", { plain = true })
  local status = parts[1]
  local file = parts[2]
  local original_file = nil
  if status and (status:sub(1, 1) == "R" or status:sub(1, 1) == "C") then
    original_file = file
    file = parts[3] or file
  end
  if status and file then
    return status, file, original_file
  end
  status, file = line:match("^(%S+)%s+(.+)$")
  return status, file
end

---@param status string?
---@return boolean
local function git_status_is_added(status)
  return type(status) == "string" and status:sub(1, 1) == "A"
end

---@param status string?
---@return boolean
local function git_status_is_deleted(status)
  return type(status) == "string" and status:sub(1, 1) == "D"
end

---@param status string?
---@return boolean
local function git_status_is_renamed(status)
  return type(status) == "string" and status:sub(1, 1) == "R"
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
  local current_hunk_old_remaining = 0
  local current_hunk_new_remaining = 0
  local current_hunk_complete = false

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
    current_hunk_old_remaining = 0
    current_hunk_new_remaining = 0
    current_hunk_complete = false
  end

  ---@param line string
  ---@return integer? old_start
  ---@return integer old_count
  ---@return integer? new_start
  ---@return integer new_count
  ---@return string? context
  local function parse_hunk_header(line)
    local old_start, old_count, new_start, new_count, context = line:match("^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@ ?(.*)")
    return tonumber(old_start),
      old_count == "" and 1 or tonumber(old_count) or 1,
      tonumber(new_start),
      new_count == "" and 1 or tonumber(new_count) or 1,
      context
  end

  ---@param line string
  local function add_hunk_body_line(line)
    if current_hunk_complete then
      if line:find("^\\") then
        current_hunk_lines[#current_hunk_lines + 1] = line
      else
        flush_hunk()
      end
      return
    end

    local prefix = line:sub(1, 1)
    if not (prefix == " " or prefix == "+" or prefix == "-" or prefix == "\\") then
      flush_hunk()
      return
    end

    current_hunk_lines[#current_hunk_lines + 1] = line
    if prefix == " " then
      current_hunk_old_remaining = current_hunk_old_remaining - 1
      current_hunk_new_remaining = current_hunk_new_remaining - 1
    elseif prefix == "-" then
      current_hunk_old_remaining = current_hunk_old_remaining - 1
    elseif prefix == "+" then
      current_hunk_new_remaining = current_hunk_new_remaining - 1
    end
    current_hunk_complete = current_hunk_old_remaining <= 0 and current_hunk_new_remaining <= 0
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
      local _, old_count, new_start, new_count, context = parse_hunk_header(line)
      current_hunk_start = tonumber(new_start) or 1
      current_hunk_context = (context and context ~= "") and context or nil
      current_hunk_lines = { line }
      current_hunk_old_remaining = old_count
      current_hunk_new_remaining = new_count
      current_hunk_complete = current_hunk_old_remaining <= 0 and current_hunk_new_remaining <= 0
    elseif current_hunk_lines then
      add_hunk_body_line(line)
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
      if vim.fs.normalize(repo_file_path(cwd, hunk.file)) == norm then
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
  if file_contains_nul(filename) then return nil end

  local ok, lines = pcall(vim.fn.readfile, filename)
  if not ok or type(lines) ~= "table" or #lines == 0 then
    return nil
  end
  -- Skip binary files (a NUL byte in any line).
  if lines_contain_nul(lines) then return nil end
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
    vim.schedule(function()
      finish(parsed)
    end)
  end
end

---@param filename string
---@param cb fun(syntax?: DiffReviewTreeSitterSyntax)
function M.compute_file_syntax_async(filename, cb)
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

  local highlight_ok, highlight_query = pcall(vim.treesitter.query.get, lang, "highlights")
  if not highlight_ok then highlight_query = nil end

  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not parser_ok or not parser then
    cb(nil)
    return
  end

  local line_count = math.max(vim.api.nvim_buf_line_count(buf), 1)
  local done = false
  local function finish(trees)
    if done then return end
    done = true
    local tree = type(trees) == "table" and trees[1] or nil
    if not tree then
      cb(nil)
      return
    end
    cb({
      buf = buf,
      tree = tree,
      highlight_query = highlight_query,
    })
  end

  local parse_ok, parsed = pcall(function()
    return parser:parse({ 0, 0, line_count, 0 }, function(first, second)
      local trees = type(first) == "table" and first or second
      vim.schedule(function()
        finish(trees)
      end)
    end)
  end)
  if not parse_ok then
    cb(nil)
  elseif parsed then
    vim.schedule(function()
      finish(parsed)
    end)
  end
end

---@param filename string
---@param lines string[]
---@param cb fun(syntax?: DiffReviewTreeSitterSyntax)
function M.compute_diff_syntax_async(filename, lines, cb)
  if #lines == 0 then
    cb(nil)
    return
  end

  local ft = detect_filetype(filename, lines)
  local lang = vim.treesitter.language.get_lang(ft)
  if not lang then
    cb(nil)
    return
  end

  local highlight_ok, highlight_query = pcall(vim.treesitter.query.get, lang, "highlights")
  if not highlight_ok then highlight_query = nil end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = ft
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not parser_ok or not parser then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    cb(nil)
    return
  end

  local line_count = math.max(vim.api.nvim_buf_line_count(buf), 1)
  local done = false
  local function finish(trees)
    if done then return end
    done = true
    local tree = type(trees) == "table" and trees[1] or nil
    if not tree then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
      cb(nil)
      return
    end
    cb({
      buf = buf,
      tree = tree,
      highlight_query = highlight_query,
    })
  end

  local parse_ok, parsed = pcall(function()
    return parser:parse({ 0, 0, line_count, 0 }, function(first, second)
      local trees = type(first) == "table" and first or second
      vim.schedule(function()
        finish(trees)
      end)
    end)
  end)
  if not parse_ok then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    cb(nil)
  elseif parsed then
    vim.schedule(function()
      finish(parsed)
    end)
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

---@param filename string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax?
local function cached_file_syntax(filename, callback_key, on_update)
  M._ts_syntax_cache = M._ts_syntax_cache or {}
  local cached = M._ts_syntax_cache[filename]
  if cached == false then return nil end
  if type(cached) == "table" and not cached.pending then return cached end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil
  end

  M._ts_syntax_cache[filename] = {
    pending = true,
    callbacks = on_update and { [callback_key] = on_update } or {},
  }
  M.compute_file_syntax_async(filename, function(syntax)
    local pending = M._ts_syntax_cache and M._ts_syntax_cache[filename]
    local callbacks = type(pending) == "table" and pending.callbacks or {}
    M._ts_syntax_cache[filename] = syntax or false
    for _, callback in pairs(callbacks) do
      local ok, err = pcall(callback, syntax)
      if not ok then notify_error("Tree-sitter syntax update failed: " .. tostring(err)) end
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

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?
local function hunk_context_scope_key(context)
  if type(context) ~= "table" then return nil end
  return ("%s:%d:%d"):format(context.label, context.start_row, context.end_row)
end

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean
local function same_hunk_context_scope(left, right)
  local left_key = hunk_context_scope_key(left)
  return left_key ~= nil and left_key == hunk_context_scope_key(right)
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

---@class DiffReviewGutterSpec
---@field width integer
---@field old_width integer
---@field new_width integer

---@return DiffReviewGutterSpec
local function default_hunk_gutter_spec()
  return { width = 12, old_width = 3, new_width = 3 }
end

---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
---@return table[]
local function hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl)
  gutter = gutter or default_hunk_gutter_spec()
  local old_text = old_line and ("%" .. tostring(gutter.old_width) .. "d"):format(old_line) or string.rep(" ", gutter.old_width)
  local new_text = new_line and ("%" .. tostring(gutter.new_width) .. "d"):format(new_line) or string.rep(" ", gutter.new_width)
  local chunks = {}
  chunks[#chunks + 1] = { old_text, old_line and (sign == "-" and "DiffReviewDeleteLineNr" or "DiffReviewContextLineNr") or line_hl }
  chunks[#chunks + 1] = { "  ", line_hl }
  chunks[#chunks + 1] = { new_text, new_line and (sign == "+" and "DiffReviewAddLineNr" or "DiffReviewContextLineNr") or line_hl }
  chunks[#chunks + 1] = { "  ", line_hl }
  chunks[#chunks + 1] = { sign or " ", sign_hl or line_hl }
  chunks[#chunks + 1] = { " ", line_hl }
  return chunks
end

---@param row table
---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
local function hunk_add_gutter(row, gutter, old_line, new_line, sign, sign_hl, line_hl)
  -- Inline virtual text, not buffer text: visual selection, yank, and search
  -- then operate on the code content only. The default hl_mode "replace"
  -- keeps the Visual highlight from bleeding into the gutter; the row's
  -- line background is carried explicitly on every chunk via line_hl.
  row[#row + 1] = {
    col = 0,
    virt_text = hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl),
    virt_text_pos = "inline",
  }
end

---@param text string
---@param segments? DiffReviewHighlightSegment[]
---@param line_number? integer
---@param gutter? DiffReviewGutterSpec
---@return table
local function hunk_boundary_row(text, segments, line_number, gutter)
  local row = { diff_review_boundary = true }
  gutter = gutter or default_hunk_gutter_spec()
  hunk_add_gutter(row, gutter, line_number, line_number, " ", nil)
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
---@param gutter? DiffReviewGutterSpec
---@return table
local function hunk_boundary_ellipsis_row(reference_text, gutter)
  return hunk_boundary_row(line_indent(reference_text) .. "...", nil, nil, gutter)
end

---@param header_parts table[]
---@return table
local function hunk_header_row(header_parts)
  local row = { diff_review_hunk_header = true }
  for _, part in ipairs(header_parts) do
    row[#row + 1] = part
  end
  return row
end

-- Cache for treesitter context per file (cleared on refresh)
M._ts_context_cache = {}
M._ts_source_bufs = {}

---@param win integer?
function M._hide_line_numbers(win)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  M._saved_wo = M._saved_wo or {}
  if not M._saved_wo[win] then
    M._saved_wo[win] = {
      number = vim.wo[win].number,
      relativenumber = vim.wo[win].relativenumber,
      virtualedit = vim.wo[win].virtualedit,
      wrap = vim.wo[win].wrap,
      linebreak = vim.wo[win].linebreak,
      breakindent = vim.wo[win].breakindent,
    }
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].virtualedit = "all"
end

---@param win integer?
---@param state? table
function M._apply_status_window_options(win, state)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  M._hide_line_numbers(win)
  local soft_wrap = state ~= nil and state.view_kind == "review"
  vim.wo[win].wrap = soft_wrap
  vim.wo[win].linebreak = soft_wrap
  vim.wo[win].breakindent = soft_wrap
end

---@param win integer?
function M._restore_line_numbers(win)
  if not (win and vim.api.nvim_win_is_valid(win) and M._saved_wo and M._saved_wo[win]) then return end
  local saved = M._saved_wo[win]
  vim.wo[win].number = saved.number
  vim.wo[win].relativenumber = saved.relativenumber
  vim.wo[win].virtualedit = saved.virtualedit or ""
  vim.wo[win].wrap = saved.wrap
  vim.wo[win].linebreak = saved.linebreak
  vim.wo[win].breakindent = saved.breakindent
  M._saved_wo[win] = nil
end

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
  M._empty_diff_rows = {}
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
  local staged_original_by_file = {}
  if results.staged_name_status_code == 0 then
    for _, line in ipairs(results.staged_name_status) do
      local status, file, original_file = parse_name_status_line(line)
      if status and file then
        staged_status_by_file[file] = status
        staged_original_by_file[file] = original_file
      end
    end
  end

  local unstaged_status_by_file = {}
  local unstaged_original_by_file = {}
  if results.unstaged_name_status_code == 0 then
    for _, line in ipairs(results.unstaged_name_status) do
      local status, file, original_file = parse_name_status_line(line)
      if status and file then
        unstaged_status_by_file[file] = status
        unstaged_original_by_file[file] = original_file
      end
    end
  end

  local tracked_files_with_hunks = {}
  for _, h in ipairs(all_hunks) do
    tracked_files_with_hunks[h.file] = true
    h.git_status = h.staged and staged_status_by_file[h.file] or unstaged_status_by_file[h.file]
    h.git_original_file = h.staged and staged_original_by_file[h.file] or unstaged_original_by_file[h.file]
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
          git_original_file = staged_original_by_file[file],
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
          git_original_file = unstaged_original_by_file[file],
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
    local filename = repo_file_path(cwd, hunk.file)
    -- Use treesitter scope context if available, fall back to git's @@ context
    local context_text = hunk.context or ""
    if hunk.diff and not (_ctx and _ctx.skip_ts_context) then
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
        git_original_file = hunk.git_original_file,
      },
    })
  end

  -- Add untracked files
  for _, f in ipairs(untracked_files) do
    local filename = repo_file_path(cwd, f)
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
    local filename = repo_file_path(cwd, f)
    M._file_diffs[filename] = #diffs > 0 and table.concat(diffs, "\n") or false
    M._file_hunk_staged[filename] = #flags > 0 and flags or nil
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

-- Namespace for diff preview/status rendering
M._ns = vim.api.nvim_create_namespace("diff_review_preview")
M._empty_diff_rows = {}

---@param virt_text table[]?
---@return integer
function M._inline_virtual_text_width(virt_text)
  local width = 0
  for _, chunk in ipairs(virt_text or {}) do
    width = width + vim.fn.strdisplaywidth(chunk[1] or "")
  end
  return width
end

---@class DiffReviewGutterCursorBounds
---@field line string
---@field gutter_col integer 0-based buffer column where the inline gutter starts
---@field gutter_width integer virtual columns occupied by the inline gutter

---@param buf integer
---@param row integer 1-based
---@param namespace integer
---@return DiffReviewGutterCursorBounds?
function M._diff_gutter_cursor_bounds(buf, row, namespace)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  if line == nil then return nil end
  local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local col = mark[3] or 0
    local details = mark[4] or {}
    if details.virt_text and details.virt_text_pos == "inline" and col <= #line then
      local width = M._inline_virtual_text_width(details.virt_text)
      if width > 0 then
        return {
          line = line,
          gutter_col = col,
          gutter_width = width,
        }
      end
    end
  end
  return nil
end

---@param buf integer
---@param namespace integer
---@return boolean handled
function M._normalize_diff_gutter_cursor(buf, namespace)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
  if not bounds then return false end

  local pos = vim.fn.getcurpos()
  local current_col = math.max((pos[3] or 1) - 1, 0)
  local current_coladd = pos[4] or 0
  local line_length = #bounds.line
  local target_col = current_col
  local target_coladd = current_coladd

  if line_length <= bounds.gutter_col then
    target_col = bounds.gutter_col
    target_coladd = bounds.gutter_width
  else
    local last_text_col = line_length - 1
    if current_col <= bounds.gutter_col then
      target_col = bounds.gutter_col
      target_coladd = bounds.gutter_width
    elseif current_col > last_text_col then
      target_col = last_text_col
      target_coladd = 0
    else
      target_col = current_col
      target_coladd = 0
    end
  end

  if not (pos[2] == row and current_col == target_col and current_coladd == target_coladd) then
    vim.fn.setpos(".", { 0, row, target_col + 1, target_coladd })
  end
  return true
end

---@param buf integer
---@return boolean handled
function M._align_diff_cursor(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if status then
    return M._normalize_diff_gutter_cursor(buf, M._status_ns)
  end
  return M._normalize_diff_gutter_cursor(buf, M._ns)
end

---@param buf integer
---@return integer? row
function M._clamp_buffer_text_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  if line == nil then return row end
  local pos = vim.fn.getcurpos()
  local current_col = math.max((pos[3] or 1) - 1, 0)
  local current_coladd = pos[4] or 0
  local target_col = math.min(current_col, #line)
  if current_col ~= target_col or current_coladd ~= 0 then
    vim.fn.setpos(".", { 0, row, target_col + 1, 0 })
  end
  return row
end

---@param buf integer
---@return integer? row
function M._normalize_status_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  M._cursor_normalizing = M._cursor_normalizing or {}
  if M._cursor_normalizing[buf] then return vim.api.nvim_win_get_cursor(0)[1] end
  M._cursor_normalizing[buf] = true
  local handled = M._align_diff_cursor(buf)
  if not handled then M._clamp_buffer_text_cursor(buf) end
  M._cursor_normalizing[buf] = nil
  return vim.api.nvim_win_get_cursor(0)[1]
end

---@param buf integer
function M._align_empty_diff_cursor(buf)
  M._normalize_status_cursor(buf)
end

---@class DiffReviewParsedHunkLine
---@field prefix string
---@field code string
---@field old_line? integer
---@field new_line? integer
---@field position integer

---@class DiffReviewParsedHunk
---@field header string
---@field old_start integer
---@field old_count integer
---@field new_start integer
---@field new_count integer
---@field context string
---@field diff string[]
---@field body string[]
---@field lines DiffReviewParsedHunkLine[]
---@field added integer
---@field removed integer
---@field gutter DiffReviewGutterSpec

---@class DiffReviewParsedBlock
---@field file string
---@field hunks DiffReviewParsedHunk[]

---@param range string
---@return integer start
---@return integer count
local function parse_hunk_range(range)
  local start_text, count_text = range:match("^(%d+),?(%d*)$")
  local start = tonumber(start_text) or 0
  local count = count_text ~= "" and tonumber(count_text) or 1
  return start, count or 1
end

---@param header string
---@return integer old_start
---@return integer old_count
---@return integer new_start
---@return integer new_count
---@return string context
local function parse_hunk_header(header)
  local old_range, new_range, context = header:match("^@@ %-(%d+,?%d*) %+(%d+,?%d*) @@%s?(.*)$")
  local old_start, old_count = parse_hunk_range(old_range or "0,0")
  local new_start, new_count = parse_hunk_range(new_range or "0,0")
  return old_start, old_count, new_start, new_count, context or ""
end

---@param path string
---@return string
local function diff_path_without_prefix(path)
  if path == "/dev/null" then return path end
  return (path:gsub("^[ab]/", ""))
end

---@param diff_text string
---@return DiffReviewParsedBlock[]
local function parse_unified_diff(diff_text)
  local blocks = {} ---@type DiffReviewParsedBlock[]
  local current_block = nil ---@type DiffReviewParsedBlock?
  local current_hunk = nil ---@type DiffReviewParsedHunk?

  for _, line in ipairs(vim.split(diff_text or "", "\n", { plain = true })) do
    local left_path, right_path = line:match("^diff %-%-git a/(.-) b/(.+)$")
    if left_path or right_path then
      current_block = { file = right_path or left_path or "", hunks = {} }
      blocks[#blocks + 1] = current_block
      current_hunk = nil
    elseif line:match("^%+%+%+ ") then
      local path = line:match("^%+%+%+%s+(.+)$")
      if path and path ~= "/dev/null" then
        if not current_block then
          current_block = { file = diff_path_without_prefix(path), hunks = {} }
          blocks[#blocks + 1] = current_block
        else
          current_block.file = diff_path_without_prefix(path)
        end
      end
    elseif line:match("^@@ ") then
      if not current_block then
        current_block = { file = "", hunks = {} }
        blocks[#blocks + 1] = current_block
      end
      local old_start, old_count, new_start, new_count, context = parse_hunk_header(line)
      current_hunk = {
        header = line,
        old_start = old_start,
        old_count = old_count,
        new_start = new_start,
        new_count = new_count,
        context = context,
        diff = { line },
        body = {},
        lines = {},
        added = 0,
        removed = 0,
        gutter = default_hunk_gutter_spec(),
      }
      current_block.hunks[#current_block.hunks + 1] = current_hunk
    elseif current_hunk then
      current_hunk.diff[#current_hunk.diff + 1] = line
      current_hunk.body[#current_hunk.body + 1] = line
    end
  end

  return blocks
end

---@param value? integer
---@return integer
local function line_number_width(value)
  return math.max(3, #tostring(value or 0))
end

---@param hunk DiffReviewParsedHunk
---@return DiffReviewParsedHunk
local function parse_hunk_body(hunk)
  while #hunk.body > 0 and hunk.body[#hunk.body]:match("^%s*$") do
    hunk.body[#hunk.body] = nil
  end

  local old_line = hunk.old_start
  local new_line = hunk.new_start
  local max_old = math.max(hunk.old_start, hunk.old_start + math.max(hunk.old_count - 1, 0))
  local max_new = math.max(hunk.new_start, hunk.new_start + math.max(hunk.new_count - 1, 0))

  for position, diff_line in ipairs(hunk.body) do
    local prefix = diff_line:sub(1, 1)
    local code = diff_line:sub(2)
    if prefix == " " then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, old_line = old_line, new_line = new_line, position = position }
      max_old = math.max(max_old, old_line)
      max_new = math.max(max_new, new_line)
      old_line = old_line + 1
      new_line = new_line + 1
    elseif prefix == "-" then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, old_line = old_line, position = position }
      hunk.removed = hunk.removed + 1
      max_old = math.max(max_old, old_line)
      old_line = old_line + 1
    elseif prefix == "+" then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, new_line = new_line, position = position }
      hunk.added = hunk.added + 1
      max_new = math.max(max_new, new_line)
      new_line = new_line + 1
    end
  end

  hunk.gutter = {
    old_width = line_number_width(max_old),
    new_width = line_number_width(max_new),
    width = line_number_width(max_old) + 2 + line_number_width(max_new) + 2 + 1 + 1,
  }
  return hunk
end

---@param hunk DiffReviewParsedHunk
---@return integer
local function hunk_first_changed_current_line(hunk)
  local current_line = hunk.new_start
  for _, parsed_line in ipairs(hunk.lines) do
    if parsed_line.prefix == " " and parsed_line.new_line then
      current_line = parsed_line.new_line + 1
    elseif parsed_line.prefix == "+" and parsed_line.new_line then
      return parsed_line.new_line
    elseif parsed_line.prefix == "-" then
      return current_line
    end
  end
  return hunk.new_start
end

---@param parsed_line DiffReviewParsedHunkLine
---@param context DiffReviewHunkTreeSitterContext|string?
---@return boolean
local function hunk_line_visible_in_context_scope(parsed_line, context)
  if parsed_line.prefix ~= " " or type(context) ~= "table" then return true end
  if not parsed_line.new_line then return true end
  local scope_start = context.start_row + 1
  local scope_end = context.end_row + 1
  return parsed_line.new_line >= scope_start and parsed_line.new_line <= scope_end
end

---@param diff_text string
---@return string[]
local function diff_syntax_source_lines(diff_text)
  local lines = {}
  for _, block in ipairs(parse_unified_diff(diff_text)) do
    for _, hunk in ipairs(block.hunks) do
      local parsed_hunk = parse_hunk_body(hunk)
      for _, parsed_line in ipairs(parsed_hunk.lines) do
        lines[#lines + 1] = parsed_line.code
      end
    end
  end
  return lines
end

---@param filename string
---@param diff_text string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending
local function cached_diff_syntax(filename, diff_text, callback_key, on_update)
  M._ts_diff_syntax_cache = M._ts_diff_syntax_cache or {}
  local cache_key = filename .. ":" .. vim.fn.sha256(diff_text or "")
  local cached = M._ts_diff_syntax_cache[cache_key]
  if cached == false then return nil, false end
  if type(cached) == "table" and not cached.pending then return cached, false end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil, true
  end

  local lines = diff_syntax_source_lines(diff_text)
  M._ts_diff_syntax_cache[cache_key] = {
    pending = true,
    callbacks = on_update and { [callback_key] = on_update } or {},
  }
  M.compute_diff_syntax_async(filename, lines, function(syntax)
    local pending = M._ts_diff_syntax_cache and M._ts_diff_syntax_cache[cache_key]
    local callbacks = type(pending) == "table" and pending.callbacks or {}
    M._ts_diff_syntax_cache[cache_key] = syntax or false
    for _, callback in pairs(callbacks) do
      local ok, err = pcall(callback, syntax)
      if not ok then notify_error("Tree-sitter diff syntax update failed: " .. tostring(err)) end
    end
    if syntax and status_render_current_model and M._status and M._status.buf then
      vim.schedule(function()
        if M._status and M._status.buf and vim.api.nvim_buf_is_valid(M._status.buf) then
          status_render_current_model()
        end
      end)
    end
  end)
  return nil, true
end

---@param file DiffReviewStatusFile?
---@param callback_key_prefix string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
local function prewarm_file_diff_syntax(file, callback_key_prefix, on_update)
  if not (file and file.filename) then return end
  local hunks = status_diff_hunks_for_file(file)
  for hunk_index, hunk in ipairs(hunks) do
    if hunk.diff and hunk.diff ~= "" then
      cached_diff_syntax(
        file.filename,
        hunk.diff,
        ("%s:%s:%d"):format(callback_key_prefix, file.filename, hunk_index),
        on_update
      )
    end
  end
end

---@param parsed_line DiffReviewParsedHunkLine
---@param gutter DiffReviewGutterSpec
---@param file string
---@param syntax? DiffReviewTreeSitterSyntax
---@param syntax_row? integer
---@return table
local function hunk_body_row(parsed_line, gutter, file, syntax, syntax_row)
  local sign_hl = nil
  local line_hl = nil
  if parsed_line.prefix == "+" then
    sign_hl = "DiffReviewAddLineNr"
    line_hl = "DiffReviewAddBg"
  elseif parsed_line.prefix == "-" then
    sign_hl = "DiffReviewDeleteLineNr"
    line_hl = "DiffReviewDeleteBg"
  end

  local row = { diff_review_line_hl = line_hl }
  local diff_meta = {
    diff = {
      side = parsed_line.new_line and "right" or "left",
      file = file,
      line = parsed_line.new_line or parsed_line.old_line,
      position = parsed_line.position,
      code = parsed_line.code,
    },
  }
  row[#row + 1] = { "", nil, meta = diff_meta }
  hunk_add_gutter(row, gutter, parsed_line.old_line, parsed_line.new_line, parsed_line.prefix, sign_hl, line_hl)
  local segments = nil
  if syntax and syntax_row then
    segments = treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, syntax_row, parsed_line.code)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { parsed_line.code }
  end
  return row
end

---@param diff_text string
---@param hunk_staged? boolean[]
---@param filename? string
---@param context_callback_key? fun(hunk_line: number): string
---@param on_context_update? fun()
---@param opts? { context_line: integer?, boundary_context: boolean?, suppress_start_boundary: boolean?, suppress_end_boundary: boolean? }
local function build_fancy_diff_rows(diff_text, hunk_staged, filename, context_callback_key, on_context_update, opts)
  opts = opts or {}

  local diff = parse_unified_diff(diff_text)
  local ret = {} ---@type table[]
  local hunk_idx = 0
  local syntax = nil
  local syntax_pending = false
  local syntax_row = 0
  if filename then
    local syntax_callback_key = context_callback_key and context_callback_key(0) or ("diff-row:" .. filename .. ":0")
    syntax, syntax_pending = cached_diff_syntax(filename, diff_text, syntax_callback_key .. ":diff-syntax", on_context_update)
  end

  for _, block in ipairs(diff) do
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      hunk = parse_hunk_body(hunk)
      local hunk_line = opts.context_line or hunk_first_changed_current_line(hunk)
      local callback_key = context_callback_key and context_callback_key(hunk_line)
        or ("diff-row:" .. (filename or block.file) .. ":" .. hunk_line)
      local raw_context = nil
      local ts_context = nil
      if filename then
        raw_context = cached_hunk_context(filename, hunk_line, callback_key, on_context_update)
        ts_context = hunk_context_label(raw_context)
      end

      local header_parts = {
        { "@@ ", "DiffReviewHunkHeader" },
      }
      if ts_context then
        header_parts[#header_parts + 1] = { ts_context, "DiffReviewHunkContext" }
        header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      end
      header_parts[#header_parts + 1] = { ("+%d"):format(hunk.added), "DiffReviewAddRange" }
      header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      header_parts[#header_parts + 1] = { ("-%d"):format(hunk.removed), "DiffReviewDeleteRange" }

      local visible_hunk_lines = hunk_visible_source_lines(hunk.diff)
      local gutter = hunk.gutter
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local start_text = raw_context.start_text or ""
        if not opts.suppress_start_boundary and not visible_hunk_lines[start_text] then
          ret[#ret + 1] = hunk_boundary_row(raw_context.start_text, raw_context.start_segments, node_start, gutter)
          if node_start ~= raw_context.end_row + 1 then
            ret[#ret + 1] = hunk_boundary_ellipsis_row(start_text, gutter)
          end
        end
      end
      ret[#ret + 1] = hunk_header_row(header_parts)
      for _, parsed_line in ipairs(hunk.lines) do
        if hunk_line_visible_in_context_scope(parsed_line, raw_context) then
          ret[#ret + 1] = hunk_body_row(parsed_line, gutter, filename or block.file, syntax, syntax_row)
        end
        syntax_row = syntax_row + 1
      end
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local node_end = raw_context.end_row + 1
        local end_text = raw_context.end_text or ""
        if not opts.suppress_end_boundary and not visible_hunk_lines[end_text] then
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_ellipsis_row(end_text, gutter)
          end
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_row(end_text, raw_context.end_segments, node_end, gutter)
          end
        end
      end
    end
  end

  ret.diff_review_syntax_pending = syntax_pending
  return ret
end

---@param buf integer
---@param ns integer
---@param rows table[]
local function render_highlight_rows(buf, ns, rows)
  local lines = {}
  local highlights = {}
  local line_highlights = {}
  local extmarks = {}
  local empty_diff_rows = {}

  for row_index, row in ipairs(rows) do
    local line_parts = {}
    local col = 0
    local empty_diff_row = false
    for _, chunk in ipairs(row) do
      if chunk.meta and chunk.meta.diff and chunk.meta.diff.code == "" then
        empty_diff_row = true
      end
      if type(chunk[1]) == "string" then
        local text = chunk[1]
        line_parts[#line_parts + 1] = text
        if chunk[2] and text ~= "" then
          highlights[#highlights + 1] = {
            line = row_index,
            start_col = col,
            end_col = col + #text,
            hl_group = chunk[2],
          }
        end
        col = col + #text
      elseif chunk.virt_text then
        local opts = {}
        for key, value in pairs(chunk) do
          if key ~= "col" then opts[key] = value end
        end
        extmarks[#extmarks + 1] = {
          line = row_index,
          col = chunk.col or 0,
          opts = opts,
        }
      end
    end
    lines[row_index] = table.concat(line_parts)
    if row.diff_review_line_hl then
      line_highlights[#line_highlights + 1] = {
        line = row_index,
        hl_group = row.diff_review_line_hl,
      }
    end
    if empty_diff_row then empty_diff_rows[row_index] = true end
  end

  M._empty_diff_rows = M._empty_diff_rows or {}
  M._empty_diff_rows[buf] = empty_diff_rows
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  for _, line_hl in ipairs(line_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, line_hl.line - 1, 0, {
      line_hl_group = line_hl.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  for _, extmark in ipairs(extmarks) do
    local opts = vim.tbl_extend("force", { priority = 95 }, extmark.opts)
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, extmark.line - 1, extmark.col, opts)
  end
end

--- Render a formatted diff into a buffer using DiffReview's local formatter.
---@param buf number
---@param diff_text string
---@param hunk_staged? boolean[] staged status per hunk (in order)
---@param filename? string
local function render_fancy_diff(buf, diff_text, hunk_staged, filename)
  local ft = filename and detect_filetype(filename) or ""
  if ft ~= "" and vim.bo[buf].filetype ~= ft then
    vim.bo[buf].filetype = ft
  end

  render_highlight_rows(buf, M._ns, build_fancy_diff_rows(
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
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = buf,
      callback = function()
        M._normalize_status_cursor(buf)
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
      -- Adjust column for the rendered diff gutter.
      -- The gutter (line numbers + prefix) is stored as real buffer text.
      -- The padding width = all leading spaces on a code line.
      -- To distinguish gutter padding from code indentation, check a line
      -- that has actual code (non-space after the gutter). The gutter width
      -- is the same for all lines in a hunk, so find it from any code line.
      -- Compute gutter width from the line number + prefix columns. The width can be computed from
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
        notify_debug("Hunk staged")
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
        notify_debug("Hunk unstaged")
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
          notify_debug("Hunk " .. i .. " folded=" .. tostring(h.folded) .. " range=" .. h.start_line .. "-" .. h.end_line)
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
--- 1 line for @@ separator + N code lines.
--- N = number of lines in hunk.diff AFTER the @@ header, EXCLUDING
--- the file header lines (diff --git, index, ---, +++).
---@param diff_text string
---@return table[]
function M._compute_hunk_map(diff_text)
  local raw_hunks = parse_diff(diff_text, false)
  local rendered_line = 0
  local hunk_map = {}
  for _, h in ipairs(raw_hunks) do
    -- Count code lines the same way the local formatter does: lines after @@,
    -- stripping trailing empty/whitespace lines.
    local code_lines_list = {}
    local found_hunk_header = false
    for diff_line in h.diff:gmatch("[^\n]+") do
      if found_hunk_header then
        code_lines_list[#code_lines_list + 1] = diff_line
      elseif diff_line:match("^@@") then
        found_hunk_header = true
      end
    end
    -- Strip trailing empty lines to match parse_hunk_body().
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
        line_hl_group = "DiffReviewHunkHeader",
        priority = M._hunk_header_priority,
      })
    end
    M._render_with_folds(buf)
  else
    vim.bo[buf].modifiable = true
    local message = diff_text == false and "No textual diff" or "No changes"
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { message })
    vim.bo[buf].modifiable = false
    M._buf_hunks[buf] = {}
    if M._empty_diff_rows then M._empty_diff_rows[buf] = nil end
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
local status_reconcile_delay_ms = 120

---@class DiffReviewStatusCommandSpec
---@field id string
---@field label string
---@field desc string
---@field modes string|string[]
---@field keymap? "status"|"review"
---@field visual? boolean
---@field pinned boolean
---@field views? table<DiffReviewStatusViewKind, boolean>

---@type DiffReviewStatusCommandSpec[]
local status_command_specs = {
  { id = "toggle", label = "toggle", desc = "Toggle fold", modes = "n", pinned = true, views = { status = true, pr = true, diff = true, review = true } },
  { id = "collapse_parent", label = "Collapse Parent", desc = "Collapse Parent", modes = "n", pinned = true, views = { status = true, pr = true, diff = true, review = true } },
  { id = "stage", label = "stage", desc = "Stage hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "unstage", label = "unstage", desc = "Unstage hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "discard", label = "discard", desc = "Discard hunk/file/selection", modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },
  { id = "commit", label = "commit", desc = "Commit", modes = "n", pinned = true, views = { status = true } },
  { id = "push", label = "push", desc = "Push", modes = "n", pinned = true, views = { status = true } },
  { id = "pull", label = "pull", desc = "Pull", modes = "n", pinned = true, views = { status = true } },
  { id = "pr", label = "pr", desc = "Open pull request", modes = "n", pinned = true, views = { status = true } },
  { id = "branch_create", label = "branch", desc = "Create a branch", modes = "n", pinned = false, views = { status = true } },
  { id = "walkthrough", label = "walkthrough", desc = "Review walkthrough", modes = "n", pinned = false, views = { status = true } },
  { id = "review", label = "review", desc = "Start PR review", modes = "n", pinned = true, views = { status = true, pr = true } },
  { id = "browse", label = "browse", desc = "Browse pull request", modes = { "n", "x" }, visual = true, pinned = true, views = { pr = true, review = true } },
  { id = "open", label = "open", desc = "Open PR/about or jump to file", modes = "n", pinned = true },
  { id = "refresh", label = "refresh", desc = "Refresh DiffReview", modes = "n", pinned = true },
  { id = "close", label = "close", desc = "Close DiffReview", modes = "n", pinned = true },
  { id = "help", label = "help", desc = "Show help", modes = "n", pinned = true },
  { id = "viewed", label = "viewed", desc = "Mark file as viewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "unviewed", label = "unviewed", desc = "Move file back to unviewed", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "comment", label = "comment", desc = "Add or edit an inline comment", modes = { "n", "x" }, keymap = "review", visual = true, pinned = true, views = { pr = true, review = true } },
  { id = "delete", label = "delete", desc = "Delete draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "next_comment", label = "next", desc = "Jump to next draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "prev_comment", label = "prev", desc = "Jump to previous draft comment", modes = "n", keymap = "review", pinned = true, views = { review = true } },
  { id = "sync", label = "sync", desc = "Sync inline comments to GitHub", modes = { "n", "i" }, keymap = "review", pinned = true, views = { pr = true, review = true } },
  { id = "submit", label = "submit", desc = "Submit review to GitHub", modes = "n", keymap = "review", pinned = true, views = { review = true } },
}

M._status_hint_command_ids_by_view = {
  status = {
    "stage",
    "unstage",
    "discard",
    "commit",
    "open",
    "refresh",
    "close",
    "help",
  },
  pr = {
    "browse",
    "review",
    "comment",
    "open",
    "refresh",
    "close",
    "help",
  },
  review = {
    "toggle",
    "collapse_parent",
    "viewed",
    "unviewed",
    "comment",
    "delete",
    "sync",
    "submit",
    "open",
    "close",
    "help",
  },
  diff = {
    "open",
    "refresh",
    "close",
    "help",
  },
}

---@type table<string, DiffReviewStatusCommandSpec>
local status_command_specs_by_id = {}
for _, spec in ipairs(status_command_specs) do
  status_command_specs_by_id[spec.id] = spec
end

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

function M._refresh_status_windows_after_resize()
  if not M._status_states then return end
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(win) then
      local buf = vim.api.nvim_win_get_buf(win)
      local state = M._status_states[buf]
      if state then
        M._apply_status_window_options(win, state)
        M._status_apply_hint_bar(buf, win)
        if state.view_kind == "review" and M._review and M._review.refresh_inline_comment_rules then
          local width = M._review.comment_rule_width(win, buf)
          if state.review_comment_rule_width ~= width then
            state.review_comment_rule_width = width
            M._review.refresh_inline_comment_rules(buf, win)
          end
        end
      end
    end
  end
end

function M._ensure_status_resize_autocmd()
  if M._status_resize_autocmd then return end
  local group = vim.api.nvim_create_augroup("DiffReviewStatusResize", { clear = true })
  M._status_resize_autocmd = vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, {
    group = group,
    callback = function()
      M._refresh_status_windows_after_resize()
    end,
  })
end

---@param buf integer
---@param state table
local function attach_status_state(buf, state)
  M._ensure_status_resize_autocmd()
  M._status_states = M._status_states or {}
  M._status_states[buf] = state
  vim.api.nvim_create_autocmd("BufEnter", {
    buffer = buf,
    callback = function()
      local current = M._status_states and M._status_states[buf] or nil
      if current then M._status = current end
      M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
      M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    buffer = buf,
    callback = function()
      local current = M._status_states and M._status_states[buf] or nil
      if current then M._status = current end
      M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
      if current and current.view_kind == "review" and M._review and M._review.refresh_inline_comment_rules then
        M._review.refresh_inline_comment_rules(buf, vim.api.nvim_get_current_win())
      end
      M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      M._normalize_status_cursor(buf)
    end,
  })
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    callback = function()
      M._restore_line_numbers(vim.api.nvim_get_current_win())
      M._status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", {
    buffer = buf,
    callback = function()
      M._status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buf,
    callback = function()
      if M._status_states then M._status_states[buf] = nil end
      if M._main_status == state then M._main_status = nil end
      if M._status == state then M._status = M._main_status end
    end,
  })
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

---@param pr_state DiffReviewStatusPRState?
---@return DiffReviewStatusHeadLine
local function status_pr_head_line(pr_state)
  local segments = {
    { ("%-8s"):format("PR:"), "DiffReviewStatusLabel" },
  }
  local entry = { id = "pr", kind = "pr" }

  if not pr_state or pr_state.state == "fetching" then
    segments[#segments + 1] = { "...fetching...", "DiffReviewStatusFetching" }
  elseif pr_state.state == "ready" and pr_state.pr then
    entry.pr = pr_state.pr
    segments[#segments + 1] = { pr_state.pr.title ~= "" and pr_state.pr.title or ("PR #" .. tostring(pr_state.pr.number)), "DiffReviewStatusPR" }
  elseif pr_state.state == "error" then
    segments[#segments + 1] = { "error", "ErrorMsg" }
  elseif pr_state.state == "unavailable" then
    segments[#segments + 1] = { "unavailable", "Comment" }
  else
    segments[#segments + 1] = { "none", "Comment" }
  end

  return { segments = segments, entry = entry }
end

---@param about_state DiffReviewAICommitState?
---@return DiffReviewStatusHeadLine
local function status_about_head_line(about_state)
  local segments = {
    { ("%-8s"):format("About:"), "DiffReviewStatusLabel" },
  }
  local entry = { id = "about", kind = "about", about = about_state }

  if not about_state or about_state.state == "none" then
    segments[#segments + 1] = { "none", "Comment" }
  elseif about_state.state == "generating" then
    segments[#segments + 1] = { "...generating...", "DiffReviewStatusFetching" }
  elseif about_state.state == "ready" and about_state.message then
    entry.about = about_state
    segments[#segments + 1] = { ai_commit.subject(about_state.message), "DiffReviewStatusPath" }
  elseif about_state.state == "error" then
    segments[#segments + 1] = { "error", "ErrorMsg" }
  else
    segments[#segments + 1] = { "none", "Comment" }
  end

  return { segments = segments, entry = entry }
end

---@param buf integer
---@return boolean
function M._status_patch_about_line(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not status then return false end
  return M._status_patch_head_line(buf, "about", status_about_head_line(status.about))
end

---@param values table
---@param pr_state DiffReviewStatusPRState?
---@param about_state DiffReviewAICommitState?
---@return DiffReviewStatusHeadLine[]
local function status_build_head_lines(values, pr_state, about_state)
  local remote_action = M._status and M._status.remote_action
  local lines = {}
  lines[#lines + 1] = {
    segments = status_head_row(
      "Head",
      values.head_oid or "0000000",
      values.branch or "(detached)",
      "DiffReviewStatusBranch",
      values.subject or "(no commits)"
    ),
  }

  if values.upstream then
    lines[#lines + 1] = {
      segments = status_head_row(
        "Merge",
        values.upstream_oid or "",
        values.upstream,
        "DiffReviewStatusRemote",
        values.upstream_subject or ""
      ),
    }
  end

  if remote_action and remote_action.action == "push" then
    lines[#lines + 1] = {
      segments = {
        { ("%-8s"):format("Push:"), "DiffReviewStatusLabel" },
        { remote_action.status or "Pushing...", "DiffReviewStatusFetching" },
      },
    }
  elseif values.push_ref then
    lines[#lines + 1] = {
      segments = status_head_row(
        "Push",
        values.push_oid or "",
        values.push_ref,
        "DiffReviewStatusRemote",
        values.push_subject or ""
      ),
    }
  end

  lines[#lines + 1] = status_pr_head_line(pr_state)
  lines[#lines + 1] = status_about_head_line(about_state)
  return lines
end

---@param text string
---@return string[]
local function status_markdown_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text == "" then return { "_No description._" } end
  return vim.split(text, "\n", { plain = true })
end

---@param title string
---@param count integer
---@return string
function M._status_section_heading_text(title, count)
  title = tostring(title or ""):gsub("%s*:%s*$", "")
  return ("%s (%d):"):format(title, math.max(0, math.floor(tonumber(count) or 0)))
end

---@param title string
---@param count integer
---@return table[]
function M._status_section_heading_segments(title, count)
  return { { M._status_section_heading_text(title, count), "DiffReviewStatusHeader" } }
end

---@param section DiffReviewStatusSection
---@return integer
function M._status_section_count(section)
  if type(section.reviews) == "table" then return #section.reviews end
  if type(section.commits) == "table" then return #section.commits end
  return #(section.files or {})
end

---@param value any
---@return string
function M._pr_overview.comment_datetime(value)
  local text = tostring(value or "")
  local year, month, day, hour, minute = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)T(%d%d):(%d%d)")
  if year then return ("%s-%s-%s %s:%s"):format(year, month, day, hour, minute) end
  if text ~= "" then return text end
  return ""
end

---@param body string
---@return string
function M._pr_overview.comment_preview(body)
  local parts = {}
  for _, line in ipairs(vim.split(tostring(body or ""), "\n", { plain = true })) do
    local trimmed = vim.trim(line)
    if trimmed ~= "" then parts[#parts + 1] = trimmed end
  end
  return table.concat(parts, " "):gsub("%s+", " ")
end

---@param comment table
---@return string
function M._pr_overview.comment_author(comment)
  return tostring(comment and comment.user or "unknown")
end

---@param comment table
---@return string
function M._pr_overview.issue_comment_line(comment)
  local datetime = M._pr_overview.comment_datetime(comment.updated_at or comment.created_at)
  local prefix = ("%s %s"):format(M._comment_icon, M._pr_overview.comment_author(comment))
  if datetime ~= "" then prefix = prefix .. " | " .. datetime end
  local preview = M._pr_overview.comment_preview(comment.body or "")
  if preview ~= "" then prefix = prefix .. " | " .. preview end
  return prefix
end

---@param oid string?
---@return string
local function status_short_oid(oid)
  oid = tostring(oid or "")
  if oid == "" then return "0000000" end
  return oid:sub(1, 7)
end

---@param pr DiffReviewGhPR
---@return string
local function status_pr_head_subject(pr)
  local commits = pr.commits or {}
  local commit = commits[#commits]
  return commit and commit.messageHeadline or ""
end

---@param pr DiffReviewGhPR
---@param comments? DiffReviewGhPRCommentsResult
---@return DiffReviewStatusHeadLine[]
local function status_pr_detail_head_lines(pr, comments)
  local title = pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))
  local lines = {
    { segments = { { "Title:  ", "DiffReviewStatusLabel" }, { title, "DiffReviewStatusPath" } } },
  }
  if pr.repo and pr.repo ~= "" then
    lines[#lines + 1] = { segments = { { "Repo:   ", "DiffReviewStatusLabel" }, { pr.repo, "DiffReviewStatusRemote" } } }
  end
  lines[#lines + 1] = {
    segments = status_head_row("Head", status_short_oid(pr.headRefOid), pr.headRefName or "", "DiffReviewStatusBranch", status_pr_head_subject(pr)),
  }
  if pr.url and pr.url ~= "" then
    lines[#lines + 1] = { segments = { { "URL:    ", "DiffReviewStatusLabel" }, { pr.url, "DiffReviewStatusPR" } } }
  end
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = { segments = { { "Description:", "DiffReviewStatusHeader" } } }
  for _, line in ipairs(status_markdown_lines(pr.body)) do
    lines[#lines + 1] = { segments = { { line } } }
  end
  local issue_comments = comments and comments.issue_comments or {}
  if type(issue_comments) == "table" and #issue_comments > 0 then
    lines[#lines + 1] = { segments = { { "" } } }
    lines[#lines + 1] = {
      segments = M._status_section_heading_segments("Comments", #issue_comments),
    }
    for index, comment in ipairs(issue_comments) do
      lines[#lines + 1] = {
        entry = {
          id = ("pr-issue-comment:%s"):format(tostring(comment.remote_node_id or comment.remote_id or index)),
          kind = "pr_comment",
          pr_comment = comment,
        },
        segments = {
          { M._pr_overview.issue_comment_line(comment), "DiffReviewReviewComment" },
        },
      }
    end
  end
  return lines
end

---@param cwd string
---@param cb fun(lines: DiffReviewStatusHeadLine[], values: table)
local function status_head_lines_async(cwd, cb)
  local values = {}
  local pending = 9

  local function done(key, value)
    values[key] = value
    pending = pending - 1
    if pending > 0 then return end

    local pr_state = M._status and M._status.pr
    local about_state = M._status and M._status.about
    cb(status_build_head_lines(values, pr_state, about_state), values)
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

---@param oid string
---@return string
local function status_commit_key(oid)
  return "commit:" .. oid
end

---@param oid string
---@param filename string
---@return string
local function status_commit_file_key(oid, filename)
  return ("commit-file:%s:%s"):format(oid, filename)
end

---@param oid string
---@param filename string
---@param diff string?
---@return string
local function status_commit_hunk_key(oid, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("commit-hunk:%s:%s:%s"):format(oid, filename, hash)
end

---@param provider_key string
---@param filename string
---@return string
local function status_provider_file_key(provider_key, filename)
  return ("provider-file:%s:%s"):format(provider_key, filename)
end

---@param provider_key string
---@param filename string
---@param diff string?
---@return string
local function status_provider_hunk_key(provider_key, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("provider-hunk:%s:%s:%s"):format(provider_key, filename, hash)
end

---@param sections DiffReviewStatusSection[]?
function M._status_restore_initial_folds(sections)
  M._status = M._status or {}
  M._status.folds = {}
end

---@param sections DiffReviewStatusSection[]?
---@return string?
function M._status_first_grouping_id(sections)
  local section = sections and sections[1] or nil
  return section and status_section_key(section.name) or nil
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

function M._status_segment_line_parts(segments)
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
  return table.concat(parts), segment_highlights
end

local function status_add_segment_line(segments, entry)
  local text, segment_highlights = M._status_segment_line_parts(segments)
  local line = status_add_line(text, entry)
  for _, highlight in ipairs(segment_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
end

---@param buf integer
---@param entry_id string
---@param head_line DiffReviewStatusHeadLine
---@return boolean
function M._status_patch_head_line(buf, entry_id, head_line)
  local status = M._status_states and M._status_states[buf] or M._status
  if not (
    status
    and status.entries
    and head_line
    and head_line.segments
    and vim.api.nvim_buf_is_valid(buf)
  ) then return false end

  local target_line = nil
  for line, entry in pairs(status.entries) do
    if entry and entry.id == entry_id then
      target_line = line
      break
    end
  end
  if not target_line then return false end

  local text, segment_highlights = M._status_segment_line_parts(head_line.segments)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, target_line - 1, target_line, false, { text })
  vim.bo[buf].modifiable = false
  status.entries[target_line] = head_line.entry
  if status.lines then status.lines[target_line] = text end

  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, target_line - 1, target_line)
  for _, highlight in ipairs(segment_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, target_line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  return true
end

---@return DiffReviewStatusKeymapConfig
local function status_keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.status), keymaps.status or {})
end

---@param spec DiffReviewStatusCommandSpec
---@return boolean
local function status_command_visible(spec)
  local view_kind = M._status and M._status.view_kind or "status"
  return M._status_command_visible_for_view(spec, view_kind)
end

---@param spec DiffReviewStatusCommandSpec
---@param view_kind DiffReviewStatusViewKind
---@return boolean
function M._status_command_visible_for_view(spec, view_kind)
  return not spec.views or spec.views[view_kind] == true
end

---@param command_id string
---@return string[]
local function status_keys_for(command_id)
  local spec = status_command_specs_by_id[command_id]
  local keymaps = spec and spec.keymap == "review" and M._review.keymap_config() or status_keymap_config()
  local key = keymaps[command_id]
  if key == false or key == nil then return {} end
  if type(key) == "table" then return key end
  return { key }
end

---@param command_id string
---@return string
local function status_primary_key(command_id)
  return status_keys_for(command_id)[1] or ""
end

---@param keys string[]
---@return string
local function status_key_text(keys)
  return table.concat(keys, ", ")
end

---@param state? table
---@return table[]
function M._status_hint_segments(state)
  local view_kind = state and state.view_kind or (M._status and M._status.view_kind) or "status"
  local segments = {}
  local first = true
  local hint_command_ids = M._status_hint_command_ids_by_view[view_kind] or M._status_hint_command_ids_by_view.status
  for _, command_id in ipairs(hint_command_ids) do
    local spec = status_command_specs_by_id[command_id]
    if spec and spec.pinned and M._status_command_visible_for_view(spec, view_kind) then
      local key = status_primary_key(spec.id)
      if key ~= "" then
        if not first then
          segments[#segments + 1] = { " | ", "DiffReviewStatusHint" }
        end
        first = false
        segments[#segments + 1] = { key, "DiffReviewStatusHintKey" }
        segments[#segments + 1] = { " " .. spec.label, "DiffReviewStatusHint" }
      end
    end
  end
  return segments
end

---@param state table
---@return string
function M._status_hint_title(state)
  local view_kind = state.view_kind or "status"
  local options = M.config or config.options or config.defaults
  if view_kind == "pr" then
    local number = state.pr and state.pr.number
    return number and ("PR #" .. tostring(number)) or options.pr_buffer_name
  end
  if view_kind == "review" then
    local number = state.pr and state.pr.number
    return number and ("Review #" .. tostring(number)) or "Review"
  end
  if view_kind == "diff" then
    return state.diff_file and "GitBranchDiffFile" or "GitBranchDiff"
  end
  return options.status_buffer_name or config.defaults.status_buffer_name or "GitStatus"
end

---@param segments table[]
---@param title string
---@return string
function M._status_hint_winbar(segments, title)
  local parts = { ("%%#DiffReviewStatusLabel#%s%%*"):format(tostring(title or ""):gsub("%%", "%%%%")) }
  if #segments > 0 then
    parts[#parts + 1] = "%="
  end
  for _, segment in ipairs(segments) do
    local text = tostring(segment[1] or ""):gsub("%%", "%%%%")
    local highlight = segment[2]
    if highlight and text ~= "" then
      parts[#parts + 1] = ("%%#%s#%s%%*"):format(highlight, text)
    else
      parts[#parts + 1] = text
    end
  end
  return table.concat(parts)
end

---@param buf integer
---@param win? integer
function M._status_apply_hint_bar(buf, win)
  local state = M._status_states and M._status_states[buf] or (M._status and M._status.buf == buf and M._status) or nil
  if not state then return end
  local winbar = M._status_hint_winbar(M._status_hint_segments(state), M._status_hint_title(state))
  if win then
    if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf then
      vim.wo[win].winbar = winbar
    end
    return
  end
  for _, status_win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_win_is_valid(status_win) then
      vim.wo[status_win].winbar = winbar
    end
  end
end

---@param win integer
function M._status_clear_hint_bar(win)
  if vim.api.nvim_win_is_valid(win) then
    vim.wo[win].winbar = ""
  end
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

  local line_text = table.concat(text_parts)

  local line_entry = entry
  if row.diff_review_boundary then
    line_entry = nil
  end
  if diff_line and entry then
    line_entry = vim.tbl_extend("force", entry, { diff_line = diff_line })
  end
  local line = status_add_line(line_text, line_entry, row.diff_review_line_hl)
  if row.diff_review_boundary then
    M._status.boundary_lines = M._status.boundary_lines or {}
    M._status.boundary_lines[line] = true
  end
  for _, highlight in ipairs(row_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
  for _, extmark in ipairs(row_extmarks) do
    status_add_extmark(line, extmark.col, extmark.opts)
  end
  -- Review view: emit any draft comments anchored on this diff row as real
  -- (navigable, editable) lines right below it.
  if M._status.review_after_row and diff_line and entry then
    M._status.review_after_row(diff_line, indent)
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
          original_relpath = data.git_original_file,
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
          git_original_file = data.git_original_file,
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

---@class DiffReviewStatusDiffProviderFile
---@field path string
---@field additions? integer
---@field deletions? integer
---@field status? string
---@field changeType? string

---@class DiffReviewStatusDiffProvider
---@field section_name string
---@field default_status string
---@field files? DiffReviewStatusDiffProviderFile[]

---@param cwd string
---@param provider DiffReviewStatusDiffProvider
---@param diff_text? string
---@return DiffReviewStatusFile[]
local function status_files_from_diff_provider(cwd, provider, diff_text)
  local files_by_name = {} ---@type table<string, DiffReviewStatusFile>
  local files_with_provider_stats = {} ---@type table<string, boolean>
  local files = {} ---@type DiffReviewStatusFile[]

  local function ensure_file(relpath, stats)
    local filename = cwd .. "/" .. relpath
    local file = files_by_name[filename]
    if not file then
      file = {
        filename = filename,
        relpath = relpath,
        section_name = provider.section_name,
        added = 0,
        removed = 0,
        hunks = {},
        untracked = false,
        status = provider.default_status,
      }
      files_by_name[filename] = file
      files[#files + 1] = file
    end
    if stats then
      files_with_provider_stats[filename] = true
      file.added = stats.additions or file.added or 0
      file.removed = stats.deletions or file.removed or 0
      file.status = stats.changeType or stats.status or file.status
      file.git_status = stats.changeType or stats.status or file.git_status
    end
    return file
  end

  for _, provider_file in ipairs(provider.files or {}) do
    if provider_file.path and provider_file.path ~= "" then ensure_file(provider_file.path, provider_file) end
  end

  for _, parsed_hunk in ipairs(parse_diff(diff_text or "", false)) do
    local file = ensure_file(parsed_hunk.file, nil)
    if not files_with_provider_stats[file.filename] then
      file.added = file.added + (parsed_hunk.added or 0)
      file.removed = file.removed + (parsed_hunk.removed or 0)
    end

    file.hunks[#file.hunks + 1] = {
      file = parsed_hunk.file,
      filename = file.filename,
      section_name = provider.section_name,
      pos = parsed_hunk.pos,
      diff = parsed_hunk.diff,
      staged = false,
      context_text = parsed_hunk.context or "",
      added = parsed_hunk.added or 0,
      removed = parsed_hunk.removed or 0,
    }
  end

  table.sort(files, function(left_file, right_file)
    return left_file.relpath < right_file.relpath
  end)
  for _, file in ipairs(files) do
    table.sort(file.hunks, function(left_hunk, right_hunk)
      return (left_hunk.pos or 0) < (right_hunk.pos or 0)
    end)
  end
  return files
end

---@param cwd string
---@param commit DiffReviewStatusCommit
---@param diff_text string
---@return DiffReviewStatusFile[]
local function status_commit_files_from_diff(cwd, commit, diff_text)
  return status_files_from_diff_provider(cwd, {
    section_name = status_commit_key(commit.oid),
    default_status = "modified",
  }, diff_text)
end

M._section_builder = {}

---@param files DiffReviewStatusFile[]
---@return table<string, DiffReviewStatusFile>
function M._section_builder.files_by_name(files)
  local files_by_name = {} ---@type table<string, DiffReviewStatusFile>
  for _, file in ipairs(files or {}) do
    files_by_name[file.filename] = file
  end
  return files_by_name
end

---@param cwd string
---@param spec { section_name: string, default_status?: string, files?: DiffReviewGhPRFile[] }
---@param diff_text? string
---@return DiffReviewStatusFile[]
function M._section_builder.files_from_diff(cwd, spec, diff_text)
  return status_files_from_diff_provider(cwd, {
    section_name = spec.section_name,
    default_status = spec.default_status or "",
    files = spec.files,
  }, diff_text)
end

---@param title string
---@param files DiffReviewStatusFile[]
---@param opts? { name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@return DiffReviewStatusSection?
function M._section_builder.section_from_files(title, files, opts)
  opts = opts or {}
  files = files or {}
  if #files == 0 and not opts.keep_empty then return nil end
  return {
    name = opts.name or title,
    title = title,
    default_folded = opts.default_folded == true,
    files = files,
    files_by_name = M._section_builder.files_by_name(files),
    file_key_prefix = opts.file_key_prefix,
    file_entry_kind = opts.file_entry_kind,
    hunk_entry_kind = opts.hunk_entry_kind,
  }
end

---@param title string
---@param files DiffReviewStatusFile[]
---@param opts? { name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@return DiffReviewStatusSection[]
function M._section_builder.sections_from_files(title, files, opts)
  local section = M._section_builder.section_from_files(title, files, opts)
  return section and { section } or {}
end

---@param cwd string
---@param spec { title: string, section_name: string, default_status?: string, files?: DiffReviewGhPRFile[], name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@param diff_text? string
---@return DiffReviewStatusSection[]
---@return DiffReviewStatusFile[]
function M._section_builder.sections_from_diff(cwd, spec, diff_text)
  local files = M._section_builder.files_from_diff(cwd, spec, diff_text)
  return M._section_builder.sections_from_files(spec.title, files, spec), files
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param section_name string
---@return DiffReviewStatusFile
function M._section_builder.file_with_hunks(file, hunks, section_name)
  local clone = vim.deepcopy(file)
  clone.hunks = {}
  clone.section_name = section_name
  clone.added = 0
  clone.removed = 0
  for _, hunk in ipairs(hunks or {}) do
    local cloned_hunk = vim.deepcopy(hunk)
    cloned_hunk.section_name = section_name
    clone.hunks[#clone.hunks + 1] = cloned_hunk
    clone.added = clone.added + (cloned_hunk.added or 0)
    clone.removed = clone.removed + (cloned_hunk.removed or 0)
  end
  return clone
end

---@param comment table
---@return "LEFT"|"RIGHT"
function M._section_builder.comment_side(comment)
  return tostring(comment and comment.side or "RIGHT") == "LEFT" and "LEFT" or "RIGHT"
end

---@param cwd string
---@param comments table[]
---@return table<string, table[]>
function M._section_builder.comments_by_path(cwd, comments)
  local by_path = {}
  for _, comment in ipairs(type(comments) == "table" and comments or {}) do
    local path = tostring(comment.path or "")
    if path ~= "" then
      comment.abs_file = comment.abs_file or vim.fs.normalize(vim.fs.joinpath(cwd, path))
      comment.side = M._section_builder.comment_side(comment)
      comment.local_state = comment.local_state or "clean"
      by_path[path] = by_path[path] or {}
      by_path[path][#by_path[path] + 1] = comment
      by_path[comment.abs_file] = by_path[comment.abs_file] or by_path[path]
    end
  end
  for _, path_comments in pairs(by_path) do
    table.sort(path_comments, function(left_comment, right_comment)
      local left_line = tonumber(left_comment.line or left_comment.start_line) or math.huge
      local right_line = tonumber(right_comment.line or right_comment.start_line) or math.huge
      if left_line ~= right_line then return left_line < right_line end
      return tostring(left_comment.created_at or "") < tostring(right_comment.created_at or "")
    end)
  end
  return by_path
end

---@param cwd string
---@param files DiffReviewStatusFile[]
---@param comments table[]
---@param opts? { field?: string }
---@return table<string, table[]>
function M._section_builder.attach_comments(cwd, files, comments, opts)
  opts = opts or {}
  local field = opts.field or "comments"
  local by_path = M._section_builder.comments_by_path(cwd, comments)
  for _, file in ipairs(files or {}) do
    file[field] = by_path[file.relpath] or by_path[file.filename] or {}
  end
  return by_path
end

---@param file string?
---@param side string?
---@param line integer|string?
---@return string?
function M._section_builder.comment_anchor_key(file, side, line)
  local line_number = tonumber(line)
  if not (file and file ~= "" and line_number) then return nil end
  local normalized_file = vim.fs.normalize(tostring(file))
  return ("%s\0%s\0%d"):format(normalized_file, tostring(side or "RIGHT"), math.floor(line_number))
end

---@param comments table[]
---@return table<string, { comment: table, index: integer }[]>
function M._section_builder.comment_anchor_index(comments)
  local by_anchor = {}
  for index, comment in ipairs(type(comments) == "table" and comments or {}) do
    local key = M._section_builder.comment_anchor_key(comment.abs_file or comment.path, M._section_builder.comment_side(comment), comment.line)
    if key then
      by_anchor[key] = by_anchor[key] or {}
      by_anchor[key][#by_anchor[key] + 1] = { comment = comment, index = index }
    end
  end
  return by_anchor
end

---@param sections DiffReviewStatusSection[]
---@param opts? { field?: string }
---@return table<string, { comment: table, index: integer }[]>
function M._section_builder.comment_anchor_index_from_sections(sections, opts)
  opts = opts or {}
  local field = opts.field or "comments"
  local by_anchor = {}
  for _, section in ipairs(sections or {}) do
    for _, file in ipairs(section.files or {}) do
      for index, comment in ipairs(type(file[field]) == "table" and file[field] or {}) do
        local key = M._section_builder.comment_anchor_key(comment.abs_file or file.filename, M._section_builder.comment_side(comment), comment.line)
        if key then
          by_anchor[key] = by_anchor[key] or {}
          by_anchor[key][#by_anchor[key] + 1] = { comment = comment, index = index }
        end
      end
    end
  end
  return by_anchor
end

---@param state table
---@param diff_line table
---@param indent integer
---@param opts { index_field: string, count_field?: string, skip?: fun(comment: table): boolean }
function M._section_builder.emit_anchored_comments(state, diff_line, indent, opts)
  local key = M._section_builder.comment_anchor_key(diff_line.file, M._review.side_of(diff_line), diff_line.line)
  local comments = key and state[opts.index_field] and state[opts.index_field][key] or nil
  for _, item in ipairs(comments or {}) do
    if item.comment
      and item.comment.local_state ~= "deleted"
      and not (opts.skip and opts.skip(item.comment)) then
      if opts.count_field then state[opts.count_field] = (state[opts.count_field] or 0) + 1 end
      M._review.emit_comment(item.comment, item.index, indent)
    end
  end
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M._pr_overview.review_key(review)
  local id = tostring(review.node_id or "")
  if id == "" then id = tostring(review.id or "") end
  if id == "" then id = vim.fn.sha256(tostring(review.user or "") .. tostring(review.submitted_at or "") .. tostring(review.body or "")) end
  return "pr-review:" .. id
end

---@param state string?
---@return string
function M._pr_overview.review_state_label(state)
  state = tostring(state or "")
  if state == "CHANGES_REQUESTED" then return "REJECTED" end
  if state == "COMMENTED" then return "COMMENTED" end
  if state == "DISMISSED" then return "DISMISSED" end
  if state == "APPROVED" then return "APPROVED" end
  if state == "" then return "REVIEW" end
  return state
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M._pr_overview.review_summary_line(review)
  local parts = {
    M._comment_icon,
    ("%s by %s"):format(M._pr_overview.review_state_label(review.state), tostring(review.user or "unknown")),
  }
  local datetime = M._pr_overview.comment_datetime(review.submitted_at or review.updated_at or review.created_at)
  if datetime ~= "" then parts[#parts + 1] = datetime end
  local preview = M._pr_overview.comment_preview(review.body or "")
  if preview ~= "" then parts[#parts + 1] = preview end
  return table.concat(parts, " | ")
end

---@param comments? DiffReviewGhPRCommentsResult
---@return DiffReviewStatusSection?
function M._pr_overview.reviews_section(comments)
  local reviews = comments and comments.reviews or nil
  if type(reviews) ~= "table" or #reviews == 0 then return nil end
  return {
    name = "pr:reviews",
    title = "Reviews",
    default_folded = false,
    files = {},
    files_by_name = {},
    reviews = reviews,
  }
end

---@param comment table
---@return "LEFT"|"RIGHT"
function M._pr_overview.comment_side(comment)
  return M._section_builder.comment_side(comment)
end

---@param hunk DiffReviewHunk
---@param comment table
---@return boolean
function M._pr_overview.hunk_contains_comment(hunk, comment)
  local target_line = tonumber(comment and comment.line)
  if not target_line then return false end
  local start_line = tonumber(comment.start_line) or target_line
  if start_line > target_line then start_line, target_line = target_line, start_line end
  local wanted_side = M._pr_overview.comment_side(comment)
  local old_line, new_line
  for diff_line in tostring(hunk.diff or ""):gmatch("[^\n]+") do
    local old_start, new_start = diff_line:match("^@@ %-(%d+),?%d* %+(%d+),?%d* @@")
    if old_start and new_start then
      old_line = tonumber(old_start)
      new_line = tonumber(new_start)
    elseif old_line and new_line then
      local prefix = diff_line:sub(1, 1)
      if prefix == " " then
        if wanted_side == "LEFT" and old_line >= start_line and old_line <= target_line then return true end
        if wanted_side == "RIGHT" and new_line >= start_line and new_line <= target_line then return true end
        old_line = old_line + 1
        new_line = new_line + 1
      elseif prefix == "-" then
        if wanted_side == "LEFT" and old_line >= start_line and old_line <= target_line then return true end
        old_line = old_line + 1
      elseif prefix == "+" then
        if wanted_side == "RIGHT" and new_line >= start_line and new_line <= target_line then return true end
        new_line = new_line + 1
      end
    end
  end
  return false
end

---@param cwd string
---@param pr DiffReviewGhPR
---@param diff_text? string
---@param review DiffReviewGhSubmittedReview
---@return DiffReviewStatusFile[]
function M._pr_overview.review_context_files(cwd, pr, diff_text, review)
  local by_path = M._section_builder.comments_by_path(cwd, review.comments or {})
  local files = M._section_builder.files_from_diff(cwd, {
    section_name = M._pr_overview.review_key(review),
    default_status = "",
    files = pr.files,
  }, diff_text)
  local result = {}
  for _, file in ipairs(files) do
    local file_comments = by_path[file.relpath] or by_path[file.filename] or {}
    if #file_comments > 0 then
      local hunks = {}
      for _, hunk in ipairs(file.hunks or {}) do
        for _, comment in ipairs(file_comments) do
          if M._pr_overview.hunk_contains_comment(hunk, comment) then
            hunks[#hunks + 1] = hunk
            break
          end
        end
      end
      local copy = M._section_builder.file_with_hunks(file, hunks, file.section_name)
      copy.pr_review_comments = file_comments
      result[#result + 1] = copy
    end
  end
  return result
end

---@param cwd string
---@param pr DiffReviewGhPR
---@param diff_text? string
---@param comments? DiffReviewGhPRCommentsResult
---@param local_comments? table[]
---@return DiffReviewStatusSection[]
local function status_pr_sections(cwd, pr, diff_text, comments, local_comments)
  local provider_key = "pr:" .. tostring(pr.number)
  local change_sections, files = M._section_builder.sections_from_diff(cwd, {
    title = "Changes",
    section_name = provider_key .. ":changes",
    default_status = "",
    files = pr.files,
    name = provider_key .. ":changes",
    file_key_prefix = provider_key,
    file_entry_kind = "pr_file",
    hunk_entry_kind = "pr_hunk",
  }, diff_text)
  local code_comments = {}
  local local_comment_keys = {}
  for _, comment in ipairs(local_comments or {}) do
    local key = M._pr_overview.comment_identity_key(comment)
    if key then local_comment_keys[key] = true end
  end
  for _, comment in ipairs(comments and comments.code_comments or {}) do
    local key = M._pr_overview.comment_identity_key(comment)
    if not (key and local_comment_keys[key]) then code_comments[#code_comments + 1] = comment end
  end
  vim.list_extend(code_comments, local_comments or {})
  M._section_builder.attach_comments(cwd, files, code_comments, { field = "pr_comments" })
  local sections = {}
  local reviews_section = M._pr_overview.reviews_section(comments)
  if reviews_section then sections[#sections + 1] = reviews_section end
  vim.list_extend(sections, change_sections)
  return sections
end

---@class DiffReviewCommitLogSectionSpec
---@field name DiffReviewStatusSectionName
---@field title string
---@field args string[]
---@field branch? string
---@field upstream? string
---@field default_folded boolean
---@field limit? integer

---@param spec DiffReviewCommitLogSectionSpec
---@param output string[]
---@return DiffReviewStatusCommit[]
local function status_commits_from_log_output(spec, output)
  local commits = {} ---@type DiffReviewStatusCommit[]
  for index, line in ipairs(output or {}) do
    if spec.limit and index > spec.limit then break end
    local oid, short_oid, subject = line:match("^([^\t]+)\t([^\t]+)\t(.*)$")
    if oid and oid ~= "" then
      local cache = M._status and M._status.commit_file_cache and M._status.commit_file_cache[oid] or nil
      commits[#commits + 1] = {
        oid = oid,
        short_oid = short_oid or oid:sub(1, 7),
        branch = index == 1 and spec.branch or nil,
        subject = subject or "",
        upstream = spec.upstream,
        files = cache and cache.files or nil,
        files_loaded = cache and cache.files_loaded or false,
        files_loading = cache and cache.files_loading or false,
        files_error = cache and cache.files_error or nil,
      }
    end
  end
  return commits
end

---@param cwd string
---@param spec DiffReviewCommitLogSectionSpec
---@param cb fun(section?: DiffReviewStatusSection)
local function status_commit_log_section_async(cwd, spec, cb)
  if #spec.args == 0 then
    cb(nil)
    return
  end

  local command = { "git", "-C", cwd, "log", "--no-color", "--format=%H%x09%h%x09%s" }
  vim.list_extend(command, spec.args)
  systemlist_async(command, function(output, code)
    if code ~= 0 then
      cb(nil)
      return
    end

    local commits = status_commits_from_log_output(spec, output or {})
    if #commits == 0 then
      cb(nil)
      return
    end

    cb({
      name = spec.name,
      title = spec.title,
      default_folded = spec.default_folded,
      files = {},
      files_by_name = {},
      commits = commits,
      upstream = spec.upstream,
    })
  end)
end

---@param cwd string
---@param upstream string?
---@param branch string?
---@param cb fun(section?: DiffReviewStatusSection)
local function status_unmerged_section_async(cwd, upstream, branch, cb)
  if not upstream or upstream == "" then
    cb(nil)
    return
  end
  status_commit_log_section_async(cwd, {
    name = "unmerged",
    title = "Unmerged into " .. upstream,
    args = { upstream .. "..HEAD" },
    branch = branch,
    upstream = upstream,
    default_folded = false,
  }, cb)
end

---@param cwd string
---@param upstream string?
---@param branch string?
---@param cb fun(section?: DiffReviewStatusSection)
local function status_recent_commits_section_async(cwd, upstream, branch, cb)
  local args = { "-20" }
  if upstream and upstream ~= "" then
    args[#args + 1] = upstream
  end
  status_commit_log_section_async(cwd, {
    name = "recent",
    title = "Recent Commits",
    args = args,
    branch = branch,
    default_folded = true,
    limit = 20,
  }, cb)
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
  if section_map.unmerged and #(section_map.unmerged.commits or {}) > 0 then
    ordered[#ordered + 1] = section_map.unmerged
  end
  if section_map.recent and #(section_map.recent.commits or {}) > 0 then
    ordered[#ordered + 1] = section_map.recent
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

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return boolean
local function status_append_hunk_to_file(file, hunk)
  for _, existing_hunk in ipairs(file.hunks or {}) do
    if existing_hunk.diff == hunk.diff then return false end
  end
  file.hunks = file.hunks or {}
  file.hunks[#file.hunks + 1] = hunk
  file.added = (file.added or 0) + (hunk.added or 0)
  file.removed = (file.removed or 0) + (hunk.removed or 0)
  return true
end

---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection>
---@param section_name DiffReviewStatusSectionName
---@param file DiffReviewStatusFile
local function status_merge_file_into_section(section_map, section_name, file)
  local section = section_map[section_name]
  local existing_file = section.files_by_name[file.filename]
  if not existing_file then
    section.files[#section.files + 1] = file
    section.files_by_name[file.filename] = file
    return
  end

  local moved_hunks = file.hunks or {}
  if #moved_hunks == 0 then
    existing_file.added = (existing_file.added or 0) + (file.added or 0)
    existing_file.removed = (existing_file.removed or 0) + (file.removed or 0)
    return
  end
  for _, hunk in ipairs(moved_hunks) do
    status_append_hunk_to_file(existing_file, hunk)
  end
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
        status_merge_file_into_section(section_map, target_section, moved_file)
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
        status_append_hunk_to_file(target_file, moved_hunk)
      end
    end
  end
  return status_order_section_map(section_map)
end

---@param cwd string
---@param cb fun(result: { head_lines: DiffReviewStatusHeadLine[], head_values: table, sections: DiffReviewStatusSection[] })
local function status_load_async(cwd, cb)
  local head_lines = nil
  local head_values = nil
  local sections = nil
  local unmerged_section = nil
  local unmerged_loaded = false
  local recent_commits_section = nil
  local recent_commits_loaded = false

  local function maybe_done()
    if not (head_lines and sections and unmerged_loaded and recent_commits_loaded) then return end
    local ordered_sections = {}
    vim.list_extend(ordered_sections, sections)
    if unmerged_section then ordered_sections[#ordered_sections + 1] = unmerged_section end
    if recent_commits_section then ordered_sections[#ordered_sections + 1] = recent_commits_section end
    cb({ head_lines = head_lines, head_values = head_values or {}, sections = ordered_sections })
  end

  status_head_lines_async(cwd, function(lines, values)
    head_lines = lines
    head_values = values
    status_unmerged_section_async(cwd, values.upstream, values.branch, function(section)
      unmerged_section = section
      unmerged_loaded = true
      maybe_done()
    end)
    status_recent_commits_section_async(cwd, values.upstream, values.branch, function(section)
      recent_commits_section = section
      recent_commits_loaded = true
      maybe_done()
    end)
    maybe_done()
  end)
  collect_items_from_git(cwd, function(items)
    sections = status_sections_from_items(items or {})
    maybe_done()
  end, { skip_pre_render = true, skip_ts_context = true })
end

---@param file DiffReviewStatusFile
---@return DiffReviewHunk[]
status_diff_hunks_for_file = function(file)
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

local status_cursor_target
local status_operations_pending
local status_request_reconcile

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param previous_hunk? DiffReviewHunk
---@param next_hunk? DiffReviewHunk
---@param entry_kind? "hunk"|"commit_hunk"|"pr_hunk"
---@param hunk_key_override? string
local function status_render_hunk(file, hunk, previous_hunk, next_hunk, entry_kind, hunk_key_override)
  local hunk_key = hunk_key_override or status_hunk_key(file.section_name, file.filename, hunk.diff)
  local hunk_folded = status_folded(hunk_key, false)
  local entry = { id = hunk_key, kind = entry_kind or "hunk", file = file, hunk = hunk }
  M._status.fancy_rows = M._status.fancy_rows or {}
  local rows_key
  local function rerender_with_context()
    M._status = M._status or {}
    if M._status.context_rerender_pending then return end
    M._status.context_rerender_pending = true
    vim.schedule(function()
      if not M._status then return end
      M._status.context_rerender_pending = false
      M._status.fancy_rows = {}
      local buf = M._status.buf
      if buf and vim.api.nvim_buf_is_valid(buf) then
        if status_operations_pending() then return end
        local target_id, fallback_line = status_cursor_target(buf)
        M.render_status(buf, target_id, fallback_line, { reuse_sections = true })
      end
    end)
  end
  local current_context = cached_hunk_context(
    file.filename,
    hunk.pos,
    "status-neighbor:" .. hunk_key .. ":current",
    rerender_with_context
  )
  local previous_context = previous_hunk and cached_hunk_context(
    file.filename,
    previous_hunk.pos,
    "status-neighbor:" .. hunk_key .. ":previous",
    rerender_with_context
  ) or nil
  local next_context = next_hunk and cached_hunk_context(
    file.filename,
    next_hunk.pos,
    "status-neighbor:" .. hunk_key .. ":next",
    rerender_with_context
  ) or nil
  local suppress_start_boundary = same_hunk_context_scope(previous_context, current_context)
  local suppress_end_boundary = same_hunk_context_scope(current_context, next_context)
  rows_key = ("%s:%s:%s:%s"):format(
    hunk_key,
    hunk.staged and "staged" or "unstaged",
    suppress_start_boundary and "no-start" or "start",
    suppress_end_boundary and "no-end" or "end"
  )
  local rows = M._status.fancy_rows[rows_key]
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
      {
        context_line = hunk.pos,
        boundary_context = true,
        suppress_start_boundary = suppress_start_boundary,
        suppress_end_boundary = suppress_end_boundary,
      }
    )
    if ok then
      rows = built_rows
      if not rows.diff_review_syntax_pending then
        M._status.fancy_rows[rows_key] = rows
      end
    end
  end
  if not rows then
    local ts_context = current_context or cached_hunk_context(file.filename, hunk.pos, "status-fallback:" .. hunk_key, rerender_with_context)
    local context_text = hunk_context_label(ts_context) or hunk.context_text or ""
    local context = context_text ~= "" and (context_text .. " ") or ""
    local header = ("%s@@ %s+%d -%d"):format(string.rep(" ", status_hunk_indent), context, hunk.added or 0, hunk.removed or 0)
    local visible_hunk_lines = nil
    local node_start = nil
    local node_end = nil
    local start_text = nil
    local end_text = nil
    if not hunk_folded and type(ts_context) == "table" then
      visible_hunk_lines = hunk_visible_source_lines(hunk.diff)
      node_start = ts_context.start_row + 1
      node_end = ts_context.end_row + 1
      start_text = ts_context.start_text or ""
      end_text = ts_context.end_text or ""
      if not suppress_start_boundary and not visible_hunk_lines[start_text] then
        status_add_fancy_row(hunk_boundary_row(start_text, ts_context.start_segments, node_start), nil, status_hunk_indent)
        if node_start ~= node_end then
          status_add_fancy_row(hunk_boundary_ellipsis_row(start_text), nil, status_hunk_indent)
        end
      end
    end
    status_add_line(header, entry, hunk_folded and "DiffReviewActiveHunkHeader" or "DiffReviewHunkHeader")
    if visible_hunk_lines and node_start and node_end and end_text then
      if not suppress_end_boundary and not visible_hunk_lines[end_text] and node_end ~= node_start then
        status_add_fancy_row(hunk_boundary_ellipsis_row(end_text), nil, status_hunk_indent)
        status_add_fancy_row(hunk_boundary_row(end_text, ts_context.end_segments, node_end), nil, status_hunk_indent)
      end
    end
    return
  end

  if hunk_folded then
    local header_row = rows[1]
    for _, row in ipairs(rows) do
      if row.diff_review_hunk_header then
        header_row = row
        break
      end
    end
    status_add_fancy_row(header_row, entry, status_hunk_indent)
    return
  end

  for row_index = 1, #rows do
    local row = rows[row_index]
    if row then status_add_fancy_row(row, row.diff_review_boundary and nil or entry, status_hunk_indent) end
  end
end

---@param pr DiffReviewGhPR?
---@return string?
function M._pr_overview.pr_base_url(pr)
  if not (pr and pr.url and pr.url ~= "") then return nil end
  return pr.url:gsub("/$", "")
end

---@param pr DiffReviewGhPR?
---@param entry DiffReviewStatusEntry?
---@return string?
function M._pr_overview.entry_url(pr, entry)
  if not entry then return nil end
  local base_url = M._pr_overview.pr_base_url(pr)
  local reply = entry.pr_comment_reply or entry.review_reply
  if reply then
    if reply.url and reply.url ~= "" then return reply.url end
    if base_url and reply.remote_id then return ("%s/changes#r%s"):format(base_url, tostring(reply.remote_id)) end
  end

  local comment = entry.pr_comment or entry.review_comment
  if comment then
    if comment.url and comment.url ~= "" then return comment.url end
    if base_url and comment.remote_id then
      if comment.path and comment.path ~= "" then return ("%s/changes#r%s"):format(base_url, tostring(comment.remote_id)) end
      return ("%s#issuecomment-%s"):format(base_url, tostring(comment.remote_id))
    end
  end

  local review = entry.pr_review
  if review then
    if review.url and review.url ~= "" then return review.url end
    if base_url and review.id and review.id ~= 0 then return ("%s#pullrequestreview-%s"):format(base_url, tostring(review.id)) end
  end

  return nil
end

---@param file DiffReviewStatusFile
---@param entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@param hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"
---@param file_key_override? string
---@param hunk_key_builder? fun(hunk: DiffReviewHunk): string
---@param opts? { force_open?: boolean }
local function status_render_file(file, entry_kind, hunk_entry_kind, file_key_override, hunk_key_builder, opts)
  opts = opts or {}
  local file_key = file_key_override or status_file_key(file.section_name, file.filename)
  local file_folded = (not opts.force_open) and status_folded(file_key, true)
  local stats = file.untracked and "new" or ("+%d -%d"):format(file.added, file.removed)
  local line = ("%s%s %s"):format(string.rep(" ", status_file_indent), file.relpath, stats)
  local entry = { id = file_key, kind = entry_kind or "file", file = file }
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
  for hunk_index, hunk in ipairs(hunks) do
    local hunk_key = hunk_key_builder and hunk_key_builder(hunk) or nil
    status_render_hunk(file, hunk, hunks[hunk_index - 1], hunks[hunk_index + 1], hunk_entry_kind, hunk_key)
  end
end

---@param review DiffReviewGhSubmittedReview
---@param diff_line table
---@param indent integer
function M._pr_overview.emit_review_comments_for(review, diff_line, indent)
  for index, comment in ipairs(review.comments or {}) do
    if comment.local_state ~= "deleted"
      and comment.abs_file == diff_line.file
      and M._pr_overview.comment_side(comment) == M._review.side_of(diff_line)
      and tonumber(comment.line) == tonumber(diff_line.line) then
      M._review.emit_comment(comment, index, indent)
    end
  end
end

---@param review DiffReviewGhSubmittedReview
function M._pr_overview.render_review_context(review)
  local status = M._status
  if not (status and status.cwd and status.pr) then return end
  local comments = type(review.comments) == "table" and review.comments or {}
  if #comments == 0 then
    status_add_line("  No inline comments", { kind = "pr_review", pr_review = review }, "Comment")
    return
  end
  if not status.pr_diff_text or status.pr_diff_text == "" then
    status_add_line("  ...loading diff context...", { kind = "pr_review", pr_review = review }, "DiffReviewStatusFetching")
    return
  end

  local files = M._pr_overview.review_context_files(status.cwd, status.pr, status.pr_diff_text, review)
  if #files == 0 then
    status_add_line("  No matching diff context", { kind = "pr_review", pr_review = review }, "Comment")
    return
  end

  local review_key = M._pr_overview.review_key(review)
  local previous_hook = status.review_after_row
  status.review_after_row = function(diff_line, indent)
    M._pr_overview.emit_review_comments_for(review, diff_line, indent)
  end
  local ok, err = pcall(function()
    for _, file in ipairs(files) do
      status_render_file(
        file,
        "pr_review_file",
        "pr_review_hunk",
        status_provider_file_key(review_key, file.filename),
        function(hunk)
          return status_provider_hunk_key(review_key, file.filename, hunk.diff)
        end,
        { force_open = true }
      )
    end
  end)
  status.review_after_row = previous_hook
  if not ok then error(err) end
end

---@param review DiffReviewGhSubmittedReview
function M._pr_overview.render_review(review)
  local review_key = M._pr_overview.review_key(review)
  local summary = M._pr_overview.review_summary_line(review)
  local entry = { id = review_key, kind = "pr_review", pr_review = review }
  local line_number = status_add_line(summary, entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(line_number, 0, #summary, "DiffReviewReviewCommentHeader")
  if status_folded(review_key, true) then return end
  M._pr_overview.render_review_context(review)
end

---@param commit DiffReviewStatusCommit
local function status_render_commit(commit)
  local commit_key = status_commit_key(commit.oid)
  local commit_folded = status_folded(commit_key, true)
  local line_parts = {
    commit.short_oid,
  }
  if commit.branch and commit.branch ~= "" then
    line_parts[#line_parts + 1] = commit.branch
  end
  line_parts[#line_parts + 1] = commit.subject
  local line = table.concat(line_parts, " ")
  local entry = { id = commit_key, kind = "commit", commit = commit }
  local line_number = status_add_line(line, entry)
  local col = 0
  status_add_highlight(line_number, col, col + #commit.short_oid, "DiffReviewStatusObjectId")
  col = col + #commit.short_oid + 1
  if commit.branch and commit.branch ~= "" then
    status_add_highlight(line_number, col, col + #commit.branch, "DiffReviewStatusBranch")
  end

  if commit_folded then return end
  if commit.files_loading then
    status_add_line("...loading...", entry, "DiffReviewStatusFetching")
    return
  end
  if commit.files_error then
    status_add_line(commit.files_error, entry, "ErrorMsg")
    return
  end
  if not commit.files_loaded then
    status_add_line("...loading...", entry, "DiffReviewStatusFetching")
    return
  end
  if #(commit.files or {}) == 0 then
    status_add_line("No textual diff", entry, "Comment")
    return
  end

  for _, file in ipairs(commit.files or {}) do
    status_render_file(
      file,
      "commit_file",
      "commit_hunk",
      status_commit_file_key(commit.oid, file.filename),
      function(hunk)
        return status_commit_hunk_key(commit.oid, file.filename, hunk.diff)
      end
    )
  end
end

---@param section DiffReviewStatusSection
local function status_render_section(section)
  local section_key = status_section_key(section.name)
  local section_folded = status_folded(section_key, section.default_folded)
  local line = M._status_section_heading_text(section.title, M._status_section_count(section))
  local entry = { id = section_key, kind = "section", section = section }
  status_add_line(line, entry, "DiffReviewStatusHeader")
  if section_folded then return end
  if section.reviews then
    for _, review in ipairs(section.reviews) do
      M._pr_overview.render_review(review)
    end
    return
  end
  if section.commits then
    for _, commit in ipairs(section.commits) do
      status_render_commit(commit)
    end
    return
  end
  for _, file in ipairs(section.files) do
    if section.file_key_prefix then
      status_render_file(
        file,
        section.file_entry_kind or "file",
        section.hunk_entry_kind or "hunk",
        status_provider_file_key(section.file_key_prefix, file.filename),
        function(hunk)
          return status_provider_hunk_key(section.file_key_prefix, file.filename, hunk.diff)
        end
      )
    else
      status_render_file(file)
    end
  end
end

---@param commit DiffReviewStatusCommit
local function status_load_commit_files(commit)
  local status = M._status
  local cwd = status and status.cwd
  local buf = status and status.buf
  if not (cwd and buf and vim.api.nvim_buf_is_valid(buf)) then return end

  status.commit_file_cache = status.commit_file_cache or {}
  local cached = status.commit_file_cache[commit.oid]
  if cached and (cached.files_loaded or cached.files_loading) then
    commit.files = cached.files
    commit.files_loaded = cached.files_loaded
    commit.files_loading = cached.files_loading
    commit.files_error = cached.files_error
    return
  end

  local request_id = (status.commit_file_request_id or 0) + 1
  status.commit_file_request_id = request_id
  commit.files_loading = true
  commit.files_error = nil
  status.commit_file_cache[commit.oid] = {
    files = nil,
    files_loaded = false,
    files_loading = true,
    files_error = nil,
  }

  systemlist_async({
    "git", "-C", cwd, "show", "--format=", "--no-color", "--no-ext-diff", commit.oid,
  }, function(output, code)
    local latest_status = M._status
    if not (latest_status and latest_status.buf and vim.api.nvim_buf_is_valid(latest_status.buf)) then return end
    if latest_status.cwd ~= cwd then return end

    latest_status.commit_file_cache = latest_status.commit_file_cache or {}
    local next_cache = {
      files = nil,
      files_loaded = true,
      files_loading = false,
      files_error = nil,
    }
    if code ~= 0 then
      next_cache.files_loaded = false
      next_cache.files_error = "Unable to load commit diff"
    else
      next_cache.files = status_commit_files_from_diff(cwd, commit, table.concat(output or {}, "\n"))
    end
    latest_status.commit_file_cache[commit.oid] = next_cache

    for _, section in ipairs(latest_status.sections or {}) do
      for _, current_commit in ipairs(section.commits or {}) do
        if current_commit.oid == commit.oid then
          current_commit.files = next_cache.files
          current_commit.files_loaded = next_cache.files_loaded
          current_commit.files_loading = false
          current_commit.files_error = next_cache.files_error
        end
      end
    end

    M.render_status(latest_status.buf, nil, nil, { reuse_sections = true })
  end)
end

---@return DiffReviewStatusEntry?
local function status_entry_under_cursor()
  local status = M._status
  if not status then return nil end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  if status.boundary_lines and status.boundary_lines[line] then return nil end
  return status.entries[line]
end

---@param buf integer
---@return string?
function M._pr_overview.url_under_cursor(buf)
  if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
  local status = M._status
  if not (status and status.view_kind == "pr") then return nil end
  return M._pr_overview.entry_url(status.pr, status_entry_under_cursor())
end

---@return integer? line
---@return DiffReviewStatusEntry? entry
function M._status_entry_line_under_cursor()
  local status = M._status
  if not (status and status.entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil, nil end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = math.min(cursor_line, max_line), 1, -1 do
    local entry = status.entries[line]
    if entry then return line, entry end
  end
  return nil, nil
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_is_file_like(entry)
  return entry ~= nil
    and (entry.kind == "file" or entry.kind == "commit_file" or entry.kind == "pr_file" or entry.kind == "pr_review_file")
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_is_hunk_like(entry)
  return entry ~= nil
    and (entry.kind == "hunk" or entry.kind == "commit_hunk" or entry.kind == "pr_hunk" or entry.kind == "pr_review_hunk")
end

---@param current_line integer
---@param entry DiffReviewStatusEntry
---@return DiffReviewStatusEntry?
function M._status_parent_entry(current_line, entry)
  local status = M._status
  if not (status and status.entries) then return nil end
  for line = current_line - 1, 1, -1 do
    local candidate = status.entries[line]
    if entry.kind == "commit_hunk" and candidate and candidate.kind == "commit_file" then return candidate end
    if M._status_entry_is_hunk_like(entry) and M._status_entry_is_file_like(candidate) then return candidate end
    if M._status_entry_is_file_like(entry) and candidate and candidate.kind == "pr_review" then return candidate end
    if M._status_entry_is_file_like(entry) and candidate and candidate.kind == "commit" then return candidate end
    if (M._status_entry_is_file_like(entry) or entry.kind == "commit" or entry.kind == "pr_review") and candidate and candidate.kind == "section" then
      return candidate
    end
  end
  return nil
end

---@param entry DiffReviewStatusEntry?
local function status_prewarm_entry_syntax(entry)
  if not entry then return end
  if M._status_entry_is_file_like(entry) and entry.file then
    prewarm_file_diff_syntax(entry.file, "status-cursor-prewarm:" .. (entry.id or entry.file.filename))
  elseif M._status_entry_is_hunk_like(entry) and entry.file and entry.hunk then
    cached_diff_syntax(
      entry.file.filename,
      entry.hunk.diff,
      "status-cursor-prewarm:" .. (entry.id or entry.file.filename),
      nil
    )
  end
end

---@param buf integer
local function status_defer_prewarm_under_cursor(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not status then return end
  status.cursor_prewarm_request_id = (status.cursor_prewarm_request_id or 0) + 1
  local request_id = status.cursor_prewarm_request_id
  local entry = status_entry_under_cursor()
  local entry_id = entry and entry.id or nil

  vim.defer_fn(function()
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.cursor_prewarm_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
    M._status = latest_status
    local current_entry = status_entry_under_cursor()
    if entry_id and current_entry and current_entry.id == entry_id then
      status_prewarm_entry_syntax(current_entry)
    end
  end, 35)
end

local status_files_from_set

---@param entry DiffReviewStatusEntry?
---@return string[]
local function status_files_for_entry(entry)
  if not entry then return {} end
  if M._status_entry_is_hunk_like(entry) and entry.file then return { entry.file.filename } end
  if M._status_entry_is_file_like(entry) and entry.file then return { entry.file.filename } end
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

local function status_leave_visual_mode()
  local mode = vim.api.nvim_get_mode().mode
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "nx", false)
  end
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
  notify_debug(("%s %s"):format(action, table.concat(parts, ", ")), vim.log.levels.INFO, { title = "DiffReview" })
end

---@param buf integer?
---@return string? target_id
---@return integer? fallback_line
status_cursor_target = function(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil, nil end
  local ok_current_buf, current_buf = pcall(vim.api.nvim_get_current_buf)
  if not ok_current_buf or current_buf ~= buf then return nil, nil end
  local ok_cursor, cursor = pcall(vim.api.nvim_win_get_cursor, 0)
  if not ok_cursor then return nil, nil end

  local line = cursor[1]
  local status = M._status
  local entry = status and status.entries and status.entries[line] or nil
  return entry and entry.id or nil, line
end

---@param entries DiffReviewStatusEntry[]
---@return string?
local function status_hunk_action_target_id(entries)
  local status = M._status
  if not (status and status.entries) then return nil end

  local action_ids = {}
  local has_hunk = false
  for _, entry in ipairs(entries or {}) do
    if entry.kind == "hunk" and entry.id then
      has_hunk = true
      action_ids[entry.id] = true
    end
  end
  if not has_hunk then return nil end

  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  if not (status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = cursor_line + 1, max_line do
    local entry = status.entries[line]
    if entry and entry.kind == "hunk" and entry.id and not action_ids[entry.id] then
      return entry.id
    end
  end
  for line = cursor_line - 1, 1, -1 do
    local entry = status.entries[line]
    if entry and entry.kind == "hunk" and entry.id and not action_ids[entry.id] then
      return entry.id
    end
  end
  return nil
end

---@param entries DiffReviewStatusEntry[]
---@return boolean
local function status_entries_are_headers(entries)
  if #entries == 0 then return false end
  for _, entry in ipairs(entries) do
    if not (
      entry.kind == "section"
      or entry.kind == "file"
      or entry.kind == "commit"
      or entry.kind == "commit_file"
      or entry.kind == "pr_file"
      or entry.kind == "pr_review"
      or entry.kind == "pr_review_file"
    ) then return false end
  end
  return true
end

---@param selected_entries DiffReviewStatusEntry[]
---@param action_entries DiffReviewStatusEntry[]
---@param target_section? DiffReviewStatusSectionName
---@param opts? { file_target?: "destination"|"next" }
---@return string?
local function status_action_target_id(selected_entries, action_entries, target_section, opts)
  if status_entries_are_headers(selected_entries) then
    return nil
  end
  return status_hunk_action_target_id(action_entries) or (action_entries[1] and action_entries[1].id or nil)
end

---@param target_id? string
---@return boolean
local function status_target_is_header(target_id)
  return type(target_id) == "string"
    and (
      target_id:find("^section:") ~= nil
      or target_id:find("^file:") ~= nil
      or target_id:find("^commit:") ~= nil
      or target_id:find("^commit%-file:") ~= nil
      or target_id:find("^provider%-file:") ~= nil
      or target_id:find("^pr%-review:") ~= nil
    )
end

---@param fallback_line integer
---@return integer?
local function status_nearest_header_line(fallback_line)
  local status = M._status
  if not (status and status.entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  local line = math.min(math.max(fallback_line, 1), max_line)
  local max_offset = math.max(line - 1, max_line - line)
  for offset = 0, max_offset do
    local previous_line = line - offset
    local previous_entry = status.entries[previous_line]
    if previous_entry and (
      previous_entry.kind == "section"
      or previous_entry.kind == "file"
      or previous_entry.kind == "commit"
      or previous_entry.kind == "commit_file"
      or previous_entry.kind == "pr_file"
    ) then return previous_line end
    local next_line = line + offset
    local next_entry = status.entries[next_line]
    if offset > 0 and next_entry and (
      next_entry.kind == "section"
      or next_entry.kind == "file"
      or next_entry.kind == "commit"
      or next_entry.kind == "commit_file"
      or next_entry.kind == "pr_file"
    ) then return next_line end
  end
  return nil
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
  if not target_line and not fallback_line then return end
  if not target_line and fallback_line and status_target_is_header(target_id) then
    target_line = status_nearest_header_line(fallback_line)
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
  M._status_apply_hint_bar(buf)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
---@param head_lines table[]
---@param sections DiffReviewStatusSection[]
local function status_render_loaded(buf, target_id, fallback_line, opts, head_lines, sections)
  opts = opts or {}
  if M._pr_edit.blocks_render(buf) then return end
  setup_bg_highlights()
  M._status = M._status or {}
  M._status.buf = buf
  M._status.lines = {}
  M._status.entries = {}
  M._status.highlights = {}
  M._status.line_highlights = {}
  M._status.extmarks = {}
  M._status.boundary_lines = {}

  for _, head_line in ipairs(head_lines) do
    if head_line.segments then
      status_add_segment_line(head_line.segments, head_line.entry)
    else
      status_add_segment_line(head_line)
    end
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

  M._status_apply_hint_bar(buf)
  status_restore_cursor(buf, target_id, fallback_line)

  -- Re-apply walkthrough decorations after rows shift; package.loaded guard
  -- avoids loading the module when no walkthrough has ever started.
  local walkthrough = package.loaded["diff_review.walkthrough"]
  if walkthrough then walkthrough.on_status_rendered(buf) end

  -- Rendering wipes view-specific extmarks (PR-edit regions, review comment
  -- regions); re-anchor them.
  local view_kind = M._status.view_kind or "status"
  if view_kind == "pr" then
    M._pr_edit.on_render(buf)
    M._review.on_render(buf)
    M._pr_edit.sync_modifiable(buf)
  elseif view_kind == "review" then
    M._review.on_render(buf)
  end
end

---@param cwd string
---@param buf integer
---@param force? boolean
local function status_start_pr_lookup(cwd, buf, request_id)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local status = M._status_states and M._status_states[buf] or M._status
  if not (status and status.pr_request_id == request_id and status.pr_root == cwd and status.pr) then return end
  if status.pr.lookup_started then return end
  status.pr.lookup_started = true

  local function complete_pr_lookup(result)
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.pr_request_id == request_id and latest_status.pr_root == cwd) then return end
    M._status = latest_status
    local pending_open = latest_status.pr and latest_status.pr.open_when_ready
    local pending_open_win = latest_status.pr and latest_status.pr.open_when_ready_win

    if result.ok and result.pr then
      latest_status.pr = { state = "ready", pr = result.pr, lookup_started = true }
    elseif result.ok and result.unavailable then
      latest_status.pr = { state = "unavailable", message = result.message, lookup_started = true }
    elseif result.ok then
      latest_status.pr = { state = "none", lookup_started = true }
    else
      local message = result.message or "Unable to fetch GitHub pull request"
      latest_status.pr = { state = "error", message = message, lookup_started = true }
      notify_error("GitHub PR lookup failed: " .. message)
    end

    if latest_status.head_values then
      latest_status.head_lines = status_build_head_lines(latest_status.head_values, latest_status.pr, latest_status.about)
    end
    if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
      if not M._status_patch_head_line(buf, "pr", status_pr_head_line(latest_status.pr)) then
        M.render_status(buf, nil, nil, { reuse_sections = true })
      end
    end
    if pending_open then
      vim.schedule(function()
        if not (status_open_pr and vim.api.nvim_buf_is_valid(buf)) then return end
        local current_status = M._status_states and M._status_states[buf] or M._status
        if not (current_status and current_status.pr_request_id == request_id and current_status.pr_root == cwd) then return end
        M._status = current_status

        local function open_pending_pr()
          status_open_pr(nil)
        end

        if pending_open_win and vim.api.nvim_win_is_valid(pending_open_win) then
          local ok, win_buf = pcall(vim.api.nvim_win_get_buf, pending_open_win)
          if ok and win_buf == buf then
            vim.api.nvim_win_call(pending_open_win, open_pending_pr)
            return
          end
        end
        if vim.api.nvim_get_current_buf() == buf then open_pending_pr() end
      end)
    end
  end

  local options = M.config or config.options or config.defaults
  if options.pr_lookup_mode == "mock-delay" then
    vim.defer_fn(function()
      complete_pr_lookup({ ok = true })
    end, math.max(1, tonumber(options.pr_mock_delay_ms) or 5000))
    return
  end

  gh.current_pr_async(cwd, complete_pr_lookup)
end

---@param cwd string
---@param buf integer
---@param force? boolean
---@return integer? request_id
local function status_ensure_pr_state(cwd, buf, force)
  M._status = M._status or {}
  local status = M._status
  if not force and status.pr_root == cwd and status.pr then return nil end

  status.pr_root = cwd
  status.pr = { state = "fetching" }
  status.pr_request_id = (status.pr_request_id or 0) + 1
  return status.pr_request_id
end

---@param sections DiffReviewStatusSection[]?
---@return boolean
local function status_has_changes(sections)
  for _, section in ipairs(sections or {}) do
    if #(section.files or {}) > 0 then return true end
  end
  return false
end

---@param cwd string
---@param buf integer
---@param has_changes boolean
---@param force? boolean
---@param allow_generation? boolean
local function status_ensure_about_state(cwd, buf, has_changes, force, allow_generation)
  M._status = M._status or {}
  local status = M._status
  if not has_changes then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    return
  end

  if not allow_generation and (M.config or config.options or config.defaults).about_auto_generate == false then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    status.about_pending = nil
    return
  end

  local current = ai_commit.state()
  if not force and not allow_generation and status.about_root == cwd and status.about_pending and status.about then return end
  if not force and status.about_root == cwd and status.about and current == status.about then return end

  status.about_root = cwd
  status.about = current and current.cwd == cwd and current or { state = "generating", cwd = cwd, waiters = {} }
  status.about_request_id = (status.about_request_id or 0) + 1
  local request_id = status.about_request_id
  if allow_generation then
    M._status_patch_about_line(buf)
  end

  local function start_generation()
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
    latest_status.about_pending = nil
    ai_commit.ensure(cwd, { force = force }, function(result)
      latest_status = M._status_states and M._status_states[buf] or M._status
      if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
      M._status = latest_status
      latest_status.about = result
      latest_status.about_pending = nil
      if latest_status.head_values then
        latest_status.head_lines = status_build_head_lines(latest_status.head_values, latest_status.pr, latest_status.about)
      end
      if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
        if not M._status_patch_about_line(buf) then
          M.render_status(buf, nil, nil, { reuse_sections = true })
        end
      end
    end)
  end

  local delay = allow_generation and 0 or ((M.config or config.options or config.defaults).about_auto_generate_delay_ms or 0)
  if delay > 0 then
    status.about_pending = true
    vim.defer_fn(start_generation, delay)
    return
  end

  start_generation()
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean, refresh_pr?: boolean, refresh_about?: boolean, restore_initial_folds?: boolean }
function M.render_status(buf, target_id, fallback_line, opts)
  opts = opts or {}
  setup_bg_highlights()
  if M._status_states and M._status_states[buf] then
    M._status = M._status_states[buf]
  end
  M._status = M._status or {}
  M._status.buf = buf
  M._status.reconcile_generation = (M._status.reconcile_generation or 0) + 1
  local render_state = M._status
  local preserve_current_cursor = target_id == nil and fallback_line == nil

  if opts.reuse_sections and M._status.head_lines and M._status.sections then
    if preserve_current_cursor then
      target_id, fallback_line = status_cursor_target(buf)
    end
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
    local latest_status = M._status_states and M._status_states[buf] or render_state
    if not (latest_status and latest_status.request_id == request_id) then return end
    M._status = latest_status
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      if not has_existing_view then
        status_set_plain_lines(buf, { "Not a git repository" })
      end
      return
    end

    latest_status.cwd = cwd
    local pr_request_id = status_ensure_pr_state(cwd, buf, opts.refresh_pr)

    status_load_async(cwd, function(result)
      local current_status = M._status_states and M._status_states[buf] or render_state
      if not (current_status and current_status.request_id == request_id) then return end
      M._status = current_status
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if status_operations_pending() then
        status_request_reconcile(buf, target_id)
        return
      end
      if opts.restore_initial_folds then
        M._status_restore_initial_folds(result.sections)
        target_id = M._status_first_grouping_id(result.sections)
        fallback_line = nil
      end
      status_ensure_about_state(cwd, buf, status_has_changes(result.sections), opts.refresh_about)
      result.head_lines = status_build_head_lines(result.head_values or {}, current_status.pr, current_status.about)
      current_status.head_lines = result.head_lines
      current_status.head_values = result.head_values
      current_status.sections = result.sections
      current_status.fancy_rows = {}
      if preserve_current_cursor and not opts.restore_initial_folds then
        target_id, fallback_line = status_cursor_target(buf)
      end
      status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
      if pr_request_id then
        vim.defer_fn(function()
          status_start_pr_lookup(cwd, buf, pr_request_id)
        end, 50)
      end
    end)
  end)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean, refresh_pr?: boolean, refresh_about?: boolean, restore_initial_folds?: boolean }
local function render_status_or_notify(buf, target_id, fallback_line, opts)
  local ok, err = xpcall(function()
    M.render_status(buf, target_id, fallback_line, opts)
  end, debug.traceback)
  if not ok then
    notify_error("DiffReview render failed: " .. tostring(err))
  end
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
---@param diff_text? string
local function render_pr_status(pr, cwd, buf, diff_text)
  local status = M._status
  if not (status and status.buf == buf) then return end
  if M._pr_edit.blocks_render(buf) then return end
  diff_text = diff_text or status.pr_diff_text
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.review_comments = status.pr_standalone_comments
  status.head_lines = status_pr_detail_head_lines(pr, status.pr_comments)
  status.sections = status_pr_sections(cwd, pr, diff_text, status.pr_comments, status.pr_standalone_comments)
  status.fancy_rows = {}
  status.pr_code_comments_by_anchor = M._section_builder.comment_anchor_index_from_sections(status.sections, { field = "pr_comments" })
  status.review_after_row = function(diff_line, indent)
    M._section_builder.emit_anchored_comments(status, diff_line, indent, { index_field = "pr_code_comments_by_anchor" })
  end
  status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
local function load_pr_diff(pr, cwd, buf)
  local status = M._status
  if not (status and status.buf == buf) then return end
  status.pr_diff_request_id = (status.pr_diff_request_id or 0) + 1
  local request_id = status.pr_diff_request_id
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    if result.code ~= 0 then
      notify_error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest_status.pr_diff_text = result.stdout or ""
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M._pr_overview.load_comments(pr, cwd, buf)
  local status = M._status
  if not (status and status.buf == buf and pr.repo and pr.repo ~= "") then return end
  status.pr_comments_request_id = (status.pr_comments_request_id or 0) + 1
  local request_id = status.pr_comments_request_id
  gh.pr_comments_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_comments_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    if not result.ok then
      notify_error("GitHub PR comments failed: " .. (result.message or "gh failed"), "DiffReview")
      return
    end
    latest_status.pr_comments = result
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end

-- PR review mode (":or" in the PR/status view). A review buffer
-- (view_kind = "review") shows the PR title, an editable review summary, and
-- the changed files split into Unviewed/Viewed sections. `S`/`U` move a file
-- between them, `C` posts an inline comment to GitHub at the cursor/selection,
-- `y`/`n` jump between comments, and the submit key posts the review verdict.
-- Everything hangs off the module table because init.lua is at Lua's
-- 200-local limit.
M._review = {
  ns = vim.api.nvim_create_namespace("diff_review_review"),
  comment_icon = M._comment_icon,
  reply_icon = M._reply_icon,
  -- Test seams: the comment-body input and the verdict picker. Defaults open
  -- real UI; tests override them. Mirrors the set_backend/set_reader pattern.
  input_provider = nil, ---@type (fun(title: string, on_submit: fun(text: string)))?
  verdict_provider = nil, ---@type (fun(on_choice: fun(event: string?)))?
  data_dir_for_test = nil,
}

---@param buf integer
---@return table? state the review-capable status-state, or nil if buf has no editable inline comments
function M._review.state(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if status and status.view_kind == "review" then return status end
  if status and status.view_kind == "pr" and status.pr_standalone_comments then
    status.review_comments = status.pr_standalone_comments
    return status
  end
  return nil
end

---@return DiffReviewReviewKeymapConfig
function M._review.keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.review), keymaps.review or {})
end

---@param id string
---@return string[]
function M._review.keys_for(id)
  return status_keys_for(id)
end

---@param path string?
function M._review.set_data_dir_for_test(path)
  M._review.data_dir_for_test = path
end

---@return string
function M._review.data_dir()
  return M._review.data_dir_for_test
    or vim.fs.joinpath(vim.fn.stdpath("data"), "gitstatus", "reviews")
end

---@param state table
---@return string
function M._review.storage_repo(state)
  local pr = state.pr or {}
  local repo = type(pr.repo) == "string" and vim.trim(pr.repo) or ""
  if repo ~= "" then return repo end
  local url = type(pr.url) == "string" and pr.url or ""
  local owner, name = url:match("github%.com[:/]([^/]+)/([^/]+)/pull/%d+")
  if owner and name then return owner .. "/" .. name end
  return "unknown/" .. vim.fn.sha256(tostring(state.cwd or ""))
end

---@param segment string|number?
---@return string
function M._review.storage_segment(segment)
  local text = vim.trim(tostring(segment or ""))
  if text == "" then text = "current" end
  text = text:gsub("[<>:\"/\\|?*%c]", "-"):gsub("^%.+$", "-")
  return text
end

---@param state table
---@return string[]
function M._review.storage_segments(state)
  local segments = {}
  for _, segment in ipairs(vim.split(M._review.storage_repo(state), "/", { plain = true, trimempty = true })) do
    segments[#segments + 1] = M._review.storage_segment(segment)
  end
  segments[#segments + 1] = M._review.storage_segment((state.pr or {}).number)
  return segments
end

---@param state table
---@return string
function M._review.storage_path(state)
  local parts = { M._review.data_dir() }
  vim.list_extend(parts, M._review.storage_segments(state))
  parts[#parts + 1] = "review.json"
  return vim.fs.joinpath(unpack(parts))
end

---@param state table
function M._review.load_draft(state)
  local path = M._review.storage_path(state)
  if vim.uv.fs_stat(path) == nil then return end
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return end
  local decoded_ok, draft = pcall(vim.json.decode, table.concat(lines, "\n"))
  if not decoded_ok or type(draft) ~= "table" then return end
  if type(draft.commit_id) == "string" then state.review_draft_commit_id = draft.commit_id end
  if type(draft.review_viewed) == "table" then state.review_viewed = draft.review_viewed end
  if type(draft.review_viewed_hunks) == "table" then state.review_viewed_hunks = draft.review_viewed_hunks end
  if type(draft.review_comment_text) == "string" then state.review_comment_text = draft.review_comment_text end
  if type(draft.review_comments) == "table" then state.review_comments = draft.review_comments end
  if type(draft.review_remote) == "table" then state.review_remote = draft.review_remote end
end

---@param state table
---@return table[]
function M._review.comments_for_storage(state)
  local comments = {}
  for _, comment in ipairs(state.review_comments or {}) do
    local copy = vim.deepcopy(comment)
    copy.syncing = nil
    copy.review_body_start = nil
    copy.review_body_end = nil
    copy.review_header_mark = nil
    copy.review_footer_mark = nil
    copy.review_reply_header_marks = nil
    copy.review_body_line_marks = nil
    copy.review_body_render_rows = nil
    copy.review_body_render_row_count = nil
    copy.review_body_prefix_width = nil
    copy.review_rendered_body_text = nil
    comments[#comments + 1] = copy
  end
  return comments
end

---@param state table
function M._review.save_draft(state)
  local path = M._review.storage_path(state)
  local directory = vim.fs.dirname(path) or M._review.data_dir()
  local mkdir_ok, mkdir_err = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok or mkdir_err == 0 then
    notify_error("Could not create review draft directory: " .. directory, "DiffReview")
    return
  end
  local payload = {
    cwd = state.cwd,
    repo = state.pr and state.pr.repo or nil,
    number = state.pr and state.pr.number or nil,
    commit_id = state.commit_id,
    review_viewed = state.review_viewed or {},
    review_viewed_hunks = state.review_viewed_hunks or {},
    review_comment_text = state.review_comment_text or "",
    review_comments = M._review.comments_for_storage(state),
    review_remote = state.review_remote or {},
  }
  local encode_ok, encoded = pcall(vim.json.encode, payload)
  if not encode_ok then
    notify_error("Could not encode review draft", "DiffReview")
    return
  end
  local write_ok, write_err = pcall(vim.fn.writefile, { encoded }, path)
  if not write_ok or write_err ~= 0 then
    notify_error("Could not write review draft", "DiffReview")
  end
end

---@param state table
function M._review.delete_draft(state)
  pcall(vim.fn.delete, M._review.storage_path(state))
end

---@param state table
---@param comment table
---@return table
function M._review.normalize_comment(state, comment)
  if not comment.local_id or comment.local_id == "" then
    local seed = table.concat({
      tostring(state.pr and state.pr.repo or ""),
      tostring(state.pr and state.pr.number or ""),
      tostring(comment.path or ""),
      tostring(comment.line or ""),
      tostring(comment.position or ""),
      tostring(comment.body or ""),
      tostring(vim.uv.hrtime()),
    }, "\n")
    comment.local_id = vim.fn.sha256(seed):sub(1, 16)
  end
  if (not comment.abs_file or comment.abs_file == "") and comment.path and comment.path ~= "" then
    comment.abs_file = vim.fs.normalize(vim.fs.joinpath(state.cwd, comment.path))
  end
  comment.side = comment.side or "RIGHT"
  comment.local_state = comment.local_state or (comment.remote_id and "clean" or "new")
  if comment.base_body == nil then comment.base_body = comment.remote_id and comment.body or "" end
  if type(comment.user) == "table" then comment.user = comment.user.login end
  if not comment.user or comment.user == "" then
    local author = type(comment.author) == "table" and comment.author or nil
    local remote_user = type(state.review_remote) == "table" and state.review_remote.user or nil
    comment.user = comment.author_login or (author and author.login) or (remote_user and remote_user.login) or "you"
  end
  comment.created_at = comment.created_at or comment.createdAt or os.date("%Y-%m-%d %H:%M")
  comment.updated_at = comment.updated_at or comment.updatedAt or comment.created_at
  local replies = {}
  for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    if type(reply) == "table" then
      if type(reply.user) == "table" then reply.user = reply.user.login end
      if not reply.user or reply.user == "" then
        local reply_author = type(reply.author) == "table" and reply.author or nil
        reply.user = reply.author_login or (reply_author and reply_author.login) or "unknown"
      end
      reply.created_at = reply.created_at or reply.createdAt or comment.created_at
      reply.updated_at = reply.updated_at or reply.updatedAt or reply.created_at
      replies[#replies + 1] = reply
    end
  end
  comment.replies = replies
  return comment
end

---@param state table
function M._review.normalize_comments(state)
  for _, comment in ipairs(state.review_comments or {}) do
    M._review.normalize_comment(state, comment)
  end
end

---@param state table
---@param remote table
---@return table
function M._review.comment_from_remote(state, remote)
  return M._review.normalize_comment(state, {
    local_id = remote.remote_node_id or ("remote:" .. tostring(remote.remote_id or "")),
    remote_id = remote.remote_id,
    remote_node_id = remote.remote_node_id,
    remote_review_id = remote.review_id or (state.review_remote and state.review_remote.id),
    path = remote.path,
    abs_file = remote.path and vim.fs.normalize(vim.fs.joinpath(state.cwd, remote.path)) or nil,
    side = "RIGHT",
    line = remote.line,
    position = remote.position,
    body = remote.body or "",
    base_body = remote.body or "",
    user = remote.user,
    created_at = remote.created_at,
    updated_at = remote.updated_at,
    replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or {},
    local_state = "clean",
  })
end

---@param comment table
---@return string
function M._review.comment_fingerprint(comment)
  return table.concat({
    tostring(comment.path or ""),
    tostring(comment.side or ""),
    tostring(comment.line or ""),
    tostring(comment.position or ""),
  }, "\t")
end

---@param state table
---@param remote table
---@return table?
function M._review.find_comment_for_remote(state, remote)
  for _, comment in ipairs(state.review_comments or {}) do
    if remote.remote_node_id and comment.remote_node_id == remote.remote_node_id then return comment end
    if remote.remote_id and tonumber(comment.remote_id) == tonumber(remote.remote_id) then return comment end
  end
  local remote_fingerprint = M._review.comment_fingerprint({
    path = remote.path,
    side = "RIGHT",
    line = remote.line,
    position = remote.position,
  })
  for _, comment in ipairs(state.review_comments or {}) do
    if not comment.remote_id
      and comment.local_state ~= "deleted"
      and M._review.comment_fingerprint(comment) == remote_fingerprint
      and M._review.comment_body_for_sync(comment.body or "") == tostring(remote.body or "") then
      return comment
    end
  end
  return nil
end

---@param state table
---@param remote_comments table[]
---@return boolean changed
function M._review.merge_remote_comments(state, remote_comments)
  M._review.normalize_comments(state)
  local changed = false
  for _, remote in ipairs(remote_comments or {}) do
    local comment = M._review.find_comment_for_remote(state, remote)
    if comment then
      comment.remote_id = remote.remote_id or comment.remote_id
      comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
      comment.remote_review_id = remote.review_id or comment.remote_review_id
      comment.position = remote.position or comment.position
      comment.line = remote.line or comment.line
      comment.user = remote.user or comment.user
      comment.created_at = remote.created_at or comment.created_at
      comment.updated_at = remote.updated_at or comment.updated_at
      local remote_replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or {}
      if not vim.deep_equal(comment.replies or {}, remote_replies) then
        comment.replies = remote_replies
        changed = true
      end
      local remote_body = tostring(remote.body or "")
      local base_body = tostring(comment.base_body or "")
      local local_body = tostring(comment.body or "")
      local base_sync_body = M._review.comment_body_for_sync(base_body)
      local local_sync_body = M._review.comment_body_for_sync(local_body)
      if comment.local_state == "dirty" or comment.local_state == "new" then
        if remote_body == local_sync_body then
          comment.base_body = remote_body
          comment.local_state = "clean"
          changed = true
        elseif base_body ~= "" and remote_body ~= base_sync_body then
          comment.remote_body = remote_body
          comment.local_state = "conflict"
          changed = true
        end
      elseif comment.local_state ~= "deleted" and local_sync_body ~= remote_body then
        comment.body = remote_body
        comment.base_body = remote_body
        comment.local_state = "clean"
        changed = true
      end
    else
      state.review_comments[#state.review_comments + 1] = M._review.comment_from_remote(state, remote)
      changed = true
    end
  end
  return changed
end

---@param state table
---@param result table
---@return boolean ok
---@return boolean changed
function M._review.apply_remote_review_result(state, result)
  if not result.ok then return false, false end
  local comment_count = type(result.comments) == "table" and #result.comments or 0
  if result.review then
    notify_debug(
      ("Pending review %s found; importing %d comment%s"):format(
        tostring(result.review.id or result.review.node_id or "?"),
        comment_count,
        comment_count == 1 and "" or "s"
      ),
      vim.log.levels.INFO,
      { title = "DiffReview" }
    )
  else
    notify_debug("No pending review found", vim.log.levels.INFO, { title = "DiffReview" })
  end
  local changed = false
  if result.review then
    state.review_remote = result.review
    changed = true
  elseif type(state.review_remote) == "table" and (state.review_remote.id or state.review_remote.node_id) then
    state.review_remote = nil
    changed = true
  end
  if M._review.merge_remote_comments(state, result.comments or {}) then changed = true end
  return true, changed
end

---@param buf integer
---@param cb fun(ok: boolean)
function M._review.load_remote_before_open(buf, cb)
  local state = M._review.state(buf)
  if not (state and state.pr and state.pr.repo and state.pr.repo ~= "") then
    notify_debug("PR review sync skipped: missing owner/repo", vim.log.levels.WARN, { title = "DiffReview" })
    cb(true)
    return
  end
  state.review_sync_request_id = (state.review_sync_request_id or 0) + 1
  local request_id = state.review_sync_request_id
  gh.pending_review_async(state.cwd, state.pr.number, state.pr.repo, function(result)
    local latest = M._review.state(buf)
    if not (latest and latest.review_sync_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    local ok, changed = M._review.apply_remote_review_result(latest, result)
    if not ok then
      notify_error("PR review sync failed: " .. (result.message or "gh failed"), "DiffReview")
      cb(false)
      return
    end
    if changed then M._review.save_draft(latest) end
    cb(true)
  end)
end

---@param state table
---@param cb fun(ok: boolean)
function M._review.ensure_remote_review(state, cb)
  local remote = state.review_remote or {}
  if remote.id and remote.node_id and remote.node_id ~= "" then
    cb(true)
    return
  end
  gh.create_pending_review_async(state.cwd, state.pr.number, state.pr.repo, { commit_id = state.commit_id }, function(result)
    if result.ok and result.review then
      state.review_remote = result.review
      M._review.save_draft(state)
      cb(true)
      return
    end
    notify_error("PR review sync failed: " .. (result.message or "could not create pending review"), "DiffReview")
    cb(false)
  end)
end

---@param buf integer
function M._review.process_sync_queue(buf)
  local state = M._review.state(buf)
  if not state then return end
  state.review_sync = state.review_sync or { queue = {}, running = false, waiters = {} }
  if state.review_sync.running then return end
  local item = table.remove(state.review_sync.queue, 1)
  if not item then
    local waiters = state.review_sync.waiters or {}
    state.review_sync.waiters = {}
    for _, waiter in ipairs(waiters) do waiter(true) end
    return
  end
  state.review_sync.running = true
  local function finish(ok)
    state.review_sync.running = false
    if not ok then
      if item.op == "upsert" and item.comment then
        item.comment.syncing = nil
        M._review.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M._review.render_preserving_inline_cursor(buf) end
      elseif item.op == "delete" and item.comment and item.comment.local_state == "deleted" then
        item.comment.local_state = "clean"
        M._review.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M._review.render_preserving_inline_cursor(buf) end
      end
      local waiters = state.review_sync.waiters or {}
      state.review_sync.waiters = {}
      for _, waiter in ipairs(waiters) do waiter(false) end
      return
    end
    M._review.process_sync_queue(buf)
  end
  M._review.ensure_remote_review(state, function(ok)
    if not ok then finish(false) return end
    local comment = item.comment
    if item.op == "delete" then
      if not comment.remote_node_id then
        finish(true)
        return
      end
      gh.delete_review_comment_async(state.cwd, comment.remote_node_id, function(result)
        if not result.ok then
          notify_error("PR review comment delete failed: " .. (result.message or "gh failed"), "DiffReview")
          finish(false)
          return
        end
        for index, existing in ipairs(state.review_comments or {}) do
          if existing == comment then
            table.remove(state.review_comments, index)
            break
          end
        end
        M._review.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M._review.render(buf) end
        finish(true)
      end)
      return
    end
    if comment.local_state == "deleted" then finish(true) return end
    local queued_body = item.body or M._review.comment_body_for_sync(comment.body or "")
    if comment.remote_node_id then
      gh.update_review_comment_async(state.cwd, comment.remote_node_id, queued_body, function(result)
        if not result.ok then
          notify_error("PR review comment update failed: " .. (result.message or "gh failed"), "DiffReview")
          finish(false)
          return
        end
        local remote = result.comments and result.comments[1] or nil
        if remote then
          comment.user = remote.user or comment.user
          comment.created_at = remote.created_at or comment.created_at
          comment.updated_at = remote.updated_at or comment.updated_at
        end
        comment.syncing = nil
        if M._review.comment_body_for_sync(comment.body or "") == queued_body then
          comment.local_state = "clean"
          comment.base_body = queued_body
        else
          comment.local_state = comment.remote_node_id and "dirty" or "new"
        end
        M._review.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M._review.render_preserving_inline_cursor(buf) end
        finish(true)
      end)
      return
    end
    if not comment.position then
      notify_error("PR review comment sync failed: missing diff position for " .. tostring(comment.path or ""), "DiffReview")
      finish(false)
      return
    end
    gh.add_pending_review_comment_async(state.cwd, state.pr.number, state.pr.repo, state.review_remote.id, {
      body = queued_body,
      path = comment.path,
      line = comment.line,
      side = comment.side,
      start_line = comment.start_line,
      start_side = comment.start_side,
      position = comment.position,
    }, function(result)
      if not result.ok then
        notify_error("PR review comment create failed: " .. (result.message or "gh failed"), "DiffReview")
        finish(false)
        return
      end
      local remote = result.comments and result.comments[1] or nil
      if remote then
        comment.remote_id = remote.remote_id
        comment.remote_node_id = remote.remote_node_id
        comment.remote_review_id = state.review_remote.id
        comment.line = remote.line or comment.line
        comment.position = remote.position or comment.position
        comment.user = remote.user or comment.user
        comment.created_at = remote.created_at or comment.created_at
        comment.updated_at = remote.updated_at or comment.updated_at
      end
      comment.syncing = nil
      if M._review.comment_body_for_sync(comment.body or "") == queued_body then
        comment.local_state = "clean"
        comment.base_body = queued_body
      else
        comment.local_state = "dirty"
      end
      M._review.save_draft(state)
      if vim.api.nvim_buf_is_valid(buf) then M._review.render_preserving_inline_cursor(buf) end
      finish(true)
    end)
  end)
end

---@param buf integer
---@param op "upsert"|"delete"
---@param comment table
function M._review.enqueue_sync(buf, op, comment)
  local state = M._review.state(buf)
  if not state then return end
  state.review_sync = state.review_sync or { queue = {}, running = false, waiters = {} }
  local sync_body = op == "upsert" and M._review.comment_body_for_sync(comment.body or "") or nil
  if op == "upsert" then
    for _, queued in ipairs(state.review_sync.queue) do
      if queued.op == "upsert" and queued.comment == comment then
        queued.body = sync_body
        return
      end
    end
  end
  state.review_sync.queue[#state.review_sync.queue + 1] = { op = op, comment = comment, body = sync_body }
  M._review.process_sync_queue(buf)
end

---@param comment table
---@return boolean
function M._review.comment_needs_sync(comment)
  if not comment or comment.syncing then return false end
  if comment.local_state == "deleted" then return comment.remote_node_id ~= nil end
  if not comment.remote_node_id and M._review.comment_body_for_sync(comment.body or "") == "" then return false end
  if comment.local_state == "new" or comment.local_state == "dirty" then return true end
  return not comment.remote_node_id
end

---@param comment table
---@return boolean
function M._review.comment_has_dirty_marker(comment)
  if not comment or comment.syncing or comment.local_state == "deleted" then return false end
  return M._review.comment_needs_sync(comment)
end

---@param buf integer
---@return integer count
function M._review.enqueue_dirty_comments(buf)
  local state = M._review.state(buf)
  if not state then return 0 end
  local items = {}
  local render_marker_change = false
  for _, comment in ipairs(state.review_comments or {}) do
    if M._review.comment_needs_sync(comment) then
      if comment.local_state == "deleted" then
        items[#items + 1] = { op = "delete", comment = comment }
      else
        comment.syncing = true
        render_marker_change = true
        items[#items + 1] = { op = "upsert", comment = comment }
      end
    end
  end
  if #items == 0 then return 0 end
  if render_marker_change then M._review.render_preserving_inline_cursor(buf) end
  for _, item in ipairs(items) do
    M._review.enqueue_sync(buf, item.op, item.comment)
  end
  return #items
end

---@param buf integer
---@param cb fun(ok: boolean)
function M._review.flush_sync(buf, cb)
  local state = M._review.state(buf)
  if not state then cb(false) return end
  M._review.enqueue_dirty_comments(buf)
  state.review_sync = state.review_sync or { queue = {}, running = false, waiters = {} }
  if not state.review_sync.running and #state.review_sync.queue == 0 then
    cb(true)
    return
  end
  state.review_sync.waiters[#state.review_sync.waiters + 1] = cb
end

---@param buf integer
function M._review.sync_remote(buf)
  local state = M._review.state(buf)
  if not (state and state.pr and state.pr.repo and state.pr.repo ~= "") then return end
  state.review_sync_request_id = (state.review_sync_request_id or 0) + 1
  local request_id = state.review_sync_request_id
  gh.pending_review_async(state.cwd, state.pr.number, state.pr.repo, function(result)
    local latest = M._review.state(buf)
    if not (latest and latest.review_sync_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    if not result.ok then
      notify_error("PR review sync failed: " .. (result.message or "gh failed"), "DiffReview")
      return
    end
    local _, changed = M._review.apply_remote_review_result(latest, result)
    if changed then
      M._review.save_draft(latest)
      M._review.render(buf)
    end
  end)
end

function M._review.leave_visual()
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  end
end

---@param dl table diff_line meta
---@return "LEFT"|"RIGHT"
function M._review.side_of(dl)
  return dl.side == "left" and "LEFT" or "RIGHT"
end

---@param path? string
---@param side? string
---@param line? integer|string
---@param end_line? integer|string
---@return string?
function M._review.github_diff_fragment(path, side, line, end_line)
  local line_number = tonumber(line)
  if not (path and path ~= "" and line_number) then return nil end
  local normalized_path = tostring(path):gsub("\\", "/")
  local side_prefix = side == "LEFT" and "L" or "R"
  local start_number = math.floor(line_number)
  local stop_number = tonumber(end_line)
  if stop_number then
    local finish_number = math.floor(stop_number)
    if finish_number ~= start_number then
      if finish_number < start_number then start_number, finish_number = finish_number, start_number end
      return ("diff-%s%s%d-%s%d"):format(vim.fn.sha256(normalized_path), side_prefix, start_number, side_prefix, finish_number)
    end
  end
  return ("diff-%s%s%d"):format(vim.fn.sha256(normalized_path), side_prefix, start_number)
end

---@param state table
---@return string?
function M._review.visual_browse_fragment(state)
  local mode = vim.fn.mode()
  if not (mode == "v" or mode == "V" or mode:byte() == 22) then return nil end

  local start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
  if start_row > end_row then start_row, end_row = end_row, start_row end

  local first, last
  for row = start_row, end_row do
    local entry = state.entries and state.entries[row] or nil
    local diff_line = entry and entry.diff_line or nil
    if diff_line then
      first = first or diff_line
      last = diff_line
    end
  end
  if not (first and last and first.file == last.file and first.side == last.side) then return nil end

  local path = repo_relative(first.file, state.cwd) or first.file
  return M._review.github_diff_fragment(path, M._review.side_of(first), first.line, last.line)
end

---@param buf integer
---@return string?
function M._review.browse_fragment_under_cursor(buf)
  local state = M._review.state(buf)
  if not state then return nil end
  local visual_fragment = M._review.visual_browse_fragment(state)
  if visual_fragment then return visual_fragment end

  local row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = state.entries and state.entries[row] or nil

  local function comment_fragment(comment)
    if not comment then return nil end
    local remote_id = tonumber(comment.remote_id)
    if remote_id then return ("r%d"):format(math.floor(remote_id)) end
    return M._review.github_diff_fragment(comment.path, comment.side, comment.line)
  end

  local body_comment = M._review.comment_body_at_row(buf, row)
  if body_comment then return comment_fragment(body_comment) end

  if not entry then return nil end

  if entry.kind == "review_comment" and entry.review_reply then
    return comment_fragment(entry.review_reply)
  end

  if entry.kind == "review_comment" and entry.review_comment then
    return comment_fragment(entry.review_comment)
  end

  local diff_line = entry.diff_line
  if diff_line then
    local path = repo_relative(diff_line.file, state.cwd) or diff_line.file
    return M._review.github_diff_fragment(path, M._review.side_of(diff_line), diff_line.line)
  end

  return nil
end

---@param state table
---@return DiffReviewStatusHeadLine[]
function M._review.head_lines(state)
  local pr = state.pr
  local title = pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))
  local lines = {
    { segments = { { "Title: ", "DiffReviewStatusLabel" }, { title, "DiffReviewStatusPath" } } },
    { segments = { { "Review Comment:", "DiffReviewReviewCommentHeader" } } },
  }
  local comment_lines = vim.split(state.review_comment_text or "", "\n", { plain = true })
  if #comment_lines == 0 then comment_lines = { "" } end
  for _, line in ipairs(comment_lines) do
    lines[#lines + 1] = { segments = { { line, "DiffReviewReviewComment" } } }
  end
  return lines
end

---@param diff string?
---@return string
function M._review.normalized_hunk_diff(diff)
  local body = {}
  local in_hunk = false
  for line in tostring(diff or ""):gmatch("[^\n]+") do
    if line:match("^@@") then
      local context = line:match("^@@.-@@%s*(.*)$") or ""
      body[#body + 1] = "@@ " .. context
      in_hunk = true
    elseif in_hunk and line:match("^[ +%-]") then
      body[#body + 1] = line
    end
  end
  if #body == 0 then return tostring(diff or "") end
  return table.concat(body, "\n")
end

---@param state table
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return string
function M._review.hunk_view_key(state, file, hunk)
  local relpath = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename
  return "hunk:" .. vim.fn.sha256(relpath .. "\n" .. M._review.normalized_hunk_diff(hunk.diff))
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param section_name string
---@return DiffReviewStatusFile
function M._review.file_with_hunks(file, hunks, section_name)
  return M._section_builder.file_with_hunks(file, hunks, section_name)
end

---@param state table
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param key string
---@return boolean
function M._review.hunk_is_viewed(state, file, hunk, key)
  state.review_viewed_hunks = state.review_viewed_hunks or {}
  if state.review_viewed_hunks[key] ~= nil then return true end
  local relpath = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename
  if state.review_viewed[relpath] and state.review_draft_commit_id == state.commit_id then
    state.review_viewed_hunks[key] = {
      file = relpath,
      commit_id = state.commit_id,
    }
    state.review_viewed_dirty = true
    return true
  end
  return false
end

---@param state table
---@param active_keys table<string, boolean>
function M._review.prune_viewed_hunks(state, active_keys)
  state.review_viewed_hunks = state.review_viewed_hunks or {}
  for key in pairs(state.review_viewed_hunks) do
    if not active_keys[key] then
      state.review_viewed_hunks[key] = nil
      state.review_viewed_dirty = true
    end
  end
end

---@param state table
---@return DiffReviewStatusSection[]
function M._review.sections(state)
  local files = M._section_builder.files_from_diff(state.cwd, {
    section_name = "review",
    default_status = "",
    files = state.pr.files,
  }, state.diff_text or "")
  local unviewed, viewed = {}, {}
  local active_keys = {}
  for _, file in ipairs(files) do
    local file_viewed_hunks = {}
    local file_unviewed_hunks = {}
    local hunks = file.hunks or {}
    if #hunks == 0 then
      if state.review_viewed[file.relpath] then
        viewed[#viewed + 1] = file
      else
        unviewed[#unviewed + 1] = file
      end
    else
      for _, hunk in ipairs(hunks) do
        local key = M._review.hunk_view_key(state, file, hunk)
        active_keys[key] = true
        if M._review.hunk_is_viewed(state, file, hunk, key) then
          file_viewed_hunks[#file_viewed_hunks + 1] = hunk
        else
          file_unviewed_hunks[#file_unviewed_hunks + 1] = hunk
        end
      end
      if #file_unviewed_hunks > 0 then
        unviewed[#unviewed + 1] = M._review.file_with_hunks(file, file_unviewed_hunks, "review:unviewed")
      end
      if #file_viewed_hunks > 0 then
        viewed[#viewed + 1] = M._review.file_with_hunks(file, file_viewed_hunks, "review:viewed")
      end
    end
  end
  if state.diff_text and state.diff_text ~= "" then
    M._review.prune_viewed_hunks(state, active_keys)
  end
  return {
    M._section_builder.section_from_files("Unviewed Changes", unviewed, {
      name = "review:unviewed",
      keep_empty = true,
      file_key_prefix = "review:unviewed",
      file_entry_kind = "pr_file",
      hunk_entry_kind = "pr_hunk",
    }),
    M._section_builder.section_from_files("Viewed Changes", viewed, {
      name = "review:viewed",
      keep_empty = true,
      file_key_prefix = "review:viewed",
      file_entry_kind = "pr_file",
      hunk_entry_kind = "pr_hunk",
    }),
  }
end

--- Files fold by default; a review wants every diff open so the reviewer can
--- read and comment. Expand each file once (user <Tab> toggles then persist).
---@param state table
function M._review.ensure_expanded(state)
  if state.review_expanded or not state.diff_text or state.diff_text == "" then return end
  state.review_expanded = true
  local files = M._section_builder.files_from_diff(state.cwd, {
    section_name = "review",
    default_status = "",
    files = state.pr.files,
  }, state.diff_text)
  for _, file in ipairs(files) do
    for _, prefix in ipairs({ "review:unviewed", "review:viewed" }) do
      set_status_folded(status_provider_file_key(prefix, file.filename), false)
    end
  end
end

---@param buf integer
function M._review.render(buf)
  local state = M._review.state(buf)
  if not state then return end
  M._status = state
  M._review.ensure_expanded(state)
  state.head_lines = M._review.head_lines(state)
  state.sections = M._review.sections(state)
  if state.review_viewed_dirty then
    state.review_viewed_dirty = nil
    M._review.save_draft(state)
  end
  state.fancy_rows = {}
  state.review_rendered_comment_count = 0
  state.review_comment_anchor_index = M._section_builder.comment_anchor_index(state.review_comments)
  -- status_add_fancy_row consults this hook to interleave comment lines.
  state.review_after_row = function(diff_line, indent)
    M._review.emit_comments_for(state, diff_line, indent)
  end
  state.review_rendering = true
  status_render_loaded(buf, nil, nil, { reuse_sections = true }, state.head_lines, state.sections)
  state.review_rendering = nil
  local total_comments = 0
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then total_comments = total_comments + 1 end
  end
  notify_debug(
    ("Rendered %d/%d review comment%s"):format(
      state.review_rendered_comment_count or 0,
      total_comments,
      total_comments == 1 and "" or "s"
    ),
    vim.log.levels.INFO,
    { title = "DiffReview" }
  )
end

---@param buf integer
function M._review.render_preserving_inline_cursor(buf)
  local comment
  local snapshot
  if buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    comment = M._review.comment_body_at_row(buf, cursor[1])
    if comment then snapshot = M._review.inline_comment_cursor_snapshot(buf, comment) end
  end
  M._review.render(buf)
  if comment and snapshot and comment.local_state ~= "deleted" then
    M._review.restore_inline_comment_cursor(buf, comment, snapshot)
  end
end

---@param value any
---@return string
function M._review.format_comment_datetime(value)
  local text = tostring(value or "")
  local year, month, day, hour, minute = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)T(%d%d):(%d%d)")
  if year then return ("%s-%s-%s %s:%s"):format(year, month, day, hour, minute) end
  if text ~= "" then return text end
  return os.date("%Y-%m-%d %H:%M")
end

---@param text string
---@param width integer
---@return string
function M._review.truncate_display(text, width)
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= width then return text end
  local output = ""
  local output_width = 0
  local char_count = vim.fn.strchars(text)
  for char_index = 0, char_count - 1 do
    local char = vim.fn.strcharpart(text, char_index, 1)
    local char_width = vim.fn.strdisplaywidth(char)
    if output_width + char_width > width then break end
    output = output .. char
    output_width = output_width + char_width
  end
  return output
end

---@param comment table
---@return string left_text
---@return string right_text
function M._review.comment_header_text(comment)
  local user = tostring(comment.user or "you")
  local datetime = M._review.format_comment_datetime(comment.updated_at or comment.created_at)
  local marker = M._review.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s%s | %s "):format(M._review.comment_icon, marker, user, datetime)
  local line_number = tonumber(comment.line)
  local right_text = line_number and (" L%d"):format(line_number) or " L?"
  return left_text, right_text
end

---@param reply table
---@return string left_text
---@return string right_text
function M._review.reply_header_text(reply)
  local user = tostring(reply.user or "unknown")
  local datetime = M._review.format_comment_datetime(reply.updated_at or reply.created_at)
  return ("%s %s | %s "):format(M._review.reply_icon, user, datetime), ""
end

---@param win integer?
---@param buf integer?
---@return integer
function M._review.comment_rule_width(win, buf)
  local width = tonumber(vim.o.columns) or 80
  if win and win > 0 and vim.api.nvim_win_is_valid(win) then
    width = vim.api.nvim_win_get_width(win)
    local wininfo = vim.fn.getwininfo(win)[1]
    local textoff = tonumber(wininfo and wininfo.textoff) or 0
    width = width - textoff
  else
    local status = M._status
    buf = buf or (status and status.buf) or nil
    if buf and vim.api.nvim_buf_is_valid(buf) then
      local displayed_win = vim.fn.bufwinid(buf)
      if displayed_win and displayed_win > 0 and vim.api.nvim_win_is_valid(displayed_win) then
        width = vim.api.nvim_win_get_width(displayed_win)
        local wininfo = vim.fn.getwininfo(displayed_win)[1]
        local textoff = tonumber(wininfo and wininfo.textoff) or 0
        width = width - textoff
      end
    end
  end
  return math.max(40, width - 1)
end

---@param left_text string
---@param right_text string
---@param win integer?
---@param buf integer?
---@return string
function M._review.comment_rule_line(left_text, right_text, win, buf)
  local width = M._review.comment_rule_width(win, buf)
  left_text = tostring(left_text or "")
  right_text = tostring(right_text or "")
  local fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  if fixed_width > width then
    local available_left_width = math.max(0, width - vim.fn.strdisplaywidth(right_text))
    left_text = M._review.truncate_display(left_text, available_left_width)
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  if fixed_width > width then
    right_text = M._review.truncate_display(right_text, math.max(0, width - vim.fn.strdisplaywidth(left_text)))
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  return left_text .. ("-"):rep(math.max(width - fixed_width, 0)) .. right_text
end

---@param comment table
---@param win integer?
---@param buf integer?
---@return string
function M._review.comment_header_line(comment, win, buf)
  local left_text, right_text = M._review.comment_header_text(comment)
  return M._review.comment_rule_line(left_text, right_text, win, buf)
end

---@param reply table
---@param win integer?
---@param buf integer?
---@return string
function M._review.reply_header_line(reply, win, buf)
  local left_text, right_text = M._review.reply_header_text(reply)
  return M._review.comment_rule_line(left_text, right_text, win, buf)
end

---@param win integer?
---@param buf integer?
---@return string
function M._review.comment_footer_line(win, buf)
  local width = M._review.comment_rule_width(win, buf)
  return ("-"):rep(width)
end

---@param body string
---@return string
function M._review.comment_preview_text(body)
  local parts = {}
  for _, line in ipairs(vim.split(tostring(body or ""), "\n", { plain = true })) do
    local trimmed = vim.trim(line)
    if trimmed ~= "" then parts[#parts + 1] = trimmed end
  end
  if #parts == 0 then return "" end
  return table.concat(parts, " "):gsub("%s+", " ")
end

---@param text string
---@param width integer
---@return string
function M._review.truncate_preview_text(text, width)
  text = tostring(text or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= width then return text end
  if width <= 4 then return M._review.truncate_display(text, width) end
  return M._review.truncate_display(text, width - 4) .. " ..."
end

---@param comment table
---@param win integer?
---@param buf integer?
---@return string
function M._review.comment_folded_line(comment, win, buf)
  local user = tostring(comment.user or "you")
  local datetime = M._review.format_comment_datetime(comment.updated_at or comment.created_at)
  local marker = M._review.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s%s | %s | "):format(M._review.comment_icon, marker, user, datetime)
  local width = M._review.comment_rule_width(win, buf)
  local preview_width = math.max(0, width - vim.fn.strdisplaywidth(left_text))
  local preview = M._review.truncate_preview_text(M._review.comment_preview_text(comment.body or ""), preview_width)
  return M._review.truncate_display(left_text .. preview, width)
end

---@param body string
---@return string[]
function M._review.comment_body_lines(body)
  local lines = vim.split(tostring(body or ""), "\n", { plain = true })
  if #lines == 0 then lines = { "" } end
  return lines
end

---@param comment table
---@param comment_index integer
---@param reply table
---@param reply_index integer
function M._review.emit_comment_reply(comment, comment_index, reply, reply_index)
  local header_entry = {
    kind = "review_comment",
    review_comment = comment,
    comment_index = comment_index,
    review_reply = reply,
    review_reply_index = reply_index,
    review_boundary = "reply_header",
  }
  local header = M._review.reply_header_line(reply)
  local header_line = status_add_line(header, header_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(header_line, 0, #header, "DiffReviewReviewCommentHeader")

  for body_index, line in ipairs(M._review.comment_body_lines(reply.body or "")) do
    local body_entry = {
      kind = "review_comment",
      review_comment = comment,
      comment_index = comment_index,
      review_reply = reply,
      review_reply_index = reply_index,
      review_reply_body = true,
      review_reply_body_index = body_index,
    }
    local body_line = status_add_line(line, body_entry)
    if line ~= "" then status_add_highlight(body_line, 0, #line, "DiffReviewReviewComment") end
  end
end

---Emit a draft comment as plain, navigable buffer lines below its anchor diff
---row. Header/footer are read-only rule rows; body rows are full-width editable
---buffer lines with no prefixes, virtual borders, or render wrapping.
---@param comment table
---@param index integer comment number (1-based)
---@param indent integer unused; retained for the review_after_row hook shape
function M._review.emit_comment(comment, index, indent)
  if comment.review_folded then
    local folded_entry = {
      kind = "review_comment",
      review_comment = comment,
      comment_index = index,
      review_first = true,
      review_boundary = "folded",
    }
    local folded = M._review.comment_folded_line(comment)
    local folded_line = status_add_line(folded, folded_entry, "DiffReviewReviewCommentHeader")
    status_add_highlight(folded_line, 0, #folded, "DiffReviewReviewCommentHeader")
    return
  end

  local body_lines = M._review.comment_body_lines(comment.body or "")
  comment.review_rendered_body_text = table.concat(body_lines, "\n")

  local header_entry = {
    kind = "review_comment",
    review_comment = comment,
    comment_index = index,
    review_first = true,
    review_boundary = "header",
  }
  local header = M._review.comment_header_line(comment)
  local header_line = status_add_line(header, header_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(header_line, 0, #header, "DiffReviewReviewCommentHeader")

  for body_index, line in ipairs(body_lines) do
    local body_entry = {
      kind = "review_comment",
      review_comment = comment,
      comment_index = index,
      review_body = true,
      review_body_index = body_index,
    }
    local body_line = status_add_line(line, body_entry)
    if line ~= "" then status_add_highlight(body_line, 0, #line, "DiffReviewReviewComment") end
  end

  for reply_index, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    if type(reply) == "table" then M._review.emit_comment_reply(comment, index, reply, reply_index) end
  end

  local footer_entry = {
    kind = "review_comment",
    review_comment = comment,
    comment_index = index,
    review_boundary = "footer",
  }
  local footer = M._review.comment_footer_line()
  local footer_line = status_add_line(footer, footer_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(footer_line, 0, #footer, "DiffReviewReviewCommentHeader")
end

---Renderer hook: emit any draft comments anchored on `diff_line`.
---@param state table
---@param diff_line table
---@param indent integer
function M._review.emit_comments_for(state, diff_line, indent)
  M._section_builder.emit_anchored_comments(state, diff_line, indent, {
    index_field = "review_comment_anchor_index",
    count_field = "review_rendered_comment_count",
    skip = function(comment) return comment == state.review_editing_comment end,
  })
end

---@param buf integer
---@param row integer
---@return table? comment
---@return integer? index
function M._review.comment_at_row(buf, row)
  local state = M._review.state(buf)
  if not state then return nil end
  local entry = state.entries and state.entries[row] or nil
  if entry and entry.kind == "review_comment" then
    return entry.review_comment, entry.comment_index
  end
  local body_comment = M._review.comment_body_at_row(buf, row)
  if body_comment then
    for index, comment in ipairs(state.review_comments or {}) do
      if M._review.same_comment(comment, body_comment) then return comment, index end
    end
  end
  return nil
end

---@param buf integer
---@return table? comment
---@return integer? index
function M._review.comment_under_cursor(buf)
  local state = M._review.state(buf)
  if not state then return nil end
  M._status = state
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  for _, row in ipairs({ cursor_row, cursor_row - 1, cursor_row + 1 }) do
    local comment, index = M._review.comment_at_row(buf, row)
    if comment then return comment, index end
  end
  return nil
end

---@param buf integer
---@return table? comment
---@return integer? index
function M._review.comment_at_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  return M._review.comment_at_row(buf, row)
end

---@param buf integer
---@return boolean toggled
function M._review.toggle_comment_fold(buf)
  local state = M._review.state(buf)
  if not state then return false end
  M._status = state
  M._review.sync_inline_comment_text(buf)
  local comment = M._review.comment_at_cursor(buf)
  if not comment then return false end
  comment.review_folded = not comment.review_folded
  M._review.save_draft(state)
  M._review.render(buf)
  local header_row0 = M._review.comment_header_row0(buf, comment)
  if header_row0 ~= nil and vim.api.nvim_get_current_buf() == buf then
    pcall(vim.api.nvim_win_set_cursor, 0, { header_row0 + 1, 0 })
    M._review.sync_modifiable(buf)
  end
  return true
end

---Re-anchor editable review regions after a render wipes the buffer. Inline
---comment body rows are emitted during the render and tracked with extmarks.
---@param buf integer
function M._review.on_render(buf)
  local state = M._review.state(buf)
  if not state then return end
  vim.api.nvim_buf_clear_namespace(buf, M._review.ns, 0, -1)
  state.review_comment_start, state.review_comment_end = nil, nil
  for _, comment in ipairs(state.review_comments or {}) do
    comment.review_header_mark = nil
    comment.review_body_start, comment.review_body_end = nil, nil
    comment.review_footer_mark = nil
    comment.review_reply_header_marks = nil
    comment.review_body_line_marks = nil
    comment.review_body_render_rows = nil
    comment.review_body_render_row_count = nil
  end

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local label_row
  for index, line in ipairs(lines) do
    if line == "Review Comment:" then
      label_row = index
      break
    end
  end
  if label_row then
    local count = #vim.split(state.review_comment_text or "", "\n", { plain = true })
    if count == 0 then count = 1 end
    -- Start mark: left gravity so retyping the first line keeps the mark on it.
    -- End mark: right gravity so it stays past the region and follows the
    -- region as it grows/shrinks (a left-gravity end mark at the boundary gets
    -- pulled into an edit of the adjacent line).
    state.review_comment_start = vim.api.nvim_buf_set_extmark(buf, M._review.ns, label_row, 0, { right_gravity = false })
    state.review_comment_end = vim.api.nvim_buf_set_extmark(buf, M._review.ns, label_row + count, 0, { right_gravity = true })
  end

  local range_comment, range_start
  local function close_comment_range(end_row)
    if range_comment and range_start and end_row >= range_start then
      range_comment.review_body_start = vim.api.nvim_buf_set_extmark(buf, M._review.ns, range_start - 1, 0, { right_gravity = false })
      range_comment.review_body_end = vim.api.nvim_buf_set_extmark(buf, M._review.ns, end_row, 0, { right_gravity = true })
    end
    range_comment, range_start = nil, nil
  end

  for row = 1, #lines do
    local entry = state.entries and state.entries[row] or nil
    if entry
      and entry.kind == "review_comment"
      and (entry.review_boundary == "header" or entry.review_boundary == "folded")
      and entry.review_comment then
      close_comment_range(row - 1)
      entry.review_comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = false })
    elseif entry
      and entry.kind == "review_comment"
      and entry.review_boundary == "reply_header"
      and entry.review_comment then
      close_comment_range(row - 1)
      local comment = entry.review_comment
      comment.review_reply_header_marks = comment.review_reply_header_marks or {}
      if entry.review_reply_index then
        comment.review_reply_header_marks[entry.review_reply_index] = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = false })
      end
    elseif entry and entry.kind == "review_comment" and entry.review_boundary == "footer" and entry.review_comment then
      close_comment_range(row - 1)
      entry.review_comment.review_footer_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = true })
    elseif entry and entry.review_body and entry.review_comment then
      local comment = entry.review_comment
      comment.review_body_line_marks = comment.review_body_line_marks or {}
      local mark_id = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = false })
      comment.review_body_line_marks[#comment.review_body_line_marks + 1] = { id = mark_id, line_index = entry.review_body_index }
      if comment ~= range_comment then
        close_comment_range(row - 1)
        range_comment, range_start = comment, row
      end
    else
      close_comment_range(row - 1)
    end
  end
  close_comment_range(#lines)
  state.review_comment_rule_width = M._review.comment_rule_width(nil, buf)
  if state.view_kind == "review" then M._review.sync_modifiable(buf) end
end

---@param buf integer
---@param id integer?
---@return integer? row0
function M._review.mark_row(buf, id)
  if not id then return nil end
  local pos = vim.api.nvim_buf_get_extmark_by_id(buf, M._review.ns, id, {})
  return pos and pos[1] or nil
end

---@param left table?
---@param right table?
---@return boolean
function M._review.same_comment(left, right)
  if not (left and right) then return false end
  if left == right then return true end
  if left.local_id and right.local_id and left.local_id == right.local_id then return true end
  if left.remote_node_id and right.remote_node_id and left.remote_node_id == right.remote_node_id then return true end
  if left.remote_id and right.remote_id and tonumber(left.remote_id) == tonumber(right.remote_id) then return true end
  return false
end

---@param buf integer
---@param comment table
---@return integer? row0
function M._review.comment_header_row0(buf, comment)
  local mark_row = M._review.mark_row(buf, comment.review_header_mark)
  if mark_row ~= nil then return mark_row end
  local state = M._review.state(buf)
  for row, entry in pairs((state and state.entries) or {}) do
    if entry.kind == "review_comment"
      and (entry.review_boundary == "header" or entry.review_boundary == "folded")
      and M._review.same_comment(entry.review_comment, comment) then
      return row - 1
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? row0
function M._review.comment_footer_row0(buf, comment)
  local mark_row = M._review.mark_row(buf, comment.review_footer_mark)
  if mark_row ~= nil then return mark_row end
  local state = M._review.state(buf)
  for row, entry in pairs((state and state.entries) or {}) do
    if entry.kind == "review_comment" and entry.review_boundary == "footer" and M._review.same_comment(entry.review_comment, comment) then
      return row - 1
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? start_row0
---@return integer? end_row0
function M._review.comment_body_range0(buf, comment)
  local start_row0 = M._review.mark_row(buf, comment.review_body_start)
  local end_row0 = M._review.mark_row(buf, comment.review_body_end)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil, nil end
  return start_row0, end_row0
end

---@param buf integer
---@param row integer 1-based
---@return boolean
function M._review.in_comment_region(buf, row)
  local state = M._review.state(buf)
  if not state then return false end
  local s0 = M._review.mark_row(buf, state.review_comment_start)
  local e0 = M._review.mark_row(buf, state.review_comment_end)
  return s0 ~= nil and e0 ~= nil and row >= s0 + 1 and row <= e0
end

---@param buf integer
---@param row integer 1-based
---@return table? comment
function M._review.comment_body_at_row(buf, row)
  local state = M._review.state(buf)
  if not state then return nil end
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then
      local start_row0, end_row0 = M._review.comment_body_range0(buf, comment)
      if start_row0 ~= nil and end_row0 ~= nil and row >= start_row0 + 1 and row <= end_row0 then
        return comment
      end
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? start_row 1-based
---@return integer? end_row 1-based
function M._review.comment_body_rows(buf, comment)
  local start_row0, end_row0 = M._review.comment_body_range0(buf, comment)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil, nil end
  return start_row0 + 1, end_row0
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer
function M._review.comment_body_prefix_width(buf, row, line)
  return 0
end

---@param prefix_width integer
---@return string
function M._review.comment_body_prefix(prefix_width)
  return ""
end

---@param line string
---@param prefix_width integer
---@return boolean
function M._review.line_has_comment_body_prefix(line, prefix_width)
  return true
end

---@param buf integer
---@param row integer 1-based
---@param line string
---@param prefix_width integer
---@return string
function M._review.ensure_comment_body_prefix(buf, row, line, prefix_width)
  return tostring(line or "")
end

---@param line string
---@param prefix_width? integer
---@return integer
function M._review.comment_body_effective_prefix_width(line, prefix_width)
  return math.max(tonumber(prefix_width) or 0, 0)
end

---@param line string
---@param prefix_width? integer
---@return string
function M._review.comment_body_text_from_line(line, prefix_width)
  local text = tostring(line or "")
  local width = math.max(tonumber(prefix_width) or 0, 0)
  if width > 0 and #text >= width then
    local prefix = text:sub(1, width)
    if prefix:match("^%s*$") then text = text:sub(width + 1) end
  end
  return text
end

---@param lines string[]
---@return string[]
function M._review.trim_comment_body_lines(lines)
  local first, last = 1, #lines
  while first <= last and vim.trim(lines[first] or "") == "" do
    first = first + 1
  end
  while last >= first and vim.trim(lines[last] or "") == "" do
    last = last - 1
  end
  local trimmed = {}
  for index = first, last do
    trimmed[#trimmed + 1] = lines[index] or ""
  end
  return trimmed
end

---@param body string
---@return string
function M._review.comment_body_for_sync(body)
  local lines = M._review.trim_comment_body_lines(vim.split(tostring(body or ""), "\n", { plain = true }))
  return table.concat(lines, "\n")
end

---@param line string
---@param prefix_width? integer
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M._review.comment_body_text_bounds(line, prefix_width)
  local text = tostring(line or "")
  local start_col = M._review.comment_body_effective_prefix_width(text, prefix_width)
  local body_text = M._review.comment_body_text_from_line(text, start_col)
  if body_text == "" then return start_col, start_col end
  return start_col, start_col + #body_text
end

---@param line string
---@param prefix_width? integer
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M._review.comment_body_text_selection_bounds(line, prefix_width)
  local start_col, end_col = M._review.comment_body_text_bounds(line, prefix_width)
  if end_col > start_col then end_col = end_col - 1 end
  return start_col, end_col
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M._review.comment_body_text_selection_bounds_at_row(buf, row, line)
  local text = line or vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local prefix_width = M._review.comment_body_prefix_width(buf, row, text)
  return M._review.comment_body_text_selection_bounds(text, prefix_width)
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M._review.comment_body_text_bounds_at_row(buf, row, line)
  local text = line or vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local prefix_width = M._review.comment_body_prefix_width(buf, row, text)
  return M._review.comment_body_text_bounds(text, prefix_width)
end

---@param buf integer
---@return integer? row 1-based current row after clamping
function M._review.clamp_comment_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local state = M._review.state(buf)
  if not state or state.review_clamping_cursor then return nil end
  state.review_clamping_cursor = true
  local row = M._normalize_status_cursor(buf) or vim.api.nvim_win_get_cursor(0)[1]
  state.review_clamping_cursor = nil
  state.review_last_cursor_row = row
  return row
end

---@param buf integer
---@param row integer 1-based
---@return boolean
function M._review.in_editable_region(buf, row)
  return M._review.in_comment_region(buf, row) or M._review.comment_body_at_row(buf, row) ~= nil
end

---@param buf integer
---@return boolean
function M._review.cursor_or_selection_in_editable_region(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    local start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
    if start_row > end_row then start_row, end_row = end_row, start_row end
    local found_editable = false
    for row = start_row, end_row do
      if M._review.in_editable_region(buf, row) then
        found_editable = true
      else
        return false
      end
    end
    return found_editable
  end
  return M._review.in_editable_region(buf, vim.api.nvim_win_get_cursor(0)[1])
end

---@param buf integer
---@return table? state
function M._review.command_map_state(buf)
  local state = M._status_states and M._status_states[buf] or M._status
  if not (state and state.view_kind == "review") then return nil end
  state.review_command_maps = state.review_command_maps or {}
  state.review_command_maps_by_key = state.review_command_maps_by_key or {}
  return state
end

---@param mode string|string[]
---@return string[]
function M._review.keymap_modes(mode)
  if type(mode) == "table" then return mode end
  return { mode }
end

---@param buf integer
---@param command_id string
---@return boolean
function M._review.command_map_should_be_active(buf, command_id)
  if command_id == "sync" then return true end
  if command_id == "browse" and vim.fn.mode():sub(1, 1) == "n" then return true end
  if command_id == "toggle" and vim.fn.mode():sub(1, 1) == "n" then return true end
  return not M._review.cursor_or_selection_in_editable_region(buf)
end

---@param map table
function M._review.install_command_map(map)
  if map.removed then return end
  if map.active then return end
  vim.keymap.set(map.modes, map.key, map.callback, map.opts)
  map.active = true
end

---@param map table
function M._review.delete_command_map(map)
  if not map.active then return end
  for _, mode in ipairs(map.mode_list) do
    pcall(vim.keymap.del, mode, map.key, { buffer = map.buf })
  end
  map.active = false
end

---@param buf integer
---@param command_id string
---@param modes string|string[]
---@param key string
---@param callback function
---@param opts table
function M._review.register_command_map(buf, command_id, modes, key, callback, opts)
  local state = M._review.command_map_state(buf)
  if not state then
    vim.keymap.set(modes, key, callback, opts)
    return
  end
  local mode_list = M._review.keymap_modes(modes)
  local signature = table.concat(mode_list, "\0") .. "\0" .. key
  local existing = state.review_command_maps_by_key[signature]
  if existing then
    M._review.delete_command_map(existing)
    existing.removed = true
  end
  local map = {
    buf = buf,
    command_id = command_id,
    modes = modes,
    mode_list = mode_list,
    key = key,
    callback = callback,
    opts = opts,
    active = false,
  }
  state.review_command_maps_by_key[signature] = map
  state.review_command_maps[#state.review_command_maps + 1] = map
  if M._review.command_map_should_be_active(buf, command_id) then M._review.install_command_map(map) end
end

---@param buf integer
function M._review.sync_command_keymaps(buf)
  local state = M._review.command_map_state(buf)
  if not state then return end
  for _, map in ipairs(state.review_command_maps or {}) do
    if map.removed then goto continue end
    if M._review.command_map_should_be_active(buf, map.command_id) then
      M._review.install_command_map(map)
    else
      M._review.delete_command_map(map)
    end
    ::continue::
  end
end

---Unlock the buffer only while the cursor sits in an editable review region.
---@param buf integer
function M._review.sync_modifiable(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if vim.api.nvim_get_current_buf() ~= buf then return end
  local row = M._review.clamp_comment_cursor(buf) or vim.api.nvim_win_get_cursor(0)[1]
  local wanted = M._review.in_editable_region(buf, row)
  if vim.bo[buf].modifiable ~= wanted then
    vim.bo[buf].modifiable = wanted
  end
  M._review.sync_command_keymaps(buf)
end

---Read edited review text back into the state.
---@param buf integer
function M._review.sync_comment_text(buf)
  local state = M._review.state(buf)
  if not state then return end
  local s0 = M._review.mark_row(buf, state.review_comment_start)
  local e0 = M._review.mark_row(buf, state.review_comment_end)
  local changed = false
  if s0 and e0 then
    local review_comment_text = table.concat(vim.api.nvim_buf_get_lines(buf, s0, e0, false), "\n")
    if review_comment_text ~= state.review_comment_text then
      state.review_comment_text = review_comment_text
      changed = true
    end
  end
  if changed then M._review.save_draft(state) end
end

---@param buf integer
---@param comment table
---@return string?
function M._review.comment_body_text_from_buffer(buf, comment)
  local start_row0, end_row0 = M._review.comment_body_range0(buf, comment)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil end
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row0, end_row0, false)
  return table.concat(M._review.trim_comment_body_lines(raw_lines), "\n")
end

---@param buf integer
---@param comment table
---@return table? snapshot
function M._review.inline_comment_cursor_snapshot(buf, comment)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local start_row, end_row = M._review.comment_body_rows(buf, comment)
  if not start_row or not end_row then return nil end
  local cursor = vim.api.nvim_win_get_cursor(0)
  if cursor[1] < start_row or cursor[1] > end_row then return nil end

  local offset = 0
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
  for index, raw_line in ipairs(raw_lines) do
    local row = start_row + index - 1
    local body_text = tostring(raw_line or "")
    if row == cursor[1] then
      return {
        offset = offset + math.min(math.max(cursor[2] or 0, 0), #body_text),
        insert = vim.api.nvim_get_mode().mode:sub(1, 1) == "i",
      }
    end
    offset = offset + #body_text + 1
  end
  return nil
end

---@param buf integer
---@param comment table
---@param snapshot table?
function M._review.restore_inline_comment_cursor(buf, comment, snapshot)
  if not (snapshot and buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  local start_row, end_row = M._review.comment_body_rows(buf, comment)
  if not start_row or not end_row then return end

  local remaining = math.max(tonumber(snapshot.offset) or 0, 0)
  local target_row, target_col = start_row, 0
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
  for index, raw_line in ipairs(raw_lines) do
    target_row = start_row + index - 1
    local body_text = tostring(raw_line or "")
    target_col = math.min(remaining, #body_text)
    if remaining <= #body_text then break end
    remaining = remaining - #body_text - 1
  end
  pcall(vim.api.nvim_win_set_cursor, 0, { target_row, target_col })
  M._review.sync_modifiable(buf)
  if snapshot.insert then
    vim.schedule(function()
      if vim.api.nvim_get_current_buf() == buf then pcall(vim.cmd, "startinsert") end
    end)
  end
end

---@param buf integer
---@param comment table
---@param snapshot table?
function M._review.schedule_inline_comment_render(buf, comment, snapshot)
  local state = M._review.state(buf)
  if not state or state.review_inline_render_pending then
    if state then state.review_inline_render_pending = { comment = comment, snapshot = snapshot } end
    return
  end
  state.review_inline_render_pending = { comment = comment, snapshot = snapshot }
  local pending = state.review_inline_render_pending
  state.review_inline_render_pending = nil
  if not (pending and pending.comment and pending.comment.local_state ~= "deleted") then return end
  M._review.render(buf)
  M._review.restore_inline_comment_cursor(buf, pending.comment, pending.snapshot)
end

---@param buf integer
---@param row0 integer?
---@param line string
---@return boolean changed
function M._review.replace_comment_rule_line(buf, row0, line)
  if row0 == nil or row0 < 0 then return false end
  local old_line = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1]
  if old_line == nil or old_line == line then return false end
  pcall(vim.api.nvim_buf_clear_namespace, buf, M._status_ns, row0, row0 + 1)
  pcall(vim.api.nvim_buf_set_text, buf, row0, 0, row0, #old_line, { line })
  if line ~= "" then
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, row0, 0, {
      line_hl_group = "DiffReviewReviewCommentHeader",
      priority = 80,
    })
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, row0, 0, {
      end_col = #line,
      hl_group = "DiffReviewReviewCommentHeader",
      priority = 90,
    })
  end
  local state = M._review.state(buf)
  if state and state.lines then state.lines[row0 + 1] = line end
  return true
end

---@param buf integer
---@param win integer?
---@param target_comment table?
function M._review.refresh_inline_comment_rules(buf, win, target_comment)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local state = M._review.state(buf)
  if not state then return end

  local was_modifiable = vim.bo[buf].modifiable
  vim.bo[buf].modifiable = true
  for _, comment in ipairs(state.review_comments or {}) do
    if (not target_comment or M._review.same_comment(comment, target_comment)) and comment.local_state ~= "deleted" then
      local header_row0 = M._review.comment_header_row0(buf, comment)
      if comment.review_folded then
        local folded = M._review.comment_folded_line(comment, win, buf)
        if M._review.replace_comment_rule_line(buf, header_row0, folded) and header_row0 ~= nil then
          pcall(vim.api.nvim_buf_del_extmark, buf, M._review.ns, comment.review_header_mark)
          comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, header_row0, 0, { right_gravity = false })
        end
        goto continue
      end
      local footer_row0 = M._review.comment_footer_row0(buf, comment)
      local header = M._review.comment_header_line(comment, win, buf)
      local footer = M._review.comment_footer_line(win, buf)
      if M._review.replace_comment_rule_line(buf, header_row0, header) and header_row0 ~= nil then
        pcall(vim.api.nvim_buf_del_extmark, buf, M._review.ns, comment.review_header_mark)
        comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, header_row0, 0, { right_gravity = false })
      end
      if M._review.replace_comment_rule_line(buf, footer_row0, footer) and footer_row0 ~= nil then
        pcall(vim.api.nvim_buf_del_extmark, buf, M._review.ns, comment.review_footer_mark)
        comment.review_footer_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, footer_row0, 0, { right_gravity = true })
      end
      for reply_index, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
        local reply_mark_id = comment.review_reply_header_marks and comment.review_reply_header_marks[reply_index] or nil
        local reply_row0 = M._review.mark_row(buf, reply_mark_id)
        local reply_header = M._review.reply_header_line(reply, win, buf)
        if M._review.replace_comment_rule_line(buf, reply_row0, reply_header) and reply_row0 ~= nil then
          pcall(vim.api.nvim_buf_del_extmark, buf, M._review.ns, reply_mark_id)
          comment.review_reply_header_marks = comment.review_reply_header_marks or {}
          comment.review_reply_header_marks[reply_index] = vim.api.nvim_buf_set_extmark(buf, M._review.ns, reply_row0, 0, { right_gravity = false })
        end
      end
    end
    ::continue::
  end
  vim.bo[buf].modifiable = was_modifiable
end

---@param buf integer
---@param comment table
function M._review.refresh_inline_comment_header(buf, comment)
  M._review.refresh_inline_comment_rules(buf, nil, comment)
end

---@param buf integer
---@return table? comment
---@return string? edited_text
function M._review.changed_inline_comment_body(buf)
  local state = M._review.state(buf)
  if not state or state.review_rendering or state.review_editing_comment then return nil end
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then
      local edited_text = M._review.comment_body_text_from_buffer(buf, comment)
      local rendered_text = tostring(comment.body or "")
      if edited_text and edited_text ~= rendered_text then return comment, edited_text end
    end
  end
  return nil
end

---@param buf integer
---@return boolean changed
function M._review.sync_inline_comment_text(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return false end
  local state = M._review.state(buf)
  if not state or state.review_rendering or state.review_editing_comment then return false end
  local comment, edited_text = M._review.changed_inline_comment_body(buf)
  if not (comment and edited_text) then return false end
  comment.body = edited_text
  comment.review_rendered_body_text = edited_text
  comment.updated_at = os.date("%Y-%m-%d %H:%M")
  comment.local_state = comment.remote_node_id and "dirty" or "new"
  comment.syncing = nil
  M._review.normalize_comment(state, comment)
  if state.view_kind == "review" then
    M._review.save_draft(state)
  elseif state.view_kind == "pr" then
    vim.bo[buf].modified = true
  end
  M._review.refresh_inline_comment_header(buf, comment)
  return true
end

---@param buf integer
---@param viewed boolean
function M._review.toggle_viewed(buf, viewed)
  local state = M._review.state(buf)
  if not state then return end
  M._status = state
  local entry = status_entry_under_cursor()
  if not entry then return end
  state.review_viewed_hunks = state.review_viewed_hunks or {}

  local function apply_file(file, selected_hunk)
    local hunks = {}
    if selected_hunk then
      hunks[#hunks + 1] = selected_hunk
    else
      for _, hunk in ipairs(status_diff_hunks_for_file(file)) do
        hunks[#hunks + 1] = hunk
      end
    end
    if #hunks == 0 then
      state.review_viewed[file.relpath] = viewed or nil
    else
      state.review_viewed[file.relpath] = nil
      for _, hunk in ipairs(hunks) do
        local key = M._review.hunk_view_key(state, file, hunk)
        if viewed then
          state.review_viewed_hunks[key] = {
            file = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename,
            commit_id = state.commit_id,
          }
        else
          state.review_viewed_hunks[key] = nil
        end
      end
    end
  end

  if entry.file then
    apply_file(entry.file, entry.hunk)
  elseif entry.kind == "section" and entry.section then
    for _, file in ipairs(entry.section.files or {}) do
      apply_file(file)
    end
  else
    return
  end

  M._review.save_draft(state)
  M._review.render(buf)
end

---Build the comment target (path/side/line range) from the visual selection
---or the cursor line. Only changed (diff body) rows can carry a comment.
---@param state table
---@return table? payload
function M._review.selection_payload(state)
  local mode = vim.fn.mode()
  local srow, erow
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    srow, erow = vim.fn.line("v"), vim.fn.line(".")
  else
    srow = vim.fn.line(".")
    erow = srow
  end
  if srow > erow then srow, erow = erow, srow end

  local first, last
  for row = srow, erow do
    local entry = state.entries[row]
    local dl = entry and entry.diff_line
    if dl then
      first = first or dl
      last = dl
    end
  end
  if not first then return nil end

  -- Diff rows carry the absolute path; GitHub wants it repo-relative. Keep the
  -- absolute path too so inline comments can re-anchor against rendered rows.
  local path = repo_relative(last.file, state.cwd) or last.file
  local payload = { path = path, abs_file = last.file, side = M._review.side_of(last), line = last.line, position = last.position }
  if first.file == last.file and not (first.line == last.line and first.side == last.side) then
    payload.start_line = first.line
    payload.start_side = M._review.side_of(first)
    payload.start_position = first.position
    -- GitHub requires start_line < line when both endpoints are on one side.
    if payload.start_side == payload.side and payload.start_line > payload.line then
      payload.start_line, payload.line = payload.line, payload.start_line
    end
  end
  return payload
end

---@param title string
---@param on_submit fun(text: string)
---@param prefill? string existing text to edit
---@param opts? { on_cancel?: fun(), cursor?: integer[] }
function M._review.open_input(title, on_submit, prefill, opts)
  if M._review.input_provider then
    M._review.input_provider(title, on_submit, prefill)
    return
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  if prefill and prefill ~= "" then
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(prefill, "\n", { plain = true }))
  end
  local width = math.min(72, math.max(40, math.floor(vim.o.columns * 0.6)))
  local content_height = math.max(1, #vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  local max_height = math.max(1, vim.o.lines - 6)
  local height = math.min(content_height, max_height)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width,
    height = height,
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " (Ctrl-s submit, Ctrl-q cancel) ",
    title_pos = "center",
  })
  pcall(vim.api.nvim_set_current_win, win)
  if opts and opts.cursor then
    local popup_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local target_row = math.min(math.max(opts.cursor[1] or 1, 1), math.max(#popup_lines, 1))
    local line = popup_lines[target_row] or ""
    local target_col = math.min(math.max(opts.cursor[2] or 0, 0), #line)
    pcall(vim.api.nvim_win_set_cursor, win, { target_row, target_col })
  end
  local done = false
  local function enter_insert()
    if done or not vim.api.nvim_win_is_valid(win) then return end
    pcall(vim.api.nvim_set_current_win, win)
    pcall(vim.cmd, "startinsert")
    vim.defer_fn(function()
      if done or not vim.api.nvim_win_is_valid(win) then return end
      pcall(vim.api.nvim_set_current_win, win)
      pcall(vim.cmd, "startinsert")
    end, 10)
  end
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  local function cancel()
    if done then return end
    done = true
    close()
    if opts and opts.on_cancel then opts.on_cancel() end
  end
  local function submit()
    if done then return end
    done = true
    vim.cmd("stopinsert")
    local text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    close()
    on_submit(text)
  end
  vim.keymap.set({ "n", "i" }, "<C-s>", submit, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-q>", cancel, { buffer = buf, nowait = true })
  vim.keymap.set("n", "<Esc>", cancel, { buffer = buf, nowait = true })
  enter_insert()
  vim.schedule(enter_insert)
end

---@param buf integer
---@param comment table
---@param opts? { insert?: boolean }
function M._review.focus_inline_comment(buf, comment, opts)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  if comment.review_folded then
    local state = M._review.state(buf)
    comment.review_folded = false
    if state and state.view_kind == "review" then M._review.save_draft(state) end
    if state and state.view_kind == "pr" then
      render_pr_status(state.pr, state.cwd, buf, state.pr_diff_text)
    else
      M._review.render(buf)
    end
  end
  local start_row, end_row = M._review.comment_body_rows(buf, comment)
  if not start_row or not end_row then return end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local target_row = cursor[1]
  if target_row < start_row or target_row > end_row then target_row = start_row end
  local line = vim.api.nvim_buf_get_lines(buf, target_row - 1, target_row, false)[1] or ""
  local target_col = math.min(math.max(cursor[2] or 0, 0), #line)
  pcall(vim.api.nvim_win_set_cursor, 0, { target_row, target_col })
  M._review.sync_modifiable(buf)
  if opts and opts.insert then
    vim.schedule(function()
      if vim.api.nvim_get_current_buf() == buf then pcall(vim.cmd, "startinsert") end
    end)
  end
end

---@param state table
---@param payload table
---@return table comment
function M._review.create_inline_comment(state, payload)
  local created_at = os.date("%Y-%m-%d %H:%M")
  local comment = {
    path = payload.path,
    abs_file = payload.abs_file,
    side = payload.side,
    line = payload.line,
    position = payload.position,
    start_line = payload.start_line,
    start_side = payload.start_side,
    start_position = payload.start_position,
    body = "",
    user = "you",
    created_at = created_at,
    updated_at = created_at,
    local_state = "new",
  }
  M._review.normalize_comment(state, comment)
  state.review_comments[#state.review_comments + 1] = comment
  return comment
end

---`C`: on an existing comment, focus its inline editable body; on a changed diff line, add a draft
---comment. On anything else it is a no-op. Comments are local until submit.
---@param buf integer
function M._review.add_comment(buf)
  local state = M._review.state(buf)
  if not state then return end
  M._status = state
  local existing = M._review.comment_under_cursor(buf)
  if existing then
    M._review.focus_inline_comment(buf, existing)
    return
  end
  local payload = M._review.selection_payload(state)
  M._review.leave_visual()
  if not payload then return end
  local comment = M._review.create_inline_comment(state, payload)
  M._review.save_draft(state)
  M._review.render(buf)
  M._review.focus_inline_comment(buf, comment, { insert = true })
end

---@param buf integer
---@return table? status
function M._pr_overview.state(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if not (status and status.view_kind == "pr" and status.pr) then return nil end
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.review_comments = status.pr_standalone_comments
  return status
end

---@param comment table?
---@return string?
function M._pr_overview.comment_identity_key(comment)
  if not comment then return nil end
  if comment.remote_node_id and comment.remote_node_id ~= "" then return "node:" .. tostring(comment.remote_node_id) end
  if comment.remote_id then return "id:" .. tostring(comment.remote_id) end
  return nil
end

---@param status table
---@param comment table?
---@return boolean
function M._pr_overview.is_standalone_comment(status, comment)
  if not (status and comment) then return false end
  for _, local_comment in ipairs(status.pr_standalone_comments or {}) do
    if M._review.same_comment(local_comment, comment) then return true end
  end
  return false
end

---@param buf integer
---@return table? comment
function M._pr_overview.standalone_comment_under_cursor(buf)
  local status = M._pr_overview.state(buf)
  if not status then return nil end
  local comment = M._review.comment_under_cursor(buf)
  if M._pr_overview.is_standalone_comment(status, comment) then return comment end
  return nil
end

---@param buf integer
function M._pr_overview.refresh_modified(buf)
  local status = M._pr_overview.state(buf)
  if not status or not vim.api.nvim_buf_is_valid(buf) then return end
  local title_dirty, desc_dirty = M._pr_edit.dirty_flags(buf, status)
  local comments_dirty = false
  for _, comment in ipairs(status.pr_standalone_comments or {}) do
    if M._review.comment_needs_sync(comment) then
      comments_dirty = true
      break
    end
  end
  vim.bo[buf].modified = title_dirty or desc_dirty or comments_dirty
end

---@param buf integer
function M._pr_overview.render_preserving_inline_cursor(buf)
  local status = M._pr_overview.state(buf)
  if not status then return end
  local comment
  local snapshot
  if vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    comment = M._review.comment_body_at_row(buf, cursor[1])
    if comment then snapshot = M._review.inline_comment_cursor_snapshot(buf, comment) end
  end
  render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  if comment and snapshot and comment.local_state ~= "deleted" then
    M._review.restore_inline_comment_cursor(buf, comment, snapshot)
  end
end

---@param status table
---@param comment table
---@param remote table?
---@param queued_body string
function M._pr_overview.apply_synced_standalone_comment(status, comment, remote, queued_body)
  if remote then
    comment.remote_id = remote.remote_id or comment.remote_id
    comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
    comment.remote_review_id = remote.review_id or comment.remote_review_id
    comment.review_node_id = remote.review_node_id or comment.review_node_id
    comment.position = remote.position or comment.position
    comment.line = remote.line or comment.line
    if remote.path and remote.path ~= "" then comment.path = remote.path end
    comment.side = remote.side or comment.side
    comment.user = remote.user or comment.user
    comment.created_at = remote.created_at or comment.created_at
    comment.updated_at = remote.updated_at or comment.updated_at
    comment.url = remote.url or comment.url
    comment.replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or (comment.replies or {})
  end
  if M._review.comment_body_for_sync(comment.body or "") == queued_body then
    comment.body = queued_body
    comment.base_body = queued_body
    comment.local_state = "clean"
  else
    comment.base_body = queued_body
    comment.local_state = "dirty"
  end
  comment.syncing = nil
  M._review.normalize_comment(status, comment)
end

---@param status table
---@param operation fun(done: fun())
function M._pr_overview.enqueue_standalone_sync(status, operation)
  status.pr_standalone_sync = status.pr_standalone_sync or { queue = {}, running = false }
  status.pr_standalone_sync.queue[#status.pr_standalone_sync.queue + 1] = operation
  local function run_next()
    if status.pr_standalone_sync.running then return end
    local next_operation = table.remove(status.pr_standalone_sync.queue, 1)
    if not next_operation then return end
    status.pr_standalone_sync.running = true
    next_operation(function()
      status.pr_standalone_sync.running = false
      run_next()
    end)
  end
  run_next()
end

---@param buf integer
---@param comment table
function M._pr_overview.sync_standalone_comment(buf, comment)
  local status = M._pr_overview.state(buf)
  if not (status and status.pr and comment) then return end
  local body = M._review.comment_body_for_sync(comment.body or "")
  if body == "" then
    comment.syncing = nil
    M._pr_overview.refresh_modified(buf)
    return
  end
  local pr = status.pr
  local opts = {
    body = body,
    commit_id = pr.headRefOid,
    path = comment.path,
    line = comment.line,
    side = comment.side or "RIGHT",
    start_line = comment.start_line,
    start_side = comment.start_side,
    position = comment.position,
  }
  local update_node_id = comment.remote_node_id
  M._pr_overview.enqueue_standalone_sync(status, function(done)
    local latest = M._pr_overview.state(buf)
    if not latest then
      done()
      return
    end
    local callback = function(result)
      latest = M._pr_overview.state(buf)
      if not latest then
        done()
        return
      end
      if not result.ok then
        comment.syncing = nil
        notify_error("GitHub inline comment sync failed: " .. (result.message or "gh failed"), "DiffReview")
        if vim.api.nvim_buf_is_valid(buf) then
          M._pr_overview.render_preserving_inline_cursor(buf)
          M._pr_overview.refresh_modified(buf)
        end
        done()
        return
      end
      M._pr_overview.apply_synced_standalone_comment(latest, comment, result.comments and result.comments[1] or nil, body)
      if vim.api.nvim_buf_is_valid(buf) then
        M._pr_overview.render_preserving_inline_cursor(buf)
        M._pr_overview.refresh_modified(buf)
      end
      vim.notify("Inline comment synced", vim.log.levels.INFO, { title = "DiffReview" })
      done()
    end
    if update_node_id and update_node_id ~= "" then
      gh.update_review_comment_async(latest.cwd, update_node_id, body, callback)
    else
      gh.create_pr_review_comment_async(latest.cwd, pr.number, pr.repo, opts, callback)
    end
  end)
end

---@param buf integer
---@return integer count
function M._pr_overview.sync_standalone_comments(buf)
  local status = M._pr_overview.state(buf)
  if not status then return 0 end
  M._review.sync_inline_comment_text(buf)
  local comments = {}
  for _, comment in ipairs(status.pr_standalone_comments or {}) do
    if M._review.comment_needs_sync(comment) then
      comment.syncing = true
      comments[#comments + 1] = comment
    end
  end
  if #comments == 0 then return 0 end
  for _, comment in ipairs(comments) do
    M._review.refresh_inline_comment_header(buf, comment)
  end
  M._pr_overview.refresh_modified(buf)
  for _, comment in ipairs(comments) do
    M._pr_overview.sync_standalone_comment(buf, comment)
  end
  return #comments
end

---@param buf integer
function M._pr_overview.add_standalone_comment(buf)
  local status = M._pr_overview.state(buf)
  if not status then return end
  M._status = status
  local existing = M._pr_overview.standalone_comment_under_cursor(buf)
  if existing then
    M._review.focus_inline_comment(buf, existing)
    return
  end
  local payload = M._review.selection_payload(status)
  M._review.leave_visual()
  if not payload then return end
  local comment = M._review.create_inline_comment(status, payload)
  comment.standalone = true
  render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  M._review.focus_inline_comment(buf, comment, { insert = true })
  M._pr_overview.refresh_modified(buf)
end

---`J`: remove the draft comment under the cursor.
---@param buf integer
function M._review.delete_comment(buf)
  local state = M._review.state(buf)
  if not state then return end
  local comment, index = M._review.comment_under_cursor(buf)
  if not (comment and index) then return end
  if comment.remote_node_id then
    comment.local_state = "deleted"
  else
    table.remove(state.review_comments, index)
  end
  M._review.save_draft(state)
  M._review.render(buf)
end

---Sync dirty inline comments to the pending GitHub review.
---@param buf integer
function M._review.sync(buf)
  local state = M._review.state(buf)
  if not state then return end
  M._status = state
  M._review.sync_comment_text(buf)
  M._review.sync_inline_comment_text(buf)
  local count = M._review.enqueue_dirty_comments(buf)
  if count == 0 then
    vim.notify("No review comments to sync", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  M._review.flush_sync(buf, function(ok)
    if not vim.api.nvim_buf_is_valid(buf) then return end
    if ok then
      vim.notify(("Synced %d review comment%s"):format(count, count == 1 and "" or "s"), vim.log.levels.INFO, { title = "DiffReview" })
    end
  end)
end

---@param buf integer
---@param direction 1|-1
function M._review.navigate(buf, direction)
  local state = M._review.state(buf)
  if not state then return end
  local rows = {}
  for row, entry in pairs(state.entries or {}) do
    if entry.kind == "review_comment" and entry.review_first then
      rows[#rows + 1] = row
    end
  end
  if #rows == 0 then
    vim.notify("No comments yet", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  table.sort(rows)
  local cursor = vim.api.nvim_win_get_cursor(0)[1]
  local target
  if direction == 1 then
    for _, row in ipairs(rows) do
      if row > cursor then target = row break end
    end
    target = target or rows[1]
  else
    for index = #rows, 1, -1 do
      if rows[index] < cursor then target = rows[index] break end
    end
    target = target or rows[#rows]
  end
  pcall(vim.api.nvim_win_set_cursor, 0, { target, 0 })
  vim.cmd("normal! zz")
end

---Submit the review. Inline comments are synced to a pending remote review as
---part of submission; the final review summary stays local-only until this call.
---@param buf integer
function M._review.submit(buf)
  local state = M._review.state(buf)
  if not state then return end
  M._status = state
  M._review.sync_comment_text(buf)
  local function with_event(event)
    if not event then return end
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local comments = {}
    for _, comment in ipairs(state.review_comments) do
      if comment.local_state ~= "deleted" then
        local body = M._review.comment_body_for_sync(comment.body or "")
        local item = {
          path = comment.path,
          body = body,
          line = comment.line,
          side = comment.side,
        }
        if body ~= "" then
          if comment.start_line then
            item.start_line = comment.start_line
            item.start_side = comment.start_side
          end
          comments[#comments + 1] = item
        end
      end
    end
    local count = #comments
    local function handle_submit_result(result)
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if result.code ~= 0 then
        notify_error(
          "PR review submit failed: " .. (result.output ~= "" and result.output or ("gh exited " .. tostring(result.code))),
          "DiffReview"
        )
        return
      end
      state.review_comments = {}
      state.review_comment_text = ""
      state.review_remote = nil
      M._review.delete_draft(state)
      M._review.render(buf)
      vim.notify(
        ("Review submitted (%s, %d comment%s)"):format(event, count, count == 1 and "" or "s"),
        vim.log.levels.INFO,
        { title = "DiffReview" }
      )
    end
    M._review.flush_sync(buf, function(ok)
      if not ok then return end
      if state.review_remote and state.review_remote.id then
        gh.submit_pending_review_async(state.cwd, state.pr.number, state.pr.repo, state.review_remote.id, {
          body = state.review_comment_text or "",
          event = event,
        }, handle_submit_result)
      else
        gh.submit_pr_review_async(state.cwd, state.pr.number, state.pr.repo, {
          body = state.review_comment_text or "",
          event = event,
          commit_id = state.commit_id,
          comments = comments,
        }, handle_submit_result)
      end
    end)
  end
  if M._review.verdict_provider then
    M._review.verdict_provider(with_event)
    return
  end
  M._review.pick_verdict(with_event)
end

--- Verdict chooser as a small popup window (not vim.ui.select / snacks):
--- `c`/`a`/`r` pick, `q`/`<Esc>` cancels.
---@param on_choice fun(event: string?)
function M._review.pick_verdict(on_choice)
  local options = {
    { key = "c", event = "COMMENT", label = "Comment (no verdict)" },
    { key = "a", event = "APPROVE", label = "Approve" },
    { key = "r", event = "REQUEST_CHANGES", label = "Request changes" },
  }
  local lines = { "" }
  for _, opt in ipairs(options) do
    lines[#lines + 1] = ("  [%s]  %s"):format(opt.key, opt.label)
  end
  lines[#lines + 1] = "  [q]  cancel"
  lines[#lines + 1] = ""

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  local width = 0
  for _, line in ipairs(lines) do
    width = math.max(width, #line)
  end
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width + 2,
    height = #lines,
    style = "minimal",
    border = "rounded",
    title = " Submit review ",
    title_pos = "center",
  })
  local chosen = false
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  local function choose(event)
    if chosen then return end
    chosen = true
    close()
    on_choice(event)
  end
  for _, opt in ipairs(options) do
    vim.keymap.set("n", opt.key, function() choose(opt.event) end, { buffer = buf, nowait = true, silent = true })
  end
  vim.keymap.set("n", "q", function() choose(nil) end, { buffer = buf, nowait = true, silent = true })
  vim.keymap.set("n", "<Esc>", function() choose(nil) end, { buffer = buf, nowait = true, silent = true })
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buf, once = true, callback = function() choose(nil) end })
end

---@param buf integer
function M._review.attach(buf)
  local group = vim.api.nvim_create_augroup("DiffReviewReview" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter", "ModeChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._review.sync_modifiable(buf)
    end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "InsertLeave" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._review.sync_comment_text(buf)
      M._review.sync_inline_comment_text(buf)
    end,
  })
end

-- Per-repository config, read from `<repo root>/.diffreview.json`. Currently
-- only `branch_prefix` (used by the `bc` branch-create action). On the module
-- table because init.lua is at Lua's 200-local limit.
M._repo_config = { reader = nil }

---@param fn fun(path: string): string? test seam returning file contents
function M._repo_config.set_reader(fn)
  M._repo_config.reader = fn
end

function M._repo_config.reset_reader()
  M._repo_config.reader = nil
end

---@param cwd string repo root
---@return DiffReviewRepoConfig
function M._repo_config.read(cwd)
  local path = (cwd:gsub("[/\\]+$", "")) .. "/.diffreview.json"
  local content
  if M._repo_config.reader then
    content = M._repo_config.reader(path)
  else
    local handle = io.open(path, "r")
    if handle then
      content = handle:read("*a")
      handle:close()
    end
  end
  if not content or content == "" then return {} end
  local ok, decoded = pcall(vim.json.decode, content)
  if ok and type(decoded) == "table" then return decoded end
  return {}
end

---@param cwd string repo root
---@return string
function M._repo_config.branch_prefix(cwd)
  local repo = M._repo_config.read(cwd)
  if type(repo.branch_prefix) == "string" and repo.branch_prefix ~= "" then
    return repo.branch_prefix
  end
  local options = M.config or config.options or config.defaults
  return options.branch_prefix or config.defaults.branch_prefix or ""
end

--- Centered single-line input popup (not vim.ui.input / snacks). `<CR>`
--- submits, `<Esc>`/`q` cancels (calling back with nil).
---@param prefix string prefilled text
---@param on_submit fun(name: string?)
function M._prompt_branch_name(prefix, on_submit)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { prefix })
  local width = math.min(60, math.max(30, math.floor(vim.o.columns * 0.4)))
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    row = math.floor((vim.o.lines - 3) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    width = width,
    height = 1,
    style = "minimal",
    border = "rounded",
    title = " New branch ",
    title_pos = "center",
  })
  local done = false
  local function finish(value)
    if done then return end
    done = true
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
    on_submit(value)
  end
  vim.keymap.set({ "i", "n" }, "<CR>", function()
    vim.cmd("stopinsert")
    finish(vim.api.nvim_buf_get_lines(buf, 0, -1, false)[1] or "")
  end, { buffer = buf })
  vim.keymap.set("i", "<C-c>", function() vim.cmd("stopinsert"); finish(nil) end, { buffer = buf })
  vim.keymap.set("n", "<Esc>", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buf, once = true, callback = function() finish(nil) end })
  vim.cmd("startinsert!")
end

--- `bc`: prompt for a name (prefilled with the repo's branch prefix) and
--- create + switch to the new branch, then refresh the status view.
---@param buf integer
function M._create_branch(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  local cwd = status and status.cwd
  if not cwd then
    notify_error("Not a git repository", "DiffReview")
    return
  end
  local prefix = M._repo_config.branch_prefix(cwd)
  M._prompt_branch_name(prefix, function(name)
    if not name then return end
    name = vim.trim(name)
    if name == "" or name == prefix then return end
    run_git_at_root_async(cwd, { "switch", "-c", name }, nil, function(result)
      if not result.ok then
        notify_error(
          "Create branch failed: " .. (result.output ~= "" and result.output or ("git exited " .. tostring(result.code))),
          "DiffReview"
        )
        return
      end
      vim.notify("Created branch " .. name, vim.log.levels.INFO, { title = "DiffReview" })
      if vim.api.nvim_buf_is_valid(buf) then
        M._status = status
        status.pr = nil
        status.about = nil
        render_status_or_notify(buf, nil, nil, { restore_initial_folds = true, refresh_pr = true, refresh_about = true })
      end
    end)
  end)
end

-- Branch-diff view (":GitDiff <branch>") helpers. Grouped on the module
-- table rather than as locals: this chunk is at Lua's hard limit of 200
-- local variables, so new file-scope `local function`s break the module.
M._branch_diff = {}

---@param branch string
---@param file? string repo-relative path when diffing a single file
---@return DiffReviewStatusHeadLine[]
function M._branch_diff.head_lines(branch, file)
  local lines = {
    {
      segments = {
        { ("%-8s"):format("Diff:"), "DiffReviewStatusLabel" },
        { branch, "DiffReviewStatusBranch" },
        { " -> working tree", "DiffReviewStatusHint" },
      },
    },
  }
  if file then
    lines[#lines + 1] = {
      segments = {
        { ("%-8s"):format("File:"), "DiffReviewStatusLabel" },
        { file, "DiffReviewStatusPath" },
      },
    }
  end
  return lines
end

---@param cwd string
---@param branch string
---@param diff_text? string
---@param file? string
---@return DiffReviewStatusSection[]
function M._branch_diff.sections(cwd, branch, diff_text, file)
  local provider_key = "diff:" .. branch .. (file and (":" .. file) or "")
  local sections = M._section_builder.sections_from_diff(cwd, {
    title = "Changes vs " .. branch,
    section_name = provider_key .. ":changes",
    default_status = "",
    name = provider_key .. ":changes",
    file_key_prefix = provider_key,
    file_entry_kind = "pr_file",
    hunk_entry_kind = "pr_hunk",
  }, diff_text)
  return sections
end

---@param branch string
---@param cwd string
---@param buf integer
---@param diff_text? string
---@param file? string
function M._branch_diff.render(branch, cwd, buf, diff_text, file)
  local status = M._status
  if not (status and status.buf == buf) then return end
  status.head_lines = M._branch_diff.head_lines(branch, file)
  status.sections = M._branch_diff.sections(cwd, branch, diff_text, file)
  status.fancy_rows = {}
  status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param branch string
---@param cwd string
---@param buf integer
---@param file? string
function M._branch_diff.load(branch, cwd, buf, file)
  local status = M._status
  if not (status and status.buf == buf) then return end
  status.branch_diff_request_id = (status.branch_diff_request_id or 0) + 1
  local request_id = status.branch_diff_request_id
  local command = { "git", "-C", cwd, "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff", branch }
  if file then
    vim.list_extend(command, { "--", file })
  end
  systemlist_async(command, function(output, code, stderr)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.branch_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    if code ~= 0 then
      local message = vim.trim(stderr or "")
      notify_error("Git diff failed: " .. (message ~= "" and message or ("git exited " .. code)), "GitBranchDiff")
      return
    end
    M._branch_diff.render(branch, cwd, buf, table.concat(output or {}, "\n"), file)
  end)
end

---@param target_id? string
status_render_current_model = function(target_id)
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
  if target_id then
    render_status_or_notify(buf, target_id, vim.api.nvim_win_get_cursor(0)[1])
  else
    render_status_or_notify(buf)
  end
end

status_operations_pending = function()
  local status = M._status
  return status and (status.operation_running or #(status.operation_queue or {}) > 0) or false
end

---@param buf integer?
---@param target_id? string
status_request_reconcile = function(buf, target_id)
  local status = M._status
  if not status then return end
  status.reconcile_buf = buf or status.buf
  status.reconcile_target_id = target_id
  if status_operations_pending() or not status.reconcile_buf then return end

  status.reconcile_generation = (status.reconcile_generation or 0) + 1
  local generation = status.reconcile_generation
  vim.defer_fn(function()
    local latest_status = M._status
    if not latest_status or latest_status.reconcile_generation ~= generation then return end
    if status_operations_pending() then return end
    local reconcile_buf = latest_status.reconcile_buf
    local reconcile_target_id = latest_status.reconcile_target_id
    latest_status.reconcile_buf = nil
    latest_status.reconcile_target_id = nil
    if reconcile_buf then refresh_status_after_action(reconcile_buf, reconcile_target_id) end
  end, status_reconcile_delay_ms)
end

---@param operation fun(done: fun())
local function status_enqueue_operation(operation)
  M._status = M._status or {}
  local status = M._status
  status.reconcile_generation = (status.reconcile_generation or 0) + 1
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

-- PR title/description editing in the PR view: insert mode is gated to the
-- title line and description block, ":w" syncs via `gh pr edit`, and a "*"
-- marks unsynced fields. Functions hang off the module table because
-- init.lua is at Lua's 200-local limit.
M._pr_edit = { ns = vim.api.nvim_create_namespace("diff_review_pr_edit") }

---@class DiffReviewPrEditState
---@field queue fun(done: fun())[]
---@field running boolean
---@field title_mark? integer
---@field desc_start_mark? integer
---@field desc_end_mark? integer beyond-the-last-body-line mark (end-exclusive)
---@field title_marker_id? integer
---@field desc_marker_id? integer
---@field lock_initial? boolean
---@field initial_cursor_row? integer

---@param buf integer
---@param id integer?
---@return integer? row0
function M._pr_edit.mark_row(buf, id)
  if not id then return nil end
  local pos = vim.api.nvim_buf_get_extmark_by_id(buf, M._pr_edit.ns, id, {})
  return pos and pos[1] or nil
end

---@param buf integer
---@param status table
---@return string? title without the "Title:" label
---@return string? desc description block joined with newlines
function M._pr_edit.current_values(buf, status)
  local state = status.pr_edit
  if not state then return nil end
  local title_row0 = M._pr_edit.mark_row(buf, state.title_mark)
  local first0 = M._pr_edit.mark_row(buf, state.desc_start_mark)
  local after0 = M._pr_edit.mark_row(buf, state.desc_end_mark)
  if not (title_row0 and first0 and after0) then return nil end
  local title_line = vim.api.nvim_buf_get_lines(buf, title_row0, title_row0 + 1, false)[1] or ""
  local title = vim.trim((title_line:gsub("^Title:%s*", "")))
  local desc = table.concat(vim.api.nvim_buf_get_lines(buf, first0, after0, false), "\n")
  return title, desc
end

---@param buf integer
---@param status table
---@return boolean title_dirty
---@return boolean desc_dirty
function M._pr_edit.dirty_flags(buf, status)
  local title, desc = M._pr_edit.current_values(buf, status)
  if title == nil or not status.pr then return false, false end
  local baseline = table.concat(status_markdown_lines(status.pr.body), "\n")
  return title ~= status.pr.title, desc ~= baseline
end

---@param buf integer
---@param row integer 1-based cursor row
---@return "title"|"desc"|nil
function M._pr_edit.region_kind_at(buf, row)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not state then return nil end
  local title_row0 = M._pr_edit.mark_row(buf, state.title_mark)
  local first0 = M._pr_edit.mark_row(buf, state.desc_start_mark)
  local after0 = M._pr_edit.mark_row(buf, state.desc_end_mark)
  if title_row0 and row == title_row0 + 1 then return "title" end
  if first0 and after0 and row >= first0 + 1 and row <= after0 then return "desc" end
  return nil
end

---@param buf integer
---@param state DiffReviewPrEditState
function M._pr_edit.clear_markers(buf, state)
  for _, key in ipairs({ "title_marker_id", "desc_marker_id" }) do
    if state[key] then
      pcall(vim.api.nvim_buf_del_extmark, buf, M._pr_edit.ns, state[key])
      state[key] = nil
    end
  end
end

---Show/hide the "*" out-of-sync markers on the Title/Description labels.
---@param buf integer
function M._pr_edit.refresh_markers(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end
  local title_dirty, desc_dirty = M._pr_edit.dirty_flags(buf, status)
  local title_row0 = M._pr_edit.mark_row(buf, state.title_mark)
  local first0 = M._pr_edit.mark_row(buf, state.desc_start_mark)

  local function set_marker(key, wanted, row0)
    if wanted and not state[key] and row0 then
      state[key] = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, row0, 0, {
        virt_text = { { "*", "DiffReviewPrDirty" } },
        virt_text_pos = "inline",
        right_gravity = false,
      })
    elseif not wanted and state[key] then
      pcall(vim.api.nvim_buf_del_extmark, buf, M._pr_edit.ns, state[key])
      state[key] = nil
    end
  end

  set_marker("title_marker_id", title_dirty, title_row0)
  set_marker("desc_marker_id", desc_dirty, first0 and first0 - 1 or nil)
end

---Re-renders are blocked while the buffer holds unsynced PR edits; they
---would clobber them.
---@param buf integer
---@return boolean
function M._pr_edit.blocks_render(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and status.pr and vim.api.nvim_buf_is_valid(buf)) then return false end
  local title_dirty, desc_dirty = M._pr_edit.dirty_flags(buf, status)
  if not (title_dirty or desc_dirty) then return false end
  vim.notify("Unsynced PR edits — :w to sync before refreshing", vim.log.levels.WARN, { title = "DiffReview" })
  return true
end

---Locate the title/description regions after a render and re-anchor the
---tracking extmarks. Rendering wipes the buffer, so this runs on every
---PR-view render.
---@param buf integer
function M._pr_edit.on_render(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and status.pr and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.api.nvim_buf_clear_namespace(buf, M._pr_edit.ns, 0, -1)
  state.title_mark, state.desc_start_mark, state.desc_end_mark = nil, nil, nil
  state.title_marker_id, state.desc_marker_id = nil, nil

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local title_row, label_row
  for index, line in ipairs(lines) do
    if not title_row and line:match("^Title:") then title_row = index end
    if not label_row and line == "Description:" then label_row = index end
    if title_row and label_row then break end
  end
  if not (title_row and label_row) then return end

  -- right_gravity=false: a mark with right gravity slides to the next line
  -- when its own line is replaced (retyped), losing the region.
  local body_count = #status_markdown_lines(status.pr.body)
  state.title_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, title_row - 1, 0, { right_gravity = false })
  state.desc_start_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, label_row, 0, { right_gravity = false })
  state.desc_end_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, label_row + body_count, 0, { right_gravity = false })
  vim.bo[buf].modified = false
  M._pr_edit.refresh_markers(buf)
  M._pr_edit.sync_modifiable(buf)
end

---Sequential sync queue, mirroring status_enqueue_operation without its
---status-view reconcile tail (which would rebuild the PR buffer as a
---status view).
---@param state DiffReviewPrEditState
---@param operation fun(done: fun())
function M._pr_edit.enqueue(state, operation)
  state.queue[#state.queue + 1] = operation
  local function run_next()
    if state.running then return end
    local next_operation = table.remove(state.queue, 1)
    if not next_operation then return end
    state.running = true
    next_operation(function()
      state.running = false
      run_next()
    end)
  end
  run_next()
end

---BufWriteCmd handler: clear the "*" markers immediately, then sync the
---dirty fields to GitHub through the queue.
---@param buf integer
function M._pr_edit.sync(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (status and state and status.pr) then return end
  M._status = status
  local title, desc = M._pr_edit.current_values(buf, status)
  local title_dirty, desc_dirty = M._pr_edit.dirty_flags(buf, status)
  vim.bo[buf].modified = false
  if not (title_dirty or desc_dirty) then return end

  M._pr_edit.clear_markers(buf, state)
  local pr = status.pr
  local edit = {
    title = title_dirty and title or nil,
    body = desc_dirty and desc or nil,
  }
  M._pr_edit.enqueue(state, function(done)
    gh.update_pr_async(status.cwd, pr.number, pr.repo, edit, function(result)
      if not vim.api.nvim_buf_is_valid(buf) then
        done()
        return
      end
      if result.code ~= 0 then
        notify_error(
          "GitHub PR update failed: " .. (result.output ~= "" and result.output or ("gh exited " .. tostring(result.code))),
          "DiffReview"
        )
        vim.bo[buf].modified = true
        M._pr_edit.refresh_markers(buf)
        done()
        return
      end
      if edit.title then pr.title = edit.title end
      if edit.body then pr.body = edit.body end
      local latest = M._status_states and M._status_states[buf] or nil
      if latest == status then
        M._status = status
        render_pr_status(pr, status.cwd, buf, status.pr_diff_text)
      end
      vim.notify(("PR #%s updated"):format(tostring(pr.number)), vim.log.levels.INFO, { title = "DiffReview" })
      done()
    end)
  end)
end

---Unlock the buffer exactly when the cursor sits in an editable region, so
---every native editing command works there and nowhere else.
---@param buf integer
function M._pr_edit.sync_modifiable(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if vim.api.nvim_get_current_buf() ~= buf then return end
  local row = M._normalize_status_cursor(buf) or vim.api.nvim_win_get_cursor(0)[1]
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  local wanted = M._pr_edit.region_kind_at(buf, row) ~= nil
    or M._review.in_editable_region(buf, row)
  if state and state.lock_initial then
    if state.initial_cursor_row == nil then state.initial_cursor_row = row end
    if row == state.initial_cursor_row then
      wanted = false
    else
      state.lock_initial = false
    end
  end
  if vim.bo[buf].modifiable ~= wanted then
    vim.bo[buf].modifiable = wanted
  end
end

---Wire editing into a freshly created PR-view buffer: acwrite, the
---cursor-follows-modifiable lock, and lifecycle autocmds.
---@param buf integer
function M._pr_edit.attach(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if not status then return end
  status.pr_edit = { queue = {}, running = false, lock_initial = true }
  vim.bo[buf].buftype = "acwrite"

  -- The title is single-line: swallow newline attempts there.
  vim.keymap.set("i", "<CR>", function()
    local row = vim.api.nvim_win_get_cursor(0)[1]
    if M._pr_edit.region_kind_at(buf, row) == "title" then return "" end
    return vim.api.nvim_replace_termcodes("<CR>", true, false, true)
  end, { buffer = buf, expr = true })

  local group = vim.api.nvim_create_augroup("DiffReviewPrEdit" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._pr_edit.sync_modifiable(buf)
    end,
  })
  vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._review.sync_inline_comment_text(buf)
      M._pr_edit.refresh_markers(buf)
    end,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buf,
    callback = function()
      M._review.sync_inline_comment_text(buf)
      M._pr_overview.sync_standalone_comments(buf)
      M._pr_edit.sync(buf)
    end,
  })
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
---@return string[] tracked_files
---@return string[] untracked_files
local function status_split_action_entries(entries)
  local hunk_diffs = {}
  local tracked_files = {}
  local untracked_files = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk then
      hunk_diffs[#hunk_diffs + 1] = entry.hunk.diff
    elseif entry.kind == "file" and entry.file then
      if entry.file.untracked then
        untracked_files[entry.file.filename] = true
      else
        tracked_files[entry.file.filename] = true
      end
    end
  end
  return hunk_diffs, status_files_from_set(tracked_files), status_files_from_set(untracked_files)
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
---@return DiffReviewStatusSectionName?
local function status_unstage_target_section(entries)
  local has_added = false
  for _, entry in ipairs(entries) do
    if status_entry_is_added(entry) then
      has_added = true
    else
      return "unstaged"
    end
  end
  if has_added then return "untracked" end
  return nil
end

---@class DiffReviewStatusActionOpts
---@field preserve_cursor? boolean

---@param entries DiffReviewStatusEntry[]
---@param opts? DiffReviewStatusActionOpts
local function status_stage_entries(entries, opts)
  opts = opts or {}
  if #entries == 0 then return end
  local expanded_entries = status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_action_entries_for_target(expanded_entries, "staged")
  if #action_entries == 0 then return end

  local target_id = nil
  if not opts.preserve_cursor then
    target_id = status_action_target_id(entries, action_entries, "staged", { file_target = "next" })
  end
  local hunk_diffs, tracked_files, untracked_files = status_split_action_entries(action_entries)
  local staged_hunks = 0
  local staged_files = 0
  local status_buf = M._status and M._status.buf

  status_apply_optimistic_entries(action_entries, "staged", target_id)

  local function finish()
    if staged_hunks > 0 or staged_files > 0 then
      status_notify_action("Staged", staged_hunks, staged_files)
    end
    status_request_reconcile(status_buf, target_id)
  end

  local function stage_untracked_files()
    if #untracked_files == 0 then
      finish()
      return
    end
    M.stage_files_async(untracked_files, function(result)
      staged_files = staged_files + #result.successes
      finish()
    end)
  end

  local function stage_files_after_hunks()
    if #tracked_files == 0 then
      stage_untracked_files()
      return
    end
    M.stage_tracked_files_async(tracked_files, function(result)
      staged_files = staged_files + #result.successes
      stage_untracked_files()
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
---@param opts? DiffReviewStatusActionOpts
local function status_unstage_entries(entries, opts)
  opts = opts or {}
  if #entries == 0 then return end
  local expanded_entries = status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_unstage_action_entries(expanded_entries)
  if #action_entries == 0 then return end

  local target_id = nil
  if not opts.preserve_cursor then
    target_id = status_action_target_id(entries, action_entries, status_unstage_target_section(action_entries))
  end
  local tracked_entries, added_entries = status_partition_unstage_entries(action_entries)
  local hunk_diffs, files, added_files = status_split_unstage_entries(action_entries)
  local unstaged_hunks = 0
  local unstaged_files = 0
  local status_buf = M._status and M._status.buf

  status_apply_optimistic_entries(tracked_entries, "unstaged", target_id)
  status_apply_optimistic_entries(added_entries, "untracked", target_id)

  local function finish()
    if unstaged_hunks > 0 or unstaged_files > 0 then
      status_notify_action("Unstaged", unstaged_hunks, unstaged_files)
    end
    status_request_reconcile(status_buf, target_id)
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
  local status_buf = M._status and M._status.buf
  git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    local failures = {}
    if #entries == 0 then return end

    local function finish_all()
      if #failures > 0 then M.notify_git_failures("Discard failed", failures) end
      refresh_status_after_action(status_buf, target_id)
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
          ---@param result DiffReviewGitCommandResult
          local function add_failure(result)
            failures[#failures + 1] = {
              file = entry.file.filename,
              output = result.output,
              code = result.code,
            }
          end

          ---@param path string
          ---@param context string
          ---@return boolean
          local function delete_file(path, context)
            local delete_code = delete_path(path)
            if delete_code == 0 then return true end
            failures[#failures + 1] = {
              file = path,
              message = ("delete() failed with code %d%s"):format(delete_code, context),
            }
            return false
          end

          if entry.file.section_name == "unstaged" then
            if git_status_is_added(entry.file.git_status) then
              run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
                if restore_result.ok then
                  delete_file(entry.file.filename, " after unstaging")
                else
                  add_failure(restore_result)
                end
                next_entry()
              end)
            else
              run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end
          elseif git_status_is_added(entry.file.git_status) then
            run_git_at_root_async(cwd, { "rm", "--cached", "--ignore-unmatch", "--", relpath }, nil, function(rm_result)
              if rm_result.ok then
                delete_file(entry.file.filename, " after unstaging")
              else
                add_failure(rm_result)
              end
              next_entry()
            end)
          elseif git_status_is_renamed(entry.file.git_status) then
            local original_relpath = entry.file.original_relpath
            if not original_relpath or original_relpath == "" then
              failures[#failures + 1] = { file = entry.file.filename, message = "Missing original path for renamed file" }
              next_entry()
            else
              run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath, original_relpath }, nil, function(restore_result)
                if not restore_result.ok then
                  add_failure(restore_result)
                  next_entry()
                  return
                end
                run_git_at_root_async(cwd, { "checkout", "--", original_relpath }, nil, function(checkout_result)
                  if checkout_result.ok then
                    delete_file(entry.file.filename, " after restoring renamed file")
                  else
                    add_failure(checkout_result)
                  end
                  next_entry()
                end)
              end)
            end
          elseif git_status_is_deleted(entry.file.git_status) then
            run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end)
          else
            run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end)
          end
        end
      end
    end

    discard_at(1)
  end)
end

---@param entries DiffReviewStatusEntry[]
---@param target_id? string
---@param opts? DiffReviewStatusActionOpts
local function status_discard_entry_list(entries, target_id, opts)
  opts = opts or {}
  local discard_entries = {}
  for _, entry in ipairs(status_expanded_entries(entries)) do
    if entry.kind == "hunk" or entry.kind == "file" then
      discard_entries[#discard_entries + 1] = entry
    end
  end
  if #discard_entries == 0 then return end
  local action_target_id = nil
  if not opts.preserve_cursor then
    action_target_id = status_action_target_id(entries, discard_entries) or target_id
  end

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
    status_discard_entries(discard_entries, action_target_id)
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

-- File revision buffers: read-only scratch buffers holding a file's content
-- at a git revision, named GitFileRevision://<path>@<rev>. Functions hang off
-- the module table because init.lua is at Lua's 200-local limit.
M._file_revision = { buffers = {} }

---Resolve the revision holding the pre-change content for a deleted diff
---line: the base of the diff the entry was rendered from, which is the only
---revision where the old line number is exact.
---@param entry DiffReviewStatusEntry
---@param status table?
---@return string? rev revision for `git show <rev>:<path>`
---@return string? path repo-relative path in that revision
function M._file_revision.target(entry, status)
  if not (entry.hunk and entry.file and status and status.cwd) then return nil end
  local relpath = repo_relative(entry.file.filename, status.cwd)
  if not relpath then return nil end
  local view_kind = status.view_kind or "status"
  if view_kind == "diff" and status.diff_branch then
    return status.diff_branch, relpath
  end
  if view_kind == "status" and entry.kind == "hunk" then
    if entry.hunk.staged then
      return "HEAD", entry.hunk.git_original_file or relpath
    end
    return ":0", relpath
  end
  return nil
end

---Open (or refresh) the read-only buffer for `path` as it exists at `rev`.
---The buffer is named with the short sha of the underlying commit (HEAD for
---the index revision `:0`, since the index is not a commit).
---@param opts { rev: string, path: string, cwd: string, line: integer?, on_error: fun(message: string) }
function M._file_revision.open(opts)
  local label_rev = opts.rev == ":0" and "HEAD" or opts.rev
  systemlist_async({ "git", "-C", opts.cwd, "rev-parse", "--short", label_rev }, function(label_lines, label_code)
    local label = label_code == 0 and vim.trim(tostring(label_lines and label_lines[1] or "")) or ""
    if label == "" then label = opts.rev end
    M._file_revision.show(opts, label)
  end)
end

---@param opts { rev: string, path: string, cwd: string, line: integer?, on_error: fun(message: string) }
---@param label string short sha (or raw revision when it cannot be resolved)
function M._file_revision.show(opts, label)
  local command = { "git", "-C", opts.cwd, "show", ("%s:%s"):format(opts.rev, opts.path) }
  systemlist_async(command, function(lines, code, output)
    if code ~= 0 then
      opts.on_error(vim.trim(tostring(output or "")))
      return
    end
    local name = ("GitFileRevision://%s@%s"):format(opts.path, label)
    local buf = M._file_revision.buffers[name]
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then
      buf = vim.api.nvim_create_buf(true, false)
      -- "nowrite", not "nofile": buffer pickers (e.g. snacks) hide nofile
      -- scratch buffers, and this buffer should stay reachable like a file.
      vim.bo[buf].buftype = "nowrite"
      vim.bo[buf].bufhidden = "hide"
      vim.bo[buf].swapfile = false
      if not pcall(vim.api.nvim_buf_set_name, buf, name) then
        pcall(vim.api.nvim_buf_set_name, buf, ("%s#%d"):format(name, buf))
      end
      vim.keymap.set("n", "q", function()
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end, { buffer = buf, silent = true, nowait = true, desc = "Close file revision" })
      -- Sticky header: a red winbar marks the buffer as a historical revision.
      -- Window-local, so it must be applied/cleared as windows show the
      -- buffer; dropbar skips windows whose winbar is already set.
      local header = ("%%#DiffReviewFileRevisionHeader# %s @ %s — read-only revision %%*"):format(
        opts.path:gsub("%%", "%%%%"),
        label:gsub("%%", "%%%%")
      )
      vim.api.nvim_create_autocmd("BufWinEnter", {
        buffer = buf,
        callback = function()
          vim.wo.winbar = header
        end,
      })
      vim.api.nvim_create_autocmd("BufWinLeave", {
        buffer = buf,
        callback = function()
          vim.wo.winbar = ""
        end,
      })
      M._file_revision.buffers[name] = buf
    end
    vim.bo[buf].readonly = false
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
    vim.bo[buf].readonly = true
    local ft = detect_filetype(opts.path, lines)
    if ft ~= "" and vim.bo[buf].filetype ~= ft then
      vim.bo[buf].filetype = ft
    end
    vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
    if opts.line then
      local target = math.min(math.max(opts.line, 1), vim.api.nvim_buf_line_count(buf))
      pcall(vim.api.nvim_win_set_cursor, 0, { target, 0 })
      vim.cmd("normal! zz")
    end
  end)
end

---Open a read-only buffer with `file` as it exists at git revision `rev`.
---Entry point for :GitFileRevision.
---@param file string
---@param rev string
function M.open_file_revision(file, rev)
  file = vim.trim(tostring(file or ""))
  rev = vim.trim(tostring(rev or ""))
  if file == "" or rev == "" then
    notify_error("GitFileRevision requires a file and a revision", "DiffReview")
    return
  end
  git_root_async(function(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "DiffReview")
      return
    end
    local relpath, rel_err = repo_relative(file, root)
    if not relpath then
      notify_error(rel_err or ("Path is outside the git root: " .. file), "DiffReview")
      return
    end
    M._file_revision.open({
      rev = rev,
      path = relpath,
      cwd = root,
      on_error = function(message)
        notify_error(message ~= "" and message or ("Git show failed for %s:%s"):format(rev, relpath), "DiffReview")
      end,
    })
  end)
end

---@param on_yes fun()
function M._status_confirm_create_pr(on_yes)
  local body = {
    "No GitHub PR found for this branch.",
    "",
    "Create a draft PR now?",
    "",
    "  [y] yes    [n] no",
  }
  local width = 40
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
    title = " GitStatus ",
    title_pos = "center",
  })

  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end

  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true, desc = "Create pull request" })

  vim.keymap.set("n", "n", close, { buffer = buf, nowait = true, silent = true, desc = "Cancel pull request creation" })
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true, desc = "Cancel pull request creation" })
  end
end

---@param entry DiffReviewStatusEntry?
status_open_pr = function(entry)
  local pr = entry and entry.pr
  if not pr and M._status and M._status.pr and M._status.pr.state == "ready" then
    pr = M._status.pr.pr
  end
  if not pr then
    local pr_state = M._status and M._status.pr
    if pr_state and pr_state.state == "fetching" then
      pr_state.open_when_ready = true
      pr_state.open_when_ready_win = vim.api.nvim_get_current_win()
      return
    end
    if pr_state and pr_state.state == "error" then
      notify_error("GitHub PR lookup failed: " .. (pr_state.message or "Unable to fetch GitHub pull request"), "DiffReview")
      return
    end
    if pr_state and pr_state.state == "unavailable" then
      vim.notify(pr_state.message or "GitHub PR lookup is unavailable", vim.log.levels.INFO, { title = "DiffReview" })
      return
    end
    M._status_confirm_create_pr(function()
      require("github.open_pr").open()
    end)
    return
  end
  M.open_pr(pr, { cwd = M._status and M._status.cwd or nil })
end

---@param entry DiffReviewStatusEntry?
local function status_open_about(entry)
  local about = entry and entry.about
  if not about and M._status then about = M._status.about end
  if not about or about.state == "none" then
    local status = M._status
    if status and status.cwd and status.buf and status_has_changes(status.sections) then
      status_ensure_about_state(status.cwd, status.buf, true, false, true)
      vim.notify("Generating commit message...", vim.log.levels.INFO, { title = "DiffReview" })
      return
    end
    vim.notify("No generated commit message", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  if about.state == "generating" then
    vim.notify("Commit message is still generating", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  if about.state == "error" then
    vim.notify(about.error or "Commit message generation failed", vim.log.levels.WARN, { title = "DiffReview" })
    return
  end
  if not about.message or about.message == "" then return end

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "gitcommit"
  local name = ("DiffReviewAbout://%s"):format(vim.fn.sha256(about.message):sub(1, 8))
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(about.message, "\n", { plain = true }))
  vim.bo[buf].modifiable = false
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
end

---@param entry DiffReviewStatusEntry?
---@param skip_revision boolean? open the working-tree file even for deleted lines
local function status_jump(entry, skip_revision)
  if not (entry and entry.file and entry.file.filename) then return end
  if not skip_revision and entry.diff_line and entry.diff_line.side == "left" and entry.diff_line.line then
    local rev, path = M._file_revision.target(entry, M._status)
    if rev and path then
      M._file_revision.open({
        rev = rev,
        path = path,
        cwd = M._status.cwd,
        line = entry.diff_line.line,
        on_error = function()
          status_jump(entry, true)
        end,
      })
      return
    end
  end
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
  if entry.kind == "file" or entry.kind == "pr_file" then
    default = true
  elseif entry.kind == "commit" or entry.kind == "commit_file" then
    default = true
  elseif entry.kind == "pr_review" or entry.kind == "pr_review_file" then
    default = true
  elseif entry.kind == "section" then
    local section_config = status_section_by_name[entry.section.name]
    default = section_config and section_config.default_folded or entry.section.default_folded
  end
  local next_folded = not status_folded(entry.id, default)
  if not next_folded then
    status_prewarm_entry_syntax(entry)
  end
  set_status_folded(entry.id, next_folded)
  if entry.kind == "commit" and not next_folded and entry.commit then
    status_load_commit_files(entry.commit)
  end
  render_status_or_notify(M._status.buf, entry.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
end

function M._status_collapse_parent()
  local line, entry = M._status_entry_line_under_cursor()
  if not (line and entry) then return end
  local parent = M._status_parent_entry(line, entry)
  local target = parent or entry
  set_status_folded(target.id, true)
  render_status_or_notify(M._status.buf, target.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
end

---@param title string
---@param lines string[]
---@return integer
local function status_open_popup(title, lines)
  local width = #title + 4
  for _, line in ipairs(lines) do
    width = math.max(width, #line + 4)
  end
  width = math.min(math.max(width, 44), math.max(vim.o.columns - 4, 20))
  local height = math.min(#lines, math.max(vim.o.lines - 6, 1))

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "DiffReviewHelp"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " ",
    title_pos = "center",
  })

  local function close()
    if vim.api.nvim_win_is_valid(win) then
      pcall(vim.api.nvim_win_close, win, true)
    end
  end
  for _, key in ipairs({ "q", "<Esc>", status_primary_key("help") }) do
    if key ~= "" then
      vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true, desc = "Close help" })
    end
  end

  local key_width = 0
  for _, spec in ipairs(status_command_specs) do
    if status_command_visible(spec) then
      key_width = math.max(key_width, #status_key_text(status_keys_for(spec.id)))
    end
  end
  local line_index = 0
  for _, spec in ipairs(status_command_specs) do
    if not status_command_visible(spec) then goto continue end
    local key_text = status_key_text(status_keys_for(spec.id))
    if key_text ~= "" then
      line_index = line_index + 1
      pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, line_index - 1, 2, {
        end_col = 2 + #key_text,
        hl_group = "DiffReviewStatusHintKey",
        priority = 90,
      })
      pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, line_index - 1, key_width + 6, {
        end_col = #lines[line_index],
        hl_group = "DiffReviewStatusHint",
        priority = 80,
      })
    end
    ::continue::
  end

  return buf
end

local function status_show_help()
  local key_width = 0
  for _, spec in ipairs(status_command_specs) do
    if status_command_visible(spec) then
      key_width = math.max(key_width, #status_key_text(status_keys_for(spec.id)))
    end
  end

  local lines = {}
  for _, spec in ipairs(status_command_specs) do
    if not status_command_visible(spec) then goto continue end
    local key_text = status_key_text(status_keys_for(spec.id))
    if key_text ~= "" then
      lines[#lines + 1] = ("  %-" .. key_width .. "s  %s"):format(key_text, spec.desc)
    end
    ::continue::
  end
  status_open_popup("DiffReview Commands", lines)
end

---@param buf integer
---@param action "push"|"pull"
local function status_remote_action(buf, action)
  git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    local title = action == "push" and "Push" or "Pull"
    local running_status = action == "push" and "Pushing..." or "Pulling..."
    local function update_remote_status(line)
      line = vim.trim(line or "")
      if line == "" then return end
      if M._status then
        M._status.remote_action = M._status.remote_action or { action = action, state = "running" }
        M._status.remote_action.status = line
        if M._status.head_values then
          M._status.head_lines = status_build_head_lines(M._status.head_values, M._status.pr, M._status.about)
          if vim.api.nvim_buf_is_valid(buf) then
            M.render_status(buf, nil, nil, { reuse_sections = true })
          end
        end
      end
    end
    if M._status then
      M._status.remote_action = { action = action, state = "running", status = running_status }
      if M._status.head_values then
        M._status.head_lines = status_build_head_lines(M._status.head_values, M._status.pr, M._status.about)
        if vim.api.nvim_buf_is_valid(buf) then
          M.render_status(buf, nil, nil, { reuse_sections = true })
        end
      end
    end
    notify_debug(title .. "ing changes...", vim.log.levels.INFO, { title = "DiffReview" })
    system_text_stream_async({ "git", "-C", cwd, action, "--progress" }, nil, update_remote_status, function(result)
      local compact = {}
      for _, line in ipairs(text_to_lines((result.stdout or "") .. (result.stderr or ""))) do
        if line ~= "" then
          compact[#compact + 1] = line
        end
      end
      if result.code == 0 then
        notify_debug(title .. " complete", vim.log.levels.INFO, { title = "DiffReview" })
      else
        notify_error(title .. " failed: " .. (#compact > 0 and table.concat(compact, "\n") or ("git exited " .. result.code)))
      end
      if vim.api.nvim_buf_is_valid(buf) then
        if M._status then M._status.remote_action = nil end
        render_status_or_notify(buf)
      end
    end)
  end)
end

--- Narrow interface handed to diff_review.walkthrough so the module never
--- reaches into init internals.
---@param buf integer
---@return DiffReviewWalkthroughHost
function M._walkthrough_host(buf)
  return {
    buf = buf,
    cwd = function()
      local state = M._status_states and M._status_states[buf] or M._status
      return state and state.cwd
    end,
    get_state = function()
      return M._status_states and M._status_states[buf] or M._status
    end,
    file_key = status_file_key,
    hunk_key = status_hunk_key,
    set_folded = set_status_folded,
    rerender = function()
      render_status_or_notify(buf, nil, nil, { reuse_sections = true })
    end,
    git_list_async = systemlist_async,
  }
end

---@param buf integer
local function setup_status_keymaps(buf)
  local opts = { buffer = buf, silent = true, nowait = true }
  local view_kind = M._status and M._status.view_kind or "status"
  local is_pr_view = view_kind == "pr"
  local function map(command_id, mode, callback, desc)
    local spec = status_command_specs_by_id[command_id]
    if spec and not status_command_visible(spec) then return end
    for _, key in ipairs(status_keys_for(command_id)) do
      local map_opts = vim.tbl_extend("force", opts, {
        desc = desc or (spec and spec.desc) or command_id,
      })
      local mapped
      mapped = function(...)
        if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
        callback(...)
      end
      if view_kind == "review" then
        M._review.register_command_map(buf, command_id, mode, key, mapped, map_opts)
      else
        vim.keymap.set(mode, key, mapped, map_opts)
      end
    end
  end

  map("close", "n", function()
    local state = M._status_states and M._status_states[buf] or M._status
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    if M._status_states then M._status_states[buf] = nil end
    if M._main_status == state then M._main_status = nil end
    if M._status == state then M._status = M._main_status end
  end)

  map("refresh", "n", function()
    if view_kind == "diff" then
      local status = M._status_states and M._status_states[buf] or M._status
      if status and status.diff_branch and status.cwd then
        M._status = status
        M._branch_diff.load(status.diff_branch, status.cwd, buf, status.diff_file)
      end
      return
    end
    if is_pr_view then
      local status = M._status_states and M._status_states[buf] or M._status
      if status and status.pr and status.cwd then
        M._status = status
        render_pr_status(status.pr, status.cwd, buf)
        load_pr_diff(status.pr, status.cwd, buf)
        M._pr_overview.load_comments(status.pr, status.cwd, buf)
      end
      return
    end
    if M._status then
      M._status.pr = nil
      M._status.about = nil
    end
    render_status_or_notify(buf, nil, nil, {
      restore_initial_folds = true,
      refresh_pr = true,
      refresh_about = true,
    })
  end)

  map("toggle", "n", function()
    if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
    if view_kind == "review" and M._review.toggle_comment_fold(buf) then return end
    status_toggle(status_entry_under_cursor())
  end)

  map("collapse_parent", "n", function()
    if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
    M._status_collapse_parent()
  end)

  -- The review view gets its own action set (S/U/C/y/n/submit); the read-only
  -- PR and branch-diff views only get navigation commands.
  if view_kind == "review" then
    M._review.setup_keymaps(buf)
    map("browse", { "n", "x" }, function()
      local status = M._status_states and M._status_states[buf] or M._status
      local fragment = M._review.browse_fragment_under_cursor(buf)
      M._review.leave_visual()
      if not gh.browse_pr_changes(status and status.pr, fragment) then
        notify_error("Unable to open PR URL", "DiffReview")
      end
    end)
    local function review_open_action()
      if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
      status_jump(status_entry_under_cursor())
    end
    map("open", "n", review_open_action, "Jump to file")
    vim.schedule(function()
      if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
      if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
      map("open", "n", review_open_action, "Jump to file")
    end)
    map("help", "n", status_show_help)
    return
  end
  if view_kind ~= "status" then
    map("browse", "n", function()
      local status = M._status_states and M._status_states[buf] or M._status
      if status and status.view_kind == "pr" then
        local url = M._pr_overview.url_under_cursor(buf)
        if url and gh.browse_url(url) then return end
      end
      local pr = status and status.pr
      if not gh.browse_pr(pr) then
        notify_error("Unable to open PR URL", "DiffReview")
      end
    end)
    if is_pr_view then
      map("comment", { "n", "x" }, function()
        M._pr_overview.add_standalone_comment(buf)
      end)
      map("sync", { "n", "i" }, function()
        M._pr_overview.sync_standalone_comments(buf)
      end)
    end
    map("review", "n", function()
      M._review.start(buf)
    end)
    map("open", "n", function()
      if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
      status_jump(status_entry_under_cursor())
    end, "Jump to file")
    map("help", "n", status_show_help)
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = buf,
      callback = function()
        if (M.config or config.options or config.defaults).status_cursor_prewarm == false then return end
        if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
        status_defer_prewarm_under_cursor(buf)
      end,
    })
    return
  end

  map("stage", "n", function()
    status_stage(status_entry_under_cursor())
  end, "Stage hunk/file")
  map("stage", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_stage_entries(entries, { preserve_cursor = true })
  end, "Stage selection")

  map("unstage", "n", function()
    status_unstage(status_entry_under_cursor())
  end, "Unstage hunk/file")
  map("unstage", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_unstage_entries(entries, { preserve_cursor = true })
  end, "Unstage selection")

  map("discard", "n", function()
    status_discard(status_entry_under_cursor())
  end, "Discard hunk/file")
  map("discard", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_discard_entry_list(entries, nil, { preserve_cursor = true })
  end, "Discard selection")

  map("open", "n", function()
    local entry = status_entry_under_cursor()
    if entry and entry.kind == "pr" then
      status_open_pr(entry)
    elseif entry and entry.kind == "about" then
      status_open_about(entry)
    else
      status_jump(entry)
    end
  end)

  local function commit()
    require("diff_review.commit").commit({
      win = vim.api.nvim_get_current_win(),
      on_done = function()
        if vim.api.nvim_buf_is_valid(buf) then render_status_or_notify(buf) end
      end,
    })
  end
  map("commit", "n", commit)

  map("push", "n", function()
    status_remote_action(buf, "push")
  end)

  map("pull", "n", function()
    status_remote_action(buf, "pull")
  end)

  map("pr", "n", function()
    status_open_pr(status_entry_under_cursor())
  end)

  map("branch_create", "n", function()
    M._create_branch(buf)
  end)

  map("walkthrough", "n", function()
    require("diff_review.walkthrough").start(M._walkthrough_host(buf))
  end)

  map("review", "n", function()
    M._review.start(buf)
  end)

  map("help", "n", status_show_help)

  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      if (M.config or config.options or config.defaults).status_cursor_prewarm == false then return end
      if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
      status_defer_prewarm_under_cursor(buf)
    end,
  })
end

---@class DiffReviewOpenPROptions
---@field cwd? string
---@field repo? string

---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return DiffReviewGhPR
local function pr_with_resolved_repo(pr, opts)
  if pr.repo and pr.repo ~= "" then return pr end
  opts = opts or {}
  local repo = opts.repo and opts.repo ~= "" and opts.repo or gh.repo_from_pr_url(pr.url)
  if not repo then return pr end
  pr.repo = repo
  return pr
end

---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return integer? buf
function M.open_pr(pr, opts)
  opts = opts or {}
  if not pr then return nil end
  setup_bg_highlights()

  pr = pr_with_resolved_repo(pr, opts)
  local cwd = opts.cwd or (M._status and M._status.cwd) or vim.fn.getcwd()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GitStatus"
  local options = M.config or config.options or config.defaults
  local name = ("%s://%s"):format(options.pr_buffer_name, pr.number or "current")
  if pr.repo and pr.repo ~= "" then name = ("%s/%s"):format(name, pr.repo:gsub("/", "%%2F")) end
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  local state = {
    view_kind = "pr",
    buf = buf,
    cwd = cwd,
    pr = pr,
    folds = {},
    lines = {},
    entries = {},
    highlights = {},
    line_highlights = {},
    extmarks = {},
    boundary_lines = {},
    sections = nil,
    fancy_rows = {},
  }
  state.pr_standalone_comments = {}
  state.review_comments = state.pr_standalone_comments
  M._status = state
  attach_status_state(buf, state)
  setup_status_keymaps(buf)
  M._pr_edit.attach(buf)

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview PR open failed: " .. tostring(err))
    return nil
  end
  M._apply_status_window_options(win, state)
  vim.wo[win].foldcolumn = "0"

  render_pr_status(pr, cwd, buf)
  load_pr_diff(pr, cwd, buf)
  M._pr_overview.load_comments(pr, cwd, buf)
  return buf
end

---@param number integer|string
---@param opts? DiffReviewOpenPROptions
function M.open_pr_number(number, opts)
  opts = opts or {}
  local cwd = opts.cwd or vim.fn.getcwd()
  gh.pr_async(cwd, number, opts.repo, function(result)
    if not result.ok or not result.pr then
      notify_error(result.message or "Unable to load GitHub pull request", "DiffReview")
      return
    end
    M.open_pr(result.pr, { cwd = cwd, repo = opts.repo })
  end)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M._review.load_diff(pr, cwd, buf)
  local state = M._review.state(buf)
  if not state then return end
  state.pr_diff_request_id = (state.pr_diff_request_id or 0) + 1
  local request_id = state.pr_diff_request_id
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest = M._review.state(buf)
    if not (latest and latest.pr_diff_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    M._status = latest
    if result.code ~= 0 then
      notify_error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest.diff_text = result.stdout or ""
    M._review.render(buf)
  end)
end

--- Open a PR review buffer (":or"): the PR title, an editable review summary,
--- and the changed files split into Unviewed/Viewed sections.
---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return integer? buf
function M.open_review(pr, opts)
  opts = opts or {}
  if not pr then return nil end
  setup_bg_highlights()
  pr = pr_with_resolved_repo(pr, opts)
  local cwd = opts.cwd or (M._status and M._status.cwd) or vim.fn.getcwd()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GitStatus"
  local options = M.config or config.options or config.defaults
  local name = ("%sReview://%s"):format(options.pr_buffer_name, pr.number or "current")
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  local state = {
    view_kind = "review",
    buf = buf,
    cwd = cwd,
    pr = pr,
    commit_id = pr.headRefOid or "",
    diff_text = "",
    review_viewed = {},
    review_viewed_hunks = {},
    review_comment_text = "",
    review_comments = {},
    folds = {},
    lines = {},
    entries = {},
    highlights = {},
    line_highlights = {},
    extmarks = {},
    boundary_lines = {},
    sections = nil,
    fancy_rows = {},
  }
  M._review.load_draft(state)
  M._review.normalize_comments(state)
  M._status = state
  attach_status_state(buf, state)
  setup_status_keymaps(buf)
  M._review.attach(buf)

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview review open failed: " .. tostring(err))
    return nil
  end
  M._apply_status_window_options(win, state)
  vim.wo[win].foldcolumn = "0"

  status_set_plain_lines(buf, { "Loading GitHub pending review..." })
  M._review.load_remote_before_open(buf, function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    M._review.render(buf)
    M._review.load_diff(pr, cwd, buf)
  end)
  return buf
end

--- Resolve the PR from the current status/PR buffer and open its review.
---@param buf integer
function M._review.start(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not status then return end
  local pr
  if status.view_kind == "pr" then
    pr = status.pr
  elseif status.pr and status.pr.state == "ready" then
    pr = status.pr.pr
  end
  if not pr then
    vim.notify("No GitHub PR for this branch", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  M.open_review(pr, { cwd = status.cwd })
end

---@param buf integer
function M._review.setup_keymaps(buf)
  local function map(id, fn)
    local spec = status_command_specs_by_id[id]
    if not (spec and status_command_visible(spec)) then return end
    for _, key in ipairs(status_keys_for(id)) do
      local mapped
      mapped = function()
        if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
        fn()
      end
      M._review.register_command_map(buf, id, spec.modes, key, mapped, { buffer = buf, silent = true, nowait = true, desc = spec.desc })
    end
  end
  map("viewed", function() M._review.toggle_viewed(buf, true) end)
  map("unviewed", function() M._review.toggle_viewed(buf, false) end)
  map("comment", function() M._review.add_comment(buf) end)
  map("delete", function() M._review.delete_comment(buf) end)
  map("next_comment", function() M._review.navigate(buf, 1) end)
  map("prev_comment", function() M._review.navigate(buf, -1) end)
  map("sync", function() M._review.sync(buf) end)
  map("submit", function() M._review.submit(buf) end)
end

---@class DiffReviewBranchDiffOptions
---@field cwd? string
---@field file? string limit the diff to one repo-relative file

--- Open a read-only diff of the working tree against a branch or revision
--- (":GitBranchDiff <branch>", ":GitBranchDiffFile <file> <branch>"): the
--- diff part of the status view without staging, commit, or remote actions.
---@param branch string
---@param opts? DiffReviewBranchDiffOptions
function M.open_branch_diff(branch, opts)
  opts = opts or {}
  branch = vim.trim(branch or "")
  local file = opts.file and vim.trim(opts.file) or nil
  if file == "" then file = nil end
  if branch == "" then
    notify_error("GitBranchDiff requires a branch or revision", "GitBranchDiff")
    return
  end
  setup_bg_highlights()

  local function open_for_root(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "GitBranchDiff")
      return
    end

    local buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "GitStatus"
    local name = "GitBranchDiff"
    if not pcall(vim.api.nvim_buf_set_name, buf, name) then
      pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
    end

    local state = {
      view_kind = "diff",
      buf = buf,
      cwd = root,
      diff_branch = branch,
      diff_file = file,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      boundary_lines = {},
      sections = nil,
      fancy_rows = {},
    }
    M._status = state
    attach_status_state(buf, state)
    setup_status_keymaps(buf)

    local win = vim.api.nvim_get_current_win()
    local ok, set_err = pcall(vim.api.nvim_win_set_buf, win, buf)
    if not ok then
      notify_error("GitBranchDiff open failed: " .. tostring(set_err), "GitBranchDiff")
      return
    end
    M._apply_status_window_options(win, state)
    vim.wo[win].foldcolumn = "0"

    M._branch_diff.render(branch, root, buf, nil, file)
    M._branch_diff.load(branch, root, buf, file)
  end

  if opts.cwd then
    open_for_root(opts.cwd, nil)
    return
  end
  git_root_async(open_for_root)
end

---@class DiffReviewCompactPreviewOptions
---@field cwd? string
---@field staged? boolean

---@param opts? DiffReviewCompactPreviewOptions
function M.open_compact_preview(opts)
  opts = opts or {}
  local function open_for_root(root, err)
    local cwd = root
    if not cwd then
      notify_error(err or "Not a git repository", "GitDiffCompactPreview")
      return
    end

    local command = { "git", "-C", cwd, "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff" }
    if opts.staged then command[#command + 1] = "--cached" end
    systemlist_async(command, function(output, code, stderr)
      if code ~= 0 then
        local message = vim.trim(stderr or "")
        notify_error(message ~= "" and message or "Unable to read git diff", "GitDiffCompactPreview")
        return
      end

      local compacted, was_compacted, metrics = require("git.diff").compact_lines(output or {})
      local lines = compacted == "" and { "No diff." } or vim.split(compacted, "\n", { plain = true })
      local buf = vim.api.nvim_create_buf(true, true)
      vim.bo[buf].bufhidden = "wipe"
      vim.bo[buf].buftype = "nofile"
      vim.bo[buf].swapfile = false
      vim.bo[buf].filetype = "diff"
      vim.bo[buf].modifiable = true
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      vim.bo[buf].modifiable = false

      local name = ("GitDiffCompactPreview://%s/%s"):format(
        opts.staged and "staged" or "unstaged",
        was_compacted and "compact" or "full"
      )
      pcall(vim.api.nvim_buf_set_name, buf, name)

      local win = vim.api.nvim_get_current_win()
      local ok, set_err = pcall(vim.api.nvim_win_set_buf, win, buf)
      if not ok then
        notify_error("GitDiffCompactPreview open failed: " .. tostring(set_err), "GitDiffCompactPreview")
        return
      end
      M._apply_status_window_options(win, nil)
      vim.wo[win].foldcolumn = "0"
      vim.b[buf].git_diff_compact_metrics = metrics
      vim.b[buf].git_diff_compacted = was_compacted
    end)
  end

  if opts.cwd then
    open_for_root(opts.cwd)
  else
    git_root_async(open_for_root)
  end
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
    vim.bo[buf].filetype = "GitStatus"
    pcall(vim.api.nvim_buf_set_name, buf, (M.config or config.options).status_buffer_name)
    M._main_status = {
      view_kind = "status",
      buf = buf,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      boundary_lines = {},
      sections = nil,
      fancy_rows = {},
    }
    M._status = M._main_status
    attach_status_state(buf, M._main_status)
    setup_status_keymaps(buf)
  else
    M._main_status = M._status_states and M._status_states[buf] or M._main_status or M._status
    M._status = M._main_status
  end

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview open failed: " .. tostring(err))
    return
  end
  M._apply_status_window_options(win, state)
  vim.wo[win].foldcolumn = "0"
  render_status_or_notify(buf)
end

return M
