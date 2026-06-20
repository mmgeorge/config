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
---@field raw_hunks? DiffReviewHunk[]

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
---@field committed_at? string
---@field authored_at? string
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
---@field issue_comments? table[]
---@field upstream? string
---@field file_key_prefix? string
---@field file_entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@field hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"

---@class DiffReviewStatusEntry
---@field id? string
---@field kind "section"|"file"|"hunk"|"context_line"|"commit"|"commit_file"|"commit_hunk"|"pr_file"|"pr_hunk"|"pr_comment"|"pr_comment_reply"|"pr_review"|"pr_review_file"|"pr_review_hunk"|"review_comment"|"pr"|"about"|"pr_check"|"pr_head_section"|"pr_head_line"
---@field section? DiffReviewStatusSection
---@field file? DiffReviewStatusFile
---@field hunk? DiffReviewHunk
---@field commit? DiffReviewStatusCommit
---@field pr_review? DiffReviewGhSubmittedReview
---@field pr_comment? DiffReviewGhPendingReviewComment|DiffReviewGhIssueComment
---@field pr_comment_body? boolean
---@field pr_comment_body_index? integer
---@field pr_comment_reply? DiffReviewGhReviewCommentReply
---@field review_comment? table
---@field review_reply? table
---@field diff_line? table
---@field pr? DiffReviewGhPR
---@field pr_check? DiffReviewGhPRCheck
---@field about? DiffReviewAICommitState
---@field fold_target_id? string
---@field diff_lines? table[]
---@field inline_jump_spans? table[]

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
---@field parent_id? string
---@field default_folded? boolean

---@alias DiffReviewStatusSectionName "unstaged"|"staged"|"untracked"|"unmerged"|"recent"|"pr_commits"

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
---@field ancestor_boundaries DiffReviewHunkBoundaryContext[]
---@field path_start_rows integer[] 1-based rows from the target node path
---@field path_end_rows integer[] 1-based rows from the target node path
---@field sibling_before_rows integer[] 1-based same-parent rows before the target row
---@field sibling_after_rows integer[] 1-based same-parent rows after the target row

---@class DiffReviewHunkBoundaryContext
---@field key string
---@field row integer 1-based row
---@field text string
---@field segments DiffReviewHighlightSegment[]
---@field end_row integer 1-based row
---@field end_text string
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
---@field _diff_uses_file_syntax fun(hunk_staged?: boolean[], opts?: table): boolean
---@field _prewarm_diff_syntax fun(filename: string, diff_text: string, hunk_staged?: boolean[], callback_key: string, on_update?: fun(syntax?: DiffReviewTreeSitterSyntax), opts?: table)
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
M._pending_review_icon = "◷"
M._codeowner_review_icon = "⚠"
M._milestone_icon = "◆"

---@param cwd string
---@param extra_args? string[]
---@return string[]
function M._git_diff_command(cwd, extra_args)
  local command = {
    "git", "-C", cwd,
    "-c", "core.quotepath=false",
    "diff", "--no-color", "--no-ext-diff", "--unified=0",
  }
  for _, arg in ipairs(extra_args or {}) do
    command[#command + 1] = arg
  end
  return command
end

---@param cwd string
---@param commit_oid string
---@return string[]
function M._git_show_diff_command(cwd, commit_oid)
  return {
    "git", "-C", cwd,
    "show", "--format=", "--no-color", "--no-ext-diff", "--unified=0", commit_oid,
  }
end
M._pr_overview = {}
M._datetime = {}
M._comment_rows = require("github.comment_rows")

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
M._intraline_diff = require("diff_review.intraline_diff")

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
---@return string[]?
function M._file_source_lines(filename)
  if file_contains_nul(filename) then return nil end
  local loaded = loaded_file_buffer(filename)
  if loaded then
    if vim.bo[loaded].binary then return nil end
    return vim.api.nvim_buf_get_lines(loaded, 0, -1, false)
  end
  local read_ok, lines = pcall(vim.fn.readfile, filename)
  if not read_ok or type(lines) ~= "table" then return nil end
  if lines_contain_nul(lines) then return nil end
  return lines
end

---@param filename string
---@return integer?
local function treesitter_source_buffer(filename)
  if file_contains_nul(filename) then return nil end
  local loaded = loaded_file_buffer(filename)
  if loaded then
    if vim.bo[loaded].binary then return nil end
    return loaded
  end

  M._ts_source_bufs = M._ts_source_bufs or {}
  local cached = M._ts_source_bufs[filename]
  if cached and vim.api.nvim_buf_is_valid(cached) then return cached end

  local lines = M._file_source_lines(filename)
  if not lines then return nil end

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
  run_git_async({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
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
  run_git_async({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
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
  local result = run_git_sync_for_test_backend({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
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
  local result = run_git_sync_for_test_backend({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
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

---@param file DiffReviewStatusFile
---@return string label
---@return string hl_group
function M._status_file_change_label(file)
  local status = type(file.git_status) == "string" and file.git_status or file.status
  status = type(status) == "string" and status:lower() or ""
  if file.untracked or status:sub(1, 1) == "a" or status == "added" or status == "new" then
    return "New", "DiffReviewStatusFileNew"
  end
  if status:sub(1, 1) == "d" or status == "deleted" or status == "removed" then
    return "Removed", "DiffReviewStatusFileDeleted"
  end
  return "Modified", "DiffReviewStatusFileModified"
end

---@class DiffReviewStatusFileStatSegment
---@field start_col integer
---@field end_col integer
---@field hl_group string

---@param file DiffReviewStatusFile
---@return string
---@return DiffReviewStatusFileStatSegment[]
function M._status_file_stat_text_and_segments(file)
  if file.untracked then
    return "new", {
      { start_col = 0, end_col = 3, hl_group = "Comment" },
    }
  end

  local added_text = ("+%d"):format(file.added or 0)
  local removed_text = ("-%d"):format(file.removed or 0)
  return added_text .. " " .. removed_text, {
    { start_col = 0, end_col = #added_text, hl_group = "DiffReviewAddRange" },
    { start_col = #added_text + 1, end_col = #added_text + 1 + #removed_text, hl_group = "DiffReviewDeleteRange" },
  }
end

-- Parse unified diff output into structured file/hunk data
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
  local args = M._git_diff_command(cwd, staged and { "--cached" } or nil)
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

  -- Sort by start row descending (innermost scope first). If two captures start
  -- on the same row, prefer the shorter range as the inner scope.
  table.sort(scopes, function(a, b)
    if a.sr ~= b.sr then return a.sr > b.sr end
    return a.er < b.er
  end)

  -- Limit to 3 levels max
  local max_depth = math.min(#scopes, 3)

  local function scope_name(scope)
    if scope.name ~= nil then return scope.name end
    for cid, cnode in query:iter_captures(scope.node, buf) do
      if query.captures[cid] == "scope.name" then
        local name_text = vim.treesitter.get_node_text(cnode, buf)
        if name_text and name_text ~= "" then
          scope.name = name_text
          return scope.name
        end
        break
      end
    end
    scope.name = false
    return nil
  end

  local function scope_key(scope)
    return ("%s:%d:%d"):format(scope_name(scope) or scope.node:type(), scope.sr, scope.er)
  end

  -- Collect names from outermost to innermost
  local names = {}
  for i = max_depth, 1, -1 do
    local scope = scopes[i]
    local name_text = scope_name(scope)
    if name_text then names[#names + 1] = name_text end
  end

  if #names == 0 then return nil end
  local selected_scope = scopes[1]
  local lines = vim.api.nvim_buf_get_lines(buf, selected_scope.sr, selected_scope.er + 1, false)
  local start_text = lines[1] or ""
  local end_text = lines[#lines] or start_text

  local function same_node(left, right)
    if not left or not right then return false end
    local left_start_row, left_start_col, left_end_row, left_end_col = left:range()
    local right_start_row, right_start_col, right_end_row, right_end_col = right:range()
    return left:type() == right:type()
      and left_start_row == right_start_row
      and left_start_col == right_start_col
      and left_end_row == right_end_row
      and left_end_col == right_end_col
  end

  local function sorted_row_set(row_set)
    local rows = {}
    for row in pairs(row_set) do
      rows[#rows + 1] = row
    end
    table.sort(rows)
    return rows
  end

  local root = trees[1]:root()
  local function row_node_for_context(row)
    local row_text = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
    local row_node = nil
    local node_ok, node = pcall(function()
      return root:named_descendant_for_range(row, 0, row, #row_text)
    end)
    if node_ok then row_node = node end
    if not row_node then
      node_ok, node = pcall(function()
        return root:descendant_for_range(row, 0, row, #row_text)
      end)
      if node_ok then row_node = node end
    end
    while row_node do
      local parent = row_node:parent()
      if not parent then break end
      local parent_start_row, _, parent_end_row, _ = parent:range()
      if parent_start_row == row
        and parent_end_row == row
        and parent_start_row >= selected_scope.sr
        and parent_end_row <= selected_scope.er
        and not same_node(parent, selected_scope.node) then
        row_node = parent
      else
        break
      end
    end
    return row_node
  end

  local path_start_rows = {}
  local path_end_rows = {}
  local target_node = row_node_for_context(target)
  local target_row_node = target_node
  while target_node do
    local node_start_row, _, node_end_row, _ = target_node:range()
    if node_start_row < selected_scope.sr or node_end_row > selected_scope.er then break end
    if node_start_row < target then path_start_rows[node_start_row + 1] = true end
    if node_end_row > target then path_end_rows[node_end_row + 1] = true end
    if same_node(target_node, selected_scope.node) then break end
    target_node = target_node:parent()
  end

  local function same_parent_neighbor_rows(direction)
    local rows = {}
    local target_parent = target_row_node and target_row_node:parent() or nil
    if not target_parent then return rows end
    for offset = 1, 3 do
      local row = target + (direction * offset)
      if row < selected_scope.sr or row > selected_scope.er then break end
      local row_text = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
      if vim.trim(row_text) == "" then break end
      local neighbor_node = row_node_for_context(row)
      local neighbor_parent = neighbor_node and neighbor_node:parent() or nil
      if not same_node(neighbor_parent, target_parent) then break end
      rows[row + 1] = true
    end
    return rows
  end

  local ancestor_boundaries = {}
  local ancestor_scope = scopes[2]
  if ancestor_scope and ancestor_scope.sr < selected_scope.sr then
    local ancestor_text = vim.api.nvim_buf_get_lines(buf, ancestor_scope.sr, ancestor_scope.sr + 1, false)[1] or ""
    local ancestor_end_text = vim.api.nvim_buf_get_lines(buf, ancestor_scope.er, ancestor_scope.er + 1, false)[1] or ""
    ancestor_boundaries[#ancestor_boundaries + 1] = {
      key = scope_key(ancestor_scope),
      row = ancestor_scope.sr + 1,
      text = ancestor_text,
      segments = treesitter_line_segments(buf, trees[1], highlight_query, ancestor_scope.sr, ancestor_text),
      end_row = ancestor_scope.er + 1,
      end_text = ancestor_end_text,
      end_segments = treesitter_line_segments(buf, trees[1], highlight_query, ancestor_scope.er, ancestor_end_text),
    }
  end

  return {
    label = table.concat(names, "."),
    start_row = selected_scope.sr,
    end_row = selected_scope.er,
    start_text = start_text,
    end_text = end_text,
    start_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.sr, start_text),
    end_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.er, end_text),
    ancestor_boundaries = ancestor_boundaries,
    path_start_rows = sorted_row_set(path_start_rows),
    path_end_rows = sorted_row_set(path_end_rows),
    sibling_before_rows = sorted_row_set(same_parent_neighbor_rows(-1)),
    sibling_after_rows = sorted_row_set(same_parent_neighbor_rows(1)),
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
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending
local function cached_file_syntax(filename, callback_key, on_update)
  M._ts_syntax_cache = M._ts_syntax_cache or {}
  local cached = M._ts_syntax_cache[filename]
  if cached == false then return nil, false end
  if type(cached) == "table" and not cached.pending then return cached, false end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil, true
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
  return nil, true
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

---@param context DiffReviewHunkTreeSitterContext|string?
---@return DiffReviewHunkBoundaryContext?
function M._hunk_context_ancestor_boundary(context)
  if type(context) ~= "table" or type(context.ancestor_boundaries) ~= "table" then return nil end
  return context.ancestor_boundaries[#context.ancestor_boundaries]
end

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?
function M._hunk_context_ancestor_key(context)
  local boundary = M._hunk_context_ancestor_boundary(context)
  return boundary and boundary.key or nil
end

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean
function M._same_hunk_ancestor_scope(left, right)
  local left_key = M._hunk_context_ancestor_key(left)
  return left_key ~= nil and left_key == M._hunk_context_ancestor_key(right)
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

---@param hunk DiffReviewParsedHunk
---@return fun(parsed_line: DiffReviewParsedHunkLine): boolean
function M._hunk_render_line_filter(hunk)
  local first_changed_position = nil
  local last_changed_position = nil
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      first_changed_position = math.min(first_changed_position or parsed_line.position, parsed_line.position)
      last_changed_position = math.max(last_changed_position or parsed_line.position, parsed_line.position)
    end
  end
  if not first_changed_position or not last_changed_position then
    return function() return true end
  end
  return function(parsed_line)
    if parsed_line.prefix ~= " " then return true end
    return parsed_line.position >= first_changed_position and parsed_line.position <= last_changed_position
  end
end

---@param parsed_lines DiffReviewParsedHunkLine[]
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<string, boolean>
function M._hunk_visible_parsed_source_lines(parsed_lines, include_line)
  local visible = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    if (not include_line or include_line(parsed_line)) and (parsed_line.prefix == " " or parsed_line.prefix == "+" or parsed_line.prefix == "-") then
      visible[parsed_line.code] = true
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
---@param changed_line_hl? string
---@return table[]
local function hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl)
  gutter = gutter or default_hunk_gutter_spec()
  local old_text = old_line and ("%" .. tostring(gutter.old_width) .. "d"):format(old_line) or string.rep(" ", gutter.old_width)
  local new_text = new_line and ("%" .. tostring(gutter.new_width) .. "d"):format(new_line) or string.rep(" ", gutter.new_width)
  local old_hl = line_hl
  if old_line then
    if sign == "-" then
      old_hl = changed_line_hl or sign_hl or "DiffReviewDeleteLineNr"
    elseif sign == "~" then
      old_hl = changed_line_hl or sign_hl or "DiffReviewModifyLineNr"
    else
      old_hl = "DiffReviewContextLineNr"
    end
  end
  local new_hl = line_hl
  if new_line then
    if sign == "+" then
      new_hl = changed_line_hl or sign_hl or "DiffReviewAddLineNr"
    elseif sign == "~" then
      new_hl = changed_line_hl or sign_hl or "DiffReviewModifyLineNr"
    else
      new_hl = "DiffReviewContextLineNr"
    end
  end
  local chunks = {}
  chunks[#chunks + 1] = { old_text, old_hl }
  chunks[#chunks + 1] = { "  ", line_hl }
  chunks[#chunks + 1] = { new_text, new_hl }
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
---@param changed_line_hl? string
local function hunk_add_gutter(row, gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl)
  -- Inline virtual text, not buffer text: visual selection, yank, and search
  -- then operate on the code content only. The default hl_mode "replace"
  -- keeps the Visual highlight from bleeding into the gutter; the row's
  -- line background is carried explicitly on every chunk via line_hl.
  row[#row + 1] = {
    col = 0,
    virt_text = hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl),
    virt_text_pos = "inline",
  }
end

---@param text string
---@param segments? DiffReviewHighlightSegment[]
---@param line_number? integer
---@param gutter? DiffReviewGutterSpec
---@param file? string
---@param old_line? integer
---@param new_line? integer
---@return table
local function hunk_boundary_row(text, segments, line_number, gutter, file, old_line, new_line)
  local row = { diff_review_boundary = true }
  gutter = gutter or default_hunk_gutter_spec()
  old_line = old_line or line_number
  new_line = new_line or line_number
  if file and line_number then
    row[#row + 1] = {
      "",
      nil,
      meta = {
        diff = M._hunk_diff_line_meta({
          prefix = " ",
          old_line = old_line,
          new_line = new_line,
          code = text,
        }, file),
      },
    }
  end
  hunk_add_gutter(row, gutter, old_line, new_line, " ", nil)
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
  local soft_wrap = state ~= nil and (state.view_kind == "review" or state.view_kind == "pr")
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
  M._diff_line_content_lengths = {}
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
M._diff_gutter_visual_ns = vim.api.nvim_create_namespace("diff_review_visual_gutter")
M._empty_diff_rows = {}
M._diff_line_content_lengths = {}

function M._diff_visual_fill_width(buf)
  local width = vim.o.columns
  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_win_is_valid(win) then
      width = math.max(width, vim.api.nvim_win_get_width(win))
    end
  end
  return math.max(width + 8, 160)
end

function M._diff_pad_highlighted_line(line_text, buf)
  local content_length = #line_text
  local fill_width = M._diff_visual_fill_width(buf)
  local display_width = vim.fn.strdisplaywidth(line_text)
  if display_width < fill_width then
    line_text = line_text .. string.rep(" ", fill_width - display_width)
  end
  return line_text, content_length
end

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
---@field content_length integer real buffer text length before visual highlight padding
---@field virt_text table[]

---@param buf integer
---@param row integer 1-based
---@param namespace integer
---@return DiffReviewGutterCursorBounds?
function M._diff_gutter_cursor_bounds(buf, row, namespace)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  if line == nil then return nil end
  local content_lengths = M._diff_line_content_lengths and M._diff_line_content_lengths[buf] or nil
  local content_length = content_lengths and content_lengths[row] or #line
  local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local col = mark[3] or 0
    local details = mark[4] or {}
    if details.virt_text and details.virt_text_pos == "inline" and col <= content_length then
      local width = M._inline_virtual_text_width(details.virt_text)
      if width > 0 then
        return {
          line = line,
          gutter_col = col,
          gutter_width = width,
          content_length = content_length,
          virt_text = details.virt_text,
        }
      end
    end
  end
  return nil
end

---@param chunks table[]?
---@return table[]
function M._diff_gutter_visual_chunks(chunks)
  local visual_chunks = {}
  for _, chunk in ipairs(chunks or {}) do
    visual_chunks[#visual_chunks + 1] = { chunk[1] or "", "Visual" }
  end
  return visual_chunks
end

---@param chunks table[]?
---@return string
function M._diff_gutter_text(chunks)
  local parts = {}
  for _, chunk in ipairs(chunks or {}) do
    parts[#parts + 1] = chunk[1] or ""
  end
  return table.concat(parts)
end

---@param mode? string
---@return boolean
function M._is_visual_mode(mode)
  mode = mode or vim.api.nvim_get_mode().mode
  return mode == "v" or mode == "V" or mode:byte() == 22
end

---@param buf integer
function M._clear_diff_gutter_visual_overlay(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  pcall(vim.api.nvim_buf_clear_namespace, buf, M._diff_gutter_visual_ns, 0, -1)
end

---@param buf integer
function M._clear_diff_gutter_visual_line(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if M._diff_gutter_visual_line_yank_maps and M._diff_gutter_visual_line_yank_maps[buf] then
    pcall(vim.keymap.del, "x", "<Space>l", { buffer = buf })
    M._diff_gutter_visual_line_yank_maps[buf] = nil
  end
  M._clear_diff_gutter_visual_overlay(buf)
end

---@param buf integer
function M._install_diff_gutter_visual_line_yank_maps(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  M._diff_gutter_visual_line_yank_maps = M._diff_gutter_visual_line_yank_maps or {}
  if M._diff_gutter_visual_line_yank_maps[buf] then return end
  M._diff_gutter_visual_line_yank_maps[buf] = true
  vim.keymap.set("x", "<Space>l", function()
    if M._yank_diff_gutter_visual_line(buf, "+") then return end
    M._clear_diff_gutter_visual_line(buf)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Space>l", true, false, true), "m", false)
  end, { buffer = buf, nowait = true, silent = true, desc = "Yank selection to clipboard" })
end

---@param buf integer
---@return boolean
function M._diff_gutter_visual_line_active(buf)
  local selections = M._diff_gutter_visual_line_selections
  if not (selections and selections[buf]) then return false end
  if selections[buf] == "starting" then return true end
  if M._is_visual_mode() then return true end
  selections[buf] = nil
  M._clear_diff_gutter_visual_line(buf)
  return false
end

---@param buf integer
---@return integer
function M._diff_gutter_namespace(buf)
  local status = M._status_states and M._status_states[buf] or nil
  return status and M._status_ns or M._ns
end

---@param buf integer
function M._refresh_diff_gutter_visual_line(buf)
  if not M._diff_gutter_visual_line_active(buf) then return end
  M._clear_diff_gutter_visual_overlay(buf)
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  local start_pos = vim.fn.getpos("v")
  local start_row = start_pos and start_pos[2] or cursor_row
  if start_row == 0 then start_row = cursor_row end
  local first_row = math.min(start_row, cursor_row)
  local last_row = math.max(start_row, cursor_row)
  local namespace = M._diff_gutter_namespace(buf)
  for row = first_row, last_row do
    local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
    if bounds and bounds.virt_text then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._diff_gutter_visual_ns, row - 1, 0, {
        virt_text = M._diff_gutter_visual_chunks(bounds.virt_text),
        virt_text_pos = "overlay",
        virt_text_win_col = bounds.gutter_col,
        hl_mode = "replace",
        priority = 250,
      })
    end
  end
end

---@param buf integer
function M._start_diff_gutter_visual_line(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  M._diff_gutter_visual_line_selections = M._diff_gutter_visual_line_selections or {}
  M._diff_gutter_visual_line_selections[buf] = "starting"
  M._install_diff_gutter_visual_line_yank_maps(buf)
  local row = vim.api.nvim_win_get_cursor(0)[1]
  vim.fn.setpos(".", { 0, row, 1, 0 })
  vim.cmd("normal! V")
  M._diff_gutter_visual_line_selections[buf] = true
  M._refresh_diff_gutter_visual_line(buf)
end

---@param buf integer
---@return string[]
function M._diff_gutter_visual_line_text(buf)
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  local start_pos = vim.fn.getpos("v")
  local start_row = start_pos and start_pos[2] or cursor_row
  if start_row == 0 then start_row = cursor_row end
  local first_row = math.min(start_row, cursor_row)
  local last_row = math.max(start_row, cursor_row)
  local namespace = M._diff_gutter_namespace(buf)
  local lines = {}
  for row = first_row, last_row do
    local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
    local bounds = M._diff_gutter_cursor_bounds(buf, row, namespace)
    if bounds and bounds.virt_text then
      lines[#lines + 1] = M._diff_gutter_text(bounds.virt_text) .. line:sub(1, bounds.content_length)
    else
      lines[#lines + 1] = line
    end
  end
  return lines
end

---@param buf integer
---@param register? string
---@return boolean handled
function M._yank_diff_gutter_visual_line(buf, register)
  if not M._diff_gutter_visual_line_active(buf) then return false end
  local lines = M._diff_gutter_visual_line_text(buf)
  register = register or vim.v.register
  if register == nil or register == "" then register = '"' end
  vim.fn.setreg(register, lines, "V")
  if register == '"' and vim.o.clipboard:find("unnamedplus", 1, true) then
    pcall(vim.fn.setreg, "+", lines, "V")
  end
  if register == '"' and vim.o.clipboard:find("unnamed", 1, true) then
    pcall(vim.fn.setreg, "*", lines, "V")
  end
  M._diff_gutter_visual_line_selections[buf] = nil
  M._clear_diff_gutter_visual_line(buf)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  return true
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
  local line_length = bounds.content_length or #bounds.line
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
  local content_lengths = M._diff_line_content_lengths and M._diff_line_content_lengths[buf] or nil
  local line_length = content_lengths and content_lengths[row] or #line
  local pos = vim.fn.getcurpos()
  local current_col = math.max((pos[3] or 1) - 1, 0)
  local current_coladd = pos[4] or 0
  local target_col = math.min(current_col, line_length)
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
  if M._diff_gutter_visual_line_active(buf) then
    M._refresh_diff_gutter_visual_line(buf)
    M._cursor_normalizing[buf] = nil
    return vim.api.nvim_win_get_cursor(0)[1]
  end
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
---@param opts? { preserve_trailing_blank?: boolean }
---@return DiffReviewParsedHunk
local function parse_hunk_body(hunk, opts)
  opts = opts or {}
  if not opts.preserve_trailing_blank then
    while #hunk.body > 0 and hunk.body[#hunk.body]:match("^%s*$") do
      hunk.body[#hunk.body] = nil
    end
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

---@param hunk DiffReviewParsedHunk
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return integer
function M._hunk_after_current_line(hunk, include_line)
  local last_new_line = nil
  local saw_removed_line = false
  for _, parsed_line in ipairs(hunk.lines) do
    if not include_line or include_line(parsed_line) then
      if parsed_line.new_line then last_new_line = parsed_line.new_line end
      if parsed_line.prefix == "-" then saw_removed_line = true end
    end
  end
  if last_new_line then return last_new_line + 1 end
  if saw_removed_line then return hunk.new_start end
  return hunk.new_start
end

---@param hunk DiffReviewParsedHunk
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<integer, boolean>
function M._hunk_current_line_set(hunk, include_line)
  local lines = {}
  for _, parsed_line in ipairs(hunk.lines) do
    if parsed_line.new_line and (not include_line or include_line(parsed_line)) then lines[parsed_line.new_line] = true end
  end
  return lines
end

---@param item table
---@return DiffReviewParsedHunkLine
function M._hunk_render_item_line(item)
  return item.line or item.display_line
end

---@param item table
---@return DiffReviewParsedHunkLine[]
function M._hunk_render_item_backing_lines(item)
  if item.kind == "replacement" then return item.diff_lines or { item.display_line } end
  return { item.line }
end

---@param item table
---@return boolean
function M._hunk_render_item_changed(item)
  if item.kind == "replacement" then return true end
  local parsed_line = M._hunk_render_item_line(item)
  return parsed_line.prefix == "+" or parsed_line.prefix == "-"
end

---@param hunk DiffReviewParsedHunk
---@return table<integer, integer>
function M._hunk_current_line_by_position(hunk)
  local by_position = {}
  local current_line = hunk.new_start
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == " " then
      if parsed_line.position and parsed_line.new_line then by_position[parsed_line.position] = parsed_line.new_line end
      if parsed_line.new_line then current_line = parsed_line.new_line + 1 end
    elseif parsed_line.prefix == "+" then
      if parsed_line.position and parsed_line.new_line then by_position[parsed_line.position] = parsed_line.new_line end
      if parsed_line.new_line then current_line = parsed_line.new_line + 1 end
    elseif parsed_line.prefix == "-" then
      if parsed_line.position then by_position[parsed_line.position] = current_line end
    end
  end
  return by_position
end

---@param parsed_line DiffReviewParsedHunkLine?
---@param line_by_position table<integer, integer>
---@return integer?
function M._hunk_parsed_line_current_line(parsed_line, line_by_position)
  if not parsed_line then return nil end
  return parsed_line.new_line or (parsed_line.position and line_by_position[parsed_line.position]) or parsed_line.old_line
end

---@param item table
---@param line_by_position table<integer, integer>
---@return integer?
function M._hunk_render_item_context_line(item, line_by_position)
  if item.kind == "replacement" then
    for _, parsed_line in ipairs(item.new_lines or {}) do
      if parsed_line.new_line then return parsed_line.new_line end
    end
  end
  return M._hunk_parsed_line_current_line(M._hunk_render_item_line(item), line_by_position)
end

---@param item table
---@param line_by_position table<integer, integer>
---@return integer?
---@return integer?
function M._hunk_render_item_changed_line_range(item, line_by_position)
  local first_line = nil
  local last_line = nil
  for _, parsed_line in ipairs(M._hunk_render_item_backing_lines(item)) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      local current_line = M._hunk_parsed_line_current_line(parsed_line, line_by_position)
      if current_line then
        first_line = math.min(first_line or current_line, current_line)
        last_line = math.max(last_line or current_line, current_line)
      end
    end
  end
  return first_line, last_line
end

---@param context DiffReviewHunkTreeSitterContext|string?
---@param line integer?
---@return boolean
function M._hunk_context_contains_line(context, line)
  if type(context) ~= "table" or not line then return true end
  return line >= context.start_row + 1 and line <= context.end_row + 1
end

---@param hunk DiffReviewParsedHunk
---@param line_by_position? table<integer, integer>
---@return table<integer, boolean>
function M._hunk_changed_current_line_set(hunk, line_by_position)
  line_by_position = line_by_position or M._hunk_current_line_by_position(hunk)
  local lines = {}
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      local current_line = M._hunk_parsed_line_current_line(parsed_line, line_by_position)
      if current_line then lines[current_line] = true end
    end
  end
  return lines
end

---@class DiffReviewHunkChangeRegion
---@field first_item integer
---@field last_item integer
---@field context_line integer?
---@field context DiffReviewHunkTreeSitterContext|string?
---@field context_key string?
---@field changed_line integer?
---@field after_line integer?
---@field added integer
---@field removed integer

---@param item table
---@return integer added
---@return integer removed
function M._hunk_render_item_stats(item)
  local added = 0
  local removed = 0
  for _, parsed_line in ipairs(M._hunk_render_item_backing_lines(item)) do
    if parsed_line.prefix == "+" then
      added = added + 1
    elseif parsed_line.prefix == "-" then
      removed = removed + 1
    end
  end
  return added, removed
end

---@param render_items table[]
---@param line_by_position table<integer, integer>
---@param context_for_line fun(line: integer): DiffReviewHunkTreeSitterContext|string?
---@return DiffReviewHunkChangeRegion[]
function M._hunk_change_regions(render_items, line_by_position, context_for_line)
  local regions = {}
  local current_region = nil
  for item_index, item in ipairs(render_items or {}) do
    if M._hunk_render_item_changed(item) then
      local context_line = M._hunk_render_item_context_line(item, line_by_position)
      local item_context = nil
      if type(current_region and current_region.context) == "table"
        and M._hunk_context_contains_line(current_region.context, context_line) then
        item_context = current_region.context
      else
        item_context = context_line and context_for_line(context_line) or nil
      end
      local context_key = hunk_context_scope_key(item_context)
      local changed_line, last_changed_line = M._hunk_render_item_changed_line_range(item, line_by_position)
      local added, removed = M._hunk_render_item_stats(item)
      local can_merge = current_region ~= nil
        and (
          (
            context_key ~= nil
            and current_region.context_key == context_key
            and M._hunk_context_contains_line(current_region.context, context_line)
          )
          or (
            context_key == nil
            and current_region.context_key == nil
            and type(current_region.context) ~= "table"
            and type(item_context) ~= "table"
          )
        )
      if not can_merge then
        current_region = {
          first_item = item_index,
          last_item = item_index,
          context_line = context_line,
          context = item_context,
          context_key = context_key,
          changed_line = changed_line or context_line,
          after_line = last_changed_line and (last_changed_line + 1) or context_line,
          added = added,
          removed = removed,
        }
        regions[#regions + 1] = current_region
      else
        current_region.last_item = item_index
        if changed_line then current_region.changed_line = math.min(current_region.changed_line or changed_line, changed_line) end
        if last_changed_line then current_region.after_line = math.max(current_region.after_line or last_changed_line + 1, last_changed_line + 1) end
        current_region.added = (current_region.added or 0) + added
        current_region.removed = (current_region.removed or 0) + removed
      end
    end
  end
  if #regions == 0 and #render_items > 0 then
    regions[1] = {
      first_item = 1,
      last_item = #render_items,
      added = 0,
      removed = 0,
    }
  end
  return regions
end

---@param region DiffReviewHunkChangeRegion
---@param fallback_hunk? DiffReviewParsedHunk
---@return table[]
function M._hunk_virtual_header_parts(region, fallback_hunk)
  local added = region.added
  local removed = region.removed
  if added == nil and fallback_hunk then added = fallback_hunk.added end
  if removed == nil and fallback_hunk then removed = fallback_hunk.removed end
  return {
    { "@@ ", "DiffReviewHunkHeader" },
    { ("+%d"):format(added or 0), "DiffReviewAddRange" },
    { " ", "DiffReviewHunkHeader" },
    { ("-%d"):format(removed or 0), "DiffReviewDeleteRange" },
  }
end

---@param hunk DiffReviewHunk?
---@return integer?
function M._status_hunk_context_line(hunk)
  if not hunk then return nil end
  local diff = tostring(hunk.diff or "")
  local blocks = parse_unified_diff(diff)
  local parsed_hunk = blocks[1] and blocks[1].hunks and blocks[1].hunks[1] or nil
  if not parsed_hunk then return hunk.pos end
  return hunk_first_changed_current_line(parse_hunk_body(parsed_hunk))
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

---@param parsed_line DiffReviewParsedHunkLine
---@param previous_visible_changed boolean
---@param context DiffReviewHunkTreeSitterContext|string?
---@param visible_in_hunk? boolean
---@return boolean
function M._hunk_hidden_closing_boundary_after_change(parsed_line, previous_visible_changed, context, visible_in_hunk)
  if not previous_visible_changed then return false end
  if parsed_line.prefix ~= " " or type(context) ~= "table" then return false end
  if visible_in_hunk and hunk_line_visible_in_context_scope(parsed_line, context) then return false end
  if not parsed_line.new_line then return false end
  return parsed_line.code:match("^%s*[})%]]+[,;]?%s*$") ~= nil
end

---@alias DiffReviewDiffSyntaxSide "old"|"new"

---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@return string[]
local function diff_syntax_source_lines(diff_text, side)
  local lines = {}
  for _, block in ipairs(parse_unified_diff(diff_text)) do
    for _, hunk in ipairs(block.hunks) do
      local parsed_hunk = parse_hunk_body(hunk)
      for _, parsed_line in ipairs(parsed_hunk.lines) do
        if parsed_line.prefix == " "
          or (side == "old" and parsed_line.prefix == "-")
          or (side == "new" and parsed_line.prefix == "+") then
          lines[#lines + 1] = parsed_line.code
        end
      end
    end
  end
  return lines
end

---@param filename string
---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending
local function cached_diff_syntax(filename, diff_text, side, callback_key, on_update)
  M._ts_diff_syntax_cache = M._ts_diff_syntax_cache or {}
  local cache_key = ("%s:%s:%s"):format(filename, side, vim.fn.sha256(diff_text or ""))
  local cached = M._ts_diff_syntax_cache[cache_key]
  if cached == false then return nil, false end
  if type(cached) == "table" and not cached.pending then return cached, false end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil, true
  end

  local lines = diff_syntax_source_lines(diff_text, side)
  if #lines == 0 then return nil, false end
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

---@param filename string
---@param diff_text string
---@return string[]?
function M._old_file_syntax_source_lines(filename, diff_text)
  local lines = M._file_source_lines(filename)
  if not lines then return nil end

  local hunks = {}
  for _, block in ipairs(parse_unified_diff(diff_text)) do
    for _, hunk in ipairs(block.hunks) do
      hunks[#hunks + 1] = parse_hunk_body(hunk, { preserve_trailing_blank = true })
    end
  end
  if #hunks == 0 then return nil end

  local old_lines = vim.deepcopy(lines)
  for hunk_index = #hunks, 1, -1 do
    local hunk = hunks[hunk_index]
    local replacement = {}
    for _, parsed_line in ipairs(hunk.lines) do
      if parsed_line.prefix == " " or parsed_line.prefix == "-" then
        replacement[#replacement + 1] = parsed_line.code
      end
    end

    local start_index = math.max(tonumber(hunk.new_start) or 1, 1)
    local remove_count = math.max(tonumber(hunk.new_count) or 0, 0)
    if start_index > #old_lines + 1 then return nil end
    if remove_count > 0 and (start_index + remove_count - 1) > #old_lines then return nil end
    for _ = 1, remove_count do
      if start_index <= #old_lines then
        table.remove(old_lines, start_index)
      end
    end
    for replacement_index = #replacement, 1, -1 do
      table.insert(old_lines, start_index, replacement[replacement_index])
    end
  end

  return old_lines
end

---@param filename string
---@param diff_text string
---@return boolean
function M._diff_new_side_matches_file(filename, diff_text)
  local lines = M._file_source_lines(filename)
  if not lines then return false end

  for _, block in ipairs(parse_unified_diff(diff_text)) do
    for _, hunk in ipairs(block.hunks) do
      local parsed_hunk = parse_hunk_body(hunk, { preserve_trailing_blank = true })
      local expected = {}
      for _, parsed_line in ipairs(parsed_hunk.lines) do
        if parsed_line.prefix == " " or parsed_line.prefix == "+" then
          expected[#expected + 1] = parsed_line.code
        end
      end
      local start_index = math.max(tonumber(parsed_hunk.new_start) or 1, 1)
      if start_index > #lines + 1 then return false end
      if #expected > 0 and (start_index + #expected - 1) > #lines then return false end
      for offset, expected_line in ipairs(expected) do
        if lines[start_index + offset - 1] ~= expected_line then return false end
      end
    end
  end

  return true
end

---@param filename string
---@param diff_text string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending
function M._cached_old_file_syntax(filename, diff_text, callback_key, on_update)
  M._ts_diff_syntax_cache = M._ts_diff_syntax_cache or {}
  local cache_key = ("%s:old-file:%s"):format(filename, vim.fn.sha256(diff_text or ""))
  local cached = M._ts_diff_syntax_cache[cache_key]
  if cached == false then return nil, false end
  if type(cached) == "table" and not cached.pending then return cached, false end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil, true
  end

  local lines = M._old_file_syntax_source_lines(filename, diff_text)
  if not lines or #lines == 0 then return nil, false end
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
      if not ok then notify_error("Tree-sitter old file syntax update failed: " .. tostring(err)) end
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

---@param hunk_staged? boolean[]
---@param opts? table
---@return boolean
function M._diff_uses_file_syntax(hunk_staged, opts)
  opts = opts or {}
  if opts.syntax_source == "file" then return true end
  if opts.syntax_source == "diff" then return false end
  for _, staged in ipairs(hunk_staged or {}) do
    if staged then return false end
  end
  return true
end

---@param filename string
---@param diff_text string
---@param hunk_staged? boolean[]
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? table
function M._prewarm_diff_syntax(filename, diff_text, hunk_staged, callback_key, on_update, opts)
  opts = opts or {}
  local syntax_diff_text = opts.syntax_diff_text or diff_text
  if M._diff_uses_file_syntax(hunk_staged, opts) and M._diff_new_side_matches_file(filename, syntax_diff_text) then
    local old_syntax, old_pending = M._cached_old_file_syntax(filename, syntax_diff_text, callback_key .. ":old-file", on_update)
    if not old_syntax and not old_pending then
      cached_diff_syntax(filename, diff_text, "old", callback_key .. ":old", on_update)
    end
    local syntax, pending = cached_file_syntax(filename, callback_key .. ":file", on_update)
    if not syntax and not pending then
      cached_diff_syntax(filename, diff_text, "new", callback_key .. ":new", on_update)
    end
  else
    cached_diff_syntax(filename, diff_text, "old", callback_key .. ":old", on_update)
    cached_diff_syntax(filename, diff_text, "new", callback_key .. ":new", on_update)
  end
end

---@param file DiffReviewStatusFile?
---@return string?
function M._status_file_syntax_diff_text(file)
  if not file then return nil end
  local hunks = status_diff_hunks_for_file(file)
  local diffs = {}
  for _, hunk in ipairs(hunks) do
    if hunk.diff and hunk.diff ~= "" then diffs[#diffs + 1] = hunk.diff end
  end
  if #diffs == 0 then return nil end
  return table.concat(diffs, "\n")
end

---@param file DiffReviewStatusFile?
---@param callback_key_prefix string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? { syntax_source?: "file"|"diff" }
local function prewarm_file_diff_syntax(file, callback_key_prefix, on_update, opts)
  if not (file and file.filename) then return end
  opts = opts or {}
  local combined_diff = opts.syntax_source == "file" and M._status_file_syntax_diff_text(file) or nil
  if type(combined_diff) == "string" and combined_diff ~= "" then
    opts = vim.tbl_extend("force", opts, { syntax_diff_text = combined_diff })
  end
  local hunks = status_diff_hunks_for_file(file)
  for hunk_index, hunk in ipairs(hunks) do
    if hunk.diff and hunk.diff ~= "" then
      local callback_key = ("%s:%s:%d"):format(callback_key_prefix, file.filename, hunk_index)
      M._prewarm_diff_syntax(file.filename, hunk.diff, { hunk.staged }, callback_key, on_update, opts)
    end
  end
end

---@param parsed_line DiffReviewParsedHunkLine
---@param file string
---@return table
function M._hunk_diff_line_meta(parsed_line, file)
  return {
    side = parsed_line.new_line and "right" or "left",
    file = file,
    line = parsed_line.new_line or parsed_line.old_line,
    position = parsed_line.position,
    code = parsed_line.code,
    prefix = parsed_line.prefix,
  }
end

---@param parsed_lines DiffReviewParsedHunkLine[]?
---@param file string
---@return table[]
function M._hunk_diff_lines_meta(parsed_lines, file)
  local diff_lines = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    diff_lines[#diff_lines + 1] = M._hunk_diff_line_meta(parsed_line, file)
  end
  return diff_lines
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

  local row = { diff_review_bg_hl = line_hl }
  local diff_meta = {
    diff = M._hunk_diff_line_meta(parsed_line, file),
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

---@param replacement table
---@param gutter DiffReviewGutterSpec
---@param file string
---@param syntax? DiffReviewTreeSitterSyntax
---@param syntax_row? integer
---@return table
function M._hunk_replacement_row(replacement, gutter, file, syntax, syntax_row)
  local display_line = replacement.display_line
  local backing_lines = replacement.diff_lines or { display_line }
  local sign_hl = "DiffReviewModifyLineNr"
  local line_hl = "DiffReviewModifyBg"
  local row = {
    diff_review_bg_hl = line_hl,
    diff_review_inline_highlights = replacement.inline_spans or {},
  }
  row[#row + 1] = {
    "",
    nil,
    meta = {
      diff = M._hunk_diff_line_meta(display_line, file),
      diff_lines = M._hunk_diff_lines_meta(backing_lines, file),
    },
  }
  local old_line = replacement.old_lines and replacement.old_lines[1] and replacement.old_lines[1].old_line or display_line.old_line
  local new_line = replacement.new_lines and replacement.new_lines[1] and replacement.new_lines[1].new_line or display_line.new_line
  hunk_add_gutter(row, gutter, old_line, new_line, "~", sign_hl, line_hl, sign_hl)
  local segments = nil
  if syntax and syntax_row then
    segments = treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, syntax_row, display_line.code)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { display_line.code }
  end
  return row
end

---@param line_number integer
---@param text string
---@param gutter DiffReviewGutterSpec
---@param file? string
---@param syntax? DiffReviewTreeSitterSyntax
---@param old_line? integer
---@param new_line? integer
---@return table
function M._hunk_context_padding_row(line_number, text, gutter, file, syntax, old_line, new_line)
  local row = { diff_review_context_padding = true }
  old_line = old_line or line_number
  new_line = new_line or line_number
  if file then
    row[#row + 1] = {
      "",
      nil,
      meta = {
        diff = M._hunk_diff_line_meta({
          prefix = " ",
          old_line = old_line,
          new_line = new_line,
          code = text,
        }, file),
      },
    }
  end
  hunk_add_gutter(row, gutter, old_line, new_line, " ", nil)
  local segments = nil
  if syntax then
    segments = treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, line_number - 1, text)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { text }
  end
  return row
end

---@class DiffReviewHunkContextPaddingLine
---@field line_number integer
---@field old_line? integer
---@field new_line? integer
---@field text string

---@param text string?
---@return boolean
function M._hunk_context_padding_line_is_useful(text)
  if type(text) ~= "string" then return false end
  if text:match("^%s*$") then return false end
  if text:match("^%s*[})%]]+[,;]?%s*$") then return false end
  return true
end

---@param text string?
---@return boolean
function M._hunk_context_padding_line_starts_scope(text)
  if type(text) ~= "string" then return false end
  return text:match("^%s*#%[") ~= nil
    or text:match("^%s*pub%s+struct%s+") ~= nil
    or text:match("^%s*struct%s+") ~= nil
    or text:match("^%s*pub%s+enum%s+") ~= nil
    or text:match("^%s*enum%s+") ~= nil
    or text:match("^%s*pub%s+trait%s+") ~= nil
    or text:match("^%s*trait%s+") ~= nil
    or text:match("^%s*impl%s+") ~= nil
    or text:match("^%s*pub%s+fn%s+") ~= nil
    or text:match("^%s*fn%s+") ~= nil
    or text:match("^%s*pub%s+mod%s+") ~= nil
    or text:match("^%s*mod%s+") ~= nil
    or text:match("^%s*use%s+") ~= nil
end

---@param source_lines string[]?
---@param hunk DiffReviewParsedHunk
---@param context DiffReviewHunkTreeSitterContext|string?
---@param side "before"|"after"
---@param occupied_lines? table<integer, boolean>
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@param bounds? { changed_line?: integer, after_line?: integer }
---@return DiffReviewHunkContextPaddingLine[]
function M._hunk_context_padding_lines(source_lines, hunk, context, side, occupied_lines, include_line, bounds)
  if type(source_lines) ~= "table" or #source_lines == 0 then return {} end
  occupied_lines = occupied_lines or M._hunk_current_line_set(hunk)
  bounds = bounds or {}
  local padding_lines = {}
  local padding_limit = M._hunk_context_padding_limit()
  if type(context) == "table" then
    local changed_line = bounds.changed_line or hunk_first_changed_current_line(hunk)
    local after_line = bounds.after_line or M._hunk_after_current_line(hunk, include_line)
    local path_rows = side == "before" and context.path_start_rows or context.path_end_rows
    local sibling_rows = side == "before" and context.sibling_before_rows or context.sibling_after_rows
    local seen_candidates = {}
    local function collect_candidates(rows)
      local candidates = {}
      for _, line_number in ipairs(rows or {}) do
        local is_scope_boundary = line_number == (context.start_row + 1) or line_number == (context.end_row + 1)
        local eligible_before = side == "before" and line_number < changed_line
        local eligible_after = side == "after" and line_number >= after_line
        if not is_scope_boundary
          and not occupied_lines[line_number]
          and not seen_candidates[line_number]
          and (eligible_before or eligible_after) then
          seen_candidates[line_number] = true
          candidates[#candidates + 1] = line_number
        end
      end
      table.sort(candidates, function(left, right)
        if side == "before" then return left > right end
        return left < right
      end)
      return candidates
    end
    local candidates = {}
    for _, line_number in ipairs(collect_candidates(path_rows)) do
      if #candidates >= padding_limit then break end
      candidates[#candidates + 1] = line_number
    end
    for _, line_number in ipairs(collect_candidates(sibling_rows)) do
      if #candidates >= padding_limit then break end
      candidates[#candidates + 1] = line_number
    end
    if #candidates == 0 then
      local scope_start = context.start_row + 1
      local scope_end = context.end_row + 1
      if side == "before" then
        local first_line = math.max(scope_start + 1, changed_line - padding_limit)
        for line_number = changed_line - 1, first_line, -1 do
          local is_scope_boundary = line_number == scope_start or line_number == scope_end
          if is_scope_boundary or not M._hunk_context_padding_line_is_useful(source_lines[line_number]) then break end
          if not occupied_lines[line_number] and not seen_candidates[line_number] then
            seen_candidates[line_number] = true
            candidates[#candidates + 1] = line_number
          end
        end
      else
        local last_line = math.min(scope_end - 1, after_line + padding_limit - 1)
        for line_number = after_line, last_line do
          local is_scope_boundary = line_number == scope_start or line_number == scope_end
          if is_scope_boundary or not M._hunk_context_padding_line_is_useful(source_lines[line_number]) then break end
          if not occupied_lines[line_number] and not seen_candidates[line_number] then
            seen_candidates[line_number] = true
            candidates[#candidates + 1] = line_number
          end
        end
      end
    end
    table.sort(candidates)
    for _, line_number in ipairs(candidates) do
      padding_lines[#padding_lines + 1] = {
        line_number = line_number,
        text = source_lines[line_number] or "",
      }
    end
    return padding_lines
  end

  local first_line = nil
  local last_line = nil
  if side == "before" then
    local changed_line = bounds.changed_line or hunk_first_changed_current_line(hunk)
    first_line = math.max(1, changed_line - padding_limit)
    last_line = changed_line - 1
  else
    first_line = bounds.after_line or M._hunk_after_current_line(hunk, include_line)
    last_line = math.min(#source_lines, first_line + padding_limit - 1)
  end

  for line_number = first_line, last_line do
    if side == "after"
      and line_number > first_line
      and M._hunk_context_padding_line_starts_scope(source_lines[line_number]) then
      break
    end
    if not occupied_lines[line_number] then
      padding_lines[#padding_lines + 1] = {
        line_number = line_number,
        text = source_lines[line_number] or "",
      }
    end
  end
  return padding_lines
end

---@return integer
function M._hunk_context_padding_limit()
  return 3
end

---@param hunks DiffReviewParsedHunk[]
---@param new_line integer?
---@return integer?
function M._hunk_old_line_for_new_line(hunks, new_line)
  if not new_line then return nil end
  local delta = 0
  for _, hunk in ipairs(hunks or {}) do
    local old_start = hunk.old_start or 0
    local old_count = hunk.old_count or 0
    local new_start = hunk.new_start or 0
    local new_count = hunk.new_count or 0
    if new_line < new_start then return new_line + delta end
    if old_count > 0 and new_count == 0 then
      if new_line == new_start then return math.max(1, old_start - 1) end
      delta = old_start + old_count - new_start - 1
    else
      delta = delta + old_count - new_count
    end
    if new_count > 0 and new_line <= new_start + new_count - 1 then
      local parsed_hunk = hunk.lines and #hunk.lines > 0 and hunk or parse_hunk_body(vim.deepcopy(hunk))
      for _, parsed_line in ipairs(parsed_hunk.lines or {}) do
        if parsed_line.new_line == new_line and parsed_line.old_line then return parsed_line.old_line end
      end
      if old_count == new_count then return old_start + (new_line - new_start) end
      return nil
    end
  end
  return new_line + delta
end

---@param padding_lines DiffReviewHunkContextPaddingLine[]
---@param block DiffReviewParsedBlock
---@return DiffReviewHunkContextPaddingLine[]
function M._hunk_annotate_padding_line_numbers(padding_lines, block)
  for _, padding_line in ipairs(padding_lines or {}) do
    padding_line.new_line = padding_line.line_number
    padding_line.old_line = M._hunk_old_line_for_new_line(block and block.hunks or {}, padding_line.line_number)
  end
  return padding_lines
end

---@class DiffReviewHunkRenderPlan
---@field block DiffReviewParsedBlock
---@field hunk DiffReviewParsedHunk
---@field hunk_index integer
---@field region DiffReviewHunkChangeRegion
---@field region_index integer
---@field region_count integer
---@field render_items table[]
---@field include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@field gutter DiffReviewGutterSpec
---@field source_lines string[]?
---@field occupied_lines table<integer, boolean>
---@field syntax_by_item table<integer, { syntax?: DiffReviewTreeSitterSyntax, row?: integer }>
---@field visible_source_lines table<string, boolean>
---@field before_padding_lines DiffReviewHunkContextPaddingLine[]
---@field after_padding_lines DiffReviewHunkContextPaddingLine[]
---@field changed_lines table<integer, boolean>
---@field display_start integer?
---@field display_end integer?

---@class DiffReviewHunkDisplayGroup
---@field plans DiffReviewHunkRenderPlan[]
---@field gutter DiffReviewGutterSpec
---@field added integer
---@field removed integer
---@field display_start integer?
---@field display_end integer?
---@field changed_lines table<integer, boolean>

---@param region DiffReviewHunkChangeRegion
---@param render_items table[]
---@param include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<string, boolean>
function M._hunk_region_visible_source_lines(region, render_items, include_render_line)
  local visible = {}
  for item_index = region.first_item, region.last_item do
    local item = render_items[item_index]
    if item then
      for _, parsed_line in ipairs(M._hunk_render_item_backing_lines(item)) do
        if (not include_render_line or include_render_line(parsed_line))
          and (parsed_line.prefix == " " or parsed_line.prefix == "+" or parsed_line.prefix == "-") then
          visible[parsed_line.code] = true
        end
      end
      if item.kind == "replacement" and item.display_line then visible[item.display_line.code] = true end
    end
  end
  return visible
end

---@param region DiffReviewHunkChangeRegion
---@param render_items table[]
---@param line_by_position table<integer, integer>
---@return table<integer, boolean>
function M._hunk_region_changed_current_lines(region, render_items, line_by_position)
  local changed_lines = {}
  for item_index = region.first_item, region.last_item do
    local item = render_items[item_index]
    if item then
      for _, parsed_line in ipairs(M._hunk_render_item_backing_lines(item)) do
        if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
          local current_line = M._hunk_parsed_line_current_line(parsed_line, line_by_position)
          if current_line then changed_lines[current_line] = true end
        end
      end
    end
  end
  return changed_lines
end

---@param region DiffReviewHunkChangeRegion
---@param before_padding_lines DiffReviewHunkContextPaddingLine[]
---@param after_padding_lines DiffReviewHunkContextPaddingLine[]
---@return integer? display_start
---@return integer? display_end
function M._hunk_region_display_window(region, before_padding_lines, after_padding_lines)
  local display_start = region.changed_line or region.context_line
  local display_end = region.after_line and (region.after_line - 1) or display_start
  for _, padding_line in ipairs(before_padding_lines or {}) do
    display_start = math.min(display_start or padding_line.line_number, padding_line.line_number)
    display_end = math.max(display_end or padding_line.line_number, padding_line.line_number)
  end
  for _, padding_line in ipairs(after_padding_lines or {}) do
    display_start = math.min(display_start or padding_line.line_number, padding_line.line_number)
    display_end = math.max(display_end or padding_line.line_number, padding_line.line_number)
  end
  return display_start, display_end
end

---@param left DiffReviewGutterSpec?
---@param right DiffReviewGutterSpec?
---@return DiffReviewGutterSpec
function M._merge_hunk_gutter_specs(left, right)
  left = left or default_hunk_gutter_spec()
  right = right or default_hunk_gutter_spec()
  local old_width = math.max(left.old_width or 0, right.old_width or 0)
  local new_width = math.max(left.new_width or 0, right.new_width or 0)
  return {
    old_width = old_width,
    new_width = new_width,
    width = old_width + 2 + new_width + 2 + 1 + 1,
  }
end

---@param group DiffReviewHunkDisplayGroup
---@param plan DiffReviewHunkRenderPlan
function M._add_plan_to_hunk_display_group(group, plan)
  group.plans[#group.plans + 1] = plan
  group.gutter = M._merge_hunk_gutter_specs(group.gutter, plan.gutter)
  group.added = (group.added or 0) + (plan.region.added or 0)
  group.removed = (group.removed or 0) + (plan.region.removed or 0)
  if plan.display_start then group.display_start = math.min(group.display_start or plan.display_start, plan.display_start) end
  if plan.display_end then group.display_end = math.max(group.display_end or plan.display_end, plan.display_end) end
  for line_number in pairs(plan.changed_lines or {}) do
    group.changed_lines[line_number] = true
  end
end

---@param previous_plan DiffReviewHunkRenderPlan
---@param next_plan DiffReviewHunkRenderPlan
---@return boolean
function M._hunk_render_plans_should_merge(previous_plan, next_plan)
  if previous_plan.block.file ~= next_plan.block.file then return false end
  if previous_plan.display_end and next_plan.display_start and next_plan.display_start <= previous_plan.display_end + 1 then
    return true
  end
  if not M._hunk_contexts_related(previous_plan.region.context, next_plan.region.context) then return false end
  local gap = M._hunk_visible_plan_gap(previous_plan, next_plan)
  if gap and gap > 0 and type(previous_plan.source_lines) ~= "table" then return false end
  return gap ~= nil and gap >= 0 and gap <= M._hunk_context_bridge_limit()
end

---@param plans DiffReviewHunkRenderPlan[]
---@return DiffReviewHunkDisplayGroup[]
function M._merge_hunk_render_plans(plans)
  local groups = {}
  local current_group = nil ---@type DiffReviewHunkDisplayGroup?
  for _, plan in ipairs(plans) do
    local previous_plan = current_group and current_group.plans[#current_group.plans] or nil
    if not (current_group and previous_plan and M._hunk_render_plans_should_merge(previous_plan, plan)) then
      current_group = {
        plans = {},
        gutter = plan.gutter,
        added = 0,
        removed = 0,
        changed_lines = {},
      }
      groups[#groups + 1] = current_group
    end
    M._add_plan_to_hunk_display_group(current_group, plan)
  end
  return groups
end

---@param left_old integer?
---@param left_new integer?
---@param right_old integer?
---@param right_new integer?
---@return boolean
function M._hunk_render_coords_adjacent(left_old, left_new, right_old, right_new)
  if left_old and right_old and right_old == left_old + 1 then return true end
  if left_new and right_new and right_new == left_new + 1 then return true end
  return false
end

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean
function M._hunk_contexts_related(left, right)
  if same_hunk_context_scope(left, right) then return true end
  if type(left) ~= "table" or type(right) ~= "table" then return false end
  local left_start = left.start_row + 1
  local left_end = left.end_row + 1
  local right_start = right.start_row + 1
  local right_end = right.end_row + 1
  return (right_start >= left_start and right_end <= left_end)
    or (left_start >= right_start and left_end <= right_end)
end

---@return integer
function M._hunk_context_bridge_limit()
  return M._hunk_context_padding_limit() * 2
end

---@param left_old integer?
---@param left_new integer?
---@param right_old integer?
---@param right_new integer?
---@return integer?
function M._hunk_render_coord_gap(left_old, left_new, right_old, right_new)
  local gap = nil
  local function include_gap(left, right)
    if not (left and right) then return end
    local candidate = right - left - 1
    if gap == nil or candidate > gap then gap = candidate end
  end
  include_gap(left_new, right_new)
  include_gap(left_old, right_old)
  return gap
end

---@param left_new integer?
---@param right_new integer?
---@return integer?
function M._hunk_single_hidden_new_line(left_new, right_new)
  if left_new and right_new and right_new == left_new + 2 then return left_new + 1 end
  return nil
end

---@param source_lines string[]?
---@param block DiffReviewParsedBlock
---@param line_number integer?
---@param gutter DiffReviewGutterSpec
---@param file string?
---@param syntax? DiffReviewTreeSitterSyntax
---@return table?
function M._hunk_single_hidden_context_row(source_lines, block, line_number, gutter, file, syntax)
  if not line_number or type(source_lines) ~= "table" or not source_lines[line_number] then return nil end
  local old_line = M._hunk_old_line_for_new_line(block and block.hunks or {}, line_number)
  return M._hunk_context_padding_row(line_number, source_lines[line_number], gutter, file, syntax, old_line, line_number)
end

---@param source_lines string[]?
---@param block DiffReviewParsedBlock
---@param left_new integer?
---@param right_new integer?
---@param changed_lines table<integer, boolean>
---@param emitted_context_lines table<integer, boolean>
---@param gutter DiffReviewGutterSpec
---@param file string?
---@param syntax? DiffReviewTreeSitterSyntax
---@return table? row
---@return integer? line_number
function M._hunk_single_hidden_context_gap_row(source_lines, block, left_new, right_new, changed_lines, emitted_context_lines, gutter, file, syntax)
  local hidden_line = M._hunk_single_hidden_new_line(left_new, right_new)
  if not hidden_line or emitted_context_lines[hidden_line] or changed_lines[hidden_line] then return nil, nil end
  return M._hunk_single_hidden_context_row(source_lines, block, hidden_line, gutter, file, syntax), hidden_line
end

---@param plan DiffReviewHunkRenderPlan
---@param raw_context DiffReviewHunkTreeSitterContext|string?
---@return integer? old_line
---@return integer? new_line
function M._hunk_first_visible_plan_coords(plan, raw_context)
  local padding_line = plan.before_padding_lines and plan.before_padding_lines[1] or nil
  if padding_line then return padding_line.old_line, padding_line.new_line end
  for item_index = plan.region.first_item, plan.region.last_item do
    local item = plan.render_items[item_index]
    local parsed_line = M._hunk_render_item_line(item)
    if item.kind == "replacement" or (plan.include_render_line(parsed_line) and hunk_line_visible_in_context_scope(parsed_line, raw_context)) then
      return parsed_line.old_line, parsed_line.new_line
    end
  end
  return nil, nil
end

---@param plan DiffReviewHunkRenderPlan
---@param raw_context DiffReviewHunkTreeSitterContext|string?
---@return integer? old_line
---@return integer? new_line
function M._hunk_last_visible_plan_coords(plan, raw_context)
  local padding_line = plan.after_padding_lines and plan.after_padding_lines[#plan.after_padding_lines] or nil
  if padding_line then return padding_line.old_line, padding_line.new_line end
  for item_index = plan.region.last_item, plan.region.first_item, -1 do
    local item = plan.render_items[item_index]
    local parsed_line = M._hunk_render_item_line(item)
    if item.kind == "replacement" or (plan.include_render_line(parsed_line) and hunk_line_visible_in_context_scope(parsed_line, raw_context)) then
      return parsed_line.old_line, parsed_line.new_line
    end
  end
  return nil, nil
end

---@param previous_plan DiffReviewHunkRenderPlan
---@param next_plan DiffReviewHunkRenderPlan
---@return integer?
function M._hunk_visible_plan_gap(previous_plan, next_plan)
  local previous_old_line, previous_new_line = M._hunk_last_visible_plan_coords(previous_plan, previous_plan.region.context)
  local next_old_line, next_new_line = M._hunk_first_visible_plan_coords(next_plan, next_plan.region.context)
  return M._hunk_render_coord_gap(previous_old_line, previous_new_line, next_old_line, next_new_line)
end

---@param diff_text string
---@param hunk_staged? boolean[]
---@param filename? string
---@param context_callback_key? fun(hunk_line: number): string
---@param on_context_update? fun()
---@param opts? { context_line: integer?, boundary_context: boolean?, suppress_start_boundary: boolean?, suppress_end_boundary: boolean?, suppress_start_boundary_keys?: table<string, boolean>, suppress_end_boundary_keys?: table<string, boolean>, syntax_source?: "file"|"diff", syntax_diff_text?: string, compact_replacements?: boolean }
local function build_fancy_diff_rows(diff_text, hunk_staged, filename, context_callback_key, on_context_update, opts)
  opts = opts or {}

  local diff = parse_unified_diff(diff_text)
  local syntax_diff_text = opts.syntax_diff_text or diff_text
  local ret = {} ---@type table[]
  local hunk_idx = 0
  local old_syntax = nil
  local old_file_syntax = nil
  local new_syntax = nil
  local file_syntax = nil
  local syntax_pending = false
  local context_pending = false
  local old_syntax_row = 0
  local new_syntax_row = 0
  if filename then
    local syntax_callback_key = context_callback_key and context_callback_key(0) or ("diff-row:" .. filename .. ":0")
    local old_pending = false
    local old_file_pending = false
    local new_pending = false
    local file_pending = false
    if M._diff_uses_file_syntax(hunk_staged, opts) and M._diff_new_side_matches_file(filename, syntax_diff_text) then
      old_file_syntax, old_file_pending = M._cached_old_file_syntax(filename, syntax_diff_text, syntax_callback_key .. ":old-file-syntax", on_context_update)
      if not old_file_syntax and not old_file_pending then
        old_syntax, old_pending = cached_diff_syntax(filename, diff_text, "old", syntax_callback_key .. ":old-diff-syntax", on_context_update)
      end
      file_syntax, file_pending = cached_file_syntax(filename, syntax_callback_key .. ":file-syntax", on_context_update)
      if not file_syntax and not file_pending then
        new_syntax, new_pending = cached_diff_syntax(filename, diff_text, "new", syntax_callback_key .. ":new-diff-syntax", on_context_update)
      end
    else
      old_syntax, old_pending = cached_diff_syntax(filename, diff_text, "old", syntax_callback_key .. ":old-diff-syntax", on_context_update)
      new_syntax, new_pending = cached_diff_syntax(filename, diff_text, "new", syntax_callback_key .. ":new-diff-syntax", on_context_update)
    end
    syntax_pending = old_pending or old_file_pending or new_pending or file_pending
  end

  local source_lines = filename and M._file_source_lines(filename) or nil
  local plans = {} ---@type DiffReviewHunkRenderPlan[]

  local function context_for_line(block, line)
    if not (filename and line) then return nil end
    local callback_key = context_callback_key and context_callback_key(line)
      or ("diff-row:" .. (filename or block.file) .. ":" .. line)
    local context = cached_hunk_context(filename, line, callback_key, on_context_update)
    local cached_context = M._ts_context_cache and M._ts_context_cache[filename .. ":" .. line] or nil
    if type(cached_context) == "table" and cached_context.pending then context_pending = true end
    return context
  end

  local function syntax_for_line(parsed_line)
    local row_syntax = nil
    local row_syntax_row = nil
    if parsed_line.prefix == "-" then
      row_syntax = old_file_syntax or old_syntax
      row_syntax_row = old_file_syntax and parsed_line.old_line and (parsed_line.old_line - 1) or old_syntax_row
    elseif file_syntax and parsed_line.new_line then
      row_syntax = file_syntax
      row_syntax_row = parsed_line.new_line - 1
    else
      row_syntax = new_syntax
      row_syntax_row = new_syntax_row
    end
    return row_syntax, row_syntax_row
  end

  local function advance_syntax_rows(parsed_line)
    if parsed_line.prefix == " " then
      old_syntax_row = old_syntax_row + 1
      new_syntax_row = new_syntax_row + 1
    elseif parsed_line.prefix == "-" then
      old_syntax_row = old_syntax_row + 1
    elseif parsed_line.prefix == "+" then
      new_syntax_row = new_syntax_row + 1
    end
  end

  for _, block in ipairs(diff) do
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      hunk = parse_hunk_body(hunk)
      local include_render_line = M._hunk_render_line_filter(hunk)
      local line_by_position = M._hunk_current_line_by_position(hunk)
      local occupied_lines = M._hunk_changed_current_line_set(hunk, line_by_position)
      local render_items = nil
      if opts.compact_replacements then
        render_items = M._intraline_diff.compact_hunk_lines(hunk.lines)
      else
        render_items = {}
        for _, parsed_line in ipairs(hunk.lines) do
          render_items[#render_items + 1] = { kind = "line", line = parsed_line }
        end
      end

      local regions = M._hunk_change_regions(render_items, line_by_position, function(line)
        return context_for_line(block, line)
      end)
      local syntax_by_item = {}
      for item_index, item in ipairs(render_items) do
        local parsed_line = M._hunk_render_item_line(item)
        local row_syntax, row_syntax_row = syntax_for_line(parsed_line)
        syntax_by_item[item_index] = {
          syntax = row_syntax,
          row = row_syntax_row,
        }
        for _, backing_line in ipairs(M._hunk_render_item_backing_lines(item)) do
          advance_syntax_rows(backing_line)
        end
      end

      for region_index, region in ipairs(regions) do
        local region_bounds = {
          changed_line = region.changed_line,
          after_line = region.after_line,
        }
        local before_padding_lines = M._hunk_context_padding_lines(
          source_lines,
          hunk,
          region.context,
          "before",
          occupied_lines,
          include_render_line,
          region_bounds
        )
        M._hunk_annotate_padding_line_numbers(before_padding_lines, block)
        local after_padding_lines = M._hunk_context_padding_lines(
          source_lines,
          hunk,
          region.context,
          "after",
          occupied_lines,
          include_render_line,
          region_bounds
        )
        M._hunk_annotate_padding_line_numbers(after_padding_lines, block)
        local display_start, display_end = M._hunk_region_display_window(region, before_padding_lines, after_padding_lines)
        plans[#plans + 1] = {
          block = block,
          hunk = hunk,
          hunk_index = hunk_idx,
          region = region,
          region_index = region_index,
          region_count = #regions,
          render_items = render_items,
          include_render_line = include_render_line,
          gutter = hunk.gutter,
          source_lines = source_lines,
          occupied_lines = occupied_lines,
          syntax_by_item = syntax_by_item,
          visible_source_lines = M._hunk_region_visible_source_lines(region, render_items, include_render_line),
          before_padding_lines = before_padding_lines,
          after_padding_lines = after_padding_lines,
          changed_lines = M._hunk_region_changed_current_lines(region, render_items, line_by_position),
          display_start = display_start,
          display_end = display_end,
        }
      end
    end
  end

  local display_groups = M._merge_hunk_render_plans(plans)
  local emitted_start_scope_keys = {}
  for group_index, group in ipairs(display_groups) do
    local next_group = display_groups[group_index + 1]
    ret[#ret + 1] = hunk_header_row(M._hunk_virtual_header_parts(group, group.plans[1] and group.plans[1].hunk or nil))
    local emitted_context_lines = {}

    local function add_context_padding_rows(padding_lines, gutter, block_file)
      for _, padding_line in ipairs(padding_lines or {}) do
        if not group.changed_lines[padding_line.line_number] and not emitted_context_lines[padding_line.line_number] then
          ret[#ret + 1] = M._hunk_context_padding_row(
            padding_line.line_number,
            padding_line.text,
            gutter,
            filename or block_file,
            file_syntax,
            padding_line.old_line,
            padding_line.new_line
          )
          emitted_context_lines[padding_line.line_number] = true
        end
      end
    end

    local function mark_emitted_context_line(parsed_line)
      if parsed_line.new_line then emitted_context_lines[parsed_line.new_line] = true end
    end

    local function add_context_bridge_rows(plan, next_plan)
      if not (plan and next_plan and type(source_lines) == "table") then return end
      local previous_old_line, previous_new_line = M._hunk_last_visible_plan_coords(plan, plan.region.context)
      local next_old_line, next_new_line = M._hunk_first_visible_plan_coords(next_plan, next_plan.region.context)
      local gap = M._hunk_render_coord_gap(previous_old_line, previous_new_line, next_old_line, next_new_line)
      if not (gap and gap > 0 and gap <= M._hunk_context_bridge_limit()) then return end
      if not (previous_new_line and next_new_line and next_new_line > previous_new_line + 1) then return end
      for line_number = previous_new_line + 1, next_new_line - 1 do
        if not group.changed_lines[line_number] and not emitted_context_lines[line_number] then
          ret[#ret + 1] = M._hunk_context_padding_row(
            line_number,
            source_lines[line_number] or "",
            group.gutter,
            filename or plan.block.file,
            file_syntax,
            M._hunk_old_line_for_new_line(plan.block.hunks, line_number),
            line_number
          )
          emitted_context_lines[line_number] = true
        end
      end
    end

    for plan_index, plan in ipairs(group.plans) do
      local raw_context = plan.region.context
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local start_text = raw_context.start_text or ""
        local previous_plan = group.plans[plan_index - 1]
        local start_boundaries = {}
        for _, boundary in ipairs(raw_context.ancestor_boundaries or {}) do
          start_boundaries[#start_boundaries + 1] = boundary
        end
        start_boundaries[#start_boundaries + 1] = {
          key = hunk_context_scope_key(raw_context),
          row = node_start,
          text = start_text,
          segments = raw_context.start_segments,
          selected = true,
        }
        for boundary_index, boundary in ipairs(start_boundaries) do
          local boundary_key = boundary.key
          local boundary_seen = boundary_key and emitted_start_scope_keys[boundary_key] == true
          local suppress_boundary_key = boundary_key
            and opts.suppress_start_boundary_keys
            and opts.suppress_start_boundary_keys[boundary_key] == true
          local suppress_start_boundary = (group_index == 1 and plan_index == 1 and opts.suppress_start_boundary)
            or same_hunk_context_scope(previous_plan and previous_plan.region.context, raw_context)
            or boundary_seen
            or suppress_boundary_key
          local boundary_visible = plan.visible_source_lines[boundary.text] == true
          local boundary_rendered = false
          if not suppress_start_boundary
            and not boundary_visible
            and not emitted_context_lines[boundary.row] then
            local boundary_old_line = M._hunk_old_line_for_new_line(plan.block.hunks, boundary.row)
            local boundary_new_line = boundary.row
            ret[#ret + 1] = hunk_boundary_row(
              boundary.text,
              boundary.segments,
              boundary.row,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
            boundary_rendered = true
            emitted_context_lines[boundary.row] = true
            local next_old_line = nil
            local next_new_line = nil
            local next_boundary = start_boundaries[boundary_index + 1]
            if next_boundary then
              next_old_line = M._hunk_old_line_for_new_line(plan.block.hunks, next_boundary.row)
              next_new_line = next_boundary.row
            else
              next_old_line, next_new_line = M._hunk_first_visible_plan_coords(plan, raw_context)
            end
            local adjacent_to_next = M._hunk_render_coords_adjacent(boundary_old_line, boundary_new_line, next_old_line, next_new_line)
            if not adjacent_to_next then
              if boundary.selected then
                local hidden_row, hidden_line = M._hunk_single_hidden_context_gap_row(
                  source_lines,
                  plan.block,
                  boundary_new_line,
                  next_new_line,
                  group.changed_lines,
                  emitted_context_lines,
                  group.gutter,
                  filename or plan.block.file,
                  file_syntax
                )
                if hidden_row then
                  ret[#ret + 1] = hidden_row
                  emitted_context_lines[hidden_line] = true
                else
                  ret[#ret + 1] = hunk_boundary_ellipsis_row(boundary.text, group.gutter)
                end
              else
                ret[#ret + 1] = hunk_boundary_ellipsis_row(boundary.text, group.gutter)
              end
            end
          end
          if boundary_key and not boundary_seen and (boundary_rendered or boundary_visible) then
            emitted_start_scope_keys[boundary_key] = true
          end
        end
      end
      add_context_padding_rows(plan.before_padding_lines, group.gutter, plan.block.file)

      local previous_visible_changed = false
      local last_visible_new_line = nil
      for item_index = plan.region.first_item, plan.region.last_item do
        local item = plan.render_items[item_index]
        local parsed_line = M._hunk_render_item_line(item)
        local syntax_entry = plan.syntax_by_item[item_index] or {}
        local row_syntax = syntax_entry.syntax
        local row_syntax_row = syntax_entry.row
        local visible_in_scope = hunk_line_visible_in_context_scope(parsed_line, raw_context)
        local visible_in_hunk = plan.include_render_line(parsed_line)
        local context_already_emitted = parsed_line.prefix == " " and parsed_line.new_line and emitted_context_lines[parsed_line.new_line]
        if item.kind == "replacement" then
          ret[#ret + 1] = M._hunk_replacement_row(item, group.gutter, filename or plan.block.file, row_syntax, row_syntax_row)
          for _, backing_line in ipairs(item.diff_lines or {}) do
            mark_emitted_context_line(backing_line)
          end
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = true
        elseif visible_in_hunk and visible_in_scope and not context_already_emitted then
          ret[#ret + 1] = hunk_body_row(parsed_line, group.gutter, filename or plan.block.file, row_syntax, row_syntax_row)
          mark_emitted_context_line(parsed_line)
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = parsed_line.prefix == "+" or parsed_line.prefix == "-"
        elseif M._hunk_hidden_closing_boundary_after_change(parsed_line, previous_visible_changed, raw_context, visible_in_hunk) then
          local boundary_segments = nil
          if row_syntax and row_syntax_row then
            boundary_segments = treesitter_line_segments(row_syntax.buf, row_syntax.tree, row_syntax.highlight_query, row_syntax_row, parsed_line.code)
          end
          local previous_line_emitted = parsed_line.new_line and emitted_context_lines[parsed_line.new_line - 1] == true
          if not previous_line_emitted then
            local hidden_row, hidden_line = M._hunk_single_hidden_context_gap_row(
              source_lines,
              plan.block,
              last_visible_new_line,
              parsed_line.new_line,
              group.changed_lines,
              emitted_context_lines,
              group.gutter,
              filename or plan.block.file,
              file_syntax
            )
            if hidden_row then
              ret[#ret + 1] = hidden_row
              emitted_context_lines[hidden_line] = true
            else
              ret[#ret + 1] = hunk_boundary_ellipsis_row(parsed_line.code, group.gutter)
            end
          end
          ret[#ret + 1] = hunk_boundary_row(
            parsed_line.code,
            boundary_segments,
            parsed_line.new_line or parsed_line.old_line,
            group.gutter,
            filename or plan.block.file,
            parsed_line.old_line,
            parsed_line.new_line
          )
          mark_emitted_context_line(parsed_line)
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = false
        end
      end

      local next_plan = group.plans[plan_index + 1]
      local after_padding_lines = plan.after_padding_lines
      if next_plan and next_plan.region.changed_line then
        after_padding_lines = {}
        for _, padding_line in ipairs(plan.after_padding_lines or {}) do
          if padding_line.line_number < next_plan.region.changed_line then after_padding_lines[#after_padding_lines + 1] = padding_line end
        end
      end
      add_context_padding_rows(after_padding_lines, group.gutter, plan.block.file)
      add_context_bridge_rows(plan, next_plan)
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local node_end = raw_context.end_row + 1
        local end_text = raw_context.end_text or ""
        local next_group_plan = next_plan == nil and next_group and next_group.plans and next_group.plans[1] or nil
        local suppress_end_boundary = next_plan ~= nil
          or (group_index == #display_groups and plan_index == #group.plans and opts.suppress_end_boundary)
          or same_hunk_context_scope(raw_context, next_plan and next_plan.region.context)
          or same_hunk_context_scope(raw_context, next_group_plan and next_group_plan.region.context)
        if not suppress_end_boundary and not plan.visible_source_lines[end_text] and not emitted_context_lines[node_end] then
          local boundary_old_line = M._hunk_old_line_for_new_line(plan.block.hunks, node_end)
          local boundary_new_line = node_end
          local previous_old_line, previous_new_line = M._hunk_last_visible_plan_coords(plan, raw_context)
          local adjacent_to_previous = M._hunk_render_coords_adjacent(previous_old_line, previous_new_line, boundary_old_line, boundary_new_line)
          if node_end ~= node_start and not adjacent_to_previous then
            local hidden_row, hidden_line = M._hunk_single_hidden_context_gap_row(
              source_lines,
              plan.block,
              previous_new_line,
              boundary_new_line,
              group.changed_lines,
              emitted_context_lines,
              group.gutter,
              filename or plan.block.file,
              file_syntax
            )
            if hidden_row then
              ret[#ret + 1] = hidden_row
              emitted_context_lines[hidden_line] = true
            else
              ret[#ret + 1] = hunk_boundary_ellipsis_row(end_text, group.gutter)
            end
          end
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_row(
              end_text,
              raw_context.end_segments,
              node_end,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
          end
        end
        local ancestor_boundary = M._hunk_context_ancestor_boundary(raw_context)
        if ancestor_boundary
          and ancestor_boundary.end_row
          and ancestor_boundary.end_row ~= node_end then
          local suppress_ancestor_end = (ancestor_boundary.key ~= nil
              and opts.suppress_end_boundary_keys
              and opts.suppress_end_boundary_keys[ancestor_boundary.key] == true)
            or (next_plan ~= nil and M._hunk_context_ancestor_key(next_plan.region.context) == ancestor_boundary.key)
            or (next_group_plan ~= nil and M._hunk_context_ancestor_key(next_group_plan.region.context) == ancestor_boundary.key)
          if not suppress_ancestor_end and not emitted_context_lines[ancestor_boundary.end_row] then
            local boundary_old_line = M._hunk_old_line_for_new_line(plan.block.hunks, ancestor_boundary.end_row)
            local boundary_new_line = ancestor_boundary.end_row
            local previous_old_line, previous_new_line = M._hunk_last_visible_plan_coords(plan, raw_context)
            local adjacent_to_previous = M._hunk_render_coords_adjacent(previous_old_line, previous_new_line, boundary_old_line, boundary_new_line)
            if not adjacent_to_previous then
              local hidden_row, hidden_line = M._hunk_single_hidden_context_gap_row(
                source_lines,
                plan.block,
                previous_new_line,
                boundary_new_line,
                group.changed_lines,
                emitted_context_lines,
                group.gutter,
                filename or plan.block.file,
                file_syntax
              )
              if hidden_row then
                ret[#ret + 1] = hidden_row
                emitted_context_lines[hidden_line] = true
              else
                ret[#ret + 1] = hunk_boundary_ellipsis_row(ancestor_boundary.end_text, group.gutter)
              end
            end
            ret[#ret + 1] = hunk_boundary_row(
              ancestor_boundary.end_text,
              ancestor_boundary.end_segments,
              ancestor_boundary.end_row,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
            emitted_context_lines[ancestor_boundary.end_row] = true
          end
        end
      end
    end
  end

  ret.diff_review_syntax_pending = syntax_pending
  ret.diff_review_context_pending = context_pending
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
  local content_lengths = {}

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
    if row.diff_review_bg_hl then
      local content_length
      lines[row_index], content_length = M._diff_pad_highlighted_line(lines[row_index], buf)
      content_lengths[row_index] = content_length
      extmarks[#extmarks + 1] = {
        line = row_index,
        col = 0,
        opts = {
          end_col = #lines[row_index],
          hl_group = row.diff_review_bg_hl,
          priority = 60,
        },
      }
    end
    for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
      highlights[#highlights + 1] = {
        line = row_index,
        start_col = inline_highlight.start_col,
        end_col = inline_highlight.end_col,
        hl_group = inline_highlight.hl_group,
        priority = inline_highlight.priority or 110,
      }
    end
    if empty_diff_row then empty_diff_rows[row_index] = true end
  end

  M._empty_diff_rows = M._empty_diff_rows or {}
  M._empty_diff_rows[buf] = empty_diff_rows
  M._diff_line_content_lengths = M._diff_line_content_lengths or {}
  M._diff_line_content_lengths[buf] = content_lengths
  M._clear_diff_gutter_visual_line(buf)
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
      priority = highlight.priority or 90,
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
    if M._diff_line_content_lengths then M._diff_line_content_lengths[buf] = nil end
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

local function confirm(lines, on_yes, on_no)
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
  local function cancel()
    close()
    if on_no then on_no() end
  end
  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, cancel, { buffer = buf, nowait = true, silent = true })
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
  { id = "visual_line_with_gutter", label = "select gutter", desc = "Start visual line selection including the diff gutter", modes = "n", pinned = false, views = { status = true, pr = true, diff = true, review = true } },
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
    "toggle",
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
      if M._status_issues then M._status_issues.sync_modifiable(buf) end
    end,
  })
  vim.api.nvim_create_autocmd("ModeChanged", {
    buffer = buf,
    callback = function()
      M._normalize_status_cursor(buf)
      if M._status_issues then M._status_issues.sync_modifiable(buf) end
    end,
  })
  if state.view_kind == "status" and M._status_issues then
    M._status_issues.attach(buf)
  end
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
      if M._diff_line_content_lengths then M._diff_line_content_lengths[buf] = nil end
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

local function status_head_row(name, oid, ref, ref_hl, subject, date_text, ref_width)
  local segments = {
    { ("%-8s"):format(name .. ":"), "DiffReviewStatusLabel" },
    { ("%-7s"):format(oid or ""), "DiffReviewStatusObjectId" },
  }
  if date_text and date_text ~= "" then
    segments[#segments + 1] = { " " }
    segments[#segments + 1] = { date_text, "DiffReviewStatusDate" }
  end
  local ref_text = ref or ""
  local ref_padding = ""
  if ref_width and ref_width > 0 then
    local padding_width = ref_width - vim.fn.strdisplaywidth(ref_text)
    if padding_width > 0 then ref_padding = string.rep(" ", padding_width) end
  end
  vim.list_extend(segments, {
    { " " },
    { ref_text, ref_hl },
  })
  if ref_padding ~= "" then segments[#segments + 1] = { ref_padding } end
  segments[#segments + 1] = { " " }
  vim.list_extend(segments, M._status_conventional_commit_subject_segments(subject or "", nil))
  return segments
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
    local subject = pr_state.pr.title ~= "" and pr_state.pr.title or ("PR #" .. tostring(pr_state.pr.number))
    vim.list_extend(segments, M._status_conventional_commit_subject_segments(subject, "DiffReviewStatusPR"))
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
    vim.list_extend(segments, M._status_conventional_commit_subject_segments(ai_commit.subject(about_state.message), "DiffReviewStatusPath"))
  elseif about_state.state == "error" then
    segments[#segments + 1] = { "error", "ErrorMsg" }
  else
    segments[#segments + 1] = { "none", "Comment" }
  end

  return { segments = segments, entry = entry }
end

M._status_issues = M._status_issues or {}

---@param cwd string?
---@return string?
function M._status_issues.path(cwd)
  if not cwd or cwd == "" then return nil end
  return vim.fs.joinpath(cwd, ".session")
end

---@param values any
---@return integer[]
function M._status_issues.normalize_numbers(values)
  local numbers = {}
  local seen = {}
  if type(values) ~= "table" then return numbers end
  for _, value in ipairs(values) do
    local number = tonumber(value)
    if number and number > 0 and math.floor(number) == number and not seen[number] then
      seen[number] = true
      numbers[#numbers + 1] = number
    end
  end
  table.sort(numbers)
  return numbers
end

---@param line string?
---@return integer[]
function M._status_issues.parse_line(line)
  local body = tostring(line or ""):gsub("^%s*Issues:%s*", "")
  body = vim.trim(body)
  if body == "" or body:lower() == "none" then return {} end
  local values = {}
  local seen_text = {}
  for issue_text in body:gmatch("#%s*(%d+)") do
    if not seen_text[issue_text] then
      seen_text[issue_text] = true
      values[#values + 1] = issue_text
    end
  end
  for issue_text in body:gmatch("%f[%d](%d+)%f[%D]") do
    if not seen_text[issue_text] then
      seen_text[issue_text] = true
      values[#values + 1] = issue_text
    end
  end
  return M._status_issues.normalize_numbers(values)
end

---@param numbers integer[]?
---@return string
function M._status_issues.text(numbers)
  numbers = M._status_issues.normalize_numbers(numbers)
  if #numbers == 0 then return "none" end
  local parts = {}
  for _, number in ipairs(numbers) do
    parts[#parts + 1] = "#" .. tostring(number)
  end
  return table.concat(parts, " ")
end

---@param left integer[]?
---@param right integer[]?
---@return boolean
function M._status_issues.equal(left, right)
  left = M._status_issues.normalize_numbers(left)
  right = M._status_issues.normalize_numbers(right)
  if #left ~= #right then return false end
  for index, number in ipairs(left) do
    if number ~= right[index] then return false end
  end
  return true
end

---@param cwd string
---@return table
function M._status_issues.read_state(cwd)
  local path = M._status_issues.path(cwd)
  if not path or vim.fn.filereadable(path) ~= 1 then
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local read_ok, lines = pcall(vim.fn.readfile, path)
  if not read_ok then
    vim.notify("GitStatus session read failed: " .. tostring(lines), vim.log.levels.WARN, { title = "GitStatus" })
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local content = table.concat(lines or {}, "\n")
  if vim.trim(content) == "" then
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local decode_ok, decoded = pcall(vim.json.decode, content)
  if not (decode_ok and type(decoded) == "table") then
    vim.notify("GitStatus session read failed: invalid .session JSON", vim.log.levels.WARN, { title = "GitStatus" })
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local numbers = M._status_issues.normalize_numbers(decoded.issues)
  return { cwd = cwd, numbers = numbers, saved_numbers = vim.deepcopy(numbers), data = decoded }
end

---@param status table?
---@param cwd string?
---@return table?
function M._status_issues.ensure_state(status, cwd)
  if not (status and cwd and cwd ~= "") then return nil end
  if status.issues and status.issues.cwd == cwd then return status.issues end
  status.issues = M._status_issues.read_state(cwd)
  return status.issues
end

---@param cwd string
---@param numbers integer[]
---@param existing table?
---@return boolean
---@return string?
function M._status_issues.write(cwd, numbers, existing)
  local path = M._status_issues.path(cwd)
  if not path then return false, "missing git root" end
  local data = type(existing) == "table" and vim.deepcopy(existing) or {}
  if vim.fn.filereadable(path) == 1 and not existing then
    local read_ok, lines = pcall(vim.fn.readfile, path)
    if not read_ok then return false, tostring(lines) end
    local content = table.concat(lines or {}, "\n")
    if vim.trim(content) ~= "" then
      local decode_ok, decoded = pcall(vim.json.decode, content)
      if not (decode_ok and type(decoded) == "table") then return false, "invalid .session JSON" end
      data = decoded
    end
  end
  data.issues = M._status_issues.normalize_numbers(numbers)
  local write_ok, write_result = pcall(vim.fn.writefile, { vim.json.encode(data) }, path)
  if not write_ok then return false, tostring(write_result) end
  if write_result ~= 0 then return false, "writefile returned " .. tostring(write_result) end
  return true, nil
end

---@param issues_state table?
---@return DiffReviewStatusHeadLine
function M._status_issues.head_line(issues_state)
  local numbers = issues_state and issues_state.numbers or {}
  local text = M._status_issues.text(numbers)
  return {
    segments = {
      { ("%-8s"):format("Issues:"), "DiffReviewStatusLabel" },
      { text, text == "none" and "Comment" or "DiffReviewStatusPR" },
    },
    entry = { id = "issues", kind = "issues", issues = issues_state },
  }
end

---@param status table
---@param head_line DiffReviewStatusHeadLine
function M._status_issues.replace_head_line(status, head_line)
  if not (status and status.head_lines) then return end
  for index, line in ipairs(status.head_lines) do
    if line.entry and line.entry.id == "issues" then
      status.head_lines[index] = head_line
      return
    end
  end
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
---@param issues_state table?
---@return DiffReviewStatusHeadLine[]
local function status_build_head_lines(values, pr_state, about_state, issues_state)
  local remote_action = M._status and M._status.remote_action
  local ref_width = vim.fn.strdisplaywidth(values.branch or "(detached)")
  if values.upstream then ref_width = math.max(ref_width, vim.fn.strdisplaywidth(values.upstream)) end
  if values.push_ref then ref_width = math.max(ref_width, vim.fn.strdisplaywidth(values.push_ref)) end
  local lines = {}
  lines[#lines + 1] = {
    segments = status_head_row(
      "Head",
      values.head_oid or "0000000",
      values.branch or "(detached)",
      "DiffReviewStatusBranch",
      values.subject or "(no commits)",
      nil,
      ref_width
    ),
  }

  if values.upstream then
    lines[#lines + 1] = {
      segments = status_head_row(
        "Merge",
        values.upstream_oid or "",
        values.upstream,
        "DiffReviewStatusRemote",
        values.upstream_subject or "",
        nil,
        ref_width
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
        values.push_subject or "",
        nil,
        ref_width
      ),
    }
  end

  lines[#lines + 1] = status_pr_head_line(pr_state)
  lines[#lines + 1] = status_about_head_line(about_state)
  lines[#lines + 1] = M._status_issues.head_line(issues_state)
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
  if type(section.issue_comments) == "table" then return #section.issue_comments end
  if type(section.commits) == "table" then return #section.commits end
  return #(section.files or {})
end

---@param value any
---@return integer? epoch_seconds
---@return table? date_parts
function M._datetime.parse(value)
  local text = vim.trim(tostring(value or ""))
  if text == "" then return nil, nil end

  local year, month, day, hour, minute, second, zone = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)T(%d%d):(%d%d):?(%d*)%.?%d*([Zz]?)")
  if not year then
    year, month, day, hour, minute, second = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):?(%d*)")
  end
  if not year then return nil, nil end

  local parts = {
    year = tonumber(year),
    month = tonumber(month),
    day = tonumber(day),
    hour = tonumber(hour) or 0,
    min = tonumber(minute) or 0,
    sec = tonumber(second ~= "" and second or "0") or 0,
  }
  if not (parts.year and parts.month and parts.day) then return nil, nil end

  local epoch = os.time(parts)
  if zone and zone:lower() == "z" then
    local now = os.time()
    local utc_now_parts = os.date("!*t", now)
    if type(utc_now_parts) == "table" then
      utc_now_parts.isdst = false
      local local_epoch_for_utc_now = os.time(utc_now_parts)
      if local_epoch_for_utc_now then epoch = epoch + os.difftime(now, local_epoch_for_utc_now) end
    end
  end
  return epoch, parts
end

---@return integer
function M._datetime.now()
  if type(M._datetime.now_override) == "function" then
    local ok, value = pcall(M._datetime.now_override)
    if ok and type(value) == "number" then return value end
  end
  return os.time()
end

---@param count integer
---@param unit string
---@return string
function M._datetime.ago(count, unit)
  count = math.max(1, count)
  local suffix = count == 1 and unit or (unit .. "s")
  return ("%d %s ago"):format(count, suffix)
end

---@param epoch integer
---@return string
function M._datetime.absolute_date(epoch)
  local parts = os.date("*t", epoch)
  if type(parts) ~= "table" then return "" end
  local months = {
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
  }
  return ("%s %d, %d"):format(months[parts.month] or "", parts.day or 1, parts.year or 1970)
end

---@param epoch integer
---@param now integer
---@return integer
function M._datetime.calendar_day_delta(epoch, now)
  local then_parts = os.date("*t", epoch)
  local now_parts = os.date("*t", now)
  if type(then_parts) ~= "table" or type(now_parts) ~= "table" then return 0 end
  local then_midnight = os.time({ year = then_parts.year, month = then_parts.month, day = then_parts.day, hour = 0, min = 0, sec = 0 })
  local now_midnight = os.time({ year = now_parts.year, month = now_parts.month, day = now_parts.day, hour = 0, min = 0, sec = 0 })
  if not (then_midnight and now_midnight) then return 0 end
  return math.floor((os.difftime(now_midnight, then_midnight) / 86400) + 0.5)
end

---@param value any
---@param opts? { yesterday?: boolean }
---@return string
function M._datetime.relative(value, opts)
  opts = opts or {}
  local text = vim.trim(tostring(value or ""))
  if text == "" then return "" end
  local epoch = M._datetime.parse(text)
  if not epoch then return text end

  local now = M._datetime.now()
  local seconds = math.floor(os.difftime(now, epoch))
  if seconds < 60 then return "just now" end
  if seconds < 3600 then return M._datetime.ago(math.floor(seconds / 60), "minute") end
  if seconds < 86400 then return M._datetime.ago(math.floor(seconds / 3600), "hour") end

  local days = M._datetime.calendar_day_delta(epoch, now)
  if days == 1 and opts.yesterday ~= false then return "Yesterday" end
  if days >= 1 and days < 7 then return M._datetime.ago(days, "day") end
  if days >= 7 and days < 14 then return "Last week" end
  if days >= 14 and days < 30 then return M._datetime.ago(days, "day") end
  if days >= 30 and days < 60 then return "Last month" end
  return M._datetime.absolute_date(epoch)
end

---@param user string
---@param action string
---@param value any
---@return string
function M._datetime.action_phrase(user, action, value)
  local relative = M._datetime.relative(value)
  if relative == "" then return ("%s %s"):format(user, action) end
  return ("%s %s %s"):format(user, action, relative)
end

---@param text string
---@return table[]
function M._datetime.date_highlight_ranges(text)
  text = tostring(text or "")
  local ranges = {}
  local patterns = {
    "%f[%w]just now%f[%W]",
    "%f[%w]%d+ minutes? ago%f[%W]",
    "%f[%w]%d+ hours? ago%f[%W]",
    "%f[%w]%d+ days? ago%f[%W]",
    "%f[%w]Yesterday%f[%W]",
    "%f[%w]Last week%f[%W]",
    "%f[%w]Last month%f[%W]",
    "%f[%a]%u%l+ %d%d?, %d%d%d%d%f[%W]",
  }
  for _, pattern in ipairs(patterns) do
    local start_index = 1
    while start_index <= #text do
      local match_start, match_end = text:find(pattern, start_index)
      if not match_start then break end
      ranges[#ranges + 1] = {
        start_col = match_start - 1,
        end_col = match_end,
      }
      start_index = match_end + 1
    end
  end
  table.sort(ranges, function(left, right) return left.start_col < right.start_col end)
  return ranges
end

---@param value any
---@return string
function M._pr_overview.comment_datetime(value)
  return M._datetime.relative(value)
end

---@param body string
---@return string
function M._pr_overview.comment_preview(body)
  return M._comment_rows.preview_text(body)
end

---@param comment table
---@return string
function M._pr_overview.comment_author(comment)
  return M._comment_rows.author(comment)
end

---@param text any
---@param width integer?
---@return string
function M._pr_overview.pad_display_right(text, width)
  return M._comment_rows.pad_right(text, width)
end

---@return GithubCommentRowsOpts
function M._pr_overview.issue_comment_row_options()
  return {
    comment_icon = M._comment_icon,
    relative_date = M._pr_overview.comment_datetime,
    entry_id = M._pr_overview.issue_comment_entry_id,
    preview_width = function(prefix)
      return math.max(0, M._review.comment_rule_width(nil, nil) - vim.fn.strdisplaywidth(prefix))
    end,
    truncate_preview = M._review.truncate_preview_text,
    body_lines = M._review.comment_body_lines,
    kind = "pr_comment",
    line_hl_group = "DiffReviewReviewComment",
    body_hl_group = "DiffReviewReviewComment",
    date_hl_group = "DiffReviewStatusDate",
  }
end

---@param comment table
---@return string
function M._pr_overview.issue_comment_date(comment)
  return M._comment_rows.date(comment, M._pr_overview.issue_comment_row_options())
end

---@param comments table[]?
---@return { author_width: integer, date_width: integer }
function M._pr_overview.issue_comment_alignment(comments)
  return M._comment_rows.alignment(comments, M._pr_overview.issue_comment_row_options())
end

---@param comment table
---@param alignment? { author_width?: integer, date_width?: integer }
---@param has_preview? boolean
---@return string
function M._pr_overview.issue_comment_prefix(comment, alignment, has_preview)
  return M._comment_rows.prefix(comment, alignment, has_preview, M._pr_overview.issue_comment_row_options())
end

---@param comment table
---@param expanded? boolean
---@param alignment? { author_width?: integer, date_width?: integer }
---@return string
function M._pr_overview.issue_comment_line(comment, expanded, alignment)
  return M._comment_rows.line(comment, expanded, alignment, M._pr_overview.issue_comment_row_options())
end

---@param comment table?
---@param index integer
---@return string
function M._pr_overview.issue_comment_entry_id(comment, index)
  local key = M._pr_overview.comment_identity_key(comment)
  if key then return "pr-issue-comment:" .. key end
  local local_id = comment and comment.local_id or nil
  if local_id and local_id ~= "" then return "pr-issue-comment:local:" .. tostring(local_id) end
  return "pr-issue-comment:" .. tostring(index)
end

---@param pr DiffReviewGhPR
---@param status table?
---@return string
function M._pr_overview.activity_text(pr, status)
  local latest_epoch = nil
  local latest_value = nil

  local function consider(value)
    local epoch = M._datetime.parse(value)
    if epoch and (not latest_epoch or epoch > latest_epoch) then
      latest_epoch = epoch
      latest_value = value
    end
  end

  local function consider_comment(comment)
    if type(comment) ~= "table" then return end
    consider(comment.updated_at or comment.updatedAt or comment.created_at or comment.createdAt)
    for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
      if type(reply) == "table" then consider(reply.updated_at or reply.updatedAt or reply.created_at or reply.createdAt) end
    end
  end

  for _, commit in ipairs(type(pr.commits) == "table" and pr.commits or {}) do
    if type(commit) == "table" then
      consider(commit.committedDate or commit.committed_date or commit.committed_at or commit.authoredDate or commit.authored_date or commit.authored_at)
    end
  end

  local comments = status and status.pr_comments or nil
  for _, review in ipairs(type(comments and comments.reviews) == "table" and comments.reviews or {}) do
    if type(review) == "table" then
      consider(review.submitted_at or review.submittedAt or review.updated_at or review.updatedAt or review.created_at or review.createdAt)
      for _, comment in ipairs(type(review.comments) == "table" and review.comments or {}) do
        consider_comment(comment)
      end
    end
  end
  for _, comment in ipairs(type(comments and comments.code_comments) == "table" and comments.code_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(type(comments and comments.issue_comments) == "table" and comments.issue_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(status and status.pr_standalone_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(status and status.pr_regular_comments or {}) do
    consider_comment(comment)
  end

  return latest_value and M._datetime.relative(latest_value) or ""
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
---@return string date_text
local function status_pr_head_subject(pr)
  local commits = pr.commits or {}
  if type(commits) ~= "table" then return "", "" end
  local head_oid = vim.trim(tostring(pr.headRefOid or ""))
  local commit = nil
  if head_oid ~= "" then
    for commit_index = #commits, 1, -1 do
      local candidate = commits[commit_index]
      if type(candidate) == "table" then
        local oid = vim.trim(tostring(candidate.oid or candidate.sha or candidate.id or ""))
        if oid == head_oid then
          commit = candidate
          break
        end
      end
    end
  end
  commit = commit or commits[#commits]
  if type(commit) ~= "table" then return "", "" end
  local date_text = M._datetime.relative(
    commit.committedDate or commit.committed_date or commit.committed_at or commit.authoredDate or commit.authored_date or commit.authored_at,
    { yesterday = false }
  )
  return tostring(commit.messageHeadline or commit.subject or commit.message or ""), date_text
end

---@param username string
---@return string
function M._pr_overview.reviewer_token(username)
  return "@" .. tostring(username or ""):gsub("^@", "")
end

---@param reviewer any
---@return string
function M._pr_overview.reviewer_login(reviewer)
  if type(reviewer) == "table" then
    return tostring(reviewer.login or reviewer.slug or reviewer.name or ""):gsub("^@", "")
  end
  return tostring(reviewer or ""):gsub("^@", "")
end

---@param reviewer any
---@return boolean
function M._pr_overview.reviewer_is_code_owner(reviewer)
  return type(reviewer) == "table" and reviewer.is_code_owner == true
end

---@param login string
---@param is_code_owner boolean?
---@return DiffReviewGhRequestedReviewer
function M._pr_overview.reviewer_entry(login, is_code_owner)
  return {
    login = tostring(login or ""):gsub("^@", ""),
    is_code_owner = is_code_owner or nil,
  }
end

---@param reviewers any[]?
---@param out DiffReviewGhRequestedReviewer[]
---@param seen table<string, boolean>
function M._pr_overview.add_reviewers(reviewers, out, seen)
  for _, reviewer in ipairs(reviewers or {}) do
    local username = M._pr_overview.reviewer_login(reviewer)
    local key = username:lower()
    if username ~= "" and not seen[key] then
      seen[key] = true
      out[#out + 1] = M._pr_overview.reviewer_entry(username, M._pr_overview.reviewer_is_code_owner(reviewer))
    elseif username ~= "" and M._pr_overview.reviewer_is_code_owner(reviewer) then
      for _, existing in ipairs(out) do
        if M._pr_overview.reviewer_login(existing):lower() == key then
          existing.is_code_owner = true
          break
        end
      end
    end
  end
end

---@param pr DiffReviewGhPR
---@param status table?
---@return DiffReviewGhRequestedReviewer[]
function M._pr_overview.pending_reviewers(pr, status)
  local reviewers = {}
  local seen = {}
  M._pr_overview.add_reviewers(pr and pr.requestedReviewers or nil, reviewers, seen)
  local edit_state = status and status.pr_edit or nil
  M._pr_overview.add_reviewers(edit_state and edit_state.pending_reviewers or nil, reviewers, seen)
  return reviewers
end

---@param pr DiffReviewGhPR
---@param status table?
---@return string
function M._pr_overview.pending_review_text(pr, status)
  local reviewers = M._pr_overview.pending_reviewers(pr, status)
  if #reviewers == 0 then return "" end
  local tokens = { M._pending_review_icon }
  for _, reviewer in ipairs(reviewers) do
    local token = M._pr_overview.reviewer_token(M._pr_overview.reviewer_login(reviewer))
    if pr and pr.isDraft and M._pr_overview.reviewer_is_code_owner(reviewer) then
      token = M._codeowner_review_icon .. token
    end
    tokens[#tokens + 1] = token
  end
  return table.concat(tokens, " ")
end

---@param pr DiffReviewGhPR?
---@return string
function M._pr_overview.milestone_title(pr)
  local milestone = pr and pr.milestone or nil
  if type(milestone) == "table" then return vim.trim(tostring(milestone.title or "")) end
  if type(milestone) == "string" then return vim.trim(milestone) end
  return ""
end

---@param pr DiffReviewGhPR?
---@return string
function M._pr_overview.milestone_text(pr)
  local title = M._pr_overview.milestone_title(pr)
  if title == "" then return "" end
  return ("%s %s"):format(M._milestone_icon, title)
end

---@param pr DiffReviewGhPR?
---@return string
function M._pr_overview.status_text(pr)
  return pr and pr.isDraft and "DRAFT" or "READY"
end

---@param check DiffReviewGhPRCheck
---@return string icon
---@return string hl_group
function M._pr_overview.check_icon(check)
  local state = tostring(check and check.state or ""):lower()
  if state == "success" then return "✓", "DiffReviewAddRange" end
  if state == "failure" or state == "error" or state == "timed_out" or state == "startup_failure" or state == "action_required" then
    return "✗", "DiffReviewDeleteRange"
  end
  if state == "cancelled" or state == "skipped" or state == "neutral" then return "!", "DiffReviewStatusFetching" end
  return "◷", "DiffReviewStatusFetching"
end

---@param check DiffReviewGhPRCheck
---@return string
function M._pr_overview.check_workflow_suffix(check)
  local workflow_name = vim.trim(tostring(check and check.workflow_name or ""))
  local name = vim.trim(tostring(check and check.name or ""))
  if workflow_name == "" or workflow_name == name then return "" end
  return workflow_name
end

---@param pr DiffReviewGhPR
---@param status table?
---@return DiffReviewStatusHeadLine[]
function M._pr_overview.check_head_lines(pr, status)
  local section_id = "pr-head-section:checks"
  local lines = {}
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = {
    segments = { { "Checks:", "DiffReviewStatusHeader" } },
    entry = { id = section_id, kind = "pr_head_section" },
  }
  if not (pr.repo and pr.repo ~= "") then
    lines[#lines + 1] = {
      segments = { { "No checks", "Comment" } },
      parent_id = section_id,
      entry = { id = section_id .. ":message", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if status and status.pr_checks_error then
    lines[#lines + 1] = {
      segments = { { tostring(status.pr_checks_error), "ErrorMsg" } },
      parent_id = section_id,
      entry = { id = section_id .. ":error", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if status and status.pr_checks_loading then
    lines[#lines + 1] = {
      segments = { { "...loading checks...", "DiffReviewStatusFetching" } },
      parent_id = section_id,
      entry = { id = section_id .. ":loading", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  local checks = status and status.pr_checks or nil
  if type(checks) ~= "table" then
    lines[#lines + 1] = {
      segments = { { "...loading checks...", "DiffReviewStatusFetching" } },
      parent_id = section_id,
      entry = { id = section_id .. ":loading", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if #checks == 0 then
    lines[#lines + 1] = {
      segments = { { "No checks", "Comment" } },
      parent_id = section_id,
      entry = { id = section_id .. ":message", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  for check_index, check in ipairs(checks) do
    local icon, icon_hl = M._pr_overview.check_icon(check)
    local workflow_suffix = M._pr_overview.check_workflow_suffix(check)
    local segments = {
      { icon .. " ", icon_hl },
      { tostring(check.name or "Check"), "DiffReviewStatusPath" },
    }
    if workflow_suffix ~= "" then
      segments[#segments + 1] = { " | ", "Comment" }
      segments[#segments + 1] = { workflow_suffix, "DiffReviewStatusRemote" }
    end
    lines[#lines + 1] = {
      segments = segments,
      parent_id = section_id,
      entry = {
        id = "pr:check:" .. tostring(check_index) .. ":" .. tostring(check.name or "check"),
        kind = "pr_check",
        pr_check = check,
        fold_target_id = section_id,
      },
    }
  end
  return lines
end

---@param pr DiffReviewGhPR
---@param status table?
---@return DiffReviewStatusHeadLine[]
local function status_pr_detail_head_lines(pr, status)
  local description_section_id = "pr-head-section:description"
  local title = pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))
  local lines = {
    { segments = { { "Title:  ", "DiffReviewStatusLabel" }, { title, "DiffReviewStatusPath" } } },
  }
  if pr.repo and pr.repo ~= "" then
    lines[#lines + 1] = { segments = { { "Repo:   ", "DiffReviewStatusLabel" }, { pr.repo, "DiffReviewStatusRemote" } } }
  end
  local head_subject, head_date = status_pr_head_subject(pr)
  lines[#lines + 1] = {
    segments = status_head_row(
      "Head",
      status_short_oid(pr.headRefOid),
      pr.headRefName or "",
      "DiffReviewStatusBranch",
      head_subject,
      head_date
    ),
  }
  if pr.url and pr.url ~= "" then
    lines[#lines + 1] = { segments = { { "URL:    ", "DiffReviewStatusLabel" }, { pr.url, "DiffReviewStatusPR" } } }
  end
  lines[#lines + 1] = {
    segments = {
      { "Release: ", "DiffReviewStatusLabel" },
      { M._pr_overview.milestone_text(pr), "DiffReviewStatusBranch" },
    },
  }
  local pending_review_text = M._pr_overview.pending_review_text(pr, status)
  lines[#lines + 1] = {
    segments = {
      { "Review: ", "DiffReviewStatusLabel" },
      { pending_review_text, "DiffReviewReviewPending" },
    },
  }
  lines[#lines + 1] = {
    segments = {
      { "Status: ", "DiffReviewStatusLabel" },
      { M._pr_overview.status_text(pr), pr.isDraft and "DiffReviewStatusFetching" or "DiffReviewStatusBranch" },
    },
  }
  lines[#lines + 1] = {
    segments = {
      { "Activity: ", "DiffReviewStatusLabel" },
      { M._pr_overview.activity_text(pr, status), "DiffReviewStatusDate" },
    },
  }
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = {
    segments = { { "Description:", "DiffReviewStatusHeader" } },
    entry = { id = description_section_id, kind = "pr_head_section" },
  }
  for line_index, line in ipairs(status_markdown_lines(pr.body)) do
    lines[#lines + 1] = {
      segments = { { line } },
      parent_id = description_section_id,
      entry = {
        id = description_section_id .. ":line:" .. tostring(line_index),
        kind = "pr_head_line",
        fold_target_id = description_section_id,
      },
    }
  end
  vim.list_extend(lines, M._pr_overview.check_head_lines(pr, status))
  return lines
end

---@param cwd string?
---@param repo string?
function M._status_enable_repo_completion(cwd, repo)
  if not (cwd and repo and repo ~= "") then return end
  local cache = require("github.repo_cache")
  for buf, state in pairs(M._status_states or {}) do
    if state and state.cwd == cwd and vim.api.nvim_buf_is_valid(buf) then
      cache.enable_user_completion(buf, repo)
    end
  end
end

---@param cwd string?
---@param repo string?
function M.github_load_repo_metadata(cwd, repo)
  local cache = require("github.repo_cache")
  local issue_index_ok, issue_index = pcall(require, "github.issue_index")
  if repo and repo ~= "" then
    M._status_enable_repo_completion(cwd, repo)
    cache.ensure_metadata(cwd, repo, function(done)
      gh.repo_contributors_async(cwd, repo, done)
    end, { remember_cwd = false })
    if issue_index_ok then issue_index.ensure_repo(cwd, repo, { manual = false }) end
    return
  end
  local cached_repo = cache.repo_for_cwd(cwd)
  if cached_repo then M._status_enable_repo_completion(cwd, cached_repo) end
  cache.ensure_metadata_for_cwd(cwd, function(done)
    gh.current_repo_async(cwd, function(result)
      if result and result.ok and result.repo then M._status_enable_repo_completion(cwd, result.repo) end
      done(result)
    end)
  end, function(resolved_repo, done)
    M._status_enable_repo_completion(cwd, resolved_repo)
    gh.repo_contributors_async(cwd, resolved_repo, done)
  end)
  if issue_index_ok then issue_index.ensure_current(cwd, { manual = false }) end
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

    local status = M._status
    local pr_state = status and status.pr
    local about_state = status and status.about
    local issues_state = M._status_issues.ensure_state(status, cwd)
    cb(status_build_head_lines(values, pr_state, about_state, issues_state), values)
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

local function status_add_highlight(line, start_col, end_col, hl_group, priority)
  M._status.highlights[#M._status.highlights + 1] = {
    line = line,
    start_col = start_col,
    end_col = end_col,
    hl_group = hl_group,
    priority = priority,
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
  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, target_line - 1, target_line, false, { text })
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering
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

---@param buf integer
---@return boolean
function M._status_issues.patch_line(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not status then return false end
  local head_line = M._status_issues.head_line(status.issues)
  M._status_issues.replace_head_line(status, head_line)
  return M._status_patch_head_line(buf, "issues", head_line)
end

---@param buf integer
---@return integer?
function M._status_issues.row(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if status and status.entries then
    for row, entry in pairs(status.entries) do
      if entry and entry.id == "issues" then return row end
    end
  end
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil end
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:match("^Issues:%s*") then return index end
  end
  return nil
end

---@param buf integer
---@return string
function M._status_issues.current_line(buf)
  local row = M._status_issues.row(buf)
  if not row then return "" end
  return vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
end

---@param buf integer
---@return integer[]
function M._status_issues.current_numbers(buf)
  return M._status_issues.parse_line(M._status_issues.current_line(buf))
end

---@param buf integer
---@return boolean
function M._status_issues.cursor_on_row(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local row = M._status_issues.row(buf)
  return row ~= nil and vim.api.nvim_win_get_cursor(0)[1] == row
end

---@param buf integer
function M._status_issues.refresh_modified(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.b[buf].diff_review_status_rendering then return end
  local status = M._status_states and M._status_states[buf] or M._status
  if not (status and status.view_kind == "status") then return end
  local issues_state = status.issues or M._status_issues.ensure_state(status, status.cwd)
  local saved_numbers = issues_state and issues_state.saved_numbers or {}
  vim.bo[buf].modified = not M._status_issues.equal(M._status_issues.current_numbers(buf), saved_numbers)
end

---@param buf integer
function M._status_issues.sync_modifiable(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.b[buf].diff_review_status_rendering then return end
  local status = M._status_states and M._status_states[buf] or M._status
  if not (status and status.view_kind == "status") then return end
  vim.bo[buf].modifiable = M._status_issues.cursor_on_row(buf)
end

---@param buf integer
function M._status_issues.save(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not (status and status.view_kind == "status" and status.cwd) then
    notify_error("GitStatus session save failed: not in a git status buffer", "GitStatus")
    return
  end
  local issues_state = status.issues or M._status_issues.ensure_state(status, status.cwd) or {}
  local numbers = M._status_issues.current_numbers(buf)
  local ok, err = M._status_issues.write(status.cwd, numbers, issues_state.data)
  if not ok then
    notify_error("GitStatus session save failed: " .. tostring(err), "GitStatus")
    vim.bo[buf].modified = true
    M._status_issues.sync_modifiable(buf)
    return
  end
  issues_state.cwd = status.cwd
  issues_state.numbers = vim.deepcopy(numbers)
  issues_state.saved_numbers = vim.deepcopy(numbers)
  issues_state.data = issues_state.data or {}
  issues_state.data.issues = vim.deepcopy(numbers)
  status.issues = issues_state
  M._status_issues.patch_line(buf)
  vim.bo[buf].modified = false
  M._status_issues.sync_modifiable(buf)
end

---@param buf integer
function M._status_issues.attach(buf)
  if vim.b[buf].diff_review_status_issues_attached then return end
  vim.b[buf].diff_review_status_issues_attached = true
  vim.bo[buf].buftype = "acwrite"
  local group = vim.api.nvim_create_augroup("DiffReviewStatusIssues" .. tostring(buf), { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "CursorMovedI", "ModeChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._status_issues.sync_modifiable(buf)
    end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "InsertLeave" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._status_issues.refresh_modified(buf)
      M._status_issues.sync_modifiable(buf)
    end,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buf,
    callback = function()
      M._status_issues.save(buf)
    end,
  })
  vim.keymap.set("i", "<CR>", function()
    if M._status_issues.cursor_on_row(buf) then return "" end
    return "\r"
  end, { buffer = buf, expr = true, desc = "Keep GitStatus issue list on one line" })
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
  local diff_lines = nil
  for _, chunk in ipairs(row) do
    if chunk.meta and chunk.meta.diff then
      diff_line = chunk.meta.diff
    end
    if chunk.meta and chunk.meta.diff_lines then
      diff_lines = chunk.meta.diff_lines
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
  local content_length = nil
  if row.diff_review_bg_hl then
    line_text, content_length = M._diff_pad_highlighted_line(line_text, M._status.buf)
  end

  local line_entry = entry
  if row.diff_review_boundary or row.diff_review_context_padding then
    line_entry = nil
  end
  if diff_line and entry and (row.diff_review_boundary or row.diff_review_context_padding) then
    line_entry = {
      kind = "context_line",
      file = entry.file,
      diff_line = diff_line,
    }
    if diff_lines then line_entry.diff_lines = diff_lines end
  elseif diff_line and entry then
    local diff_entry = { diff_line = diff_line }
    if diff_lines then diff_entry.diff_lines = diff_lines end
    local inline_jump_spans = {}
    for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
      inline_jump_spans[#inline_jump_spans + 1] = {
        start_col = indent + inline_highlight.start_col,
        end_col = indent + inline_highlight.end_col,
        hl_group = inline_highlight.hl_group,
        kind = inline_highlight.kind,
      }
    end
    if #inline_jump_spans > 0 then diff_entry.inline_jump_spans = inline_jump_spans end
    line_entry = vim.tbl_extend("force", entry, diff_entry)
  end
  local line = status_add_line(line_text, line_entry)
  if row.diff_review_bg_hl then
    M._diff_line_content_lengths = M._diff_line_content_lengths or {}
    M._diff_line_content_lengths[M._status.buf] = M._diff_line_content_lengths[M._status.buf] or {}
    M._diff_line_content_lengths[M._status.buf][line] = content_length or #line_text
    status_add_extmark(line, 0, {
      end_col = #line_text,
      hl_group = row.diff_review_bg_hl,
      priority = 60,
    })
  end
  if row.diff_review_boundary then
    M._status.boundary_lines = M._status.boundary_lines or {}
    M._status.boundary_lines[line] = true
  end
  for _, highlight in ipairs(row_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
  for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
    status_add_highlight(
      line,
      indent + inline_highlight.start_col,
      indent + inline_highlight.end_col,
      inline_highlight.hl_group,
      inline_highlight.priority or 110
    )
  end
  for _, extmark in ipairs(row_extmarks) do
    status_add_extmark(line, extmark.col, extmark.opts)
  end
  -- Review view: emit any draft comments anchored on this diff row as real
  -- (navigable, editable) lines right below it.
  if M._status.review_after_row and diff_line and entry and not (row.diff_review_boundary or row.diff_review_context_padding) then
    local emitted = {}
    local targets = diff_lines or { diff_line }
    for _, target_line in ipairs(targets) do
      local key = ("%s\0%s\0%s"):format(tostring(target_line.file), tostring(target_line.side), tostring(target_line.line))
      if not emitted[key] then
        emitted[key] = true
        M._status.review_after_row(target_line, indent)
      end
    end
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

---@class DiffReviewDiffFileStatus
---@field relpath string
---@field status string
---@field original_relpath? string

---@class DiffReviewStatusDiffProvider
---@field section_name string
---@field default_status string
---@field files? DiffReviewStatusDiffProviderFile[]

---@param diff_text string
---@return table<string, DiffReviewDiffFileStatus>
function M._diff_file_statuses(diff_text)
  local statuses = {} ---@type table<string, DiffReviewDiffFileStatus>
  local current = nil ---@type { old_path?: string, new_path?: string, status?: string }?

  local function clean_path(value)
    if not value or value == "" or value == "/dev/null" then return nil end
    local path = value
    local tab_index = path:find("\t", 1, true)
    if tab_index then path = path:sub(1, tab_index - 1) end
    if path:sub(1, 2) == "a/" or path:sub(1, 2) == "b/" then
      path = path:sub(3)
    end
    return path ~= "" and path or nil
  end

  local function flush_current()
    if not current then return end
    local relpath = current.new_path or current.old_path
    if not relpath then
      current = nil
      return
    end

    local status = current.status
    if not status then
      if current.old_path == nil and current.new_path ~= nil then
        status = "A"
      elseif current.old_path ~= nil and current.new_path == nil then
        status = "D"
      else
        status = "M"
      end
    end

    statuses[relpath] = {
      relpath = relpath,
      status = status,
      original_relpath = current.old_path and current.old_path ~= relpath and current.old_path or nil,
    }
    current = nil
  end

  for _, line in ipairs(vim.split(diff_text or "", "\n", { plain = true })) do
    if line:find("^diff %-%-git ") then
      flush_current()
      current = {}
    elseif current then
      if line:find("^new file mode") then
        current.status = "A"
      elseif line:find("^deleted file mode") then
        current.status = "D"
      elseif line:find("^rename from ") then
        current.status = "R"
        current.old_path = clean_path(line:sub(13))
      elseif line:find("^rename to ") then
        current.status = "R"
        current.new_path = clean_path(line:sub(11))
      elseif line:find("^copy from ") then
        current.status = "C"
        current.old_path = clean_path(line:sub(11))
      elseif line:find("^copy to ") then
        current.status = "C"
        current.new_path = clean_path(line:sub(9))
      elseif line:find("^%-%-%- ") then
        current.old_path = clean_path(line:sub(5))
      elseif line:find("^%+%+%+ ") then
        current.new_path = clean_path(line:sub(5))
      end
    end
  end
  flush_current()

  return statuses
end

---@param cwd string
---@param provider DiffReviewStatusDiffProvider
---@param diff_text? string
---@return DiffReviewStatusFile[]
local function status_files_from_diff_provider(cwd, provider, diff_text)
  local files_by_name = {} ---@type table<string, DiffReviewStatusFile>
  local files_with_provider_stats = {} ---@type table<string, boolean>
  local diff_file_statuses = M._diff_file_statuses(diff_text or "")
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
    elseif not files_with_provider_stats[filename] then
      local diff_file_status = diff_file_statuses[relpath]
      if diff_file_status then
        file.status = diff_file_status.status or file.status
        file.git_status = diff_file_status.status or file.git_status
        file.original_relpath = diff_file_status.original_relpath or file.original_relpath
      end
    end
    return file
  end

  for _, provider_file in ipairs(provider.files or {}) do
    if provider_file.path and provider_file.path ~= "" then ensure_file(provider_file.path, provider_file) end
  end

  for relpath in pairs(diff_file_statuses) do
    ensure_file(relpath, nil)
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
---@return string icon
---@return string status_hl
function M._pr_overview.review_state_style(state)
  state = tostring(state or "")
  if state == "APPROVED" then return "✓", "DiffReviewAddRange" end
  if state == "CHANGES_REQUESTED" then return "✗", "DiffReviewDeleteRange" end
  return M._comment_icon, "DiffReviewReviewComment"
end

---@param state string?
---@return string
function M._pr_overview.review_action(state)
  state = tostring(state or "")
  if state == "APPROVED" then return "approved" end
  if state == "CHANGES_REQUESTED" then return "requested changes" end
  if state == "DISMISSED" then return "dismissed" end
  if state == "COMMENTED" then return "commented" end
  return "reviewed"
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M._pr_overview.review_author(review)
  return tostring(review.user or "unknown")
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M._pr_overview.review_date(review)
  return M._pr_overview.comment_datetime(review.submitted_at or review.updated_at or review.created_at)
end

---@param reviews DiffReviewGhSubmittedReview[]?
---@return { author_width: integer, date_width: integer }
function M._pr_overview.review_summary_alignment(reviews)
  local alignment = { author_width = 0, date_width = 0 }
  for _, review in ipairs(reviews or {}) do
    alignment.author_width = math.max(alignment.author_width, vim.fn.strdisplaywidth(M._pr_overview.review_author(review)))
    alignment.date_width = math.max(alignment.date_width, vim.fn.strdisplaywidth(M._pr_overview.review_date(review)))
  end
  return alignment
end

---@param review DiffReviewGhSubmittedReview
---@return string[] tail_parts
function M._pr_overview.review_summary_tail_parts(review)
  local tail_parts = {}
  local preview = M._pr_overview.comment_preview(review.body or "")
  if preview ~= "" then tail_parts[#tail_parts + 1] = preview end
  return tail_parts
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
---@return string
function M._pr_overview.review_summary_line(review, alignment)
  local icon = M._pr_overview.review_state_style(review.state)
  alignment = alignment or {}
  local tail_parts = M._pr_overview.review_summary_tail_parts(review)
  local line = ("%s %s"):format(
    icon,
    M._pr_overview.pad_display_right(M._pr_overview.review_author(review), alignment.author_width)
  )
  local date_text = M._pr_overview.review_date(review)
  if date_text ~= "" then
    line = line .. " " .. M._pr_overview.pad_display_right(date_text, #tail_parts > 0 and alignment.date_width or 0)
  end
  if #tail_parts > 0 then line = line .. "  " .. table.concat(tail_parts, "  ") end
  return line:gsub("%s+$", "")
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
---@return table[]
function M._pr_overview.review_summary_segments(review, alignment)
  local icon, status_hl = M._pr_overview.review_state_style(review.state)
  alignment = alignment or {}
  local tail_parts = M._pr_overview.review_summary_tail_parts(review)
  local segments = {
    { icon .. " ", status_hl },
    {
      M._pr_overview.pad_display_right(M._pr_overview.review_author(review), alignment.author_width),
      "DiffReviewReviewComment",
    },
  }
  local date_text = M._pr_overview.review_date(review)
  if date_text ~= "" then
    segments[#segments + 1] = { " ", "DiffReviewReviewComment" }
    segments[#segments + 1] = {
      M._pr_overview.pad_display_right(date_text, #tail_parts > 0 and alignment.date_width or 0),
      "DiffReviewStatusDate",
    }
  end
  for _, tail in ipairs(tail_parts) do
    segments[#segments + 1] = { "  ", "DiffReviewReviewComment" }
    segments[#segments + 1] = { tail, "DiffReviewReviewComment" }
  end
  return segments
end

---@param comments? DiffReviewGhPRCommentsResult
---@return DiffReviewStatusSection?
function M._pr_overview.reviews_section(comments)
  local reviews = comments and comments.reviews or nil
  if type(reviews) ~= "table" or #reviews == 0 then return nil end
  local visible_reviews = {}
  for _, review in ipairs(reviews) do
    local is_empty_single_comment_shell = tostring(review.state or "") == "COMMENTED"
      and vim.trim(tostring(review.body or "")) == ""
      and #(review.comments or {}) <= 1
    if not is_empty_single_comment_shell then visible_reviews[#visible_reviews + 1] = review end
  end
  if #visible_reviews == 0 then return nil end
  return {
    name = "pr:reviews",
    title = "Reviews",
    default_folded = false,
    files = {},
    files_by_name = {},
    reviews = visible_reviews,
  }
end

---@param comments? DiffReviewGhPRCommentsResult
---@param local_comments? table[]
---@return DiffReviewStatusSection?
function M._pr_overview.issue_comments_section(comments, local_comments)
  local remote_comments = comments and comments.issue_comments or nil
  if type(remote_comments) ~= "table" then remote_comments = {} end
  local local_keys = {}
  for _, comment in ipairs(local_comments or {}) do
    local key = M._pr_overview.comment_identity_key(comment)
    if key then local_keys[key] = true end
  end

  local issue_comments = {}
  for _, comment in ipairs(remote_comments) do
    local key = M._pr_overview.comment_identity_key(comment)
    if not (key and local_keys[key]) then issue_comments[#issue_comments + 1] = comment end
  end
  for _, comment in ipairs(local_comments or {}) do
    issue_comments[#issue_comments + 1] = comment
  end
  if #issue_comments == 0 then return nil end

  return {
    name = "pr:comments",
    title = "Comments",
    default_folded = false,
    files = {},
    files_by_name = {},
    issue_comments = issue_comments,
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

M._pr_overview.review_context_radius = 2

---@class DiffReviewReviewContextRecord
---@field raw string
---@field prefix string
---@field old_before integer
---@field new_before integer
---@field old_line? integer
---@field new_line? integer
---@field position integer

---@class DiffReviewReviewContextHunk
---@field context string
---@field records DiffReviewReviewContextRecord[]

---@param hunk DiffReviewHunk
---@return DiffReviewReviewContextHunk?
function M._pr_overview.review_context_hunk(hunk)
  local header = nil
  local body = {}
  for _, line in ipairs(vim.split(tostring(hunk.diff or ""), "\n", { plain = true })) do
    if not header and line:match("^@@ ") then
      header = line
    elseif header then
      body[#body + 1] = line
    end
  end
  if not header then return nil end

  local old_line, _, new_line, _, context = parse_hunk_header(header)
  local records = {}
  for position, raw in ipairs(body) do
    local prefix = raw:sub(1, 1)
    if prefix == " " or prefix == "-" or prefix == "+" then
      local record = {
        raw = raw,
        prefix = prefix,
        old_before = old_line,
        new_before = new_line,
        position = position,
      }
      if prefix == " " then
        record.old_line = old_line
        record.new_line = new_line
        old_line = old_line + 1
        new_line = new_line + 1
      elseif prefix == "-" then
        record.old_line = old_line
        old_line = old_line + 1
      elseif prefix == "+" then
        record.new_line = new_line
        new_line = new_line + 1
      end
      records[#records + 1] = record
    end
  end
  return { context = context, records = records }
end

---@param context_hunk DiffReviewReviewContextHunk
---@param comment table
---@param radius integer
---@return table?
function M._pr_overview.review_comment_context_window(context_hunk, comment, radius)
  local target_line = tonumber(comment and comment.line)
  if not target_line then return nil end
  local start_line = tonumber(comment.start_line) or target_line
  if start_line > target_line then start_line, target_line = target_line, start_line end
  start_line = math.max(0, start_line - radius)
  target_line = target_line + radius

  local side = M._pr_overview.comment_side(comment)
  local start_index, end_index
  for index, record in ipairs(context_hunk.records or {}) do
    local line = side == "LEFT" and record.old_line or record.new_line
    if line and line >= start_line and line <= target_line then
      start_index = start_index or index
      end_index = index
    end
  end
  if not start_index and comment.position then
    local position = tonumber(comment.position)
    for index, record in ipairs(context_hunk.records or {}) do
      if position and record.position == position then
        start_index = math.max(1, index - radius)
        end_index = math.min(#context_hunk.records, index + radius)
        break
      end
    end
  end
  if not start_index then return nil end
  return { start_index = start_index, end_index = end_index }
end

---@param windows table[]
---@return table[]
function M._pr_overview.merge_context_windows(windows)
  table.sort(windows, function(left, right)
    return (left.start_index or 0) < (right.start_index or 0)
  end)
  local merged = {}
  for _, window in ipairs(windows) do
    local previous = merged[#merged]
    if previous and (window.start_index or 0) <= (previous.end_index or 0) + 1 then
      previous.end_index = math.max(previous.end_index or 0, window.end_index or 0)
    else
      merged[#merged + 1] = {
        start_index = window.start_index,
        end_index = window.end_index,
      }
    end
  end
  return merged
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param context_hunk DiffReviewReviewContextHunk
---@param window table
---@return DiffReviewHunk?
function M._pr_overview.context_window_hunk(file, hunk, context_hunk, window)
  local records = context_hunk.records or {}
  local first = records[window.start_index]
  if not first then return nil end

  local old_count, new_count = 0, 0
  local added, removed = 0, 0
  local body = {}
  for index = window.start_index, window.end_index do
    local record = records[index]
    if record then
      body[#body + 1] = record.raw
      if record.prefix ~= "+" then old_count = old_count + 1 end
      if record.prefix ~= "-" then new_count = new_count + 1 end
      if record.prefix == "+" then
        added = added + 1
      elseif record.prefix == "-" then
        removed = removed + 1
      end
    end
  end
  if #body == 0 then return nil end

  local suffix = context_hunk.context ~= "" and (" " .. context_hunk.context) or ""
  local lines = {
    ("@@ -%d,%d +%d,%d @@%s"):format(first.old_before or 0, old_count, first.new_before or 0, new_count, suffix),
  }
  vim.list_extend(lines, body)
  return {
    filename = file.filename,
    section_name = file.section_name,
    pos = first.new_before and first.new_before > 0 and first.new_before or first.old_before,
    diff = table.concat(lines, "\n"),
    staged = hunk.staged,
    context_text = hunk.context_text,
    added = added,
    removed = removed,
  }
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param comments table[]
---@return DiffReviewHunk[]
function M._pr_overview.review_context_hunks_for_hunk(file, hunk, comments)
  local context_hunk = M._pr_overview.review_context_hunk(hunk)
  if not context_hunk then return {} end
  local windows = {}
  for _, comment in ipairs(comments or {}) do
    if M._pr_overview.hunk_contains_comment(hunk, comment) then
      local window = M._pr_overview.review_comment_context_window(context_hunk, comment, M._pr_overview.review_context_radius)
      if window then windows[#windows + 1] = window end
    end
  end
  local hunks = {}
  for _, window in ipairs(M._pr_overview.merge_context_windows(windows)) do
    local context_hunk_copy = M._pr_overview.context_window_hunk(file, hunk, context_hunk, window)
    if context_hunk_copy then hunks[#hunks + 1] = context_hunk_copy end
  end
  return hunks
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
        vim.list_extend(hunks, M._pr_overview.review_context_hunks_for_hunk(file, hunk, file_comments))
      end
      local copy = M._section_builder.file_with_hunks(file, hunks, file.section_name)
      copy.added = file.added
      copy.removed = file.removed
      copy.pr_review_comments = file_comments
      if #copy.hunks > 0 then result[#result + 1] = copy end
    end
  end
  return result
end

---@param raw_commit DiffReviewGhPRCommit|table
---@param branch? string
---@return DiffReviewStatusCommit?
function M._pr_overview.status_commit_from_pr_commit(raw_commit, branch)
  if type(raw_commit) ~= "table" then return nil end
  local oid = vim.trim(tostring(raw_commit.oid or raw_commit.sha or raw_commit.id or ""))
  if oid == "" then return nil end
  local cache = M._status and M._status.commit_file_cache and M._status.commit_file_cache[oid] or nil
  local short_oid = vim.trim(tostring(raw_commit.shortOid or raw_commit.short_oid or raw_commit.abbreviatedOid or ""))
  if short_oid == "" then short_oid = status_short_oid(oid) end
  return {
    oid = oid,
    short_oid = short_oid,
    branch = branch,
    subject = tostring(raw_commit.messageHeadline or raw_commit.subject or raw_commit.message or ""),
    committed_at = raw_commit.committedDate or raw_commit.committed_date or raw_commit.committed_at,
    authored_at = raw_commit.authoredDate or raw_commit.authored_date or raw_commit.authored_at,
    files = cache and cache.files or nil,
    files_loaded = cache and cache.files_loaded or false,
    files_loading = cache and cache.files_loading or false,
    files_error = cache and cache.files_error or nil,
  }
end

---@param pr DiffReviewGhPR
---@return DiffReviewStatusSection?
function M._pr_overview.commits_section(pr)
  local raw_commits = pr.commits or {}
  if type(raw_commits) ~= "table" or #raw_commits == 0 then return nil end
  local commits = {}
  for raw_index = #raw_commits, 1, -1 do
    local branch = #commits == 0 and pr.headRefName or nil
    local commit = M._pr_overview.status_commit_from_pr_commit(raw_commits[raw_index], branch)
    if commit then commits[#commits + 1] = commit end
  end
  if #commits == 0 then return nil end
  return {
    name = "pr_commits",
    title = "Recent Commits",
    default_folded = true,
    files = {},
    files_by_name = {},
    commits = commits,
  }
end

---@param cwd string
---@param pr DiffReviewGhPR
---@param diff_text? string
---@param comments? DiffReviewGhPRCommentsResult
---@param local_comments? table[]
---@return DiffReviewStatusSection[]
local function status_pr_sections(cwd, pr, diff_text, comments, local_comments, local_issue_comments)
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
  local issue_comments_section = M._pr_overview.issue_comments_section(comments, local_issue_comments)
  if issue_comments_section then sections[#sections + 1] = issue_comments_section end
  vim.list_extend(sections, change_sections)
  local commits_section = M._pr_overview.commits_section(pr)
  if commits_section then sections[#sections + 1] = commits_section end
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
    local oid, short_oid, committed_at, subject = line:match("^([^\t]+)\t([^\t]+)\t([^\t]*)\t(.*)$")
    if not oid then
      oid, short_oid, subject = line:match("^([^\t]+)\t([^\t]+)\t(.*)$")
      committed_at = nil
    end
    if oid and oid ~= "" then
      local cache = M._status and M._status.commit_file_cache and M._status.commit_file_cache[oid] or nil
      commits[#commits + 1] = {
        oid = oid,
        short_oid = short_oid or oid:sub(1, 7),
        branch = index == 1 and spec.branch or nil,
        subject = subject or "",
        committed_at = committed_at ~= "" and committed_at or nil,
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

  local command = { "git", "-C", cwd, "log", "--no-color", "--format=%H%x09%h%x09%cI%x09%s" }
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

---@param hunk DiffReviewHunk
---@return integer? first_line
---@return integer? last_line
function M._status_hunk_changed_current_range(hunk)
  local first_line = nil
  local last_line = nil
  for _, block in ipairs(parse_unified_diff(tostring(hunk.diff or ""))) do
    for _, parsed_hunk in ipairs(block.hunks or {}) do
      parsed_hunk = parse_hunk_body(parsed_hunk)
      local line_by_position = M._hunk_current_line_by_position(parsed_hunk)
      for _, parsed_line in ipairs(parsed_hunk.lines or {}) do
        if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
          local current_line = M._hunk_parsed_line_current_line(parsed_line, line_by_position)
          if current_line then
            first_line = math.min(first_line or current_line, current_line)
            last_line = math.max(last_line or current_line, current_line)
          end
        end
      end
    end
  end
  if first_line then return first_line, last_line or first_line end
  if hunk.pos then return hunk.pos, hunk.pos end
  return nil, nil
end

---@param hunk DiffReviewHunk
---@return integer? first_line
---@return integer? last_line
function M._status_hunk_virtual_display_range(hunk)
  local first_line, last_line = M._status_hunk_changed_current_range(hunk)
  if not (first_line and last_line) then return nil, nil end
  local padding_limit = M._hunk_context_padding_limit()
  return math.max(1, first_line - padding_limit), last_line + padding_limit
end

---@param diff_text string?
---@return string[] header_lines
---@return string[][] hunk_sections
function M._status_hunk_diff_parts(diff_text)
  local header_lines = {}
  local hunk_sections = {}
  local current_section = nil ---@type string[]?
  for _, line in ipairs(vim.split(tostring(diff_text or ""), "\n", { plain = true })) do
    if line:match("^@@ ") then
      current_section = { line }
      hunk_sections[#hunk_sections + 1] = current_section
    elseif current_section then
      current_section[#current_section + 1] = line
    else
      header_lines[#header_lines + 1] = line
    end
  end
  return header_lines, hunk_sections
end

---@param left DiffReviewHunk?
---@param right DiffReviewHunk?
---@return boolean
function M._status_hunks_should_display_together(left, right)
  if not (left and right) then return false end
  if left.file ~= right.file then return false end
  if left.staged ~= right.staged then return false end
  if left.section_name ~= right.section_name then return false end
  local _, left_display_end = M._status_hunk_virtual_display_range(left)
  local right_display_start = M._status_hunk_virtual_display_range(right)
  if not (left_display_end and right_display_start) then return false end
  return right_display_start <= left_display_end + 1
end

---@param hunks DiffReviewHunk[]
---@return DiffReviewHunk
function M._status_combine_display_hunks(hunks)
  if #hunks == 1 then return hunks[1] end
  local header_lines = M._status_hunk_diff_parts(hunks[1].diff)
  local combined_lines = vim.deepcopy(header_lines)
  local added_count = 0
  local removed_count = 0
  for _, hunk in ipairs(hunks) do
    local _, hunk_sections = M._status_hunk_diff_parts(hunk.diff)
    for _, section in ipairs(hunk_sections) do
      vim.list_extend(combined_lines, section)
    end
    added_count = added_count + (hunk.added or 0)
    removed_count = removed_count + (hunk.removed or 0)
  end
  local combined_hunk = vim.deepcopy(hunks[1])
  combined_hunk.diff = table.concat(combined_lines, "\n")
  combined_hunk.added = added_count
  combined_hunk.removed = removed_count
  combined_hunk.raw_hunks = vim.deepcopy(hunks)
  return combined_hunk
end

---@param hunks DiffReviewHunk[]
---@return DiffReviewHunk[]
function M._status_display_hunks(hunks)
  local display_hunks = {}
  local current_group = {} ---@type DiffReviewHunk[]
  local function flush_group()
    if #current_group == 0 then return end
    display_hunks[#display_hunks + 1] = M._status_combine_display_hunks(current_group)
    current_group = {}
  end

  for _, hunk in ipairs(hunks or {}) do
    if #current_group == 0 or M._status_hunks_should_display_together(current_group[#current_group], hunk) then
      current_group[#current_group + 1] = hunk
    else
      flush_group()
      current_group[#current_group + 1] = hunk
    end
  end
  flush_group()
  return display_hunks
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
  local context_line = M._status_hunk_context_line(hunk) or hunk.pos
  local previous_context_line = previous_hunk and M._status_hunk_context_line(previous_hunk) or nil
  local next_context_line = next_hunk and M._status_hunk_context_line(next_hunk) or nil
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
    context_line,
    "status-neighbor:" .. hunk_key .. ":current",
    rerender_with_context
  )
  local previous_context = previous_hunk and cached_hunk_context(
    file.filename,
    previous_context_line or previous_hunk.pos,
    "status-neighbor:" .. hunk_key .. ":previous",
    rerender_with_context
  ) or nil
  local next_context = next_hunk and cached_hunk_context(
    file.filename,
    next_context_line or next_hunk.pos,
    "status-neighbor:" .. hunk_key .. ":next",
    rerender_with_context
  ) or nil
  local suppress_start_boundary = same_hunk_context_scope(previous_context, current_context)
  local suppress_end_boundary = same_hunk_context_scope(current_context, next_context)
  local current_ancestor_key = M._hunk_context_ancestor_key(current_context)
  local suppress_ancestor_start = current_ancestor_key ~= nil and M._same_hunk_ancestor_scope(previous_context, current_context)
  local suppress_ancestor_end = current_ancestor_key ~= nil and M._same_hunk_ancestor_scope(current_context, next_context)
  local suppress_start_boundary_keys = suppress_ancestor_start and { [current_ancestor_key] = true } or nil
  local suppress_end_boundary_keys = suppress_ancestor_end and { [current_ancestor_key] = true } or nil
  local syntax_source = (entry_kind == nil or entry_kind == "hunk") and "file" or "diff"
  local syntax_diff_text = nil
  if syntax_source == "file" then
    syntax_diff_text = M._status_file_syntax_diff_text(file)
  end
  rows_key = ("%s:%s:%s:%s:%s:%s:%s"):format(
    hunk_key,
    hunk.staged and "staged" or "unstaged",
    suppress_start_boundary and "no-start" or "start",
    suppress_end_boundary and "no-end" or "end",
    suppress_ancestor_start and "no-ancestor-start" or "ancestor-start",
    suppress_ancestor_end and "no-ancestor-end" or "ancestor-end",
    syntax_source .. ":compact:" .. vim.fn.sha256(syntax_diff_text or hunk.diff or "")
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
        context_line = context_line,
        boundary_context = true,
        suppress_start_boundary = suppress_start_boundary,
        suppress_end_boundary = suppress_end_boundary,
        suppress_start_boundary_keys = suppress_start_boundary_keys,
        suppress_end_boundary_keys = suppress_end_boundary_keys,
        syntax_source = syntax_source,
        syntax_diff_text = syntax_diff_text,
        compact_replacements = true,
      }
    )
    if ok then
      rows = built_rows
      if not rows.diff_review_syntax_pending and not rows.diff_review_context_pending then
        M._status.fancy_rows[rows_key] = rows
      end
    end
  end
  if not rows then
    local ts_context = current_context or cached_hunk_context(file.filename, context_line, "status-fallback:" .. hunk_key, rerender_with_context)
    local header = ("%s@@ +%d -%d"):format(string.rep(" ", status_hunk_indent), hunk.added or 0, hunk.removed or 0)
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
    end
    status_add_line(header, entry, hunk_folded and "DiffReviewActiveHunkHeader" or "DiffReviewHunkHeader")
    if visible_hunk_lines and node_start and node_end and end_text then
      if not suppress_start_boundary and not visible_hunk_lines[start_text] then
        status_add_fancy_row(hunk_boundary_row(start_text, ts_context.start_segments, node_start), nil, status_hunk_indent)
        if node_start ~= node_end then
          status_add_fancy_row(hunk_boundary_ellipsis_row(start_text), nil, status_hunk_indent)
        end
      end
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
    if row then status_add_fancy_row(row, entry, status_hunk_indent) end
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
  local check = entry.pr_check
  if check and check.url and check.url ~= "" then return check.url end
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
---@param opts? { force_open?: boolean, default_open?: boolean }
local function status_render_file(file, entry_kind, hunk_entry_kind, file_key_override, hunk_key_builder, opts)
  opts = opts or {}
  local file_key = file_key_override or status_file_key(file.section_name, file.filename)
  local default_folded = not opts.default_open
  local file_folded = (not opts.force_open) and status_folded(file_key, default_folded)
  local stats, stat_segments = M._status_file_stat_text_and_segments(file)
  local change_label, change_label_hl = M._status_file_change_label(file)
  local change_label_width = #"Modified"
  local padded_change_label = change_label .. string.rep(" ", math.max(0, change_label_width - #change_label))
  local line = ("%s%s %s %s"):format(string.rep(" ", status_file_indent), padded_change_label, file.relpath, stats)
  local entry = { id = file_key, kind = entry_kind or "file", file = file }
  local line_number = status_add_line(line, entry)
  local label_start = status_file_indent
  local label_end = label_start + #change_label
  local path_start = label_start + #padded_change_label + 1
  local stats_start = #line - #stats
  status_add_highlight(line_number, label_start, label_end, change_label_hl)
  status_add_highlight(line_number, path_start, stats_start - 1, "DiffReviewStatusPath")
  for _, stat_segment in ipairs(stat_segments) do
    status_add_highlight(
      line_number,
      stats_start + stat_segment.start_col,
      stats_start + stat_segment.end_col,
      stat_segment.hl_group
    )
  end

  if file_folded then return end

  local hunks = status_diff_hunks_for_file(file)
  if #hunks == 0 then
    status_add_line(string.rep(" ", status_hunk_indent) .. "No textual diff", entry, "Comment")
    return
  end
  local display_hunks = M._status_display_hunks(hunks)
  for hunk_index, hunk in ipairs(display_hunks) do
    local hunk_key = hunk_key_builder and hunk_key_builder(hunk) or nil
    status_render_hunk(file, hunk, display_hunks[hunk_index - 1], display_hunks[hunk_index + 1], hunk_entry_kind, hunk_key)
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
        { default_open = true }
      )
    end
  end)
  status.review_after_row = previous_hook
  if not ok then error(err) end
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
function M._pr_overview.render_review(review, alignment)
  local review_key = M._pr_overview.review_key(review)
  local entry = { id = review_key, kind = "pr_review", pr_review = review }
  status_add_segment_line(M._pr_overview.review_summary_segments(review, alignment), entry)
  if status_folded(review_key, true) then return end
  M._pr_overview.render_review_context(review)
end

---@param comment table
---@param index integer
---@param alignment? { author_width?: integer, date_width?: integer }
function M._pr_overview.render_issue_comment(comment, index, alignment)
  for _, row in ipairs(M._pr_overview.issue_comment_rows({ comment }, { alignment = alignment, start_index = index })) do
    local line_number = status_add_line(row.text, row.entry, row.line_hl_group)
    for _, highlight in ipairs(row.highlights or {}) do
      status_add_highlight(line_number, highlight.start_col, highlight.end_col, highlight.hl_group)
    end
  end
end

---@param comments table[]
---@param opts? { alignment?: { author_width?: integer, date_width?: integer }, start_index?: integer, is_folded?: fun(entry_id: string, default: boolean, entry: table): boolean }
---@return table[]
function M._pr_overview.issue_comment_rows(comments, opts)
  opts = opts or {}
  comments = comments or {}
  local row_opts = M._pr_overview.issue_comment_row_options()
  row_opts.alignment = opts.alignment or M._pr_overview.issue_comment_alignment(comments)
  row_opts.start_index = opts.start_index
  row_opts.is_folded = opts.is_folded or function(entry_id, default)
    return status_folded(entry_id, default)
  end
  return M._comment_rows.rows(comments, row_opts)
end

---@param comments table[]
function M._pr_overview.render_issue_comments(comments)
  local alignment = M._pr_overview.issue_comment_alignment(comments)
  for index, comment in ipairs(comments or {}) do
    M._pr_overview.render_issue_comment(comment, index, alignment)
  end
end

---@param commit DiffReviewStatusCommit
---@return string?
local function status_commit_relative_date(commit)
  local value = commit.committed_at or commit.authored_at
  local relative = M._datetime.relative(value, { yesterday = false })
  if relative == "" then return nil end
  return relative
end

---@param subject string?
---@return integer? type_end_col zero-based exclusive byte column within subject
function M._status_conventional_commit_type_end(subject)
  subject = tostring(subject or "")
  local commit_type = subject:match("^([a-z][a-z0-9-]*)")
  if not commit_type then return nil end

  local cursor = #commit_type + 1
  local next_char = subject:sub(cursor, cursor)
  if next_char == "!" then
    return subject:sub(cursor + 1, cursor + 2) == ": " and #commit_type or nil
  end
  if next_char == ":" then
    return subject:sub(cursor + 1, cursor + 1) == " " and #commit_type or nil
  end
  if next_char ~= "(" then return nil end

  local close_scope = subject:find(")", cursor + 1, true)
  if not close_scope or close_scope == cursor + 1 then return nil end
  local scope = subject:sub(cursor + 1, close_scope - 1)
  if scope:find("[%c%s]") then return nil end
  local after_scope = subject:sub(close_scope + 1, close_scope + 1)
  if after_scope == "!" then
    return subject:sub(close_scope + 2, close_scope + 3) == ": " and #commit_type or nil
  end
  if after_scope == ":" then
    return subject:sub(close_scope + 2, close_scope + 2) == " " and #commit_type or nil
  end
  return nil
end

---@param subject string
---@param hl_group string
---@return table[]
function M._status_conventional_commit_subject_segments(subject, hl_group)
  local conventional_type_end = M._status_conventional_commit_type_end(subject)
  if not conventional_type_end then return { { subject, hl_group } } end
  return {
    { subject:sub(1, conventional_type_end), "DiffReviewStatusCommitType" },
    { subject:sub(conventional_type_end + 1), hl_group },
  }
end

---@param commit DiffReviewStatusCommit
---@param date_width integer
local function status_render_commit(commit, date_width)
  local commit_key = status_commit_key(commit.oid)
  local commit_folded = status_folded(commit_key, true)
  local line = commit.short_oid
  local date_text = status_commit_relative_date(commit)
  local date_start_col = nil
  if date_text then
    line = line .. "  "
    date_start_col = #line
    line = line .. date_text
    if date_width > #date_text then line = line .. string.rep(" ", date_width - #date_text) end
  end
  local suffix = commit.subject or ""
  local suffix_start_col = nil
  if suffix ~= "" then
    suffix_start_col = #line + 1
    line = line .. " " .. suffix
  end
  local entry = { id = commit_key, kind = "commit", commit = commit }
  local line_number = status_add_line(line, entry)
  status_add_highlight(line_number, 0, #commit.short_oid, "DiffReviewStatusObjectId")
  if date_start_col then
    status_add_highlight(line_number, date_start_col, date_start_col + #date_text, "DiffReviewStatusDate")
  end
  local conventional_type_end = M._status_conventional_commit_type_end(suffix)
  if suffix_start_col and conventional_type_end then
    status_add_highlight(line_number, suffix_start_col, suffix_start_col + conventional_type_end, "DiffReviewStatusCommitType")
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
  if section.issue_comments then
    M._pr_overview.render_issue_comments(section.issue_comments)
    return
  end
  if section.reviews then
    local alignment = M._pr_overview.review_summary_alignment(section.reviews)
    for _, review in ipairs(section.reviews) do
      M._pr_overview.render_review(review, alignment)
    end
    return
  end
  if section.commits then
    local date_width = 0
    for _, commit in ipairs(section.commits) do
      local date_text = status_commit_relative_date(commit)
      if date_text and #date_text > date_width then date_width = #date_text end
    end
    for _, commit in ipairs(section.commits) do
      status_render_commit(commit, date_width)
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

  systemlist_async(M._git_show_diff_command(cwd, commit.oid), function(output, code)
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
    if entry.fold_target_id and candidate and candidate.id == entry.fold_target_id then return candidate end
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
    local syntax_source = entry.kind == "file" and "file" or "diff"
    prewarm_file_diff_syntax(entry.file, "status-cursor-prewarm:" .. (entry.id or entry.file.filename), nil, { syntax_source = syntax_source })
  elseif M._status_entry_is_hunk_like(entry) and entry.file and entry.hunk then
    local callback_key = "status-cursor-prewarm:" .. (entry.id or entry.file.filename)
    local syntax_source = entry.kind == "hunk" and "file" or "diff"
    local syntax_diff_text = nil
    if syntax_source == "file" then
      syntax_diff_text = M._status_file_syntax_diff_text(entry.file)
    end
    M._prewarm_diff_syntax(entry.file.filename, entry.hunk.diff, { entry.hunk.staged }, callback_key, nil, {
      syntax_source = syntax_source,
      syntax_diff_text = syntax_diff_text,
    })
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
      or entry.kind == "pr_head_section"
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
      or target_id:find("^pr%-head%-section:") ~= nil
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
      or previous_entry.kind == "pr_head_section"
    ) then return previous_line end
    local next_line = line + offset
    local next_entry = status.entries[next_line]
    if offset > 0 and next_entry and (
      next_entry.kind == "section"
      or next_entry.kind == "file"
      or next_entry.kind == "commit"
      or next_entry.kind == "commit_file"
      or next_entry.kind == "pr_file"
      or next_entry.kind == "pr_head_section"
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
  if M._diff_line_content_lengths then M._diff_line_content_lengths[buf] = nil end
  M._clear_diff_gutter_visual_line(buf)
  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering
  M._status_apply_hint_bar(buf)
end

---@return string
function M._gitstatus_debug_log_path()
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  return dir .. "/gitstatus-debug.log"
end

M._gitstatus_debug = M._gitstatus_debug or {}

function M._gitstatus_debug.enabled()
  local global_enabled = vim.g.diff_review_gitstatus_debug
  return M._gitstatus_debug_force == true
    or M._gitstatus_debug_enabled == true
    or global_enabled == true
    or global_enabled == 1
end

---@param value any
---@return string
function M._gitstatus_debug.one_line(value)
  return vim.inspect(value):gsub("\r", "\\r"):gsub("\n", "\\n")
end

---@param value any
---@return string
function M._gitstatus_debug.text(value)
  return tostring(value or ""):gsub("\r", "\\r"):gsub("\n", "\\n")
end

---@param cache any
---@return string
function M._gitstatus_debug.cache_state(cache)
  if cache == nil then return "nil" end
  if cache == false then return "false" end
  if type(cache) == "table" then
    if cache.pending then return "pending" end
    local parts = { "ready" }
    if cache.buf then parts[#parts + 1] = "buf=" .. tostring(cache.buf) end
    if cache.tree then parts[#parts + 1] = "tree=true" end
    if cache.highlight_query then parts[#parts + 1] = "query=true" end
    return table.concat(parts, " ")
  end
  return type(cache)
end

---@param details table
---@return string
function M._gitstatus_debug.extmark_details(details)
  local parts = {}
  if details.hl_group ~= nil then parts[#parts + 1] = "hl=" .. M._gitstatus_debug.one_line(details.hl_group) end
  if details.line_hl_group ~= nil then parts[#parts + 1] = "line_hl=" .. M._gitstatus_debug.one_line(details.line_hl_group) end
  if details.hl_eol ~= nil then parts[#parts + 1] = "eol=" .. tostring(details.hl_eol) end
  if details.priority ~= nil then parts[#parts + 1] = "prio=" .. tostring(details.priority) end
  if details.end_col ~= nil then parts[#parts + 1] = "end=" .. tostring(details.end_col) end
  if details.virt_text ~= nil then parts[#parts + 1] = "virt=" .. M._gitstatus_debug.one_line(details.virt_text) end
  if details.conceal ~= nil then parts[#parts + 1] = "conceal=" .. M._gitstatus_debug.one_line(details.conceal) end
  if details.url ~= nil then parts[#parts + 1] = "url=" .. M._gitstatus_debug.one_line(details.url) end
  return table.concat(parts, " ")
end

---@param buf integer
---@param row integer 1-based
---@return string[]
function M._gitstatus_debug.extmarks_for_row(buf, row)
  local lines = {}
  local namespaces = vim.api.nvim_get_namespaces()
  local namespace_items = {}
  for name, namespace in pairs(namespaces) do
    namespace_items[#namespace_items + 1] = { name = name, namespace = namespace }
  end
  table.sort(namespace_items, function(left, right)
    return left.name < right.name
  end)

  for _, item in ipairs(namespace_items) do
    local ok, marks = pcall(
      vim.api.nvim_buf_get_extmarks,
      buf,
      item.namespace,
      { row - 1, 0 },
      { row - 1, -1 },
      { details = true, overlap = true }
    )
    if ok and type(marks) == "table" and #marks > 0 then
      for _, mark in ipairs(marks) do
        local details = mark[4] or {}
        lines[#lines + 1] = ("    ns=%s(%d) col=%s %s"):format(
          item.name,
          item.namespace,
          tostring(mark[3]),
          M._gitstatus_debug.extmark_details(details)
        )
      end
    end
  end
  return lines
end

---@param win integer
---@param row integer 1-based
---@param line string
---@return string
function M._gitstatus_debug.first_token_cell(win, row, line)
  local start_col, end_col = line:find("[%a_][%w_]*")
  if not start_col then return "token=<none>" end
  local token = line:sub(start_col, end_col)
  local screen_ok, screen_pos = pcall(vim.fn.screenpos, win, row, start_col)
  if not (screen_ok and type(screen_pos) == "table" and tonumber(screen_pos.row) and tonumber(screen_pos.row) > 0) then
    return ("token=%s screen=<not-visible>"):format(M._gitstatus_debug.one_line(token))
  end
  local cell_ok, cell = pcall(vim.api.nvim__inspect_cell, 1, screen_pos.row - 1, screen_pos.col - 1)
  if not cell_ok then
    return ("token=%s screen=%s cell_error=%s"):format(
      M._gitstatus_debug.one_line(token),
      M._gitstatus_debug.one_line(screen_pos),
      M._gitstatus_debug.text(cell)
    )
  end
  return ("token=%s screen=%s cell=%s"):format(
    M._gitstatus_debug.one_line(token),
    M._gitstatus_debug.one_line(screen_pos),
    M._gitstatus_debug.one_line(cell)
  )
end

---@param entry DiffReviewStatusEntry?
---@return string
function M._gitstatus_debug.entry(entry)
  if not entry then return "entry=nil" end
  local parts = {
    "kind=" .. tostring(entry.kind),
    "id=" .. tostring(entry.id),
  }
  if entry.file then
    parts[#parts + 1] = "file=" .. tostring(entry.file.filename)
    parts[#parts + 1] = "relpath=" .. tostring(entry.file.relpath)
    parts[#parts + 1] = "file_status=" .. tostring(entry.file.git_status or entry.file.status)
  end
  if entry.hunk then
    parts[#parts + 1] = "hunk_pos=" .. tostring(entry.hunk.pos)
    parts[#parts + 1] = "hunk_staged=" .. tostring(entry.hunk.staged)
  end
  if entry.diff_line then
    parts[#parts + 1] = "diff=" .. M._gitstatus_debug.one_line(entry.diff_line)
  end
  return table.concat(parts, " ")
end

---@param state table
---@param filename string
---@return string[]
function M._gitstatus_debug.file_syntax(state, filename)
  local lines = {}
  if not filename or filename == "" then return lines end
  if state.gitstatus_debug_seen_files[filename] then return lines end
  state.gitstatus_debug_seen_files[filename] = true

  lines[#lines + 1] = "  syntax file=" .. filename
  local file_entry = nil
  for _, section in ipairs(state.sections or {}) do
    for _, file in ipairs(section.files or {}) do
      if file.filename == filename then
        file_entry = file
        break
      end
    end
    if file_entry then break end
  end
  if file_entry then
    local diff_text = M._status_file_syntax_diff_text(file_entry)
    lines[#lines + 1] = "    combined_diff_len=" .. tostring(diff_text and #diff_text or 0)
    lines[#lines + 1] = "    new_side_matches_file=" .. tostring(diff_text and M._diff_new_side_matches_file(filename, diff_text) or nil)
  else
    lines[#lines + 1] = "    status_file=<not-found>"
  end
  local source_lines = M._file_source_lines(filename)
  lines[#lines + 1] = "    source_lines=" .. tostring(source_lines and #source_lines or nil)
  lines[#lines + 1] = "    file_syntax_cache=" .. M._gitstatus_debug.cache_state(M._ts_syntax_cache and M._ts_syntax_cache[filename])
  return lines
end

---@param buf integer
---@param reason string
function M._gitstatus_debug.dump(buf, reason)
  if not M._gitstatus_debug.enabled() then return end
  local state = M._status_states and M._status_states[buf] or (M._status and M._status.buf == buf and M._status) or nil
  if not (state and state.view_kind == "status" and vim.api.nvim_buf_is_valid(buf)) then return end
  state.gitstatus_debug_dump_reason = reason
  if state.gitstatus_debug_dump_pending then return end
  state.gitstatus_debug_dump_pending = true

  vim.defer_fn(function()
    state = M._status_states and M._status_states[buf] or (M._status and M._status.buf == buf and M._status) or nil
    if state then state.gitstatus_debug_dump_pending = false end
    if not M._gitstatus_debug.enabled() then return end
    if not (state and state.view_kind == "status" and vim.api.nvim_buf_is_valid(buf)) then return end
    reason = state.gitstatus_debug_dump_reason or reason
    local win = vim.fn.win_findbuf(buf)[1]
    local lines = {
      "GitStatus debug dump",
      "reason=" .. tostring(reason),
      "time=" .. os.date("%Y-%m-%d %H:%M:%S"),
      "buf=" .. tostring(buf),
      "win=" .. tostring(win),
      "cwd=" .. tostring(state.cwd),
      "view_kind=" .. tostring(state.view_kind),
      "request_id=" .. tostring(state.request_id),
      "reconcile_generation=" .. tostring(state.reconcile_generation),
      "line_count=" .. tostring(vim.api.nvim_buf_line_count(buf)),
      "filetype=" .. tostring(vim.bo[buf].filetype),
      "buftype=" .. tostring(vim.bo[buf].buftype),
    }

    if win and vim.api.nvim_win_is_valid(win) then
      lines[#lines + 1] = "window_options=" .. M._gitstatus_debug.one_line({
        conceallevel = vim.wo[win].conceallevel,
        concealcursor = vim.wo[win].concealcursor,
        foldenable = vim.wo[win].foldenable,
        foldlevel = vim.wo[win].foldlevel,
        foldmethod = vim.wo[win].foldmethod,
        linebreak = vim.wo[win].linebreak,
        wrap = vim.wo[win].wrap,
        winbar = vim.wo[win].winbar,
      })
    else
      lines[#lines + 1] = "window_options=<no-valid-window>"
    end

    lines[#lines + 1] = ""
    lines[#lines + 1] = "syntax caches:"
    state.gitstatus_debug_seen_files = {}
    for _, entry in pairs(state.entries or {}) do
      if entry and entry.file and entry.file.filename then
        vim.list_extend(lines, M._gitstatus_debug.file_syntax(state, entry.file.filename))
      end
    end
    state.gitstatus_debug_seen_files = nil

    lines[#lines + 1] = ""
    lines[#lines + 1] = "visible rows:"
    local start_row = 1
    local end_row = vim.api.nvim_buf_line_count(buf)
    if win and vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_call(win, function()
        start_row = vim.fn.line("w0")
        end_row = vim.fn.line("w$")
      end)
    end
    local buffer_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
    for offset, line in ipairs(buffer_lines) do
      local row = start_row + offset - 1
      local entry = state.entries and state.entries[row] or nil
      lines[#lines + 1] = ("%4d | %s"):format(row, M._gitstatus_debug.text(line))
      lines[#lines + 1] = "    " .. M._gitstatus_debug.entry(entry)
      if win and vim.api.nvim_win_is_valid(win) then
        lines[#lines + 1] = "    " .. M._gitstatus_debug.first_token_cell(win, row, line)
      end
      vim.list_extend(lines, M._gitstatus_debug.extmarks_for_row(buf, row))
    end

    lines[#lines + 1] = ""
    lines[#lines + 1] = "diff rows missing Tree-sitter extmarks:"
    local missing_count = 0
    for row, entry in pairs(state.entries or {}) do
      local diff_line = entry and entry.diff_line or nil
      if diff_line and diff_line.code and diff_line.code:match("%S") then
        local has_treesitter = false
        for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, M._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
          local details = mark[4] or {}
          if type(details.hl_group) == "string" and details.hl_group:sub(1, 1) == "@" then
            has_treesitter = true
            break
          elseif type(details.hl_group) == "table" then
            for _, group in ipairs(details.hl_group) do
              if tostring(group):sub(1, 1) == "@" then
                has_treesitter = true
                break
              end
            end
            if has_treesitter then break end
          end
        end
        if not has_treesitter then
          missing_count = missing_count + 1
          lines[#lines + 1] = ("%4d | %s | %s"):format(row, M._gitstatus_debug.text(diff_line.code), M._gitstatus_debug.entry(entry))
        end
      end
    end
    if missing_count == 0 then lines[#lines + 1] = "  none" end

    local ok, err = pcall(vim.fn.writefile, lines, M._gitstatus_debug_log_path())
    if not ok then
      notify_debug("GitStatus debug dump failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
    end
  end, 250)
end

---@param buf integer
function M._status_stop_markdown_highlighter(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.bo[buf].filetype ~= "GitStatus" then return end
  if vim.treesitter and type(vim.treesitter.stop) == "function" then
    pcall(vim.treesitter.stop, buf, "markdown")
  end
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
  M._diff_line_content_lengths = M._diff_line_content_lengths or {}
  M._diff_line_content_lengths[buf] = {}
  M._clear_diff_gutter_visual_line(buf)

  local folded_head_parents = {}
  for _, head_line in ipairs(head_lines) do
    if not (head_line.parent_id and folded_head_parents[head_line.parent_id]) then
      if head_line.segments then
        status_add_segment_line(head_line.segments, head_line.entry)
      else
        status_add_segment_line(head_line)
      end
      local entry = head_line.entry
      if entry and entry.kind == "pr_head_section" and entry.id then
        folded_head_parents[entry.id] = status_folded(entry.id, head_line.default_folded == true)
      end
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

  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, M._status.lines)
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering

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
      priority = highlight.priority or 90,
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
    M._pr_edit.activate_markdown_code(buf)
    M._pr_edit.sync_modifiable(buf)
  elseif view_kind == "review" then
    M._review.on_render(buf)
  elseif view_kind == "status" then
    M._status_stop_markdown_highlighter(buf)
    M._status_issues.sync_modifiable(buf)
  end
  M._gitstatus_debug.dump(buf, "status_render_loaded")
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
      latest_status.head_lines = status_build_head_lines(
        latest_status.head_values,
        latest_status.pr,
        latest_status.about,
        latest_status.issues
      )
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
        latest_status.head_lines = status_build_head_lines(
          latest_status.head_values,
          latest_status.pr,
          latest_status.about,
          latest_status.issues
        )
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
    M._status_issues.ensure_state(latest_status, cwd)
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
      M._status_issues.ensure_state(current_status, cwd)
      result.head_lines = status_build_head_lines(
        result.head_values or {},
        current_status.pr,
        current_status.about,
        current_status.issues
      )
      current_status.head_lines = result.head_lines
      current_status.head_values = result.head_values
      current_status.sections = result.sections
      current_status.fancy_rows = {}
      if preserve_current_cursor and not opts.restore_initial_folds then
        target_id, fallback_line = status_cursor_target(buf)
      end
      status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
      vim.schedule(function()
        local metadata_status = M._status_states and M._status_states[buf] or render_state
        if not (
          metadata_status
          and metadata_status.request_id == request_id
          and metadata_status.cwd == cwd
          and vim.api.nvim_buf_is_valid(buf)
        ) then return end
        M.github_load_repo_metadata(cwd, require("github.repo_cache").repo_for_cwd(cwd))
      end)
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
  status.pr_regular_comments = status.pr_regular_comments or {}
  M._pr_overview.refresh_editable_comments(status)
  status.head_lines = status_pr_detail_head_lines(pr, status)
  status.sections = status_pr_sections(cwd, pr, diff_text, status.pr_comments, status.pr_standalone_comments, status.pr_regular_comments)
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

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M._pr_overview.load_checks(pr, cwd, buf)
  local status = M._status
  if not (status and status.buf == buf and pr.repo and pr.repo ~= "") then return end
  status.pr_checks_request_id = (status.pr_checks_request_id or 0) + 1
  status.pr_checks_loading = true
  status.pr_checks_error = nil
  local request_id = status.pr_checks_request_id
  render_pr_status(pr, cwd, buf, status.pr_diff_text)
  gh.pr_checks_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_checks_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    latest_status.pr_checks_loading = false
    if not result.ok then
      latest_status.pr_checks_error = result.message or "gh failed"
      notify_error("GitHub PR checks failed: " .. latest_status.pr_checks_error, "DiffReview")
      render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
      return
    end
    latest_status.pr_checks_error = nil
    latest_status.pr_checks = result.checks or {}
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
---@return table? state the review-capable status-state, or nil if buf has no inline comment rows
function M._review.state(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if status and status.view_kind == "review" then return status end
  if status and status.view_kind == "pr" then
    status.pr_standalone_comments = status.pr_standalone_comments or {}
    status.pr_regular_comments = status.pr_regular_comments or {}
    if M._pr_overview.editable_comments then
      status.review_comments = M._pr_overview.editable_comments(status)
    else
      status.review_comments = status.pr_standalone_comments
    end
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
  require("github.repo_cache").set_data_dir_for_test(path)
end

---@return string
function M._review.data_dir()
  return require("github.repo_cache").base_dir()
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
  return require("github.repo_cache").review_path(M._review.storage_repo(state), (state.pr or {}).number)
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
    gh.add_pending_review_comment_async(state.cwd, state.review_remote.node_id, {
      body = queued_body,
      path = comment.path,
      position = comment.position,
      commit_id = state.commit_id,
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
  local relative = M._datetime.relative(value)
  if relative ~= "" then return relative end
  return "just now"
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
  local marker = M._review.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s "):format(
    M._review.comment_icon,
    M._datetime.action_phrase(marker .. user, "commented", comment.updated_at or comment.created_at)
  )
  local right_text = ""
  if not comment.pr_issue_comment then
    local line_number = tonumber(comment.line)
    right_text = line_number and (" L%d"):format(line_number) or " L?"
  end
  return left_text, right_text
end

---@param reply table
---@return string left_text
---@return string right_text
function M._review.reply_header_text(reply)
  local user = tostring(reply.user or "unknown")
  return ("%s %s "):format(
    M._review.reply_icon,
    M._datetime.action_phrase(user, "replied", reply.updated_at or reply.created_at)
  ), ""
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
  local marker = M._review.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s | "):format(
    M._review.comment_icon,
    M._datetime.action_phrase(marker .. user, "commented", comment.updated_at or comment.created_at)
  )
  local width = M._review.comment_rule_width(win, buf)
  local preview_width = math.max(0, width - vim.fn.strdisplaywidth(left_text))
  local preview = M._review.truncate_preview_text(M._review.comment_preview_text(comment.body or ""), preview_width)
  return M._review.truncate_display(left_text .. preview, width)
end

---@param line_number integer
---@param text string
function M._review.add_comment_rule_date_highlights(line_number, text)
  for _, range in ipairs(M._datetime.date_highlight_ranges(text)) do
    status_add_highlight(line_number, range.start_col, range.end_col, "DiffReviewStatusDate")
  end
end

---@param buf integer
---@param row0 integer
---@param text string
function M._review.set_comment_rule_date_extmarks(buf, row0, text)
  for _, range in ipairs(M._datetime.date_highlight_ranges(text)) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, row0, range.start_col, {
      end_col = range.end_col,
      hl_group = "DiffReviewStatusDate",
      priority = 95,
    })
  end
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
  M._review.add_comment_rule_date_highlights(header_line, header)

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
    M._review.add_comment_rule_date_highlights(folded_line, folded)
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
  M._review.add_comment_rule_date_highlights(header_line, header)

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
  if entry and entry.kind == "pr_comment" and entry.pr_comment then
    for index, comment in ipairs(state.review_comments or {}) do
      if M._review.same_comment(comment, entry.pr_comment) then return comment, index end
    end
    return nil
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
  if state.view_kind == "pr" then
    render_pr_status(state.pr, state.cwd, buf, state.pr_diff_text)
  else
    M._review.save_draft(state)
    M._review.render(buf)
  end
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
      and entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body then
      close_comment_range(row - 1)
      entry.pr_comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = false })
    elseif entry
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
    elseif entry and ((entry.review_body and entry.review_comment) or (entry.pr_comment_body and entry.pr_comment)) then
      local comment = entry.review_comment or entry.pr_comment
      local body_index = entry.review_body_index or entry.pr_comment_body_index
      comment.review_body_line_marks = comment.review_body_line_marks or {}
      local mark_id = vim.api.nvim_buf_set_extmark(buf, M._review.ns, row - 1, 0, { right_gravity = false })
      comment.review_body_line_marks[#comment.review_body_line_marks + 1] = { id = mark_id, line_index = body_index }
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
    elseif entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and M._review.same_comment(entry.pr_comment, comment) then
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
    M._review.set_comment_rule_date_extmarks(buf, row0, line)
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
  local is_regular_pr_comment = state.view_kind == "pr"
    and M._pr_overview.is_regular_comment
    and M._pr_overview.is_regular_comment(state, comment)
  if not is_regular_pr_comment then M._review.refresh_inline_comment_header(buf, comment) end
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
      for _, hunk in ipairs(selected_hunk.raw_hunks or { selected_hunk }) do
        hunks[#hunks + 1] = hunk
      end
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
function M._review.inline_comment_from_payload(state, payload)
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
  return M._review.normalize_comment(state, comment)
end

---@param state table
---@param payload table
---@return table comment
function M._review.create_inline_comment(state, payload)
  local comment = M._review.inline_comment_from_payload(state, payload)
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

---@param status table
---@return table[]
function M._pr_overview.editable_comments(status)
  local comments = {}
  for _, comment in ipairs(status.pr_standalone_comments or {}) do
    comments[#comments + 1] = comment
  end
  for _, comment in ipairs(status.pr_regular_comments or {}) do
    comments[#comments + 1] = comment
  end
  return comments
end

---@param status table
---@return table[]
function M._pr_overview.refresh_editable_comments(status)
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  status.review_comments = M._pr_overview.editable_comments(status)
  return status.review_comments
end

---@param buf integer
---@return table? status
function M._pr_overview.state(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if not (status and status.view_kind == "pr" and status.pr) then return nil end
  M._pr_overview.refresh_editable_comments(status)
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
---@param target table?
---@return integer?
function M._pr_overview.regular_comment_index(status, target)
  if not (status and target) then return nil end
  for index, comment in ipairs(status.pr_regular_comments or {}) do
    if M._review.same_comment(comment, target) then return index end
  end
  return nil
end

---@param status table
---@param comment table?
---@return boolean
function M._pr_overview.is_regular_comment(status, comment)
  return M._pr_overview.regular_comment_index(status, comment) ~= nil
end

---@param status table
---@param comment table?
---@return boolean
function M._pr_overview.is_standalone_comment(status, comment)
  if not (status and comment) then return false end
  for _, local_comment in ipairs(status.pr_standalone_comments or {}) do
    if M._review.same_comment(local_comment, comment) then return true end
  end
  for _, local_comment in ipairs(status.pr_regular_comments or {}) do
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

---@param status table
---@return boolean
function M._pr_overview.selection_is_in_changes(status)
  local mode = vim.fn.mode()
  local start_row, end_row
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
  else
    start_row = vim.fn.line(".")
    end_row = start_row
  end
  if start_row > end_row then start_row, end_row = end_row, start_row end

  local changes_section_name = "pr:" .. tostring(status.pr and status.pr.number or "") .. ":changes"
  local found_diff_row = false
  for row = start_row, end_row do
    local entry = status.entries and status.entries[row] or nil
    if entry and entry.diff_line then
      if not (entry.kind == "pr_hunk" and entry.hunk and entry.hunk.section_name == changes_section_name) then
        return false
      end
      found_diff_row = true
    end
  end
  return found_diff_row
end

---@param status table
---@return table? payload
function M._pr_overview.selection_payload(status)
  if not M._pr_overview.selection_is_in_changes(status) then return nil end
  return M._review.selection_payload(status)
end

---@param buf integer
function M._pr_overview.refresh_modified(buf)
  local status = M._pr_overview.state(buf)
  if not status or not vim.api.nvim_buf_is_valid(buf) then return end
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  local comments_dirty = false
  for _, comment in ipairs(status.review_comments or {}) do
    if M._review.comment_needs_sync(comment) then
      comments_dirty = true
      break
    end
  end
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty or comments_dirty
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
---@param comment table
---@param remote table?
---@param queued_body string
function M._pr_overview.apply_synced_regular_comment(status, comment, remote, queued_body)
  local comment_index = M._pr_overview.regular_comment_index(status, comment)
  local old_entry_id = comment_index and M._pr_overview.issue_comment_entry_id(comment, comment_index) or nil
  local old_folded = old_entry_id and status_folded(old_entry_id, true) or nil
  if remote then
    comment.remote_id = remote.remote_id or comment.remote_id
    comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
    comment.user = remote.user or comment.user
    comment.created_at = remote.created_at or comment.created_at
    comment.updated_at = remote.updated_at or comment.updated_at
    comment.url = remote.url or comment.url
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
  M._pr_overview.refresh_editable_comments(status)
  if comment_index then
    local new_entry_id = M._pr_overview.issue_comment_entry_id(comment, comment_index)
    if old_entry_id and old_entry_id ~= new_entry_id and old_folded ~= nil then
      set_status_folded(new_entry_id, old_folded)
    end
  end
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
function M._pr_overview.sync_regular_comment(buf, comment)
  local status = M._pr_overview.state(buf)
  if not (status and status.pr and comment) then return end
  local body = M._review.comment_body_for_sync(comment.body or "")
  if body == "" then
    comment.syncing = nil
    M._pr_overview.refresh_modified(buf)
    return
  end
  local pr = status.pr
  local update_id = comment.remote_id
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
        notify_error("GitHub PR comment sync failed: " .. (result.message or "gh failed"), "DiffReview")
        if vim.api.nvim_buf_is_valid(buf) then
          M._pr_overview.render_preserving_inline_cursor(buf)
          M._pr_overview.refresh_modified(buf)
        end
        done()
        return
      end
      M._pr_overview.apply_synced_regular_comment(latest, comment, result.issue_comments and result.issue_comments[1] or nil, body)
      if vim.api.nvim_buf_is_valid(buf) then
        M._pr_overview.render_preserving_inline_cursor(buf)
        M._pr_overview.refresh_modified(buf)
      end
      vim.notify("PR comment synced", vim.log.levels.INFO, { title = "DiffReview" })
      done()
    end
    if update_id then
      gh.update_issue_comment_async(latest.cwd, pr.repo, update_id, body, callback)
    else
      gh.create_issue_comment_async(latest.cwd, pr.number, pr.repo, body, callback)
    end
  end)
end

---@param buf integer
---@param comment table
function M._pr_overview.sync_standalone_comment(buf, comment)
  local status = M._pr_overview.state(buf)
  if not (status and status.pr and comment) then return end
  if M._pr_overview.is_regular_comment(status, comment) or comment.pr_issue_comment then
    M._pr_overview.sync_regular_comment(buf, comment)
    return
  end
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
  for _, comment in ipairs(status.review_comments or {}) do
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

---@param status table
---@param payload table
---@return table comment
function M._pr_overview.create_inline_comment(status, payload)
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  local comment = M._review.inline_comment_from_payload(status, payload)
  comment.standalone = true
  status.pr_standalone_comments[#status.pr_standalone_comments + 1] = comment
  M._pr_overview.refresh_editable_comments(status)
  return comment
end

---@param status table
---@return table comment
function M._pr_overview.create_regular_comment(status)
  status.pr_regular_comments = status.pr_regular_comments or {}
  local created_at = os.date("%Y-%m-%d %H:%M")
  local comment = {
    body = "",
    user = "you",
    created_at = created_at,
    updated_at = created_at,
    local_state = "new",
  }
  M._review.normalize_comment(status, comment)
  status.pr_regular_comments[#status.pr_regular_comments + 1] = comment
  set_status_folded(M._pr_overview.issue_comment_entry_id(comment, #status.pr_regular_comments), false)
  M._pr_overview.refresh_editable_comments(status)
  return comment
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
  local payload = M._pr_overview.selection_payload(status)
  M._review.leave_visual()
  local comment = payload
    and M._pr_overview.create_inline_comment(status, payload)
    or M._pr_overview.create_regular_comment(status)
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
  local command = M._git_diff_command(cwd, { branch })
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
---@field review_mark? integer
---@field milestone_mark? integer
---@field status_mark? integer
---@field desc_start_mark? integer
---@field desc_end_mark? integer beyond-the-last-body-line mark (end-exclusive)
---@field title_marker_id? integer
---@field review_marker_id? integer
---@field milestone_marker_id? integer
---@field desc_marker_id? integer
---@field description_rendered_folded? boolean
---@field lock_initial? boolean
---@field initial_cursor_row? integer
---@field pending_reviewers? DiffReviewGhRequestedReviewer[]

---@param buf integer
---@param id integer?
---@return integer? row0
function M._pr_edit.mark_row(buf, id)
  if not id then return nil end
  local pos = vim.api.nvim_buf_get_extmark_by_id(buf, M._pr_edit.ns, id, {})
  return pos and pos[1] or nil
end

---@param buf integer
---@param label_pattern string
---@return integer? row0
function M._pr_edit.label_row(buf, label_pattern)
  if not vim.api.nvim_buf_is_valid(buf) then return nil end
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match(label_pattern) then return index - 1 end
  end
  return nil
end

---@param buf integer
---@param id integer?
---@param label_pattern string
---@return integer? row0
function M._pr_edit.field_row(buf, id, label_pattern)
  return M._pr_edit.label_row(buf, label_pattern) or M._pr_edit.mark_row(buf, id)
end

---@return integer namespace
function M._pr_edit.markdown_namespace()
  if M._pr_edit.render_markdown_ns then return M._pr_edit.render_markdown_ns end
  local ok, ui = pcall(require, "render-markdown.core.ui")
  M._pr_edit.render_markdown_ns = ok and ui and ui.ns or vim.api.nvim_create_namespace("render-markdown.nvim")
  return M._pr_edit.render_markdown_ns
end

---@param buf integer
---@return integer? first0
---@return integer? after0
function M._pr_edit.description_range0(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not state then return nil, nil end
  return M._pr_edit.mark_row(buf, state.desc_start_mark), M._pr_edit.mark_row(buf, state.desc_end_mark)
end

---@param buf integer
function M._pr_edit.prune_description_markdown(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local namespace = M._pr_edit.markdown_namespace()
  local first0, after0 = M._pr_edit.description_range0(buf)
  if not (first0 and after0) then
    vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
    return
  end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, namespace, 0, -1, {})) do
    local row0 = mark[2]
    if row0 < first0 or row0 >= after0 then pcall(vim.api.nvim_buf_del_extmark, buf, namespace, mark[1]) end
  end
end

---@param buf integer
---@param win integer
---@return table
function M._pr_edit.description_markdown_config(buf, win)
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  return {
    enabled = true,
    render_modes = true,
    debounce = 0,
    sign = { enabled = false },
    win_options = {
      conceallevel = { default = conceallevel, rendered = conceallevel },
      concealcursor = { default = concealcursor, rendered = concealcursor },
    },
    on = {
      render = function(ctx)
        M._pr_edit.prune_description_markdown(ctx.buf)
      end,
    },
  }
end

---@param buf integer
---@return integer?
function M._pr_edit.description_markdown_window(buf)
  if vim.api.nvim_get_current_buf() == buf then return vim.api.nvim_get_current_win() end
  local wins = vim.fn.win_findbuf(buf)
  for _, win in ipairs(wins) do
    if vim.api.nvim_win_is_valid(win) then return win end
  end
  return nil
end

---@param buf integer
function M._pr_edit.activate_markdown_code(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if not M._pr_edit.gitstatus_markdown_language_registered then
    pcall(vim.treesitter.language.register, "markdown", "GitStatus")
    M._pr_edit.gitstatus_markdown_language_registered = true
  end
  local ok, markdown_code = pcall(require, "markdown_code")
  if ok and type(markdown_code) == "table" and type(markdown_code.activate) == "function" then
    markdown_code.activate(buf, {
      filetype = "GitStatus",
      register_as_markdown = true,
      notify_title = "DiffReview",
    })
  end
end

---@param buf integer
function M._pr_edit.render_description_markdown(buf)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end

  M._pr_edit.activate_markdown_code(buf)

  if M._pr_edit.render_markdown_unavailable then return end
  local ok, render_markdown = pcall(require, "render-markdown")
  if not ok or type(render_markdown) ~= "table" or type(render_markdown.render) ~= "function" then
    M._pr_edit.render_markdown_unavailable = true
    return
  end

  if not M._pr_edit.gitstatus_markdown_language_registered then
    pcall(vim.treesitter.language.register, "markdown", "GitStatus")
    M._pr_edit.gitstatus_markdown_language_registered = true
  end

  local win = M._pr_edit.description_markdown_window(buf)
  if not win then return end
  local render_ok, err = pcall(render_markdown.render, {
    buf = buf,
    win = win,
    config = M._pr_edit.description_markdown_config(buf, win),
  })
  if render_ok then
    M._pr_edit.prune_description_markdown(buf)
  elseif not M._pr_edit.render_markdown_error_notified then
    M._pr_edit.render_markdown_error_notified = true
    vim.notify("PR description markdown render failed: " .. tostring(err), vim.log.levels.WARN, { title = "DiffReview" })
  end
end

---@param buf integer
---@param status table
---@return string? title without the "Title:" label
---@return string? desc description block joined with newlines
---@return string? review reviewer-request field without the "Review:" label
---@return string? milestone milestone field without the "Release:" label or marker icon
function M._pr_edit.current_values(buf, status)
  local state = status.pr_edit
  if not state then return nil end
  local title_row0 = M._pr_edit.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M._pr_edit.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M._pr_edit.field_row(buf, state.milestone_mark, "^Release:")
  local first0 = M._pr_edit.mark_row(buf, state.desc_start_mark)
  local after0 = M._pr_edit.mark_row(buf, state.desc_end_mark)
  if not (title_row0 and review_row0 and milestone_row0 and first0 and after0) then return nil end
  local title_line = vim.api.nvim_buf_get_lines(buf, title_row0, title_row0 + 1, false)[1] or ""
  local review_line = vim.api.nvim_buf_get_lines(buf, review_row0, review_row0 + 1, false)[1] or ""
  local milestone_line = vim.api.nvim_buf_get_lines(buf, milestone_row0, milestone_row0 + 1, false)[1] or ""
  local title = vim.trim((title_line:gsub("^Title:%s*", "")))
  local review = vim.trim((review_line:gsub("^Review:%s*", "")))
  local milestone = M._pr_edit.milestone_value(milestone_line)
  local desc = state.description_rendered_folded
    and table.concat(status_markdown_lines(status.pr.body), "\n")
    or table.concat(vim.api.nvim_buf_get_lines(buf, first0, after0, false), "\n")
  return title, desc, review, milestone
end

---@param buf integer
---@param status table
---@return boolean title_dirty
---@return boolean desc_dirty
---@return boolean review_dirty
---@return boolean milestone_dirty
function M._pr_edit.dirty_flags(buf, status)
  local title, desc, review, milestone = M._pr_edit.current_values(buf, status)
  if title == nil or not status.pr then return false, false, false, false end
  local baseline = table.concat(status_markdown_lines(status.pr.body), "\n")
  return title ~= status.pr.title,
    desc ~= baseline,
    M._pr_edit.reviewer_change(status, review).changed,
    M._pr_edit.milestone_change(status, milestone).changed
end

---@param buf integer
---@param row integer 1-based cursor row
---@return "title"|"review"|"milestone"|"desc"|nil
function M._pr_edit.region_kind_at(buf, row)
  local status = M._status_states and M._status_states[buf] or nil
  local state = status and status.pr_edit or nil
  if not state then return nil end
  local title_row0 = M._pr_edit.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M._pr_edit.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M._pr_edit.field_row(buf, state.milestone_mark, "^Release:")
  local first0 = M._pr_edit.mark_row(buf, state.desc_start_mark)
  local after0 = M._pr_edit.mark_row(buf, state.desc_end_mark)
  if title_row0 and row == title_row0 + 1 then return "title" end
  if review_row0 and row == review_row0 + 1 then return "review" end
  if milestone_row0 and row == milestone_row0 + 1 then return "milestone" end
  if first0 and after0 and row >= first0 + 1 and row <= after0 then return "desc" end
  return nil
end

---@param buf integer
---@param state DiffReviewPrEditState
function M._pr_edit.clear_markers(buf, state)
  for _, key in ipairs({ "title_marker_id", "review_marker_id", "milestone_marker_id", "desc_marker_id" }) do
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
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  local title_row0 = M._pr_edit.field_row(buf, state.title_mark, "^Title:")
  local review_row0 = M._pr_edit.field_row(buf, state.review_mark, "^Review:")
  local milestone_row0 = M._pr_edit.field_row(buf, state.milestone_mark, "^Release:")
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
  set_marker("review_marker_id", review_dirty, review_row0)
  set_marker("milestone_marker_id", milestone_dirty, milestone_row0)
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
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  if not (title_dirty or desc_dirty or review_dirty or milestone_dirty) then return false end
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
  state.title_mark, state.review_mark, state.milestone_mark, state.status_mark, state.desc_start_mark, state.desc_end_mark = nil, nil, nil, nil, nil, nil
  state.title_marker_id, state.review_marker_id, state.milestone_marker_id, state.desc_marker_id = nil, nil, nil, nil

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local title_row, review_row, milestone_row, status_row, label_row
  for index, line in ipairs(lines) do
    if not title_row and line:match("^Title:") then title_row = index end
    if not review_row and line:match("^Review:") then review_row = index end
    if not milestone_row and line:match("^Release:") then milestone_row = index end
    if not status_row and line:match("^Status:") then status_row = index end
    if not label_row and line == "Description:" then label_row = index end
    if title_row and review_row and milestone_row and status_row and label_row then break end
  end
  if not (title_row and review_row and milestone_row and status_row and label_row) then return end

  -- right_gravity=false: a mark with right gravity slides to the next line
  -- when its own line is replaced (retyped), losing the region.
  local description_folded = status_folded("pr-head-section:description", false)
  state.description_rendered_folded = description_folded
  local body_count = description_folded and 0 or #status_markdown_lines(status.pr.body)
  state.title_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, title_row - 1, 0, { right_gravity = false })
  state.review_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, review_row - 1, 0, { right_gravity = false })
  state.milestone_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, milestone_row - 1, 0, { right_gravity = false })
  state.status_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, status_row - 1, 0, { right_gravity = false })
  state.desc_start_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, label_row, 0, { right_gravity = false })
  state.desc_end_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, label_row + body_count, 0, { right_gravity = false })
  vim.bo[buf].modified = false
  M._pr_edit.refresh_markers(buf)
  M._pr_edit.sync_modifiable(buf)
  if vim.api.nvim_get_current_buf() ~= buf then M._pr_edit.render_description_markdown(buf) end
end

---@param line string?
---@return string
function M._pr_edit.milestone_value(line)
  local value = vim.trim(tostring(line or ""):gsub("^Release:%s*", ""):gsub("^Milestone:%s*", ""))
  value = vim.trim(value:gsub("^" .. vim.pesc(M._milestone_icon) .. "%s*", ""))
  return value
end

---@param status table
---@param milestone string?
---@return { current: string, desired: string, changed: boolean }
function M._pr_edit.milestone_change(status, milestone)
  local current = M._pr_overview.milestone_title(status and status.pr or nil)
  local desired = vim.trim(tostring(milestone or ""))
  return {
    current = current,
    desired = desired,
    changed = current ~= desired,
  }
end

---@param milestones DiffReviewGhMilestone[]?
---@param title string
---@return DiffReviewGhMilestone?
function M._pr_edit.find_milestone(milestones, title)
  local desired = vim.trim(tostring(title or ""))
  if desired == "" then return nil end
  local desired_key = desired:lower()
  for _, milestone in ipairs(milestones or {}) do
    if vim.trim(tostring(milestone.title or "")):lower() == desired_key then return milestone end
  end
  return nil
end

---@param text string?
---@return string[]
function M._pr_edit.reviewer_usernames(text)
  local usernames = {}
  local seen = {}
  for token in tostring(text or ""):gmatch("@?[%w][%w_-]*") do
    local username = token:gsub("^@", "")
    local key = username:lower()
    if username ~= "" and not seen[key] then
      seen[key] = true
      usernames[#usernames + 1] = username
    end
  end
  return usernames
end

---@param reviewers any[]?
---@return table<string, boolean>
function M._pr_edit.reviewer_set(reviewers)
  local set = {}
  for _, reviewer in ipairs(reviewers or {}) do
    local username = M._pr_overview.reviewer_login(reviewer)
    if username ~= "" then set[username:lower()] = true end
  end
  return set
end

---@param status table
---@param review string?
---@return { current: DiffReviewGhRequestedReviewer[], desired: string[], add: string[], remove: string[], changed: boolean }
function M._pr_edit.reviewer_change(status, review)
  local current = status and status.pr and M._pr_overview.pending_reviewers(status.pr, status) or {}
  local desired = M._pr_edit.reviewer_usernames(review)
  local current_set = M._pr_edit.reviewer_set(current)
  local desired_set = M._pr_edit.reviewer_set(desired)
  local add = {}
  local remove = {}
  for _, reviewer in ipairs(desired) do
    if not current_set[reviewer:lower()] then add[#add + 1] = reviewer end
  end
  for _, reviewer in ipairs(current) do
    local username = M._pr_overview.reviewer_login(reviewer)
    if username ~= "" and not desired_set[username:lower()] then remove[#remove + 1] = username end
  end
  return {
    current = current,
    desired = desired,
    add = add,
    remove = remove,
    changed = #add > 0 or #remove > 0,
  }
end

---@param buf integer
---@param status table
---@param mark_id integer?
---@param segments table[]
---@param label_pattern string?
---@return integer? row0
function M._pr_edit.patch_head_field_row(buf, status, mark_id, segments, label_pattern)
  local state = status and status.pr_edit or nil
  local row0 = state and M._pr_edit.mark_row(buf, mark_id) or nil
  if label_pattern and vim.api.nvim_buf_is_valid(buf) then
    for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
      if line:match(label_pattern) then
        row0 = index - 1
        break
      end
    end
  end
  if not (row0 and status and status.pr and vim.api.nvim_buf_is_valid(buf)) then return end
  local text, segment_highlights = M._status_segment_line_parts(segments)
  local was_modifiable = vim.bo[buf].modifiable
  local was_modified = vim.bo[buf].modified
  local old_text = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1] or ""
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_text(buf, row0, 0, row0, #old_text, { text })
  vim.bo[buf].modifiable = was_modifiable
  vim.bo[buf].modified = was_modified
  if status.lines then status.lines[row0 + 1] = text end
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, row0, row0 + 1)
  for _, highlight in ipairs(segment_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, row0, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  return row0
end

---@param buf integer
---@param status table
function M._pr_edit.patch_review_row(buf, status)
  local state = status and status.pr_edit or nil
  local row0 = M._pr_edit.patch_head_field_row(buf, status, state and state.review_mark, {
    { "Review: ", "DiffReviewStatusLabel" },
    { M._pr_overview.pending_review_text(status and status.pr or nil, status), "DiffReviewReviewPending" },
  }, "^Review:")
  if state and row0 then
    if state.review_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M._pr_edit.ns, state.review_mark) end
    state.review_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table
function M._pr_edit.patch_milestone_row(buf, status)
  local state = status and status.pr_edit or nil
  local row0 = M._pr_edit.patch_head_field_row(buf, status, state and state.milestone_mark, {
    { "Release: ", "DiffReviewStatusLabel" },
    { M._pr_overview.milestone_text(status and status.pr or nil), "DiffReviewStatusBranch" },
  }, "^Release:")
  if state and row0 then
    if state.milestone_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M._pr_edit.ns, state.milestone_mark) end
    state.milestone_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table
function M._pr_edit.patch_status_row(buf, status)
  local state = status and status.pr_edit or nil
  local pr = status and status.pr or nil
  local row0 = M._pr_edit.patch_head_field_row(buf, status, state and state.status_mark, {
    { "Status: ", "DiffReviewStatusLabel" },
    { M._pr_overview.status_text(pr), pr and pr.isDraft and "DiffReviewStatusFetching" or "DiffReviewStatusBranch" },
  }, "^Status:")
  if state and row0 then
    if state.status_mark then pcall(vim.api.nvim_buf_del_extmark, buf, M._pr_edit.ns, state.status_mark) end
    state.status_mark = vim.api.nvim_buf_set_extmark(buf, M._pr_edit.ns, row0, 0, { right_gravity = false })
  end
end

---@param buf integer
---@param status table?
---@return integer? row0
function M._pr_edit.status_row(buf, status)
  local state = status and status.pr_edit or nil
  return M._pr_edit.field_row(buf, state and state.status_mark, "^Status:")
end

---@param buf integer
---@return boolean
function M._pr_edit.cursor_on_status_value(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  local row0 = M._pr_edit.status_row(buf, status)
  if not row0 then return false end
  local cursor = vim.api.nvim_win_get_cursor(0)
  if cursor[1] ~= row0 + 1 then return false end
  local line = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1] or ""
  local value_start = line:find(M._pr_overview.status_text(status and status.pr or nil), 1, true)
  return value_start ~= nil and cursor[2] >= value_start - 1
end

---@param change { add: string[], remove: string[] }
---@return string[]
function M._pr_edit.reviewer_change_usernames(change)
  local usernames = {}
  local seen = {}
  for _, list in ipairs({ change.add or {}, change.remove or {} }) do
    for _, reviewer in ipairs(list) do
      local username = M._pr_overview.reviewer_login(reviewer)
      local key = username:lower()
      if username ~= "" and not seen[key] then
        seen[key] = true
        usernames[#usernames + 1] = username
      end
    end
  end
  return usernames
end

---@param username string
---@param users? table<string, DiffReviewGhUser>
---@return string
function M._pr_edit.reviewer_display(username, users)
  local user = users and users[username:lower()] or nil
  local name = user and vim.trim(user.name or "") or ""
  if name ~= "" then
    return ("@%s (%s)"):format(username, name)
  end
  return "@" .. username
end

---@param change { add: string[], remove: string[] }
---@param users? table<string, DiffReviewGhUser>
---@return string[]
function M._pr_edit.review_change_confirmation_lines(change, users)
  local lines = { "Confirm review request changes:" }
  if #(change.remove or {}) > 0 then
    lines[#lines + 1] = "Remove review request:"
    for _, username in ipairs(change.remove) do
      lines[#lines + 1] = "  - " .. M._pr_edit.reviewer_display(username, users)
    end
  end
  if #(change.add or {}) > 0 then
    lines[#lines + 1] = "Request review:"
    for _, username in ipairs(change.add) do
      lines[#lines + 1] = "  + " .. M._pr_edit.reviewer_display(username, users)
    end
  end
  return lines
end

---@param buf integer
function M._pr_edit.restore_dirty(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.bo[buf].modified = true
  M._pr_edit.refresh_markers(buf)
end

---@param change { desired: string[], add: string[], remove: string[] }
---@return string
function M._pr_edit.review_change_summary(change)
  local parts = {}
  if #(change.remove or {}) > 0 then
    parts[#parts + 1] = "removed " .. table.concat(vim.tbl_map(function(username) return "@" .. username end, change.remove), ", ")
  end
  if #(change.add or {}) > 0 then
    parts[#parts + 1] = "requested " .. table.concat(vim.tbl_map(function(username) return "@" .. username end, change.add), ", ")
  end
  return table.concat(parts, "; ")
end

---@param buf integer
---@param status table
function M._pr_edit.revert_review_row(buf, status)
  if not (status and status.pr) then return end
  M._pr_edit.patch_review_row(buf, status)
  M._pr_edit.refresh_markers(buf)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty
end

---@param status table
---@param reviewers any[]?
function M._pr_edit.set_pending_reviewers(status, reviewers)
  if not (status and status.pr) then return end
  local desired = {}
  local seen = {}
  M._pr_overview.add_reviewers(reviewers, desired, seen)
  status.pr.requestedReviewers = desired
  if status.pr_edit then status.pr_edit.pending_reviewers = nil end
end

---@param buf integer
---@param status table
function M._pr_edit.revert_milestone_row(buf, status)
  if not (status and status.pr) then return end
  M._pr_edit.patch_milestone_row(buf, status)
  M._pr_edit.refresh_markers(buf)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty
end

---@param status table
---@param milestone DiffReviewGhMilestone?
function M._pr_edit.set_pr_milestone(status, milestone)
  if not (status and status.pr) then return end
  status.pr.milestone = milestone
end

---@param buf integer
---@param status table
---@param change { desired: string[], add: string[], remove: string[] }
---@param done fun(ok: boolean)
function M._pr_edit.apply_reviewer_change(buf, status, change, done)
  local latest = M._status_states and M._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local function fail(message)
    notify_error(message, "DiffReview")
    M._pr_edit.restore_dirty(buf)
    done(false)
  end

  local function request_added()
    if #(change.add or {}) == 0 then
      M._pr_edit.set_pending_reviewers(latest, change.desired)
      M._pr_edit.patch_review_row(buf, latest)
      vim.bo[buf].modified = false
      M._pr_edit.refresh_markers(buf)
      vim.notify(("Review requests updated: %s"):format(M._pr_edit.review_change_summary(change)), vim.log.levels.INFO, { title = "DiffReview" })
      done(true)
      return
    end
    gh.request_reviewers_async(latest.cwd, latest.pr.number, latest.pr.repo, change.add, function(result)
      if not vim.api.nvim_buf_is_valid(buf) then
        done(false)
        return
      end
      if not result.ok then
        fail("GitHub reviewer request failed: " .. (result.message or "gh failed"))
        return
      end
      M._pr_edit.set_pending_reviewers(latest, change.desired)
      M._pr_edit.patch_review_row(buf, latest)
      vim.bo[buf].modified = false
      M._pr_edit.refresh_markers(buf)
      vim.notify(("Review requests updated: %s"):format(M._pr_edit.review_change_summary(change)), vim.log.levels.INFO, { title = "DiffReview" })
      done(true)
    end)
  end

  if #(change.remove or {}) == 0 then
    request_added()
    return
  end
  gh.remove_reviewers_async(latest.cwd, latest.pr.number, latest.pr.repo, change.remove, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      fail("GitHub reviewer removal failed: " .. (result.message or "gh failed"))
      return
    end
    request_added()
  end)
end

---@param change { desired: string[], add: string[], remove: string[] }
---@param done fun(ok: boolean)
function M._pr_edit.confirm_and_apply_reviewer_change(buf, status, change, done)
  if not change.changed then
    done(false)
    return
  end

  local usernames = M._pr_edit.reviewer_change_usernames(change)
  gh.resolve_users_async(status.cwd, usernames, function(user_result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    local lines = M._pr_edit.review_change_confirmation_lines(change, user_result.users or {})
    confirm(lines, function()
      M._pr_edit.apply_reviewer_change(buf, status, change, done)
    end, function()
      local latest = M._status_states and M._status_states[buf] or status
      M._pr_edit.revert_review_row(buf, latest)
      done(false)
    end)
  end)
end

---@param change { desired: string }
---@return string[]
function M._pr_edit.milestone_create_confirmation_lines(change)
  return {
    "Release not found:",
    "  " .. change.desired,
    "",
    "Create it now?",
  }
end

---@param change { desired: string }
---@return string
function M._pr_edit.milestone_change_summary(change)
  if change.desired == "" then return "cleared" end
  return M._milestone_icon .. " " .. change.desired
end

---@param buf integer
---@param status table
---@param milestone DiffReviewGhMilestone?
---@param done fun(ok: boolean)
function M._pr_edit.apply_milestone(buf, status, milestone, done)
  local latest = M._status_states and M._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local milestone_number = milestone and tonumber(milestone.number) or nil
  if milestone and (not milestone_number or milestone_number <= 0) then
    notify_error("GitHub milestone is missing its number", "DiffReview")
    M._pr_edit.restore_dirty(buf)
    done(false)
    return
  end

  gh.set_pr_milestone_async(latest.cwd, latest.pr.number, latest.pr.repo, milestone_number, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notify_error("GitHub milestone update failed: " .. (result.message or "gh failed"), "DiffReview")
      M._pr_edit.restore_dirty(buf)
      done(false)
      return
    end
    M._pr_edit.set_pr_milestone(latest, milestone or result.milestone)
    M._pr_edit.patch_milestone_row(buf, latest)
    vim.bo[buf].modified = false
    M._pr_edit.refresh_markers(buf)
    vim.notify(("Release updated: %s"):format(M._pr_edit.milestone_change_summary({
      desired = M._pr_overview.milestone_title(latest.pr),
    })), vim.log.levels.INFO, { title = "DiffReview" })
    done(true)
  end)
end

---@param buf integer
---@param status table
---@param change { current: string, desired: string, changed: boolean }
---@param done fun(ok: boolean)
function M._pr_edit.confirm_and_apply_milestone_change(buf, status, change, done)
  if not change.changed then
    done(false)
    return
  end

  if change.desired == "" then
    M._pr_edit.apply_milestone(buf, status, nil, done)
    return
  end

  gh.repo_milestones_async(status.cwd, status.pr.repo, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notify_error("GitHub milestones lookup failed: " .. (result.message or "gh failed"), "DiffReview")
      M._pr_edit.restore_dirty(buf)
      done(false)
      return
    end

    local milestone = M._pr_edit.find_milestone(result.milestones or {}, change.desired)
    if milestone then
      M._pr_edit.apply_milestone(buf, status, milestone, done)
      return
    end

    confirm(M._pr_edit.milestone_create_confirmation_lines(change), function()
      local latest = M._status_states and M._status_states[buf] or status
      if not (latest and latest.pr) then
        done(false)
        return
      end
      gh.create_milestone_async(latest.cwd, latest.pr.repo, change.desired, function(create_result)
        if not vim.api.nvim_buf_is_valid(buf) then
          done(false)
          return
        end
        if not create_result.ok or not create_result.milestone then
          notify_error("GitHub milestone create failed: " .. (create_result.message or "gh failed"), "DiffReview")
          M._pr_edit.restore_dirty(buf)
          done(false)
          return
        end
        M._pr_edit.apply_milestone(buf, latest, create_result.milestone, done)
      end)
    end, function()
      local latest = M._status_states and M._status_states[buf] or status
      M._pr_edit.revert_milestone_row(buf, latest)
      done(false)
    end)
  end)
end

---@param pr DiffReviewGhPR
---@param desired_draft boolean
---@return string[]
function M._pr_edit.draft_status_confirmation_lines(pr, desired_draft)
  if desired_draft then
    return {
      ("Move PR #%s back to draft?"):format(tostring(pr.number)),
      "",
      "Status will change from READY to DRAFT.",
    }
  end
  return {
    ("Mark PR #%s ready for review?"):format(tostring(pr.number)),
    "",
    "Status will change from DRAFT to READY.",
  }
end

---@param status table
---@return string
function M._pr_edit.pr_node_id(status)
  local pr = status and status.pr or nil
  return type(pr and pr.id) == "string" and pr.id or ""
end

---@param buf integer
---@param status table
---@param desired_draft boolean
---@param done fun(ok: boolean)
function M._pr_edit.apply_draft_status(buf, status, desired_draft, done)
  local latest = M._status_states and M._status_states[buf] or status
  if not (latest and latest.pr) then
    done(false)
    return
  end

  local pr_node_id = M._pr_edit.pr_node_id(latest)
  if pr_node_id == "" then
    notify_error("GitHub draft status update failed: missing pull request node id", "DiffReview")
    done(false)
    return
  end

  gh.set_pr_draft_async(latest.cwd, pr_node_id, desired_draft, function(result)
    if not vim.api.nvim_buf_is_valid(buf) then
      done(false)
      return
    end
    if not result.ok then
      notify_error("GitHub draft status update failed: " .. (result.message or "gh failed"), "DiffReview")
      done(false)
      return
    end
    latest.pr.isDraft = type(result.is_draft) == "boolean" and result.is_draft or desired_draft
    M._pr_edit.patch_status_row(buf, latest)
    vim.notify(("PR #%s status updated: %s"):format(tostring(latest.pr.number), M._pr_overview.status_text(latest.pr)), vim.log.levels.INFO, { title = "DiffReview" })
    done(true)
  end)
end

---@param buf integer
---@return boolean
function M._pr_edit.toggle_draft_status_under_cursor(buf)
  if not M._pr_edit.cursor_on_status_value(buf) then return false end
  local status = M._status_states and M._status_states[buf] or M._status
  local state = status and status.pr_edit or nil
  if not (status and status.pr and state) then return true end
  local desired_draft = not status.pr.isDraft
  confirm(M._pr_edit.draft_status_confirmation_lines(status.pr, desired_draft), function()
    M._pr_edit.enqueue(state, function(done)
      M._pr_edit.apply_draft_status(buf, status, desired_draft, function()
        done()
      end)
    end)
  end)
  return true
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
  local title, desc, review, milestone = M._pr_edit.current_values(buf, status)
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = M._pr_edit.dirty_flags(buf, status)
  vim.bo[buf].modified = false
  if not (title_dirty or desc_dirty or review_dirty or milestone_dirty) then
    M._pr_edit.patch_review_row(buf, status)
    M._pr_edit.patch_milestone_row(buf, status)
    M._pr_edit.patch_status_row(buf, status)
    return
  end

  M._pr_edit.clear_markers(buf, state)
  local pr = status.pr
  local function sync_title_and_description()
    if not (title_dirty or desc_dirty) then return end
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

  local function sync_milestone_then_title_and_description()
    if milestone_dirty then
      M._pr_edit.confirm_and_apply_milestone_change(buf, status, M._pr_edit.milestone_change(status, milestone), function(confirmed)
        if confirmed then sync_title_and_description() end
      end)
    else
      M._pr_edit.patch_milestone_row(buf, status)
      sync_title_and_description()
    end
  end

  if review_dirty then
    M._pr_edit.confirm_and_apply_reviewer_change(buf, status, M._pr_edit.reviewer_change(status, review), function(confirmed)
      if confirmed then sync_milestone_then_title_and_description() end
    end)
  else
    M._pr_edit.patch_review_row(buf, status)
    sync_milestone_then_title_and_description()
  end
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
  M._pr_edit.render_description_markdown(buf)
end

---Wire editing into a freshly created PR-view buffer: acwrite, the
---cursor-follows-modifiable lock, and lifecycle autocmds.
---@param buf integer
function M._pr_edit.attach(buf)
  local status = M._status_states and M._status_states[buf] or nil
  if not status then return end
  status.pr_edit = { queue = {}, running = false, lock_initial = true }
  vim.bo[buf].buftype = "acwrite"
  require("github.repo_cache").enable_user_completion(buf, status.pr and status.pr.repo or nil)

  -- The title, reviewer request, and milestone fields are single-line: swallow newline attempts there.
  vim.keymap.set("i", "<CR>", function()
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local region = M._pr_edit.region_kind_at(buf, row)
    if region == "title" or region == "review" or region == "milestone" then return "" end
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
  vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = buf,
    callback = function()
      M._review.sync_inline_comment_text(buf)
      M._pr_edit.refresh_markers(buf)
      M._pr_edit.render_description_markdown(buf)
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
        local args = { "apply", "--reverse", "--whitespace=nowarn", "--unidiff-zero" }
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

---@param span table
---@return "add"|"delete"|nil
function M._status_inline_span_kind(span)
  if not span then return nil end
  if span.kind == "add" or span.hl_group == "DiffReviewInlineAddBg" then return "add" end
  if span.kind == "delete" or span.hl_group == "DiffReviewInlineDeleteBg" then return "delete" end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@param prefix string
---@return table?
function M._status_diff_line_with_prefix(entry, prefix)
  for _, diff_line in ipairs(entry and entry.diff_lines or {}) do
    if diff_line.prefix == prefix then return diff_line end
  end
  if entry and entry.diff_line and entry.diff_line.prefix == prefix then return entry.diff_line end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@param cursor_col integer?
---@return table?
function M._status_jump_diff_line(entry, cursor_col)
  if not entry then return nil end
  local spans = entry.inline_jump_spans
  if type(spans) ~= "table" or #spans == 0 or type(entry.diff_lines) ~= "table" then return entry.diff_line end

  local has_add = false
  local has_delete = false
  local cursor_span = nil
  for _, span in ipairs(spans) do
    local kind = M._status_inline_span_kind(span)
    if kind == "add" then has_add = true end
    if kind == "delete" then has_delete = true end
    if type(cursor_col) == "number"
      and type(span.start_col) == "number"
      and type(span.end_col) == "number"
      and cursor_col >= span.start_col
      and cursor_col < span.end_col then
      cursor_span = span
    end
  end

  local cursor_kind = M._status_inline_span_kind(cursor_span)
  if cursor_kind == "add" then return M._status_diff_line_with_prefix(entry, "+") or entry.diff_line end
  if cursor_kind == "delete" then return M._status_diff_line_with_prefix(entry, "-") or entry.diff_line end
  if has_add and not has_delete then return M._status_diff_line_with_prefix(entry, "-") or entry.diff_line end
  if has_delete and not has_add then return M._status_diff_line_with_prefix(entry, "+") or entry.diff_line end
  return entry.diff_line
end

---@param entry DiffReviewStatusEntry?
---@param skip_revision boolean? open the working-tree file even for deleted lines
---@param selected_diff_line? table already resolved jump target for async fallbacks
local function status_jump(entry, skip_revision, selected_diff_line)
  if not (entry and entry.file and entry.file.filename) then return end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local jump_diff_line = selected_diff_line or M._status_jump_diff_line(entry, cursor and cursor[2])
  if not skip_revision and jump_diff_line and jump_diff_line.side == "left" and jump_diff_line.line then
    local rev, path = M._file_revision.target(entry, M._status)
    if rev and path then
      M._file_revision.open({
        rev = rev,
        path = path,
        cwd = M._status.cwd,
        line = jump_diff_line.line,
        on_error = function()
          status_jump(entry, true, jump_diff_line)
        end,
      })
      return
    end
  end
  vim.cmd.edit(vim.fn.fnameescape(entry.file.filename))
  local target_line
  if jump_diff_line and jump_diff_line.line then
    if jump_diff_line.side == "left" and entry.hunk and entry.hunk.diff then
      target_line = closest_current_line_for_deleted_diff_line(entry.hunk.diff, jump_diff_line.line)
    else
      target_line = jump_diff_line.line
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

---@param entry_id string?
---@return DiffReviewStatusEntry?
function M._status_entry_by_id(entry_id)
  if not (entry_id and M._status and M._status.entries) then return nil end
  for _, entry in pairs(M._status.entries) do
    if entry and entry.id == entry_id then return entry end
  end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_default_folded(entry)
  if not entry then return false end
  if entry.kind == "file" or entry.kind == "pr_file" then
    return true
  elseif entry.kind == "commit" or entry.kind == "commit_file" then
    return true
  elseif entry.kind == "pr_comment" and entry.pr_comment then
    return true
  elseif entry.kind == "pr_review" then
    return true
  elseif entry.kind == "pr_review_file" then
    return false
  elseif entry.kind == "section" and entry.section then
    local section_config = status_section_by_name[entry.section.name]
    if section_config then return section_config.default_folded end
    return entry.section.default_folded
  end
  return false
end

---@param entry DiffReviewStatusEntry?
local function status_toggle(entry)
  if not entry then return end
  local fold_id = entry.fold_target_id or entry.id
  if not fold_id then return end
  if M._status and M._status.buf and M._pr_edit.blocks_render(M._status.buf) then return end
  local fold_entry = entry
  if entry.fold_target_id then
    fold_entry = M._status_entry_by_id(fold_id) or { id = fold_id, kind = "pr_head_section" }
  end
  local default = M._status_entry_default_folded(fold_entry)
  local next_folded = not status_folded(fold_id, default)
  if not next_folded then
    status_prewarm_entry_syntax(fold_entry)
  end
  set_status_folded(fold_id, next_folded)
  if fold_entry.kind == "commit" and not next_folded and fold_entry.commit then
    status_load_commit_files(fold_entry.commit)
  end
  render_status_or_notify(M._status.buf, fold_id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
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
          M._status.head_lines = status_build_head_lines(M._status.head_values, M._status.pr, M._status.about, M._status.issues)
          if vim.api.nvim_buf_is_valid(buf) then
            M.render_status(buf, nil, nil, { reuse_sections = true })
          end
        end
      end
    end
    if M._status then
      M._status.remote_action = { action = action, state = "running", status = running_status }
      if M._status.head_values then
        M._status.head_lines = status_build_head_lines(M._status.head_values, M._status.pr, M._status.about, M._status.issues)
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
  require("github.repo_cache").enable_user_completion(buf)
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
        M._pr_overview.load_checks(status.pr, status.cwd, buf)
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
    if M._review.toggle_comment_fold(buf) then return end
    status_toggle(status_entry_under_cursor())
  end)

  map("collapse_parent", "n", function()
    if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
    M._status_collapse_parent()
  end)

  map("visual_line_with_gutter", "n", function()
    M._start_diff_gutter_visual_line(buf)
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
        M._review.sync_inline_comment_text(buf)
        M._pr_overview.sync_standalone_comments(buf)
        M._pr_edit.sync(buf)
      end)
    end
    map("review", "n", function()
      M._review.start(buf)
    end)
    map("open", "n", function()
      if M._status_states and M._status_states[buf] then M._status = M._status_states[buf] end
      if is_pr_view and M._pr_edit.toggle_draft_status_under_cursor(buf) then return end
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
  state.pr_regular_comments = {}
  state.review_comments = M._pr_overview.editable_comments(state)
  if pr.repo and pr.repo ~= "" then vim.b[buf].github_repo = pr.repo end
  M._status = state
  attach_status_state(buf, state)
  setup_status_keymaps(buf)
  M._pr_edit.attach(buf)
  M.github_load_repo_metadata(cwd, pr.repo)

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
  M._pr_overview.load_checks(pr, cwd, buf)
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
  if pr.repo and pr.repo ~= "" then vim.b[buf].github_repo = pr.repo end
  M._review.load_draft(state)
  M._review.normalize_comments(state)
  M._status = state
  attach_status_state(buf, state)
  setup_status_keymaps(buf)
  M._review.attach(buf)
  M.github_load_repo_metadata(cwd, pr.repo)

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

    local command = M._git_diff_command(cwd)
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
