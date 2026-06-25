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
---@field kind "section"|"file"|"hunk"|"context_line"|"commit"|"commit_message"|"commit_file"|"commit_hunk"|"pr_file"|"pr_hunk"|"pr_comment"|"pr_comment_reply"|"pr_review"|"pr_review_file"|"pr_review_hunk"|"review_comment"|"pr"|"about"|"pr_check"|"pr_head_section"|"pr_head_line"
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
---@field commit_subject_start_col? integer
---@field commit_subject_end_col? integer
---@field walkthrough_step? table

---@alias DiffReviewStatusViewKind "status"|"pr"|"diff"|"review"

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

---@alias DiffReviewStatusSectionName "unstaged"|"staged"|"unmerged"|"recent"|"pr_commits"

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
---@field _status_prewarm_hunk_budget fun(hunk_count: integer, options?: DiffReviewConfig): integer
---@field _diff_mutation_queue_model table
---@field _main_win integer?
---@field _saved_wo table<integer, { number: boolean, relativenumber: boolean }>?
---@field suspend_preview boolean?

---@type DiffReviewModule
local M = {}
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
M._pr_overview = require("diff_review.pr_overview")
M._datetime = require("diff_review.datetime")
M._conventional_commit = require("diff_review.conventional_commit")
M._status_conventional_commit_type_end = M._conventional_commit.type_end
M._status_conventional_commit_subject_segments = M._conventional_commit.subject_segments
M._comment_rows = require("github.comment_rows")

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._status_decorate_ns = vim.api.nvim_create_namespace("diff_review_status_decorate")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

local config = require("diff_review.config")
local ai_commit = require("diff_review.ai_commit")
local gh = require("diff_review.gh")
local highlights = require("diff_review.highlights")
local notifications = require("diff_review.notifications")
local syntax_engine = require("diff_review.syntax_engine")
local diff_render = require("diff_review.diff_render")
local status_render = require("diff_review.status_render")
local keymaps = require("diff_review.keymaps")
local hunk_model = require("diff_review.hunk_model")
-- Re-expose the pure hunk model under its original M._hunk_* names so init render
-- helpers and tests reach it unchanged.
for _hunk_name, _hunk_fn in pairs(hunk_model) do M["_hunk_" .. _hunk_name] = _hunk_fn end
M._diff_perf = require("diff_review.perf")
M._diff_source_model = require("diff_review.source")
M._diff_annotation_model = require("diff_review.annotations")
M._diff_layout_model = require("diff_review.layout")
M._diff_mutation_queue_model = require("diff_review.mutation_queue")
M._diff_hunk_index_model = require("diff_review.hunk_index")
M._diff_row_tree_model = require("diff_review.row_tree")
M._diff_source_loader_model = require("diff_review.source_loader")
M._diff_syntax_context_model = require("diff_review.syntax_context")
M._diff_decoration_model = require("diff_review.decoration")
M._diff_view_controller_model = require("diff_review.view_controller")
M._diff_view_command_set_model = require("diff_review.view_command_set")
M._intraline_diff = require("diff_review.intraline_diff")
M._inventory = require("diff_review.inventory")

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

---@param filename string
---@return integer?


---@param buf integer
---@param tree TSTree
---@param query vim.treesitter.Query?
---@param row integer 0-based row
---@param text string
---@return DiffReviewHighlightSegment[]

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
-- Expose the shared filetype resolver so extracted view modules (file_revision)
-- can reuse it without re-implementing buffer/contents detection.
M._detect_filetype = detect_filetype

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

local git_backend = require("diff_review.git_backend")
M.set_git_backend = git_backend.set_backend
M.reset_git_backend = git_backend.reset_backend
M._system_output = git_backend.system_output

local paths = require("diff_review.paths")
local repo_file_path = paths.repo_file_path
local repo_relative = paths.repo_relative
M._repo_relative_for_test = paths.repo_relative_for_test

-- Render accumulators live in status_buffer (state-passing). The locals below stay as
-- thin orchestration wrappers that thread the active M._status into each module call,
-- so existing call sites render into the active buffer's state unchanged.
local status_buffer = require("diff_review.status_buffer")
M._status_buffer = status_buffer


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
  git_backend.git_root_async(function(root, root_err)
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
    git_backend.run_git_at_root_async(root, task.args, nil, function(result)
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
  local root, root_err = git_backend.git_root_sync_for_test_backend()
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
        local result = git_backend.run_git_sync_for_test_backend(args_for_file(relpath))
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
  git_backend.run_git_async({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
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
  git_backend.run_git_async({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
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
  local result = git_backend.run_git_sync_for_test_backend({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
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
  local result = git_backend.run_git_sync_for_test_backend({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
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
  git_backend.systemlist_async(args, function(result, code)
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

--- Compute Tree-sitter scope context for a hunk without blocking UI render.
---@param filename string absolute path
---@param line number 1-based line number
---@param cb fun(context?: DiffReviewHunkTreeSitterContext|string)
function M.compute_hunk_context_async(filename, line, cb)
  local buf = syntax_engine.treesitter_source_buffer(filename)
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
    local context = syntax_engine.hunk_context_from_trees(buf, query, highlight_query, trees, target)
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
  return M._status_perf_span("treesitter.compute_file_syntax_async", M._status and M._status.buf or nil, {
    file = filename,
  }, function()
    local buf = syntax_engine.treesitter_source_buffer(filename)
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

    local parser_ok, parser = M._status_perf_span("treesitter.compute_file_syntax_async.get_parser", M._status and M._status.buf or nil, {
      file = filename,
      lang = lang,
    }, function()
      return pcall(vim.treesitter.get_parser, buf, lang)
    end)
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

    local parse_ok, parsed = M._status_perf_span("treesitter.compute_file_syntax_async.parse_call", M._status and M._status.buf or nil, {
      file = filename,
      lang = lang,
      source_line_count = line_count,
    }, function()
      return pcall(function()
        return parser:parse({ 0, 0, line_count, 0 }, function(first, second)
          local trees = type(first) == "table" and first or second
          vim.schedule(function()
            finish(trees)
          end)
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
  end)
end

---@param filename string
---@param lines string[]
---@param cb fun(syntax?: DiffReviewTreeSitterSyntax)
function M.compute_diff_syntax_async(filename, lines, cb)
  return M._status_perf_span("treesitter.compute_diff_syntax_async", M._status and M._status.buf or nil, {
    file = filename,
    source_line_count = #lines,
  }, function()
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
    M._status_perf_span("treesitter.compute_diff_syntax_async.set_lines", M._status and M._status.buf or nil, {
      file = filename,
      lang = lang,
      source_line_count = #lines,
    }, function()
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    end)

    local parser_ok, parser = M._status_perf_span("treesitter.compute_diff_syntax_async.get_parser", M._status and M._status.buf or nil, {
      file = filename,
      lang = lang,
    }, function()
      return pcall(vim.treesitter.get_parser, buf, lang)
    end)
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

    local parse_ok, parsed = M._status_perf_span("treesitter.compute_diff_syntax_async.parse_call", M._status and M._status.buf or nil, {
      file = filename,
      lang = lang,
      source_line_count = line_count,
    }, function()
      return pcall(function()
        return parser:parse({ 0, 0, line_count, 0 }, function(first, second)
          local trees = type(first) == "table" and first or second
          vim.schedule(function()
            finish(trees)
          end)
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
  end)
end

--- Return cached Tree-sitter context and kick off an async request if needed.
---@param filename string
---@param line number
---@param callback_key string
---@param on_update? fun(context?: DiffReviewHunkTreeSitterContext|string)
---@return DiffReviewHunkTreeSitterContext|string?

---@param filename string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean



---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean

---@param text string?
---@return string

---@param diff_lines string|string[]?
---@return table<string, boolean>



---@class DiffReviewGutterSpec
---@field width integer
---@field old_width integer
---@field new_width integer

---@return DiffReviewGutterSpec

---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
---@param changed_line_hl? string
---@return table[]

---@param row table
---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
---@param changed_line_hl? string

---@param text string
---@param segments? DiffReviewHighlightSegment[]
---@param line_number? integer
---@param gutter? DiffReviewGutterSpec
---@param file? string
---@param old_line? integer
---@param new_line? integer
---@return table

---@param reference_text string?
---@param gutter? DiffReviewGutterSpec
---@return table

---@param header_parts table[]
---@return table

-- Cache for treesitter context per file (cleared on refresh)
M._ts_context_cache = {}
M._ts_source_bufs = {}

---@param value string?
---@param key string
---@param replacement string
---@return string
function M._status_window_option_with_pair(value, key, replacement)
  local option_by_key = {}
  local option_order = {}
  for option in tostring(value or ""):gmatch("[^,]+") do
    local option_key, option_value = option:match("^([^:]+):(.*)$")
    if option_key and option_key ~= "" then
      if option_by_key[option_key] == nil then option_order[#option_order + 1] = option_key end
      option_by_key[option_key] = option_value
    end
  end
  if option_by_key[key] == nil then option_order[#option_order + 1] = key end
  option_by_key[key] = replacement
  local parts = {}
  for _, option_key in ipairs(option_order) do
    parts[#parts + 1] = option_key .. ":" .. option_by_key[option_key]
  end
  return table.concat(parts, ",")
end

---@param win integer?
function M._hide_line_numbers(win)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  M._saved_wo = M._saved_wo or {}
  if not M._saved_wo[win] then
    M._saved_wo[win] = {
      number = vim.wo[win].number,
      relativenumber = vim.wo[win].relativenumber,
      signcolumn = vim.wo[win].signcolumn,
      foldcolumn = vim.wo[win].foldcolumn,
      foldenable = vim.wo[win].foldenable,
      foldlevel = vim.wo[win].foldlevel,
      foldmethod = vim.wo[win].foldmethod,
      foldtext = vim.wo[win].foldtext,
      fillchars = vim.wo[win].fillchars,
      winhighlight = vim.wo[win].winhighlight,
      virtualedit = vim.wo[win].virtualedit,
      wrap = vim.wo[win].wrap,
      linebreak = vim.wo[win].linebreak,
      breakindent = vim.wo[win].breakindent,
      conceallevel = vim.wo[win].conceallevel,
      concealcursor = vim.wo[win].concealcursor,
    }
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].foldcolumn = "0"
  vim.wo[win].virtualedit = "all"
end

---@param win integer?
---@param _state? table
function M._apply_status_window_options(win, _state)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  M._hide_line_numbers(win)
  vim.wo[win].wrap = false
  vim.wo[win].linebreak = false
  vim.wo[win].breakindent = false
  vim.wo[win].conceallevel = 0
  vim.wo[win].concealcursor = ""
  vim.wo[win].foldenable = true
  vim.wo[win].foldlevel = 99
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldtext = "v:lua.diff_review_status_foldtext()"
  vim.wo[win].fillchars = M._status_window_option_with_pair(vim.wo[win].fillchars, "fold", " ")
  vim.wo[win].winhighlight = M._status_window_option_with_pair(vim.wo[win].winhighlight, "Folded", "Normal")
end

---@param win integer?
function M._restore_line_numbers(win)
  if not (win and vim.api.nvim_win_is_valid(win) and M._saved_wo and M._saved_wo[win]) then return end
  local saved = M._saved_wo[win]
  vim.wo[win].number = saved.number
  vim.wo[win].relativenumber = saved.relativenumber
  vim.wo[win].signcolumn = saved.signcolumn or "auto"
  vim.wo[win].foldcolumn = saved.foldcolumn or "0"
  vim.wo[win].foldenable = saved.foldenable
  vim.wo[win].foldlevel = saved.foldlevel or 0
  vim.wo[win].foldmethod = saved.foldmethod or "manual"
  vim.wo[win].foldtext = saved.foldtext or "foldtext()"
  vim.wo[win].fillchars = saved.fillchars or ""
  vim.wo[win].winhighlight = saved.winhighlight or ""
  vim.wo[win].virtualedit = saved.virtualedit or ""
  vim.wo[win].wrap = saved.wrap
  vim.wo[win].linebreak = saved.linebreak
  vim.wo[win].breakindent = saved.breakindent
  vim.wo[win].conceallevel = saved.conceallevel or 0
  vim.wo[win].concealcursor = saved.concealcursor or ""
  M._saved_wo[win] = nil
end

--- Register a view controller per status-like view kind so command vocabulary and
--- render hooks resolve through a narrow per-view boundary instead of inline branches.
function M._status_register_view_controllers()
  local controllers = M._diff_view_controller_model
  local command_sets = M._diff_view_command_set_model
  controllers.reset()
  local keymaps = (M.config or config.options or config.defaults).keymaps or {}
  local after_render_by_kind = {
    status = M._status_after_render_status,
    pr = M._status_after_render_pr,
    review = M._status_after_render_review,
  }
  for _, view_kind in ipairs({ "status", "pr", "review", "diff" }) do
    local command_keys = (view_kind == "review" and keymaps.review) or keymaps.status or {}
    local set = command_sets.new()
    for command_id in pairs(command_keys) do
      command_sets.register(set, command_id, function() end)
    end
    local after_render = after_render_by_kind[view_kind]
    controllers.register(controllers.new({
      view_kind = view_kind,
      command_set = function() return set end,
      after_render = after_render and function(state) after_render(state.buf) end or nil,
    }))
  end
end

---@param opts? DiffReviewConfig
function M.setup(opts)
  M.config = config.setup(opts)
  M._diff_perf.configure_from_diff_review_options(M.config)
  setup_bg_highlights()
  M._status_register_view_controllers()
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
  syntax_engine.clear_treesitter_source_buffers()
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
      local cached = syntax_engine.cached_hunk_context(filename, hunk.pos, "items:" .. filename .. ":" .. hunk.pos)
      local cached_label = syntax_engine.hunk_context_label(cached)
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
  git_backend.systemlist_async({ "git", "-C", cwd, "ls-files", "--others", "--exclude-standard" }, function(output, code)
    results.untracked_output = output
    results.untracked_code = code
    finish_one()
  end)
  git_backend.systemlist_async({ "git", "-C", cwd, "diff", "--cached", "--name-status" }, function(output, code)
    results.staged_name_status = output
    results.staged_name_status_code = code
    finish_one()
  end)
  git_backend.systemlist_async({ "git", "-C", cwd, "diff", "--name-status" }, function(output, code)
    results.unstaged_name_status = output
    results.unstaged_name_status_code = code
    finish_one()
  end)
end

---@param cb fun(items: table[])
---@param _ctx table?
function M.get(cb, _ctx)
  git_backend.git_root_async(function(cwd)
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
        gutter = diff_render.default_hunk_gutter_spec(),
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




---@param hunk DiffReviewHunk?
---@return integer?

---@param parsed_line DiffReviewParsedHunkLine
---@param context DiffReviewHunkTreeSitterContext|string?
---@return boolean


---@alias DiffReviewDiffSyntaxSide "old"|"new"

---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@return string[]

---@param filename string
---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param filename string
---@param diff_text string
---@return string[]?

---@param filename string
---@param diff_text string
---@return boolean

---@param filename string
---@param diff_text string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param hunk_staged? boolean[]
---@param opts? table
---@return boolean

---@param filename string
---@param diff_text string
---@param hunk_staged? boolean[]
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? table

---@param file DiffReviewStatusFile?
---@return string?

---@param hunk_count integer
---@param options? DiffReviewConfig
---@return integer

---@param file DiffReviewStatusFile?
---@param callback_key_prefix string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? { syntax_source?: "file"|"diff" }



---@param parsed_line DiffReviewParsedHunkLine
---@param gutter DiffReviewGutterSpec
---@param file string
---@param syntax? DiffReviewTreeSitterSyntax
---@param syntax_row? integer
---@return table



---@class DiffReviewHunkContextPaddingLine
---@field line_number integer
---@field old_line? integer
---@field new_line? integer
---@field text string







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




---@param left DiffReviewGutterSpec?
---@param right DiffReviewGutterSpec?
---@return DiffReviewGutterSpec

---@param group DiffReviewHunkDisplayGroup
---@param plan DiffReviewHunkRenderPlan


---@param plans DiffReviewHunkRenderPlan[]
---@return DiffReviewHunkDisplayGroup[]











---@param diff_text string
---@param hunk_staged? boolean[]
---@param filename? string
---@param context_callback_key? fun(hunk_line: number): string
---@param on_context_update? fun()
---@param opts? { context_line: integer?, boundary_context: boolean?, suppress_start_boundary: boolean?, suppress_end_boundary: boolean?, suppress_start_boundary_keys?: table<string, boolean>, suppress_end_boundary_keys?: table<string, boolean>, syntax_source?: "file"|"diff", syntax_diff_text?: string, fallback_syntax_diff_text?: string, old_syntax_row_offset?: integer, new_syntax_row_offset?: integer, compact_replacements?: boolean, require_file_match_for_context?: boolean, debug_context?: table }

---@param buf integer
---@param ns integer
---@param rows table[]

--- Render a formatted diff into a buffer using DiffReview's local formatter.
---@param buf number
---@param diff_text string
---@param hunk_staged? boolean[] staged status per hunk (in order)
---@param filename? string

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
        M._status_perf_span("diff.autocmd_cursor_moved", buf, nil, function()
          M._normalize_status_cursor(buf)
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

    diff_render.render_fancy_diff(buf, diff_text, staged_flags, filename)
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
  git_backend.git_root_async(function(cwd)
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
local status_command_specs = require("diff_review.command_specs").specs

M._status_hint_command_ids_by_view = require("diff_review.command_specs").hint_command_ids_by_view

local status_command_specs_by_id = require("diff_review.command_specs").by_id

---@type table<string, DiffReviewSectionConfig>
local status_section_by_name = {}
for _, section in ipairs(status_section_order) do
  status_section_by_name[section.name] = section
end

local function status_folded(key, default, state)
  return status_buffer.folded(state or M._status or {}, key, default)
end

local function set_status_folded(key, folded, state)
  if not state then
    M._status = M._status or {}
    state = M._status
  end
  return status_buffer.set_folded(state, key, folded)
end

---@param value any
---@return string
function M._status_fold_text(value)
  if type(value) == "function" then
    local ok, text = pcall(value)
    if ok then return tostring(text or "") end
  elseif value ~= nil then
    return tostring(value)
  end
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  return vim.fn.getline(fold_start)
end

function _G.diff_review_status_foldtext()
  local buf = vim.api.nvim_get_current_buf()
  local state = M._status_states and M._status_states[buf] or M._status
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  local value = state and state.fold_text_by_start_line and state.fold_text_by_start_line[fold_start] or nil
  return M._status_fold_text(value)
end

---@param fold_id string?
---@param start_line integer?
---@param end_line integer?
---@param default_folded boolean?
---@param fold_text any
function M._status_register_fold_range(fold_id, start_line, end_line, default_folded, fold_text)
  if not (fold_id and start_line and end_line and end_line > start_line) then return end
  local status = M._status
  if not status then return end
  status.fold_ranges_by_id = status.fold_ranges_by_id or {}
  status.fold_range_order = status.fold_range_order or {}
  status.fold_text_by_start_line = status.fold_text_by_start_line or {}
  local range = {
    id = fold_id,
    start_line = start_line,
    end_line = end_line,
    default_folded = default_folded == true,
    fold_text = fold_text,
  }
  local existing = status.fold_ranges_by_id[fold_id]
  if not existing then
    status.fold_ranges_by_id[fold_id] = { range }
  elseif existing.start_line then
    status.fold_ranges_by_id[fold_id] = { existing, range }
  else
    existing[#existing + 1] = range
  end
  status.fold_range_order[#status.fold_range_order + 1] = range
  status.fold_text_by_start_line[start_line] = fold_text
end

---@param state table?
---@param fold_id string?
---@return table[]
function M._status_fold_ranges_for_id(state, fold_id)
  local value = state and state.fold_ranges_by_id and fold_id and state.fold_ranges_by_id[fold_id] or nil
  if not value then return {} end
  if value.start_line then return { value } end
  return value
end

---@param range table
---@return integer
function M._status_fold_range_span(range)
  return (tonumber(range.end_line) or 0) - (tonumber(range.start_line) or 0)
end

---@param view table
---@param ranges table[]
---@param state table?
---@return table
function M._status_view_for_fold_restore(view, ranges, state)
  local line = view and tonumber(view.lnum) or nil
  if not line then return view end
  local adjusted = nil
  for _, range in ipairs(ranges or {}) do
    local start_line = range and range.start_line or nil
    local end_line = range and range.end_line or nil
    if start_line and end_line and line > start_line and line <= end_line and status_folded(range.id, range.default_folded, state) then
      adjusted = adjusted or vim.deepcopy(view)
      adjusted.lnum = start_line
      adjusted.col = 0
      adjusted.curswant = 0
    end
  end
  return adjusted or view
end

---@param buf integer
---@param fold_id string
---@param _folded boolean
---@return boolean
function M._status_set_native_fold_state(buf, fold_id, _folded)
  local state = M._status_states and M._status_states[buf] or M._status
  if type(fold_id) == "string"
    and (fold_id:find("^hunk:")
      or fold_id:find("^commit%-hunk:")
      or fold_id:find("^provider%-hunk:")) then
    return false
  end
  local ranges = M._status_fold_ranges_for_id(state, fold_id)
  if #ranges == 0 then return false end
  local has_native_range = false
  for _, range in ipairs(ranges) do
    if M._status_fold_range_span(range) > 0 then
      has_native_range = true
      break
    end
  end
  if not has_native_range then return false end
  M._status_apply_native_folds(buf)
  return true
end

---@param buf integer?
---@param ranges table[]
---@param win integer?
---@return boolean?
function M._status_native_folded(buf, ranges, win)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and ranges and ranges[1]) then return nil end
  if not (win and vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf) then
    win = vim.fn.bufwinid(buf)
  end
  if win == -1 or not vim.api.nvim_win_is_valid(win) then return nil end
  local range = ranges[1]
  if not (range.start_line and range.end_line and range.end_line > range.start_line) then return nil end
  return vim.api.nvim_win_call(win, function()
    return vim.fn.foldclosed(range.start_line) ~= -1
  end)
end

---@param buf integer
function M._status_apply_native_folds(buf)
  local state = M._status_states and M._status_states[buf] or M._status
  if not (state and state.fold_range_order and vim.api.nvim_buf_is_valid(buf)) then return end
  return M._status_perf_span("status.apply_native_folds", buf, {
    fold_ranges = #(state.fold_range_order or {}),
  }, function()
    local max_line = vim.api.nvim_buf_line_count(buf)
    local ranges = {}
    for _, range in ipairs(state.fold_range_order) do
      ranges[#ranges + 1] = range
    end
    table.sort(ranges, function(left, right)
      local left_span = M._status_fold_range_span(left)
      local right_span = M._status_fold_range_span(right)
      if left_span == right_span then return (left.start_line or 0) > (right.start_line or 0) end
      return left_span < right_span
    end)
    for _, win in ipairs(vim.fn.win_findbuf(buf)) do
      if vim.api.nvim_win_is_valid(win) then
        M._apply_status_window_options(win, state)
        vim.api.nvim_win_call(win, function()
          local view = vim.fn.winsaveview()
          pcall(vim.cmd, "normal! zE")
          for range_index = #ranges, 1, -1 do
            local range = ranges[range_index]
            if range.start_line >= 1
              and range.end_line <= max_line
              and range.end_line > range.start_line
              and status_folded(range.id, range.default_folded, state) then
              pcall(vim.cmd, ("%d,%dfold"):format(range.start_line, range.end_line))
            end
          end
          vim.fn.winrestview(M._status_view_for_fold_restore(view, ranges, state))
        end)
      end
    end
  end)
end

---@param buf integer
function M._status_sync_after_native_folds(buf)
  local state = M._status_states and M._status_states[buf] or M._status
  if not state then return end
  return M._status_perf_span("status.sync_after_native_folds", buf, nil, function()
    if state.view_kind == "pr" then
      M._status_perf_span("status.native_folds.pr_sync_modifiable", buf, nil, function()
        M._pr_edit.sync_modifiable(buf)
      end)
    elseif state.view_kind == "review" then
      M._status_perf_span("status.native_folds.review_sync_modifiable", buf, nil, function()
        M._review.sync_modifiable(buf)
      end)
    elseif state.view_kind == "status" then
      M._status_perf_span("status.native_folds.issues_sync_modifiable", buf, nil, function()
        M._status_issues.sync_modifiable(buf)
      end)
    end
  end)
end

---@param buf integer
function M._status_schedule_native_folds(buf)
  local state = M._status_states and M._status_states[buf] or M._status
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end
  state.native_fold_generation = (state.native_fold_generation or 0) + 1
  local generation = state.native_fold_generation
  vim.defer_fn(function()
    local latest = M._status_states and M._status_states[buf] or M._status
    if not (latest and latest.native_fold_generation == generation and vim.api.nvim_buf_is_valid(buf)) then return end
    M._status_perf_span("status.native_folds_deferred", buf, { generation = generation }, function()
      M._status = latest
      M._status_apply_native_folds(buf)
      M._status_sync_after_native_folds(buf)
    end)
  end, 20)
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
      M._status_perf_span("status.autocmd_buf_enter", buf, nil, function()
        local current = M._status_states and M._status_states[buf] or nil
        if current then M._status = current end
        M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        M._status_apply_native_folds(buf)
        M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_buf_win_enter", buf, nil, function()
        local current = M._status_states and M._status_states[buf] or nil
        if current then M._status = current end
        M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        M._status_apply_native_folds(buf)
        if current and current.view_kind == "review" and M._review and M._review.refresh_inline_comment_rules then
          M._review.refresh_inline_comment_rules(buf, vim.api.nvim_get_current_win())
        end
        M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_cursor_moved", buf, nil, function()
        M._normalize_status_cursor(buf)
        if M._status_issues then M._status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("ModeChanged", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_mode_changed", buf, nil, function()
        M._normalize_status_cursor(buf)
        if M._status_issues then M._status_issues.sync_modifiable(buf) end
      end)
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
  git_backend.systemlist_async(command, function(result, code)
    if code ~= 0 then
      cb(nil)
      return
    end
    cb(vim.trim(result[1] or ""))
  end)
end

local function status_head_row(name, oid, ref, ref_hl, subject, date_text, ref_width)
  local segments = {}
  local byte_col = 0
  local function add_segment(text, hl_group)
    text = tostring(text or "")
    segments[#segments + 1] = { text, hl_group }
    byte_col = byte_col + #text
  end

  add_segment(("%-8s"):format(name .. ":"), "DiffReviewStatusLabel")
  add_segment(("%-7s"):format(oid or ""), "DiffReviewStatusObjectId")
  if date_text and date_text ~= "" then
    add_segment(" ")
    add_segment(date_text, "DiffReviewStatusDate")
  end
  local ref_text = ref or ""
  local ref_padding = ""
  if ref_width and ref_width > 0 then
    local padding_width = ref_width - vim.fn.strdisplaywidth(ref_text)
    if padding_width > 0 then ref_padding = string.rep(" ", padding_width) end
  end
  add_segment(" ")
  add_segment(ref_text, ref_hl)
  if ref_padding ~= "" then add_segment(ref_padding) end
  add_segment(" ")

  local subject_start_col = byte_col
  for _, segment in ipairs(M._status_conventional_commit_subject_segments(subject or "", nil)) do
    add_segment(segment[1], segment[2])
  end
  local subject_end_col = byte_col
  return segments, subject_start_col, subject_end_col
end

function M._status_commit_message_entry(commit, source, start_col, end_col)
  if type(commit) ~= "table" then return nil end
  local oid = vim.trim(tostring(commit.oid or commit.sha or commit.id or ""))
  local subject = tostring(commit.subject or commit.messageHeadline or commit.message or "")
  if oid == "" and subject == "" then return nil end
  return {
    id = ("commit-message:%s:%s"):format(tostring(source or "commit"), oid ~= "" and oid or vim.fn.sha256(subject):sub(1, 12)),
    kind = "commit_message",
    commit = commit,
    commit_subject_start_col = start_col,
    commit_subject_end_col = end_col,
  }
end

function M._status_head_commit_line(name, oid, ref, ref_hl, subject, date_text, ref_width, commit)
  local segments, subject_start_col, subject_end_col = status_head_row(name, oid, ref, ref_hl, subject, date_text, ref_width)
  return {
    segments = segments,
    entry = M._status_commit_message_entry(commit, name, subject_start_col, subject_end_col),
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

M._status_issues = require("diff_review.status_issues")

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
  lines[#lines + 1] = M._status_head_commit_line(
    "Head",
    values.head_oid or "0000000",
    values.branch or "(detached)",
    "DiffReviewStatusBranch",
    values.subject or "(no commits)",
    nil,
    ref_width,
    {
      oid = "HEAD",
      short_oid = values.head_oid or "0000000",
      subject = values.subject or "(no commits)",
    }
  )

  if values.upstream then
    lines[#lines + 1] = M._status_head_commit_line(
      "Merge",
      values.upstream_oid or "",
      values.upstream,
      "DiffReviewStatusRemote",
      values.upstream_subject or "",
      nil,
      ref_width,
      {
        oid = values.upstream,
        short_oid = values.upstream_oid or "",
        subject = values.upstream_subject or "",
      }
    )
  end

  if remote_action and remote_action.action == "push" then
    lines[#lines + 1] = {
      segments = {
        { ("%-8s"):format("Push:"), "DiffReviewStatusLabel" },
        { remote_action.status or "Pushing...", "DiffReviewStatusFetching" },
      },
    }
  elseif values.push_ref then
    lines[#lines + 1] = M._status_head_commit_line(
      "Push",
      values.push_oid or "",
      values.push_ref,
      "DiffReviewStatusRemote",
      values.push_subject or "",
      nil,
      ref_width,
      {
        oid = values.push_ref,
        short_oid = values.push_oid or "",
        subject = values.push_subject or "",
      }
    )
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
---@return table? commit
local function status_pr_head_subject(pr)
  local commits = pr.commits or {}
  if type(commits) ~= "table" then return "", "", nil end
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
  if type(commit) ~= "table" then return "", "", nil end
  local date_text = M._datetime.relative(
    commit.committedDate or commit.committed_date or commit.committed_at or commit.authoredDate or commit.authored_date or commit.authored_at,
    { yesterday = false }
  )
  return tostring(commit.messageHeadline or commit.subject or commit.message or ""), date_text, commit
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
  local head_subject, head_date, head_commit = status_pr_head_subject(pr)
  local status_commit = M._pr_overview.status_commit_from_pr_commit(head_commit, pr.headRefName)
    or {
      oid = pr.headRefOid,
      short_oid = status_short_oid(pr.headRefOid),
      subject = head_subject,
    }
  lines[#lines + 1] = M._status_head_commit_line(
    "Head",
    status_short_oid(pr.headRefOid),
    pr.headRefName or "",
    "DiffReviewStatusBranch",
    head_subject,
    head_date,
    nil,
    status_commit
  )
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
    entry = { id = description_section_id, kind = "pr_head_section", default_folded = false },
  }
  if not status_folded(description_section_id, false, status) then
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
  return status_buffer.add_highlight(M._status, line, start_col, end_col, hl_group, priority)
end

local function status_add_extmark(line, col, opts)
  return status_buffer.add_extmark(M._status, line, col, opts)
end

local function status_add_line(text, entry, line_hl_group)
  return status_buffer.add_line(M._status, text, entry, line_hl_group)
end

M._status_segment_line_parts = status_buffer.segment_line_parts

local function status_add_segment_line(segments, entry)
  return status_buffer.add_segment_line(M._status, segments, entry)
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


---@return DiffReviewStatusKeymapConfig

---@param spec DiffReviewStatusCommandSpec
---@return boolean

---@param spec DiffReviewStatusCommandSpec
---@param view_kind DiffReviewStatusViewKind
---@return boolean

---@param command_id string
---@return string[]

---@param command_id string
---@return string
local function status_primary_key(command_id)
  return keymaps.status_keys_for(command_id)[1] or ""
end

---@param keys string[]
---@return string
local function status_key_text(keys)
  return table.concat(keys, ", ")
end

---@param state? table
---@return table[]

---@param state table
---@return string

---@param segments table[]
---@param title string
---@return string

---@param buf integer
---@param win? integer

---@param win integer

local function status_add_fancy_row(row, entry, indent)
  return status_buffer.add_fancy_row(M._status, row, entry, indent)
end

---@param item table
---@return "unstaged"|"staged"
local function status_section_for_item(item)
  local data = item.item or {}
  if data.staged then return "staged" end
  return "unstaged"
end

---@param buf integer?
---@param file DiffReviewStatusFile
---@return integer?
function M._status_walkthrough_file_rank(buf, file)
  if not buf then return nil end
  local walkthrough = package.loaded["diff_review.walkthrough"]
  if not (walkthrough and type(walkthrough.file_sort_rank) == "function") then return nil end
  local ok, rank = pcall(walkthrough.file_sort_rank, buf, file)
  if ok and type(rank) == "number" then return rank end
  return nil
end

---@param file DiffReviewStatusFile
---@return string
function M._status_file_path_sort_key(file)
  return tostring((file and (file.relpath or file.filename)) or "")
end

---@param buf integer?
---@param section DiffReviewStatusSection
function M._status_sort_section_files(buf, section)
  if not (section and type(section.files) == "table") then return end
  local ranks = {}
  for _, file in ipairs(section.files) do
    ranks[file] = M._status_walkthrough_file_rank(buf, file) or math.huge
  end
  table.sort(section.files, function(left_file, right_file)
    local left_rank = ranks[left_file] or math.huge
    local right_rank = ranks[right_file] or math.huge
    if left_rank ~= right_rank then return left_rank < right_rank end
    return M._status_file_path_sort_key(left_file) < M._status_file_path_sort_key(right_file)
  end)
end

---@param buf integer?
---@param sections DiffReviewStatusSection[]
function M._status_sort_sections_for_render(buf, sections)
  for _, section in ipairs(sections or {}) do
    M._status_sort_section_files(buf, section)
  end
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
      local is_untracked = data.category == "Untracked Files" or data.git_status == "??"
      local file = section.files_by_name[filename]
      if not file then
        file = {
          filename = filename,
          relpath = vim.fn.fnamemodify(filename, ":."),
          section_name = section_name,
          added = 0,
          removed = 0,
          hunks = {},
          untracked = section_name ~= "staged" and is_untracked,
          status = data.stats or data.hunk_header or "",
          git_status = data.git_status,
          original_relpath = data.git_original_file,
        }
        section.files_by_name[filename] = file
        section.files[#section.files + 1] = file
      elseif section_name ~= "staged" and is_untracked then
        file.untracked = true
        file.git_status = "??"
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
    M._status_sort_section_files(M._status and M._status.buf or nil, section)
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

-- Expose the diff-to-files provider so section_builder can assemble sections without
-- pulling the diff parser out of init.
M._status_files_from_diff_provider = status_files_from_diff_provider
M._section_builder = require("diff_review.section_builder")














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
  git_backend.systemlist_async(command, function(output, code)
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
  local args = { "-30" }
  if upstream and upstream ~= "" then
    args[#args + 1] = upstream
  end
  status_commit_log_section_async(cwd, {
    name = "recent",
    title = "Recent Commits",
    args = args,
    branch = branch,
    default_folded = true,
    limit = 30,
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
    M._status_sort_section_files(M._status and M._status.buf or nil, section)
    for _, file in ipairs(section.files) do
      file.section_name = section.name
      file.untracked = section.name ~= "staged" and file.untracked == true
      if file.untracked then
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
  copied_file.untracked = section_name ~= "staged" and file.untracked == true
  if copied_file.untracked then
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
  local is_untracked = section_name ~= "staged" and file.untracked == true
  local git_status = file.git_status
  if is_untracked then
    git_status = "??"
  elseif section_name == "staged" and file.untracked then
    git_status = "A"
  end
  existing_file = {
    filename = file.filename,
    relpath = file.relpath,
    section_name = section_name,
    added = 0,
    removed = 0,
    hunks = {},
    untracked = is_untracked,
    status = file.status,
    git_status = git_status,
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
  return M._status_perf_span("status.diff_hunks_for_file", M._status and M._status.buf or nil, {
    file = file and file.filename or nil,
    relpath = file and file.relpath or nil,
    existing_hunk_count = file and file.hunks and #file.hunks or nil,
    untracked = file and file.untracked or nil,
  }, function()
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
  end)
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
  return M._status_perf_span("status.display_hunks", M._status and M._status.buf or nil, {
    hunk_count = #(hunks or {}),
  }, function()
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
  end)
end


local status_cursor_target
local status_operations_pending
local status_request_reconcile

---@param entry_kind? string
---@return "file"|"diff"

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param previous_hunk? DiffReviewHunk
---@param next_hunk? DiffReviewHunk
---@param entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"
---@param hunk_key_override? string

---@param diff_line string?
---@return boolean
function M._status_lazy_diff_body_line(diff_line)
  return M._diff_source_model.diff_body_line(diff_line)
end

---@param lines string[]
---@return integer added
---@return integer removed
function M._status_lazy_diff_stats(lines)
  return M._diff_source_model.diff_stats(lines)
end

---@param file DiffReviewStatusFile
---@param comment table
---@return boolean
function M._status_lazy_comment_matches_file(file, comment)
  if not (file and comment) then return false end
  local targets = {
    comment.abs_file,
    comment.path,
  }
  for _, target in ipairs(targets) do
    if target and target ~= "" then
      local normalized = vim.fs.normalize(tostring(target))
      if normalized == vim.fs.normalize(tostring(file.filename or "")) then return true end
      if normalized == vim.fs.normalize(tostring(file.relpath or "")) then return true end
    end
  end
  return false
end

---@param comment table
---@return integer
function M._status_lazy_comment_row_estimate(comment)
  if not comment or comment.local_state == "deleted" then return 0 end
  if comment.review_folded == true then return 1 end
  local body_count = #M._review.comment_body_lines(comment.body or "")
  local reply_count = 0
  for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    reply_count = reply_count + 1 + #M._review.comment_body_lines(reply.body or "")
  end
  return 2 + body_count + reply_count
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return integer
function M._status_lazy_hunk_comment_estimate(file, hunk)
  local total = 0
  local seen = {}
  local sources = {
    file and file.pr_comments or {},
    file and file.pr_review_comments or {},
    M._status and M._status.review_comments or {},
  }
  for _, comments in ipairs(sources) do
    for _, comment in ipairs(type(comments) == "table" and comments or {}) do
      local key = comment.local_id or comment.remote_node_id or comment.remote_id or comment.id or comment
      if not seen[key]
        and M._status_lazy_comment_matches_file(file, comment)
        and M._pr_overview.hunk_contains_comment(hunk, comment) then
        seen[key] = true
        total = total + M._status_lazy_comment_row_estimate(comment)
      end
    end
  end
  return total
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return integer
function M._status_lazy_hunk_estimate(file, hunk)
  local lazy_estimate = tonumber(hunk and hunk.lazy_estimate)
  if lazy_estimate then return math.max(1, lazy_estimate + M._status_lazy_hunk_comment_estimate(file, hunk)) end
  local count = 0
  local in_hunk = false
  for _, line in ipairs(vim.split(tostring(hunk and hunk.diff or ""), "\n", { plain = true })) do
    if line:match("^@@ ") then
      count = count + 1
      in_hunk = true
    elseif in_hunk and M._status_lazy_diff_body_line(line) then
      count = count + 1
    end
  end
  return math.max(1, count + M._status_lazy_hunk_comment_estimate(file, hunk))
end


--- Resolve the per-file diff-body row budget for the size gate, or nil when disabled.
--- Bounds how many real rows a single file body renders up front so a huge diff does
--- not freeze the render; the rest loads on demand through its load-more row.
---@return integer?
function M._status_file_render_row_budget()
  local options = M.config or config.options or config.defaults
  local base = tonumber(options.status_diff_viewport_threshold) or 0
  if base <= 0 then return nil end
  return base
end

--- Count the leading display hunks a file body must render regardless of the row
--- budget, so activating its load-more row always reveals more of a huge diff.
---@param file_key string
---@return integer
function M._status_file_forced_hunk_count(file_key)
  return (M._status and M._status.file_render_limits and M._status.file_render_limits[file_key]) or 0
end

--- Decide whether the size gate should defer the next hunk behind a load-more row.
--- Always render the first hunk and any force-loaded hunks, then stop once the budget
--- is reached or the next hunk would overshoot it, so one giant hunk cannot freeze the
--- render while load-more still guarantees progress.
---@param rendered_rows integer rows already emitted for this file body
---@param next_estimate integer estimated rendered rows of the next hunk
---@param hunk_index integer 1-based index of the next hunk
---@param forced_hunks integer leading hunks to render regardless of the budget
---@param budget integer? row budget, or nil when the gate is disabled
---@return boolean
function M._status_size_gate_should_defer(rendered_rows, next_estimate, hunk_index, forced_hunks, budget)
  if not budget then return false end
  if hunk_index <= 1 or hunk_index <= forced_hunks then return false end
  return rendered_rows >= budget or (rendered_rows + (next_estimate or 0)) > budget
end


---@param file? DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@return DiffReviewDiffSourceKind
function M._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
  if entry_kind == "commit_file" or hunk_entry_kind == "commit_hunk" then return "commit" end
  if entry_kind == "pr_review_file" or hunk_entry_kind == "pr_review_hunk" then return "review" end
  if entry_kind == "pr_file" or hunk_entry_kind == "pr_hunk" then return "pr" end
  if M._status and M._status.view_kind == "diff" then return "branch" end
  if file and file.section_name == "staged" then return "staged" end
  return "unstaged"
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@return string
function M._status_diff_source_id(file, entry_kind, hunk_entry_kind)
  if file and file.diff_source_id then return file.diff_source_id end
  local section_name = file and file.section_name or nil
  if section_name and section_name ~= "" then return section_name end
  return M._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
end

---@param status table
---@return DiffReviewDiffSourceRegistry
function M._status_diff_source_registry(status)
  status.diff_source_registry = status.diff_source_registry or M._diff_source_model.new_registry()
  return status.diff_source_registry
end

---@param registry DiffReviewDiffSourceRegistry
function M._status_configure_diff_source_policies(registry)
  if not registry then return end
  M._diff_source_model.set_kind_policy(registry, "unstaged", {
    stage = true,
    discard = true,
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "staged", {
    unstage = true,
    discard = true,
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "pr", {
    comment = true,
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "review", {
    comment = true,
    viewed = true,
    unviewed = true,
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "commit", {
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "branch", {
    jump = true,
  })
  M._diff_source_model.set_kind_policy(registry, "walkthrough", {
    jump = true,
  })
end

---@param commit DiffReviewStatusCommit
---@return string?
function M._status_commit_source_id(commit)
  if not (commit and commit.oid) then return nil end
  return "commit:" .. tostring(commit.oid)
end

---@param commit DiffReviewStatusCommit
---@param status table
---@return DiffReviewDiffSourceHandle?
function M._status_commit_source_handle(commit, status)
  local source_id = M._status_commit_source_id(commit)
  if not (source_id and status and status.cwd) then return nil end
  local cwd = status.cwd
  return {
    id = source_id,
    kind = "commit",
    label = commit.short_oid or commit.oid,
    cwd = cwd,
    commit_oid = commit.oid,
    lazy = true,
    metadata = {
      view_kind = status.view_kind,
      subject = commit.subject,
    },
    load = function(source_state, done)
      M._status_perf_event("source.commit.load.start", status.buf, {
        source_id = source_id,
        commit_oid = commit.oid,
      })
      git_backend.systemlist_async(M._git_show_diff_command(cwd, commit.oid), function(output, code, stderr)
        if code ~= 0 then
          local message = vim.trim(tostring(stderr or ""))
          done(false, message ~= "" and message or "Unable to load commit diff")
          return
        end
        local diff_text = table.concat(output or {}, "\n")
        local files = M._status_perf_span("source.commit.parse", status.buf, {
          source_id = source_id,
          diff_len = #diff_text,
        }, function()
          return status_commit_files_from_diff(cwd, commit, diff_text)
        end)
        M._status_populate_commit_source_files(source_state, commit, files, status)
        source_state.metadata = source_state.metadata or {}
        source_state.metadata.files = files
        source_state.metadata.diff_len = #diff_text
        M._status_perf_event("source.commit.load.done", status.buf, {
          source_id = source_id,
          file_count = #files,
        })
        done(true)
      end)
    end,
  }
end

---@param commit DiffReviewStatusCommit
function M._status_register_commit_source_handle(commit)
  local status = M._status
  if not (status and status.diff_source_registry and commit and commit.oid) then return end
  local handle = M._status_commit_source_handle(commit, status)
  if not handle then return end
  M._diff_source_model.ensure_handle(status.diff_source_registry, handle)
  commit.diff_source_id = handle.id
end

---@param commit DiffReviewStatusCommit
---@return DiffReviewDiffSourceState?
function M._status_ensure_commit_source_state(commit)
  local status = M._status
  if not (status and status.diff_source_registry and commit and commit.oid) then return nil end
  local handle = M._status_commit_source_handle(commit, status)
  if not handle then return nil end
  local loader = M._diff_source_loader_model.ensure(status.diff_source_registry, handle)
  commit.diff_source_id = handle.id
  return loader.source
end

---@param comment table
---@return string?
function M._status_annotation_id(comment)
  if not comment then return nil end
  local id = comment.local_id or comment.remote_node_id or comment.remote_id or comment.id
  if id then return tostring(id) end
  return nil
end

---@param comment table
---@return DiffReviewAnnotationState
function M._status_annotation_state(comment)
  if not comment then return "clean" end
  if comment.local_state == "deleted" then return "deleted" end
  if comment.local_state == "error" then return "error" end
  if comment.local_state == "new" then return "new" end
  if comment.local_state == "dirty" then return "dirty" end
  if comment.local_state == "syncing" then return "syncing" end
  if comment.dirty == true then return "dirty" end
  return "clean"
end

---@param file_state DiffReviewDiffFileState
---@param comment table
---@param kind string
function M._status_add_diff_file_annotation(file_state, comment, kind)
  local id = M._status_annotation_id(comment)
  if not id then return end
  local annotation = M._diff_annotation_model.upsert(file_state.annotation_index, {
    id = id,
    kind = kind,
    source_id = file_state.source_id,
    file_key = file_state.key,
    path = file_state.path,
    side = comment.side or comment.original_side or comment.start_side or "RIGHT",
    line = tonumber(comment.line or comment.original_line or comment.start_line) or 0,
    end_line = tonumber(comment.end_line) or nil,
    body = comment.body or "",
    base_body = comment.base_body or comment.body or "",
    remote_body = comment.remote_body or comment.body or "",
    author = comment.author,
    remote_id = comment.remote_node_id or comment.remote_id or comment.id,
    editable = comment.editable == true,
    state = M._status_annotation_state(comment),
    metadata = {
      comment = comment,
      renderer = kind,
    },
  })
  M._diff_source_model.add_annotation(file_state, annotation)
end

---@param file DiffReviewStatusFile
---@param source_kind DiffReviewDiffSourceKind
---@return DiffReviewDiffFileStageState
function M._status_diff_stage_state(file, source_kind)
  if source_kind == "review" then return "unviewed" end
  if source_kind == "commit" or source_kind == "pr" or source_kind == "branch" then return "readonly" end
  if file and file.section_name == "staged" then return "staged" end
  return "unstaged"
end

---@param file DiffReviewStatusFile
---@param status table?
---@return string
function M._status_diff_file_path(file, status)
  if file and status and status.cwd and file.filename then
    local relative = repo_relative(file.filename, status.cwd)
    if relative and relative ~= "" then return relative:gsub("\\", "/") end
  end
  local relpath = file and file.relpath or nil
  if relpath and relpath ~= "" and not relpath:match("^%a:[/\\]") and not relpath:match("^/") then
    return relpath:gsub("\\", "/")
  end
  return tostring((file and (file.filename or file.relpath)) or ""):gsub("\\", "/")
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param status table
---@param source_kind string
function M._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
  if not (file_state and file and status and status.cwd) then return end
  local relpath = M._status_diff_file_path(file, status)
  if source_kind == "commit" and file.diff_source_id then
    local commit_oid = tostring(file.diff_source_id):gsub("^commit:", "")
    M._diff_source_model.set_text_loader(file_state, "new", function(done)
      git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s:%s"):format(commit_oid, relpath) }, function(lines, code, output)
        if code ~= 0 then
          done(false, nil, nil, vim.trim(tostring(output or "")))
          return
        end
        done(true, table.concat(lines or {}, "\n"), commit_oid)
      end)
    end)
    M._diff_source_model.set_text_loader(file_state, "old", function(done)
      git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s^:%s"):format(commit_oid, file.original_relpath or relpath) }, function(lines, code, output)
        if code ~= 0 then
          done(false, nil, nil, vim.trim(tostring(output or "")))
          return
        end
        done(true, table.concat(lines or {}, "\n"), commit_oid .. "^")
      end)
    end)
    return
  end
  M._diff_source_model.set_text_loader(file_state, "new", function(done)
    local ok, lines = pcall(vim.fn.readfile, file.filename)
    if not ok then
      done(false, nil, nil, tostring(lines))
      return
    end
    done(true, table.concat(lines or {}, "\n"), "worktree")
  end)
  local old_revision = nil
  if source_kind == "unstaged" then
    old_revision = ":0"
  elseif source_kind == "staged" then
    old_revision = "HEAD"
  elseif source_kind == "branch" and status.diff_branch then
    old_revision = status.diff_branch
  elseif source_kind == "commit" and file.diff_source_id then
    old_revision = tostring(file.diff_source_id):gsub("^commit:", "")
  end
  if not old_revision or old_revision == "" then return end
  M._diff_source_model.set_text_loader(file_state, "old", function(done)
    git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s:%s"):format(old_revision, relpath) }, function(lines, code, output)
      if code ~= 0 then
        done(false, nil, nil, vim.trim(tostring(output or "")))
        return
      end
      done(true, table.concat(lines or {}, "\n"), old_revision)
    end)
  end)
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  local status = M._status
  if not (status and file) then return nil end
  local source_id = M._status_diff_source_id(file, entry_kind, hunk_entry_kind)
  local source_kind = M._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
  local source_handle = {
    id = source_id,
    kind = source_kind,
    label = file.section_name,
    cwd = status.cwd,
    lazy = false,
    metadata = {
      view_kind = status.view_kind,
      section_name = file.section_name,
    },
  }
  if (source_kind == "unstaged" or source_kind == "staged") and M._status_source_reload_paths_loader then
    source_handle.reload_paths = M._status_source_reload_paths_loader(source_kind, source_id, status)
  end
  local registry = M._status_diff_source_registry(status)
  local file_path = M._status_diff_file_path(file, status)
  local file_state = M._diff_source_loader_model.ensure_file(registry, source_handle, file_path, {
    original_path = file.original_relpath,
    status = file.git_status or file.status,
    stage_state = M._status_diff_stage_state(file, source_kind),
    added = file.added,
    removed = file.removed,
    expanded = status_folded(file_key, false) == false,
    metadata = {
      status_file = file,
      file_key = file_key,
      entry_kind = entry_kind,
      hunk_entry_kind = hunk_entry_kind,
    },
  })
  file_state.source_id = source_id
  file_state.status = file.git_status or file.status
  file_state.stage_state = M._status_diff_stage_state(file, source_kind)
  file_state.added = file.added
  file_state.removed = file.removed
  file_state.expanded = status_folded(file_key, false) == false
  file_state.metadata = file_state.metadata or {}
  file_state.metadata.status_file = file
  file_state.metadata.file_key = file_key
  file_state.metadata.entry_kind = entry_kind
  file_state.metadata.hunk_entry_kind = hunk_entry_kind
  file.diff_source_id = source_id
  file.diff_file_key = file_state.key
  file_state.syntax_context = M._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
  M._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
  return file_state
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
function M._status_populate_diff_file_hunks(file_state, file, hunks)
  local file_path = M._status_diff_file_path(file, M._status)
  for hunk_index, hunk in ipairs(hunks or {}) do
    hunk.source_id = file_state.source_id
    hunk.file_key = file_state.key
    hunk.raw_hunks = hunk.raw_hunks or M._diff_source_model.raw_hunks_from_diff(hunk.diff or "", {
      id_prefix = ("%s:%s:%d"):format(file_state.source_id, file_path, hunk_index),
      source_id = file_state.source_id,
      file_key = file_state.key,
      staged = hunk.staged,
      metadata = {
        status_hunk = hunk,
      },
    })
    for _, raw_hunk in ipairs(hunk.raw_hunks or {}) do
      M._diff_source_model.add_raw_hunk(file_state, raw_hunk)
    end
  end
end

---@param source_state DiffReviewDiffSourceState
---@param commit DiffReviewStatusCommit
---@param files DiffReviewStatusFile[]
---@param status table
function M._status_populate_commit_source_files(source_state, commit, files, status)
  if not (source_state and commit and status) then return end
  source_state.file_by_key = {}
  source_state.file_order = {}
  source_state.revision = (source_state.revision or 0) + 1
  for _, file in ipairs(files or {}) do
    file.diff_source_id = source_state.handle.id
    local file_key = status_commit_file_key(commit.oid, file.filename)
    local file_path = M._status_diff_file_path(file, status)
    local file_state = M._diff_source_model.ensure_file(source_state, file_path, {
      original_path = file.original_relpath,
      status = file.git_status or file.status,
      stage_state = "readonly",
      added = file.added,
      removed = file.removed,
      expanded = status_folded(file_key, true) == false,
      metadata = {
        status_file = file,
        file_key = file_key,
        entry_kind = "commit_file",
        hunk_entry_kind = "commit_hunk",
        commit_oid = commit.oid,
      },
    })
    file.diff_file_key = file_state.key
    file_state.hunks = {}
    file_state.hunk_index_by_id = {}
    file_state.annotations = {}
    file_state.annotation_index = M._diff_annotation_model.new_index()
    file_state.layout = nil
    file_state.body_layout = nil
    file_state.layout_dirty = true
    file_state.status = file.git_status or file.status
    file_state.stage_state = "readonly"
    file_state.added = file.added
    file_state.removed = file.removed
    file_state.expanded = status_folded(file_key, true) == false
    file_state.metadata = file_state.metadata or {}
    file_state.metadata.status_file = file
    file_state.metadata.file_key = file_key
    file_state.metadata.entry_kind = "commit_file"
    file_state.metadata.hunk_entry_kind = "commit_hunk"
    file_state.metadata.commit_oid = commit.oid
    file_state.syntax_context = M._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
    M._status_set_diff_file_text_loaders(file_state, file, status, "commit")
    M._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  end
end

---@param source_state DiffReviewDiffSourceState
---@param files DiffReviewStatusFile[]
---@param status table
---@param source_kind DiffReviewDiffSourceKind
---@param source_id string
function M._status_populate_reloaded_source_files(source_state, files, status, source_kind, source_id)
  if not (source_state and status and source_kind and source_id) then return end
  for _, file in ipairs(files or {}) do
    file.diff_source_id = source_id
    local file_key = status_file_key(source_kind, file.filename)
    local file_path = M._status_diff_file_path(file, status)
    local file_state = M._diff_source_model.ensure_file(source_state, file_path, {
      original_path = file.original_relpath,
      status = file.git_status or file.status,
      stage_state = M._status_diff_stage_state(file, source_kind),
      added = file.added,
      removed = file.removed,
      expanded = status_folded(file_key, true) == false,
      metadata = {
        status_file = file,
        file_key = file_key,
        entry_kind = "file",
        hunk_entry_kind = "hunk",
      },
    })
    file.diff_file_key = file_state.key
    file_state.hunks = {}
    file_state.hunk_index_by_id = {}
    file_state.annotations = {}
    file_state.annotation_index = M._diff_annotation_model.new_index()
    file_state.layout = nil
    file_state.body_layout = nil
    file_state.layout_dirty = true
    file_state.pending = false
    file_state.stale = false
    file_state.status = file.git_status or file.status
    file_state.stage_state = M._status_diff_stage_state(file, source_kind)
    file_state.added = file.added
    file_state.removed = file.removed
    file_state.expanded = status_folded(file_key, true) == false
    file_state.metadata = file_state.metadata or {}
    file_state.metadata.status_file = file
    file_state.metadata.file_key = file_key
    file_state.metadata.entry_kind = "file"
    file_state.metadata.hunk_entry_kind = "hunk"
    file_state.syntax_context = M._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
    M._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
    M._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  end
end

---@param source_kind DiffReviewDiffSourceKind
---@param source_id string
---@param status table
---@return fun(source_state: DiffReviewDiffSourceState, paths: string[], done: fun(ok: boolean, err?: string))
function M._status_source_reload_paths_loader(source_kind, source_id, status)
  return function(source_state, paths, done)
    local cwd = status and status.cwd
    if not cwd then
      done(false, "missing git root")
      return
    end
    local extra_args = {}
    if source_kind == "staged" then extra_args[#extra_args + 1] = "--cached" end
    if #(paths or {}) > 0 then
      extra_args[#extra_args + 1] = "--"
      for _, path in ipairs(paths or {}) do
        extra_args[#extra_args + 1] = path
      end
    end
    M._status_perf_event("source.reload_paths.start", status.buf, {
      source_id = source_id,
      source_kind = source_kind,
      paths = paths,
    })
    git_backend.systemlist_async(M._git_diff_command(cwd, extra_args), function(output, code, stderr)
      if code ~= 0 then
        local message = vim.trim(tostring(stderr or ""))
        done(false, message ~= "" and message or "git diff failed")
        return
      end
      for _, path in ipairs(paths or {}) do
        M._diff_source_model.remove_file(source_state, path)
      end
      local diff_text = table.concat(output or {}, "\n")
      local files = status_files_from_diff_provider(cwd, {
        section_name = source_id,
        default_status = "modified",
      }, diff_text)
      M._status_populate_reloaded_source_files(source_state, files, status, source_kind, source_id)
      source_state.metadata = source_state.metadata or {}
      source_state.metadata.last_reload_paths = vim.deepcopy(paths or {})
      M._status_perf_event("source.reload_paths.done", status.buf, {
        source_id = source_id,
        source_kind = source_kind,
        path_count = #(paths or {}),
        file_count = #files,
      })
      done(true)
    end)
  end
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
function M._status_populate_diff_file_annotations(file_state, file)
  local status = M._status
  for _, comment in ipairs(type(file.pr_comments) == "table" and file.pr_comments or {}) do
    M._status_add_diff_file_annotation(file_state, comment, "pr_comment")
  end
  for _, comment in ipairs(type(file.pr_review_comments) == "table" and file.pr_review_comments or {}) do
    M._status_add_diff_file_annotation(file_state, comment, "pr_review_comment")
  end
  for _, comment in ipairs(type(status and status.review_comments) == "table" and status.review_comments or {}) do
    if M._status_lazy_comment_matches_file(file, comment) then
      M._status_add_diff_file_annotation(file_state, comment, "draft_comment")
    end
  end
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return string[]
function M._status_diff_hunk_annotation_ids(file_state, file, hunk)
  local ids = {}
  for _, annotation in ipairs(file_state.annotations or {}) do
    local comment = annotation.metadata and annotation.metadata.comment or nil
    if comment and M._status_lazy_comment_matches_file(file, comment) and M._pr_overview.hunk_contains_comment(hunk, comment) then
      ids[#ids + 1] = annotation.id
    end
  end
  return ids
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param display_hunks DiffReviewHunk[]
function M._status_build_diff_file_layout(file_state, file, display_hunks)
  local key_by_item = {}
  local body_blocks = {}
  for hunk_index, hunk in ipairs(display_hunks or {}) do
    local key = hunk.display_id or ("%s:display:%d"):format(file_state.key, hunk_index)
    key_by_item[#key_by_item + 1] = key
    local height = M._status_lazy_hunk_estimate(file, hunk)
    body_blocks[#body_blocks + 1] = {
      key = key,
      kind = "hunk",
      height = height,
      metadata = {
        kind = "hunk",
        hunk = hunk,
        raw_hunks = hunk.raw_hunks or {},
        annotation_ids = M._status_diff_hunk_annotation_ids(file_state, file, hunk),
      },
    }
  end

  file_state.body_layout = M._diff_layout_model.new_file_body_layout(body_blocks)
  file_state.layout = file_state.body_layout.view
  for hunk_index, _ in ipairs(display_hunks or {}) do
    local key = key_by_item[hunk_index]
    local span = file_state.layout.span_by_key[key]
    local block = file_state.body_layout.block_by_key[key]
    if span then
      span.metadata = block and block.metadata or span.metadata
    end
  end
  file_state.layout_dirty = false
  file_state.layout_revision = (file_state.layout_revision or 0) + 1
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_record_diff_file_header_state(file, entry_kind, hunk_entry_kind, file_key)
  local file_state = M._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  if not file_state then return nil end
  file_state.hunks = {}
  file_state.hunk_index_by_id = {}
  file_state.annotations = {}
  file_state.annotation_index = M._diff_annotation_model.new_index()
  file_state.layout = nil
  file_state.layout_dirty = true
  M._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  M._status_populate_diff_file_annotations(file_state, file)
  return file_state
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param display_hunks DiffReviewHunk[]
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_record_diff_file_state(file, hunks, display_hunks, entry_kind, hunk_entry_kind, file_key)
  local file_state = M._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  if not file_state then return nil end
  file_state.hunks = {}
  file_state.hunk_index_by_id = {}
  file_state.annotations = {}
  file_state.annotation_index = M._diff_annotation_model.new_index()
  file_state.layout_dirty = true
  M._status_populate_diff_file_hunks(file_state, file, hunks)
  for hunk_index, hunk in ipairs(display_hunks or {}) do
    hunk.source_id = file_state.source_id
    hunk.file_key = file_state.key
    hunk.display_id = hunk.display_id or ("%s:display:%d"):format(file_state.key, hunk_index)
    hunk.diff_review_hunk_index = hunk.diff_review_hunk_index or M._diff_hunk_index_model.from_hunk(hunk)
    file_state.hunk_index_by_id = file_state.hunk_index_by_id or {}
    file_state.hunk_index_by_id[hunk.display_id] = hunk.diff_review_hunk_index
  end
  M._status_populate_diff_file_annotations(file_state, file)
  M._status_build_diff_file_layout(file_state, file, display_hunks)
  return file_state
end




---@param file DiffReviewStatusFile
---@param entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@param hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"
---@param file_key_override? string
---@param hunk_key_builder? fun(hunk: DiffReviewHunk): string
---@param opts? { force_open?: boolean, default_open?: boolean }







---@param commit DiffReviewStatusCommit
---@return string?
local function status_commit_relative_date(commit)
  local value = commit.committed_at or commit.authored_at
  local relative = M._datetime.relative(value, { yesterday = false })
  if relative == "" then return nil end
  return relative
end


---@param commit DiffReviewStatusCommit
---@param date_width integer

---@param section DiffReviewStatusSection

---@param commit DiffReviewStatusCommit
local function status_load_commit_files(commit)
  local status = M._status
  local cwd = status and status.cwd
  local buf = status and status.buf
  if not (cwd and buf and vim.api.nvim_buf_is_valid(buf)) then return end

  local source_state = M._status_ensure_commit_source_state(commit)
  if not source_state then return end
  local source_id = source_state.handle.id
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

  M._diff_source_model.ensure_loaded(source_state, function(ok, err)
    local latest_status = M._status
    if not (latest_status and latest_status.buf and vim.api.nvim_buf_is_valid(latest_status.buf)) then return end
    if latest_status.cwd ~= cwd then return end

    latest_status.commit_file_cache = latest_status.commit_file_cache or {}
    local next_cache = {
      files = nil,
      files_loaded = ok == true,
      files_loading = false,
      files_error = nil,
    }
    local latest_source = latest_status.diff_source_registry
      and latest_status.diff_source_registry.source_by_id
      and latest_status.diff_source_registry.source_by_id[source_id]
      or source_state
    if not ok then
      next_cache.files_error = "Unable to load commit diff" .. (err and err ~= "" and (": " .. tostring(err)) or "")
    else
      next_cache.files = latest_source.metadata and latest_source.metadata.files or {}
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

---@param state? table
---@return DiffReviewStatusEntry?
local function status_entry_under_cursor(state)
  local status = state or M._status
  if not status then return nil end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  return status.entries[line]
end


---@return integer? line
---@return DiffReviewStatusEntry? entry
function M._status_entry_line_under_cursor()
  local status = M._status
  if not (status and status.entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil, nil end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local entries = status.entries
  local max_line = vim.api.nvim_buf_line_count(status.buf)
  for line = math.min(cursor_line, max_line), 1, -1 do
    local entry = entries[line]
    if entry then return line, entry end
  end
  return nil, nil
end

---@param status table?
---@param entry DiffReviewStatusEntry?
---@return table?
function M._status_source_policy_for_entry(status, entry)
  if not (status and status.diff_source_registry and entry) then return nil end
  local file = entry.file
  local source_id = file and file.diff_source_id or nil
  if not source_id and entry.hunk and entry.hunk.section_name then source_id = entry.hunk.section_name end
  if not source_id and entry.kind == "pr_hunk" and status.pr then source_id = "pr:" .. tostring(status.pr.number) .. ":changes" end
  if not source_id and entry.kind == "pr_review_hunk" then source_id = "review:unviewed" end
  if not source_id then return nil end
  return M._diff_source_model.policy(status.diff_source_registry, source_id)
end

---@param status table?
---@param command string
---@return boolean
function M._status_source_policy_allows_cursor(status, command)
  if not (status and status.entries) then return true end
  local _, entry = M._status_entry_line_under_cursor()
  local policy = M._status_source_policy_for_entry(status, entry)
  if not policy then return true end
  return policy[command] == true
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
  local viewport = status.diff_viewport
  local entries = viewport and viewport.enabled and viewport.logical_entries or status.entries
  for line = current_line - 1, 1, -1 do
    local candidate = entries[line]
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
  return M._status_perf_span("status.prewarm_entry_syntax", M._status and M._status.buf or nil, {
    entry_id = entry and entry.id or nil,
    entry_kind = entry and entry.kind or nil,
    file = entry and entry.file and entry.file.filename or nil,
  }, function()
    if not entry then return end
    if M._status_entry_is_file_like(entry) and entry.file then
      local syntax_source = M._status_syntax_source_for_entry_kind(entry.kind)
      syntax_engine.prewarm_file_diff_syntax(entry.file, "status-cursor-prewarm:" .. (entry.id or entry.file.filename), nil, { syntax_source = syntax_source })
    elseif M._status_entry_is_hunk_like(entry) and entry.file and entry.hunk then
      local callback_key = "status-cursor-prewarm:" .. (entry.id or entry.file.filename)
      local syntax_source = M._status_syntax_source_for_entry_kind(entry.kind)
      local syntax_diff_text = nil
      if syntax_source == "file" then
        syntax_diff_text = M._status_perf_span("status.prewarm_entry_syntax.hunk_combined_diff", M._status and M._status.buf or nil, {
          entry_id = entry.id,
          entry_kind = entry.kind,
          file = entry.file.filename,
        }, function()
          return M._status_file_syntax_diff_text(entry.file)
        end)
      end
      M._prewarm_diff_syntax(entry.file.filename, entry.hunk.diff, { entry.hunk.staged }, callback_key, nil, {
        syntax_source = syntax_source,
        syntax_diff_text = syntax_diff_text,
      })
    end
  end)
end

--- Map a 1-based status buffer row to a decoration request, or nil for chrome rows.
---@param buf integer
---@param row integer 1-based buffer line
---@return DiffReviewRowDecorationRequest?
function M._status_resolve_decoration_row(buf, row)
  local status = M._status_states and M._status_states[buf] or nil
  if not (status and status.entries) then return nil end
  local entry = status.entries[row]
  if not entry then return nil end
  local diff_line = (entry.diff_lines and entry.diff_lines[1]) or entry.diff_line
  local file = entry.file
  if not (diff_line and file) then return nil end
  local source_id = M._status_diff_source_id(file, entry.kind)
  return {
    file_key = M._diff_source_model.file_key(source_id, file.relpath or file.filename),
    revision = status.render_revision or 0,
    line = diff_line.line,
    side = diff_line.side == "left" and "old" or "new",
    kind = entry.kind,
  }
end

--- Prewarm Tree-sitter syntax for the file and hunk entries visible in a row range.
--- Scope syntax work to the viewport so off-screen hunks of a large diff stay cheap.
---@param buf integer
---@param first_row integer 1-based
---@param last_row integer 1-based

--- Debounce a visible-window syntax prewarm so the redraw callback never works inline.
---@param buf integer
---@param first_row integer 1-based
---@param last_row integer 1-based
function M._status_schedule_decorate_visible(buf, first_row, last_row)
  local status = M._status_states and M._status_states[buf] or nil
  if not status then return end
  if status.decorate_first == first_row and status.decorate_last == last_row then return end
  status.decorate_first = first_row
  status.decorate_last = last_row
  status.decorate_request_id = (status.decorate_request_id or 0) + 1
  local request_id = status.decorate_request_id
  vim.defer_fn(function()
    local current = M._status_states and M._status_states[buf] or nil
    if not (current and current.decorate_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
    M._status_decorate_visible(buf, first_row, last_row)
  end, 30)
end

--- Place one diff row's cached decoration spans into a namespace.
--- Pass ephemeral=true for the decoration provider's on_line; omit it (persistent)
--- for the test seam so headless tests can read the marks back.
---@param buf integer
---@param namespace integer
---@param row integer 0-based buffer row
---@param spans DiffReviewRowSpans?
---@param ephemeral boolean?

--- Apply a buffer row range's diff decoration into the decorate namespace as
--- persistent marks, for the test seam and non-redraw refreshes.
--- Returns the spans it applied so callers can assert per-row decoration.
---@param buf integer
---@param first_row integer? 1-based inclusive
---@param last_row integer? 1-based inclusive
---@return table<integer, DiffReviewRowSpans>

--- Register the global diff decoration provider once so diff-body decoration
--- (syntax/gutter/intraline/bg) is emitted ephemerally for visible rows only.
--- Skip non-status windows so unrelated redraws stay cheap.

---@param buf integer
local function status_defer_prewarm_under_cursor(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  if not status then return end
  status.cursor_prewarm_request_id = (status.cursor_prewarm_request_id or 0) + 1
  local request_id = status.cursor_prewarm_request_id
  local entry = status_entry_under_cursor()
  local entry_id = entry and entry.id or nil
  M._status_perf_event("status.cursor_prewarm_schedule", buf, {
    request_id = request_id,
    entry_id = entry_id,
    entry_kind = entry and entry.kind or nil,
  })

  vim.defer_fn(function()
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.cursor_prewarm_request_id == request_id) then return end
    if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
    M._status_perf_span("status.cursor_prewarm_run", buf, {
      request_id = request_id,
      scheduled_entry_id = entry_id,
    }, function()
      M._status = latest_status
      local current_entry = status_entry_under_cursor()
      if entry_id and current_entry and current_entry.id == entry_id then
        status_prewarm_entry_syntax(current_entry)
      end
    end)
  end, 35)
end

local status_files_from_set


---@param start_line integer
---@param end_line integer
---@return DiffReviewStatusEntry[]
local function status_entries_for_lines(start_line, end_line)
  local status = M._status
  if not status then return {} end
  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

  local entries_by_line = status.entries

  local entries = {}
  local seen = {}
  for line = start_line, end_line do
    local entry = entries_by_line[line]
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
  local viewport = status and status.diff_viewport or nil
  local entries = viewport and viewport.enabled and viewport.logical_entries or (status and status.entries)
  if not (status and entries and status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return nil end
  if viewport and viewport.enabled and viewport.logical_entry_line_by_id then
    local best_line = nil
    local best_distance = nil
    for entry_id, line in pairs(viewport.logical_entry_line_by_id) do
      local entry = M._status_entry_by_id(entry_id)
      if entry and (
        entry.kind == "section"
        or entry.kind == "file"
        or entry.kind == "commit"
        or entry.kind == "commit_file"
        or entry.kind == "pr_file"
        or entry.kind == "pr_head_section"
      ) then
        local distance = math.abs((tonumber(line) or 0) - fallback_line)
        if not best_distance or distance < best_distance then
          best_distance = distance
          best_line = line
        end
      end
    end
    if best_line then return best_line end
  end
  local max_line = viewport and viewport.enabled and viewport.total or vim.api.nvim_buf_line_count(status.buf)
  local line = math.min(math.max(fallback_line, 1), max_line)
  local max_offset = math.max(line - 1, max_line - line)
  for offset = 0, max_offset do
    local previous_line = line - offset
    local previous_entry = entries[previous_line]
    if previous_entry and (
      previous_entry.kind == "section"
      or previous_entry.kind == "file"
      or previous_entry.kind == "commit"
      or previous_entry.kind == "commit_file"
      or previous_entry.kind == "pr_file"
      or previous_entry.kind == "pr_head_section"
    ) then return previous_line end
    local next_line = line + offset
    local next_entry = entries[next_line]
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

--- Find the buffer line of a status entry by id, preferring the fallback line.
---@param entries table<integer, DiffReviewStatusEntry>?
---@param entry_id string?
---@param fallback_line integer?
---@return integer?
function M._status_find_entry_line(entries, entry_id, fallback_line)
  if not entries then return fallback_line end
  if entry_id and fallback_line then
    local fallback_entry = entries[fallback_line]
    if fallback_entry and fallback_entry.id == entry_id then return fallback_line end
  end
  if entry_id then
    for line, entry in pairs(entries) do
      if entry and entry.id == entry_id then return line end
    end
  end
  return fallback_line
end

local function status_restore_cursor(buf, target_id, fallback_line)
  local target_line = nil
  local entries = M._status and M._status.entries
  if target_id then
    target_line = M._status_find_entry_line(entries, target_id, fallback_line)
  end
  if not target_line and not fallback_line then return end
  if not target_line and fallback_line and status_target_is_header(target_id) then
    target_line = status_nearest_header_line(fallback_line)
  end
  target_line = target_line or fallback_line or 1
  target_line = math.min(target_line, vim.api.nvim_buf_line_count(buf))
  pcall(vim.api.nvim_win_set_cursor, 0, { math.max(target_line, 1), 0 })
end

---@param buf integer
---@param lines string[]
local function status_set_plain_lines(buf, lines)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if M._diff_line_content_lengths then M._diff_line_content_lengths[buf] = nil end
  local state = M._status_states and M._status_states[buf] or (M._status and M._status.buf == buf and M._status) or nil
  if state then state.diff_viewport = nil end
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
  if M._gitstatus_debug_log_file then return M._gitstatus_debug_log_file end
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  M._gitstatus_debug_log_file = dir .. "/gitstatus-debug.log"
  return M._gitstatus_debug_log_file
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

---@param event string
---@param payload? table
function M._gitstatus_debug.event(event, payload)
  if not M._gitstatus_debug.enabled() then return end
  local line = ("GitStatus debug event time=%s event=%s payload=%s"):format(
    os.date("%Y-%m-%d %H:%M:%S"),
    tostring(event),
    M._gitstatus_debug.one_line(payload or {})
  )
  local ok, err = pcall(vim.fn.writefile, { line }, M._gitstatus_debug_log_path(), "a")
  if not ok then
    notify_debug("GitStatus debug event failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
  end
end

function M._gitstatus_debug.perf_enabled()
  local options = M.config or config.options or config.defaults
  local global_enabled = vim.g.diff_review_gitstatus_perf
  return M._gitstatus_debug_perf_force == true
    or M._gitstatus_debug_perf_enabled == true
    or global_enabled == true
    or global_enabled == 1
    or (options and options.status_perf_logging == true)
    or M._gitstatus_debug.enabled()
end

---@return integer
function M._gitstatus_debug.perf_now()
  local uv = vim.uv or vim.loop
  if uv and uv.hrtime then return uv.hrtime() end
  return math.floor(vim.fn.reltimefloat(vim.fn.reltime()) * 1000000000)
end

---@param started integer
---@return number
function M._gitstatus_debug.perf_elapsed_ms(started)
  local elapsed = M._gitstatus_debug.perf_now() - started
  return math.floor((elapsed / 1000000) * 1000 + 0.5) / 1000
end

function M._gitstatus_debug.flush_perf()
  local lines = M._gitstatus_debug_perf_queue
  M._gitstatus_debug_perf_queue = nil
  M._gitstatus_debug_perf_flush_pending = false
  if not (lines and #lines > 0) then return end
  local path = M._gitstatus_debug_log_path()
  local text = table.concat(lines, "\n") .. "\n"
  local uv = vim.uv or vim.loop
  local function report_error(err)
    if not err then return end
    pcall(vim.schedule, function()
      notify_debug("GitStatus perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
    end)
  end
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    uv.fs_open(path, "a", 438, function(open_err, fd)
      if open_err or not fd then
        report_error(open_err or "open failed")
        return
      end
      uv.fs_write(fd, text, -1, function(write_err)
        uv.fs_close(fd, function(close_err)
          report_error(write_err or close_err)
        end)
      end)
    end)
    return
  end
  local ok, err = pcall(vim.fn.writefile, lines, M._gitstatus_debug_log_path(), "a")
  if not ok then
    notify_debug("GitStatus perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
  end
end

---@param event string
---@param payload? table
function M._gitstatus_debug.perf_event(event, payload)
  if not M._gitstatus_debug.perf_enabled() then return end
  M._gitstatus_debug_perf_sequence = (M._gitstatus_debug_perf_sequence or 0) + 1
  local line = ("GitStatus perf seq=%d time=%s event=%s payload=%s"):format(
    M._gitstatus_debug_perf_sequence,
    os.date("%Y-%m-%d %H:%M:%S"),
    tostring(event),
    M._gitstatus_debug.one_line(payload or {})
  )
  M._gitstatus_debug_perf_queue = M._gitstatus_debug_perf_queue or {}
  M._gitstatus_debug_perf_queue[#M._gitstatus_debug_perf_queue + 1] = line
  if M._gitstatus_debug_perf_flush_pending then return end
  M._gitstatus_debug_perf_flush_pending = true
  vim.defer_fn(function()
    M._gitstatus_debug.flush_perf()
  end, 25)
end

---@param event string
---@param payload table?
---@param callback fun(): any
---@return any
function M._gitstatus_debug.perf_span(event, payload, callback)
  if not M._gitstatus_debug.perf_enabled() then return callback() end
  local started = M._gitstatus_debug.perf_now()
  local function pack_results(...)
    return { n = select("#", ...), ... }
  end
  local results = pack_results(pcall(callback))
  local ok = results[1]
  local next_payload = vim.deepcopy(payload or {})
  next_payload.ms = M._gitstatus_debug.perf_elapsed_ms(started)
  if not ok then
    next_payload.error = tostring(results[2])
    M._gitstatus_debug.perf_event(event .. ".error", next_payload)
    error(results[2], 0)
  end
  M._gitstatus_debug.perf_event(event, next_payload)
  local unpack_values = table.unpack or unpack
  return unpack_values(results, 2, results.n)
end

---@param buf integer?
---@param extra? table
---@return table
function M._status_perf_payload(buf, extra)
  local state = buf and M._status_states and M._status_states[buf] or nil
  if not state and M._status and (not buf or M._status.buf == buf) then state = M._status end
  local payload = vim.deepcopy(extra or {})
  payload.buf = buf
  payload.view_kind = state and state.view_kind or nil
  if buf and vim.api.nvim_buf_is_valid(buf) then
    payload.line_count = vim.api.nvim_buf_line_count(buf)
  end
  if buf and vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    payload.cursor_row = cursor[1]
    payload.cursor_col = cursor[2]
  end
  local viewport = state and state.diff_viewport or nil
  if viewport and viewport.enabled then
    payload.viewport_top = viewport.top
    payload.viewport_total = viewport.total
    payload.viewport_logical_total = viewport.logical_total
    payload.viewport_render_count = viewport.render_count
  end
  return payload
end

---@param event string
---@param buf integer?
---@param extra table?
function M._status_perf_event(event, buf, extra)
  local debug = M._gitstatus_debug
  local payload = M._status_perf_payload(buf, extra)
  if M._diff_perf and M._diff_perf.enabled() then M._diff_perf.event(event, payload) end
  if not (debug and debug.perf_event) then return end
  debug.perf_event(event, payload)
end

---@param event string
---@param buf integer?
---@param extra table?
---@param callback fun(): any
---@return any
function M._status_perf_span(event, buf, extra, callback)
  local debug = M._gitstatus_debug
  local payload = M._status_perf_payload(buf, extra)
  if M._diff_perf and M._diff_perf.enabled() then
    return M._diff_perf.span(event, payload, function()
      if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
      return debug.perf_span(event, payload, callback)
    end)
  end
  if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
  return debug.perf_span(event, payload, callback)
end

---@param row table
---@return string
function M._gitstatus_debug.row_text(row)
  if type(row) ~= "table" then return M._gitstatus_debug.text(row) end
  local parts = {}
  for _, chunk in ipairs(row) do
    if type(chunk) == "string" then
      parts[#parts + 1] = chunk
    elseif type(chunk) == "table" then
      if type(chunk[1]) == "string" then parts[#parts + 1] = chunk[1] end
      if type(chunk.virt_text) == "table" then
        local virtual_parts = {}
        for _, virtual_chunk in ipairs(chunk.virt_text) do
          if type(virtual_chunk) == "table" and type(virtual_chunk[1]) == "string" then
            virtual_parts[#virtual_parts + 1] = virtual_chunk[1]
          end
        end
        if #virtual_parts > 0 then parts[#parts + 1] = "<virt:" .. table.concat(virtual_parts, "") .. ">" end
      end
    end
  end
  return table.concat(parts, "")
end

---@param rows table?
---@param limit integer
---@return table[]
function M._gitstatus_debug.row_preview(rows, limit)
  local preview = {}
  if type(rows) ~= "table" then return preview end
  for row_index = 1, math.min(#rows, limit) do
    local row = rows[row_index]
    preview[#preview + 1] = {
      index = row_index,
      text = M._gitstatus_debug.row_text(row),
      hunk_header = type(row) == "table" and row.diff_review_hunk_header == true or nil,
      boundary = type(row) == "table" and row.diff_review_boundary == true or nil,
    }
  end
  return preview
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
  if not (state and (state.view_kind == "status" or state.view_kind == "pr" or state.view_kind == "review") and vim.api.nvim_buf_is_valid(buf)) then return end
  state.gitstatus_debug_dump_reason = reason
  if state.gitstatus_debug_dump_pending then return end
  state.gitstatus_debug_dump_pending = true

  vim.defer_fn(function()
    state = M._status_states and M._status_states[buf] or (M._status and M._status.buf == buf and M._status) or nil
    if state then state.gitstatus_debug_dump_pending = false end
    if not M._gitstatus_debug.enabled() then return end
    if not (state and (state.view_kind == "status" or state.view_kind == "pr" or state.view_kind == "review") and vim.api.nvim_buf_is_valid(buf)) then return end
    reason = state.gitstatus_debug_dump_reason or reason
    local win = vim.fn.win_findbuf(buf)[1]
    local lines = {
      "",
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

    local ok, err = pcall(vim.fn.writefile, lines, M._gitstatus_debug_log_path(), "a")
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

---@param buf integer

--- Run the PR overview view's after-render work (its view controller's after_render hook).
---@param buf integer
function M._status_after_render_pr(buf)
  M._status_perf_span("status.after_render.pr_edit_on_render", buf, nil, function() M._pr_edit.on_render(buf) end)
  M._status_perf_span("status.after_render.review_on_render", buf, nil, function() M._review.on_render(buf) end)
  M._status_perf_span("status.after_render.markdown", buf, nil, function() M._pr_edit.render_markdown_regions(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.pr_sync_modifiable", buf, nil, function() M._pr_edit.sync_modifiable(buf) end)
  M._status_perf_span("status.after_render.schedule_native_folds", buf, nil, function() M._status_schedule_native_folds(buf) end)
end

--- Run the PR review view's after-render work.
---@param buf integer
function M._status_after_render_review(buf)
  M._status_perf_span("status.after_render.review_on_render", buf, nil, function() M._review.on_render(buf) end)
  M._status_perf_span("status.after_render.markdown", buf, nil, function() M._pr_edit.render_markdown_regions(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.review_sync_modifiable", buf, nil, function() M._review.sync_modifiable(buf) end)
  M._status_perf_span("status.after_render.schedule_native_folds", buf, nil, function() M._status_schedule_native_folds(buf) end)
end

--- Run the GitStatus view's after-render work.
---@param buf integer
function M._status_after_render_status(buf)
  M._status_perf_span("status.after_render.stop_markdown", buf, nil, function() M._status_stop_markdown_highlighter(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.issues_sync_modifiable", buf, nil, function() M._status_issues.sync_modifiable(buf) end)
end

--- Run the after-render work for branch diff and any other status-like view.
---@param buf integer
function M._status_after_render_default(buf)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
end

---@param buf integer
---@param walkthrough table?


---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
---@param head_lines table[]
---@param sections DiffReviewStatusSection[]

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
    status_render.status_render_loaded(buf, target_id, fallback_line, opts, M._status.head_lines, M._status.sections)
    return
  end

  M._status.request_id = (M._status.request_id or 0) + 1
  local request_id = M._status.request_id
  local has_existing_view = M._status.head_lines ~= nil or M._status.sections ~= nil
  if not has_existing_view then
    status_set_plain_lines(buf, { "Loading DiffReview..." })
  end

  git_backend.git_root_async(function(cwd, root_err)
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
      status_render.status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
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
  M._gitstatus_debug.event("render_pr_status.start", {
    buf = buf,
    cwd = cwd,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
    diff_len = #(diff_text or ""),
    comment_count = status.pr_comments and #status.pr_comments or nil,
    standalone_comment_count = status.pr_standalone_comments and #status.pr_standalone_comments or nil,
    regular_comment_count = status.pr_regular_comments and #status.pr_regular_comments or nil,
  })
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  M._pr_overview.refresh_editable_comments(status)
  status.head_lines = status_pr_detail_head_lines(pr, status)
  status.sections = status_pr_sections(cwd, pr, diff_text, status.pr_comments, status.pr_standalone_comments, status.pr_regular_comments)
  local section_file_count = 0
  for _, section in ipairs(status.sections or {}) do
    section_file_count = section_file_count + #(section.files or {})
  end
  M._gitstatus_debug.event("render_pr_status.sections", {
    buf = buf,
    pr_number = pr and pr.number or nil,
    section_count = status.sections and #status.sections or nil,
    file_count = section_file_count,
  })
  status.fancy_rows = {}
  status.pr_code_comments_by_anchor = M._section_builder.comment_anchor_index_from_sections(status.sections, { field = "pr_comments" })
  status.review_after_row = function(diff_line, indent)
    M._section_builder.emit_anchored_comments(status, diff_line, indent, { index_field = "pr_code_comments_by_anchor" })
  end
  status_render.status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
local function load_pr_diff(pr, cwd, buf)
  local status = M._status
  if not (status and status.buf == buf) then return end
  status.pr_diff_request_id = (status.pr_diff_request_id or 0) + 1
  local request_id = status.pr_diff_request_id
  M._gitstatus_debug.event("load_pr_diff.start", {
    buf = buf,
    cwd = cwd,
    request_id = request_id,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
  })
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    M._gitstatus_debug.event("load_pr_diff.done", {
      buf = buf,
      cwd = cwd,
      request_id = request_id,
      pr_number = pr and pr.number or nil,
      repo = pr and pr.repo or nil,
      code = result and result.code or nil,
      stdout_len = result and result.stdout and #result.stdout or nil,
      stdout_preview = result and result.stdout and result.stdout:sub(1, 800) or nil,
      stderr = result and result.stderr ~= "" and result.stderr or nil,
      output = result and result.code ~= 0 and result.output or nil,
    })
    if result.code ~= 0 then
      notify_error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest_status.pr_diff_text = result.stdout or ""
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
M._review = require("diff_review.review")
M._review.comment_icon = M._comment_icon
M._review.reply_icon = M._reply_icon





















































































































































-- Per-repository config, read from `<repo root>/.diffreview.json`. Currently
-- only `branch_prefix` (used by the `bc` branch-create action). On the module
-- table because init.lua is at Lua's 200-local limit.
M._repo_config = require("diff_review.repo_config")


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
    git_backend.run_git_at_root_async(cwd, { "switch", "-c", name }, nil, function(result)
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
-- Seam: expose the render orchestrator for the extracted branch_diff module.
M._status_render_loaded = status_render.status_render_loaded
M._branch_diff = require("diff_review.branch_diff")

---@param target_id? string

---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@param target_id? string
local function status_apply_optimistic_entries(entries, target_section, target_id)
  local status = M._status
  if not (status and status.sections) then return end
  local next_sections = status_apply_optimistic_move(status.sections, entries, target_section)
  if not next_sections then return end
  status.sections = next_sections
  status_render.status_render_current_model(target_id)
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
  if not status then return false end
  if status.operation_queue_model then
    return M._diff_mutation_queue_model.pending(status.operation_queue_model)
  end
  return status.operation_running or #(status.operation_queue or {}) > 0
end

---@param status table
---@param done fun()
function M._status_reload_invalidated_diff_sources(status, done)
  local registry = status and status.diff_source_registry or nil
  if not registry then
    done()
    return
  end
  local source_ids = {}
  local path_set = {}
  for source_id, invalidated in pairs(registry.invalidation_by_source or {}) do
    source_ids[#source_ids + 1] = source_id
    for path in pairs(invalidated or {}) do
      path_set[path] = true
    end
  end
  local paths = status_files_from_set(path_set)
  if #source_ids == 0 or #paths == 0 then
    done()
    return
  end
  M._status_perf_event("source.reload_invalidated.start", status.buf, {
    source_ids = source_ids,
    paths = paths,
  })
  M._diff_source_model.reload_paths(registry, source_ids, paths, function(ok, err)
    if not ok then
      notify_error("Diff source reload failed: " .. tostring(err or "unknown error"), "DiffReview")
    end
    M._status_perf_event("source.reload_invalidated.done", status.buf, {
      ok = ok,
      source_ids = source_ids,
      paths = paths,
    })
    done()
  end)
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
    M._status_reload_invalidated_diff_sources(latest_status, function()
      if reconcile_buf then refresh_status_after_action(reconcile_buf, reconcile_target_id) end
    end)
  end, status_reconcile_delay_ms)
end

---@param operation fun(done: fun())
local function status_enqueue_operation(operation)
  M._status = M._status or {}
  local status = M._status
  status.reconcile_generation = (status.reconcile_generation or 0) + 1
  status.operation_queue_model = status.operation_queue_model or M._diff_mutation_queue_model.new()
  M._diff_mutation_queue_model.enqueue(status.operation_queue_model, operation)
  M._diff_mutation_queue_model.on_idle(status.operation_queue_model, function()
    status_request_reconcile(status.reconcile_buf, status.reconcile_target_id)
  end)
end

-- PR title/description editing in the PR view: insert mode is gated to the
-- title line and description block, ":w" syncs via `gh pr edit`, and a "*"
-- marks unsynced fields. Functions hang off the module table because
-- init.lua is at Lua's 200-local limit.
-- Render/confirm seams used by the extracted pr_edit module (all in scope here).
M._confirm = confirm
M._status_markdown_lines = status_markdown_lines
M._render_pr_status = render_pr_status
-- Seams shared with the extracted pr_overview module.
M._parse_hunk_header = parse_hunk_header
M._status_short_oid = status_short_oid
M._status_provider_file_key = status_provider_file_key
M._status_provider_hunk_key = status_provider_hunk_key
M._status_entry_under_cursor = status_entry_under_cursor
M._status_render_file = status_render.status_render_file
-- Seams shared with the extracted review module.
M._status_command_visible = keymaps.status_command_visible
M._status_keys_for = keymaps.status_keys_for
M._notify_debug = notify_debug
M._status_command_specs_by_id = status_command_specs_by_id
M._status_diff_hunks_for_file = status_diff_hunks_for_file
M._status_cursor_target = status_cursor_target
M._status_operations_pending = status_operations_pending
-- Seams for the extracted syntax_engine module.
M._file_source_lines = syntax_engine.file_source_lines
M._same_hunk_ancestor_scope = syntax_engine.same_hunk_ancestor_scope
M._status_hunk_context_line = syntax_engine.status_hunk_context_line
M._old_file_syntax_source_lines = syntax_engine.old_file_syntax_source_lines
M._diff_new_side_matches_file = syntax_engine.diff_new_side_matches_file
M._cached_old_file_syntax = syntax_engine.cached_old_file_syntax
M._diff_uses_file_syntax = syntax_engine.diff_uses_file_syntax
M._prewarm_diff_syntax = syntax_engine.prewarm_diff_syntax
M._status_file_syntax_diff_text = syntax_engine.status_file_syntax_diff_text
M._status_prewarm_hunk_budget = syntax_engine.status_prewarm_hunk_budget
M._status_syntax_source_for_entry_kind = syntax_engine.status_syntax_source_for_entry_kind
M._parse_unified_diff = parse_unified_diff
M._loaded_file_buffer = loaded_file_buffer
M._file_contains_nul = file_contains_nul
M._lines_contain_nul = lines_contain_nul
M._status_render_current_model = status_render.status_render_current_model
-- Seams for the extracted diff_render module.
M._merge_hunk_gutter_specs = diff_render.merge_hunk_gutter_specs
M._add_plan_to_hunk_display_group = diff_render.add_plan_to_hunk_display_group
M._merge_hunk_render_plans = diff_render.merge_hunk_render_plans
-- Seams for the extracted status_render module.
M._status_decorate_visible = status_render.status_decorate_visible
M._status_emit_row_spans = status_render.status_emit_row_spans
M._status_decorate_rows = status_render.status_decorate_rows
M._status_register_decoration_provider = status_render.status_register_decoration_provider
M._status_write_rendered_buffer = status_render.status_write_rendered_buffer
M._status_apply_rendered_extmarks = status_render.status_apply_rendered_extmarks
M._status_after_buffer_render = status_render.status_after_buffer_render
M._status_commit_relative_date = status_commit_relative_date
M._status_hunk_key = status_hunk_key
M._status_prewarm_entry_syntax = status_prewarm_entry_syntax
M._setup_bg_highlights = setup_bg_highlights
M._status_restore_cursor = status_restore_cursor
M._status_commit_key = status_commit_key
M._status_commit_hunk_key = status_commit_hunk_key
M._status_commit_file_key = status_commit_file_key
M._status_file_key = status_file_key
M._status_section_key = status_section_key
-- Seams shared with the extracted hunk_model module.
M._parse_hunk_body = parse_hunk_body
M._hunk_add_gutter = diff_render.hunk_add_gutter
M._treesitter_line_segments = syntax_engine.treesitter_line_segments
M._hunk_context_scope_key = syntax_engine.hunk_context_scope_key
M._same_hunk_context_scope = syntax_engine.same_hunk_context_scope
M._hunk_first_changed_current_line = hunk_first_changed_current_line
M._hunk_line_visible_in_context_scope = syntax_engine.hunk_line_visible_in_context_scope
M._pr_edit = require("diff_review.pr_edit")

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

---@param entry DiffReviewStatusEntry
---@return string?
function M._status_entry_source_path(entry)
  local status = M._status
  if not (entry and entry.file and status) then return nil end
  return M._status_diff_file_path(entry.file, status)
end

---@param entries DiffReviewStatusEntry[]
---@param source_ids string[]
function M._status_mark_diff_paths_pending(entries, source_ids)
  local status = M._status
  if not (status and status.diff_source_registry) then return end
  local path_set = {}
  for _, entry in ipairs(entries or {}) do
    local path = M._status_entry_source_path(entry)
    if path and path ~= "" then path_set[path] = true end
  end
  local paths = status_files_from_set(path_set)
  if #paths == 0 then return end
  M._status_perf_event("source.invalidate_paths", status.buf, {
    source_ids = source_ids,
    paths = paths,
  })
  M._diff_source_loader_model.invalidate(status.diff_source_registry, source_ids, paths)
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
  if has_added then return "unstaged" end
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

  M._status_mark_diff_paths_pending(action_entries, { "unstaged", "staged" })
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

  M._status_mark_diff_paths_pending(action_entries, { "staged", "unstaged" })
  status_apply_optimistic_entries(tracked_entries, "unstaged", target_id)
  status_apply_optimistic_entries(added_entries, "unstaged", target_id)

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
  git_backend.git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    local failures = {}
    if #entries == 0 then return end
    M._status_mark_diff_paths_pending(entries, { "unstaged", "staged" })

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
        git_backend.run_git_at_root_async(cwd, args, entry.hunk.diff .. "\n", function(result)
          if not result.ok then
            failures[#failures + 1] = { file = entry.file.filename, output = result.output, code = result.code }
          end
          next_entry()
        end)
      elseif entry.file.untracked then
        local delete_code = git_backend.delete_path(entry.file.filename)
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
            local delete_code = git_backend.delete_path(path)
            if delete_code == 0 then return true end
            failures[#failures + 1] = {
              file = path,
              message = ("delete() failed with code %d%s"):format(delete_code, context),
            }
            return false
          end

          if entry.file.section_name == "unstaged" then
            if git_status_is_added(entry.file.git_status) then
              git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
                if restore_result.ok then
                  delete_file(entry.file.filename, " after unstaging")
                else
                  add_failure(restore_result)
                end
                next_entry()
              end)
            else
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end
          elseif git_status_is_added(entry.file.git_status) then
            git_backend.run_git_at_root_async(cwd, { "rm", "--cached", "--ignore-unmatch", "--", relpath }, nil, function(rm_result)
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
              git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath, original_relpath }, nil, function(restore_result)
                if not restore_result.ok then
                  add_failure(restore_result)
                  next_entry()
                  return
                end
                git_backend.run_git_at_root_async(cwd, { "checkout", "--", original_relpath }, nil, function(checkout_result)
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
            git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end)
          else
            git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
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

M._file_revision = require("diff_review.file_revision")

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
  git_backend.git_root_async(function(root, root_err)
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

function M._status_commit_message_target(entry, cursor_col)
  if not (entry and (entry.kind == "commit" or entry.kind == "commit_message") and entry.commit) then return nil end
  local start_col = entry.commit_subject_start_col
  local end_col = entry.commit_subject_end_col
  if type(start_col) == "number" and type(end_col) == "number" then
    if start_col >= end_col then return nil end
    if type(cursor_col) ~= "number" or cursor_col < start_col or cursor_col >= end_col then return nil end
  end
  return entry.commit
end

function M._status_commit_message_fallback_lines(commit)
  local message = tostring(commit and (commit.full_message or commit.messageBody or commit.message_body or commit.body or commit.message) or "")
  if message == "" then message = tostring(commit and (commit.subject or commit.messageHeadline) or "") end
  if message == "" then return { "No commit message." } end
  return vim.split(message:gsub("\r\n", "\n"), "\n", { plain = true })
end

function M._status_open_commit_message(commit, cwd)
  if type(commit) ~= "table" then return false end
  local oid = vim.trim(tostring(commit.oid or commit.sha or commit.id or ""))
  local short_oid = vim.trim(tostring(commit.short_oid or commit.shortOid or commit.abbreviatedOid or ""))
  if short_oid == "" then short_oid = status_short_oid(oid) end
  local title_id = short_oid ~= "" and short_oid or vim.fn.sha256(tostring(commit.subject or commit.messageHeadline or "")):sub(1, 8)

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "gitcommit"
  local name = ("GitCommit://%s"):format(title_id)
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "Loading commit message..." })
  vim.bo[buf].modifiable = false
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end, { buffer = buf, nowait = true, silent = true, desc = "Close commit message" })
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)

  local function set_lines(lines)
    if not vim.api.nvim_buf_is_valid(buf) then return end
    if type(lines) ~= "table" or #lines == 0 then lines = M._status_commit_message_fallback_lines(commit) end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
  end

  if oid == "" or not (cwd and cwd ~= "") then
    set_lines(M._status_commit_message_fallback_lines(commit))
    return true
  end

  git_backend.systemlist_async({ "git", "-C", cwd, "show", "-s", "--format=%B", oid }, function(output, code, stderr)
    if code ~= 0 then
      local message = vim.trim(stderr or "")
      notify_error("Commit message failed: " .. (message ~= "" and message or ("git exited " .. tostring(code))), "DiffReview")
      set_lines(M._status_commit_message_fallback_lines(commit))
      return
    end
    set_lines(output or {})
  end)
  return true
end

function M._status_open_commit_message_under_cursor(entry)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local commit = M._status_commit_message_target(entry, cursor and cursor[2])
  if not commit then return false end
  return M._status_open_commit_message(commit, M._status and M._status.cwd)
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
  local viewport = M._status.diff_viewport
  if viewport and viewport.enabled and viewport.logical_entries then
    for _, entry in pairs(viewport.logical_entries) do
      if entry and entry.id == entry_id then return entry end
    end
  end
  for _, entry in pairs(M._status.entries) do
    if entry and entry.id == entry_id then return entry end
  end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_default_folded(entry)
  if not entry then return false end
  if entry.default_folded ~= nil then return entry.default_folded == true end
  local ranges = M._status_fold_ranges_for_id(M._status, entry.id)
  local range = ranges[1]
  if range and range.default_folded ~= nil then return range.default_folded == true end
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
---@param state? table
local function status_toggle(entry, state)
  state = state or M._status
  if not state then return end
  if not entry then return end
  local fold_id = entry.fold_target_id or entry.id
  if not fold_id then return end
  if state and state.buf and M._pr_edit.blocks_render(state.buf) then return end
  local fold_entry = entry
  if entry.fold_target_id then
    fold_entry = M._status_entry_by_id(fold_id) or { id = fold_id, kind = "pr_head_section" }
  end
  local default = M._status_entry_default_folded(fold_entry)
  local current_folded = status_folded(fold_id, default, state)
  local native_state = M._status_native_folded(state.buf, M._status_fold_ranges_for_id(state, fold_id), vim.api.nvim_get_current_win())
  if native_state ~= nil then current_folded = native_state end
  local next_folded = not current_folded
  if not next_folded then
    status_prewarm_entry_syntax(fold_entry)
  end
  set_status_folded(fold_id, next_folded, state)
  if fold_entry.kind == "commit" and not next_folded and fold_entry.commit then
    status_load_commit_files(fold_entry.commit)
  end
  local native_folded = M._status_set_native_fold_state(state.buf, fold_id, next_folded)
  if not native_folded then
    render_status_or_notify(state.buf, fold_id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
  elseif state.view_kind == "status" then
    local walkthrough = package.loaded["diff_review.walkthrough"]
    if walkthrough and walkthrough.on_status_rendered then walkthrough.on_status_rendered(state.buf) end
  end
  if state.view_kind == "pr" then
    M._pr_edit.sync_modifiable(state.buf)
  elseif state.view_kind == "review" then
    M._review.sync_modifiable(state.buf)
  end
end

function M._status_collapse_parent()
  local line, entry = M._status_entry_line_under_cursor()
  if not (line and entry) then return end
  local parent = M._status_parent_entry(line, entry)
  local target = parent or entry
  set_status_folded(target.id, true)
  if not M._status_set_native_fold_state(M._status.buf, target.id, true) then
    render_status_or_notify(M._status.buf, target.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
  elseif M._status.view_kind == "status" then
    local walkthrough = package.loaded["diff_review.walkthrough"]
    if walkthrough and walkthrough.on_status_rendered then walkthrough.on_status_rendered(M._status.buf) end
  end
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
    if keymaps.status_command_visible(spec) then
      key_width = math.max(key_width, #status_key_text(keymaps.status_keys_for(spec.id)))
    end
  end
  local line_index = 0
  for _, spec in ipairs(status_command_specs) do
    if not keymaps.status_command_visible(spec) then goto continue end
    local key_text = status_key_text(keymaps.status_keys_for(spec.id))
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
    if keymaps.status_command_visible(spec) then
      key_width = math.max(key_width, #status_key_text(keymaps.status_keys_for(spec.id)))
    end
  end

  local lines = {}
  for _, spec in ipairs(status_command_specs) do
    if not keymaps.status_command_visible(spec) then goto continue end
    local key_text = status_key_text(keymaps.status_keys_for(spec.id))
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
  git_backend.git_root_async(function(cwd, root_err)
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
    git_backend.system_text_stream_async({ "git", "-C", cwd, action, "--progress" }, nil, update_remote_status, function(result)
      local compact = {}
      for _, line in ipairs(git_backend.text_to_lines((result.stdout or "") .. (result.stderr or ""))) do
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
    set_folded = function(key, folded)
      local state = M._status_states and M._status_states[buf] or M._status
      if state then M._status = state end
      set_status_folded(key, folded, state)
    end,
    rerender = function(target_id, fallback_line)
      render_status_or_notify(buf, target_id, fallback_line, { reuse_sections = true })
    end,
    git_list_async = git_backend.systemlist_async,
    inventory_async = function(cb)
      local state = M._status_states and M._status_states[buf] or M._status
      local cwd = state and state.cwd
      if not cwd or cwd == "" then
        cb({ rows = {} })
        return
      end
      M._inventory.compute_async({
        cwd = cwd,
        sections = state.sections or {},
        git_list_async = git_backend.systemlist_async,
        read_file_lines = M._file_source_lines,
        repo_relative = function(filename)
          return repo_relative(filename, cwd)
        end,
      }, function(result)
        if result and result.error and result.error ~= "" then
          notify_error("Walkthrough inventory failed: " .. result.error, "DiffReview")
        end
        cb(result or { rows = {} })
      end)
    end,
  }
end

---@param buf integer

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
  keymaps.setup_status_keymaps(buf)
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
  keymaps.setup_status_keymaps(buf)
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
    keymaps.setup_status_keymaps(buf)

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
  git_backend.git_root_async(open_for_root)
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
    git_backend.systemlist_async(command, function(output, code, stderr)
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
    git_backend.git_root_async(open_for_root)
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
    keymaps.setup_status_keymaps(buf)
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
  M._apply_status_window_options(win, M._main_status)
  vim.wo[win].foldcolumn = "0"
  render_status_or_notify(buf)
end

-- Seams for the extracted keymaps module.
M._status_open_pr = status_open_pr
M._status_show_help = status_show_help
M._render_status_or_notify = render_status_or_notify
M._status_leave_visual_mode = status_leave_visual_mode
M._status_entries_from_visual_selection = status_entries_from_visual_selection
M._status_jump = status_jump
M._status_remote_action = status_remote_action
M._status_defer_prewarm_under_cursor = status_defer_prewarm_under_cursor
M._status_toggle = status_toggle
M._status_stage_entries = status_stage_entries
M._status_stage = status_stage
M._status_primary_key = status_primary_key
M._status_open_about = status_open_about
M._status_discard_entry_list = status_discard_entry_list
M._status_discard = status_discard
M._load_pr_diff = load_pr_diff
M._status_unstage = status_unstage
M._status_unstage_entries = status_unstage_entries
M._status_command_visible_for_view = keymaps.status_command_visible_for_view
M._status_hint_segments = keymaps.status_hint_segments
M._status_hint_title = keymaps.status_hint_title
M._status_hint_winbar = keymaps.status_hint_winbar
M._status_apply_hint_bar = keymaps.status_apply_hint_bar
M._status_clear_hint_bar = keymaps.status_clear_hint_bar

return M
