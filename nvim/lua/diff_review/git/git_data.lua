--- Owns the git/diff data layer for the status views: unified-diff parsing, hunk
--- extraction/ordering, the git-status item collector, and the
--- async tree-sitter syntax computation entry points.
---
--- Reads the notify/filetype/nul helpers and live status state from session.lua and sibling modules,
--- and the git backend, syntax engine, and path helpers as direct requires.

local git_backend = require("diff_review.git.git_backend")
local syntax_engine = require("diff_review.render.syntax_engine")
local paths = require("diff_review.infra.paths")

--- Resolve the init module lazily so git data ops can reach the shared notify/filetype seams and
--- orchestrator state without a load-time circular require.
local util = require("diff_review.infra.util")
local diff_buffer = require("diff_review.views.diff_buffer")
local notifications = require("diff_review.infra.notifications")
local trace = require("diff_review.infra.perf_trace")
local session = require("diff_review.session")

local M = {}
require("diff_review.query_runtime")

--- Resolve the path snapshot seam lazily to avoid its parser dependency closing a load-time cycle.
---@return DiffReviewPathStatusSnapshotModule
local function status_snapshot()
  return (require("diff_review.git.status_snapshot"))
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
  if type(status) ~= "string" then return false end
  local status_kind = status:sub(1, 1)
  return status_kind == "A" or status_kind == "C"
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
      local a, r = util.count_stats(table.concat(current_hunk_lines, "\n"))
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
  local args = git_backend.git_diff_command(cwd, staged and { "--cached" } or nil)
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
      if vim.fs.normalize(paths.repo_file_path(cwd, hunk.file)) == norm then
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

  local ft = syntax_engine.syntax_buffer_filetype(buf, filename)
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
  return trace.span("treesitter.compute_file_syntax_async", session.status and session.status.buf or nil, {
    file = filename,
  }, function()
    local buf = syntax_engine.treesitter_source_buffer(filename)
    if not buf then
      cb(nil)
      return
    end

    local ft = syntax_engine.syntax_buffer_filetype(buf, filename)
    local lang = vim.treesitter.language.get_lang(ft)
    if not lang then
      cb(nil)
      return
    end

    local highlight_ok, highlight_query = pcall(vim.treesitter.query.get, lang, "highlights")
    if not highlight_ok then highlight_query = nil end

    local parser_ok, parser = trace.span("treesitter.compute_file_syntax_async.get_parser", session.status and session.status.buf or nil, {
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

    local parse_ok, parsed = trace.span("treesitter.compute_file_syntax_async.parse_call", session.status and session.status.buf or nil, {
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
  return trace.span("treesitter.compute_diff_syntax_async", session.status and session.status.buf or nil, {
    file = filename,
    source_line_count = #lines,
  }, function()
    if #lines == 0 then
      cb(nil)
      return
    end

    local ft = util.detect_filetype(filename, lines)
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
    syntax_engine.mark_syntax_scratch_buffer(buf, ft)
    trace.span("treesitter.compute_diff_syntax_async.set_lines", session.status and session.status.buf or nil, {
      file = filename,
      lang = lang,
      source_line_count = #lines,
    }, function()
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    end)

    local parser_ok, parser = trace.span("treesitter.compute_diff_syntax_async.get_parser", session.status and session.status.buf or nil, {
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

    local parse_ok, parsed = trace.span("treesitter.compute_diff_syntax_async.parse_call", session.status and session.status.buf or nil, {
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

---@param file_snapshot DiffReviewPathStatusFileSnapshot
---@param staged boolean
---@return DiffReviewHunk
local function status_placeholder_hunk(file_snapshot, staged)
  local record = file_snapshot.status_record
  local git_status = staged and record.index_status or record.worktree_status
  return {
    file = file_snapshot.path,
    filename = file_snapshot.abs_file,
    pos = 1,
    context = nil,
    diff = nil,
    staged = staged,
    added = 0,
    removed = 0,
    status = git_status,
    git_status = git_status,
    git_original_file = record.original_path,
    git_path_change_kind = (record.kind == "renamed" or record.kind == "copied") and record.kind or nil,
  }
end

---@param snapshot DiffReviewPathStatusSnapshot
---@return DiffReviewHunk[] all_hunk_list
---@return DiffReviewPathStatusFileSnapshot[] untracked_file_list
local function snapshot_status_hunk_list(snapshot)
  local all_hunk_list = {}
  local untracked_file_list = {}
  for _, file_snapshot in ipairs(snapshot.file_list) do
    local record = file_snapshot.status_record
    if record.untracked then
      untracked_file_list[#untracked_file_list + 1] = file_snapshot
    else
      local has_hunk = #file_snapshot.unstaged_hunk_list > 0 or #file_snapshot.staged_hunk_list > 0
      vim.list_extend(all_hunk_list, file_snapshot.unstaged_hunk_list)
      vim.list_extend(all_hunk_list, file_snapshot.staged_hunk_list)
      if not has_hunk and record.staged then
        all_hunk_list[#all_hunk_list + 1] = status_placeholder_hunk(file_snapshot, true)
      end
      if not has_hunk and record.unstaged then
        all_hunk_list[#all_hunk_list + 1] = status_placeholder_hunk(file_snapshot, false)
      end
    end
  end
  return all_hunk_list, untracked_file_list
end

---@param all_hunk_list DiffReviewHunk[]
---@return table<string, { added: number, removed: number, total: number, staged: number }>
local function snapshot_file_stat_by_path(all_hunk_list)
  local file_stat_by_path = {}
  for _, hunk in ipairs(all_hunk_list) do
    local path = hunk.file
    if not file_stat_by_path[path] then
      file_stat_by_path[path] = { added = 0, removed = 0, total = 0, staged = 0 }
    end
    local file_stat = file_stat_by_path[path]
    file_stat.added = file_stat.added + hunk.added
    file_stat.removed = file_stat.removed + hunk.removed
    file_stat.total = file_stat.total + 1
    if hunk.staged then file_stat.staged = file_stat.staged + 1 end
  end
  return file_stat_by_path
end

---@param cwd string
---@param all_hunk_list DiffReviewHunk[]
---@param file_stat_by_path table<string, { added: number, removed: number, total: number, staged: number }>
---@param context? { skip_ts_context?: boolean }
---@return table[]
local function snapshot_tracked_item_list(cwd, all_hunk_list, file_stat_by_path, context)
  local item_list = {}
  for _, hunk in ipairs(all_hunk_list) do
    local filename = hunk.filename or paths.repo_file_path(cwd, hunk.file)
    local context_text = hunk.context or ""
    if hunk.diff and not (context and context.skip_ts_context) then
      local cached = syntax_engine.cached_hunk_context(filename, hunk.pos, "items:" .. filename .. ":" .. hunk.pos)
      local cached_label = syntax_engine.hunk_context_label(cached)
      if cached_label then context_text = cached_label end
    end

    local full_header = rawget(hunk, "status") or "@@"
    if hunk.diff then
      full_header = hunk.diff:match("\n(@@[^@]+@@)") or hunk.diff:match("^(@@[^@]+@@)") or "@@"
    end
    local old_range = full_header:match("%-(%d+,?%d*)") or ""
    local new_range = full_header:match("%+(%d+,?%d*)") or ""
    local file_stat = file_stat_by_path[hunk.file]
    local file_check = "[ ]"
    if file_stat.staged == file_stat.total then
      file_check = "[x]"
    elseif file_stat.staged > 0 then
      file_check = "[-]"
    end

    item_list[#item_list + 1] = {
      filename = filename,
      relpath = hunk.file,
      pos = { hunk.pos, 0 },
      item = {
        category = "Tracked Changes",
        check = hunk.staged and "[x]" or "[ ]",
        file_check = file_check,
        hunk_header = "-" .. old_range .. " +" .. new_range,
        old_range = "-" .. old_range,
        new_range = "+" .. new_range,
        context_text = context_text,
        staged = hunk.staged,
        diff = hunk.diff,
        added = hunk.added,
        removed = hunk.removed,
        added_pad = hunk.added,
        removed_pad = hunk.removed,
        file_added = file_stat.added,
        file_removed = file_stat.removed,
        git_status = hunk.git_status,
        git_original_file = hunk.git_original_file,
        git_path_change_kind = hunk.git_path_change_kind,
      },
    }
  end
  return item_list
end

---@param item_list table[]
---@param untracked_file_list DiffReviewPathStatusFileSnapshot[]
local function append_untracked_items(item_list, untracked_file_list)
  for _, file_snapshot in ipairs(untracked_file_list) do
    item_list[#item_list + 1] = {
      filename = file_snapshot.abs_file,
      relpath = file_snapshot.path,
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
    }
  end
end

---@param snapshot_error DiffReviewPathStatusSnapshotError
local function notify_snapshot_error(snapshot_error)
  if snapshot_error.failure_list and #snapshot_error.failure_list > 0 then
    local failure_list = {}
    for _, failure in ipairs(snapshot_error.failure_list) do
      failure_list[#failure_list + 1] = {
        path = failure.source,
        message = failure.message,
        code = failure.code,
        stdout = failure.stdout,
        stderr = failure.stderr,
        output = failure.output,
      }
    end
    notifications.git_failures("Git status refresh failed", failure_list)
    return
  end
  notifications.error(snapshot_error.message, "Git status refresh failed")
end

---@param cwd string
---@param callback fun(item_list?: table[], error?: DiffReviewPathStatusSnapshotError, snapshot?: DiffReviewPathStatusSnapshot)
---@param context? { skip_pre_render?: boolean, skip_ts_context?: boolean }
local function collect_items_from_git(cwd, callback, context)
  syntax_engine.clear_context_cache()
  syntax_engine.clear_treesitter_source_buffers()
  status_snapshot().collect_async(cwd, {}, function(snapshot, snapshot_error)
    if not snapshot then
      local effective_error = snapshot_error or { kind = "parse", message = "Git status snapshot returned no result" }
      notify_snapshot_error(effective_error)
      callback(nil, effective_error)
      return
    end

    local all_hunk_list, untracked_file_list = snapshot_status_hunk_list(snapshot)
    local file_stat_by_path = snapshot_file_stat_by_path(all_hunk_list)
    local item_list = snapshot_tracked_item_list(cwd, all_hunk_list, file_stat_by_path, context)
    append_untracked_items(item_list, untracked_file_list)

    callback(item_list, nil, snapshot)

    if not (context and context.skip_pre_render) then
      vim.schedule(function()
        for filename, diff_text in pairs(snapshot.file_diffs) do
          if diff_text and diff_text ~= "" and not snapshot.untracked_by_file[filename] then
            local buf = diff_buffer.open_diff_buffer(filename)
            diff_buffer._refresh_diff_buffer(buf, filename)
          end
        end
      end)
    end
  end)
end

-- Expose the bare-local git data builders that init and other modules call by name.
M._parse_diff = parse_diff
M._order_file_hunks = order_file_hunks
M._file_diff_and_flags_async = file_diff_and_flags_async
M._collect_items_from_git = collect_items_from_git
M._parse_name_status_line = parse_name_status_line
M._git_status_is_added = git_status_is_added
M._git_status_is_deleted = git_status_is_deleted
M._git_status_is_renamed = git_status_is_renamed

return M
