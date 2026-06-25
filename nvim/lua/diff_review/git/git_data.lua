--- Owns the git/diff data layer for the status views: the async stage/unstage execution wrappers,
--- unified-diff parsing, hunk extraction/ordering, untracked-diff synthesis, the git-status item
--- collector, and the async tree-sitter syntax computation entry points.
---
--- Reads the notify/filetype/nul helpers and live status state through the init module via dr(),
--- and the git backend, syntax engine, and path helpers as direct requires.

local git_backend = require("diff_review.git.git_backend")
local syntax_engine = require("diff_review.render.syntax_engine")
local paths = require("diff_review.infra.paths")

--- Resolve the init module lazily so git data ops can reach the shared notify/filetype seams and
--- orchestrator state without a load-time circular require.
local util = require("diff_review.infra.util")
local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

local M = {}
require("diff_review.query_runtime")

local function run_file_batch_async(files, args_for_file, title, cb)
  git_backend.git_root_async(function(root, root_err)
  if not root then
    local failures = { { message = root_err or "Unable to find git root" } }
    dr().notify_git_failures(title, failures)
    cb({ ok = false, successes = {}, failures = failures })
    return
  end

  local successes = {}
  local failures = {}
  local seen = {}
  local tasks = {}

  local function finish_all()
    if #failures > 0 then
      dr().notify_git_failures(title, failures)
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
      local relpath, rel_err = paths.repo_relative(filename, root)
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
    dr().notify_git_failures(title, failures)
    return { ok = false, successes = {}, failures = failures }
  end

  local successes = {}
  local failures = {}
  local seen = {}
  for _, filename in ipairs(files) do
    if filename and filename ~= "" and not seen[filename] then
      seen[filename] = true
      local relpath, rel_err = paths.repo_relative(filename, root)
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
    dr().notify_git_failures(title, failures)
  end
  return { ok = #failures == 0, successes = successes, failures = failures }
end

---@param diff string?
---@param cb fun(ok: boolean)
function M.stage_patch_async(diff, cb)
  if not diff or diff == "" then
    dr()._notify_error("No patch to stage")
    cb(false)
    return
  end
  git_backend.run_git_async({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
    if not result.ok then
      dr()._notify_error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
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
    dr()._notify_error("No patch to unstage")
    cb(false)
    return
  end
  git_backend.run_git_async({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n", function(result)
    if not result.ok then
      dr()._notify_error("Unstage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
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
    dr()._notify_error("No patch to stage")
    return false
  end
  local result = git_backend.run_git_sync_for_test_backend({ "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
  if not result.ok then
    dr()._notify_error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return false
  end
  return true
end

---@param diff string?
---@return boolean
function M.unstage_patch(diff)
  if not diff or diff == "" then
    dr()._notify_error("No patch to unstage")
    return false
  end
  local result = git_backend.run_git_sync_for_test_backend({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "--unidiff-zero", "-" }, diff .. "\n")
  if not result.ok then
    dr()._notify_error("Unstage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
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
  local args = dr()._git_diff_command(cwd, staged and { "--cached" } or nil)
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

--- Build a synthetic "new file" diff (every line an addition) for an untracked
--- file, so it previews as a single hunk straight from disk — no git, which is
--- both faster and the only way to show content git doesn't track yet.
---@param filename string absolute path
---@param relpath string path relative to the git root (forward slashes)
---@return string? diff_text
local function build_untracked_diff(filename, relpath)
  if util.file_contains_nul(filename) then return nil end

  local ok, lines = pcall(vim.fn.readfile, filename)
  if not ok or type(lines) ~= "table" or #lines == 0 then
    return nil
  end
  -- Skip binary files (a NUL byte in any line).
  if util.lines_contain_nul(lines) then return nil end
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
    ft = util.detect_filetype(filename)
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
  return dr()._status_perf_span("treesitter.compute_file_syntax_async", session.status and session.status.buf or nil, {
    file = filename,
  }, function()
    local buf = syntax_engine.treesitter_source_buffer(filename)
    if not buf then
      cb(nil)
      return
    end

    local ft = vim.bo[buf].filetype
    if ft == "" then
      ft = util.detect_filetype(filename)
    end
    local lang = vim.treesitter.language.get_lang(ft)
    if not lang then
      cb(nil)
      return
    end

    local highlight_ok, highlight_query = pcall(vim.treesitter.query.get, lang, "highlights")
    if not highlight_ok then highlight_query = nil end

    local parser_ok, parser = dr()._status_perf_span("treesitter.compute_file_syntax_async.get_parser", session.status and session.status.buf or nil, {
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

    local parse_ok, parsed = dr()._status_perf_span("treesitter.compute_file_syntax_async.parse_call", session.status and session.status.buf or nil, {
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
  return dr()._status_perf_span("treesitter.compute_diff_syntax_async", session.status and session.status.buf or nil, {
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
    vim.bo[buf].filetype = ft
    dr()._status_perf_span("treesitter.compute_diff_syntax_async.set_lines", session.status and session.status.buf or nil, {
      file = filename,
      lang = lang,
      source_line_count = #lines,
    }, function()
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    end)

    local parser_ok, parser = dr()._status_perf_span("treesitter.compute_diff_syntax_async.get_parser", session.status and session.status.buf or nil, {
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

    local parse_ok, parsed = dr()._status_perf_span("treesitter.compute_diff_syntax_async.parse_call", session.status and session.status.buf or nil, {
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
local function collect_items_from_git(cwd, cb, _ctx)
  syntax_engine.clear_context_cache() -- clear treesitter context cache on refresh
  syntax_engine.clear_treesitter_source_buffers()
  session.untracked = {} -- map of absolute path -> repo-relative path for untracked files
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
    local filename = paths.repo_file_path(cwd, hunk.file)
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
    local filename = paths.repo_file_path(cwd, f)
    session.untracked[filename] = f -- remember repo-relative path for the synthetic diff
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
  session.file_diffs = {}
  session.file_hunk_staged = {}
  for f, hunks in pairs(file_hunks) do
    local diffs, flags = order_file_hunks(hunks)
    local filename = paths.repo_file_path(cwd, f)
    session.file_diffs[filename] = #diffs > 0 and table.concat(diffs, "\n") or false
    session.file_hunk_staged[filename] = #flags > 0 and flags or nil
  end

  cb(items)

  -- Pre-render all diff buffers so file switching is instant
  if not (_ctx and _ctx.skip_pre_render) then
    vim.schedule(function()
      for filename, diff_text in pairs(session.file_diffs) do
        if diff_text and diff_text ~= "" then
          local buf = dr().open_diff_buffer(filename)
          dr()._refresh_diff_buffer(buf, filename)
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

-- Expose the bare-local git data builders that init and other modules call by name.
M._parse_diff = parse_diff
M._build_untracked_diff = build_untracked_diff
M._file_diff_and_flags_async = file_diff_and_flags_async
M._collect_items_from_git = collect_items_from_git
M._parse_name_status_line = parse_name_status_line
M._git_status_is_added = git_status_is_added
M._git_status_is_deleted = git_status_is_deleted
M._git_status_is_renamed = git_status_is_renamed

return M
