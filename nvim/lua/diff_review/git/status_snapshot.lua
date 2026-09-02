--- Collects authoritative path-scoped Git status and preview metadata for status rendering,
--- keeping every replacement source in one atomic snapshot.

local git_backend = require("diff_review.git.git_backend")
local git_data = require("diff_review.git.git_data")
local paths = require("diff_review.infra.paths")
local untracked_read_limit = 16

---@alias DiffReviewPathStatusKind "ordinary"|"renamed"|"copied"|"unmerged"|"untracked"
---@alias DiffReviewPathStatusSnapshotSource "status"|"unstaged_diff"|"staged_diff"|"unstaged_added_numstat"|"staged_added_numstat"
---@alias DiffReviewPreviewState "loaded"|"unloaded"|"loading"|"omitted"|"error"
---@alias DiffReviewPreviewSource "diff"|"worktree_added"|"index_added"|"index_deleted"|"head_deleted"

---@class DiffReviewPathPreview
---@field state DiffReviewPreviewState
---@field source DiffReviewPreviewSource
---@field added integer
---@field removed integer
---@field line_stats_complete boolean
---@field binary boolean
---@field oid? string
---@field mode? string

---@class DiffReviewPathStatusRecord
---@field kind DiffReviewPathStatusKind
---@field path string repository-relative current path
---@field original_path? string repository-relative source path for a rename or copy
---@field xy string porcelain v2 index and worktree status pair
---@field index_status string porcelain v2 index status code
---@field worktree_status string porcelain v2 worktree status code
---@field submodule? string porcelain v2 submodule state
---@field score? string porcelain v2 rename or copy score
---@field head_mode? string
---@field index_mode? string
---@field worktree_mode? string
---@field head_oid? string
---@field index_oid? string
---@field staged boolean
---@field unstaged boolean
---@field untracked boolean
---@field added boolean
---@field deleted boolean
---@field renamed boolean
---@field copied boolean

---@class DiffReviewPathStatusFileSnapshot
---@field path string repository-relative current path
---@field abs_file string absolute current path
---@field original_path? string repository-relative source path for a rename or copy
---@field status_record DiffReviewPathStatusRecord
---@field unstaged_hunk_list DiffReviewHunk[]
---@field staged_hunk_list DiffReviewHunk[]
---@field unstaged_diff string|false
---@field staged_diff string|false
---@field combined_diff string|false
---@field staged_flag_list? boolean[]
---@field unstaged_preview DiffReviewPathPreview
---@field staged_preview DiffReviewPathPreview

---@class DiffReviewPathStatusCommandFailure
---@field source DiffReviewPathStatusSnapshotSource
---@field command DiffReviewGitCommand
---@field code integer
---@field message string
---@field stdout string
---@field stderr string
---@field output string

---@class DiffReviewPathStatusSnapshotError
---@field kind "input"|"command"|"parse"
---@field message string
---@field source? DiffReviewPathStatusSnapshotSource
---@field failure_list? DiffReviewPathStatusCommandFailure[]

---@class DiffReviewPathStatusSnapshot
---@field root string
---@field full_repository boolean
---@field requested_path_list string[]
---@field affected_path_list string[] repository-relative paths cleared before replacement
---@field affected_file_list string[] absolute paths cleared before cache replacement
---@field status_record_list DiffReviewPathStatusRecord[]
---@field status_record_by_path table<string, DiffReviewPathStatusRecord>
---@field file_list DiffReviewPathStatusFileSnapshot[]
---@field file_by_path table<string, DiffReviewPathStatusFileSnapshot>
---@field file_diffs table<string, string|false> session-ready absolute-path diff cache
---@field file_hunk_staged table<string, boolean[]> session-ready absolute-path staged flag cache
---@field untracked_by_file table<string, string> absolute path to repository-relative path
---@field unstaged_diff_by_path table<string, string|false>
---@field staged_diff_by_path table<string, string|false>
---@field status_output string
---@field unstaged_output string
---@field staged_output string
---@field unstaged_added_numstat_output string
---@field staged_added_numstat_output string

---@class DiffReviewPathStatusSnapshotModule
local M = {}

--- Tests whether a file path string is an absolute filesystem path.
---@param value string Path string to evaluate.
---@return boolean absolute True if path starts with root slash or Windows drive prefix.
local function is_absolute_path(value)
  return value:sub(1, 1) == "/"
    or value:sub(1, 1) == "\\"
    or value:match("^%a:[/\\]") ~= nil
end

--- Normalizes path separators to forward slashes and removes leading relative `./` prefixes.
---@param value string Input path string.
---@return string normalized Normalized relative path string.
local function normalize_relative_path(value)
  return (tostring(vim.fs.normalize(value)):gsub("\\", "/"):gsub("^%./", ""))
end

--- Validates, deduplicates, and converts requested file paths relative to the repository root.
---@param root string Git repository root path.
---@param path_list string[] Array of requested path strings.
---@return string[]? normalized_path_list Validated relative path array, or nil on error.
---@return DiffReviewPathStatusSnapshotError? error Validation error descriptor, or nil.
local function normalize_requested_path_list(root, path_list)
  if type(root) ~= "string" or root == "" then
    return nil, { kind = "input", message = "Git status snapshot requires a repository root" }
  end
  if type(path_list) ~= "table" then
    return nil, { kind = "input", message = "Git status snapshot paths must be a list" }
  end

  local normalized_path_list = {}
  local seen_path = {}
  for path_index, raw_path in ipairs(path_list) do
    if type(raw_path) ~= "string" or raw_path == "" then
      return nil, {
        kind = "input",
        message = ("Git status snapshot path %d must be a non-empty string"):format(path_index),
      }
    end
    if raw_path:find("\0", 1, true) then
      return nil, {
        kind = "input",
        message = ("Git status snapshot path %d contains a NUL byte"):format(path_index),
      }
    end

    local normalized_path
    if is_absolute_path(raw_path) then
      local relative_path, relative_error = paths.repo_relative(raw_path, root)
      if not relative_path then
        return nil, { kind = "input", message = relative_error or "Snapshot path is outside the Git root" }
      end
      normalized_path = normalize_relative_path(relative_path)
    else
      normalized_path = normalize_relative_path(raw_path)
      if normalized_path == ".." or normalized_path:sub(1, 3) == "../" then
        return nil, {
          kind = "input",
          message = ("Git status snapshot path is outside the Git root: %s"):format(raw_path),
        }
      end
    end

    if normalized_path == "." then return {}, nil end
    if not seen_path[normalized_path] then
      seen_path[normalized_path] = true
      normalized_path_list[#normalized_path_list + 1] = normalized_path
    end
  end
  return normalized_path_list, nil
end

--- Splits a NUL-delimited byte string into an array of string tokens.
---@param input string NUL-delimited input string.
---@return string[] parts Array of parsed token strings.
local function split_nul(input)
  local part_list = {}
  local part_start = 1
  while part_start <= #input do
    local separator_start = input:find("\0", part_start, true)
    if not separator_start then
      part_list[#part_list + 1] = input:sub(part_start)
      break
    end
    part_list[#part_list + 1] = input:sub(part_start, separator_start - 1)
    part_start = separator_start + 1
  end
  return part_list
end

--- Extracts a fixed number of space-delimited fields from a porcelain record line.
---@param body string Porcelain record text.
---@param field_count integer Number of fields to extract.
---@return string[]? field_list Array of extracted field strings, or nil if insufficient fields.
---@return string? tail Remaining unparsed tail string.
local function take_fields(body, field_count)
  local field_list = {}
  local field_start = 1
  for field_index = 1, field_count do
    local separator_start = body:find(" ", field_start, true)
    if not separator_start then return nil, nil end
    field_list[field_index] = body:sub(field_start, separator_start - 1)
    field_start = separator_start + 1
  end
  return field_list, body:sub(field_start)
end

--- Constructs a typed path status record from porcelain v2 tokens and metadata.
---@param kind DiffReviewPathStatusKind Record category descriptor.
---@param path string Current relative file path.
---@param xy string Two-character index and worktree status code.
---@param submodule? string Submodule status state.
---@param original_path? string Source path for renames or copies.
---@param score? string Rename or copy similarity score.
---@param metadata? table Mode and OID metadata.
---@return DiffReviewPathStatusRecord record Populated path status descriptor table.
local function status_record(kind, path, xy, submodule, original_path, score, metadata)
  metadata = metadata or {}
  local index_status = xy:sub(1, 1)
  local worktree_status = xy:sub(2, 2)
  local untracked = kind == "untracked"
  local change_kind = score and score:sub(1, 1) or nil
  local renamed = change_kind == "R" or index_status == "R" or worktree_status == "R"
  local copied = change_kind == "C" or index_status == "C" or worktree_status == "C"
  return {
    kind = kind,
    path = normalize_relative_path(path),
    original_path = original_path and normalize_relative_path(original_path) or nil,
    xy = xy,
    index_status = index_status,
    worktree_status = worktree_status,
    submodule = submodule,
    score = score,
    head_mode = metadata.head_mode,
    index_mode = metadata.index_mode,
    worktree_mode = metadata.worktree_mode,
    head_oid = metadata.head_oid,
    index_oid = metadata.index_oid,
    staged = not untracked and index_status ~= ".",
    unstaged = untracked or worktree_status ~= ".",
    untracked = untracked,
    added = untracked or copied or index_status == "A" or worktree_status == "A",
    deleted = index_status == "D" or worktree_status == "D",
    renamed = renamed,
    copied = copied,
  }
end

--- Parses NUL-delimited porcelain v2 output into typed path status records.
---@param output string Raw `git status --porcelain=v2 -z` output string.
---@return DiffReviewPathStatusRecord[]? record_list Array of parsed path records, or nil on error.
---@return string? error Error description string, or nil.
function M.parse_status(output)
  local part_list = split_nul(tostring(output or ""))
  local record_list = {}
  local part_index = 1
  while part_index <= #part_list do
    local part = part_list[part_index]
    if part == "" or part:sub(1, 2) == "# " or part:sub(1, 2) == "! " then
      part_index = part_index + 1
    else
      local record_type = part:sub(1, 1)
      if record_type == "?" then
        local path = part:sub(3)
        if path == "" then return nil, ("Malformed untracked status record %d"):format(part_index) end
        record_list[#record_list + 1] = status_record("untracked", path, "??")
      elseif record_type == "1" then
        local field_list, path = take_fields(part:sub(3), 7)
        if not field_list or not path or path == "" or #field_list[1] ~= 2 then
          return nil, ("Malformed ordinary status record %d"):format(part_index)
        end
        record_list[#record_list + 1] = status_record("ordinary", path, field_list[1], field_list[2], nil, nil, {
          head_mode = field_list[3],
          index_mode = field_list[4],
          worktree_mode = field_list[5],
          head_oid = field_list[6],
          index_oid = field_list[7],
        })
      elseif record_type == "2" then
        local field_list, path = take_fields(part:sub(3), 8)
        local original_path = part_list[part_index + 1]
        if not field_list or not path or path == "" or #field_list[1] ~= 2 or not original_path or original_path == "" then
          return nil, ("Malformed renamed status record %d"):format(part_index)
        end
        local change_kind = field_list[8]:sub(1, 1) == "C" and "copied" or "renamed"
        record_list[#record_list + 1] = status_record(
          change_kind,
          path,
          field_list[1],
          field_list[2],
          original_path,
          field_list[8],
          {
            head_mode = field_list[3],
            index_mode = field_list[4],
            worktree_mode = field_list[5],
            head_oid = field_list[6],
            index_oid = field_list[7],
          }
        )
        part_index = part_index + 1
      elseif record_type == "u" then
        local field_list, path = take_fields(part:sub(3), 9)
        if not field_list or not path or path == "" or #field_list[1] ~= 2 then
          return nil, ("Malformed unmerged status record %d"):format(part_index)
        end
        record_list[#record_list + 1] = status_record("unmerged", path, field_list[1], field_list[2])
      else
        return nil, ("Unsupported porcelain v2 status record %d: %s"):format(part_index, part)
      end
      part_index = part_index + 1
    end
  end
  return record_list, nil
end

--- Appends path arguments after a `--` pathspec separator to a command array.
---@param command DiffReviewGitCommand Command arguments array.
---@param path_list string[] Path list array.
local function append_path_list(command, path_list)
  if #path_list == 0 then return end
  command[#command + 1] = "--"
  vim.list_extend(command, path_list)
end

--- Builds the 5 Git command argument arrays used to collect an atomic status snapshot.
---@param root string Git repository root path.
---@param path_list string[] Target relative path filter list.
---@return table<DiffReviewPathStatusSnapshotSource, DiffReviewGitCommand> commands Map of commands by source key.
local function snapshot_command_by_source(root, path_list)
  local status_command = {
    "git", "--no-optional-locks", "-C", root,
    "status", "--porcelain=v2", "-z", "--untracked-files=all",
  }
  append_path_list(status_command, path_list)

  local unstaged_command = git_backend.git_diff_command(root, { "--diff-filter=MRC" })
  table.insert(unstaged_command, 2, "--no-optional-locks")
  append_path_list(unstaged_command, path_list)

  local staged_command = git_backend.git_diff_command(root, { "--cached", "--diff-filter=MRC" })
  table.insert(staged_command, 2, "--no-optional-locks")
  append_path_list(staged_command, path_list)

  local unstaged_added_numstat_command = {
    "git", "--no-optional-locks", "-C", root,
    "-c", "core.quotepath=false", "diff", "--numstat", "-z", "--diff-filter=A",
  }
  append_path_list(unstaged_added_numstat_command, path_list)

  local staged_added_numstat_command = {
    "git", "--no-optional-locks", "-C", root,
    "-c", "core.quotepath=false", "diff", "--cached", "--numstat", "-z", "--diff-filter=A",
  }
  append_path_list(staged_added_numstat_command, path_list)

  return {
    status = status_command,
    unstaged_diff = unstaged_command,
    staged_diff = staged_command,
    unstaged_added_numstat = unstaged_added_numstat_command,
    staged_added_numstat = staged_added_numstat_command,
  }
end

--- Concatenates individual hunk diff patches into a unified diff text string.
---@param hunk_list DiffReviewHunk[] Array of parsed diff hunks.
---@return string|false diff Concatenated diff patch string, or false if empty.
local function hunk_diff(hunk_list)
  local diff_list = {}
  for _, hunk in ipairs(hunk_list) do
    if hunk.diff then diff_list[#diff_list + 1] = hunk.diff end
  end
  return #diff_list > 0 and table.concat(diff_list, "\n") or false
end

--- Incurs an ordinary modified status record when diff hunks exist without porcelain records.
---@param staged boolean True if staged hunks exist.
---@param unstaged boolean True if unstaged hunks exist.
---@param path string Relative file path string.
---@return DiffReviewPathStatusRecord record Inferred path status record.
local function inferred_status_record(staged, unstaged, path)
  local xy = (staged and "M" or ".") .. (unstaged and "M" or ".")
  return status_record("ordinary", path, xy, "N...")
end

--- Builds a synthetic unified diff patch from raw untracked file bytes.
---@param relpath string Relative file path string.
---@param content string Raw file content string.
---@return string? diff Synthetic unified diff patch, or nil for binary/empty files.
local function build_untracked_diff_from_bytes(relpath, content)
  if content == "" or content:find("\0", 1, true) then return nil end

  local line_list = {}
  local line_start = 1
  while line_start <= #content do
    local newline_start = content:find("\n", line_start, true)
    if not newline_start then
      line_list[#line_list + 1] = content:sub(line_start)
      break
    end
    line_list[#line_list + 1] = content:sub(line_start, newline_start - 1)
    line_start = newline_start + 1
  end

  local diff_line_list = {
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1," .. #line_list .. " @@",
  }
  for _, line in ipairs(line_list) do
    diff_line_list[#diff_line_list + 1] = "+" .. line
  end
  if content:sub(-1) ~= "\n" then
    diff_line_list[#diff_line_list + 1] = "\\ No newline at end of file"
  end
  return table.concat(diff_line_list, "\n")
end

--- Reads untracked file content asynchronously using non-blocking LibUV file system calls.
---@param filename string Absolute file path.
---@param callback fun(content?: string) Completion callback invoked with file content or nil.
local function read_untracked_file_async(filename, callback)
  local uv = vim.uv or vim.loop
  local callback_finished = false

  ---@param content? string
  local function finish(content)
    if callback_finished then return end
    callback_finished = true
    vim.schedule(function() callback(content) end)
  end

  uv.fs_open(filename, "r", 438, function(open_error, file_descriptor)
    if open_error or not file_descriptor then
      finish(nil)
      return
    end

    local function close_and_finish(content)
      uv.fs_close(file_descriptor, function()
        finish(content)
      end)
    end

    uv.fs_fstat(file_descriptor, function(stat_error, file_stat)
      if stat_error or not file_stat or type(file_stat.size) ~= "number" then
        close_and_finish(nil)
        return
      end
      if file_stat.size == 0 then
        close_and_finish("")
        return
      end
      uv.fs_read(file_descriptor, file_stat.size, 0, function(read_error, content)
        close_and_finish(read_error and nil or content)
      end)
    end)
  end)
end

M._build_untracked_diff_from_bytes = build_untracked_diff_from_bytes
M._read_untracked_file_async = read_untracked_file_async

--- Reads an untracked file asynchronously and generates its synthetic diff patch.
---@param filename string Absolute file path.
---@param relpath string Relative repository file path.
---@param callback fun(diff?: string) Callback receiving generated unified diff patch or nil.
function M.read_untracked_diff_async(filename, relpath, callback)
  M._read_untracked_file_async(filename, function(content)
    callback(content and build_untracked_diff_from_bytes(relpath, content) or nil)
  end)
end

---@class DiffReviewNumstat
---@field added integer
---@field removed integer
---@field line_stats_complete boolean
---@field binary boolean

--- Parses NUL-delimited numstat output into exact per-path line addition/deletion statistics.
---@param output string Raw `git diff --numstat -z` output string.
---@return table<string, DiffReviewNumstat>? stat_by_path Map of line statistics by relative path, or nil on error.
---@return string? error Parse error message, or nil.
function M.parse_numstat(output)
  local stat_by_path = {}
  for part_index, part in ipairs(split_nul(tostring(output or ""))) do
    if part ~= "" then
      local first_tab = part:find("\t", 1, true)
      local second_tab = first_tab and part:find("\t", first_tab + 1, true) or nil
      if not first_tab or not second_tab then
        return nil, ("Malformed numstat record %d"):format(part_index)
      end
      local added_text = part:sub(1, first_tab - 1)
      local removed_text = part:sub(first_tab + 1, second_tab - 1)
      local path = normalize_relative_path(part:sub(second_tab + 1))
      local binary = added_text == "-" or removed_text == "-"
      local added = binary and 0 or tonumber(added_text)
      local removed = binary and 0 or tonumber(removed_text)
      if path == "" or added == nil or removed == nil then
        return nil, ("Malformed numstat record %d"):format(part_index)
      end
      stat_by_path[path] = {
        added = added,
        removed = removed,
        line_stats_complete = not binary,
        binary = binary,
      }
    end
  end
  return stat_by_path, nil
end

--- Calculates line additions and binary status directly from raw file bytes.
---@param content string Raw file content string.
---@return DiffReviewNumstat stat Calculated line addition and binary flags.
local function added_file_stat_from_bytes(content)
  if content:find("\0", 1, true) then
    return { added = 0, removed = 0, line_stats_complete = false, binary = true }
  end
  if content == "" then
    return { added = 0, removed = 0, line_stats_complete = true, binary = false }
  end
  local newline_count = select(2, content:gsub("\n", ""))
  local line_count = newline_count + (content:sub(-1) == "\n" and 0 or 1)
  return { added = line_count, removed = 0, line_stats_complete = true, binary = false }
end

--- Reads and calculates line statistics across all untracked files asynchronously in parallel batches.
---@param root string Git repository root path.
---@param status_output string Raw porcelain v2 status output string.
---@param callback fun(stat_by_path?: table<string, DiffReviewNumstat>, error?: DiffReviewPathStatusSnapshotError) Completion callback function.
local function collect_untracked_stat_async(root, status_output, callback)
  local status_record_list, status_error = M.parse_status(status_output)
  if not status_record_list then
    callback(nil, { kind = "parse", source = "status", message = status_error or "Unable to parse Git status" })
    return
  end

  local untracked_record_list = {}
  for _, record in ipairs(status_record_list) do
    if record.untracked then untracked_record_list[#untracked_record_list + 1] = record end
  end
  if #untracked_record_list == 0 then
    callback({})
    return
  end

  local pending_read_count = #untracked_record_list
  local active_read_count = 0
  local next_record_index = 1
  local stat_by_path = {}
  local launch_read
  launch_read = function()
    while active_read_count < untracked_read_limit and next_record_index <= #untracked_record_list do
      local untracked_record = untracked_record_list[next_record_index]
      next_record_index = next_record_index + 1
      active_read_count = active_read_count + 1
      local read_finished = false
      M._read_untracked_file_async(paths.repo_file_path(root, untracked_record.path), function(content)
        if read_finished then return end
        read_finished = true
        if content ~= nil then stat_by_path[untracked_record.path] = added_file_stat_from_bytes(content) end
        active_read_count = active_read_count - 1
        pending_read_count = pending_read_count - 1
        if pending_read_count == 0 then
          callback(stat_by_path)
          return
        end
        launch_read()
      end)
    end
  end
  launch_read()
end

--- Compiles an immutable path status snapshot from status records, diff outputs, and numstats.
---@param root string Git repository root path.
---@param requested_path_list string[] Normalized relative paths requested.
---@param status_output string Raw porcelain v2 status output string.
---@param unstaged_output string Raw unstaged diff output string.
---@param staged_output string Raw staged diff output string.
---@param unstaged_added_numstat_output string Raw unstaged added numstat output string.
---@param staged_added_numstat_output string Raw staged added numstat output string.
---@param untracked_stat_by_path table<string, DiffReviewNumstat> Untracked line statistics map.
---@return DiffReviewPathStatusSnapshot? snapshot Compiled path status snapshot, or nil on error.
---@return DiffReviewPathStatusSnapshotError? error Parse error descriptor, or nil.
local function build_snapshot(
  root,
  requested_path_list,
  status_output,
  unstaged_output,
  staged_output,
  unstaged_added_numstat_output,
  staged_added_numstat_output,
  untracked_stat_by_path
)
  local status_record_list, status_error = M.parse_status(status_output)
  if not status_record_list then
    return nil, { kind = "parse", source = "status", message = status_error or "Unable to parse Git status" }
  end

  local unstaged_ok, unstaged_hunk_list = pcall(git_data._parse_diff, unstaged_output, false)
  if not unstaged_ok then
    return nil, { kind = "parse", source = "unstaged_diff", message = tostring(unstaged_hunk_list) }
  end
  local staged_ok, staged_hunk_list = pcall(git_data._parse_diff, staged_output, true)
  if not staged_ok then
    return nil, { kind = "parse", source = "staged_diff", message = tostring(staged_hunk_list) }
  end
  local unstaged_added_stat_by_path, unstaged_numstat_error = M.parse_numstat(unstaged_added_numstat_output)
  if not unstaged_added_stat_by_path then
    return nil, { kind = "parse", source = "unstaged_added_numstat", message = unstaged_numstat_error or "Unable to parse unstaged added-file stats" }
  end
  local staged_added_stat_by_path, staged_numstat_error = M.parse_numstat(staged_added_numstat_output)
  if not staged_added_stat_by_path then
    return nil, { kind = "parse", source = "staged_added_numstat", message = staged_numstat_error or "Unable to parse staged added-file stats" }
  end

  local status_record_by_path = {}
  local path_order = {}
  local seen_path_order = {}
  local affected_path_list = {}
  local seen_affected_path = {}

  ---@param path string
  local function add_affected_path(path)
    path = normalize_relative_path(path)
    if seen_affected_path[path] then return end
    seen_affected_path[path] = true
    affected_path_list[#affected_path_list + 1] = path
  end

  ---@param path string
  local function add_path_order(path)
    if seen_path_order[path] then return end
    seen_path_order[path] = true
    path_order[#path_order + 1] = path
  end

  for _, requested_path in ipairs(requested_path_list) do add_affected_path(requested_path) end
  for _, record in ipairs(status_record_list) do
    if status_record_by_path[record.path] then
      return nil, {
        kind = "parse",
        source = "status",
        message = ("Git status returned duplicate path records for %s"):format(record.path),
      }
    end
    status_record_by_path[record.path] = record
    add_path_order(record.path)
    add_affected_path(record.path)
    if record.renamed and record.original_path then add_affected_path(record.original_path) end
  end

  ---@type table<string, DiffReviewHunk[]>
  local unstaged_hunk_by_path = {}
  ---@type table<string, DiffReviewHunk[]>
  local staged_hunk_by_path = {}

  ---@param hunk_list DiffReviewHunk[]
  ---@param hunk_by_path table<string, DiffReviewHunk[]>
  local function group_hunk_list(hunk_list, hunk_by_path)
    for _, hunk in ipairs(hunk_list) do
      local path = normalize_relative_path(hunk.file)
      hunk.file = path
      hunk_by_path[path] = hunk_by_path[path] or {}
      hunk_by_path[path][#hunk_by_path[path] + 1] = hunk
      add_path_order(path)
      add_affected_path(path)
    end
  end

  group_hunk_list(unstaged_hunk_list, unstaged_hunk_by_path)
  group_hunk_list(staged_hunk_list, staged_hunk_by_path)

  for _, path in ipairs(path_order) do
    if not status_record_by_path[path] then
      local inferred_record = inferred_status_record(
        #(staged_hunk_by_path[path] or {}) > 0,
        #(unstaged_hunk_by_path[path] or {}) > 0,
        path
      )
      status_record_by_path[path] = inferred_record
      status_record_list[#status_record_list + 1] = inferred_record
    end
  end

  local file_list = {}
  local file_by_path = {}
  local file_diffs = {}
  local file_hunk_staged = {}
  local untracked_by_file = {}
  local unstaged_diff_by_path = {}
  local staged_diff_by_path = {}

  for _, path in ipairs(path_order) do
    local record = status_record_by_path[path]
    local abs_file = paths.repo_file_path(root, path)
    local file_unstaged_hunk_list = unstaged_hunk_by_path[path] or {}
    local file_staged_hunk_list = staged_hunk_by_path[path] or {}

    if record.untracked then untracked_by_file[abs_file] = path end

    for _, hunk in ipairs(file_unstaged_hunk_list) do
      hunk.filename = abs_file
      hunk.git_status = record.untracked and "??" or record.worktree_status
      hunk.git_original_file = record.original_path
      hunk.git_path_change_kind = (record.kind == "renamed" or record.kind == "copied") and record.kind or nil
    end
    for _, hunk in ipairs(file_staged_hunk_list) do
      hunk.filename = abs_file
      hunk.git_status = record.index_status
      hunk.git_original_file = record.original_path
      hunk.git_path_change_kind = (record.kind == "renamed" or record.kind == "copied") and record.kind or nil
    end

    local combined_hunk_list = {}
    vim.list_extend(combined_hunk_list, file_unstaged_hunk_list)
    vim.list_extend(combined_hunk_list, file_staged_hunk_list)
    local combined_diff_list, staged_flag_list = git_data._order_file_hunks(combined_hunk_list)
    local combined_diff = #combined_diff_list > 0 and table.concat(combined_diff_list, "\n") or false
    local unstaged_diff = hunk_diff(file_unstaged_hunk_list)
    local staged_diff = hunk_diff(file_staged_hunk_list)

    ---@param staged boolean
    ---@param hunk_list DiffReviewHunk[]
    ---@return DiffReviewPathPreview
    local function preview_for_stage(staged, hunk_list)
      local status = staged and record.index_status or record.worktree_status
      if #hunk_list > 0 then
        local added = 0
        local removed = 0
        for _, hunk in ipairs(hunk_list) do
          added = added + (hunk.added or 0)
          removed = removed + (hunk.removed or 0)
        end
        return {
          state = "loaded",
          source = "diff",
          added = added,
          removed = removed,
          line_stats_complete = true,
          binary = false,
        }
      end
      if record.untracked or status == "A" then
        local stat = record.untracked and (untracked_stat_by_path and untracked_stat_by_path[path])
          or (staged and staged_added_stat_by_path[path] or unstaged_added_stat_by_path[path])
          or { added = 0, removed = 0, line_stats_complete = false, binary = false }
        return {
          state = "unloaded",
          source = staged and "index_added" or "worktree_added",
          added = stat.added,
          removed = stat.removed,
          line_stats_complete = stat.line_stats_complete,
          binary = stat.binary,
          oid = staged and record.index_oid or nil,
          mode = staged and record.index_mode or record.worktree_mode,
        }
      end
      if status == "D" then
        return {
          state = "unloaded",
          source = staged and "head_deleted" or "index_deleted",
          added = 0,
          removed = 0,
          line_stats_complete = false,
          binary = false,
          oid = staged and record.head_oid or record.index_oid,
          mode = staged and record.head_mode or record.index_mode,
        }
      end
      return {
        state = "loaded",
        source = "diff",
        added = 0,
        removed = 0,
        line_stats_complete = true,
        binary = false,
      }
    end

    local unstaged_preview = preview_for_stage(false, file_unstaged_hunk_list)
    local staged_preview = preview_for_stage(true, file_staged_hunk_list)

    ---@type DiffReviewPathStatusFileSnapshot
    local file_snapshot = {
      path = path,
      abs_file = abs_file,
      original_path = record.original_path,
      status_record = record,
      unstaged_hunk_list = file_unstaged_hunk_list,
      staged_hunk_list = file_staged_hunk_list,
      unstaged_diff = unstaged_diff,
      staged_diff = staged_diff,
      combined_diff = combined_diff,
      staged_flag_list = #staged_flag_list > 0 and staged_flag_list or nil,
      unstaged_preview = unstaged_preview,
      staged_preview = staged_preview,
    }
    file_list[#file_list + 1] = file_snapshot
    file_by_path[path] = file_snapshot
    file_diffs[abs_file] = combined_diff
    if file_snapshot.staged_flag_list then file_hunk_staged[abs_file] = file_snapshot.staged_flag_list end
    unstaged_diff_by_path[path] = unstaged_diff
    staged_diff_by_path[path] = staged_diff
  end

  local affected_file_list = {}
  for _, affected_path in ipairs(affected_path_list) do
    affected_file_list[#affected_file_list + 1] = paths.repo_file_path(root, affected_path)
  end

  return {
    root = root,
    full_repository = #requested_path_list == 0,
    requested_path_list = vim.deepcopy(requested_path_list),
    affected_path_list = affected_path_list,
    affected_file_list = affected_file_list,
    status_record_list = status_record_list,
    status_record_by_path = status_record_by_path,
    file_list = file_list,
    file_by_path = file_by_path,
    file_diffs = file_diffs,
    file_hunk_staged = file_hunk_staged,
    untracked_by_file = untracked_by_file,
    unstaged_diff_by_path = unstaged_diff_by_path,
    staged_diff_by_path = staged_diff_by_path,
    status_output = status_output,
    unstaged_output = unstaged_output,
    staged_output = staged_output,
    unstaged_added_numstat_output = unstaged_added_numstat_output,
    staged_added_numstat_output = staged_added_numstat_output,
  }, nil
end

--- Builds an authoritative path status snapshot from status, filtered diff patches, and numstat outputs.
---@param root string Git repository root path.
---@param path_list string[] Target pathspec filter array.
---@param status_output string Raw porcelain v2 status output string.
---@param unstaged_output string Raw unstaged diff output string.
---@param staged_output string Raw staged diff output string.
---@param unstaged_added_numstat_output? string Raw unstaged added numstat output string.
---@param staged_added_numstat_output? string Raw staged added numstat output string.
---@return DiffReviewPathStatusSnapshot? snapshot Compiled path status snapshot, or nil on error.
---@return DiffReviewPathStatusSnapshotError? error Snapshot construction error descriptor, or nil.
function M.build(root, path_list, status_output, unstaged_output, staged_output, unstaged_added_numstat_output, staged_added_numstat_output)
  local requested_path_list, input_error = normalize_requested_path_list(root, path_list)
  if not requested_path_list then return nil, input_error end
  return build_snapshot(
    root,
    requested_path_list,
    tostring(status_output or ""),
    tostring(unstaged_output or ""),
    tostring(staged_output or ""),
    tostring(unstaged_added_numstat_output or ""),
    tostring(staged_added_numstat_output or ""),
    {}
  )
end

--- Formats a Git process failure record from process return code and output streams.
---@param source DiffReviewPathStatusSnapshotSource Snapshot phase key.
---@param command DiffReviewGitCommand Command arguments array.
---@param result DiffReviewGitAsyncResult Process result table.
---@return DiffReviewPathStatusCommandFailure failure Formatted failure descriptor.
local function command_failure(source, command, result)
  local stdout = tostring(result.stdout or "")
  local stderr = tostring(result.stderr or "")
  local output = tostring(result.output or "")
  if output == "" then output = git_backend.system_output(stdout, stderr) end
  local message = vim.trim(stderr)
  if message == "" then message = vim.trim(output) end
  if message == "" then message = ("git exited with code %d"):format(result.code or -1) end
  return {
    source = source,
    command = vim.deepcopy(command),
    code = result.code or -1,
    message = message,
    stdout = stdout,
    stderr = stderr,
    output = output,
  }
end

--- Asynchronously collects an atomic path status snapshot covering porcelain status, diffs, and numstats.
---@param root string Git repository root path.
---@param path_list string[] Target relative path list (empty list captures full repository).
---@param callback fun(snapshot?: DiffReviewPathStatusSnapshot, error?: DiffReviewPathStatusSnapshotError) Completion callback function.
function M.collect_async(root, path_list, callback)
  local requested_path_list, input_error = normalize_requested_path_list(root, path_list)
  if not requested_path_list then
    vim.schedule(function() callback(nil, input_error) end)
    return
  end

  local command_by_source = snapshot_command_by_source(root, requested_path_list)
  ---@type table<DiffReviewPathStatusSnapshotSource, DiffReviewGitAsyncResult>
  local result_by_source = {}
  local pending = 5
  local finished_source = {}
  local callback_finished = false

  ---@param snapshot? DiffReviewPathStatusSnapshot
  ---@param snapshot_error? DiffReviewPathStatusSnapshotError
  local function complete(snapshot, snapshot_error)
    if callback_finished then return end
    callback_finished = true
    callback(snapshot, snapshot_error)
  end

  ---@param source DiffReviewPathStatusSnapshotSource
  ---@param result DiffReviewGitAsyncResult
  local function finish(source, result)
    if callback_finished or finished_source[source] then return end
    finished_source[source] = true
    result_by_source[source] = result
    pending = pending - 1
    if pending > 0 then return end

    local source_order = {
      "status",
      "unstaged_diff",
      "staged_diff",
      "unstaged_added_numstat",
      "staged_added_numstat",
    }
    local failure_list = {}
    for _, ordered_source in ipairs(source_order) do
      local ordered_result = result_by_source[ordered_source]
      if not ordered_result or (ordered_result.code or 0) ~= 0 then
        failure_list[#failure_list + 1] = command_failure(
          ordered_source,
          command_by_source[ordered_source],
          ordered_result or { code = -1, stdout = "", stderr = "Git command returned no result", output = "" }
        )
      end
    end
    if #failure_list > 0 then
      local message_list = {}
      for _, failure in ipairs(failure_list) do
        message_list[#message_list + 1] = failure.source .. ": " .. failure.message
      end
      complete(nil, {
        kind = "command",
        message = "Git status snapshot failed: " .. table.concat(message_list, "; "),
        failure_list = failure_list,
      })
      return
    end

    local status_output = tostring(result_by_source.status.stdout or "")
    collect_untracked_stat_async(root, status_output, function(untracked_stat_by_path, untracked_error)
      if not untracked_stat_by_path then
        complete(nil, untracked_error)
        return
      end
      complete(build_snapshot(
        root,
        requested_path_list,
        status_output,
        tostring(result_by_source.unstaged_diff.stdout or ""),
        tostring(result_by_source.staged_diff.stdout or ""),
        tostring(result_by_source.unstaged_added_numstat.stdout or ""),
        tostring(result_by_source.staged_added_numstat.stdout or ""),
        untracked_stat_by_path
      ))
    end)
  end

  for _, source in ipairs({
    "status",
    "unstaged_diff",
    "staged_diff",
    "unstaged_added_numstat",
    "staged_added_numstat",
  }) do
    git_backend.system_text_async(command_by_source[source], nil, function(result)
      finish(source, result)
    end)
  end
end

return M
