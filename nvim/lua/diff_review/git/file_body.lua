--- Loads deferred added and deleted file previews without requesting unified Git diffs.
--- Resolves immutable staged content through porcelain object IDs and worktree content from disk.

local config = require("diff_review.infra.config")
local git_backend = require("diff_review.git.git_backend")
local git_data = require("diff_review.git.git_data")
local status_snapshot = require("diff_review.git.status_snapshot")

---@class DiffReviewFileBodyResult
---@field state DiffReviewPreviewState
---@field hunks DiffReviewHunk[]
---@field diff string|false
---@field added integer
---@field removed integer
---@field line_stats_complete boolean
---@field binary boolean
---@field error? string

local M = {}

--- Splits raw content text into an array of lines.
---@param content string Raw text content string.
---@return string[] lines Array of line strings.
local function content_line_list(content)
  if content == "" then return {} end
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
  return line_list
end

--- Generates a synthetic unified diff patch for an added or deleted file.
---@param path string Relative file path string.
---@param content string File body content string.
---@param added boolean True if file was added, false if deleted.
---@param mode? string File mode string.
---@return string|false patch Synthetic diff text string, or false if content was empty.
local function synthetic_file_patch(path, content, added, mode)
  local line_list = content_line_list(content)
  if #line_list == 0 then return false end
  local normalized_mode = type(mode) == "string" and mode:match("^%d+$") and mode or "100644"
  local diff_line_list = {
    "diff --git a/" .. path .. " b/" .. path,
    (added and "new file mode " or "deleted file mode ") .. normalized_mode,
    added and "--- /dev/null" or "--- a/" .. path,
    added and "+++ b/" .. path or "+++ /dev/null",
    added and ("@@ -0,0 +1,%d @@"):format(#line_list) or ("@@ -1,%d +0,0 @@"):format(#line_list),
  }
  local prefix = added and "+" or "-"
  for _, line in ipairs(line_list) do
    diff_line_list[#diff_line_list + 1] = prefix .. line
  end
  if content:sub(-1) ~= "\n" then
    diff_line_list[#diff_line_list + 1] = "\\ No newline at end of file"
  end
  return table.concat(diff_line_list, "\n")
end

--- Constructs an error result descriptor for failed body resolution.
---@param message string Error description string.
---@return DiffReviewFileBodyResult result Formatted error outcome table.
local function error_result(message)
  return {
    state = "error",
    hunks = {},
    diff = false,
    added = 0,
    removed = 0,
    line_stats_complete = false,
    binary = false,
    error = message,
  }
end

--- Converts raw file text into a parsed preview result structure.
---@param file DiffReviewStatusFile Status file entry descriptor.
---@param content string Raw file content string.
---@return DiffReviewFileBodyResult result Populated file body result table.
local function content_result(file, content)
  if content:find("\0", 1, true) then
    return {
      state = "loaded",
      hunks = {},
      diff = false,
      added = 0,
      removed = 0,
      line_stats_complete = false,
      binary = true,
    }
  end
  local added = file.preview_source == "worktree_added" or file.preview_source == "index_added"
  local line_count = #content_line_list(content)
  local options = config.options or config.defaults
  local limit = math.max(0, math.floor(tonumber(options.status_file_preview_line_limit) or 1000))
  if line_count > limit then
    return {
      state = "omitted",
      hunks = {},
      diff = false,
      added = added and line_count or 0,
      removed = added and 0 or line_count,
      line_stats_complete = true,
      binary = false,
    }
  end
  local patch = synthetic_file_patch(file.relpath, content, added, file.preview_mode)
  local hunk_list = patch and git_data._parse_diff(patch, file.section_name == "staged") or {}
  for _, hunk in ipairs(hunk_list) do
    hunk.filename = file.filename
    hunk.section_name = file.section_name
    hunk.git_status = file.git_status
  end
  return {
    state = "loaded",
    hunks = hunk_list,
    diff = patch,
    added = added and line_count or 0,
    removed = added and 0 or line_count,
    line_stats_complete = true,
    binary = false,
  }
end

--- Reads immutable Git object blob text asynchronously.
---@param root string Git repository root path.
---@param file DiffReviewStatusFile Status file entry descriptor.
---@param callback fun(result: DiffReviewFileBodyResult) Completion callback function.
local function read_blob_async(root, file, callback)
  local oid = file.preview_oid
  if type(oid) ~= "string" or oid == "" or oid:match("^0+$") then
    callback(error_result(("Git status did not provide a blob object for %s"):format(file.relpath)))
    return
  end
  git_backend.system_text_async({ "git", "--no-optional-locks", "-C", root, "cat-file", "blob", oid }, nil, function(result)
    if (result.code or 0) ~= 0 then
      local message = vim.trim(result.stderr or result.output or "")
      callback(error_result(message ~= "" and message or ("Unable to read Git blob %s"):format(oid)))
      return
    end
    callback(content_result(file, tostring(result.stdout or "")))
  end)
end

--- Reads file content asynchronously from worktree disk or Git blob storage.
---@param root string Git repository root path.
---@param file DiffReviewStatusFile Status file entry descriptor.
---@param callback fun(result: DiffReviewFileBodyResult) Completion callback function.
local function read_content_async(root, file, callback)
  if file.preview_source == "worktree_added" then
    status_snapshot._read_untracked_file_async(file.filename, function(content)
      if content == nil then
        callback(error_result(("Unable to read %s"):format(file.relpath)))
        return
      end
      callback(content_result(file, content))
    end)
    return
  end
  read_blob_async(root, file, callback)
end

--- Queries deletion numstats and loads deleted file content asynchronously.
---@param root string Git repository root path.
---@param file DiffReviewStatusFile Status file entry descriptor.
---@param callback fun(result: DiffReviewFileBodyResult) Completion callback function.
local function load_deleted_async(root, file, callback)
  local command = {
    "git", "--no-optional-locks", "-C", root,
    "-c", "core.quotepath=false", "diff",
  }
  if file.preview_source == "head_deleted" then command[#command + 1] = "--cached" end
  vim.list_extend(command, { "--numstat", "-z", "--diff-filter=D", "--", file.relpath })
  git_backend.system_text_async(command, nil, function(result)
    if (result.code or 0) ~= 0 then
      local message = vim.trim(result.stderr or result.output or "")
      callback(error_result(message ~= "" and message or ("Unable to count deleted lines for %s"):format(file.relpath)))
      return
    end
    local stat_by_path, parse_error = status_snapshot.parse_numstat(result.stdout or "")
    local stat = stat_by_path and stat_by_path[file.relpath] or nil
    if not stat then
      callback(error_result(parse_error or ("Git returned no deleted-file metadata for %s"):format(file.relpath)))
      return
    end
    if stat.binary then
      callback({
        state = "loaded",
        hunks = {},
        diff = false,
        added = 0,
        removed = 0,
        line_stats_complete = false,
        binary = true,
      })
      return
    end
    local options = config.options or config.defaults
    local limit = math.max(0, math.floor(tonumber(options.status_file_preview_line_limit) or 1000))
    if stat.removed > limit then
      callback({
        state = "omitted",
        hunks = {},
        diff = false,
        added = 0,
        removed = stat.removed,
        line_stats_complete = true,
        binary = false,
      })
      return
    end
    read_content_async(root, file, callback)
  end)
end

--- Asynchronously loads deferred file bodies and hunks for added or deleted files.
---@param root string Git repository root path.
---@param file DiffReviewStatusFile Status file entry descriptor.
---@param callback fun(result: DiffReviewFileBodyResult) Completion callback function.
function M.load_async(root, file, callback)
  if file.preview_binary then
    callback({
      state = "loaded",
      hunks = {},
      diff = false,
      added = file.added or 0,
      removed = file.removed or 0,
      line_stats_complete = file.line_stats_complete == true,
      binary = true,
    })
    return
  end
  if file.preview_source == "head_deleted" or file.preview_source == "index_deleted" then
    load_deleted_async(root, file, callback)
    return
  end
  if file.preview_source == "worktree_added" or file.preview_source == "index_added" then
    read_content_async(root, file, callback)
    return
  end
  callback(error_result(("Unsupported preview source for %s"):format(file.relpath)))
end

M._synthetic_file_patch = synthetic_file_patch

return M
