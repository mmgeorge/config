--- Owns the status section model: assembling sections from git items / diffs / PR data / commit
--- logs, and the section-map data structure (ordered sections keyed by name, with file get-or-insert,
--- hunk append, optimistic stage/unstage moves, and reindexing).
---
--- Reads the diff parser, git collector, section order/index, fold state, key builders, and the
--- pr_overview/section_builder modules via direct requires.

local git_backend = require("diff_review.git.git_backend")

local pr_overview = require("diff_review.views.pr.pr_overview")
local git_data = require("diff_review.git.git_data")
local section_builder = require("diff_review.views.status.section_builder")
local status_keys = require("diff_review.views.status.status_keys")
local status_head = require("diff_review.views.status.status_head")
local session = require("diff_review.session")

local M = {}

---@alias DiffReviewStatusStageSectionName "unstaged"|"staged"
---@alias DiffReviewStatusDisplaySectionName "unstaged"|"staged"|"ignored"

--- Represents one value-addressed file or hunk selected for a status move.
---@class DiffReviewStatusMoveSelection
---@field kind "file"|"hunk"
---@field filename string
---@field source_section DiffReviewStatusStageSectionName
---@field hunk_identity? string

--- Defines one semantic stage or unstage layer for optimistic projection.
---@class DiffReviewStatusMove
---@field target_section DiffReviewStatusStageSectionName
---@field selection_list DiffReviewStatusMoveSelection[]

---@alias DiffReviewAffectedPathSet table<string, boolean>

--- Represents visible hunk state used for authoritative projection comparison.
---@class DiffReviewStatusSemanticHunk
---@field identity string
---@field pos integer
---@field diff string
---@field staged boolean
---@field context_text string
---@field git_status string
---@field git_original_file string
---@field added integer
---@field removed integer

--- Represents visible file state used for authoritative projection comparison.
---@class DiffReviewStatusSemanticFile
---@field filename string
---@field relpath string
---@field original_relpath string
---@field path_change_kind string
---@field added integer
---@field removed integer
---@field untracked boolean
---@field git_status string
---@field hunk_list DiffReviewStatusSemanticHunk[]

---@alias DiffReviewStatusSemanticSectionMap table<DiffReviewStatusDisplaySectionName, DiffReviewStatusSemanticFile[]>

--- Represents one complete status model produced by a successful load.
---@class DiffReviewStatusLoadSuccess
---@field head_lines DiffReviewStatusHeadLine[]
---@field head_values table
---@field sections DiffReviewStatusSection[]
---@field snapshot DiffReviewPathStatusSnapshot
---@field error nil

--- Represents one actionable status load failure without a synthetic model.
---@class DiffReviewStatusLoadFailure
---@field error DiffReviewPathStatusSnapshotError

---@alias DiffReviewStatusLoadResult DiffReviewStatusLoadSuccess|DiffReviewStatusLoadFailure

--- Ordered status sections. Owned here, the sole consumer; previously
--- parked on the init table as M._status_section_order during the monolith era.
---@type DiffReviewSectionConfig[]
local status_section_order = {
  { name = "unstaged", title = "Unstaged changes", default_folded = false },
  { name = "staged", title = "Staged changes", default_folded = false },
  { name = "ignored", title = "Ignored changes", default_folded = true },
}

--- Resolves target status section name for a collected Git item.
---@param item table Collected Git item descriptor.
---@return "staged"|"unstaged" section_name Target section name string.
local function status_section_for_item(item)
  local data = item.item or {}
  if data.staged then return "staged" end
  return "unstaged"
end

--- Retrieves sorting priority rank for a file from active walkthrough plan state.
---@param buf integer? Status buffer handle.
---@param file DiffReviewStatusFile Status file descriptor.
---@return integer? rank Walkthrough sort priority rank, or nil.
function M._status_walkthrough_file_rank(buf, file)
  if not buf then return nil end
  local walkthrough = package.loaded["diff_review.views.walkthrough"]
  if not (walkthrough and type(walkthrough.file_sort_rank) == "function") then return nil end
  local ok, rank = pcall(walkthrough.file_sort_rank, buf, file)
  if ok and type(rank) == "number" then return rank end
  return nil
end

--- Resolves string path sort key for ordering status file entries.
---@param file DiffReviewStatusFile Status file descriptor.
---@return string key Path string sort key.
function M._status_file_path_sort_key(file)
  return tostring((file and (file.relpath or file.filename)) or "")
end

--- Sorts files within a section by walkthrough priority rank and alphabetical path.
---@param buf integer? Status buffer handle.
---@param section DiffReviewStatusSection Status section table.
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

--- Sorts files across all sections in a section array before rendering.
---@param buf integer? Status buffer handle.
---@param sections DiffReviewStatusSection[] Array of status sections.
function M._status_sort_sections_for_render(buf, sections)
  for _, section in ipairs(sections or {}) do
    M._status_sort_section_files(buf, section)
  end
end

--- Assembles status sections, files, and hunks from collected Git items.
---@param collected_items table[] Array of raw collected Git items.
---@return DiffReviewStatusSection[] sections Array of populated and sorted sections.
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
          relpath = item.relpath or data.relpath or vim.fn.fnamemodify(filename, ":."),
          section_name = section_name,
          added = 0,
          removed = 0,
          hunks = {},
          untracked = section_name ~= "staged" and is_untracked,
          status = data.stats or data.hunk_header or "",
          git_status = data.git_status,
          original_relpath = data.git_original_file,
          path_change_kind = data.git_path_change_kind,
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
        ---@diagnostic disable-next-line: missing-fields
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
    M._status_sort_section_files(session.status and session.status.buf or nil, section)
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

--- Parses git status flags and original path renames from unified diff headers.
---@param diff_text string Unified diff text string.
---@return table<string, DiffReviewDiffFileStatus> statuses Map from relative path to diff file status.
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

--- Constructs status file descriptors and hunks for a diff provider.
---@param cwd string Git repository root path.
---@param provider DiffReviewStatusDiffProvider Diff provider configuration table.
---@param diff_text? string Raw unified diff text.
---@return DiffReviewStatusFile[] files Array of parsed status file descriptors.
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

  for _, parsed_hunk in ipairs(git_data._parse_diff(diff_text or "", false)) do
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

--- Parses status file descriptors for a specific commit diff.
---@param cwd string Git repository root path.
---@param commit DiffReviewStatusCommit Commit descriptor table.
---@param diff_text string Unified diff text string.
---@return DiffReviewStatusFile[] files Array of parsed status file descriptors.
local function status_commit_files_from_diff(cwd, commit, diff_text)
  return status_files_from_diff_provider(cwd, {
    section_name = status_keys.commit_key(commit.oid),
    default_status = "modified",
  }, diff_text)
end

--- Builds full PR section list including reviews, issue comments, changes, and commits.
---@param cwd string Git repository root path.
---@param pr DiffReviewGhPR Pull request descriptor.
---@param diff_text? string Raw unified diff text.
---@param comments? DiffReviewGhPRCommentsResult Remote PR comments payload.
---@param local_comments? table[] Array of local review comments.
---@param local_issue_comments? table[] Array of local issue comments.
---@return DiffReviewStatusSection[] sections Array of constructed PR sections.
local function status_pr_sections(cwd, pr, diff_text, comments, local_comments, local_issue_comments)
  local provider_key = "pr:" .. tostring(pr.number)
  local change_sections, files = section_builder.sections_from_diff(cwd, {
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
    local key = pr_overview.comment_identity_key(comment)
    if key then local_comment_keys[key] = true end
  end
  for _, comment in ipairs(comments and comments.code_comments or {}) do
    local key = pr_overview.comment_identity_key(comment)
    if not (key and local_comment_keys[key]) then code_comments[#code_comments + 1] = comment end
  end
  vim.list_extend(code_comments, local_comments or {})
  section_builder.attach_comments(cwd, files, code_comments, { field = "pr_comments" })
  local sections = {}
  local reviews_section = pr_overview.reviews_section(comments)
  if reviews_section then sections[#sections + 1] = reviews_section end
  local issue_comments_section = pr_overview.issue_comments_section(comments, local_issue_comments)
  if issue_comments_section then sections[#sections + 1] = issue_comments_section end
  vim.list_extend(sections, change_sections)
  local commits_section = pr_overview.commits_section(pr)
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

--- Parses git log tabular output into status commit descriptors.
---@param spec DiffReviewCommitLogSectionSpec Commit log specification table.
---@param output string[] Output lines from git log command.
---@return DiffReviewStatusCommit[] commits Array of parsed commit descriptor tables.
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
      local cache = session.status and session.status.commit_file_cache and session.status.commit_file_cache[oid] or nil
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

--- Asynchronously executes git log and builds a commit log status section.
---@param cwd string Git repository root path.
---@param spec DiffReviewCommitLogSectionSpec Section specification table.
---@param cb fun(section?: DiffReviewStatusSection) Completion callback receiving section or nil.
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

--- Asynchronously builds the unmerged commits status section for upstream comparisons.
---@param cwd string Git repository root path.
---@param upstream string? Upstream branch reference string.
---@param branch string? Current branch name string.
---@param cb fun(section?: DiffReviewStatusSection) Completion callback receiving section or nil.
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

--- Asynchronously builds the recent commits status section.
---@param cwd string Git repository root path.
---@param upstream string? Optional upstream branch reference string.
---@param branch string? Current branch name string.
---@param cb fun(section?: DiffReviewStatusSection) Completion callback receiving section or nil.
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

--- Constructs an empty section map table for standard status sections.
---@return table<DiffReviewStatusSectionName, DiffReviewStatusSection> map Empty section map dictionary.
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

--- Converts an array of status sections into a section map dictionary keyed by section name.
---@param sections DiffReviewStatusSection[]? Array of status sections.
---@return table<DiffReviewStatusSectionName, DiffReviewStatusSection> map Populated section map dictionary.
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

--- Rebuilds the `files_by_name` lookup index for a section.
---@param section DiffReviewStatusSection Target status section table.
local function status_reindex_section(section)
  section.files_by_name = {}
  for _, file in ipairs(section.files or {}) do
    section.files_by_name[file.filename] = file
  end
end

--- Removes a file from a section in a section map.
---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection> Section map dictionary.
---@param section_name DiffReviewStatusSectionName Target section name.
---@param filename string Absolute file path string.
---@return DiffReviewStatusFile? file Removed file descriptor, or nil if not found.
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

--- Sorts and flattens a section map into an ordered status section array.
---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection> Section map dictionary.
---@param source_sections DiffReviewStatusSection[] Original source section array.
---@param buf integer? Status buffer handle.
---@return DiffReviewStatusSection[] sections Ordered status section array.
local function status_order_section_map(section_map, source_sections, buf)
  local ordered = {}
  local included_section = {}
  for _, section_config in ipairs(status_section_order) do
    local section = section_map[section_config.name]
    M._status_sort_section_files(buf, section)
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
    included_section[section.name] = true
    if #section.files > 0 then ordered[#ordered + 1] = section end
  end
  for _, source_section in ipairs(source_sections or {}) do
    if not included_section[source_section.name] then
      local section = section_map[source_section.name] or source_section
      ordered[#ordered + 1] = section
      included_section[source_section.name] = true
    end
  end
  return ordered
end

--- Generates a stable deterministic fingerprint string for a diff hunk.
---@param hunk DiffReviewHunk Diff hunk descriptor table.
---@return string identity Deterministic hunk identity string.
local function status_hunk_identity(hunk)
  if type(hunk.diff) == "string" and hunk.diff ~= "" then
    return "diff:" .. vim.fn.sha256(hunk.diff)
  end
  return table.concat({
    tostring(hunk.file or hunk.filename or ""),
    tostring(hunk.pos or 0),
    tostring(hunk.added or 0),
    tostring(hunk.removed or 0),
    tostring(hunk.context_text or hunk.context or ""),
    tostring(hunk.git_original_file or ""),
  }, "\31")
end

--- Checks whether a status file represents an untracked or newly added file.
---@param file DiffReviewStatusFile Status file descriptor.
---@return boolean is_added True if file is untracked or newly created.
local function status_file_is_added(file)
  return file.untracked == true or git_data._git_status_is_added(file.git_status)
end

--- Deep copies a status file descriptor and adapts its state for a new target section.
---@param file DiffReviewStatusFile Source status file descriptor.
---@param section_name DiffReviewStatusDisplaySectionName Destination section name.
---@return DiffReviewStatusFile file Adapted clone of status file descriptor.
local function status_copy_file_for_section(file, section_name)
  local copied_file = vim.deepcopy(file)
  copied_file.section_name = section_name
  copied_file.untracked = section_name ~= "staged" and status_file_is_added(file)
  if copied_file.untracked then
    copied_file.git_status = "??"
  elseif section_name == "staged" and status_file_is_added(file) then
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

--- Retrieves or initializes a status file descriptor within a section in a section map.
---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection> Section map dictionary.
---@param section_name DiffReviewStatusStageSectionName Destination stage section name.
---@param file DiffReviewStatusFile Source status file descriptor.
---@return DiffReviewStatusFile file Created or updated file descriptor.
local function status_ensure_file(section_map, section_name, file)
  local section = section_map[section_name]
  local existing_file = section.files_by_name[file.filename]
  if existing_file then
    if section_name == "unstaged" and status_file_is_added(file) then
      existing_file.untracked = true
      existing_file.git_status = "??"
    elseif section_name == "staged" and status_file_is_added(file) then
      existing_file.git_status = "A"
    end
    return existing_file
  end
  local is_untracked = section_name == "unstaged" and status_file_is_added(file)
  local git_status = file.git_status
  if is_untracked then
    git_status = "??"
  elseif section_name == "staged" and status_file_is_added(file) then
    git_status = "A"
  end
  existing_file = {
    filename = file.filename,
    relpath = file.relpath,
    original_relpath = file.original_relpath,
    path_change_kind = file.path_change_kind,
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

--- Appends a diff hunk to a file if not already present and updates line count statistics.
---@param file DiffReviewStatusFile Target status file descriptor.
---@param hunk DiffReviewHunk Diff hunk descriptor table.
---@return boolean appended True if hunk was appended.
local function status_append_hunk_to_file(file, hunk)
  for _, existing_hunk in ipairs(file.hunks or {}) do
    if status_hunk_identity(existing_hunk) == status_hunk_identity(hunk) then return false end
  end
  file.hunks = file.hunks or {}
  file.hunks[#file.hunks + 1] = hunk
  file.added = (file.added or 0) + (hunk.added or 0)
  file.removed = (file.removed or 0) + (hunk.removed or 0)
  return true
end

--- Merges a file descriptor and its hunks into a designated section within a section map.
---@param section_map table<DiffReviewStatusSectionName, DiffReviewStatusSection> Section map dictionary.
---@param section_name DiffReviewStatusSectionName Destination section name.
---@param file DiffReviewStatusFile Status file descriptor to merge.
local function status_merge_file_into_section(section_map, section_name, file)
  local section = section_map[section_name]
  local existing_file = section.files_by_name[file.filename]
  if not existing_file then
    section.files[#section.files + 1] = file
    section.files_by_name[file.filename] = file
    return
  end

  if file.untracked then
    existing_file.untracked = true
    existing_file.git_status = "??"
  elseif section_name == "staged" and status_file_is_added(file) then
    existing_file.git_status = "A"
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

--- Constructs a semantic status move descriptor from visual or cursor selection entries.
---@param entries DiffReviewStatusEntry[] Selected status entries.
---@param target_section DiffReviewStatusStageSectionName Destination stage section (`"staged"` or `"unstaged"`).
---@return DiffReviewStatusMove move Captured semantic move descriptor.
function M.capture_move(entries, target_section)
  assert(target_section == "unstaged" or target_section == "staged", "status move target must be staged or unstaged")
  local selection_list = {} ---@type DiffReviewStatusMoveSelection[]
  local selected_key = {} ---@type table<string, boolean>
  for _, entry in ipairs(entries or {}) do
    local file = entry.file
    local source_section = file and file.section_name or nil
    if file and source_section ~= target_section and (source_section == "unstaged" or source_section == "staged") then
      local selection = nil ---@type DiffReviewStatusMoveSelection?
      if entry.kind == "file" then
        selection = {
          kind = "file",
          filename = file.filename,
          source_section = source_section,
        }
      elseif entry.kind == "hunk" and entry.hunk then
        selection = {
          kind = "hunk",
          filename = file.filename,
          source_section = source_section,
          hunk_identity = status_hunk_identity(entry.hunk),
        }
      end
      if selection then
        local key = table.concat({ selection.kind, selection.source_section, selection.filename, selection.hunk_identity or "" }, "\31")
        if not selected_key[key] then
          selection_list[#selection_list + 1] = selection
          selected_key[key] = true
        end
      end
    end
  end
  return { target_section = target_section, selection_list = selection_list }
end

--- Replays a semantic move across section models, generating an optimistic projection.
---@param sections DiffReviewStatusSection[] Source baseline sections.
---@param move DiffReviewStatusMove Semantic move descriptor.
---@param buf? integer Optional status buffer handle.
---@return DiffReviewStatusSection[] sections Projected status section array.
function M.apply_move(sections, move, buf)
  local source_sections = vim.deepcopy(sections or {})
  local mapped_section = status_section_map(source_sections)
  for _, selection in ipairs(move.selection_list or {}) do
    local source_section = mapped_section[selection.source_section]
    local source_file = source_section and source_section.files_by_name[selection.filename] or nil
    if selection.kind == "file" and source_file then
      local removed_file = status_remove_file_from_section(mapped_section, selection.source_section, selection.filename)
      if removed_file then
        local moved_file = status_copy_file_for_section(removed_file, move.target_section)
        status_merge_file_into_section(mapped_section, move.target_section, moved_file)
      end
    elseif selection.kind == "hunk" and source_file and selection.hunk_identity then
      local moved_hunk = nil ---@type DiffReviewHunk?
      for hunk_index = #source_file.hunks, 1, -1 do
        local candidate_hunk = source_file.hunks[hunk_index]
        if status_hunk_identity(candidate_hunk) == selection.hunk_identity then
          moved_hunk = table.remove(source_file.hunks, hunk_index)
          break
        end
      end
      if moved_hunk then
        source_file.added = math.max(0, source_file.added - (moved_hunk.added or 0))
        source_file.removed = math.max(0, source_file.removed - (moved_hunk.removed or 0))
        if #source_file.hunks == 0 then
          status_remove_file_from_section(mapped_section, selection.source_section, source_file.filename)
        end
        local target_file = status_ensure_file(mapped_section, move.target_section, source_file)
        moved_hunk.section_name = move.target_section
        moved_hunk.staged = move.target_section == "staged"
        moved_hunk.git_status = target_file.git_status or moved_hunk.git_status
        status_append_hunk_to_file(target_file, moved_hunk)
      end
    end
  end
  return status_order_section_map(mapped_section, source_sections, buf)
end

--- Normalizes a filesystem path string to use standard forward slashes.
---@param path string File path string.
---@return string path Normalized path string.
local function status_normalized_path(path)
  return vim.fs.normalize(path):gsub("\\", "/")
end

--- Builds a dual-indexed normalized path dictionary from an affected path set.
---@param affected_path_set DiffReviewAffectedPathSet Set of affected relative or absolute paths.
---@return DiffReviewAffectedPathSet normalized Normalized affected path set.
local function status_normalized_path_set(affected_path_set)
  local normalized_path_set = {} ---@type DiffReviewAffectedPathSet
  for path, affected in pairs(affected_path_set or {}) do
    if affected then
      normalized_path_set[path] = true
      normalized_path_set[status_normalized_path(path)] = true
    end
  end
  return normalized_path_set
end

--- Checks whether a file matches any entry in a normalized affected path set.
---@param file DiffReviewStatusFile Status file descriptor.
---@param normalized_path_set DiffReviewAffectedPathSet Normalized path set.
---@return boolean affected True if file path is present in the set.
local function status_file_path_is_affected(file, normalized_path_set)
  return normalized_path_set[file.filename] == true or normalized_path_set[status_normalized_path(file.filename)] == true
end

--- Merges authoritative files for selected paths while preserving every unrelated section entry.
---@param confirmed_sections DiffReviewStatusSection[] Existing confirmed baseline sections.
---@param snapshot_sections DiffReviewStatusSection[] Fresh authoritative snapshot sections.
---@param affected_path_set DiffReviewAffectedPathSet Set of affected paths to replace.
---@return DiffReviewStatusSection[] sections Updated status sections array.
function M.replace_paths(confirmed_sections, snapshot_sections, affected_path_set)
  local source_sections = vim.deepcopy(confirmed_sections or {})
  local mapped_section = status_section_map(source_sections)
  local normalized_path_set = status_normalized_path_set(affected_path_set)
  for _, section_name in ipairs({ "unstaged", "staged" }) do
    local section = mapped_section[section_name]
    for file_index = #section.files, 1, -1 do
      if status_file_path_is_affected(section.files[file_index], normalized_path_set) then
        table.remove(section.files, file_index)
      end
    end
    status_reindex_section(section)
  end
  for _, snapshot_section in ipairs(snapshot_sections or {}) do
    if snapshot_section.name == "unstaged" or snapshot_section.name == "staged" then
      for _, snapshot_file in ipairs(snapshot_section.files or {}) do
        if status_file_path_is_affected(snapshot_file, normalized_path_set) then
          status_merge_file_into_section(mapped_section, snapshot_section.name, vim.deepcopy(snapshot_file))
        end
      end
    end
  end
  return status_order_section_map(mapped_section, source_sections, nil)
end

--- Normalizes a relative file path string by replacing backslashes and removing leading `./`.
---@param path string File path string.
---@return string normalized Normalized relative path string.
local function status_normalized_relative_path(path)
  return (tostring(path or ""):gsub("\\", "/"):gsub("^%./", ""))
end

--- Moves matching unstaged files into the virtual Ignored section by repository-relative path.
---@param section_list DiffReviewStatusSection[] Array of status sections.
---@param ignored_path_set table<string, boolean> Set of relative paths to ignore.
---@return DiffReviewStatusSection[] sections Updated array of status sections.
function M.apply_ignored_paths(section_list, ignored_path_set)
  local source_sections = vim.deepcopy(section_list or {})
  local mapped_section = status_section_map(source_sections)
  mapped_section.ignored.files = {}
  mapped_section.ignored.files_by_name = {}
  local unstaged_section = mapped_section.unstaged
  for file_index = #unstaged_section.files, 1, -1 do
    local file = unstaged_section.files[file_index]
    local relpath = status_normalized_relative_path(file.relpath)
    if ignored_path_set[relpath] then
      table.remove(unstaged_section.files, file_index)
      local ignored_file = status_copy_file_for_section(file, "ignored")
      status_merge_file_into_section(mapped_section, "ignored", ignored_file)
    end
  end
  status_reindex_section(unstaged_section)
  return status_order_section_map(mapped_section, source_sections, nil)
end

--- Converts a path status file snapshot into a renderable status file descriptor.
---@param file_snapshot DiffReviewPathStatusFileSnapshot File snapshot record.
---@param section_name DiffReviewStatusStageSectionName Target stage section name.
---@param hunk_list DiffReviewHunk[] Array of diff hunks.
---@return DiffReviewStatusFile file Renderable status file descriptor.
local function status_file_from_snapshot(file_snapshot, section_name, hunk_list)
  local status_record = file_snapshot.status_record or {}
  local preview_provided = (section_name == "staged" and file_snapshot.staged_preview or file_snapshot.unstaged_preview) ~= nil
  local preview = section_name == "staged" and file_snapshot.staged_preview or file_snapshot.unstaged_preview
  preview = preview or {
    state = "loaded",
    source = "diff",
    added = 0,
    removed = 0,
    line_stats_complete = true,
    binary = false,
  }
  local git_status = section_name == "staged" and status_record.index_status or status_record.worktree_status
  local untracked = section_name == "unstaged" and status_record.untracked == true
  if untracked then git_status = "??" end
  local file = {
    filename = file_snapshot.abs_file,
    relpath = file_snapshot.path,
    original_relpath = status_record.original_path,
    path_change_kind = (status_record.kind == "renamed" or status_record.kind == "copied") and status_record.kind or nil,
    section_name = section_name,
    added = preview.added or 0,
    removed = preview.removed or 0,
    hunks = {},
    untracked = untracked,
    status = status_record.xy or "",
    git_status = git_status,
    preview_state = preview.state,
    preview_source = preview.source,
    preview_oid = preview.oid,
    preview_mode = preview.mode,
    preview_binary = preview.binary,
    line_stats_complete = preview.line_stats_complete,
  } ---@type DiffReviewStatusFile
  for _, source_hunk in ipairs(hunk_list or {}) do
    local hunk = vim.deepcopy(source_hunk)
    hunk.filename = file.filename
    hunk.section_name = section_name
    hunk.staged = section_name == "staged"
    hunk.git_status = git_status or hunk.git_status
    hunk.git_original_file = hunk.git_original_file or status_record.original_path
    file.hunks[#file.hunks + 1] = hunk
    if not preview_provided then
      file.added = file.added + (hunk.added or 0)
      file.removed = file.removed + (hunk.removed or 0)
    end
  end
  return file
end

--- Builds authoritative stage and unstage sections from a path status snapshot.
---@param snapshot DiffReviewPathStatusSnapshot Snapshot record from Git collector.
---@return DiffReviewStatusSection[] sections Populated stage and unstage sections.
function M.sections_from_snapshot(snapshot)
  local mapped_section = status_empty_section_map()
  for _, file_snapshot in ipairs(snapshot.file_list or {}) do
    local status_record = file_snapshot.status_record or {}
    local unstaged_hunk_list = file_snapshot.unstaged_hunk_list or {}
    local staged_hunk_list = file_snapshot.staged_hunk_list or {}
    if status_record.unstaged or status_record.untracked or #unstaged_hunk_list > 0 then
      local file = status_file_from_snapshot(file_snapshot, "unstaged", unstaged_hunk_list)
      status_merge_file_into_section(mapped_section, "unstaged", file)
    end
    if status_record.staged or #staged_hunk_list > 0 then
      local file = status_file_from_snapshot(file_snapshot, "staged", staged_hunk_list)
      status_merge_file_into_section(mapped_section, "staged", file)
    end
  end
  return status_order_section_map(mapped_section, {}, nil)
end

--- Converts a diff hunk into a normalized semantic hunk record for structural comparisons.
---@param hunk DiffReviewHunk Diff hunk descriptor.
---@return DiffReviewStatusSemanticHunk record Normalized semantic hunk record.
local function status_semantic_hunk(hunk)
  return {
    identity = status_hunk_identity(hunk),
    pos = hunk.pos or 0,
    diff = hunk.diff or "",
    staged = hunk.staged == true,
    context_text = hunk.context_text or hunk.context or "",
    git_status = hunk.git_status or "",
    git_original_file = hunk.git_original_file or "",
    added = hunk.added or 0,
    removed = hunk.removed or 0,
  }
end

--- Converts a status file into a normalized semantic file record for structural comparisons.
---@param file DiffReviewStatusFile Status file descriptor.
---@return DiffReviewStatusSemanticFile record Normalized semantic file record.
local function status_semantic_file(file)
  local semantic_hunk_list = {} ---@type DiffReviewStatusSemanticHunk[]
  for _, hunk in ipairs(file.hunks or {}) do
    semantic_hunk_list[#semantic_hunk_list + 1] = status_semantic_hunk(hunk)
  end
  table.sort(semantic_hunk_list, function(left_hunk, right_hunk)
    if left_hunk.pos ~= right_hunk.pos then return left_hunk.pos < right_hunk.pos end
    return left_hunk.identity < right_hunk.identity
  end)
  return {
    filename = file.filename,
    relpath = file.relpath or "",
    original_relpath = file.original_relpath or "",
    path_change_kind = file.path_change_kind or "",
    added = file.added or 0,
    removed = file.removed or 0,
    untracked = file.untracked == true,
    git_status = file.git_status or "",
    hunk_list = semantic_hunk_list,
  }
end

--- Builds a normalized semantic section map for verifying data equivalence between models.
---@param sections DiffReviewStatusSection[] Array of status sections.
---@return DiffReviewStatusSemanticSectionMap map Dictionary of semantic file records by section.
local function status_semantic_section_map(sections)
  local semantic_section_map = { unstaged = {}, staged = {}, ignored = {} } ---@type DiffReviewStatusSemanticSectionMap
  for _, section in ipairs(sections or {}) do
    local semantic_file_list = semantic_section_map[section.name]
    if semantic_file_list then
      for _, file in ipairs(section.files or {}) do
        semantic_file_list[#semantic_file_list + 1] = status_semantic_file(file)
      end
      table.sort(semantic_file_list, function(left_file, right_file)
        return left_file.filename < right_file.filename
      end)
    end
  end
  return semantic_section_map
end

--- Validates data equivalence between two status section models while ignoring presentation state.
---@param left_sections DiffReviewStatusSection[] First section model array.
---@param right_sections DiffReviewStatusSection[] Second section model array.
---@return boolean equivalent True if both models hold identical file and hunk data.
function M.equivalent(left_sections, right_sections)
  return vim.deep_equal(status_semantic_section_map(left_sections), status_semantic_section_map(right_sections))
end

--- Helper capturing and applying an optimistic status move across section models.
---@param sections DiffReviewStatusSection[]? Baseline status sections.
---@param entries DiffReviewStatusEntry[] Selected entries being moved.
---@param target_section DiffReviewStatusStageSectionName Destination stage section (`"staged"` or `"unstaged"`).
---@return DiffReviewStatusSection[]? sections Projected status sections, or nil.
local function status_apply_optimistic_move(sections, entries, target_section)
  if not sections then return nil end
  return M.apply_move(sections, M.capture_move(entries, target_section), session.status and session.status.buf or nil)
end

--- Asynchronously coordinates Git status snapshot collection, head information, and section building.
---@param cwd string Git repository root path.
---@param cb fun(result: DiffReviewStatusLoadResult) Callback receiving success result or failure error.
local function status_load_async(cwd, cb)
  local head_lines = nil
  local head_values = nil
  local sections = nil
  local snapshot = nil ---@type DiffReviewPathStatusSnapshot?
  local unmerged_section = nil
  local unmerged_loaded = false
  local recent_commits_section = nil
  local recent_commits_loaded = false
  local completed = false

  ---@param result DiffReviewStatusLoadResult
  local function finish(result)
    if completed then return end
    completed = true
    cb(result)
  end

  local function maybe_done()
    if completed then return end
    if not (head_lines and sections and unmerged_loaded and recent_commits_loaded) then return end
    local ordered_sections = {}
    vim.list_extend(ordered_sections, sections)
    if unmerged_section then ordered_sections[#ordered_sections + 1] = unmerged_section end
    if recent_commits_section then ordered_sections[#ordered_sections + 1] = recent_commits_section end
    finish({ head_lines = head_lines, head_values = head_values or {}, sections = ordered_sections, snapshot = assert(snapshot) })
  end

  status_head._status_head_lines_async(cwd, function(lines, values)
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
  git_data._collect_status_snapshot_async(cwd, function(collected_snapshot, collection_error)
    if collection_error or not collected_snapshot then
      finish({
        error = collection_error or { kind = "parse", message = "Git status collector returned no result" },
      })
      return
    end
    snapshot = collected_snapshot
    sections = M.sections_from_snapshot(collected_snapshot)
    maybe_done()
  end)
end

-- Expose the bare-local section builders/movers that init and section_builder call by name.
M._status_load_async = status_load_async
M._status_pr_sections = status_pr_sections
M._status_apply_optimistic_move = status_apply_optimistic_move
M._status_files_from_diff_provider = status_files_from_diff_provider
M._status_commit_files_from_diff = status_commit_files_from_diff

return M
