--- Owns the status section model: assembling sections from git items / diffs / PR data / commit
--- logs, and the section-map data structure (ordered sections keyed by name, with file get-or-insert,
--- hunk append, optimistic stage/unstage moves, and reindexing).
---
--- Reads the diff parser, git collector, section order/index, fold state, key builders, and the
--- pr_overview/section_builder modules through the init module via dr().

local git_backend = require("diff_review.git_backend")

--- Resolve the init module lazily so section assembly can reach the shared parse/collect/order/fold
--- seams without a load-time circular require.
local function dr()
  return require("diff_review")
end

local M = {}

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
    ranks[file] = dr()._status_walkthrough_file_rank(buf, file) or math.huge
  end
  table.sort(section.files, function(left_file, right_file)
    local left_rank = ranks[left_file] or math.huge
    local right_rank = ranks[right_file] or math.huge
    if left_rank ~= right_rank then return left_rank < right_rank end
    return dr()._status_file_path_sort_key(left_file) < dr()._status_file_path_sort_key(right_file)
  end)
end

---@param buf integer?
---@param sections DiffReviewStatusSection[]
function M._status_sort_sections_for_render(buf, sections)
  for _, section in ipairs(sections or {}) do
    dr()._status_sort_section_files(buf, section)
  end
end

---@param collected_items table[]
---@return DiffReviewStatusSection[]
local function status_sections_from_items(collected_items)
  local sections = {} ---@type table<string, DiffReviewStatusSection>
  for _, section_config in ipairs(dr()._status_section_order) do
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
  for _, section_config in ipairs(dr()._status_section_order) do
    local section = sections[section_config.name]
    dr()._status_sort_section_files(dr()._status and dr()._status.buf or nil, section)
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
  local diff_file_statuses = dr()._diff_file_statuses(diff_text or "")
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

  for _, parsed_hunk in ipairs(dr()._parse_diff(diff_text or "", false)) do
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
    section_name = dr()._status_keys.commit_key(commit.oid),
    default_status = "modified",
  }, diff_text)
end

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
  local change_sections, files = dr()._section_builder.sections_from_diff(cwd, {
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
    local key = dr()._pr_overview.comment_identity_key(comment)
    if key then local_comment_keys[key] = true end
  end
  for _, comment in ipairs(comments and comments.code_comments or {}) do
    local key = dr()._pr_overview.comment_identity_key(comment)
    if not (key and local_comment_keys[key]) then code_comments[#code_comments + 1] = comment end
  end
  vim.list_extend(code_comments, local_comments or {})
  dr()._section_builder.attach_comments(cwd, files, code_comments, { field = "pr_comments" })
  local sections = {}
  local reviews_section = dr()._pr_overview.reviews_section(comments)
  if reviews_section then sections[#sections + 1] = reviews_section end
  local issue_comments_section = dr()._pr_overview.issue_comments_section(comments, local_issue_comments)
  if issue_comments_section then sections[#sections + 1] = issue_comments_section end
  vim.list_extend(sections, change_sections)
  local commits_section = dr()._pr_overview.commits_section(pr)
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
      local cache = dr()._status and dr()._status.commit_file_cache and dr()._status.commit_file_cache[oid] or nil
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
  for _, section_config in ipairs(dr()._status_section_order) do
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
  for _, section_config in ipairs(dr()._status_section_order) do
    local section = section_map[section_config.name]
    dr()._status_sort_section_files(dr()._status and dr()._status.buf or nil, section)
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

  dr()._status_head_lines_async(cwd, function(lines, values)
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
  dr()._collect_items_from_git(cwd, function(items)
    sections = status_sections_from_items(items or {})
    maybe_done()
  end, { skip_pre_render = true, skip_ts_context = true })
end

-- Expose the bare-local section builders/movers that init and section_builder call by name.
M._status_load_async = status_load_async
M._status_pr_sections = status_pr_sections
M._status_apply_optimistic_move = status_apply_optimistic_move
M._status_files_from_diff_provider = status_files_from_diff_provider
M._status_commit_files_from_diff = status_commit_files_from_diff

return M
