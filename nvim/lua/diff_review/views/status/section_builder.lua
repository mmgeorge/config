-- review edge kept lazy to avoid a load-time cycle.
local function review_mod() return require("diff_review.views.pr.review") end
--- Builds status sections and files from diff text and attaches review comments to them,
--- turning a raw diff plus optional PR comment data into the section/file/hunk tree the
--- render core walks.
---
--- Holds no state: every function takes its inputs explicitly, so status, PR, review, and
--- branch-diff views share one section-assembly seam.
---@class DiffReviewSectionBuilderModule
local M = {}

---@param files DiffReviewStatusFile[]
---@return table<string, DiffReviewStatusFile>
function M.files_by_name(files)
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
function M.files_from_diff(cwd, spec, diff_text)
  return require("diff_review.views.status.section_map")._status_files_from_diff_provider(cwd, {
    section_name = spec.section_name,
    default_status = spec.default_status or "",
    files = spec.files,
  }, diff_text)
end

---@param title string
---@param files DiffReviewStatusFile[]
---@param opts? { name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@return DiffReviewStatusSection?
function M.section_from_files(title, files, opts)
  opts = opts or {}
  files = files or {}
  if #files == 0 and not opts.keep_empty then return nil end
  return {
    name = opts.name or title,
    title = title,
    default_folded = opts.default_folded == true,
    files = files,
    files_by_name = M.files_by_name(files),
    file_key_prefix = opts.file_key_prefix,
    file_entry_kind = opts.file_entry_kind,
    hunk_entry_kind = opts.hunk_entry_kind,
  }
end

---@param title string
---@param files DiffReviewStatusFile[]
---@param opts? { name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@return DiffReviewStatusSection[]
function M.sections_from_files(title, files, opts)
  local section = M.section_from_files(title, files, opts)
  return section and { section } or {}
end

---@param cwd string
---@param spec { title: string, section_name: string, default_status?: string, files?: DiffReviewGhPRFile[], name?: string, default_folded?: boolean, file_key_prefix?: string, file_entry_kind?: "file"|"commit_file"|"pr_file"|"pr_review_file", hunk_entry_kind?: "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk", keep_empty?: boolean }
---@param diff_text? string
---@return DiffReviewStatusSection[]
---@return DiffReviewStatusFile[]
function M.sections_from_diff(cwd, spec, diff_text)
  local files = M.files_from_diff(cwd, spec, diff_text)
  return M.sections_from_files(spec.title, files, spec), files
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param section_name string
---@return DiffReviewStatusFile
function M.file_with_hunks(file, hunks, section_name)
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
function M.comment_side(comment)
  return tostring(comment and comment.side or "RIGHT") == "LEFT" and "LEFT" or "RIGHT"
end

---@param cwd string
---@param comments table[]
---@return table<string, table[]>
function M.comments_by_path(cwd, comments)
  local by_path = {}
  for _, comment in ipairs(type(comments) == "table" and comments or {}) do
    local path = tostring(comment.path or "")
    if path ~= "" then
      comment.abs_file = comment.abs_file or vim.fs.normalize(vim.fs.joinpath(cwd, path))
      comment.side = M.comment_side(comment)
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
function M.attach_comments(cwd, files, comments, opts)
  opts = opts or {}
  local field = opts.field or "comments"
  local by_path = M.comments_by_path(cwd, comments)
  for _, file in ipairs(files or {}) do
    file[field] = by_path[file.relpath] or by_path[file.filename] or {}
  end
  return by_path
end

---@param file string?
---@param side string?
---@param line integer|string?
---@return string?
function M.comment_anchor_key(file, side, line)
  local line_number = tonumber(line)
  if not (file and file ~= "" and line_number) then return nil end
  local normalized_file = vim.fs.normalize(tostring(file))
  return ("%s\0%s\0%d"):format(normalized_file, tostring(side or "RIGHT"), math.floor(line_number))
end

---@param comments table[]
---@return table<string, { comment: table, index: integer }[]>
function M.comment_anchor_index(comments)
  local by_anchor = {}
  for index, comment in ipairs(type(comments) == "table" and comments or {}) do
    local key = M.comment_anchor_key(comment.abs_file or comment.path, M.comment_side(comment), comment.line)
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
function M.comment_anchor_index_from_sections(sections, opts)
  opts = opts or {}
  local field = opts.field or "comments"
  local by_anchor = {}
  for _, section in ipairs(sections or {}) do
    for _, file in ipairs(section.files or {}) do
      for index, comment in ipairs(type(file[field]) == "table" and file[field] or {}) do
        local key = M.comment_anchor_key(comment.abs_file or file.filename, M.comment_side(comment), comment.line)
        if key then
          by_anchor[key] = by_anchor[key] or {}
          by_anchor[key][#by_anchor[key] + 1] = { comment = comment, index = index }
        end
      end
    end
  end
  return by_anchor
end

--- Emit any draft/published comments anchored on `diff_line` as review lines below it,
--- routing through the review view's comment renderer.
---@param state table
---@param diff_line table
---@param indent integer
---@param opts { index_field: string, count_field?: string, skip?: fun(comment: table): boolean }
function M.emit_anchored_comments(state, diff_line, indent, opts)
  local review = review_mod()
  local key = M.comment_anchor_key(diff_line.file, review.side_of(diff_line), diff_line.line)
  local comments = key and state[opts.index_field] and state[opts.index_field][key] or nil
  for _, item in ipairs(comments or {}) do
    if item.comment
      and item.comment.local_state ~= "deleted"
      and not (opts.skip and opts.skip(item.comment)) then
      if opts.count_field then state[opts.count_field] = (state[opts.count_field] or 0) + 1 end
      review.emit_comment(item.comment, item.index, indent)
    end
  end
end

return M
