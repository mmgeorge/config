local M = {}

---@class GithubCommentRowsAlignment
---@field author_width integer
---@field date_width integer

---@class GithubCommentRowsOpts
---@field comment_icon? string
---@field relative_date? fun(value: any): string
---@field author_text? fun(comment: table): string
---@field date_text? fun(comment: table): string
---@field preview_text? fun(body: string): string
---@field preview_width? fun(prefix: string): integer
---@field truncate_preview? fun(text: string, width: integer): string
---@field entry_id? fun(comment: table, index: integer): string
---@field start_index? integer
---@field is_folded? fun(entry_id: string, default: boolean, entry: table): boolean
---@field body_lines? fun(body: string): string[]
---@field kind? string
---@field line_hl_group? string
---@field body_hl_group? string
---@field date_hl_group? string
---@field alignment? GithubCommentRowsAlignment

---@param text any
---@param width integer?
---@return string
function M.pad_right(text, width)
  text = tostring(text or "")
  local padding = (width or 0) - vim.fn.strdisplaywidth(text)
  if padding <= 0 then return text end
  return text .. string.rep(" ", padding)
end

---@param value any
---@return string
function M.body_text(value)
  return tostring(value or ""):gsub("\r\n", "\n"):gsub("\r", "\n")
end

---@param body string?
---@return string
function M.preview_text(body)
  local parts = {}
  local in_fence = false
  for _, line in ipairs(vim.split(M.body_text(body), "\n", { plain = true })) do
    local trimmed = vim.trim(line)
    if trimmed:match("^```") then
      in_fence = not in_fence
      goto continue
    end
    if in_fence then goto continue end
    if trimmed ~= "" then parts[#parts + 1] = trimmed end
    ::continue::
  end
  return table.concat(parts, " "):gsub("%s+", " ")
end

---@param body string?
---@return string[]
function M.body_lines(body)
  local lines = vim.split(M.body_text(body), "\n", { plain = true })
  if #lines == 0 then lines = { "" } end
  return lines
end

---@param comment table?
---@param opts? GithubCommentRowsOpts
---@return string
function M.author(comment, opts)
  opts = opts or {}
  if type(opts.author_text) == "function" then return opts.author_text(comment or {}) end
  return tostring(comment and comment.user or "unknown")
end

---@param value any
---@param opts? GithubCommentRowsOpts
---@return string
function M.datetime(value, opts)
  opts = opts or {}
  if type(opts.relative_date) == "function" then return opts.relative_date(value) end
  return tostring(value or "")
end

---@param comment table?
---@param opts? GithubCommentRowsOpts
---@return string
function M.date(comment, opts)
  comment = comment or {}
  if opts and type(opts.date_text) == "function" then return opts.date_text(comment) end
  return M.datetime(comment.updated_at or comment.created_at, opts)
end

---@param comments table[]?
---@param opts? GithubCommentRowsOpts
---@return GithubCommentRowsAlignment
function M.alignment(comments, opts)
  local alignment = { author_width = 0, date_width = 0 }
  for _, comment in ipairs(comments or {}) do
    alignment.author_width = math.max(alignment.author_width, vim.fn.strdisplaywidth(M.author(comment, opts)))
    alignment.date_width = math.max(alignment.date_width, vim.fn.strdisplaywidth(M.date(comment, opts)))
  end
  return alignment
end

---@param comment table
---@param alignment? GithubCommentRowsAlignment
---@param has_preview? boolean
---@param opts? GithubCommentRowsOpts
---@return string
function M.prefix(comment, alignment, has_preview, opts)
  opts = opts or {}
  alignment = alignment or {}
  local date_text = M.date(comment, opts)
  local date_width = alignment.date_width or vim.fn.strdisplaywidth(date_text)
  local icon = tostring(opts.comment_icon or "")
  local prefix = icon ~= "" and (icon .. " ") or ""
  prefix = prefix .. M.pad_right(M.author(comment, opts), alignment.author_width)
  if date_text ~= "" or date_width > 0 then
    prefix = prefix .. " " .. M.pad_right(date_text, has_preview and date_width or 0)
  end
  if has_preview then return prefix end
  return prefix:gsub("%s+$", "")
end

---@param comment table
---@param expanded? boolean
---@param alignment? GithubCommentRowsAlignment
---@param opts? GithubCommentRowsOpts
---@return string
function M.line(comment, expanded, alignment, opts)
  opts = opts or {}
  local prefix = M.prefix(comment, alignment, false, opts)
  if not expanded then
    local preview = type(opts.preview_text) == "function" and opts.preview_text(comment.body or "") or M.preview_text(comment.body or "")
    if type(opts.preview_width) == "function" and type(opts.truncate_preview) == "function" then
      local preview_prefix = M.prefix(comment, alignment, preview ~= "", opts) .. "  "
      preview = opts.truncate_preview(preview, opts.preview_width(preview_prefix))
    end
    if preview ~= "" then prefix = M.prefix(comment, alignment, true, opts) .. "  " .. preview end
  end
  return prefix
end

---@param comment table?
---@param index integer
---@param opts? GithubCommentRowsOpts
---@return string
function M.entry_id(comment, index, opts)
  if opts and type(opts.entry_id) == "function" then return opts.entry_id(comment or {}, index) end
  local value = comment and (comment.remote_node_id or comment.remote_id or comment.id or comment.url or comment.local_id) or nil
  if value and value ~= "" then return "issue-comment:" .. tostring(value) end
  return "issue-comment:" .. tostring(index)
end

---@param comments table[]?
---@param opts? GithubCommentRowsOpts
---@return table[]
function M.rows(comments, opts)
  opts = opts or {}
  comments = comments or {}
  local alignment = opts.alignment or M.alignment(comments, opts)
  local rows = {}
  for offset, comment in ipairs(comments) do
    local index = (opts.start_index or 1) + offset - 1
    local entry_id = M.entry_id(comment, index, opts)
    local entry = {
      id = entry_id,
      kind = opts.kind or "pr_comment",
      pr_comment = comment,
    }
    local folded = true
    if opts.is_folded then folded = opts.is_folded(entry_id, true, entry) end
    local line = M.line(comment, not folded, alignment, opts)
    local highlights = {
      { start_col = 0, end_col = #line, hl_group = opts.line_hl_group or "DiffReviewReviewComment" },
    }
    local date_text = M.date(comment, opts)
    local date_start = date_text ~= "" and line:find(date_text, 1, true) or nil
    if date_start and opts.date_hl_group then
      highlights[#highlights + 1] = {
        start_col = date_start - 1,
        end_col = date_start - 1 + #date_text,
        hl_group = opts.date_hl_group,
      }
    end
    rows[#rows + 1] = {
      text = line,
      entry = entry,
      line_hl_group = opts.line_hl_group or "DiffReviewReviewComment",
      highlights = highlights,
    }
    if not folded then
      local body_lines = type(opts.body_lines) == "function" and opts.body_lines(comment.body or "") or M.body_lines(comment.body or "")
      for body_index, body_line in ipairs(body_lines) do
        local body_entry = {
          id = entry_id,
          kind = opts.kind or "pr_comment",
          pr_comment = comment,
          pr_comment_body = true,
          pr_comment_body_index = body_index,
        }
        local body_highlights = {}
        if body_line ~= "" then
          body_highlights[#body_highlights + 1] = {
            start_col = 0,
            end_col = #body_line,
            hl_group = opts.body_hl_group or opts.line_hl_group or "DiffReviewReviewComment",
          }
        end
        rows[#rows + 1] = {
          text = body_line,
          entry = body_entry,
          highlights = body_highlights,
        }
      end
    end
  end
  return rows
end

return M
