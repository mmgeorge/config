local comment_rows = require("github.comment_rows")
local datetime_ok, datetime = pcall(require, "forge.integrations.datetime")
if not datetime_ok then datetime = nil end
local ui_ok, ui = pcall(require, "forge.infra.ui")
if not ui_ok then ui = nil end

local M = {}
local decoration_namespace = vim.api.nvim_create_namespace("github.issue_preview.decorations")
local markdown_heading_hl = "GithubIssueMarkdownHeading"
local markdown_language_registered = false
local markdown_failed = false

---@param value any
---@return string
local function relative_datetime(value)
  if type(datetime) == "table" and type(datetime.relative) == "function" then return datetime.relative(value) end
  return tostring(value or "")
end

---@param value any
---@return string
local function reviewer_token(value)
  return "@" .. tostring(value or ""):gsub("^@", "")
end

---@param value string
---@param width integer
---@return string
local function truncate_preview(value, width)
  value = tostring(value or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(value) <= width then return value end
  local reserved = width > 4 and 4 or 0
  local result = ""
  for index = 0, vim.fn.strchars(value) - 1 do
    local character = vim.fn.strcharpart(value, index, 1)
    if vim.fn.strdisplaywidth(result .. character) > width - reserved then break end
    result = result .. character
  end
  return reserved > 0 and result .. " ..." or result
end

---@param prefix string
---@return integer
local function comment_preview_width(prefix)
  return math.max(0, vim.api.nvim_win_get_width(0) - vim.fn.strdisplaywidth(prefix) - 4)
end

---@param value string?
---@return string[]
local function split_body(value)
  value = tostring(value or "")
  return value == "" and { "" } or vim.split(value, "\n", { plain = true })
end

---@param lines string[]
---@param label string
---@param value string?
local function add_meta(lines, label, value)
  if value and value ~= "" then lines[#lines + 1] = string.format("%-14s%s", label .. ":", value) end
end

---@param value string?
---@return string
local function state_text(value)
  value = vim.trim(tostring(value or ""))
  if value == "" then return "" end
  value = value:gsub("_", " "):lower()
  return (value:gsub("(%S)(%S*)", function(first, rest) return first:upper() .. rest end))
end

---@param value string?
---@return string?
local function state_highlight(value)
  value = tostring(value or ""):gsub("_", " "):lower()
  if value == "open" then return "ForgeStatusOpen" end
  if value == "closed" then return "ForgeStatusClosed" end
  return nil
end

---@param value any
---@return integer?
local function parse_activity_time(value)
  value = tostring(value or "")
  if value == "" then return nil end
  if type(datetime) == "table" and type(datetime.parse) == "function" then
    local ok, epoch = pcall(datetime.parse, value)
    if ok and epoch then return epoch end
  end
  local ok, epoch = pcall(vim.fn.strptime, "%Y-%m-%dT%H:%M:%SZ", value)
  return ok and tonumber(epoch) and tonumber(epoch) > 0 and tonumber(epoch) or nil
end

---@param item GithubGhDetail
---@return string
local function activity_text(item)
  local latest, latest_epoch = "", nil
  local function consider(value)
    value = tostring(value or "")
    if value == "" then return end
    local epoch = parse_activity_time(value)
    if epoch and (not latest_epoch or epoch > latest_epoch) then latest, latest_epoch = value, epoch
    elseif not epoch and (latest == "" or value > latest) then latest = value end
  end
  consider(item.updated_at or item.updatedAt)
  consider(item.created_at or item.createdAt)
  for _, comment in ipairs(type(item.comments) == "table" and item.comments or {}) do
    consider(comment.updated_at or comment.updatedAt)
    consider(comment.created_at or comment.createdAt)
  end
  return latest ~= "" and relative_datetime(latest) or ""
end

---@param values any
---@param key string
---@return string[]
local function names(values, key)
  local result = {}
  for _, value in ipairs(type(values) == "table" and values or {}) do
    local name = type(value) == "table" and (value[key] or value.name or value.login) or value
    if type(name) == "string" and name ~= "" then result[#result + 1] = name end
  end
  return result
end

---@param values any
---@return string
local function assignee_text(values)
  local result, seen = {}, {}
  for _, value in ipairs(type(values) == "table" and values or {}) do
    local name = type(value) == "table" and (value.login or value.name or value.slug) or value
    name = tostring(name or ""):gsub("^@", "")
    if name ~= "" and not seen[name:lower()] then
      seen[name:lower()] = true
      result[#result + 1] = reviewer_token(name)
    end
  end
  return table.concat(result, " ")
end

local function ensure_markdown_language_registered()
  if markdown_language_registered then return end
  pcall(vim.treesitter.language.register, "markdown", "ForgeGithubIssue")
  markdown_language_registered = true
end

---@param buf integer
---@param body_start integer?
---@param body_end integer?
local function prune_markdown_range(buf, body_start, body_end)
  local ok, markdown_ui = pcall(require, "render-markdown.core.ui")
  local namespace = ok and type(markdown_ui) == "table" and markdown_ui.ns or vim.api.nvim_get_namespaces()["render-markdown.nvim"]
  if not namespace or body_start == nil or body_end == nil then return end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, namespace, 0, -1, {})) do
    if mark[2] < body_start or mark[2] >= body_end then pcall(vim.api.nvim_buf_del_extmark, buf, namespace, mark[1]) end
  end
end

---@param buf integer
---@param win integer?
---@param body_start integer?
---@param body_end integer?
local function render_markdown(buf, win, body_start, body_end)
  if body_start == nil or body_end == nil then return end
  local ok, renderer = pcall(require, "render-markdown")
  if not ok or type(renderer) ~= "table" or type(renderer.render) ~= "function" then return end
  if not (win and vim.api.nvim_win_is_valid(win)) then
    local found = vim.fn.bufwinid(buf)
    win = found ~= -1 and found or nil
  end
  if not win then return end
  ensure_markdown_language_registered()
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  local rendered, failure = pcall(renderer.render, {
    buf = buf, win = win,
    config = { enabled = true, render_modes = true, debounce = 0, completions = { lsp = { enabled = false } }, sign = { enabled = false },
      win_options = { conceallevel = { default = conceallevel, rendered = conceallevel }, concealcursor = { default = concealcursor, rendered = concealcursor } },
      on = { render = function() prune_markdown_range(buf, body_start, body_end) end } },
  })
  if rendered then prune_markdown_range(buf, body_start, body_end)
  elseif not markdown_failed then
    markdown_failed = true
    vim.notify("Issue preview markdown rendering failed: " .. tostring(failure), vim.log.levels.WARN, { title = "GitHub" })
  end
end

local function ensure_highlights()
  vim.api.nvim_set_hl(0, markdown_heading_hl, { fg = "#ffffff", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusHeader", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusOpen", { fg = "#50fa7b", bold = true })
  vim.api.nvim_set_hl(0, "ForgeStatusClosed", { fg = "#6b7280", bold = true })
end

---@class GithubIssuePreview
---@field lines string[]
---@field entries table<integer, table>
---@field highlights table[]
---@field line_highlights table[]
---@field opening_heading_row integer
---@field body_start integer
---@field body_end integer
---@field comments_heading_row integer

---@param item GithubGhDetail
---@param folds table<string, boolean>
---@return GithubIssuePreview
local function render_item(item, folds)
  local comments = {}
  for _, comment in ipairs(type(item.comments) == "table" and item.comments or {}) do
    comments[#comments + 1] = { user = comment.author or comment.user or "unknown", body = comment.body or "", created_at = comment.created_at or comment.createdAt or "", updated_at = comment.updated_at or comment.updatedAt or "", remote_id = comment.id or comment.databaseId or comment.url, url = comment.url }
  end
  local options = { comment_icon = ui and ui.comment_icon or "", entry_id = comment_rows.entry_id,
    line_hl_group = "ForgeReviewComment", body_hl_group = "ForgeReviewComment", date_hl_group = "ForgeStatusDate",
    relative_date = relative_datetime, preview_width = comment_preview_width, truncate_preview = truncate_preview }
  local alignment = comment_rows.alignment(comments, options)
  local lines, entries, highlights, line_highlights = {}, {}, {}, {}
  lines[#lines + 1] = "Title:  " .. tostring(item.title or "")
  add_meta(lines, "Author", item.author)
  local normalized_state = state_text(item.state)
  if normalized_state ~= "" then
    local row = #lines + 1
    add_meta(lines, "State", normalized_state)
    local group = state_highlight(normalized_state)
    if group then highlights[#highlights + 1] = { line = row, start_col = 14, end_col = 14 + #normalized_state, hl_group = group } end
  end
  local activity = activity_text(item)
  if activity ~= "" then
    local row = #lines + 1
    add_meta(lines, "Activity", activity)
    highlights[#highlights + 1] = { line = row, start_col = 14, end_col = 14 + #activity, hl_group = "ForgeStatusDate" }
  end
  if item.kind == "pr" then add_meta(lines, "Head", item.head_ref_name or "") add_meta(lines, "Base", item.base_ref_name or "") end
  local milestone_title = type(item.milestone) == "table" and item.milestone.title or item.milestone
  local milestone = vim.trim(tostring(milestone_title or ""))
  if milestone ~= "" and ui and ui.milestone_icon then milestone = ui.milestone_icon .. " " .. milestone end
  add_meta(lines, "Release", milestone)
  local projects = names(item.projects, "name")
  if #projects > 0 then add_meta(lines, "Projects", table.concat(projects, ", ")) end
  add_meta(lines, "Subscription", item.subscription)
  local labels = names(item.labels, "name")
  if #labels > 0 then add_meta(lines, "Labels", table.concat(labels, ", ")) end
  lines[#lines + 1] = string.format("%-14s%s", "Assignees:", assignee_text(item.assignees))
  lines[#lines + 1] = ""
  local opening_heading_row = #lines + 1
  lines[#lines + 1] = "Description:"
  local body_start = #lines + 1
  vim.list_extend(lines, split_body(item.body))
  local body_end = #lines + 1
  lines[#lines + 1] = ""
  local comments_heading_row = #lines + 1
  entries[comments_heading_row] = { id = "issue:comments", kind = "section", section = { name = "issue:comments", title = "Comments", default_folded = false, issue_comments = comments } }
  local heading = ("Comments (%d):"):format(#comments)
  lines[#lines + 1] = heading
  if not folds["issue:comments"] then
    options.alignment = alignment
    options.is_folded = function(id, default) return folds[id] == nil and default or folds[id] end
    for _, row in ipairs(comment_rows.rows(comments, options)) do
      lines[#lines + 1] = row.text
      local line = #lines
      if row.entry then entries[line] = row.entry end
      if row.line_hl_group then line_highlights[#line_highlights + 1] = { line = line, hl_group = row.line_hl_group } end
      for _, highlight in ipairs(row.highlights or {}) do highlights[#highlights + 1] = { line = line, start_col = highlight.start_col, end_col = highlight.end_col, hl_group = highlight.hl_group } end
    end
  end
  return { lines = lines, entries = entries, highlights = highlights, line_highlights = line_highlights, opening_heading_row = opening_heading_row, body_start = body_start, body_end = body_end, comments_heading_row = comments_heading_row }
end

---@param item GithubGhDetail
---@param opts? {folds?: table<string, boolean>}
---@return GithubIssuePreview
function M.render(item, opts)
  return render_item(item, opts and opts.folds or {})
end

---@param buf integer
---@param rendered GithubIssuePreview
function M.present(buf, rendered, opts)
  opts = opts or {}
  if opts.set_lines ~= false then
    local modifiable = vim.bo[buf].modifiable
    if not modifiable then vim.bo[buf].modifiable = true end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, rendered.lines)
    if not modifiable then vim.bo[buf].modifiable = false end
  end
  vim.api.nvim_buf_clear_namespace(buf, decoration_namespace, 0, -1)
  ensure_highlights()
  for _, highlight in ipairs(rendered.line_highlights) do vim.api.nvim_buf_set_extmark(buf, decoration_namespace, highlight.line - 1, 0, { line_hl_group = highlight.hl_group, priority = 80 }) end
  for _, highlight in ipairs(rendered.highlights) do vim.api.nvim_buf_set_extmark(buf, decoration_namespace, highlight.line - 1, highlight.start_col, { end_col = highlight.end_col, hl_group = highlight.hl_group, priority = 90 }) end
  for _, row in ipairs({ rendered.opening_heading_row, rendered.comments_heading_row }) do vim.api.nvim_buf_set_extmark(buf, decoration_namespace, row - 1, 0, { end_col = #(rendered.lines[row] or ""), hl_group = "ForgeStatusHeader", priority = 200 }) end
  for row = rendered.body_start, rendered.body_end - 1 do
    if (rendered.lines[row] or ""):match("^%s*#+%s+") then vim.api.nvim_buf_set_extmark(buf, decoration_namespace, row - 1, 0, { end_col = #(rendered.lines[row] or ""), hl_group = markdown_heading_hl, priority = 200 }) end
  end
  if opts.markdown then render_markdown(buf, opts.win, rendered.body_start - 1, rendered.body_end - 1) end
end

return M
