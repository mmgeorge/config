local gh = require("github.gh")
local github_comment_rows = require("github.comment_rows")
local diff_review_ok, diff_review = pcall(require, "diff_review")
if not diff_review_ok then diff_review = nil end

local namespace = vim.api.nvim_create_namespace("github.issue_view")
local decoration_namespace = vim.api.nvim_create_namespace("github.issue_view.decorations")
local markdown_heading_hl = "GithubIssueMarkdownHeading"

---@class GithubIssueViewState
---@field buf integer?
---@field win integer?
---@field cwd string
---@field kind "issue"|"pr"
---@field number integer
---@field repo string?
---@field item GithubGhDetail?
---@field loading boolean
---@field refresh_request_id integer
---@field title_mark integer?
---@field body_start_mark integer?
---@field body_end_mark integer?
---@field assignees_mark integer?
---@field title_marker_id integer?
---@field body_marker_id integer?
---@field assignees_marker_id integer?
---@field saved_title string
---@field saved_body string
---@field saved_assignees string[]
---@field markdown_failed boolean
---@field markdown_language_registered boolean

---@type GithubIssueViewState
local state = {
  cwd = "",
  kind = "issue",
  number = 0,
  loading = false,
  refresh_request_id = 0,
  saved_title = "",
  saved_body = "",
  saved_assignees = {},
  markdown_failed = false,
  folds = {},
  entries = {},
}

local M = {}

local command_specs = {
  { id = "toggle", key = "<Tab>", label = "toggle", desc = "Toggle fold", pinned = true },
  { id = "browse", key = "b", label = "browse", desc = "Browse issue", pinned = true },
  { id = "refresh", key = "R", label = "refresh", desc = "Refresh issue", pinned = true },
  { id = "sync", key = "<C-s>", label = "sync", desc = "Sync issue edits", pinned = true },
  { id = "close", key = "q", label = "close", desc = "Close issue", pinned = true },
  { id = "help", key = "?", label = "help", desc = "Show commands", pinned = true },
}

---@param cwd string?
---@param repo string?
local function load_repo_metadata(cwd, repo)
  if not (repo and repo ~= "") then return end
  require("github.repo_cache").ensure_metadata(cwd, repo, function(done)
    gh.repo_contributors_async(cwd, repo, done)
  end, { remember_cwd = false })
  local ok, issue_index = pcall(require, "github.issue_index")
  if ok then issue_index.ensure_repo(cwd, repo, { manual = false }) end
end

---@param buf integer
---@param repo? string
local function enable_user_completion(buf, repo)
  require("github.repo_cache").enable_user_completion(buf, repo)
end

---@param value string?
---@return string[]
local function split_body(value)
  value = tostring(value or "")
  if value == "" then return { "" } end
  return vim.split(value, "\n", { plain = true })
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
  return (value:gsub("(%S)(%S*)", function(first, rest)
    return first:upper() .. rest
  end))
end

---@param value string?
---@return string?
local function state_highlight(value)
  value = tostring(value or ""):gsub("_", " "):lower()
  if value == "open" then return "DiffReviewStatusOpen" end
  if value == "closed" then return "DiffReviewStatusClosed" end
  return nil
end

---@param value any
---@return integer?
local function parse_activity_time(value)
  value = tostring(value or "")
  if value == "" then return nil end
  if
    diff_review
    and type(diff_review._datetime) == "table"
    and type(diff_review._datetime.parse) == "function"
  then
    local ok, epoch = pcall(diff_review._datetime.parse, value)
    if ok and epoch then return epoch end
  end
  local ok, epoch = pcall(vim.fn.strptime, "%Y-%m-%dT%H:%M:%SZ", value)
  if ok and tonumber(epoch) and tonumber(epoch) > 0 then return tonumber(epoch) end
  return nil
end

---@param value any
---@return string
local function activity_datetime(value)
  value = tostring(value or "")
  if value == "" then return "" end
  local overview = diff_review and type(diff_review._pr_overview) == "table" and diff_review._pr_overview or nil
  if overview and type(overview.comment_datetime) == "function" then return overview.comment_datetime(value) end
  return value
end

---@param item GithubGhDetail
---@return string
local function activity_text(item)
  local latest_value = ""
  local latest_epoch = nil

  local function consider(value)
    value = tostring(value or "")
    if value == "" then return end
    local epoch = parse_activity_time(value)
    if epoch then
      if not latest_epoch or epoch > latest_epoch then
        latest_epoch = epoch
        latest_value = value
      end
      return
    end
    if latest_value == "" or value > latest_value then latest_value = value end
  end

  consider(item.updated_at or item.updatedAt)
  consider(item.created_at or item.createdAt)
  for _, comment in ipairs(type(item.comments) == "table" and item.comments or {}) do
    consider(comment.updated_at or comment.updatedAt)
    consider(comment.created_at or comment.createdAt)
  end
  return activity_datetime(latest_value)
end

---@param values any
---@param key string
---@return string[]
local function names(values, key)
  local result = {}
  for _, value in ipairs(type(values) == "table" and values or {}) do
    if type(value) == "table" then
      local name = value[key] or value.name or value.login
      if type(name) == "string" and name ~= "" then result[#result + 1] = name end
    elseif type(value) == "string" and value ~= "" then
      result[#result + 1] = value
    end
  end
  return result
end

---@return table?
local function pr_overview()
  if diff_review and type(diff_review._pr_overview) == "table" then return diff_review._pr_overview end
  return nil
end

---@return table?
local function pr_edit()
  if diff_review and type(diff_review._pr_edit) == "table" then return diff_review._pr_edit end
  return nil
end

---@param username string
---@return string
local function assignee_token(username)
  local overview = pr_overview()
  if overview and type(overview.reviewer_token) == "function" then return overview.reviewer_token(username) end
  return "@" .. tostring(username or ""):gsub("^@", "")
end

---@param value any
---@return string
local function assignee_login(value)
  local overview = pr_overview()
  if overview and type(overview.reviewer_login) == "function" then return overview.reviewer_login(value) end
  if type(value) == "table" then value = value.login or value.name or value.slug end
  return tostring(value or ""):gsub("^@", "")
end

---@param values any
---@return string[]
local function assignee_usernames(values)
  local edit = pr_edit()
  if type(values) == "string" and edit and type(edit.reviewer_usernames) == "function" then
    return edit.reviewer_usernames(values)
  end

  local result = {}
  local seen = {}
  local source = values
  if type(source) == "string" then
    source = {}
    for token in values:gmatch("@?[%w][%w_-]*") do
      source[#source + 1] = token
    end
  end
  for _, value in ipairs(type(source) == "table" and source or {}) do
    local username = assignee_login(value)
    local key = username:lower()
    if username ~= "" and not seen[key] then
      seen[key] = true
      result[#result + 1] = username
    end
  end
  return result
end

---@param values any
---@return string
local function assignee_text(values)
  local tokens = {}
  for _, username in ipairs(assignee_usernames(values)) do
    tokens[#tokens + 1] = assignee_token(username)
  end
  return table.concat(tokens, " ")
end

---@param current string[]
---@param saved string[]
---@return string[], string[]
local function assignee_delta(current, saved)
  local current_set = {}
  local saved_set = {}
  for _, username in ipairs(current or {}) do
    current_set[username:lower()] = true
  end
  for _, username in ipairs(saved or {}) do
    saved_set[username:lower()] = true
  end

  local add = {}
  local remove = {}
  for _, username in ipairs(current or {}) do
    if not saved_set[username:lower()] then add[#add + 1] = username end
  end
  for _, username in ipairs(saved or {}) do
    if not current_set[username:lower()] then remove[#remove + 1] = username end
  end
  return add, remove
end

---@param current string[]
---@param saved string[]
---@return boolean
local function assignees_changed(current, saved)
  local add, remove = assignee_delta(current, saved)
  return #add > 0 or #remove > 0
end

---@param mark integer?
---@return integer?
local function mark_row(mark)
  if not (state.buf and mark) then return nil end
  local ok, position = pcall(vim.api.nvim_buf_get_extmark_by_id, state.buf, namespace, mark, {})
  if not ok or not position or position[1] == nil then return nil end
  return position[1]
end

---@return integer?, integer?
local function body_range0()
  return mark_row(state.body_start_mark), mark_row(state.body_end_mark)
end

---@return integer?
local function title_row0()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil end
  local row = mark_row(state.title_mark)
  if row then
    local line = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
    if line:match("^Title:%s*") then return row end
  end
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match("^Title:%s*") then return index - 1 end
  end
  return row
end

---@return integer?
local function assignees_row0()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil end
  local row = mark_row(state.assignees_mark)
  if row then
    local line = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
    if line:match("^Assignees:%s*") then return row end
  end
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match("^Assignees:%s*") then return index - 1 end
  end
  return row
end

---@param row0 integer
---@return boolean
local function row_is_editable(row0)
  if state.kind ~= "issue" then return false end
  local title_row = title_row0()
  if title_row == row0 then return true end
  local assignees_row = assignees_row0()
  if assignees_row == row0 then return true end
  local body_start, body_end = body_range0()
  return body_start ~= nil and body_end ~= nil and row0 >= body_start and row0 < body_end
end

local function sync_modifiable()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local win = state.win
  if not (win and vim.api.nvim_win_is_valid(win)) then
    win = vim.fn.bufwinid(buf)
    state.win = win ~= -1 and win or nil
  end
  local editable = false
  if win and vim.api.nvim_win_is_valid(win) then
    editable = row_is_editable(vim.api.nvim_win_get_cursor(win)[1] - 1)
  end
  if vim.bo[buf].modifiable ~= editable then vim.bo[buf].modifiable = editable end
end

---@param win integer?
local function apply_window_options(win)
  local buf = state.buf
  if not (buf and win and vim.api.nvim_win_is_valid(win)) then return end
  if vim.api.nvim_win_get_buf(win) ~= buf then return end
  vim.wo[win].wrap = true
  vim.wo[win].linebreak = true
  vim.wo[win].breakindent = true
end

---@return string
local function current_title()
  local buf = state.buf
  local title_row = title_row0()
  if not (buf and title_row) then return state.saved_title end
  local line = vim.api.nvim_buf_get_lines(buf, title_row, title_row + 1, false)[1] or ""
  return vim.trim((line:gsub("^Title:%s*", "", 1)))
end

---@return string
local function current_body()
  local buf = state.buf
  local body_start, body_end = body_range0()
  if not (buf and body_start and body_end) then return state.saved_body end
  return table.concat(vim.api.nvim_buf_get_lines(buf, body_start, body_end, false), "\n")
end

---@return string[]
local function current_assignees()
  local buf = state.buf
  local row = assignees_row0()
  if not (buf and row) then return state.saved_assignees end
  local line = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
  return assignee_usernames(line:gsub("^Assignees:%s*", "", 1))
end

---@param key string?
---@param default boolean
---@return boolean
local function folded(key, default)
  state.folds = state.folds or {}
  local value = nil
  if key then value = state.folds[key] end
  if value == nil then return default end
  return value
end

---@param key string
---@param value boolean
local function set_folded(key, value)
  state.folds = state.folds or {}
  state.folds[key] = value
end

---@param entry table?
---@return boolean
local function default_folded(entry)
  if not entry then return false end
  if entry.kind == "pr_comment" and entry.pr_comment then return true end
  if entry.kind == "section" and entry.section then return entry.section.default_folded == true end
  return false
end

---@return table?
local function entry_under_cursor()
  local buf = state.buf
  local win = state.win
  if not (buf and win and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_win_is_valid(win)) then return nil end
  if vim.api.nvim_win_get_buf(win) ~= buf then return nil end
  return (state.entries or {})[vim.api.nvim_win_get_cursor(win)[1]]
end

local function preserve_current_edits()
  if state.kind ~= "issue" or not state.item then return end
  state.item.title = current_title()
  state.item.body = current_body()
  state.item.assignees = current_assignees()
end

---@return boolean, boolean, boolean
local function dirty_flags()
  local assignees = current_assignees()
  return current_title() ~= state.saved_title,
    current_body() ~= state.saved_body,
    assignees_changed(assignees, state.saved_assignees)
end

---@param buf integer
---@param marker_key "title_marker_id"|"body_marker_id"|"assignees_marker_id"
---@param wanted boolean
---@param row0 integer?
local function set_dirty_marker(buf, marker_key, wanted, row0)
  if wanted and not state[marker_key] and row0 then
    state[marker_key] = vim.api.nvim_buf_set_extmark(buf, namespace, row0, 0, {
      virt_text = { { "*", "DiffReviewPrDirty" } },
      virt_text_pos = "inline",
      right_gravity = false,
    })
  elseif not wanted and state[marker_key] then
    pcall(vim.api.nvim_buf_del_extmark, buf, namespace, state[marker_key])
    state[marker_key] = nil
  end
end

local function refresh_modified()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if state.kind ~= "issue" then
    vim.bo[buf].modified = false
    return
  end
  local title_dirty, body_dirty, assignees_dirty = dirty_flags()
  set_dirty_marker(buf, "title_marker_id", title_dirty, title_row0())
  set_dirty_marker(buf, "assignees_marker_id", assignees_dirty, assignees_row0())
  local body_start = select(1, body_range0())
  set_dirty_marker(buf, "body_marker_id", body_dirty, body_start and body_start - 1 or nil)
  vim.bo[buf].modified = title_dirty or body_dirty or assignees_dirty
end

---@return integer?
local function markdown_namespace()
  if
    diff_review
    and type(diff_review._pr_edit) == "table"
    and type(diff_review._pr_edit.markdown_namespace) == "function"
  then
    return diff_review._pr_edit.markdown_namespace()
  end
  local ok, ui = pcall(require, "render-markdown.core.ui")
  if ok and type(ui) == "table" and ui.ns then return ui.ns end
  return vim.api.nvim_get_namespaces()["render-markdown.nvim"]
end

local function prune_markdown()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local render_namespace = markdown_namespace()
  if not render_namespace then return end
  local body_start, body_end = body_range0()
  if body_start == nil or body_end == nil then return end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, render_namespace, 0, -1, {})) do
    local row = mark[2]
    if row < body_start or row >= body_end then
      pcall(vim.api.nvim_buf_del_extmark, buf, render_namespace, mark[1])
    end
  end
end

local function render_markdown()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if state.kind ~= "issue" then return end
  local ok, render_markdown_plugin = pcall(require, "render-markdown")
  if not ok or type(render_markdown_plugin) ~= "table" or type(render_markdown_plugin.render) ~= "function" then
    return
  end
  local win = state.win
  if not (win and vim.api.nvim_win_is_valid(win)) then
    local found = vim.fn.bufwinid(buf)
    win = found ~= -1 and found or nil
  end
  if not win then return end
  if not state.markdown_language_registered then
    pcall(vim.treesitter.language.register, "markdown", "GithubIssue")
    state.markdown_language_registered = true
  end
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  local render_ok, err = pcall(render_markdown_plugin.render, {
    buf = buf,
    win = win,
    config = {
      enabled = true,
      render_modes = true,
      debounce = 0,
      completions = { lsp = { enabled = false } },
      sign = { enabled = false },
      win_options = {
        conceallevel = { default = conceallevel, rendered = conceallevel },
        concealcursor = { default = concealcursor, rendered = concealcursor },
      },
      on = {
        render = function()
          prune_markdown()
        end,
      },
    },
  })
  if render_ok then
    prune_markdown()
  elseif not state.markdown_failed then
    state.markdown_failed = true
    vim.notify("Issue markdown rendering failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitHub" })
  end
end

local function ensure_decoration_highlights()
  vim.api.nvim_set_hl(0, markdown_heading_hl, { fg = "#ffffff", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusOpen", { fg = "#50fa7b", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusClosed", { fg = "#6b7280", bold = true })
end

---@param buf integer
local function clear_decorations(buf)
  if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_clear_namespace(buf, decoration_namespace, 0, -1) end
end

---@param buf integer
---@param row integer?
---@param line string?
---@param hl_group string
local function highlight_line(buf, row, line, hl_group)
  if not (row and line) then return end
  vim.api.nvim_buf_set_extmark(buf, decoration_namespace, row - 1, 0, {
    end_col = #line,
    hl_group = hl_group,
    priority = 200,
  })
end

---@param rendered GithubIssueRendered
local function apply_decorations(rendered)
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  clear_decorations(buf)
  ensure_decoration_highlights()

  for _, line_highlight in ipairs(rendered.line_highlights or {}) do
    pcall(vim.api.nvim_buf_set_extmark, buf, decoration_namespace, line_highlight.line - 1, 0, {
      line_hl_group = line_highlight.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(rendered.highlights or {}) do
    pcall(vim.api.nvim_buf_set_extmark, buf, decoration_namespace, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end

  highlight_line(buf, rendered.opening_heading_row, rendered.lines[rendered.opening_heading_row], "DiffReviewStatusHeader")
  highlight_line(buf, rendered.comments_heading_row, rendered.lines[rendered.comments_heading_row], "DiffReviewStatusHeader")

  if not (rendered.body_start and rendered.body_end) then return end
  for row = rendered.body_start, rendered.body_end - 1 do
    local line = rendered.lines[row] or ""
    if line:match("^%s*#+%s+") then highlight_line(buf, row, line, markdown_heading_hl) end
  end
end

---@param item GithubGhDetail
---@return string
local function release_text(item)
  local overview = pr_overview()
  if overview and type(overview.milestone_text) == "function" then
    return overview.milestone_text({ milestone = item and item.milestone or nil })
  end
  return tostring(item and item.milestone or "")
end

---@param title string
---@param count integer
---@return string
local function section_heading_text(title, count)
  if diff_review and type(diff_review._status_section_heading_text) == "function" then
    return diff_review._status_section_heading_text(title, count)
  end
  title = tostring(title or ""):gsub("%s*:%s*$", "")
  return ("%s (%d):"):format(title, math.max(0, math.floor(tonumber(count) or 0)))
end

---@param comment table
---@return table
local function pr_comment(comment)
  return {
    user = comment.author or comment.user or "unknown",
    body = comment.body or "",
    created_at = comment.created_at or comment.createdAt or "",
    updated_at = comment.updated_at or comment.updatedAt or "",
    remote_id = comment.id or comment.databaseId or comment.url,
    url = comment.url,
  }
end

---@param comments table[]
---@return table[]
local function pr_comments(comments)
  local converted = {}
  for _, comment in ipairs(comments) do
    converted[#converted + 1] = pr_comment(comment)
  end
  return converted
end

---@param comment table?
---@param index integer
---@return string
local function comment_entry_id(comment, index)
  return github_comment_rows.entry_id(comment, index)
end

---@return GithubCommentRowsOpts
local function comment_row_options()
  local overview = pr_overview()
  local opts = {
    comment_icon = diff_review and diff_review._comment_icon or "",
    entry_id = comment_entry_id,
    line_hl_group = "DiffReviewReviewComment",
    body_hl_group = "DiffReviewReviewComment",
    date_hl_group = "DiffReviewStatusDate",
  }
  if overview and type(overview.comment_datetime) == "function" then opts.relative_date = overview.comment_datetime end
  if
    diff_review
    and type(diff_review._review) == "table"
    and type(diff_review._review.comment_rule_width) == "function"
    and type(diff_review._review.truncate_preview_text) == "function"
  then
    opts.preview_width = function(prefix)
      return math.max(0, diff_review._review.comment_rule_width(nil, nil) - vim.fn.strdisplaywidth(prefix))
    end
    opts.truncate_preview = diff_review._review.truncate_preview_text
  end
  return opts
end

---@class GithubIssueRendered
---@field lines string[]
---@field entries table<integer, table>
---@field highlights table[]
---@field line_highlights table[]
---@field title_row integer?
---@field assignees_row integer?
---@field opening_heading_row integer?
---@field body_start integer?
---@field body_end integer?
---@field comments_heading_row integer?

---@param item GithubGhDetail
---@return GithubIssueRendered
local function render_item(item)
  local labels = names(item.labels, "name")
  local assignees = assignee_usernames(item.assignees)
  local projects = names(item.projects, "name")
  local comments = pr_comments(type(item.comments) == "table" and item.comments or {})
  local comment_opts = comment_row_options()
  local comment_alignment = github_comment_rows.alignment(comments, comment_opts)
  local body_lines = split_body(item.body)
  local lines = {}
  local entries = {}
  local highlights = {}
  local line_highlights = {}

  local title_row = #lines + 1
  lines[#lines + 1] = "Title:  " .. tostring(item.title or "")
  add_meta(lines, "Author", item.author)
  local item_state = state_text(item.state)
  if item_state ~= "" then
    local state_row = #lines + 1
    add_meta(lines, "State", item_state)
    local highlight_group = state_highlight(item_state)
    if highlight_group then
      highlights[#highlights + 1] = {
        line = state_row,
        start_col = 14,
        end_col = 14 + #item_state,
        hl_group = highlight_group,
      }
    end
  end
  local activity = activity_text(item)
  if activity ~= "" then
    local activity_row = #lines + 1
    add_meta(lines, "Activity", activity)
    highlights[#highlights + 1] = {
      line = activity_row,
      start_col = 14,
      end_col = 14 + #activity,
      hl_group = "DiffReviewStatusDate",
    }
  end
  if item.kind == "pr" then
    add_meta(lines, "Head", item.head_ref_name or "")
    add_meta(lines, "Base", item.base_ref_name or "")
  end
  add_meta(lines, "Release", release_text(item))
  if #projects > 0 then add_meta(lines, "Projects", table.concat(projects, ", ")) end
  add_meta(lines, "Subscription", item.subscription)
  if #labels > 0 then add_meta(lines, "Labels", table.concat(labels, ", ")) end
  local assignees_row = #lines + 1
  lines[#lines + 1] = string.format("%-14s%s", "Assignees:", assignee_text(assignees))

  lines[#lines + 1] = ""
  local opening_heading_row = #lines + 1
  lines[#lines + 1] = "Description:"
  local body_start = #lines + 1
  vim.list_extend(lines, body_lines)
  local body_end = body_start + #body_lines
  lines[#lines + 1] = ""
  local comments_heading_row = #lines + 1
  local comments_section = {
    name = "issue:comments",
    title = "Comments",
    default_folded = false,
    issue_comments = comments,
  }
  entries[comments_heading_row] = { id = "issue:comments", kind = "section", section = comments_section }
  lines[#lines + 1] = section_heading_text("Comments", #comments)

  if not folded("issue:comments", false) then
    comment_opts.alignment = comment_alignment
    comment_opts.is_folded = function(entry_id, default)
      return folded(entry_id, default)
    end
    for _, row in ipairs(github_comment_rows.rows(comments, comment_opts)) do
      lines[#lines + 1] = row.text
      local line_number = #lines
      if row.entry then entries[line_number] = row.entry end
      if row.line_hl_group then line_highlights[#line_highlights + 1] = { line = line_number, hl_group = row.line_hl_group } end
      for _, highlight in ipairs(row.highlights or {}) do
        highlights[#highlights + 1] = {
          line = line_number,
          start_col = highlight.start_col,
          end_col = highlight.end_col,
          hl_group = highlight.hl_group,
        }
      end
    end
  end

  return {
    lines = lines,
    entries = entries,
    highlights = highlights,
    line_highlights = line_highlights,
    title_row = title_row,
    assignees_row = assignees_row,
    opening_heading_row = opening_heading_row,
    body_start = body_start,
    body_end = body_end,
    comments_heading_row = comments_heading_row,
  }
end

---@param item GithubGhDetail
---@param opts? { folds?: table<string, boolean> }
---@return GithubIssueRendered
function M.render_item(item, opts)
  opts = opts or {}
  if not opts.folds then return render_item(item) end

  local previous_folds = state.folds
  state.folds = opts.folds
  local ok, rendered = xpcall(render_item, debug.traceback, item)
  state.folds = previous_folds
  if not ok then error(rendered, 2) end
  return rendered
end

---@return table[]
local function hint_segments()
  local segments = {}
  for index, spec in ipairs(command_specs) do
    if spec.pinned then
      if #segments > 0 then segments[#segments + 1] = { " | ", "DiffReviewStatusHint" } end
      segments[#segments + 1] = { spec.key, "DiffReviewStatusHintKey" }
      segments[#segments + 1] = { " " .. spec.label, "DiffReviewStatusHint" }
    end
  end
  return segments
end

---@return string
local function hint_title()
  local number = state.number and state.number > 0 and (" #" .. tostring(state.number)) or ""
  return "GitHub " .. (state.kind == "pr" and "PR" or "Issue") .. number
end

---@return string
local function hint_winbar()
  local segments = hint_segments()
  if diff_review and type(diff_review._status_hint_winbar) == "function" then
    return diff_review._status_hint_winbar(segments, hint_title())
  end
  local parts = { ("%%#DiffReviewStatusLabel#%s%%*"):format(hint_title():gsub("%%", "%%%%")) }
  if #segments > 0 then parts[#parts + 1] = "%=" end
  for _, segment in ipairs(segments) do
    local text = tostring(segment[1] or ""):gsub("%%", "%%%%")
    local highlight = segment[2]
    parts[#parts + 1] = highlight and ("%%#%s#%s%%*"):format(highlight, text) or text
  end
  return table.concat(parts)
end

---@param win integer?
local function apply_command_winbar(win)
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local winbar = hint_winbar()
  if win then
    if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf then vim.wo[win].winbar = winbar end
    return
  end
  for _, issue_win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_win_is_valid(issue_win) then vim.wo[issue_win].winbar = winbar end
  end
end

local function show_commands_popup()
  local lines = {}
  local key_width = 0
  for _, spec in ipairs(command_specs) do
    key_width = math.max(key_width, vim.fn.strdisplaywidth(spec.key))
  end
  for _, spec in ipairs(command_specs) do
    lines[#lines + 1] = string.format("%-" .. tostring(key_width) .. "s  %s", spec.key, spec.desc)
  end

  local title = " GitHub Issue Commands "
  local width = #title + 4
  for _, line in ipairs(lines) do
    width = math.max(width, vim.fn.strdisplaywidth(line) + 4)
  end
  width = math.min(math.max(width, 32), math.max(32, vim.o.columns - 4))
  local height = #lines
  local popup_buf = vim.api.nvim_create_buf(false, true)
  vim.bo[popup_buf].buftype = "nofile"
  vim.bo[popup_buf].bufhidden = "wipe"
  vim.bo[popup_buf].swapfile = false
  vim.bo[popup_buf].filetype = "GithubIssueHelp"
  vim.api.nvim_buf_set_lines(popup_buf, 0, -1, false, lines)
  vim.bo[popup_buf].modifiable = false
  for index, spec in ipairs(command_specs) do
    vim.api.nvim_buf_set_extmark(popup_buf, namespace, index - 1, 0, {
      end_col = #spec.key,
      hl_group = "DiffReviewStatusHintKey",
    })
  end
  local popup_win = vim.api.nvim_open_win(popup_buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = math.max(0, math.floor((vim.o.lines - height) / 2) - 1),
    col = math.max(0, math.floor((vim.o.columns - width) / 2)),
    style = "minimal",
    border = "rounded",
    title = title,
    title_pos = "center",
  })
  vim.wo[popup_win].winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder"
  local close = function()
    if vim.api.nvim_win_is_valid(popup_win) then vim.api.nvim_win_close(popup_win, true) end
  end
  for _, key in ipairs({ "q", "<Esc>", "?" }) do
    vim.keymap.set("n", key, close, { buffer = popup_buf, nowait = true, desc = "Close issue command help" })
  end
end

---@param buf integer
local function setup_autocmds(buf)
  local group = vim.api.nvim_create_augroup("GithubIssueView" .. tostring(buf), { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "CursorMovedI", "InsertEnter" }, {
    group = group,
    buffer = buf,
    callback = function(args)
      state.win = vim.api.nvim_get_current_win()
      apply_window_options(state.win)
      sync_modifiable()
      apply_command_winbar(args.win)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = group,
    buffer = buf,
    callback = function(args)
      state.win = vim.api.nvim_get_current_win()
      apply_window_options(state.win)
      apply_command_winbar(args.win)
    end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = buf,
    callback = function()
      refresh_modified()
      render_markdown()
      sync_modifiable()
    end,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buf,
    callback = function()
      M.save()
    end,
  })
end

local toggle_fold

local function browse_current_target()
  local entry = entry_under_cursor()
  local comment = entry and entry.pr_comment or nil
  if comment and comment.url and comment.url ~= "" then
    gh.open_url(comment.url)
    return
  end
  if state.item then gh.open_url(state.item.url) end
end

---@return integer
local function ensure_buffer()
  if state.buf and vim.api.nvim_buf_is_valid(state.buf) then return state.buf end

  local buf = vim.api.nvim_create_buf(true, true)
  state.buf = buf
  vim.bo[buf].buftype = "acwrite"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GithubIssue"
  enable_user_completion(buf, state.repo)
  setup_autocmds(buf)

  vim.keymap.set("n", "b", function()
    browse_current_target()
  end, { buffer = buf, desc = "Browse GitHub issue", nowait = true })

  vim.keymap.set("n", "<Tab>", function()
    if toggle_fold then toggle_fold() end
  end, { buffer = buf, desc = "Toggle GitHub issue fold", nowait = true })

  vim.keymap.set("n", "R", function()
    M.refresh()
  end, { buffer = buf, desc = "Refresh GitHub issue", nowait = true })

  vim.keymap.set({ "n", "i" }, "<C-s>", function()
    M.save()
  end, { buffer = buf, desc = "Sync GitHub issue edits", nowait = true })

  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
  end, { buffer = buf, desc = "Close GitHub issue", nowait = true })

  vim.keymap.set("n", "?", show_commands_popup, { buffer = buf, desc = "Show GitHub issue commands", nowait = true })

  vim.keymap.set("i", "<CR>", function()
    local win = state.win
    if win and vim.api.nvim_win_is_valid(win) then
      local title_row = title_row0()
      local assignees_row = assignees_row0()
      local row = vim.api.nvim_win_get_cursor(win)[1] - 1
      if title_row == row or assignees_row == row then return "" end
    end
    return "\n"
  end, { buffer = buf, expr = true, desc = "Issue title newline guard" })

  return buf
end

local function clear_marks()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  state.entries = {}
  state.title_mark = nil
  state.body_start_mark = nil
  state.body_end_mark = nil
  state.assignees_mark = nil
  state.title_marker_id = nil
  state.body_marker_id = nil
  state.assignees_marker_id = nil
end

---@param rendered GithubIssueRendered
local function set_rendered(rendered)
  local buf = ensure_buffer()
  clear_marks()
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, rendered.lines)
  state.entries = rendered.entries or {}
  state.title_mark = rendered.title_row and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.title_row - 1, 0, {})
  state.assignees_mark = rendered.assignees_row
      and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.assignees_row - 1, 0, {})
  state.body_start_mark = rendered.body_start and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.body_start - 1, 0, {})
  state.body_end_mark = rendered.body_end and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.body_end - 1, 0, {})
  vim.bo[buf].modified = false
  vim.bo[buf].modifiable = false
  sync_modifiable()
  refresh_modified()
  apply_command_winbar()
  render_markdown()
  apply_decorations(rendered)
end

toggle_fold = function()
  if not state.item then return end
  local entry = entry_under_cursor()
  local fold_id = entry and (entry.fold_target_id or entry.id) or nil
  if not fold_id then return end
  local next_folded = not folded(fold_id, default_folded(entry))
  set_folded(fold_id, next_folded)
  if entry.pr_comment then
    for index, comment in ipairs(pr_comments(type(state.item.comments) == "table" and state.item.comments or {})) do
      if comment.url ~= "" and comment.url == entry.pr_comment.url then
        local shared_id = comment_entry_id(comment, index)
        set_folded(shared_id, next_folded)
        break
      end
    end
  end
  preserve_current_edits()
  set_rendered(render_item(state.item))

  local win = state.win
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  for row, rendered_entry in pairs(state.entries or {}) do
    if rendered_entry and rendered_entry.id == fold_id then
      pcall(vim.api.nvim_win_set_cursor, win, { row, 0 })
      return
    end
  end
end

---@param lines string[]
local function set_plain_lines(lines)
  local buf = ensure_buffer()
  clear_marks()
  clear_decorations(buf)
  state.entries = {}
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modified = false
  vim.bo[buf].modifiable = false
  apply_command_winbar()
end

function M.save()
  local buf = ensure_buffer()
  if state.kind ~= "issue" then
    vim.bo[buf].modified = false
    return
  end
  local title = current_title()
  local body = current_body()
  local assignees = current_assignees()
  local add_assignees, remove_assignees = assignee_delta(assignees, state.saved_assignees)
  local edit = {}
  if title ~= state.saved_title then edit.title = title end
  if body ~= state.saved_body then edit.body = body end
  if #add_assignees > 0 then edit.add_assignees = add_assignees end
  if #remove_assignees > 0 then edit.remove_assignees = remove_assignees end
  if not edit.title and not edit.body and not edit.add_assignees and not edit.remove_assignees then
    refresh_modified()
    return
  end

  vim.bo[buf].modified = false
  refresh_modified()
  gh.update_issue_async(state.cwd, state.number, state.repo, edit, function(result)
    if not result.ok then
      vim.bo[buf].modified = true
      refresh_modified()
      vim.notify(result.message or "GitHub issue update failed", vim.log.levels.ERROR, { title = "GitHub" })
      return
    end
    state.saved_title = title
    state.saved_body = body
    state.saved_assignees = vim.deepcopy(assignees)
    if state.item then
      state.item.title = title
      state.item.body = body
      state.item.assignees = vim.deepcopy(assignees)
    end
    refresh_modified()
    vim.notify("Issue #" .. tostring(state.number) .. " updated", vim.log.levels.INFO, { title = "GitHub" })
  end)
end

---@param result GithubGhDetailResult
---@param opts? { keep_existing?: boolean }
local function on_detail(result, opts)
  opts = opts or {}
  state.loading = false
  if not result.ok or not result.item then
    local message = result.message or "Unable to load GitHub item"
    vim.notify(message, vim.log.levels.ERROR, { title = "GitHub" })
    if not opts.keep_existing then set_plain_lines({ message }) end
    return
  end
  state.item = result.item
  state.saved_title = result.item.title or ""
  state.saved_body = result.item.body or ""
  state.saved_assignees = assignee_usernames(result.item.assignees)
  if state.item.repo and state.item.repo ~= "" then
    state.repo = state.item.repo
    if state.buf and vim.api.nvim_buf_is_valid(state.buf) then enable_user_completion(state.buf, state.item.repo) end
    load_repo_metadata(state.cwd, state.item.repo)
  end
  set_rendered(render_item(result.item))
end

function M.refresh()
  local keep_existing = state.item ~= nil
  state.loading = true
  state.refresh_request_id = state.refresh_request_id + 1
  local request_id = state.refresh_request_id
  if not keep_existing then set_plain_lines({ "Loading GitHub " .. state.kind .. "..." }) end
  local function handle_detail(result)
    if request_id ~= state.refresh_request_id then return end
    on_detail(result, { keep_existing = keep_existing })
  end
  if state.kind == "pr" then
    gh.pr_view_async(state.cwd, state.number, state.repo, handle_detail)
  else
    gh.issue_view_async(state.cwd, state.number, state.repo, handle_detail)
  end
end

---@class GithubIssueViewOpenOpts
---@field kind? "issue"|"pr"
---@field number integer|string
---@field repo? string
---@field cwd? string
---@field item? GithubGhItem|GithubGhDetail

---@param opts GithubIssueViewOpenOpts
function M.open(opts)
  state.cwd = opts.cwd or vim.fn.getcwd()
  state.kind = opts.kind or "issue"
  state.number = tonumber(opts.number) or 0
  state.repo = opts.repo
  state.item = opts.item
  state.saved_title = opts.item and opts.item.title or ""
  state.saved_body = opts.item and opts.item.body or ""
  state.saved_assignees = opts.item and assignee_usernames(opts.item.assignees) or {}
  state.markdown_failed = false
  state.markdown_language_registered = false
  state.folds = {}
  state.entries = {}

  local buf = ensure_buffer()
  if state.repo and state.repo ~= "" then
    enable_user_completion(buf, state.repo)
    load_repo_metadata(state.cwd, state.repo)
  end
  local name = "github://" .. state.kind .. "/" .. (state.repo or "current") .. "/" .. tostring(state.number)
  pcall(vim.api.nvim_buf_set_name, buf, name)
  vim.api.nvim_set_current_buf(buf)
  state.win = vim.api.nvim_get_current_win()
  apply_window_options(state.win)
  apply_command_winbar(state.win)
  if state.item then set_rendered(render_item(state.item)) end
  M.refresh()
end

return M
