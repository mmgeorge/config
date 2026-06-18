local gh = require("github.gh")
local diff_review_ok, diff_review = pcall(require, "diff_review")
if not diff_review_ok then diff_review = nil end

local namespace = vim.api.nvim_create_namespace("github.issue_view")

---@class GithubIssueViewState
---@field buf integer?
---@field win integer?
---@field cwd string
---@field kind "issue"|"pr"
---@field number integer
---@field repo string?
---@field item GithubGhDetail?
---@field loading boolean
---@field title_mark integer?
---@field body_start_mark integer?
---@field body_end_mark integer?
---@field title_marker_id integer?
---@field body_marker_id integer?
---@field saved_title string
---@field saved_body string
---@field markdown_failed boolean
---@field markdown_language_registered boolean

---@type GithubIssueViewState
local state = {
  cwd = "",
  kind = "issue",
  number = 0,
  loading = false,
  saved_title = "",
  saved_body = "",
  markdown_failed = false,
}

local M = {}

local command_specs = {
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

---@param row0 integer
---@return boolean
local function row_is_editable(row0)
  if state.kind ~= "issue" then return false end
  local title_row = title_row0()
  if title_row == row0 then return true end
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

---@return boolean, boolean
local function dirty_flags()
  return current_title() ~= state.saved_title, current_body() ~= state.saved_body
end

---@param buf integer
---@param marker_key "title_marker_id"|"body_marker_id"
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
  local title_dirty, body_dirty = dirty_flags()
  set_dirty_marker(buf, "title_marker_id", title_dirty, title_row0())
  local body_start = select(1, body_range0())
  set_dirty_marker(buf, "body_marker_id", body_dirty, body_start and body_start - 1 or nil)
  vim.bo[buf].modified = title_dirty or body_dirty
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

---@return table?
local function pr_overview()
  if diff_review and type(diff_review._pr_overview) == "table" then return diff_review._pr_overview end
  return nil
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

---@param comments table[]
---@param index integer
---@param alignment table?
---@return string
local function comment_line(comments, index, alignment)
  local overview = pr_overview()
  if overview and type(overview.issue_comment_line) == "function" then
    return overview.issue_comment_line(comments[index], false, alignment)
  end
  local comment = comments[index]
  local author = tostring(comment.user or "unknown")
  local date = tostring(comment.updated_at or comment.created_at or "unknown")
  local preview = tostring(comment.body or ""):gsub("\n", " "):gsub("%s+", " ")
  if preview == "" then return ("%s %s"):format(author, date) end
  return ("%s %s  %s"):format(author, date, preview)
end

---@class GithubIssueRendered
---@field lines string[]
---@field title_row integer?
---@field body_start integer?
---@field body_end integer?

---@param item GithubGhDetail
---@return GithubIssueRendered
local function render_item(item)
  local labels = names(item.labels, "name")
  local assignees = names(item.assignees, "login")
  local projects = names(item.projects, "name")
  local comments = pr_comments(type(item.comments) == "table" and item.comments or {})
  local overview = pr_overview()
  local comment_alignment = overview and type(overview.issue_comment_alignment) == "function"
      and overview.issue_comment_alignment(comments)
    or nil
  local body_lines = split_body(item.body)
  local lines = {}

  add_meta(lines, "Repo", item.repo)
  add_meta(lines, "URL", item.url)
  add_meta(lines, "State", item.state)
  add_meta(lines, "Author", item.author)
  if item.kind == "pr" then
    add_meta(lines, "Head", item.head_ref_name or "")
    add_meta(lines, "Base", item.base_ref_name or "")
  end
  add_meta(lines, "Milestone", item.milestone)
  if #projects > 0 then add_meta(lines, "Projects", table.concat(projects, ", ")) end
  add_meta(lines, "Subscription", item.subscription)
  if #labels > 0 then add_meta(lines, "Labels", table.concat(labels, ", ")) end
  if #assignees > 0 then add_meta(lines, "Assignees", table.concat(assignees, ", ")) end

  lines[#lines + 1] = ""
  local title_row = #lines + 1
  lines[#lines + 1] = "Title:  " .. tostring(item.title or "")
  lines[#lines + 1] = ""
  lines[#lines + 1] = "Opening Comment:"
  local body_start = #lines + 1
  vim.list_extend(lines, body_lines)
  local body_end = body_start + #body_lines
  lines[#lines + 1] = ""
  lines[#lines + 1] = "Comments (" .. tostring(#comments) .. "):"

  for index, comment in ipairs(comments) do
    lines[#lines + 1] = ""
    lines[#lines + 1] = comment_line(comments, index, comment_alignment)
  end

  return {
    lines = lines,
    title_row = title_row,
    body_start = body_start,
    body_end = body_end,
  }
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
      sync_modifiable()
      apply_command_winbar(args.win)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = group,
    buffer = buf,
    callback = function(args)
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
    if state.item then gh.open_url(state.item.url) end
  end, { buffer = buf, desc = "Browse GitHub issue", nowait = true })

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
      if title_row == vim.api.nvim_win_get_cursor(win)[1] - 1 then return "" end
    end
    return "\n"
  end, { buffer = buf, expr = true, desc = "Issue title newline guard" })

  return buf
end

local function clear_marks()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  state.title_mark = nil
  state.body_start_mark = nil
  state.body_end_mark = nil
  state.title_marker_id = nil
  state.body_marker_id = nil
end

---@param rendered GithubIssueRendered
local function set_rendered(rendered)
  local buf = ensure_buffer()
  clear_marks()
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, rendered.lines)
  state.title_mark = rendered.title_row and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.title_row - 1, 0, {})
  state.body_start_mark = rendered.body_start and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.body_start - 1, 0, {})
  state.body_end_mark = rendered.body_end and vim.api.nvim_buf_set_extmark(buf, namespace, rendered.body_end - 1, 0, {})
  vim.bo[buf].modified = false
  vim.bo[buf].modifiable = false
  sync_modifiable()
  refresh_modified()
  apply_command_winbar()
  render_markdown()
end

---@param lines string[]
local function set_plain_lines(lines)
  local buf = ensure_buffer()
  clear_marks()
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
  local edit = {}
  if title ~= state.saved_title then edit.title = title end
  if body ~= state.saved_body then edit.body = body end
  if not edit.title and not edit.body then
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
    if state.item then
      state.item.title = title
      state.item.body = body
    end
    refresh_modified()
    vim.notify("Issue #" .. tostring(state.number) .. " updated", vim.log.levels.INFO, { title = "GitHub" })
  end)
end

---@param result GithubGhDetailResult
local function on_detail(result)
  state.loading = false
  if not result.ok or not result.item then
    local message = result.message or "Unable to load GitHub item"
    vim.notify(message, vim.log.levels.ERROR, { title = "GitHub" })
    set_plain_lines({ message })
    return
  end
  state.item = result.item
  state.saved_title = result.item.title or ""
  state.saved_body = result.item.body or ""
  if state.item.repo and state.item.repo ~= "" then
    state.repo = state.item.repo
    if state.buf and vim.api.nvim_buf_is_valid(state.buf) then enable_user_completion(state.buf, state.item.repo) end
    load_repo_metadata(state.cwd, state.item.repo)
  end
  set_rendered(render_item(result.item))
end

function M.refresh()
  state.loading = true
  set_plain_lines({ "Loading GitHub " .. state.kind .. "..." })
  if state.kind == "pr" then
    gh.pr_view_async(state.cwd, state.number, state.repo, on_detail)
  else
    gh.issue_view_async(state.cwd, state.number, state.repo, on_detail)
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
  state.markdown_failed = false
  state.markdown_language_registered = false

  local buf = ensure_buffer()
  if state.repo and state.repo ~= "" then
    enable_user_completion(buf, state.repo)
    load_repo_metadata(state.cwd, state.repo)
  end
  local name = "github://" .. state.kind .. "/" .. (state.repo or "current") .. "/" .. tostring(state.number)
  pcall(vim.api.nvim_buf_set_name, buf, name)
  vim.api.nvim_set_current_buf(buf)
  state.win = vim.api.nvim_get_current_win()
  apply_command_winbar(state.win)
  if state.item then set_rendered(render_item(state.item)) end
  M.refresh()
end

return M
