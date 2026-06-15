local gh = require("github.gh")

---@class GithubNotificationDetailCache
---@field loading boolean
---@field lines string[]

---@class GithubNotificationsState
---@field buf integer?
---@field cwd string
---@field request_id integer
---@field notifications GithubGhNotification[]
---@field saved table<string, boolean>
---@field unread_overlay table<string, boolean>
---@field done GithubGhNotification[]
---@field expanded table<string, boolean>
---@field details table<string, GithubNotificationDetailCache>
---@field count_requested table<string, boolean>
---@field line_entries table<integer, GithubGhNotification>

---@type GithubNotificationsState
local state = {
  cwd = "",
  request_id = 0,
  notifications = {},
  saved = {},
  unread_overlay = {},
  done = {},
  expanded = {},
  details = {},
  count_requested = {},
  line_entries = {},
}

local M = {}

---@param cwd string?
local function load_repo_metadata(cwd)
  local repo_cache = require("github.repo_cache")
  repo_cache.ensure_metadata_for_cwd(cwd, function(done)
    gh.current_repo_async(cwd, done)
  end, function(repo, done)
    gh.repo_contributors_async(cwd, repo, done)
  end)
  local ok, issue_index = pcall(require, "github.issue_index")
  if ok then issue_index.ensure_current(cwd, { manual = false }) end
end

---@param value string
---@return string[]
local function body_lines(value)
  if value == "" then return { "_No description._" } end
  return vim.split(value, "\n", { plain = true })
end

---@param notification GithubGhNotification
---@return "unread"|"saved"|"done"|nil
local function category(notification)
  if state.saved[notification.id] then return "saved" end
  for _, done_notification in ipairs(state.done) do
    if done_notification.id == notification.id then return "done" end
  end
  if notification.unread or state.unread_overlay[notification.id] then return "unread" end
  return nil
end

---@param notification GithubGhNotification
---@return string
local function notification_number(notification)
  local _, number = gh.parse_subject(notification.subject)
  return number and ("#" .. tostring(number)) or notification.subject.type
end

---@param notification GithubGhNotification
---@return string
local function notification_label(notification)
  local count = notification.comments_count
  local count_text = count and (" (" .. tostring(count) .. ")") or " (...)"
  return notification_number(notification) .. " " .. notification.subject.title .. count_text
end

---@param endpoint string
---@return string
local function web_url(endpoint)
  if endpoint == "" then return "" end
  if endpoint:find("^https?://") then return endpoint end
  local path = endpoint:gsub("^/repos/", "/")
  path = path:gsub("/pulls/", "/pull/")
  return "https://github.com" .. path
end

---@param notification GithubGhNotification
---@return GithubGhNotification
local function copy_notification(notification)
  return vim.deepcopy(notification)
end

---@param notification GithubGhNotification
local function remember_done(notification)
  local copied = copy_notification(notification)
  for index, done_notification in ipairs(state.done) do
    if done_notification.id == copied.id then
      table.remove(state.done, index)
      break
    end
  end
  table.insert(state.done, 1, copied)
  while #state.done > 10 do table.remove(state.done) end
end

---@return integer
local function ensure_buffer()
  if state.buf and vim.api.nvim_buf_is_valid(state.buf) then return state.buf end
  local buf = vim.api.nvim_create_buf(true, true)
  state.buf = buf
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GithubNotifications"
  require("github.repo_cache").enable_user_completion(buf)
  pcall(vim.api.nvim_buf_set_name, buf, "github://notifications")

  vim.keymap.set("n", "<Tab>", function()
    M.toggle_expand()
  end, { buffer = buf, desc = "Toggle GitHub notification details", nowait = true })
  vim.keymap.set("n", "<CR>", function()
    M.open_current()
  end, { buffer = buf, desc = "Open GitHub notification subject", nowait = true })
  vim.keymap.set("n", "b", function()
    M.browse_current()
  end, { buffer = buf, desc = "Browse GitHub notification subject", nowait = true })
  vim.keymap.set("n", "S", function()
    M.save_current()
  end, { buffer = buf, desc = "Save GitHub notification", nowait = true })
  vim.keymap.set("n", "U", function()
    M.unread_current()
  end, { buffer = buf, desc = "Mark GitHub notification unread locally", nowait = true })
  vim.keymap.set("n", "D", function()
    M.done_current()
  end, { buffer = buf, desc = "Done GitHub notification", nowait = true })
  vim.keymap.set("n", "r", function()
    M.refresh()
  end, { buffer = buf, desc = "Refresh GitHub notifications", nowait = true })
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
  end, { buffer = buf, desc = "Close GitHub notifications", nowait = true })

  return buf
end

---@param lines string[]
---@param entries table<integer, GithubGhNotification>
local function set_lines(lines, entries)
  local buf = ensure_buffer()
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  state.line_entries = entries
end

---@param lines string[]
---@param entries table<integer, GithubGhNotification>
---@param heading string
---@param notifications GithubGhNotification[]
local function append_section(lines, entries, heading, notifications)
  lines[#lines + 1] = heading .. ":"
  if #notifications == 0 then
    lines[#lines + 1] = "none"
    lines[#lines + 1] = ""
    return
  end

  for _, notification in ipairs(notifications) do
    local line = #lines + 1
    lines[line] = notification_label(notification)
    entries[line] = notification
    if state.expanded[notification.id] then
      local detail = state.details[notification.id]
      if not detail then
        lines[#lines + 1] = "  ...fetching..."
      else
        for _, detail_line in ipairs(detail.lines) do
          lines[#lines + 1] = "  " .. detail_line
        end
      end
    end
  end
  lines[#lines + 1] = ""
end

---@return GithubGhNotification[], GithubGhNotification[], GithubGhNotification[]
local function grouped_notifications()
  local unread = {}
  local saved = {}
  local done = {}

  for _, notification in ipairs(state.notifications) do
    local current_category = category(notification)
    if current_category == "unread" then
      unread[#unread + 1] = notification
    elseif current_category == "saved" then
      saved[#saved + 1] = notification
    end
  end

  for _, notification in ipairs(state.done) do
    done[#done + 1] = notification
  end

  return unread, saved, done
end

local function render()
  local lines = {
    "Hint: <tab> expand | <cr> open | b browse | S save | U unread | D done | r refresh | q close",
    "",
  }
  local entries = {}
  local unread, saved, done = grouped_notifications()
  append_section(lines, entries, "Unread", unread)
  append_section(lines, entries, "Saved", saved)
  append_section(lines, entries, "Done", done)
  if lines[#lines] == "" then table.remove(lines) end
  set_lines(lines, entries)
end

---@param notification GithubGhNotification
local function load_count(notification)
  if notification.comments_count ~= nil or state.count_requested[notification.id] then return end
  if notification.subject.url == "" then return end
  state.count_requested[notification.id] = true
  gh.api_get_async(state.cwd, notification.subject.url, function(result)
    if not result.ok or type(result.raw) ~= "table" then return end
    local comments_count = tonumber(result.raw.comments)
    if comments_count then
      notification.comments_count = comments_count
      render()
    end
  end)
end

local function load_counts()
  for _, notification in ipairs(state.notifications) do
    load_count(notification)
  end
  for _, notification in ipairs(state.done) do
    load_count(notification)
  end
end

---@return GithubGhNotification?
local function current_notification()
  local buf = state.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  return state.line_entries[line]
end

---@param notification GithubGhNotification
local function load_detail(notification)
  if state.details[notification.id] then return end
  state.details[notification.id] = { loading = true, lines = { "...fetching..." } }
  render()

  local endpoint = notification.subject.latest_comment_url
  local mode = endpoint ~= "" and "comment" or "subject"
  if endpoint == "" then endpoint = notification.subject.url end
  if endpoint == "" then
    state.details[notification.id] = { loading = false, lines = { "No GitHub API URL available." } }
    render()
    return
  end

  gh.api_get_async(state.cwd, endpoint, function(result)
    if not result.ok or type(result.raw) ~= "table" then
      state.details[notification.id] = { loading = false, lines = { result.message or "Unable to load notification." } }
      render()
      return
    end

    local raw = result.raw
    if raw.comments ~= nil then notification.comments_count = tonumber(raw.comments) or notification.comments_count end
    local lines = {}
    if mode == "comment" then
      local author = type(raw.user) == "table" and raw.user.login or ""
      lines[#lines + 1] = "Last comment by " .. (author ~= "" and author or "unknown")
      vim.list_extend(lines, body_lines(type(raw.body) == "string" and raw.body or ""))
    else
      vim.list_extend(lines, body_lines(type(raw.body) == "string" and raw.body or ""))
    end

    state.details[notification.id] = { loading = false, lines = lines }
    render()
  end)
end

function M.toggle_expand()
  local notification = current_notification()
  if not notification then return end
  state.expanded[notification.id] = not state.expanded[notification.id]
  if state.expanded[notification.id] then load_detail(notification) end
  render()
end

function M.open_current()
  local notification = current_notification()
  if not notification then return end
  local kind, number, repo = gh.parse_subject(notification.subject)
  if not (kind and number) then
    M.browse_current()
    return
  end
  require("github.issue_view").open({ kind = kind, number = number, repo = repo, cwd = state.cwd })
end

function M.browse_current()
  local notification = current_notification()
  if not notification then return end
  gh.open_url(web_url(notification.subject.url))
end

function M.save_current()
  local notification = current_notification()
  if not notification then return end
  state.saved[notification.id] = true
  state.unread_overlay[notification.id] = nil
  render()
  gh.notification_mark_read_async(state.cwd, notification.id, function(result)
    if result.code ~= 0 then
      vim.notify(result.output, vim.log.levels.ERROR, { title = "GitHub" })
    end
  end)
end

function M.unread_current()
  local notification = current_notification()
  if not notification then return end
  state.saved[notification.id] = nil
  state.unread_overlay[notification.id] = true
  for index, done_notification in ipairs(state.done) do
    if done_notification.id == notification.id then
      table.remove(state.done, index)
      break
    end
  end
  render()
end

function M.done_current()
  local notification = current_notification()
  if not notification then return end
  state.saved[notification.id] = nil
  state.unread_overlay[notification.id] = nil
  remember_done(notification)
  render()
  gh.notification_mark_done_async(state.cwd, notification.id, function(result)
    if result.code ~= 0 then
      vim.notify(result.output, vim.log.levels.ERROR, { title = "GitHub" })
    end
  end)
end

function M.refresh()
  state.cwd = state.cwd ~= "" and state.cwd or vim.fn.getcwd()
  state.request_id = state.request_id + 1
  local request_id = state.request_id
  gh.notifications_async(state.cwd, function(result)
    if request_id ~= state.request_id then return end
    if not result.ok then
      vim.notify(result.message or "Unable to load GitHub notifications", vim.log.levels.ERROR, { title = "GitHub" })
      return
    end
    state.notifications = result.notifications or {}
    render()
    load_counts()
  end)
end

function M.open()
  state.cwd = vim.fn.getcwd()
  local buf = ensure_buffer()
  load_repo_metadata(state.cwd)
  vim.api.nvim_set_current_buf(buf)
  set_lines({
    "Hint: <tab> expand | <cr> open | b browse | S save | U unread | D done | r refresh | q close",
    "",
    "...fetching...",
  }, {})
  M.refresh()
end

function M._reset_for_tests()
  state.notifications = {}
  state.saved = {}
  state.unread_overlay = {}
  state.done = {}
  state.expanded = {}
  state.details = {}
  state.count_requested = {}
  state.line_entries = {}
  state.request_id = 0
  if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
    vim.api.nvim_buf_delete(state.buf, { force = true })
  end
  state.buf = nil
end

return M
