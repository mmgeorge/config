---@module 'blink.cmp'
---@class blink.cmp.Source
local source = {}

---@class GithubIssueSourceMatch
---@field row integer
---@field cursor_col integer
---@field start_col integer
---@field query string

local cache_ttl_ms = 30 * 1000
local notification_ttl_ms = 10 * 1000
local completion_cache = {}
local notification_times = {}

---@param key string
---@param message string
---@param level integer
local function notify_once(key, message, level)
  local now = vim.uv.now()
  local last = notification_times[key]
  if last and now - last < notification_ttl_ms then return end
  notification_times[key] = now
  vim.schedule(function()
    vim.notify(message, level, { title = "GitHub issues" })
  end)
end

---@param opts table?
---@return table
function source.new(opts)
  local self = setmetatable({}, { __index = source })
  self.opts = opts or {}
  self.request_id = 0
  return self
end

---@param before_cursor string
---@return integer?
local function issue_token_start(before_cursor)
  local token_start
  local offset = 1
  while true do
    local found = before_cursor:find("#", offset, true)
    if not found then break end
    local previous = found > 1 and before_cursor:sub(found - 1, found - 1) or ""
    if found == 1 or previous:match("[%s%(%)%[%]%{%}<>:;,]") then token_start = found end
    offset = found + 1
  end
  return token_start
end

---@return GithubIssueSourceMatch?
local function current_match()
  local repo_cache = require("github.repo_cache")
  if not repo_cache.user_completion_enabled(0) then return nil end

  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_col = vim.api.nvim_win_get_cursor(0)[2]
  local before_cursor = vim.api.nvim_get_current_line():sub(1, cursor_col)
  local token_start = issue_token_start(before_cursor)
  if not token_start then return nil end

  local query = before_cursor:sub(token_start + 1)
  if query:find("#", 1, true) or query:match("^%s") then return nil end
  return {
    row = row,
    cursor_col = cursor_col,
    start_col = token_start - 1,
    query = vim.trim(query),
  }
end

---@return boolean
function source:enabled()
  return current_match() ~= nil
end

---@return string[]
function source:get_trigger_characters()
  return { "#" }
end

---@param item GithubGhItem
---@param match GithubIssueSourceMatch
---@param index integer
---@return table
local function completion_item(item, match, index)
  local number = "#" .. tostring(item.number)
  local title = vim.trim(tostring(item.title or ""))
  local label = title ~= "" and (number .. " " .. title) or number
  local detail = item.repo
  if item.state and item.state ~= "" then detail = detail ~= "" and (detail .. " | " .. item.state) or item.state end
  return {
    label = label,
    filterText = label,
    sortText = ("%03d-%010d"):format(index, tonumber(item.number) or 0),
    detail = detail,
    kind = vim.lsp.protocol.CompletionItemKind.Reference,
    textEdit = {
      newText = number,
      range = {
        start = { line = match.row, character = match.start_col },
        ["end"] = { line = match.row, character = match.cursor_col },
      },
    },
  }
end

---@param ctx table
---@param callback fun(result: table)
function source:get_completions(ctx, callback)
  local match = current_match()
  if not match then
    callback({ items = {}, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end

  if match.query == "" then
    callback({ items = {}, is_incomplete_backward = true, is_incomplete_forward = true })
    return
  end

  local repo_cache = require("github.repo_cache")
  local repo = repo_cache.completion_repo(0)
  if not repo then
    notify_once(
      "missing-repo:" .. tostring(vim.api.nvim_get_current_buf()),
      "GitHub issue completion has no repo context for this buffer",
      vim.log.levels.WARN
    )
    callback({ items = {}, is_incomplete_backward = true, is_incomplete_forward = true })
    return
  end

  local cache_key = table.concat({ repo:lower(), match.query:lower() }, "\n")
  local now = vim.uv.now()
  local cached = completion_cache[cache_key]
  if cached and now - cached.time < cache_ttl_ms then
    local items = {}
    for index, item in ipairs(cached.items) do
      items[#items + 1] = completion_item(item, match, index)
    end
    callback({ items = items, is_incomplete_backward = true, is_incomplete_forward = true })
    return
  end

  self.request_id = self.request_id + 1
  local request_id = self.request_id
  vim.defer_fn(function()
    if request_id ~= self.request_id then return end
    require("github.gh").search_issue_references_async(vim.fn.getcwd(), repo, match.query, function(result)
      if request_id ~= self.request_id then return end
      local raw_items = result.ok and result.items or {}
      if not result.ok then
        notify_once(
          table.concat({ "search-failed", repo:lower(), match.query:lower() }, "\n"),
          ("GitHub issue completion failed for #%s in %s:\n%s"):format(
            match.query,
            repo,
            result.message or "GitHub issue search failed"
          ),
          vim.log.levels.ERROR
        )
      elseif #raw_items == 0 then
        notify_once(
          table.concat({ "no-matches", repo:lower(), match.query:lower() }, "\n"),
          ("No open GitHub issue matched #%s in %s"):format(match.query, repo),
          vim.log.levels.INFO
        )
      end
      if result.ok then completion_cache[cache_key] = { time = vim.uv.now(), items = raw_items } end
      local items = {}
      for index, item in ipairs(raw_items) do
        items[#items + 1] = completion_item(item, match, index)
      end
      callback({ items = items, is_incomplete_backward = true, is_incomplete_forward = true })
    end)
  end, tonumber(self.opts.debounce_ms) or 120)
end

return source
