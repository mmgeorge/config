local gh = require("github.gh")

---@class GithubIssueViewState
---@field buf integer?
---@field win integer?
---@field cwd string
---@field kind "issue"|"pr"
---@field number integer
---@field repo string?
---@field item GithubGhDetail?
---@field loading boolean

---@type GithubIssueViewState
local state = {
  cwd = "",
  kind = "issue",
  number = 0,
  loading = false,
}

local M = {}

---@param cwd string?
---@param repo string?
local function load_repo_metadata(cwd, repo)
  if not (repo and repo ~= "") then return end
  require("github.repo_cache").ensure_metadata(cwd, repo, function(done)
    gh.repo_contributors_async(cwd, repo, done)
  end)
end

---@param buf integer
---@param repo? string
local function enable_user_completion(buf, repo)
  require("github.repo_cache").enable_user_completion(buf, repo)
end

---@param value string
---@return string[]
local function split_markdown(value)
  if value == "" then return { "_No description._" } end
  return vim.split(value, "\n", { plain = true })
end

---@param lines string[]
---@param label string
---@param value string
local function add_meta(lines, label, value)
  if value and value ~= "" then lines[#lines + 1] = label .. ": " .. value end
end

---@param item GithubGhDetail
---@return string[]
local function render_item(item)
  local lines = {
    "Hint: b browse | r refresh | q close",
    "",
  }
  add_meta(lines, "Repo", item.repo)
  add_meta(lines, "State", item.state)
  add_meta(lines, "Author", item.author)
  if item.kind == "pr" then
    add_meta(lines, "Head", item.head_ref_name or "")
    add_meta(lines, "Base", item.base_ref_name or "")
  end
  add_meta(lines, "URL", item.url)
  if #item.labels > 0 then add_meta(lines, "Labels", table.concat(item.labels, ", ")) end
  if #item.assignees > 0 then add_meta(lines, "Assignees", table.concat(item.assignees, ", ")) end
  lines[#lines + 1] = ""
  lines[#lines + 1] = "#" .. tostring(item.number) .. " " .. item.title
  lines[#lines + 1] = ""
  vim.list_extend(lines, split_markdown(item.body))
  lines[#lines + 1] = ""
  lines[#lines + 1] = "Comments (" .. tostring(#item.comments) .. ")"

  for _, comment in ipairs(item.comments) do
    lines[#lines + 1] = ""
    lines[#lines + 1] = "--- " .. comment.author .. " commented at " .. comment.created_at
    vim.list_extend(lines, split_markdown(comment.body))
  end

  return lines
end

---@return integer
local function ensure_buffer()
  if state.buf and vim.api.nvim_buf_is_valid(state.buf) then return state.buf end

  local buf = vim.api.nvim_create_buf(true, true)
  state.buf = buf
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "markdown"
  enable_user_completion(buf, state.repo)

  vim.keymap.set("n", "b", function()
    if state.item then gh.open_url(state.item.url) end
  end, { buffer = buf, desc = "Browse GitHub item", nowait = true })

  vim.keymap.set("n", "r", function()
    M.refresh()
  end, { buffer = buf, desc = "Refresh GitHub item", nowait = true })

  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
  end, { buffer = buf, desc = "Close GitHub item", nowait = true })

  return buf
end

---@param lines string[]
local function set_lines(lines)
  local buf = ensure_buffer()
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
end

---@param result GithubGhDetailResult
local function on_detail(result)
  state.loading = false
  if not result.ok or not result.item then
    local message = result.message or "Unable to load GitHub item"
    vim.notify(message, vim.log.levels.ERROR, { title = "GitHub" })
    set_lines({ "Hint: r refresh | q close", "", message })
    return
  end
  state.item = result.item
  if state.item.repo and state.item.repo ~= "" then
    state.repo = state.item.repo
    if state.buf and vim.api.nvim_buf_is_valid(state.buf) then enable_user_completion(state.buf, state.item.repo) end
    load_repo_metadata(state.cwd, state.item.repo)
  end
  set_lines(render_item(result.item))
end

function M.refresh()
  state.loading = true
  set_lines({ "Hint: r refresh | q close", "", "Loading GitHub " .. state.kind .. "..." })
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

---@param opts GithubIssueViewOpenOpts
function M.open(opts)
  state.cwd = opts.cwd or vim.fn.getcwd()
  state.kind = opts.kind or "issue"
  state.number = tonumber(opts.number) or 0
  state.repo = opts.repo
  state.item = nil

  local buf = ensure_buffer()
  if state.repo and state.repo ~= "" then
    enable_user_completion(buf, state.repo)
    load_repo_metadata(state.cwd, state.repo)
  end
  local name = "github://" .. state.kind .. "/" .. (state.repo or "current") .. "/" .. tostring(state.number)
  pcall(vim.api.nvim_buf_set_name, buf, name)
  vim.api.nvim_set_current_buf(buf)
  M.refresh()
end

---@param args string
function M.open_command(args)
  local parts = vim.split(vim.trim(args or ""), "%s+", { trimempty = true })
  if #parts == 0 then
    vim.notify("Usage: GithubIssueOpen [owner/repo] <number>", vim.log.levels.ERROR, { title = "GitHub" })
    return
  end

  local repo
  local number = parts[1]
  if #parts >= 2 then
    repo = parts[1]
    number = parts[2]
  end
  M.open({ kind = "issue", repo = repo, number = number })
end

return M
