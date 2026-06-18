local gh = require("github.gh")
local issue_index = require("github.issue_index")
local repo_cache = require("github.repo_cache")

local M = {}

local issue_preview_request_id = 0

---@class GithubPickerCommandTarget
---@field repo? string
---@field number string

---@param args string?
---@return GithubPickerCommandTarget?, string?
local function parse_open_args(args)
  local parts = vim.split(vim.trim(args or ""), "%s+", { trimempty = true })
  if #parts == 0 then return nil, nil end
  local number = #parts == 1 and parts[1] or parts[2]
  if not number:match("^%d+$") then return nil, "Usage: [owner/repo] <number>" end
  if #parts == 1 then return { number = number }, nil end
  if #parts == 2 then return { repo = parts[1], number = number }, nil end
  return nil, "Usage: [owner/repo] <number>"
end

---@param item GithubGhItem
---@return string
local function item_label(item)
  local prefix = item.kind == "pr" and "PR" or "Issue"
  local draft = item.is_draft and " draft" or ""
  return ("%s %s#%d%s %s (%d)"):format(
    prefix,
    item.repo,
    item.number,
    draft,
    item.title,
    tonumber(item.comments_count) or 0
  )
end

---@param item GithubGhItem
local function open_item(item)
  if item.kind == "pr" then
    require("diff_review").open_pr_number(item.number, {
      repo = item.repo,
      cwd = vim.fn.getcwd(),
    })
    return
  end
  require("github.issue_view").open({
    kind = item.kind,
    repo = item.repo,
    number = item.number,
    cwd = vim.fn.getcwd(),
    item = item,
  })
end

---@param item GithubGhItem
---@param body string?
---@return string[]
local function issue_preview_lines(item, body)
  local preview_item = vim.deepcopy(item or {})
  preview_item.kind = preview_item.kind or "issue"
  if body ~= nil then preview_item.body = body end
  local rendered = require("github.issue_view").render_item(preview_item, { folds = {} })
  return rendered.lines
end

---@param ctx table
---@param title string
---@param lines string[]
local function set_preview_lines(ctx, title, lines)
  if not (ctx and ctx.preview) then return end
  pcall(function()
    if type(ctx.preview.set_title) == "function" then ctx.preview:set_title(title) end
    if type(ctx.preview.set_lines) == "function" then ctx.preview:set_lines(lines) end
    if type(ctx.preview.highlight) == "function" then ctx.preview:highlight({ lang = "markdown" }) end
  end)
end

---@param ctx table
local function preview_issue(ctx)
  local picker_item = ctx and ctx.item
  local item = picker_item and (picker_item.item or picker_item)
  if not (item and item.kind == "issue") then return end

  local title = "Issue #" .. tostring(item.number)
  issue_preview_request_id = issue_preview_request_id + 1
  local request_id = issue_preview_request_id
  set_preview_lines(ctx, title, issue_preview_lines(item, item.body ~= "" and item.body or "Loading description..."))

  issue_index.detail_async(vim.fn.getcwd(), item.repo, item.number, nil, function(result)
    if request_id ~= issue_preview_request_id then return end
    if not (result and result.ok and result.item) then
      local message = result and result.message or "GitHub issue preview failed"
      vim.notify("GitHub issue preview failed:\n" .. tostring(message), vim.log.levels.ERROR, { title = "GitHub" })
      set_preview_lines(ctx, title, issue_preview_lines(item, tostring(message)))
      return
    end
    set_preview_lines(ctx, title, issue_preview_lines(result.item, result.item.body))
  end)
end

---@param title string
---@param items GithubGhItem[]
---@param opts? { preview?: fun(ctx: table) }
local function open_picker(title, items, opts)
  if _G.Snacks and Snacks.picker and type(Snacks.picker.pick) == "function" then
    Snacks.picker.pick({
      title = title,
      items = vim.tbl_map(function(item)
        return {
          text = item_label(item),
          item = item,
        }
      end, items),
      format = function(item)
        return {
          { item.text, item.item.kind == "pr" and "Function" or "Identifier" },
        }
      end,
      confirm = function(picker, item)
        if picker and picker.close then picker:close() end
        if item and item.item then open_item(item.item) end
      end,
      preview = opts and opts.preview or nil,
    })
    return
  end

  vim.ui.select(items, {
    prompt = title,
    format_item = item_label,
  }, function(item)
    if item then open_item(item) end
  end)
end

---@param cwd string
---@param repo string
local function open_synced_issue_picker(cwd, repo)
  repo_cache.remember_cwd_repo(cwd, repo)
  issue_index.ensure_repo(cwd, repo, { manual = false })
  local items = issue_index.list(repo, { limit = 100 })
  if #items == 0 then
    vim.notify(
      "No synced GitHub issues found for " .. repo .. ". Run :GithubIssueSync to refresh.",
      vim.log.levels.INFO,
      { title = "GitHub" }
    )
  end
  open_picker("GitHub Issues: " .. repo, items, { preview = preview_issue })
end

---@param title string
---@param loader fun(cwd: string, callback: fun(result: GithubGhListResult))
local function load_and_pick(title, loader)
  local cwd = vim.fn.getcwd()
  vim.notify("Fetching " .. title .. "...", vim.log.levels.INFO, { title = "GitHub" })
  loader(cwd, function(result)
    if not result.ok then
      vim.notify(result.message or "GitHub search failed", vim.log.levels.ERROR, { title = "GitHub" })
      return
    end

    open_picker(title, result.items or {})
  end)
end

---@param args? string
function M.issues(args)
  local target, err = parse_open_args(args)
  if err then
    vim.notify("Usage: GithubIssue [owner/repo] <number>", vim.log.levels.ERROR, { title = "GitHub" })
    return
  end
  if target then
    require("github.issue_view").open({
      kind = "issue",
      repo = target.repo,
      number = target.number,
      cwd = vim.fn.getcwd(),
    })
    return
  end

  local cwd = vim.fn.getcwd()
  local repo = repo_cache.completion_repo(0, cwd)
  if repo then
    open_synced_issue_picker(cwd, repo)
    return
  end

  vim.notify("Resolving current GitHub repo...", vim.log.levels.INFO, { title = "GitHub" })
  gh.current_repo_async(cwd, function(result)
    if not (result and result.ok and result.repo) then
      vim.notify(
        "Could not resolve current GitHub repo:\n" .. tostring(result and result.message or "unknown error"),
        vim.log.levels.ERROR,
        { title = "GitHub" }
      )
      return
    end
    open_synced_issue_picker(cwd, result.repo)
  end)
end

---@param args? string
function M.prs(args)
  local target, err = parse_open_args(args)
  if err then
    vim.notify("Usage: GithubPR [owner/repo] <number>", vim.log.levels.ERROR, { title = "GitHub" })
    return
  end
  if target then
    require("diff_review").open_pr_number(target.number, {
      repo = target.repo,
      cwd = vim.fn.getcwd(),
    })
    return
  end

  load_and_pick("My Open GitHub PRs", gh.search_open_prs_async)
end

function M.reviews()
  load_and_pick("GitHub Review Requests", gh.search_review_requests_async)
end

return M
