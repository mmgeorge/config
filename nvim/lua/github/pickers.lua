local gh = require("github.gh")

local M = {}

---@param item GithubGhItem
---@return string
local function item_label(item)
  local prefix = item.kind == "pr" and "PR" or "Issue"
  local draft = item.is_draft and " draft" or ""
  return ("%s %s#%d%s %s (%d)"):format(prefix, item.repo, item.number, draft, item.title, item.comments_count)
end

---@param item GithubGhItem
local function open_item(item)
  require("github.issue_view").open({
    kind = item.kind,
    repo = item.repo,
    number = item.number,
    cwd = vim.fn.getcwd(),
  })
end

---@param title string
---@param items GithubGhItem[]
local function open_picker(title, items)
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

function M.issues()
  load_and_pick("GitHub Issues", gh.search_issues_async)
end

function M.prs()
  load_and_pick("My Open GitHub PRs", gh.search_open_prs_async)
end

function M.reviews()
  load_and_pick("GitHub Review Requests", gh.search_review_requests_async)
end

return M
