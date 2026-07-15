local gh = require("github.gh")
local popup_window = require("diff_review.infra.popup_window")

local M = {}

---@param message string
---@param level integer
local function notify(message, level)
  vim.notify(message, level, { title = "GithubIssueCreate" })
end

---@param url string
---@return string?, string?
local function parse_issue_url(url)
  local owner, repo, number = url:match("^https?://[^/]+/([^/]+)/([^/]+)/issues/(%d+)")
  if owner and repo and number then return owner .. "/" .. repo, number end
  return nil, nil
end

---@param cwd string
---@param title string
---@param body string
local function create_issue(cwd, title, body)
  notify("Creating GitHub issue...", vim.log.levels.INFO)
  gh.create_issue_async(cwd, title, body, nil, function(result)
    if not result.ok or not result.url then
      notify("Failed to create GitHub issue: " .. tostring(result.message or "unknown error"), vim.log.levels.ERROR)
      return
    end

    local repo, number = parse_issue_url(result.url)
    if not (repo and number) then
      notify("Created GitHub issue: " .. result.url, vim.log.levels.INFO)
      gh.open_url(result.url)
      return
    end

    require("github.issue_view").open({
      kind = "issue",
      repo = repo,
      number = number,
      cwd = cwd,
    })
  end)
end

---@param args? string
function M.open(args)
  local cwd = vim.fn.getcwd()
  local title = vim.trim(args or "")
  if title ~= "" then
    create_issue(cwd, title, "")
    return
  end

  popup_window.input({ prompt = "Issue title: " }, function(input)
    input = vim.trim(input or "")
    if input == "" then
      notify("Issue creation cancelled: title is required", vim.log.levels.WARN)
      return
    end

    popup_window.input({ prompt = "Issue body (optional): " }, function(body)
      if body == nil then
        notify("Issue creation cancelled", vim.log.levels.WARN)
        return
      end
      create_issue(cwd, input, body)
    end)
  end)
end

return M
