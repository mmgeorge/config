local gh = require("github.gh")
local popup_window = require("forge.infra.popup_window")

local M = {}

---@param message string
---@param level integer
local function notify(message, level)
  vim.notify(message, level, { title = "ForgeGithubIssueCreate" })
end

---@param url string
---@return {hostname:string, owner:string, name:string}?, string?
local function parse_issue_url(url)
  local hostname, owner, name, number = url:match("^https?://([^/]+)/([^/]+)/([^/]+)/issues/(%d+)")
  if hostname and owner and name and number then
    return { hostname = hostname:lower(), owner = owner, name = name }, tonumber(number)
  end
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

    local repository, number = parse_issue_url(result.url)
    if not (repository and number) then
      notify("Created GitHub issue: " .. result.url, vim.log.levels.INFO)
      gh.open_url(result.url)
      return
    end

    require("github.issue_document").open({
      kind = "issue",
      repository = repository,
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
