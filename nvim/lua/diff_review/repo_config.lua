local config = require("diff_review.config")

--- Reads per-repository config from `<repo root>/.diffreview.json`, currently only
--- `branch_prefix` (used by the `bc` branch-create action), behind a pluggable reader
--- test seam so tests never touch the filesystem.
---@class DiffReviewRepoConfigModule
---@field reader? fun(path: string): string? test seam returning file contents
local M = { reader = nil }

---@param fn fun(path: string): string? test seam returning file contents
function M.set_reader(fn)
  M.reader = fn
end

function M.reset_reader()
  M.reader = nil
end

---@param cwd string repo root
---@return DiffReviewRepoConfig
function M.read(cwd)
  local path = (cwd:gsub("[/\\]+$", "")) .. "/.diffreview.json"
  local content
  if M.reader then
    content = M.reader(path)
  else
    local handle = io.open(path, "r")
    if handle then
      content = handle:read("*a")
      handle:close()
    end
  end
  if not content or content == "" then return {} end
  local ok, decoded = pcall(vim.json.decode, content)
  if ok and type(decoded) == "table" then return decoded end
  return {}
end

---@param cwd string repo root
---@return string
function M.branch_prefix(cwd)
  local repo = M.read(cwd)
  if type(repo.branch_prefix) == "string" and repo.branch_prefix ~= "" then
    return repo.branch_prefix
  end
  return config.options.branch_prefix or config.defaults.branch_prefix or ""
end

return M
