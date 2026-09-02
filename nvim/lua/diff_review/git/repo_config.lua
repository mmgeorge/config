local config = require("diff_review.infra.config")

--- Reads per-repository config from `<repo root>/.diffreview.json`, currently only
--- `branch_prefix` (used by the `bc` branch-create action), behind a pluggable reader
--- test seam so tests never touch the filesystem.
---@class DiffReviewRepoConfigModule
---@field reader? fun(path: string): string? test seam returning file contents
local M = { reader = nil }

--- Installs a custom file reading function for test isolation.
---@param fn (fun(path: string): string?)? Custom file reader function.
function M.set_reader(fn)
  M.reader = fn
end

--- Clears the custom file reader override and restores direct filesystem access.
function M.reset_reader()
  M.reader = nil
end

--- Reads and parses the `.diffreview.json` repository configuration file.
---@param cwd string Repository root directory path.
---@return DiffReviewRepoConfig config Decoded repository configuration table.
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

--- Resolves the branch name prefix for a repository, falling back to global options.
---@param cwd string Repository root directory path.
---@return string prefix Branch name prefix string.
function M.branch_prefix(cwd)
  local repo = M.read(cwd)
  if type(repo.branch_prefix) == "string" and repo.branch_prefix ~= "" then
    return repo.branch_prefix
  end
  return config.options.branch_prefix or config.defaults.branch_prefix or ""
end

return M
