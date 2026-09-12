---@class GithubRepoUsersFetchOptions
---@field cwd string?
---@field repo string
---@field ttl_seconds? integer
---@field callback fun(result: { ok: boolean, contributors?: table[], metadata?: table, message?: string })

local M = {}
local runner_for_test

---@param opts GithubRepoUsersFetchOptions
function M.fetch_async(opts)
  local cache = require("github.repo_cache")
  local hostname = cache.hostname()
  local directory = cache.repo_dir(opts.repo)
  local owner, name = opts.repo:match("^([^/]+)/([^/]+)$")
  if not owner then opts.callback({ ok = false, message = "Invalid GitHub repository" }) return end
  local params = {
    cache_directory = directory,
    directory = opts.cwd or vim.fn.getcwd(),
    request = { repository = { hostname = hostname, owner = owner, name = name }, ttl_seconds = opts.ttl_seconds },
  }
  local completed = false
  local function finish(metadata, failure)
    if completed then return end
    completed = true
    if failure then opts.callback({ ok = false, message = tostring(failure) }) return end
    if cache.hostname() ~= hostname or cache.repo_dir(opts.repo) ~= directory then
      opts.callback({ ok = false, message = "GitHub metadata cache context changed" })
      return
    end
    if type(metadata) ~= "table" or type(metadata.repo) ~= "string" or metadata.repo:lower() ~= opts.repo:lower()
      or (metadata.hostname ~= nil and metadata.hostname ~= hostname)
      or type(metadata.fetched_at) ~= "number" or metadata.fetched_at < 0 or metadata.fetched_at % 1 ~= 0
      or type(metadata.contributors) ~= "table" then
      opts.callback({ ok = false, message = "Forge returned invalid repository metadata" })
      return
    end
    opts.callback({ ok = true, contributors = metadata.contributors, metadata = metadata })
  end
  local succeeded, failure = pcall(function()
    if runner_for_test then runner_for_test(params, finish)
    else require("forge.client").request_host("github.metadata", params, finish) end
  end)
  if not succeeded then finish(nil, failure) end
end

---@param runner? fun(params: table, callback: fun(metadata: table?, failure: string?))
function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
