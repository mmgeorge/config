-- Git fixture repositories for integration cases. `create` initializes a repository with a fixed
-- identity, optionally writes and commits files, and returns a runner that fails the case on a
-- nonzero exit. The process timeout bounds a hung Git command.
--
--   local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
--   local repository = git_repository.create({ files = { ["README.md"] = { "# Fixture" } } })
--   repository.run({ "checkout", "-b", "feature" })
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")

local git_repository = {}

local command_timeout_ms = 30000

---Runs Git in `path` and returns its standard output, failing on a nonzero exit.
---@param path string
---@param arguments string[]
---@param options? { text?: boolean } `text = false` returns the output's bytes, carriage returns included
---@return string stdout
function git_repository.run(path, arguments, options)
  local text = not (options and options.text == false)
  local command = vim.list_extend({ "git", "-C", path }, arguments)
  local result = vim.system(command, { text = text, timeout = command_timeout_ms }):wait()
  assert(result.code == 0, ("git %s failed (%s): %s"):format(table.concat(arguments, " "), tostring(result.code), result.stderr))
  return result.stdout
end

---@class GitRepositoryOptions
---@field path? string repository directory; a temporary directory by default
---@field branch? string initial branch; defaults to "main"
---@field bare? boolean create a bare repository
---@field files? table<string, string[]> relative paths and their lines, committed as the first commit
---@field message? string first commit message; defaults to "initial"

---@class GitRepository
---@field path string
---@field run fun(arguments: string[], options?: { text?: boolean }): string

---@param options? GitRepositoryOptions
---@return GitRepository
function git_repository.create(options)
  options = options or {}
  local path = options.path or temp.directory("repository")
  vim.fn.mkdir(path, "p")
  local init = { "init", "--quiet", "-b", options.branch or "main" }
  if options.bare then init[#init + 1] = "--bare" end
  git_repository.run(path, init)
  if not options.bare then
    git_repository.run(path, { "config", "user.name", "Forge Test" })
    git_repository.run(path, { "config", "user.email", "forge-test@example.invalid" })
    git_repository.run(path, { "config", "commit.gpgsign", "false" })
  end
  if options.files then
    for relative, lines in pairs(options.files) do
      local file = path .. "/" .. relative
      vim.fn.mkdir(vim.fs.dirname(file), "p")
      assert(vim.fn.writefile(lines, file) == 0, "could not write " .. file)
    end
    git_repository.run(path, { "add", "--all" })
    git_repository.run(path, { "commit", "--quiet", "-m", options.message or "initial" })
  end
  return { path = path, run = function(arguments, run_options) return git_repository.run(path, arguments, run_options) end }
end

return git_repository
