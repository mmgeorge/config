vim.loader.enable(false)

local cache = require("github.repo_cache")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local cache_root = vim.fn.tempname()
local cwd = "D:/github-cache-test-root"
local repo = "Owner/Repo"
local normalized_repo = "Owner/Repo"

local function run()
  cache.set_data_dir_for_test(cache_root)
  cache.remember_cwd_repo(cwd, normalized_repo)
  cache.set_base_branch(cwd, "master")

  local metadata_error = cache.write_metadata(normalized_repo, {
    { login = "bobtown" },
    { login = "alice-dev", name = "Alice Developer" },
    { login = "Alice-Dev", name = "Duplicate Alice" },
  })
  assert_true(metadata_error == nil, "metadata write failed: " .. tostring(metadata_error))

  local contributors = cache.contributors(normalized_repo)
  assert_true(#contributors == 2, "contributors were not deduplicated: " .. vim.inspect(contributors))
  assert_true(contributors[1].login == "alice-dev", "contributors were not sorted by login: " .. vim.inspect(contributors))
  assert_true(contributors[2].login == "bobtown", "contributors were not sorted by login: " .. vim.inspect(contributors))
  assert_true(cache.metadata_fresh(normalized_repo), "metadata should be fresh immediately after write")
  assert_true(cache.repo_for_cwd(cwd) == normalized_repo, "cwd repo mapping was not stored")
  assert_true(cache.get_base_branch(cwd) == "master", "base branch was not stored in cwd cache")

  local review_path = cache.review_path(normalized_repo, 2)
  vim.fn.mkdir(vim.fs.dirname(review_path), "p")
  vim.fn.writefile({ "{}" }, review_path)
  assert_true(vim.uv.fs_stat(review_path) ~= nil, "review file was not created")

  local deleted = cache.delete_current(nil, cwd)
  assert_true(deleted >= 2, "delete_current did not remove mapped repo and cwd cache")
  assert_true(#cache.contributors(normalized_repo) == 0, "contributors were not deleted with repo cache")
  assert_true(cache.repo_for_cwd(cwd) == nil, "cwd repo mapping was not deleted")
  assert_true(cache.get_base_branch(cwd) == nil, "base branch was not deleted with cwd cache")
  assert_true(vim.uv.fs_stat(review_path) == nil, "review draft was not deleted with repo cache")
end

local ok, err = xpcall(run, debug.traceback)
cache.set_data_dir_for_test(nil)
vim.fn.delete(cache_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
