local M = {}

local metadata_ttl_seconds = 10 * 60
local data_dir_for_test = nil
local in_flight = {}

---@param value string|number?
---@return string
local function path_segment(value)
  local text = vim.trim(tostring(value or ""))
  if text == "" then text = "current" end
  text = text:gsub("[<>:\"/\\|?*%c]", "-"):gsub("^%.+$", "-")
  return text
end

---@param cwd string?
---@return string
local function cwd_key(cwd)
  local key = vim.fs.normalize(cwd or vim.fn.getcwd())
  if vim.fn.has("win32") == 1 then key = key:lower() end
  return vim.fn.sha256(key)
end

---@param repo string?
---@return string?
local function normalize_repo(repo)
  repo = vim.trim(tostring(repo or ""))
  if repo == "" then return nil end
  repo = repo:gsub("^https://[^/]+/", ""):gsub("%.git$", "")
  local owner, name = repo:match("^([^/]+)/([^/]+)$")
  if not owner then return nil end
  return owner .. "/" .. name
end

---@param path string
---@return table?
local function read_json(path)
  if vim.uv.fs_stat(path) == nil then return nil end
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return nil end
  local decoded_ok, decoded = pcall(vim.json.decode, table.concat(lines, "\n"))
  if decoded_ok and type(decoded) == "table" then return decoded end
  return nil
end

---@param path string
---@param payload table
---@return string? error
local function write_json(path, payload)
  local directory = vim.fs.dirname(path)
  local mkdir_ok, mkdir_result = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok or mkdir_result == 0 then return "Could not create cache directory: " .. directory end
  local encode_ok, encoded = pcall(vim.json.encode, payload)
  if not encode_ok then return "Could not encode cache JSON" end
  local write_ok, write_result = pcall(vim.fn.writefile, { encoded }, path)
  if not write_ok or write_result ~= 0 then return "Could not write cache file: " .. path end
  return nil
end

---@param path string
---@return boolean
local function delete_dir(path)
  if vim.uv.fs_stat(path) == nil then return false end
  vim.fn.delete(path, "rf")
  return true
end

---@param path string
---@return boolean
local function delete_file(path)
  if vim.uv.fs_stat(path) == nil then return false end
  return vim.fn.delete(path) == 0
end

---@param path string?
function M.set_data_dir_for_test(path)
  data_dir_for_test = path
end

---@return string
function M.base_dir()
  return data_dir_for_test or vim.fs.joinpath(vim.fn.stdpath("data"), "gitstatus")
end

---@param repo string
---@return string
function M.repo_dir(repo)
  local normalized = normalize_repo(repo) or "unknown/current"
  local parts = { M.base_dir(), "repos" }
  for _, segment in ipairs(vim.split(normalized, "/", { plain = true, trimempty = true })) do
    parts[#parts + 1] = path_segment(segment)
  end
  return vim.fs.joinpath(unpack(parts))
end

---@param cwd string?
---@return string
function M.cwd_dir(cwd)
  return vim.fs.joinpath(M.base_dir(), "cwd", cwd_key(cwd))
end

---@param cwd string?
---@return string
function M.cwd_repo_path(cwd)
  return vim.fs.joinpath(M.cwd_dir(cwd), "repo.json")
end

---@param cwd string?
---@return string?
function M.repo_for_cwd(cwd)
  local decoded = read_json(M.cwd_repo_path(cwd))
  return decoded and normalize_repo(decoded.repo) or nil
end

---@param cwd string?
---@param repo string?
function M.remember_cwd_repo(cwd, repo)
  local normalized = normalize_repo(repo)
  if not normalized then return end
  write_json(M.cwd_repo_path(cwd), {
    repo = normalized,
    updated_at = os.time(),
  })
end

---@param cwd string?
---@param repo? string
---@return boolean
function M.clear_cwd_repo(cwd, repo)
  local mapped_repo = M.repo_for_cwd(cwd)
  if not mapped_repo then return false end
  local normalized = normalize_repo(repo)
  if normalized and mapped_repo ~= normalized then return false end
  return delete_file(M.cwd_repo_path(cwd))
end

---@param cwd string?
---@return string
function M.open_pr_path(cwd)
  return vim.fs.joinpath(M.cwd_dir(cwd), "open-pr.json")
end

---@param cwd string?
---@return string?
function M.get_base_branch(cwd)
  local decoded = read_json(M.open_pr_path(cwd))
  local branch = decoded and decoded.base_branch
  if type(branch) == "string" and branch ~= "" then return branch end
  return nil
end

---@param cwd string?
---@param branch string
---@return string?
function M.set_base_branch(cwd, branch)
  return write_json(M.open_pr_path(cwd), {
    base_branch = branch,
    updated_at = os.time(),
  })
end

---@return string
function M.reviews_root()
  return vim.fs.joinpath(M.base_dir(), "repos")
end

---@param repo string
---@param number string|number
---@return string
function M.review_path(repo, number)
  return vim.fs.joinpath(M.repo_dir(repo), "reviews", path_segment(number), "review.json")
end

---@param repo string
---@return string
function M.metadata_path(repo)
  return vim.fs.joinpath(M.repo_dir(repo), "metadata.json")
end

---@param repo string?
---@return table?
function M.read_metadata(repo)
  local normalized = normalize_repo(repo)
  if not normalized then return nil end
  return read_json(M.metadata_path(normalized))
end

---@param repo string?
---@return table[]
function M.contributors(repo)
  local metadata = M.read_metadata(repo)
  local contributors = metadata and metadata.contributors or nil
  if type(contributors) ~= "table" then return {} end
  return contributors
end

---@param repo string?
---@param ttl_seconds? integer
---@return boolean
function M.metadata_fresh(repo, ttl_seconds)
  local metadata = M.read_metadata(repo)
  local fetched_at = tonumber(metadata and metadata.fetched_at)
  if not fetched_at then return false end
  return os.time() - fetched_at < (ttl_seconds or metadata_ttl_seconds)
end

---@param repo string
---@param contributors table[]
---@return string?
function M.write_metadata(repo, contributors)
  local normalized = normalize_repo(repo)
  if not normalized then return "Invalid GitHub repo" end
  local seen = {}
  local normalized_contributors = {}
  for _, contributor in ipairs(contributors or {}) do
    local login = type(contributor) == "table" and contributor.login or contributor
    login = vim.trim(tostring(login or ""))
    local key = login:lower()
    if login ~= "" and not seen[key] then
      seen[key] = true
      normalized_contributors[#normalized_contributors + 1] = {
        login = login,
        name = type(contributor) == "table" and contributor.name or nil,
      }
    end
  end
  table.sort(normalized_contributors, function(left, right)
    return left.login:lower() < right.login:lower()
  end)
  return write_json(M.metadata_path(normalized), {
    repo = normalized,
    fetched_at = os.time(),
    contributors = normalized_contributors,
  })
end

---@param cwd string?
---@param repo string?
---@param fetch fun(done: fun(result: { ok: boolean, contributors?: table[], message?: string }))
---@param opts? { ttl_seconds?: integer, remember_cwd?: boolean }
function M.ensure_metadata(cwd, repo, fetch, opts)
  local normalized = normalize_repo(repo)
  if not normalized then return end
  if M.metadata_fresh(normalized, opts and opts.ttl_seconds or nil) then return end
  if in_flight[normalized] then return end
  in_flight[normalized] = true
  fetch(function(result)
    in_flight[normalized] = nil
    if result and result.ok then
      M.write_metadata(normalized, result.contributors or {})
      if cwd and not (opts and opts.remember_cwd == false) then M.remember_cwd_repo(cwd, normalized) end
    end
  end)
end

---@param cwd string?
---@param fetch_repo fun(done: fun(result: { ok: boolean, repo?: string, message?: string }))
---@param fetch_contributors fun(repo: string, done: fun(result: { ok: boolean, contributors?: table[], message?: string }))
---@param opts? { ttl_seconds?: integer }
function M.ensure_metadata_for_cwd(cwd, fetch_repo, fetch_contributors, opts)
  local cached_repo = M.repo_for_cwd(cwd)
  local metadata_opts = vim.tbl_extend("force", opts or {}, { remember_cwd = true })
  if cached_repo then
    M.ensure_metadata(cwd, cached_repo, function(done)
      fetch_contributors(cached_repo, done)
    end, metadata_opts)
    return
  end
  fetch_repo(function(result)
    local repo = result and normalize_repo(result.repo)
    if not (result and result.ok and repo) then return end
    M.remember_cwd_repo(cwd, repo)
    M.ensure_metadata(cwd, repo, function(done)
      fetch_contributors(repo, done)
    end, metadata_opts)
  end)
end

---@param repo string?
---@return boolean
function M.delete_repo(repo)
  local normalized = normalize_repo(repo)
  if not normalized then return false end
  return delete_dir(M.repo_dir(normalized))
end

---@param cwd string?
---@return boolean
function M.delete_cwd(cwd)
  return delete_dir(M.cwd_dir(cwd))
end

---@param repo string?
---@param cwd string?
---@return integer deleted_count
function M.delete_current(repo, cwd)
  local deleted = 0
  if M.delete_repo(repo) then deleted = deleted + 1 end
  local mapped_repo = M.repo_for_cwd(cwd)
  if mapped_repo and mapped_repo ~= normalize_repo(repo) and M.delete_repo(mapped_repo) then deleted = deleted + 1 end
  if M.delete_cwd(cwd) then deleted = deleted + 1 end
  return deleted
end

---@param buf integer?
---@return string?
function M.buffer_repo(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ok, repo = pcall(function() return vim.b[buf].github_repo end)
  return ok and normalize_repo(repo) or nil
end

---@param buf integer?
---@param repo? string
function M.enable_user_completion(buf, repo)
  buf = buf or vim.api.nvim_get_current_buf()
  if repo and repo ~= "" then vim.b[buf].github_repo = repo end
  vim.b[buf].github_user_completion = true
  vim.b[buf].diff_review_pr_reviewer_completion = true
  local ok, issue_index = pcall(require, "github.issue_index")
  if ok then issue_index.ensure_for_buffer(buf, repo) end
end

---@param buf integer?
---@return boolean
function M.user_completion_enabled(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ok, enabled = pcall(function()
    return vim.b[buf].github_user_completion or vim.b[buf].diff_review_pr_reviewer_completion
  end)
  return ok and enabled == true
end

---@param buf integer?
---@param cwd? string
---@return string?
function M.completion_repo(buf, cwd)
  return M.buffer_repo(buf) or M.repo_for_cwd(cwd or vim.fn.getcwd())
end

return M
