local M = {}

---@class GithubIssueIndexBuildResult
---@field ok boolean
---@field path? string
---@field message? string

---@alias GithubIssueIndexBuildCallback fun(result: GithubIssueIndexBuildResult)

---@type GithubIssueIndexBuildCallback[]
local pending_callbacks = {}
local build_in_flight = false
local runner_for_test = nil
local crate_dir_for_test = nil

local function executable_name()
  return vim.fn.has("win32") == 1 and "github-issue-index.exe" or "github-issue-index"
end

---@return string
function M.crate_dir()
  return crate_dir_for_test or vim.fs.joinpath(vim.fn.stdpath("config"), "rust", "github-issue-index")
end

---@return string
function M.manifest_path()
  return vim.fs.joinpath(M.crate_dir(), "Cargo.toml")
end

---@return string
function M.release_binary_path()
  return vim.fs.joinpath(M.crate_dir(), "target", "release", executable_name())
end

---@return string[]
function M.build_command()
  return { "cargo", "build", "--manifest-path", M.manifest_path(), "--release" }
end

---@param path string
---@return integer?
local function mtime_ns(path)
  local stat = vim.uv.fs_stat(path)
  if not stat then return nil end
  local mtime = stat.mtime or {}
  return (mtime.sec or 0) * 1000000000 + (mtime.nsec or 0)
end

---@return string[]
local function source_paths()
  local crate_dir = M.crate_dir()
  return {
    vim.fs.joinpath(crate_dir, "Cargo.toml"),
    vim.fs.joinpath(crate_dir, "Cargo.lock"),
    vim.fs.joinpath(crate_dir, "src", "main.rs"),
  }
end

---@return boolean
function M.needs_build()
  local binary_mtime = mtime_ns(M.release_binary_path())
  if not binary_mtime then return true end

  for _, path in ipairs(source_paths()) do
    local source_mtime = mtime_ns(path)
    if source_mtime and source_mtime > binary_mtime then return true end
  end

  return false
end

---@param result GithubIssueIndexBuildResult
local function finish(result)
  build_in_flight = false
  local callbacks = pending_callbacks
  pending_callbacks = {}
  for _, callback in ipairs(callbacks) do
    callback(result)
  end
end

---@param result table
---@return GithubIssueIndexBuildResult
local function build_result_from_system(result)
  local stdout = result.stdout or ""
  local stderr = result.stderr or ""
  local output = vim.trim(stdout .. (stdout ~= "" and stderr ~= "" and "\n" or "") .. stderr)
  if result.code ~= 0 then
    return {
      ok = false,
      message = "GitHub issue indexer auto-build failed:\n" .. (output ~= "" and output or ("cargo exited " .. tostring(result.code))),
    }
  end

  local binary = M.release_binary_path()
  if not vim.uv.fs_stat(binary) then
    return {
      ok = false,
      message = "GitHub issue indexer auto-build completed but did not create " .. binary,
    }
  end

  return { ok = true, path = binary }
end

---@param callback fun(result: table)
local function start_build(callback)
  if not vim.uv.fs_stat(M.manifest_path()) then
    callback({
      code = -1,
      stdout = "",
      stderr = "missing Cargo manifest: " .. M.manifest_path(),
    })
    return
  end

  if runner_for_test then
    runner_for_test(M.build_command(), { cwd = M.crate_dir() }, callback)
    return
  end

  if vim.fn.executable("cargo") ~= 1 then
    callback({
      code = -1,
      stdout = "",
      stderr = "cargo executable not found; install Rust or build nvim/rust/github-issue-index manually",
    })
    return
  end

  local ok, process_or_error = pcall(vim.system, M.build_command(), {
    text = true,
    cwd = M.crate_dir(),
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      callback({
        code = result.code or 0,
        stdout = result.stdout or "",
        stderr = result.stderr or "",
      })
    end)
  end)

  if not ok then
    callback({ code = -1, stdout = "", stderr = tostring(process_or_error) })
  end
end

---@param callback GithubIssueIndexBuildCallback
function M.ensure(callback)
  if not M.needs_build() then
    vim.schedule(function()
      callback({ ok = true, path = M.release_binary_path() })
    end)
    return
  end

  pending_callbacks[#pending_callbacks + 1] = callback
  if build_in_flight then return end
  build_in_flight = true

  start_build(function(result)
    finish(build_result_from_system(result))
  end)
end

function M._set_runner_for_test(runner)
  runner_for_test = runner
end

function M._set_crate_dir_for_test(crate_dir)
  crate_dir_for_test = crate_dir
end

function M._reset_for_test()
  pending_callbacks = {}
  build_in_flight = false
  runner_for_test = nil
  crate_dir_for_test = nil
end

return M
