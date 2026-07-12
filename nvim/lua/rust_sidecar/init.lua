---@class RustSidecarBuildResult
---@field ok boolean
---@field path? string
---@field message? string

---@class RustSidecarSpec
---@field crate_name string
---@field crate_dir fun(): string
---@field executable_name? string

---@class RustSidecarBuilder
---@field crate_dir fun(): string
---@field manifest_path fun(): string
---@field release_binary_path fun(): string
---@field build_command fun(): string[]
---@field needs_build fun(): boolean
---@field ensure fun(callback: fun(result: RustSidecarBuildResult))
---@field _set_runner_for_test fun(runner: function?)
---@field _set_crate_dir_for_test fun(crate_dir: string?)
---@field _reset_for_test fun()

local M = {}

---@param source_path string
---@return string
function M.runtime_root(source_path)
  local directory = vim.fs.dirname(source_path)
  while directory and vim.fs.dirname(directory) ~= directory do
    if vim.fs.basename(directory) == "lua" then return vim.fs.dirname(directory) end
    directory = vim.fs.dirname(directory)
  end
  error("Rust sidecar source is not under a Neovim runtime lua directory: " .. source_path)
end

---@param spec RustSidecarSpec
---@return RustSidecarBuilder
function M.new(spec)
  assert(type(spec) == "table" and type(spec.crate_name) == "string", "Rust sidecar requires crate_name")
  assert(type(spec.crate_dir) == "function", "Rust sidecar requires crate_dir")

  local pending_callback = {}
  local build_in_flight = false
  local runner_for_test = nil
  local crate_dir_for_test = nil
  local builder = {}

  local function executable_name()
    local name = spec.executable_name or spec.crate_name
    return vim.fn.has("win32") == 1 and (name .. ".exe") or name
  end

  function builder.crate_dir()
    return crate_dir_for_test or spec.crate_dir()
  end

  function builder.manifest_path()
    return vim.fs.joinpath(builder.crate_dir(), "Cargo.toml")
  end

  function builder.release_binary_path()
    return vim.fs.joinpath(builder.crate_dir(), "target", "release", executable_name())
  end

  function builder.build_command()
    return { "cargo", "build", "--manifest-path", builder.manifest_path(), "--release" }
  end

  ---@param path string
  ---@return integer?
  local function mtime_ns(path)
    local stat = vim.uv.fs_stat(path)
    if not stat then return nil end
    local mtime = stat.mtime or {}
    return (mtime.sec or 0) * 1000000000 + (mtime.nsec or 0)
  end

  ---@param directory string
  ---@param result string[]
  local function collect_source_paths(directory, result)
    local scanner = vim.uv.fs_scandir(directory)
    if not scanner then return end
    while true do
      local name, kind = vim.uv.fs_scandir_next(scanner)
      if not name then break end
      if name ~= "target" then
        local path = vim.fs.joinpath(directory, name)
        if kind == "directory" then
          collect_source_paths(path, result)
        elseif name == "Cargo.toml" or name == "Cargo.lock" or name:match("%.rs$") then
          result[#result + 1] = path
        end
      end
    end
  end

  function builder.needs_build()
    local binary_mtime = mtime_ns(builder.release_binary_path())
    if not binary_mtime then return true end
    local source_path = {}
    collect_source_paths(builder.crate_dir(), source_path)
    for _, path in ipairs(source_path) do
      local source_mtime = mtime_ns(path)
      if source_mtime and source_mtime > binary_mtime then return true end
    end
    return false
  end

  ---@param result RustSidecarBuildResult
  local function finish(result)
    build_in_flight = false
    local callback_list = pending_callback
    pending_callback = {}
    for _, callback in ipairs(callback_list) do callback(result) end
  end

  ---@param callback fun(result: table)
  local function start_build(callback)
    if not vim.uv.fs_stat(builder.manifest_path()) then
      callback({ code = -1, stdout = "", stderr = "missing Cargo manifest: " .. builder.manifest_path() })
      return
    end
    if runner_for_test then
      runner_for_test(builder.build_command(), { cwd = builder.crate_dir() }, callback)
      return
    end
    if vim.fn.executable("cargo") ~= 1 then
      callback({ code = -1, stdout = "", stderr = "cargo executable not found" })
      return
    end
    local ok, process_error = pcall(vim.system, builder.build_command(), {
      text = true,
      cwd = builder.crate_dir(),
      stdin = false,
      stdout = true,
      stderr = true,
    }, function(result)
      vim.schedule(function()
        callback({ code = result.code or 0, stdout = result.stdout or "", stderr = result.stderr or "" })
      end)
    end)
    if not ok then callback({ code = -1, stdout = "", stderr = tostring(process_error) }) end
  end

  ---@param result table
  ---@return RustSidecarBuildResult
  local function normalize_build_result(result)
    local stdout = result.stdout or ""
    local stderr = result.stderr or ""
    local output = vim.trim(stdout .. (stdout ~= "" and stderr ~= "" and "\n" or "") .. stderr)
    if result.code ~= 0 then
      return {
        ok = false,
        message = spec.crate_name .. " auto-build failed:\n" .. (output ~= "" and output or ("cargo exited " .. tostring(result.code))),
      }
    end
    local binary = builder.release_binary_path()
    if not vim.uv.fs_stat(binary) then
      return { ok = false, message = "cargo did not create " .. binary }
    end
    return { ok = true, path = binary }
  end

  function builder.ensure(callback)
    if not builder.needs_build() then
      vim.schedule(function() callback({ ok = true, path = builder.release_binary_path() }) end)
      return
    end
    pending_callback[#pending_callback + 1] = callback
    if build_in_flight then return end
    build_in_flight = true
    start_build(function(result) finish(normalize_build_result(result)) end)
  end

  function builder._set_runner_for_test(runner) runner_for_test = runner end
  function builder._set_crate_dir_for_test(crate_dir) crate_dir_for_test = crate_dir end
  function builder._reset_for_test()
    pending_callback = {}
    build_in_flight = false
    runner_for_test = nil
    crate_dir_for_test = nil
  end

  return builder
end

return M
