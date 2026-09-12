---@class RustSidecarExecutableResult
---@field ok boolean
---@field path? string
---@field message? string

---@class RustSidecarLease
---@field path string
---@field release fun() Removes the process-owned copy after exit.

---@class RustSidecarSpec
---@field crate_name string
---@field crate_dir fun(): string
---@field executable_name? string
---@field artifact_root? fun(): string
---@field binary_target? string
---@field locked? boolean
---@field profile? 'dev'|'release' Cargo profile, defaulting to release.

---@class RustSidecarBuilder
---@field crate_dir fun(): string
---@field manifest_path fun(): string
---@field artifact_root fun(): string
---@field binary_path fun(): string
---@field build_command fun(): string[]
---@field ensure fun(callback: fun(result: RustSidecarExecutableResult)) Checks only executable presence, never builds.
---@field acquire fun(path: string, callback: fun(lease?: RustSidecarLease, failure?: string)) Copies the executable asynchronously for one process.
---@field _set_crate_dir_for_test fun(crate_dir: string?)
---@field _set_artifact_root_for_test fun(artifact_root: string?)
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
  local profile = spec.profile or "release"
  assert(profile == "dev" or profile == "release", "Rust sidecar profile must be dev or release")
  local crate_dir_for_test = nil
  local artifact_root_for_test = nil
  ---@type RustSidecarBuilder
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

  function builder.artifact_root()
    if artifact_root_for_test then return artifact_root_for_test end
    if spec.artifact_root then return spec.artifact_root() end
    return vim.fs.joinpath(vim.fn.stdpath("cache"), "rust-sidecar", spec.crate_name)
  end

  function builder.binary_path()
    return vim.fs.joinpath(builder.artifact_root(), "build", profile == "dev" and "debug" or "release", executable_name())
  end

  function builder.build_command()
    local command = {
      "cargo",
      "build",
      "--manifest-path",
      builder.manifest_path(),
      "--target-dir",
      vim.fs.joinpath(builder.artifact_root(), "build"),
    }
    if profile == "release" then command[#command + 1] = "--release"
    else vim.list_extend(command, { "--profile", "dev" }) end
    if spec.binary_target then vim.list_extend(command, { "--bin", spec.binary_target }) end
    if spec.locked then command[#command + 1] = "--locked" end
    return command
  end

  function builder.ensure(callback)
    local path = builder.binary_path()
    local stat = vim.uv.fs_stat(path)
    if not stat or stat.type ~= "file" then
      callback({ ok = false, message = "Rust sidecar executable is missing: " .. path
        .. "\nBuild it manually before opening Forge: " .. table.concat(builder.build_command(), " ") })
      return
    end
    callback({ ok = true, path = path })
  end

  function builder.acquire(path, callback)
    local lease_root = vim.fs.joinpath(builder.artifact_root(), "leases", ("%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()))
    local created, failure = pcall(vim.fn.mkdir, lease_root, "p")
    if not created then callback(nil, tostring(failure)) return end
    local leased_path = vim.fs.joinpath(lease_root, executable_name())
    vim.uv.fs_copyfile(path, leased_path, vim.schedule_wrap(function(copy_error)
      if copy_error then
        vim.uv.fs_unlink(leased_path)
        vim.uv.fs_rmdir(lease_root)
        callback(nil, "Failed to copy Rust sidecar executable: " .. tostring(copy_error))
        return
      end
      local released = false
      callback({ path = leased_path, release = function()
        if released then return end
        local removed, remove_error = vim.uv.fs_unlink(leased_path)
        if not removed then error("Failed to release executable copy: " .. tostring(remove_error)) end
        local removed_root, root_error = vim.uv.fs_rmdir(lease_root)
        if not removed_root then error("Failed to remove executable lease directory: " .. tostring(root_error)) end
        released = true
      end })
    end))
  end

  function builder._set_crate_dir_for_test(crate_dir) crate_dir_for_test = crate_dir end
  function builder._set_artifact_root_for_test(artifact_root) artifact_root_for_test = artifact_root end
  function builder._reset_for_test()
    crate_dir_for_test, artifact_root_for_test = nil, nil
  end

  return builder
end

return M
