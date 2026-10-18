vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.g.forge_build_profile = "release"
local runtime_builder = require("forge.builder")
local builder = runtime_builder.for_profile("dev")
local release_builder = runtime_builder.for_profile("release")
local original_build, original_release_build = builder.build, release_builder.build
local original_notify = vim.notify
---@type fun(result: RustSidecarExecutableResult)?
local build_callback
---@type fun(result: RustSidecarExecutableResult)?
local release_callback
---@type { message: string, level: integer, options: table }[]
local notification = {}

local succeeded, failure = xpcall(function()
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  plugin.init()
  assert(vim.fn.exists(":ForgeBuild") == 2, "build command requires the Forge plugin to load")
  assert(vim.fn.exists(":ForgeBuildRelease") == 2, "release build command requires the Forge plugin to load")
  builder.build = function(callback) build_callback = callback end
  release_builder.build = function(callback) release_callback = callback end
  vim.notify = function(message, level, options)
    notification[#notification + 1] = { message = message, level = level, options = options }
  end

  vim.cmd("ForgeBuild")
  assert(build_callback and package.loaded["forge"] == nil, "build command loaded the host facade")
  build_callback({ ok = false, message = "fixture compiler error" })
  assert(notification[1].message == "fixture compiler error" and notification[1].level == vim.log.levels.ERROR)
  assert(notification[1].options.title == "ForgeBuild")

  build_callback = nil
  vim.cmd("ForgeBuild")
  assert(build_callback, "failed build prevented a later explicit build")
  build_callback({ ok = true, path = builder.binary_path() })
  assert(notification[2].message:find(builder.binary_path(), 1, true))
  assert(notification[2].message:find("Restart Neovim", 1, true))
  assert(notification[2].message:find('forge_build_profile = "dev"', 1, true),
    "development build did not explain the explicit release runtime selection")
  assert(notification[2].level == vim.log.levels.INFO and package.loaded["forge"] == nil)

  build_callback = nil
  vim.cmd("ForgeBuildRelease")
  assert(release_callback and not build_callback, "release command selected the development builder")
  vim.cmd("ForgeBuild")
  assert(build_callback, "release request prevented an independent development build")
  release_callback({ ok = true, path = release_builder.binary_path() })
  build_callback({ ok = true, path = builder.binary_path() })
  assert(notification[3].options.title == "ForgeBuildRelease")
  assert(notification[3].message:find(release_builder.binary_path(), 1, true))
  assert(notification[4].options.title == "ForgeBuild")
  assert(notification[4].message:find(builder.binary_path(), 1, true))
  assert(vim.g.forge_build_profile == "release", "building changed the runtime profile")

  vim.g.forge_build_profile = nil
  vim.cmd("ForgeBuildRelease")
  release_callback({ ok = true, path = release_builder.binary_path() })
  assert(notification[5].message:find('forge_build_profile = "release"', 1, true),
    "release build implied that restarting alone would select the release executable")

  vim.cmd("ForgeBuildRelease")
  release_callback({ ok = false, message = "fixture release compiler error" })
  assert(notification[6].message == "fixture release compiler error")
  assert(notification[6].level == vim.log.levels.ERROR and notification[6].options.title == "ForgeBuildRelease")
end, debug.traceback)

builder.build, release_builder.build = original_build, original_release_build
vim.g.forge_build_profile = nil
vim.notify = original_notify
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("ForgeBuild command availability and build diagnostics passed")
vim.cmd("qa!")
