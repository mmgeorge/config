vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local builder = require("forge.builder")
local original_build = builder.build
local original_notify = vim.notify
---@type fun(result: RustSidecarExecutableResult)?
local build_callback
---@type { message: string, level: integer, options: table }[]
local notification = {}

local succeeded, failure = xpcall(function()
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  plugin.init()
  assert(vim.fn.exists(":ForgeBuild") == 2, "build command requires the Forge plugin to load")
  builder.build = function(callback) build_callback = callback end
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
  assert(notification[2].level == vim.log.levels.INFO and package.loaded["forge"] == nil)
end, debug.traceback)

builder.build = original_build
vim.notify = original_notify
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("ForgeBuild command availability and build diagnostics passed")
vim.cmd("qa!")
