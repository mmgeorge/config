vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local succeeded, failure = xpcall(function()
  vim.g.forge_build_profile = nil
  package.loaded["forge.builder"] = nil
  local default_builder = require("forge.builder")
  assert(not vim.tbl_contains(default_builder.build_command(), "--release"))
  assert(default_builder.binary_path():find("/debug/", 1, true), "default must use the debug binary")
  for _, profile in ipairs({ "dev", "release" }) do
    vim.g.forge_build_profile = profile
    package.loaded["forge.builder"] = nil
    local builder = require("forge.builder")
    local command = builder.build_command()
    assert(vim.tbl_contains(command, profile == "dev" and "dev" or "--release"))
    assert(vim.tbl_contains(command, "--locked"))
    assert(builder.binary_path():find(profile == "dev" and "/debug/" or "/release/", 1, true))
    local development = builder.for_profile("dev")
    local release = builder.for_profile("release")
    assert(development ~= release, "profile builds share mutable build state")
    assert(development.artifact_root() == release.artifact_root(), "profiles use different Cargo target roots")
    assert(not vim.tbl_contains(development.build_command(), "--release"))
    assert(vim.tbl_contains(release.build_command(), "--release"))
    assert(not pcall(builder.for_profile, "invalid"), "invalid explicit profile accepted")
  end
  vim.g.forge_build_profile = "invalid"
  package.loaded["forge.builder"] = nil
  assert(not pcall(require, "forge.builder"), "invalid profile accepted")
end, debug.traceback)
vim.g.forge_build_profile = nil
package.loaded["forge.builder"] = nil
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("manual build profile passed")
vim.cmd("qa!")
