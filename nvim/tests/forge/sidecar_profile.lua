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
    assert(not vim.tbl_contains(command, "--target-dir"), "build overrides Cargo's default target directory")
    assert(vim.tbl_contains(command, profile == "dev" and "dev" or "--release"))
    assert(vim.tbl_contains(command, "--locked"))
    local executable = vim.fn.has("win32") == 1 and "forge.exe" or "forge"
    assert(builder.binary_path() == vim.fs.joinpath(builder.crate_dir(), "target",
      profile == "dev" and "debug" or "release", executable), "runtime does not use the crate's target directory")
    local development = builder.for_profile("dev")
    local release = builder.for_profile("release")
    assert(development ~= release, "profile builds share mutable build state")
    assert(development.artifact_root() == release.artifact_root(), "profiles use different runtime cache roots")
    local binary_path = builder.binary_path()
    builder._set_artifact_root_for_test(vim.fn.tempname())
    assert(builder.binary_path() == binary_path, "runtime cache changes redirected Cargo output")
    builder._reset_for_test()
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
