-- Owns a host fixture's workspace and data directory under the OS temporary directory.
--
-- Fixtures that must keep their evidence after Neovim exits cannot use `vim.fn.tempname()`,
-- because Neovim deletes that directory on exit. Placing evidence inside the checkout instead
-- leaves nested Git repositories that every later status and checkpoint of the checkout reports.
-- This helper keeps evidence under `<os temp>/forge/`, removes it after a passing run, and
-- retains it after a failure so the reported diagnostics path stays readable.

---@class ForgeFixtureEvidence
---@field path string Absolute evidence directory owned by this fixture run.
---@field release fun(passed: boolean): string|nil Removes passing evidence; returns a removal failure.

---@param name string Fixture label used as the directory prefix.
---@return ForgeFixtureEvidence
return function(name)
  local root = vim.fs.joinpath(vim.uv.os_tmpdir(), "forge")
  local path = vim.fs.joinpath(root, ("%s-%d-%.0f"):format(name, vim.fn.getpid(), vim.uv.hrtime()))
  assert(vim.fn.mkdir(path, "p") == 1, "could not create fixture evidence directory " .. path)
  return {
    path = path,
    release = function(passed)
      if not passed then return nil end
      if vim.fn.delete(path, "rf") ~= 0 then return "could not remove fixture evidence " .. path end
      return nil
    end,
  }
end
