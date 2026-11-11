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

-- Bounds the wait for the host's child processes to release the evidence directory.
local removal_timeout_ms = 2000

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
      -- The host's own exit does not end the processes it launched in the workspace, such as a
      -- Git command behind a checkpoint. Windows refuses to remove a directory that a process
      -- still holds as its working directory, so removal is repeated until those processes exit.
      local removed = vim.wait(removal_timeout_ms, function()
        return vim.fn.delete(path, "rf") == 0 or vim.uv.fs_stat(path) == nil
      end, 20)
      if not removed then return "could not remove fixture evidence " .. path end
      return nil
    end,
  }
end
