local M = {}

---@return string
function M.path()
  return vim.fs.joinpath(vim.fn.stdpath("cache"), "rust-sidecar", "forge", "git-config-location.json")
end

---Removes the persisted location. A fresh Neovim host discovers the next location.
function M.reset()
  local removed, failure, code = vim.uv.fs_unlink(M.path())
  if not removed and code ~= "ENOENT" then
    vim.notify("Cannot reset Git configuration location: " .. tostring(failure), vim.log.levels.ERROR, { title = "Forge" })
    return
  end
  vim.notify("Git configuration location cache cleared. Restart Neovim to rediscover Git.", vim.log.levels.INFO, { title = "Forge" })
end

return M
