-- Resolves a session's check storage under the host's versioned state directory.
-- The host writes check records to `<data_root>/state/v<VERSION>/<session>/checks`
-- (see `forge_harness::storage::state::directory`), and only this executable's
-- version exists inside an isolated fixture data directory.
local M = {}

---@param data string Neovim data directory the fixture substituted for stdpath("data").
---@param session_id string
---@param relative string|nil Path below the session's checks directory.
---@return string
function M.path(data, session_id, relative)
  local version_directory_list = vim.fn.glob(data .. "/forge/harness/state/v*", false, true)
  assert(#version_directory_list == 1,
    "expected one versioned Harness state directory: " .. vim.inspect(version_directory_list))
  local checks = vim.fs.normalize(version_directory_list[1]) .. "/" .. session_id .. "/checks"
  return relative and (checks .. "/" .. relative) or checks
end

return M
