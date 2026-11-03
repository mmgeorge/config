local M = {}

---Materialize a navigation project without copying generated build output.
---@param source string
---@param destination string
function M.copy(source, destination)
  vim.fn.mkdir(destination, "p")
  for name, kind in vim.fs.dir(source) do
    if kind == "directory" then
      if name ~= "target" then M.copy(source .. "/" .. name, destination .. "/" .. name) end
    else
      local output = name == "Cargo.toml.fixture" and "Cargo.toml" or name
      assert(vim.uv.fs_copyfile(source .. "/" .. name, destination .. "/" .. output))
    end
  end
end

return M
