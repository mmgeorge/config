local rust_sidecar = require("rust_sidecar")
local plugin_root = rust_sidecar.runtime_root(debug.getinfo(1, "S").source:sub(2))

local builder = rust_sidecar.new({
  crate_name = "forge",
  binary_target = "forge",
  locked = true,
  profile = vim.g.forge_build_profile or "release",
  crate_dir = function()
    return vim.fs.joinpath(plugin_root, "rust", "forge")
  end,
})

return builder
