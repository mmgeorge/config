local rust_sidecar = require("rust_sidecar")
local plugin_root = rust_sidecar.runtime_root(debug.getinfo(1, "S").source:sub(2))

local builder = rust_sidecar.new({
  crate_name = "github-issue-index",
  crate_dir = function()
    return vim.fs.joinpath(plugin_root, "rust", "github-issue-index")
  end,
})

return builder
