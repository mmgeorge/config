local rust_sidecar = require("rust_sidecar")
local plugin_root = rust_sidecar.runtime_root(debug.getinfo(1, "S").source:sub(2))

---@class ForgeBuilder: RustSidecarBuilder
---@field for_profile fun(profile: 'dev'|'release'): RustSidecarBuilder Stable builder with independent build state for the requested profile. Rejects unknown profiles.

---@type table<string, RustSidecarBuilder>
local builder_by_profile = {}
for _, profile in ipairs({ "dev", "release" }) do
  builder_by_profile[profile] = rust_sidecar.new({
    crate_name = "forge",
    binary_target = "forge",
    locked = true,
    profile = profile,
    crate_dir = function()
      return vim.fs.joinpath(plugin_root, "rust", "forge")
    end,
  })
end

local builder = assert(builder_by_profile[vim.g.forge_build_profile or "dev"],
  "Forge build profile must be dev or release")
---@cast builder ForgeBuilder

---Return the stable builder for a profile without changing runtime selection.
---@param profile 'dev'|'release'
---@return RustSidecarBuilder
function builder.for_profile(profile)
  return assert(builder_by_profile[profile], "Forge build profile must be dev or release")
end

return builder
