--- Registers the plugin's bundled treesitter queries by putting its root on the runtimepath,
--- so vim.treesitter.query.get resolves queries/<lang>/{diff_context,diff_inventory}.scm.
---
--- Required by the entry point and every query consumer (git_data, inventory) so the queries
--- register no matter which module a caller or test loads first.
local root = debug.getinfo(1, "S").source:sub(2):gsub("\\", "/"):gsub("/[^/]*$", "")
if not vim.tbl_contains(vim.opt.runtimepath:get(), root) then
  vim.opt.runtimepath:append(root)
end

return {}
