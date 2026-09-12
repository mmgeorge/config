--- Protects native document buffers from the editor-wide Tree-sitter attachment.
--- Native documents receive syntax decorations in their host snapshots, so a global
--- parser would add a second, unrelated source of extmarks and indentation state.
local M = {}

--- Reports whether the editor-wide Tree-sitter attachment owns a buffer.
---@param buffer integer
---@return boolean
function M.global_parser_allowed(buffer)
  return vim.b[buffer].forge_native_document ~= true
end

--- Attaches the editor-wide Tree-sitter parser only to buffers it owns.
---@param buffer integer
---@return boolean attached
function M.attach_global_parser(buffer)
  if not M.global_parser_allowed(buffer) then return false end
  pcall(vim.treesitter.start, buffer)
  vim.bo[buffer].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  return true
end

return M
