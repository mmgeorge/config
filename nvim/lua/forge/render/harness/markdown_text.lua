local M = {}

--- Normalizes generated Markdown math delimiters into standard blocks parsed by Tree-sitter. Text
--- passes through unchanged when the editor does not provide `markdown_math`.
---@param text string Raw Markdown text string.
---@return string normalized Normalized Markdown text string.
function M.normalize_math(text)
  local blocks = require("forge.integrations.markdown_math").module("blocks")
  if not blocks then return text end
  return blocks.normalize(text)
end

return M
