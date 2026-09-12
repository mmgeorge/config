local blocks = require("markdown_math.blocks")

local M = {}

--- Normalizes generated Markdown math delimiters into standard blocks parsed by Tree-sitter.
---@param text string Raw Markdown text string.
---@return string normalized Normalized Markdown text string.
function M.normalize_math(text)
  return blocks.normalize(text)
end

return M
