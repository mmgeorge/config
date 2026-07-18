local blocks = require("markdown_math.blocks")

local M = {}

---Normalize generated Markdown math into nodes recognized by the Markdown parser.
---@param text string
---@return string
function M.normalize_math(text)
  return blocks.normalize(text)
end

return M
