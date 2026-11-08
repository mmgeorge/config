--- Optional integration with `markdown_math`, which normalizes Markdown math delimiters and
--- converts math into text art. Forge uses it only when the editor provides the module; without it,
--- Harness renders math through render-markdown's built-in handlers and leaves it unconverted.
local M = {}

---@param name "blocks"|"display_handler"
---@return table? module the `markdown_math` submodule, or nil when the editor does not provide it
function M.module(name)
  local ok, module = pcall(require, "markdown_math." .. name)
  if ok and type(module) == "table" then return module end
  return nil
end

---@return boolean
function M.available()
  return M.module("display_handler") ~= nil
end

return M
