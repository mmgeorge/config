--- `:checkhealth forge`: reports the optional integrations Forge detects at runtime.
local M = {}

function M.check()
  vim.health.start("Markdown math (optional)")
  if not require("forge.integrations.markdown_math").available() then
    vim.health.info("markdown_math is not on 'runtimepath'; Harness renders math unconverted.")
    return
  end
  vim.health.ok("markdown_math converts Harness math.")
  local converter = require("markdown_math.dependency").executable_path()
  if vim.fn.executable(converter) == 1 then
    vim.health.ok("Math converter: " .. converter)
  else
    vim.health.warn("Math converter is not installed: " .. converter, "Rebuild render-markdown.nvim to install libtexprintf.")
  end
end

return M
