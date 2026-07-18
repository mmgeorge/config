vim.loader.enable(false)

local dependency = require("markdown_math.dependency")

local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

local function assert_true(value, message)
  if not value then fail(message) end
end

local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
local source_line_list = {
  "# Equation",
  "",
  "$$",
  "L_o(x,\\omega_o)",
  "=",
  "L_e(x,\\omega_o)",
  "+",
  "\\int_{\\Omega} f_r(x,\\omega_i,\\omega_o)\\, L_i(x,\\omega_i)",
  "$$",
}
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, source_line_list)
vim.bo[buffer].filetype = "markdown"

local render_markdown = require("render-markdown")
render_markdown.setup({
  render_modes = true,
  custom_handlers = { markdown = require("markdown_math.display_handler") },
  latex = { converter = { dependency.executable_path(), "latex2text" } },
})
local configured_converter = require("render-markdown.state").get(buffer).latex.converter
local available_converter = require("render-markdown.lib.env").commands(configured_converter)
assert_true(#available_converter == 2, "render-markdown must resolve utftex and latex2text: " .. vim.inspect({
  configured = configured_converter,
  available = available_converter,
}))
render_markdown.render({ buf = buffer, win = vim.api.nvim_get_current_win(), event = "MarkdownMathTest" })

local namespace = require("render-markdown.core.ui").ns
local mark_list = {}
local rendered = ""
local rendered_ok = vim.wait(5000, function()
  mark_list = vim.api.nvim_buf_get_extmarks(buffer, namespace, 0, -1, { details = true })
  local rendered_line_list = {}
  for _, mark in ipairs(mark_list) do
    local details = mark[4]
    if details.virt_text then
      local text_list = {}
      for _, chunk in ipairs(details.virt_text) do text_list[#text_list + 1] = chunk[1] end
      rendered_line_list[#rendered_line_list + 1] = table.concat(text_list)
    end
    for _, virtual_line in ipairs(details.virt_lines or {}) do
      local text_list = {}
      for _, chunk in ipairs(virtual_line) do text_list[#text_list + 1] = chunk[1] end
      rendered_line_list[#rendered_line_list + 1] = table.concat(text_list)
    end
  end
  rendered = table.concat(rendered_line_list, "\n")
  return rendered:find("⌠", 1, true) ~= nil
end, 20)
assert_true(
  rendered_ok,
  "render-markdown must receive the utftex integral top: " .. vim.inspect({ rendered = rendered, marks = mark_list })
)
assert_true(rendered:find("Ω", 1, true) ~= nil, "render-markdown must receive the utftex lower bound")
assert_true(
  vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), source_line_list),
  "rendering must preserve the Markdown source"
)

vim.api.nvim_buf_delete(buffer, { force = true })
io.write("markdown_math_render OK\n")
vim.cmd("qa!")
