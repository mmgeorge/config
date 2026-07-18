local module = {}

local block_store = require("markdown_math.block_store")
local dependency = require("markdown_math.dependency")
local environment = require("render-markdown.lib.env")
local Marks = require("render-markdown.lib.marks")
local RequestContext = require("render-markdown.request.context")

---@class MarkdownMathHandlerContext
---@field buf integer
---@field root TSNode

---@type table<string, string[]|false>
local output_cache = {}
---@type table<string, boolean>
local notified_error = {}

---@param message string
local function notify_once(message)
  if notified_error[message] then return end
  notified_error[message] = true
  vim.notify(message, vim.log.levels.ERROR, { title = "Markdown math" })
end

---@param input string
---@return string[]?
local function convert(input)
  local cached = output_cache[input]
  if type(cached) == "table" then return cached end
  if cached == false then return nil end

  local error_list = {}
  local command_list = environment.commands({ dependency.executable_path(), "latex2text" })
  for _, command in ipairs(command_list) do
    local result = vim.system({ command }, { stdin = input, text = true }):wait()
    local output = (result.stdout or ""):gsub("\r", ""):gsub("\n+$", "")
    if result.code == 0 and output:find("%S") then
      local line_list = vim.split(output, "\n", { plain = true })
      output_cache[input] = line_list
      return line_list
    end
    local detail = vim.trim(result.stderr or "")
    if detail == "" then detail = ("exited with code %d"):format(result.code) end
    error_list[#error_list + 1] = ("%s: %s"):format(command, detail)
  end

  output_cache[input] = false
  local detail = #error_list > 0 and table.concat(error_list, "; ") or "no executable converter is available"
  notify_once("Markdown math conversion failed: " .. detail)
  return nil
end

---@param context MarkdownMathHandlerContext
---@return boolean
local function is_document_root(context)
  local start_row, start_col, end_row, end_col = context.root:range()
  if start_row ~= 0 or start_col ~= 0 then return false end
  local line_count = vim.api.nvim_buf_line_count(context.buf)
  local last_line = vim.api.nvim_buf_get_lines(context.buf, line_count - 1, line_count, false)[1] or ""
  return end_row > line_count - 1 or (end_row == line_count - 1 and end_col >= #last_line)
end

---@param block MarkdownMathBlock
---@param output string[]
---@param config render.md.latex.Config
---@param marks render.md.Marks
local function add_marks(block, output, config, marks)
  local virtual_line_list = {}
  for output_index = 2, #output do
    virtual_line_list[#virtual_line_list + 1] = { { block.indent .. output[output_index], config.highlight } }
  end
  marks:add(config, "latex", block.start_row, 0, {
    virt_text = { { block.indent .. output[1], config.highlight } },
    virt_text_pos = "overlay",
    virt_lines = virtual_line_list,
    virt_lines_above = false,
  })
  if block.end_row > block.start_row + 1 then
    marks:add(config, "latex", block.start_row + 1, 0, {
      end_row = block.end_row,
      end_col = 0,
      conceal_lines = "",
    })
  end
end

---Render physical multiline math blocks while preserving render-markdown's standard Markdown marks.
---@param context MarkdownMathHandlerContext
---@return render.md.Mark[]
function module.parse(context)
  local builtin = require("render-markdown.handler.markdown").parse(context)
  if not is_document_root(context) then return builtin end
  local block_list = block_store.get(context.buf)
  builtin = vim.tbl_filter(function(mark)
    for _, block in ipairs(block_list) do
      if mark.start_row >= block.start_row and mark.start_row < block.end_row then return false end
    end
    return true
  end, builtin)

  local request_context = RequestContext.get(context.buf)
  if not request_context then return builtin end
  local marks = Marks.new(request_context, false)
  for _, block in ipairs(block_list) do
    local output = convert(block.input)
    if output then add_marks(block, output, request_context.config.latex, marks) end
  end
  vim.list_extend(builtin, marks:get())
  return builtin
end

return module
