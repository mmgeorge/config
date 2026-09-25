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
local reveal_state = {}

---@param buffer integer
---@param config table
---@return integer?, integer?
local function reveal_range(buffer, config)
  local mode = environment.mode.get()
  local policy = config.anti_conceal
  if not policy.enabled or environment.mode.is(mode, policy.disabled_modes)
    or environment.mode.is(mode, policy.ignore.latex or {})
    or vim.api.nvim_get_current_buf() ~= buffer then return end
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  if environment.mode.is(mode, { "v", "V", "\22" }) then
    local anchor = vim.fn.getpos("v")[2] - 1
    return math.min(row, anchor), math.max(row, anchor)
  end
  return row - policy.above, row + policy.below
end

---@param block MarkdownMathBlock
---@param first integer?
---@param last integer?
---@return boolean
local function revealed(block, first, last)
  return first ~= nil and first < block.end_row and last >= block.start_row
end

---@param buffer integer
---@param config table
---@return string
local function reveal_key(buffer, config)
  local first, last = reveal_range(buffer, config)
  local selected = {}
  for _, block in ipairs(block_store.get(buffer)) do
    if revealed(block, first, last) then selected[#selected + 1] = tostring(block.start_row) end
  end
  return table.concat(selected, ",")
end

---@param buffer integer
local function track_reveal(buffer)
  if reveal_state[buffer] then return end
  local state = { key = "" }
  reveal_state[buffer] = state
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "ModeChanged", "BufEnter" }, {
    buffer = buffer,
    callback = function(event)
      local config = require("render-markdown.state").get(buffer)
      local key = reveal_key(buffer, config)
      if key == state.key then return end
      state.key = key
      require("render-markdown.core.ui").update(buffer, vim.api.nvim_get_current_win(), event.event, true)
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer, once = true,
    callback = function() reveal_state[buffer] = nil end,
  })
end

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
  local command_list = environment.commands({ dependency.executable_path() })
  for _, command in ipairs(command_list) do
    local result = vim.system({ command }, { stdin = input, text = true, stdout = true, stderr = true }):wait()
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

---@param buffer integer
---@param block MarkdownMathBlock
---@param output string[]
---@param config render.md.latex.Config
---@param marks render.md.Marks
local function add_marks(buffer, block, output, config, marks)
  local virtual_line_list = {}
  for output_index = 2, #output do
    virtual_line_list[#virtual_line_list + 1] = { { block.indent .. output[output_index], config.highlight } }
  end
  marks:add(config, false, block.start_row, 0, {
    end_row = block.end_row - 1,
    end_col = #(vim.api.nvim_buf_get_lines(buffer, block.end_row - 1, block.end_row, false)[1] or ""),
    conceal = "",
    virt_text = { { block.indent .. output[1], config.highlight } },
    virt_text_pos = "inline",
    virt_lines = virtual_line_list,
    virt_lines_above = false,
  })
  if block.end_row > block.start_row + 1 then
    marks:add(config, false, block.start_row + 1, 0, {
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
  if context.root:type() ~= "document" then return builtin end
  local first0, _, end_row, end_col = context.root:range()
  local after0 = math.min(vim.api.nvim_buf_line_count(context.buf), end_row + (end_col > 0 and 1 or 0))
  local block_list = block_store.get(context.buf, first0, after0)
  builtin = vim.tbl_filter(function(mark)
    for _, block in ipairs(block_list) do
      if mark.start_row >= block.start_row and mark.start_row < block.end_row then return false end
    end
    return true
  end, builtin)

  local request_context = RequestContext.get(context.buf)
  if not request_context then return builtin end
  track_reveal(context.buf)
  reveal_state[context.buf].key = reveal_key(context.buf, request_context.config)
  local first, last = reveal_range(context.buf, request_context.config)
  local marks = Marks.new(request_context, false)
  for _, block in ipairs(block_list) do
    if not revealed(block, first, last) then
      local output = convert(block.input)
      if output then add_marks(context.buf, block, output, request_context.config.latex, marks) end
    end
  end
  vim.list_extend(builtin, marks:get())
  return builtin
end

return module
