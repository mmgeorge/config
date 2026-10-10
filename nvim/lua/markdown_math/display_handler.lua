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
local conversion_count = 0
local pending_input = {}
local waiting_buffer = {}
local render_lifetime = {}

--- Revokes pending conversion effects when a generated document releases its buffer.
---@param buffer integer
function module.invalidate(buffer)
  render_lifetime[buffer] = (render_lifetime[buffer] or 0) + 1
  waiting_buffer[buffer] = nil
end

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
  return first and (tostring(first) .. ":" .. tostring(last)) or ""
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
---@param buffer integer
---@return string[]?
local function convert(input, buffer)
  local cached = output_cache[input]
  if type(cached) == "table" then return cached end
  if cached == false then return nil end
  waiting_buffer[buffer] = true
  if pending_input[input] or conversion_count >= 4 then return nil end
  local command_list = environment.commands({ dependency.executable_path() })
  local command = command_list[1]
  if not command then
    output_cache[input] = false
    notify_once("Markdown math conversion failed: no executable converter is available")
    return nil
  end
  pending_input[input], conversion_count = true, conversion_count + 1
  local function complete(result)
    pending_input[input], conversion_count = nil, conversion_count - 1
    local output = (result.stdout or ""):gsub("\r", ""):gsub("\n+$", "")
    if result.code == 0 and output:find("%S") then
      output_cache[input] = vim.split(output, "\n", { plain = true })
    else
      output_cache[input] = false
      local detail = vim.trim(result.stderr or "")
      if detail == "" then detail = ("exited with code %d"):format(result.code) end
      notify_once("Markdown math conversion failed: " .. command .. ": " .. detail)
    end
    local refresh = waiting_buffer
    waiting_buffer = {}
    for target in pairs(refresh) do
      if vim.api.nvim_buf_is_valid(target) then
        local window = vim.fn.win_findbuf(target)[1]
        if window then
          local lifetime = render_lifetime[target]
          local delay = require("render-markdown.state").get(target).debounce + 1
          vim.defer_fn(function()
            if render_lifetime[target] == lifetime and vim.api.nvim_buf_is_valid(target) and vim.api.nvim_win_is_valid(window)
              and vim.api.nvim_win_get_buf(window) == target then
              require("render-markdown.core.ui").update(target, window, "MathConverted", true)
            end
          end, delay)
        end
      end
    end
  end
  local accepted, failure = pcall(vim.system, { command },
    { stdin = input, text = true, stdout = true, stderr = true, timeout = 30000 },
    function(result) vim.schedule(function() complete(result) end) end)
  if not accepted then vim.schedule(function() complete({ code = -1, stderr = tostring(failure) }) end) end
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
      end_row = block.end_row - 1,
      end_col = #(vim.api.nvim_buf_get_lines(buffer, block.end_row - 1, block.end_row, false)[1] or ""),
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
      local output = convert(block.input, context.buf)
      if output then add_marks(context.buf, block, output, request_context.config.latex, marks) end
    end
  end
  vim.list_extend(builtin, marks:get())
  return builtin
end

--- Renders injected math from the same asynchronous converter as physical display blocks.
module.latex = {
  ---@param context MarkdownMathHandlerContext
  ---@return render.md.Mark[]
  parse = function(context)
    local request = RequestContext.get(context.buf)
    if not request or not request.config.latex.enabled then return {} end
    local first, _, after, column = context.root:range()
    local enclosing = block_store.get(context.buf, math.max(0, first - 1),
      math.min(vim.api.nvim_buf_line_count(context.buf), after + 2))
    for _, block in ipairs(enclosing) do
      if first >= block.start_row and after < block.end_row then return {} end
    end
    local input = vim.treesitter.get_node_text(context.root, context.buf)
    input = vim.trim(input:match("^%$*(.-)%$*$") or input)
    local output = convert(input, context.buf)
    if not output then return {} end
    local marks = Marks.new(request, false)
    local virtual = {}
    for index = 2, #output do virtual[#virtual + 1] = { { output[index], request.config.latex.highlight } } end
    local start_row, start_column = context.root:range()
    marks:add(request.config.latex, "latex", start_row, start_column, {
      end_row = after, end_col = column, conceal = "",
      virt_text = { { output[1], request.config.latex.highlight } }, virt_text_pos = "inline",
      virt_lines = #virtual > 0 and virtual or nil,
      virt_lines_above = request.config.latex.position == "above",
    })
    return marks:get()
  end,
}

return module
