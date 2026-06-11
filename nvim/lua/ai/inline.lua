-- Inline editing on top of the ai prompt library: run a prompt about the
-- current selection and write the model's text back into the buffer. Unlike
-- CodeCompanion's inline interaction this never asks the model for a diff -
-- the model generates the complete text and we splice it in:
--
--   replace - the response replaces the selected lines
--   before  - the response is inserted above the selection (documentation, attributes)
--   after   - the response is inserted below the selection (trait impls, tests)
--
-- The selection is tracked with extmarks while the request is in flight, so
-- edits elsewhere in the buffer do not shift the splice target. The splice is
-- a single undo step - `u` is the reject button.

---@class AIInlineRange
---@field start_line integer 1-based, inclusive
---@field end_line integer 1-based, inclusive

---@class AIInlineOpts
---@field type "replace"|"before"|"after"
---@field prompt string|fun(selected_text: string): string instruction; a string is wrapped together with the fenced selection (and supports the #buffer token), a function builds the full user message itself
---@field model? string model token; defaults to adapters().inline_edit
---@field system? string extra system guidance appended to the built-in placement prompt
---@field buf? integer defaults to the current buffer
---@field range? AIInlineRange defaults to the visual selection, falling back to the cursor line
---@field notify? fun(message: string, level: integer) defaults to vim.notify

local M = {}

local namespace = vim.api.nvim_create_namespace("ai_inline")

local system_prompts = {
  replace = [[You are an inline text editor inside a code editor.
The user selected a span of text and wants it rewritten.
Respond with ONLY the full replacement text - no code fences, no commentary,
no explanation. Your response replaces the selected text verbatim, so keep
the surrounding indentation style and do not repeat text outside the
selection.]],
  before = [[You are an inline text generator inside a code editor.
The user selected a span of text and wants new text inserted immediately
BEFORE it (for example a documentation comment or attributes).
Respond with ONLY the text to insert - no code fences, no commentary, and do
NOT repeat the selected text itself. Match its indentation.]],
  after = [[You are an inline text generator inside a code editor.
The user selected a span of text and wants new text inserted immediately
AFTER it (for example an implementation, a test, or a usage example).
Respond with ONLY the text to insert - no code fences, no commentary, and do
NOT repeat the selected text itself. Match its indentation.]],
}

---@param content string
---@return string
local function strip_fences(content)
  local stripped = vim.trim(content)
  stripped = stripped:gsub("^```[%w%-_]*\n", ""):gsub("\n```$", "")
  -- A response that is a single fenced line ("```lua ... ```" on one line)
  -- is left alone; only whole-block fences are stripped.
  return stripped
end

---@param buf integer
---@return AIInlineRange
local function selection_range(buf)
  local mode = vim.api.nvim_get_mode().mode
  if mode:match("^[vV\22]") then
    local visual_line = vim.fn.line("v")
    local cursor_line = vim.fn.line(".")
    -- Leave visual mode so the splice does not land inside a stale selection.
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", false)
    return {
      start_line = math.min(visual_line, cursor_line),
      end_line = math.max(visual_line, cursor_line),
    }
  end

  local start_mark = vim.api.nvim_buf_get_mark(buf, "<")
  local end_mark = vim.api.nvim_buf_get_mark(buf, ">")
  if start_mark[1] > 0 and end_mark[1] >= start_mark[1] then
    return { start_line = start_mark[1], end_line = end_mark[1] }
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  return { start_line = cursor[1], end_line = cursor[1] }
end

--- Compose the user message from a plain instruction. Section order follows
--- provider long-context guidance (Anthropic, Gemini): context first, the
--- specific instruction at the very end. `#buffer` in the instruction is a
--- magic token: it is removed from the instruction and the entire buffer
--- content is included as a leading context section.
---@param instruction string
---@param buf integer
---@param range AIInlineRange
---@param selected_text string
---@param filetype string
---@return string
local function wrap_instruction(instruction, buf, range, selected_text, filetype)
  local include_buffer = false
  instruction = instruction:gsub("#buffer%f[%W]", function()
    include_buffer = true
    return ""
  end)
  instruction = vim.trim((instruction:gsub("  +", " ")))

  local sections = {}
  if include_buffer then
    local name = vim.api.nvim_buf_get_name(buf)
    name = name ~= "" and vim.fn.fnamemodify(name, ":~:.") or "(unnamed buffer)"
    local buffer_text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    sections[#sections + 1] = table.concat({
      "Current buffer (" .. name .. "):",
      "```" .. filetype,
      buffer_text,
      "```",
    }, "\n")
  end

  sections[#sections + 1] = table.concat({
    ("Selected text (lines %d-%d%s):"):format(
      range.start_line,
      range.end_line,
      filetype ~= "" and (", " .. filetype) or ""
    ),
    "```" .. filetype,
    selected_text,
    "```",
  }, "\n")

  sections[#sections + 1] = instruction
  return table.concat(sections, "\n\n")
end

--- Run a prompt about the selection and splice the response into the buffer.
--- on_done is optional and mainly a test seam; the user-facing signal is the
--- buffer edit (or a notification on failure).
---@param opts AIInlineOpts
---@param on_done? fun(result: AIGenerateResult)
function M.inline(opts, on_done)
  local notify = opts.notify or function(message, level)
    vim.notify(message, level, { title = "AI inline" })
  end
  local function fail(message)
    notify(message, vim.log.levels.WARN)
    if on_done then on_done({ ok = false, error = message }) end
  end

  local placement = system_prompts[opts.type]
  if not placement then
    fail(("invalid inline type %q (replace, before, after)"):format(tostring(opts.type)))
    return
  end

  local buf = opts.buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) then
    fail("invalid buffer")
    return
  end
  local range = opts.range or selection_range(buf)
  local line_count = vim.api.nvim_buf_line_count(buf)
  if range.start_line < 1 or range.end_line > line_count or range.start_line > range.end_line then
    fail(("invalid range %d-%d (buffer has %d lines)"):format(range.start_line, range.end_line, line_count))
    return
  end

  local selected_lines = vim.api.nvim_buf_get_lines(buf, range.start_line - 1, range.end_line, false)
  local selected_text = table.concat(selected_lines, "\n")
  local filetype = vim.bo[buf].filetype

  local prompt
  if type(opts.prompt) == "function" then
    prompt = opts.prompt(selected_text)
  elseif type(opts.prompt) == "string" and vim.trim(opts.prompt:gsub("#buffer%f[%W]", "")) ~= "" then
    prompt = wrap_instruction(opts.prompt, buf, range, selected_text, filetype)
  end
  if type(prompt) ~= "string" or vim.trim(prompt) == "" then
    fail("prompt is required (string or function returning a string)")
    return
  end

  local system = placement
  if opts.system then
    system = system .. "\n\n" .. opts.system
  end

  local model = opts.model
  if not model then
    local ok_adapters, adapters = pcall(require, "ai.adapters")
    model = ok_adapters and adapters.get().inline_edit or "copilot-mini"
  end

  -- Track the selection while the request is in flight so edits elsewhere in
  -- the buffer do not shift the splice target.
  local start_mark = vim.api.nvim_buf_set_extmark(buf, namespace, range.start_line - 1, 0, {})
  local end_mark = vim.api.nvim_buf_set_extmark(buf, namespace, range.end_line - 1, 0, {})
  local function clear_marks()
    pcall(vim.api.nvim_buf_del_extmark, buf, namespace, start_mark)
    pcall(vim.api.nvim_buf_del_extmark, buf, namespace, end_mark)
  end

  notify(("Generating (%s)..."):format(model), vim.log.levels.INFO)

  require("ai").generate({
    model = model,
    system = system,
    prompt = prompt,
  }, function(result)
    if not result.ok then
      clear_marks()
      fail(result.error or "inline generation failed")
      return
    end
    if not vim.api.nvim_buf_is_valid(buf) then
      clear_marks()
      fail("buffer was closed before the response arrived")
      return
    end

    local start_pos = vim.api.nvim_buf_get_extmark_by_id(buf, namespace, start_mark, {})
    local end_pos = vim.api.nvim_buf_get_extmark_by_id(buf, namespace, end_mark, {})
    clear_marks()
    if not start_pos[1] or not end_pos[1] then
      fail("selection was lost before the response arrived")
      return
    end
    local start_row = start_pos[1]
    local end_row = math.max(end_pos[1], start_row)

    local content = strip_fences(result.content or "")
    if content == "" then
      fail("model returned no text")
      return
    end
    local lines = vim.split(content, "\n", { plain = true })

    if opts.type == "replace" then
      vim.api.nvim_buf_set_lines(buf, start_row, end_row + 1, false, lines)
    elseif opts.type == "before" then
      vim.api.nvim_buf_set_lines(buf, start_row, start_row, false, lines)
    else
      vim.api.nvim_buf_set_lines(buf, end_row + 1, end_row + 1, false, lines)
    end

    if on_done then on_done({ ok = true, content = content }) end
  end)
end

return M
