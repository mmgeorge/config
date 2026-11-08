-- Buffer reads and writes that test cases repeat: whole-buffer lines and text, one line, replacing
-- the contents, scratch buffers, and finding the row that contains some text.
--
--   local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
--   local scratch = buffer.scratch({ "alpha", "beta" }, { filetype = "markdown" })
--   eq(buffer.lines(scratch), { "alpha", "beta" })
--   eq(buffer.find_row(scratch, "beta"), 2)
local buffer = {}

---@param bufnr integer
---@return string[]
function buffer.lines(bufnr)
  return vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
end

---@param bufnr integer
---@return string the buffer's lines joined with newlines
function buffer.text(bufnr)
  return table.concat(buffer.lines(bufnr), "\n")
end

---@param bufnr integer
---@param row integer one-based row
---@return string?
function buffer.line(bufnr, row)
  return vim.api.nvim_buf_get_lines(bufnr, row - 1, row, false)[1]
end

---@param bufnr integer
---@param lines string[]|string a list of lines, or text split on newlines
function buffer.set(bufnr, lines)
  if type(lines) == "string" then lines = vim.split(lines, "\n", { plain = true }) end
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
end

---@class ScratchBufferOptions
---@field name? string
---@field filetype? string
---@field listed? boolean

---@param lines? string[]|string initial contents
---@param options? ScratchBufferOptions
---@return integer bufnr an unlisted scratch buffer unless `listed` is set
function buffer.scratch(lines, options)
  options = options or {}
  local bufnr = vim.api.nvim_create_buf(options.listed == true, true)
  if options.name then vim.api.nvim_buf_set_name(bufnr, options.name) end
  if lines then buffer.set(bufnr, lines) end
  if options.filetype then vim.bo[bufnr].filetype = options.filetype end
  return bufnr
end

---@param bufnr integer
---@param needle string
---@param options? { pattern?: boolean, first_row?: integer } `pattern` matches `needle` as a Lua
---  pattern instead of plain text; `first_row` starts the search at that one-based row
---@return integer? row the first one-based row at or after `first_row` containing `needle`
function buffer.find_row(bufnr, needle, options)
  options = options or {}
  local first_row = options.first_row or 1
  for row, line in ipairs(buffer.lines(bufnr)) do
    if row >= first_row and line:find(needle, 1, not options.pattern) then return row end
  end
  return nil
end

return buffer
