--- Leaf utilities for diff_review: diff stat counting, loaded-buffer lookup, NUL-byte
--- detection, and filetype resolution. Pure helpers holding no plugin state.
local M = {}

---@param diff_text string
---@return number added, number removed
local function count_stats(diff_text)
  local added, removed = 0, 0
  for line in diff_text:gmatch("[^\n]+") do
    local first = line:sub(1, 1)
    if first == "+" and not line:find("^%+%+%+") then
      added = added + 1
    elseif first == "-" and not line:find("^%-%-%-") then
      removed = removed + 1
    end
  end
  return added, removed
end
---@param filename string
---@return number?
local function loaded_file_buffer(filename)
  local buf = vim.fn.bufnr(filename)
  if buf == -1 or not vim.api.nvim_buf_is_loaded(buf) then return nil end
  return buf
end
---@param lines string[]
---@return boolean
local function lines_contain_nul(lines)
  for _, line in ipairs(lines) do
    if line:find("\0", 1, true) then return true end
  end
  return false
end
---@param filename string
---@return boolean
local function file_contains_nul(filename)
  local uv = vim.uv or vim.loop
  local fd = uv.fs_open(filename, "r", 438)
  if not fd then return false end

  local offset = 0
  local chunk_size = 65536
  while true do
    local data = uv.fs_read(fd, chunk_size, offset)
    if not data or data == "" then break end
    if data:find("\0", 1, true) then
      uv.fs_close(fd)
      return true
    end
    offset = offset + #data
    if #data < chunk_size then break end
  end

  uv.fs_close(fd)
  return false
end
---@param filename string
---@param contents? string[]
---@return string
local function detect_filetype(filename, contents)
  local args = {
    filename = filename,
  }
  local buf = loaded_file_buffer(filename)
  if buf then args.buf = buf end
  if contents then
    args.contents = contents
  end
  return vim.filetype.match(args) or ""
end

M.count_stats = count_stats
M.loaded_file_buffer = loaded_file_buffer
M.lines_contain_nul = lines_contain_nul
M.file_contains_nul = file_contains_nul
M.detect_filetype = detect_filetype

return M
