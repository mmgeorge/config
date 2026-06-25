-- Remove orphaned LuaCATS doc blocks: ---@param/---@return groups whose following code
-- line is not a function definition (left behind when a function was extracted to a module).
-- Only ever deletes comment lines; never touches code. Standalone ---@class/---@alias typedefs
-- and plain comments are preserved.
local path = arg[1] or "D:/config/nvim/lua/diff_review/init.lua"
local lines = vim.fn.readfile(path)
local n = #lines
local function is_anno(l) return l:sub(1, 3) == "---" end
local function is_func(l) return l:match("^function ") ~= nil or l:match("^local function ") ~= nil end
local out = {}
local removed = 0
local i = 1
while i <= n do
  local l = lines[i]
  if is_anno(l) then
    local j = i
    local block = {}
    while j <= n and is_anno(lines[j]) do
      block[#block + 1] = lines[j]
      j = j + 1
    end
    -- peek past blank lines to the next real code line
    local k = j
    while k <= n and lines[k]:match("^%s*$") do k = k + 1 end
    local next_code = lines[k] or ""
    local has_param, is_type = false, false
    for _, b in ipairs(block) do
      if b:match("^%-%-%-@param") or b:match("^%-%-%-@return") then has_param = true end
      if b:match("^%-%-%-@class") or b:match("^%-%-%-@alias") then is_type = true end
    end
    local keep
    if is_func(next_code) then
      keep = true
    elseif is_type then
      keep = true
    elseif has_param then
      keep = false
    else
      keep = true
    end
    if keep then
      for _, b in ipairs(block) do out[#out + 1] = b end
    else
      removed = removed + #block
      -- also drop one trailing blank so we do not leave a double blank
      if j <= n and lines[j]:match("^%s*$") then j = j + 1 end
    end
    i = j
  else
    out[#out + 1] = l
    i = i + 1
  end
end
vim.fn.writefile(out, path)
io.write(("orphan cleanup: %d -> %d lines (%d annotation lines removed)\n"):format(n, #out, removed))
