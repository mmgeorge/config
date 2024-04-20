
local provides = {}

provides.get_uuid = function(opts)
  local templates = {
    v4 = "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx",
  }

  local uuid = string.gsub(templates.v4, "[xy]", function(c)
    local r = math.random()

    -- if c == 'x', generate a random hex digit (0-15)
    -- if c == 'y', generate a random hex digit (8-11)
    local v = c == "x" and math.floor(r * 0x10) or (math.floor(r * 0x4) + 8)

    return string.format("%x", v)
  end)

  -- Convert to upper case if requested (always lower case by default)
  -- if opts.case == "upper" then
  --   uuid = string.upper(uuid)
  -- end
  --
  -- if opts.quotes == "double" then
  --   uuid = '"' .. uuid .. '"'
  -- elseif opts.quotes == "single" then
  --   uuid = "'" .. uuid .. "'"
  -- end
  --
  -- if opts.prefix ~= "" then
  --   uuid = opts.prefix .. uuid
  -- end
  --
  -- if opts.suffix ~= "" then
  --   uuid = uuid .. opts.suffix
  -- end
  --
  return uuid
end

provides.insert_v4 = function(opts)
  local uuid = provides.get_uuid(opts)
  local after = true

  vim.api.nvim_put({ uuid }, "c", after, true)
end

local function tbl_length(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

-- See https://github.com/ibhagwan/fzf-lua/blob/6ee73fdf2a79bbd74ec56d980262e29993b46f2b/lua/fzf-lua/utils.lua
provides.get_visual_selection = function ()
  -- this will exit visual mode
  -- use 'gv' to reselect the text
  local _, csrow, cscol, cerow, cecol
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode == "" then
    -- if we are in visual mode use the live position
    _, csrow, cscol, _ = unpack(vim.fn.getpos("."))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("v"))
    if mode == "V" then
      -- visual line doesn't provide columns
      cscol, cecol = 0, 999
    end
    -- exit visual mode
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<Esc>",
        true, false, true), "n", true)
  else
    -- otherwise, use the last known visual position
    _, csrow, cscol, _ = unpack(vim.fn.getpos("'<"))
    _, cerow, cecol, _ = unpack(vim.fn.getpos("'>"))
  end
  -- swap vars if needed
  if cerow < csrow then csrow, cerow = cerow, csrow end
  if cecol < cscol then cscol, cecol = cecol, cscol end
  local lines = vim.fn.getline(csrow, cerow)
  -- local n = cerow-csrow+1
  local n = tbl_length(lines)
  if n <= 0 then return "" end
  lines[n] = string.sub(lines[n], 1, cecol)
  lines[1] = string.sub(lines[1], cscol)
  return table.concat(lines, "\n")
end


return provides
