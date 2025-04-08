local provides = {}

-- local mode = 'qwerty'
local mode = 'apt3'

local keys = {
  '', 'w', 'a', 'f', '',
--           *    *
  '', 'gk', 'gj', '', 'g',
--sb    *    *    se      
  'u', 'r', 'c', 'v', 'b',
-- *    *      

  'x', 'y', 'i', 'o', 'p', 
--      *              
  'd', '',  '',  '',  '', 
-- *    h   up  down   l                   
  'z', '^', ',', ';', '$'
--      *         *    *      (start line, back, lsp-ref, end line)
}

local qwerty = {
  'q', 'w', 'e', 'r', 't', 
  'a', 's', 'd', 'f', 'g',
  'z', 'x', 'c', 'v', 'b',
  
  'y', 'u', 'i', 'o', 'p', 
  'h', 'j', 'k', 'l', ';', 
  'n', 'm', ',', '.', '/'
}

provides.qwerty = qwerty
provides.keys = keys

local apt3 = {
  'w', 'g', 'd', 'f', 'b', 
  'r', 's', 't', 'h', 'k', 
  'x', 'c', 'm', 'p', 'v',

  'q', 'l', 'u', 'o', 'y', 
  'j', 'n', 'e', 'a', 'i', 
  'z', ';', ',', '.', '/'
}

function indexOf(array, value)
  for i, v in ipairs(array) do
    if v == value then
      return i
    end
  end
  return nil
end

provides.key = function(key)
  if mode == 'qwerty' then 
    return key
  end

  if key == '<Tab>' then 
    return key 
  end 

  local out = ''
  for i = 1, #key do 
    local c = key:sub(i, i)
    local index = indexOf(qwerty, string.lower(c))
    if c == 'C' or c == 'S' then 
      out = out .. c
    elseif index then 
      if c == string.lower(c) then
        out = out .. apt3[index]
      else
        out = out .. string.upper(apt3[index])
      end
    else
      out = out .. c
    end
  end
  return out
end

provides.uremap = function(in_key, dest)
  local key = provides.key(in_key)
  local upper = string.upper(key)
  local dest_upper = string.upper(dest)
  vim.api.nvim_command(":nnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":xnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":onoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":nnoremap " .. upper .. ' ' .. dest_upper)
  vim.api.nvim_command(":xnoremap " .. upper .. ' ' .. dest_upper)
  vim.api.nvim_command(":onoremap " .. upper .. ' ' .. dest_upper)
end

provides.remap = function(in_key, dest)
  local key = provides.key(in_key)
  vim.api.nvim_command(":nnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":xnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":onoremap " .. key .. ' ' .. dest)
end

provides.keymap = function(modes, in_key, dest)
  local key = provides.key(in_key)
  vim.keymap.set(modes, key, dest, { nowait=true, silent=false })
end

provides.keymap_leader = function(modes, in_key, dest)
  local key = provides.key(in_key)
  vim.keymap.set(modes, "<leader>" .. key, dest, { nowait=true, silent=false })
end

return provides
