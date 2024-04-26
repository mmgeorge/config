vim.g.clipboard = {
  name = "wslClipboard",
  copy = {
    ['+'] = 'clip.exe',
    ['*'] = 'clip.exe'
  },
  paste = {
    ['+'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    ['*'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))'
  },
  cache_enabled = 0
}

local keys = {
  'q', 'w', 'a', 'f', 't',
--           *    *
  '', 'k', 'j', '', 'g',
--sb    *    *    se      
  'u', 'r', 'c', 'v', 'b',
-- *    *      

  'x', 'y', 'i', 'o', 'p', 
--      *              
  'd', '',  '',  '',  '', 
-- *    h   up  down   l                   
  'n', '^', ',', ';', '$'
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

local mode = 'qwerty'
-- local mode = 'apt3'
function to_layout_key(key)
  if mode == 'qwerty' then 
    return key
  end

  if key == '<Tab>' then 
    return key 
  end 

  local out = ''
  for i = 1, #key do 
    local c = key:sub(i, i)
    local index = indexOf(qwerty, c)
    if c == 'C' or c == 'S' then 
      out = out .. c
    elseif index then 
      out = out .. apt3[index]
    else
      out = out .. c
    end
  end
  return out
end

function uremap(in_key, dest)
  local key = to_layout_key(in_key)
  local upper = string.upper(key)
  local dest_upper = string.upper(dest)
  vim.api.nvim_command(":nnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":xnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":onoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":nnoremap " .. upper .. ' ' .. dest_upper)
  vim.api.nvim_command(":xnoremap " .. upper .. ' ' .. dest_upper)
  vim.api.nvim_command(":onoremap " .. upper .. ' ' .. dest_upper)
end

function remap(in_key, dest)
  local key = to_layout_key(in_key)
  vim.api.nvim_command(":nnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":xnoremap " .. key .. ' ' .. dest)
  vim.api.nvim_command(":onoremap " .. key .. ' ' .. dest)
end

function keymap(modes, in_key, dest)
  local key = to_layout_key(in_key)
  vim.keymap.set(modes, key, dest, { nowait=true, silent=false })
end

function keymap_leader(modes, in_key, dest)
  local key = to_layout_key(in_key)
  vim.keymap.set(modes, "<leader>d" .. key, dest, { nowait=true, silent=false })
end


-- Unmaps -
vim.keymap.set({'n', 'x'}, 'o', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'q', '<Nop>', {})
-- keymap({'n', 'v'}, 'xo', '<Nop>')
vim.keymap.set({'n', 'x'}, 'x', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'b', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'a%', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'w', '<Nop>', {})
vim.keymap.set({'n', 'x', 'i'}, '<C-z>', '<Nop>', {})
--keymap({'n', 'v',}, 'w', '<Nop>')

vim.api.nvim_command(":unmap w")
vim.api.nvim_command(":unmap b")
vim.api.nvim_command(":unmap a%")

local se = "<cmd>lua require('spider').motion('e')<CR>"
local sb = "<cmd>lua require('spider').motion('b')<CR>"
local up = '5<C-U>'
local down = '5<C-D>'
local back = '<C-o>'

for i, v in ipairs(keys) do
  if v ~= '' then 
    uremap(qwerty[i], v)
  end
end

keymap('n', ',', back)
keymap({'n', 'x'}, 'k', up)
keymap({'n', 'x'}, 'l', down)
keymap({'x'}, 'p', 'P') -- P will paste over in x without yanking

remap('f', se)
remap('a', sb)
remap('j', 'h') -- Left
remap(';', 'l') -- Right

-- keymap({'n'}, 'xc', 'x') -- Delete character
-- keymap({'n'}, 'cx', 'r') -- Replace character

----Enter Modes
keymap('n', 'K', 'O') -- Enter insert above
keymap('n', 'L', 'o') -- Enter insert below
-- keymap({'n', 'x'}, 'nzzzv')
-- keymap({'n', 'x'}, 'Nzzzv')

keymap({'n', 'v', 'i'}, '<C-h>', 'dd')
keymap({'n', 'x'}, 'Z', '<C-r>') -- Redo

keymap({'n'}, 'I', 'O<ESC>') -- Insert blank line below
keymap({'n'}, 'O', 'o<ESC>') -- Insert blank line above

-- Move text up or down
keymap({'x'}, 'L', "<CMD>m '<+1<CR>gv=gv") -- Kill
keymap({'x'}, 'K', "<CMD>m '>-2<CR>gv=gv") -- Kill

-- Commands
keymap_leader({'x', 'n'}, 'u', "\"+y") -- Copy to clipboard
keymap_leader({'x', 'n'}, 'U', "\"+Y") -- Copy to clipboard
keymap_leader({'x', 'n'}, 'p', "\"+P") -- Paste to clipboard
keymap_leader({'n', 'v'}, 's', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
keymap_leader({'n', 'v'}, 'iu', function() require("utils").insert_v4({}) end)
keymap_leader({'n', 'v'}, 't', 'gg=G')

---- Jump quickfix
-- keymap('n', '<leader>k', "<cmd>lnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<leader>j', "<cmd>lprev<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-k>', "<cmd>cnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-j>', "<cmd>cprev<CR>zz") -- Paste over (don't add to clipboard)

-- Window
keymap('n', 'wsb', '<CMD>split<CR>')
keymap('n', 'wko', '<CMD>only<CR>')
keymap('n', 'wks', '<CMD>close!<CR>')
keymap('n', 'wo', '<C-w>W')
keymap('n', 'wkb', '<CMD>bd<CR>')

keymap('n', 'wq', function() require("trouble").toggle("quickfix") end) 

-- Lsp --
-- keymap('n', '<C-l>f', vim.lsp.buf.code_action)

-- keymap('n', ',', '<C-o>' ) -- Jump back
keymap('n', '=', 'g;' ) -- Jump back to last change
keymap('n', '{', 'g,' ) -- Jump recent change
keymap({'n', 'v'}, '<Tab>', '==' ) 

keymap('n', '.', '<CMD>Glance definitions<CR>') 
keymap('n', 'or', '<CMD>Glance references<CR>') 
keymap('n', 'ot', '<CMD>Glance type_definitions<CR>') 
keymap('n', 'oi', '<CMD>Glance implementations<CR>') 
keymap({'n', 'x'}, 'ou', function() vim.diagnostic.open_float(nil, { focus = false }) end)


keymap('n', 'ou', vim.lsp.buf.hover) 
keymap('n', 'oe', function ()
  vim.diagnostic.open_float(0)
end) 
-- keymap('n', 'ot', vim.lsp.buf.type_definition) 
keymap('n', 'ow', vim.lsp.buf.rename) 

keymap({'n', 'x'}, '<C-f>', require("plugins.telescope.occur").occur_in_file)
keymap({'n', 'x'}, 'of', require("telescope.builtin").find_files)
keymap({'n', 'x'}, 'od', require("telescope.builtin").live_grep)
keymap({'n', 'x'}, 'os', require("telescope.builtin").buffers)
keymap({'n', 'x'}, 'oa', "<CMD>Telescope file_browser path=%:p:h select_buffer=true<CR><ESC>")
keymap({'n', 'x'}, 'oo', require("telescope.builtin").lsp_document_symbols)
keymap({'n', 'x'}, 'ows', require("telescope.builtin").lsp_workspace_symbols)
keymap({'n', 'x'}, 'opp', vim.lsp.buf.list_workspace_folders)
keymap({'n', 'x'}, 'oq', function()
  vim.lsp.buf.format {
    filter = function(client) return client.name ~= "tsserver" end
  }
end)

-- keymap({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Common
keymap({'i', 'v', 'c', 't', 's', 'o', 'x'}, '<C-o>', '<Esc>') -- Cancel

keymap('n', '<C-_>', '<Plug>(comment_toggle_linewise_current)j') -- Comment line, maps to C-/

keymap('n', '<C-e>', function() vim.diagnostic.goto_next() end)
keymap('n', '<C-q>', '<CMD>x<CR>')
keymap('n', '<S-C-e>', function() vim.diagnostic.goto_prev() end)
keymap('n', '<C-s>', '<CMD>w<CR>') -- Save file
keymap('i', '<C-s>', '<C-o>:w<CR>') -- Save file

