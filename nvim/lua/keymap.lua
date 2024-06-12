local k = require("keys")

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
local down = '5<C-e>'
local back = '<C-o>'

for i, v in ipairs(k.keys) do
  if v ~= '' then 
    k.uremap(k.qwerty[i], v)
  end
end

vim.keymap.set({'n', 'x'}, '<iw>', '<Nop>', {})

-- Semantic keymaps
vim.keymap.set({'n', 'x'}, '<C-a>', 'ggVG', {})

vim.keymap.set({'n', 'x'}, '<Tab>', '==', {})
vim.keymap.set({'i'}, '<Tab>', '<C-f>', {})

k.keymap('n', ',', back)
k.keymap({'n', 'x'}, 'k', up)
k.keymap({'n', 'x'}, 'l', down)

k.remap('f', se)
k.remap('a', sb)
k.remap('j', 'h') -- Left
k.remap(';', 'l') -- Right

k.keymap({ 'n' }, 'c/', 'cc')
k.keymap({ 'n' }, 'cc', 'ciw')
k.keymap({ 'n' }, 'vv', 'viw')
k.keymap({ 'n', 'v' }, 'm', 'g^')
k.keymap({ 'n', 'v' }, '/', 'g$')

k.keymap({ 'n', 'x' }, 'gg', 'gg')
k.keymap({ 'n', 'x' }, 'G', 'G')
-- vim.keymap.set({'n', 'x'}, 'l', 'u', {})
-- keymap({'n'}, 'xc', 'x') -- Delete character
-- keymap({'n'}, 'cx', 'r') -- Replace character

----Enter Modes
k.keymap('n', 'K', 'O') -- Enter insert above
k.keymap('n', 'L', 'o') -- Enter insert below
-- keymap({'n', 'x'}, 'nzzzv')
-- keymap({'n', 'x'}, 'Nzzzv')

k.keymap({'n', 'v', 'i'}, '<C-h>', 'dd')
k.keymap({'n', 'x'}, 'Z', '<C-r>') -- Redo

-- vim.keymap.set({'n'}, '<CR>', 'O<ESC>') -- Insert blank line below
k.keymap({'n'}, 'I', 'O<ESC>') -- Insert blank line below
k.keymap({'n'}, 'O', 'o<ESC>') -- Insert blank line above

-- Move text up or down
k.keymap({'x'}, 'L', "<CMD>m '<+1<CR>gv=gv") -- Kill
k.keymap({'x'}, 'K', "<CMD>m '>-2<CR>gv=gv") -- Kill

k.keymap({'n', 'x'}, 'p', 'P') -- P will paste before cursor & over in x without yanking
k.keymap({'i'}, '<C-p>', '<C-o>p') -- P will paste before cursor & over in x without yanking

-- Commands
k.keymap_leader({'x', 'n'}, 'u', "\"+y") -- Copy to clipboard
k.keymap_leader({'x', 'n'}, 'U', "\"+Y") -- Copy to clipboard
k.keymap_leader({'x', 'n'}, 'p', "\"+P") -- Paste to clipboard
k.keymap_leader({'n', 'v'}, 's', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
k.keymap_leader({'n', 'v'}, 'iu', function() require("utils").insert_v4({}) end)
k.keymap_leader({'n', 'v'}, 't', 'gg=G')
k.keymap_leader({'n'}, 'r', vim.lsp.buf.rename) 
k.keymap_leader({'n', 'x'}, 'd', function ()
  require("telescope.builtin")
    .live_grep({ cwd = require("telescope.utils").buffer_dir() })
end
)

---- Jump quickfix
-- keymap('n', '<leader>k', "<cmd>lnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<leader>j', "<cmd>lprev<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-k>', "<cmd>cnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-j>', "<cmd>cprev<CR>zz") -- Paste over (don't add to clipboard)

-- Window
k.keymap('n', 'wsb', '<CMD>split<CR>')
k.keymap('n', 'wko', '<CMD>only<CR>')
k.keymap('n', 'wks', '<CMD>close!<CR>')
k.keymap('n', 'wo', '<C-w>W')
k.keymap('n', 'wkb', '<CMD>bd<CR>')

k.keymap('n', 'wq', function() require("trouble").toggle("quickfix") end) 

-- Lsp --
-- keymap('n', '<C-l>f', vim.lsp.buf.code_action)

-- keymap('n', ',', '<C-o>' ) -- Jump back
k.keymap('n', '=', 'g;' ) -- Jump back to last change
k.keymap('n', '{', 'g,' ) -- Jump recent change
k.keymap({'n', 'v'}, '<Tab>', '==' ) 

k.keymap('n', '.', '<CMD>Glance definitions<CR>') 
k.keymap('n', 'or', '<CMD>Glance references<CR>') 
k.keymap('n', 'ot', '<CMD>Glance type_definitions<CR>') 
k.keymap('n', 'oi', '<CMD>Glance implementations<CR>') 
k.keymap({'n', 'x'}, 'ou', function() vim.diagnostic.open_float(nil, { focus = false }) end)


k.keymap('n', 'ou', vim.lsp.buf.hover) 
k.keymap('n', 'oe', function ()
  vim.diagnostic.open_float(0)
end) 
-- keymap('n', 'ot', vim.lsp.buf.type_definition) 
-- k.keymap('n', 'ow', vim.lsp.buf.rename) 

k.keymap({'n', 'x'}, '<C-f>', require("plugins.telescope.occur").occur_in_file)
k.keymap({'n', 'x'}, 'of', require("telescope.builtin").find_files)
k.keymap({'n', 'x'}, 'od', require("telescope.builtin").live_grep)
k.keymap({'n', 'x'}, 'os', require("telescope.builtin").buffers)
k.keymap({'n', 'x'}, 'oa', "<CMD>Telescope file_browser path=%:p:h select_buffer=true<CR>")
k.keymap({'n', 'x'}, 'oo', require("telescope.builtin").lsp_document_symbols)
k.keymap({'n', 'x'}, 'ows', require("telescope.builtin").lsp_workspace_symbols)
k.keymap({'n', 'x'}, 'opp', vim.lsp.buf.list_workspace_folders)
k.keymap({'n', 'x'}, 'oq', function()
  vim.lsp.buf.format {
    filter = function(client) return client.name ~= "tsserver" end
  }
end)

-- keymap({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Common
k.keymap({'i', 'v', 'c', 't', 's', 'o', 'x'}, '<C-o>', '<Esc>') -- Cancel

k.keymap('n', '<C-_>', '<Plug>(comment_toggle_linewise_current)j') -- Comment line, maps to C-/

k.keymap('n', '<C-e>', function() vim.diagnostic.goto_next() end)
k.keymap('n', '<C-q>', '<CMD>x<CR>')
k.keymap('n', '<S-C-e>', function() vim.diagnostic.goto_prev() end)
k.keymap('n', '<C-s>', '<CMD>w<CR>') -- Save file
k.keymap('i', '<C-s>', '<C-o>:w<CR>') -- Save file

