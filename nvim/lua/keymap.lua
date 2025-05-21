local k = require("keys")
local utils = require("utils")

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
vim.keymap.set({ 'n', 'x' }, 'B', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'o', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'q', '<Nop>', {})
-- keymap({'n', 'v'}, 'xo', '<Nop>')
vim.keymap.set({ 'n', 'x' }, 'x', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'b', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'a%', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'w', '<Nop>', {})
vim.keymap.set({ 'n', 'x', 'i' }, '<C-z>', '<Nop>', {})

vim.keymap.del("n", 'gri', {})
vim.keymap.del("n", 'grr', {})
vim.keymap.del({ "n", "x" }, 'gra', {})
vim.keymap.del("n", 'grn', {})
-- vim.keymap.del({'n', 'x'}, 'grr', {})
-- vim.keymap.del({'n', 'x'}, 'grn', {})
-- vim.keymap.del({'n', 'x'}, 'gra', {})

--keymap({'n', 'v',}, 'w', '<Nop>')

vim.api.nvim_command(":unmap w")
vim.api.nvim_command(":unmap b")
vim.api.nvim_command(":unmap a%")

local se = "<cmd>lua require('spider').motion('e')<CR>"
local sb = "<cmd>lua require('spider').motion('b')<CR>"
local up = '5<C-U>'
local down = '5<C-e>'
local forward = '<C-i>'
local back = '<C-o>'

for i, v in ipairs(k.keys) do
  if v ~= '' then
    k.uremap(k.qwerty[i], v)
  end
end

vim.keymap.del("v", 'p', {})
vim.keymap.del("v", 'P', {})
vim.keymap.del("n", "<C-w><C-d>")
vim.keymap.del("n", "<C-w>d")

vim.keymap.set("v", 'p', function() require("treesitter_smart_select").select_parent() end)
vim.keymap.set("v", 'P', function() require("treesitter_smart_select").undo_select_parent() end)

vim.keymap.set({ 'n', 'x' }, '<iw>', '<Nop>', {})
vim.keymap.set({ 'n', 'x' }, 'S', '<Nop>', {})

-- Semantic keymaps
vim.keymap.set({ 'n', 'x' }, '<C-a>', 'ggVG', {})

vim.keymap.set({ 'n', 'x' }, '<Tab>', '==', {})
vim.keymap.set({ 'i' }, '<Tab>', '<C-f>', {})

vim.keymap.set({ 'n', 'x' }, '<C-p>', '<C-v>')

k.keymap('n', ',', back)
k.keymap('n', '<', forward)
k.keymap({ 'n', 'x' }, 'k', up)
k.keymap({ 'n', 'x' }, 'l', down)

vim.api.nvim_command(":nnoremap " .. '(' .. ' ' .. '%')
vim.api.nvim_command(":xnoremap " .. '(' .. ' ' .. '%')
vim.api.nvim_command(":onoremap " .. '(' .. ' ' .. '%')
vim.keymap.set({ 'n', 'x' }, '(', '%')

vim.api.nvim_command(":nnoremap " .. ')' .. ' ' .. '%')
vim.api.nvim_command(":xnoremap " .. ')' .. ' ' .. '%')
vim.api.nvim_command(":onoremap " .. ')' .. ' ' .. '%')
-- vim.keymap.set({ 'n', 'x' }, ')', '%')


-- vim.keymap.set({ 'n', 'x' }, 'p(', 'v%')

k.remap('f', se)
k.remap('a', sb)
k.remap('j', 'h') -- Left
k.remap(';', 'l') -- Right

k.keymap({ 'n' }, 'c/', 'cc')
k.keymap({ 'n' }, 'cc', 'ciw')
-- k.keymap({ 'n' }, 'vv', 'viw')
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

k.keymap({ 'n', 'v', 'i' }, '<C-h>', 'dd')
k.keymap({ 'n', 'x' }, 'Z', '<C-r>') -- Redo

-- vim.keymap.set({'n'}, '<CR>', 'O<ESC>') -- Insert blank line below
k.keymap({ 'n' }, 'I', 'O<ESC>') -- Insert blank line below
k.keymap({ 'n' }, 'O', 'o<ESC>') -- Insert blank line above

-- Move text up or down
k.keymap({ 'x' }, 'L', "<CMD>m '<+1<CR>gv=gv") -- Kill
k.keymap({ 'x' }, 'K', "<CMD>m '>-2<CR>gv=gv") -- Kill

k.keymap({ 'n', 'x' }, 'p', 'P')               -- P will paste before cursor & over in x without yanking
-- k.keymap({'i'}, '<C-p>', '<C-o>p') -- P will paste before cursor & over in x without yanking
k.keymap({ 'i' }, '<C-p>', '<C-o>P')           -- P will paste before cursor & over in x without yanking
k.keymap({ 'x' }, 'u', 'ygv<Esc>')             -- Yank selection and move to the end
-- k.keymap({'n', 'x'}, 'uu', 'yy') -- Yank selection and move to the end
vim.keymap.set({ 'n' }, "G",
  function() require('treesj').toggle() end, { nowait = true, silent = false }) -- Join lines
vim.keymap.set({ 'x', 'n' }, "C", "J", { nowait = true, silent = false })       -- Join lines
-- vim.keymap.set({'n'}, "kM", ":Neogen<CR>", { nowait=true, silent=false }) -- Create doc

-- Surround
vim.keymap.set({ 'x' }, "'", "qqc''<Esc>Pq", { nowait = true, silent = false })
vim.keymap.set({ 'x' }, '"', 'qqc""<Esc>Pq', { nowait = true, silent = false })
vim.keymap.set({ 'x' }, '(', 'qqc()<Esc>Pq', { nowait = true, silent = false })
vim.keymap.set({ 'x' }, '<', 'qqc<><Esc>Pq', { nowait = true, silent = false })
vim.keymap.set({ 'x' }, '[', 'qqc[]<Esc>Pq', { nowait = true, silent = false })
vim.keymap.set({ 'x' }, '{', 'qqc{}<Esc>Pq', { nowait = true, silent = false })

-- Surround
-- vim.keymap.set({'n'}, "ls'", 'viwqqc""<Esc>Pq', { nowait=true, silent=false })
-- vim.keymap.set({'n'}, 'lsa', 'viwqqc()<Esc>Pq', { nowait=true, silent=false })
-- vim.keymap.set({'n'}, 'ls,', 'viwqqc<><Esc>Pq', { nowait=true, silent=false })
-- vim.keymap.set({'n'}, 'ls[', 'viwqqc[]<Esc>Pq', { nowait=true, silent=false })
-- vim.keymap.set({'n'}, 'ls.', 'viwqqc{}<Esc>Pq', { nowait=true, silent=false })

vim.keymap.set({ 'n' }, 'zz', 'zf', { nowait = true, silent = false }) -- Add spell check word

-- Spellcheck
vim.keymap.set({ 'n' }, 'Sa', 'zg', { nowait = true, silent = false }) -- Add spell check word
-- vim.keymap.set({'n'}, 'Sr', 'z=', { nowait=true, silent=false })
-- vim.keymap.set({'x'}, '(', 'S)')

-- Commands
k.keymap_leader({ 'x', 'n' }, 'u', "\"+y") -- Copy to clipboard
k.keymap_leader({ 'x', 'n' }, 'U', "\"+Y") -- Copy to clipboard
k.keymap_leader({ 'x', 'n' }, 'p', "\"+P") -- Paste to clipboard
k.keymap_leader({ 'n', 'v' }, 's', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
-- k.keymap_leader({'n', 'v'}, 's', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
-- k.keymap_leader({'n', 'v'}, 'iu', function() require("utils").insert_v4({}) end)
-- k.keymap_leader({'n', 'v'}, 't', 'gg=G')

vim.keymap.set("n", "<leader>f", function()
  return ":IncRename " .. vim.fn.expand("<cword>")
end, { expr = true })
-- k.keymap_leader({'n'}, 'r', vim.lsp.buf.rename)

-- k.keymap_leader({'n', 'x'}, 'd', function ()
--   require("telescope.builtin")
--     .live_grep({ cwd = require("telescope.utils").buffer_dir() })
-- end
-- )

-- Buffer
-- vim.keymap.set({'n', 'x'}, "<leader>" .. "bj", ":bd<cr>", { nowait=true, silent=false })

---- Jump quickfix
-- keymap('n', '<leader>k', "<cmd>lnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<leader>j', "<cmd>lprev<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-k>', "<cmd>cnext<CR>zz") -- Paste over (don't add to clipboard)
-- keymap('n', '<C-j>', "<cmd>cprev<CR>zz") -- Paste over (don't add to clipboard)

-- Window
k.keymap('n', 'wsl', '<CMD>split<CR>')
k.keymap('n', 'wo', '<C-w>W')
-- k.keymap('n', 'wko', '<CMD>only<CR>')
-- k.keymap('n', 'wks', '<CMD>close!<CR>')
-- k.keymap('n', 'wkb', '<CMD>bd<CR>')
vim.keymap.set({ 'n' }, 'gko', '<CMD>only<CR>', {})
vim.keymap.set({ 'n' }, 'gks', '<CMD>close!<CR>', {})
vim.keymap.set({ 'n' }, 'gkb', '<CMD>bd<CR>', {})

k.keymap('n', 'wq', function() require("trouble").toggle("quickfix") end)

-- Lsp --
-- keymap('n', '<C-l>f', vim.lsp.buf.code_action)

-- keymap('n', ',', '<C-o>' ) -- Jump back
k.keymap('n', '=', 'g;') -- Jump back to last change
k.keymap('n', '{', 'g,') -- Jump recent change
k.keymap({ 'n', 'v' }, '<Tab>', '==')
vim.keymap.set({ 'n' }, '<CR>', 'i<CR><ESC>')

k.keymap('n', '.', '<CMD>Glance definitions<CR>')
k.keymap('n', 'or', '<CMD>Glance references<CR>')
k.keymap('n', 'ot', '<CMD>Glance type_definitions<CR>')
k.keymap({ 'n', 'x' }, 'ou', function() vim.diagnostic.open_float(nil, { focus = false }) end)

k.keymap('n', 'ou', vim.lsp.buf.hover)
k.keymap('n', 'oe', function()
  vim.diagnostic.open_float(0)
end)
-- keymap('n', 'ot', vim.lsp.buf.type_definition)
-- k.keymap('n', 'ow', vim.lsp.buf.rename)

-- k.keymap({'n', 'x'}, '<C-f>', require("plugins.telescope.occur").occur_in_file)
-- k.keymap({'n', 'x'}, 'of', require("telescope.builtin").find_files)
-- k.keymap({'n', 'x'}, 'od', require("telescope.builtin").live_grep)
-- k.keymap({'n', 'x'}, 'os', require("telescope.builtin").buffers)
-- k.keymap({'n', 'x'}, 'oa', "<CMD>Telescope file_browser path=%:p:h select_buffer=true<CR>")
-- k.keymap({'n', 'x'}, 'oo', require("telescope.builtin").lsp_document_symbols)
-- k.keymap({'n', 'x'}, 'ows', require("telescope.builtin").lsp_workspace_symbols)
-- k.keymap({'n', 'x'}, 'opp', vim.lsp.buf.list_workspace_folders)
-- k.keymap({'n', 'x'}, 'oq', function()
-- vim.lsp.buf.format {
-- filter = function(client) return client.name ~= "tsserver" end
-- }
-- end)

-- keymap({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Common
k.keymap({ 'i', 'v', 'c', 't', 's', 'o', 'x', 't' }, '<C-o>', '<Esc>') -- Cancel
vim.api.nvim_command(":tnoremap <ESC> <C-\\><C-n>")

k.keymap('n', '<C-_>', '<Plug>(comment_toggle_linewise_current)j') -- Comment line, maps to C-/

k.keymap('n', '<C-e>', function() vim.diagnostic.goto_next() end)
-- k.keymap('n', '<C-q>', '<CMD>x<CR>')
vim.keymap.set({ 'n' }, '<C-w>', '<CMD>qa!<CR>')
k.keymap('n', '<S-C-e>', function() vim.diagnostic.goto_prev() end)
k.keymap('n', '<C-s>', '<CMD>w<CR>')  -- Save file
k.keymap('i', '<C-s>', '<C-o>:w<CR>') -- Save file


vim.keymap.set({ 'n', 'x' }, 'B', function() require "dap".toggle_breakpoint() end, {})
vim.keymap.set({ 'n', 'x' }, '<A-a>', function() require "dap".step_over() end, {})
vim.keymap.set({ 'n', 'x' }, '<A-i>', function() require "dap".step_into() end, {})
vim.keymap.set({ 'n', 'x' }, '<A-n>', function() require "dap".step_out() end, {})
vim.keymap.set({ 'n', 'x' }, '<A-r>', function() require "dap".restart() end, {})
vim.keymap.set({ 'n', 'x' }, '<A-c>', function() require "dap".continue() end, {})

-- Custom Motions

-- Select token
vim.keymap.set({ 'n' }, 'pt', 'viw', {})
vim.keymap.set({ 'n' }, 'lt', 'yiw', {})
vim.keymap.set({ 'n' }, 'jt', 'diw', {})
vim.keymap.set({ 'n' }, 'mt', 'ciw', {})
-- vim.keymap.set({'n'}, 'pot', 'vaw', {})
-- vim.keymap.set({'n'}, 'lot', 'yaw', {})
-- vim.keymap.set({'n'}, 'jot', 'daw', {})
-- vim.keymap.set({'n'}, 'mot', 'caw', {})

-- Select "
vim.keymap.set({ 'n' }, 'po"', 'va"', {})
vim.keymap.set({ 'n' }, 'lo"', 'ya"', {})
vim.keymap.set({ 'n' }, 'jo"', 'da"', {})
vim.keymap.set({ 'n' }, 'mo"', 'ca"', {})

-- Select '
vim.keymap.set({ 'n' }, 'po\'', 'va\'', {})
vim.keymap.set({ 'n' }, 'lo\'', 'ya\'', {})
vim.keymap.set({ 'n' }, 'jo\'', 'da\'', {})
vim.keymap.set({ 'n' }, 'mo\'', 'ca\'', {})

-- Select parens
vim.keymap.set({ 'n' }, 'pua', 'vi(', {})
vim.keymap.set({ 'n' }, 'poa', 'va(', {})
vim.keymap.set({ 'n' }, 'lua', 'yi(', {})
vim.keymap.set({ 'n' }, 'loa', 'ya(', {})
vim.keymap.set({ 'n' }, 'jua', 'di(', {})
vim.keymap.set({ 'n' }, 'joa', 'da(', {})
vim.keymap.set({ 'n' }, 'mua', 'ci(', {})
vim.keymap.set({ 'n' }, 'moa', 'ca(', {})

-- Select {
vim.keymap.set({ 'n' }, 'pu.', 'vi{', {})
vim.keymap.set({ 'n' }, 'po.', 'va{', {})
vim.keymap.set({ 'n' }, 'lu.', 'yi{', {})
vim.keymap.set({ 'n' }, 'lo.', 'ya{', {})
vim.keymap.set({ 'n' }, 'ju.', 'di{', {})
vim.keymap.set({ 'n' }, 'jo.', 'da{', {})
vim.keymap.set({ 'n' }, 'mu.', 'ci{', {})

-- vim.keymap.set({ 'n' }, 'pa', '<Cmd>call search(\'[([{<]\', \'b\')<CR>v%', {})
vim.keymap.set({ 'n' }, 'pa', 'v%', {})
vim.keymap.set({ 'n' }, 'la', 'y%', {})
vim.keymap.set({ 'n' }, 'ja', 'd%', {})
vim.keymap.set({ 'n' }, 'ma', 'c%', {})
vim.keymap.set({ 'n' }, 'gca', 'gc%', { remap = true })

-- Select [
vim.keymap.set({ 'n' }, 'pu/', 'vi[', {})
vim.keymap.set({ 'n' }, 'po/', 'va[', {})
vim.keymap.set({ 'n' }, 'lu/', 'yi[', {})
vim.keymap.set({ 'n' }, 'lo/', 'ya[', {})
vim.keymap.set({ 'n' }, 'ju/', 'di[', {})
vim.keymap.set({ 'n' }, 'jo/', 'da[', {})
vim.keymap.set({ 'n' }, 'mu/', 'ci[', {})
vim.keymap.set({ 'n' }, 'mo/', 'ca[', {})

-- Select <
vim.keymap.set({ 'n' }, 'pu,', 'vi<', {})
vim.keymap.set({ 'n' }, 'po,', 'va<', {})
vim.keymap.set({ 'n' }, 'lu,', 'yi<', {})
vim.keymap.set({ 'n' }, 'lo,', 'ya<', {})
vim.keymap.set({ 'n' }, 'ju,', 'di<', {})
vim.keymap.set({ 'n' }, 'jo,', 'da<', {})
vim.keymap.set({ 'n' }, 'mu,', 'ci<', {})
vim.keymap.set({ 'n' }, 'mo,', 'ca<', {})

vim.keymap.set({ 'n', 'x' }, '<A-c>', function() require "dap".continue() end, {})
