
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
-- vim.keymap.set({'n', 'v'}, 'xo', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'x', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'b', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'a%', '<Nop>', {})
vim.keymap.set({'n', 'x'}, 'w', '<Nop>', {})
vim.keymap.set({'n', 'x', 'i'}, '<C-z>', '<Nop>', {})
--vim.keymap.set({'n', 'v',}, 'w', '<Nop>', {})

vim.api.nvim_command(":unmap w")
vim.api.nvim_command(":unmap b")
vim.api.nvim_command(":unmap a%")

-- Remaps --
vim.api.nvim_command(":nnoremap f <cmd>lua require('spider').motion('e')<CR>") -- Move right word
vim.api.nvim_command(":nnoremap a <cmd>lua require('spider').motion('b')<CR>") -- Move right word
vim.api.nvim_command(":nnoremap x d") -- Delete
vim.api.nvim_command(":nnoremap X D") -- Delete
vim.api.nvim_command(":nnoremap s k") -- Up
vim.api.nvim_command(":nnoremap D K") -- Up
vim.api.nvim_command(":nnoremap d j") -- Down
vim.api.nvim_command(":nnoremap D J") -- Down
vim.api.nvim_command(":nnoremap e a") -- Down
vim.api.nvim_command(":nnoremap E A") -- Down
vim.api.nvim_command(":nnoremap h .") -- Repeat last command
vim.api.nvim_command(":nnoremap r f") -- To n'th chart to the right
vim.api.nvim_command(":nnoremap R F") -- To n'th chart to the right
vim.api.nvim_command(":nnoremap . ;") -- Repeat last f or T
vim.api.nvim_command(":nnoremap m ^") -- Start line
vim.api.nvim_command(":nnoremap / $") -- End line

vim.api.nvim_command(":xnoremap f <cmd>lua require('spider').motion('e')<CR>") -- Move right word
vim.api.nvim_command(":xnoremap a <cmd>lua require('spider').motion('b')<CR>") -- Move right word
vim.api.nvim_command(":xnoremap x d") -- Delete
vim.api.nvim_command(":xnoremap X D") -- Delete
vim.api.nvim_command(":xnoremap s k") -- Up
vim.api.nvim_command(":xnoremap D K") -- Up
vim.api.nvim_command(":xnoremap d j") -- Down
vim.api.nvim_command(":xnoremap D J") -- Down
vim.api.nvim_command(":xnoremap e a") -- Down
vim.api.nvim_command(":xnoremap E A") -- Down
vim.api.nvim_command(":xnoremap h .") -- Repeat last command
vim.api.nvim_command(":xnoremap r f") -- To n'th chart to the right
vim.api.nvim_command(":xnoremap R F") -- To n'th chart to the right
vim.api.nvim_command(":xnoremap . ;") -- Repeat last f or T
vim.api.nvim_command(":xnoremap m ^") -- Start line
vim.api.nvim_command(":xnoremap / $") -- End line

vim.api.nvim_command(":onoremap f <cmd>lua require('spider').motion('e')<CR>") -- Move right word
vim.api.nvim_command(":onoremap a <cmd>lua require('spider').motion('b')<CR>") -- Move right word
vim.api.nvim_command(":onoremap x d") -- Delete
vim.api.nvim_command(":onoremap X D") -- Delete
vim.api.nvim_command(":onoremap s k") -- Up
vim.api.nvim_command(":onoremap D K") -- Up
vim.api.nvim_command(":onoremap d j") -- Down
vim.api.nvim_command(":onoremap D J") -- Down
vim.api.nvim_command(":onoremap e a") -- Down
vim.api.nvim_command(":onoremap E A") -- Down
vim.api.nvim_command(":onoremap h .") -- Repeat last command
vim.api.nvim_command(":onoremap r f") -- To n'th chart to the right
vim.api.nvim_command(":onoremap R F") -- To n'th chart to the right
vim.api.nvim_command(":onoremap . ;") -- Repeat last f or T
vim.api.nvim_command(":onoremap m ^") -- Start line
vim.api.nvim_command(":onoremap / $") -- End line

-- vim.keymap.set({'s'}, 'a', '<Nop>', {})

---- Unmap x in favor of xc 
vim.keymap.set({'n'}, 'xc', 'x', { nowait = true }) -- Delete character
---- Unmap r in favor of cx
vim.keymap.set({'n'}, 'cx', 'r', { nowait = true }) -- Replace character

-- Right --
vim.keymap.set({'n', 'x'}, 'k', '5<C-U>', { nowait = true, silent = true }) -- Scroll Up
vim.keymap.set({'n', 'x'}, 'l', '5<C-D>', { nowait = true, silent = true }) -- Scroll Down
--vim.keymap.set({'n', 'v'}, 'k', '5<C-U>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '5<C-D>zz', { nowait = true, silent = true }) -- Scroll Down
--vim.keymap.set({'n', 'v'}, 'k', '<C-u>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '<C-d>zz', { nowait = true, silent = true }) -- Scroll Down
----Enter Modes
vim.keymap.set('n', 'K', 'O', { nowait = true }) -- Enter insert above
vim.keymap.set('n', 'L', 'o', { nowait = true }) -- Enter insert below

vim.keymap.set({'n', 'x'}, 'j', 'h', { nowait = true, silent = true }) -- Left
vim.keymap.set({'n', 'x'}, ';', 'l', { nowait = true, silent = true }) -- Right

-- Search, keeping centered
vim.keymap.set('n', 'x', 'nzzzv', { nowait = true, silent = true })
vim.keymap.set('n', 'x', 'Nzzzv', { nowait = true, silent = true })

-- Text --
---- Remaps ----
vim.keymap.set({'n', 'v', 'i'}, '<C-h>', 'dd', { })
vim.keymap.set({'n', 'x'}, 'U', '<C-r>', { }) -- Redo

vim.keymap.set({'n'}, 'I', 'O<ESC>', { }) -- Insert blank line below
vim.keymap.set({'n'}, 'O', 'o<ESC>', { }) -- Insert blank line above


-------------------------------------------------------------------------------
---- VisualMode ----
-------------------------------------------------------------------------------
-- Move text up or down
vim.keymap.set({'x'}, 'L', ":m '<+1<CR>gv=gv", { }) -- Kill
vim.keymap.set({'x'}, 'K', ":m '>-2<CR>gv=gv", { }) -- Kill

-- Selection
-- vim.keymap.set('v', 'il', 'V', { nowait = true }) -- Select line -> No, just use V?
vim.keymap.set('x', 'y', "y`]", { nowait = true }) -- Copy (Kill-copy),  `] jumps to end
vim.keymap.set('x', 'y', "\"+y", { }) -- Copy to clipboard
vim.keymap.set('x', 'Y', "\"+Y", { }) -- Copy to clipboard
vim.keymap.set({'n', 'x'}, '<leader>p', "\"_dPd", { nowait = true }) -- Paste over (don't add to clipboard)
vim.keymap.set({'n', 'x'}, '<C-p>', "\"*p", { nowait = true }) -- Paste from clipboard, removing ^M
vim.keymap.set({'i'}, '<C-p>', "<C-o>\"*p", { nowait = true }) -- Paste from clipboard, removing ^M
-- vim.keymap.set('v', "<C-y>", "\"+y", { nowait = true }) -- Copy (Kill-copy),  `] jumps to end

-- Commands --
---- Replace text
vim.keymap.set({'n', 'v'}, '<leader>s', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>", { })
vim.keymap.set({'n', 'v'}, '<leader>iu', function() require("utils").insert_v4({}) end, { nowait = true })
vim.keymap.set({'n', 'v'}, '<leader>t', 'gg=G', { nowait = true })
-- vim.keymap.set({'n', 'v'}, '<leader>q', vim.lsp.buf.code_action, { nowait = true })

---- Jump quickfix
vim.keymap.set('n', '<leader>k', "<cmd>lnext<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)
vim.keymap.set('n', '<leader>j', "<cmd>lprev<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)
-- vim.keymap.set('n', '<C-k>', "<cmd>cnext<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)
-- vim.keymap.set('n', '<C-j>', "<cmd>cprev<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)

-- Window
vim.keymap.set('n', 'wsb', ':split<CR>', { })
vim.keymap.set('n', 'wko', ':only<CR>', { })
vim.keymap.set('n', 'wks', ':close!<CR>', { })
vim.keymap.set('n', 'wo', '<C-w>W', { })
vim.keymap.set('n', 'wkb', ':bd<CR>', { })

-- Buffers
vim.keymap.set('n', 'bk', ':bd<CR>', { })
vim.keymap.set('n', 'bo', ':%bd | e# <CR>', { })

-- Goto/Open
-- Character within a line: 
vim.keymap.set({'n', 'x'}, '<Leader>/', '/\\c', { nowait = true  }) -- Search
vim.keymap.set({'n', 'x'}, 'oc', 'f', { })   -- Goto character in line
vim.keymap.set('n', 'oxc', 'df', { }) -- Goto delete character in line
vim.keymap.set({'n', 'x'}, 'orc', 'F', { })   -- Reverse, Goto character in line
vim.keymap.set('n', 'oxrc', 'dF', { }) -- Goto delete character in line
-- vim.keymap.set({'n', 'v'}, '.', ';', { nowait = true, silent = true }) -- Forward character

-- Lsp --
vim.keymap.set({'n', 'x'}, 'ou',
   --vim.lsp.buf.hover,
   function() vim.diagnostic.open_float(nil, { focus = false }) end,
   { nowait = true, silent = true }) -- Forward character

vim.keymap.set('n', '<C-l>f', vim.lsp.buf.code_action, { }) -- Top of Page

vim.keymap.set('n', ',', '<C-o>' , { nowait = true }) -- Jump back
-- vim.keymap.set('n', '.', vim.lsp.buf.definition, { nowait = true }) 
vim.keymap.set('n', '.', '<CMD>Glance definitions<CR>', { nowait = true }) 
vim.keymap.set('n', '=', 'g;' , { nowait = true }) -- Jump back to last change
vim.keymap.set('n', '{', 'g,' , { nowait = true }) -- Jump recent change
vim.keymap.set({'n', 'v'}, '<Tab>', '==' , { nowait = true }) 

vim.keymap.set('n', 'or', '<CMD>Glance references<CR>', { nowait = true }) 
vim.keymap.set('n', 'ot', '<CMD>Glance type_definitions<CR>', { nowait = true }) 
vim.keymap.set('n', 'oi', '<CMD>Glance implementations<CR>', { nowait = true }) 

-- vim.keymap.set({'i'}, '<Tab>', vim.lsp.buf.completion, { })
vim.keymap.set('n', 'wq', function() require("trouble").toggle("quickfix") end, { nowait = true }) 

vim.keymap.set('n', 'ou', vim.lsp.buf.hover, { nowait = true }) 
vim.keymap.set('n', 'oe', function ()
  vim.diagnostic.open_float(0, {})
end, { nowait = true }) 
-- vim.keymap.set('n', 'ot', vim.lsp.buf.type_definition, { nowait = true }) 
vim.keymap.set('n', 'ow', vim.lsp.buf.rename, { nowait = true }) 

-- vim.keymap.set('n', 'ok', 'gg', { }) -- Top of Page
-- vim.keymap.set('n', 'ol', 'G', { }) -- End of page

vim.keymap.set({'n', 'x'}, '<C-f>', require("plugins.telescope.occur").occur_in_file, {})
vim.keymap.set({'n', 'x'}, 'of', ":Telescope find_files<CR>", {})
vim.keymap.set({'n', 'x'}, 'oa', ":Telescope file_browser path=%:p:h select_buffer=true<CR><ESC>", {})
vim.keymap.set({'n', 'x'}, 'oo', ":Telescope lsp_document_symbols<CR>", {})
vim.keymap.set({'n', 'x'}, 'ows', ":Telescope lsp_workspace_symbols<CR>", {})
vim.keymap.set({'n', 'x'}, 'opp', vim.lsp.buf.list_workspace_folders, {})
vim.keymap.set({'n', 'x'}, 'oq', function()
      vim.lsp.buf.format {
         filter = function(client) return client.name ~= "tsserver" end
      }
end, {})

vim.keymap.set({'n', 'x'}, 'os', ":Telescope buffers<CR><ESC>", {})
-- vim.keymap.set({'n', 'v'}, 'ot', require("telescope.builtin").help_tags, {})
-- vim.keymap.set({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Common
--vim.keymap.set({'n'}, '<C-g>', '<Esc>:noh<CR>', { nowait = false, unique=true, silent=true }) -- Cancel
--vim.keymap.set({'i', 'v'}, '<C-g>', '<Esc>', { nowait = false, unique=true }) -- Cancel
--vim.keymap.set({'n', 'i', 'v'}, '<Left>', '<Esc>', { nowait = false, unique=true }) -- Cancel
vim.keymap.set({'i', 'v', 'c', 't', 's', 'o', 'x'}, '<C-o>', '<Esc>', { nowait = true }) -- Cancel
vim.keymap.set({'n'}, '<C-k>', '<Esc>:noh<CR>', { nowait = true, silent=true }) -- Cancel

-- vim.keymap.set('n', '<C-->', '<Plug>(comment_toggle_linewise_current)', {}) -- Comment line, maps to C-/
vim.keymap.set('n', '<C-_>', '<Plug>(comment_toggle_linewise_current)j', {}) -- Comment line, maps to C-/
-- vim.keymap.set('n', '<C-/>', '<Plug>(comment_toggle_linewise_current)', {}) -- Comment line, maps to C-/
vim.keymap.set('n', '<C-e>', ':x<CR>', { nowait = true }) -- Save file
vim.keymap.set('n', '<C-s>', ':w<CR>', { nowait = true }) -- Save file
vim.keymap.set('i', '<C-s>', '<C-o>:w<CR>', { nowait = true }) -- Save file
-- vim.keymap.set('n', '<C-r>', ':earlier 10f<CR>', {}) -- Revert file
