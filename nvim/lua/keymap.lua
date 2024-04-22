
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
vim.keymap.set({'n', 'v'}, 'o', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'q', '<Nop>', {})
-- vim.keymap.set({'n', 'v'}, 'xo', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'x', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'b', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'a%', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'w', '<Nop>', {})
vim.keymap.set({'n', 'v', 'i'}, '<C-z>', '<Nop>', {})
--vim.keymap.set({'n', 'v',}, 'w', '<Nop>', {})

vim.api.nvim_command(":unmap w")
vim.api.nvim_command(":unmap b")
vim.api.nvim_command(":unmap a%")

-- Remaps --
vim.api.nvim_command(":noremap f <cmd>lua require('spider').motion('e')<CR>") -- Move right word
vim.api.nvim_command(":noremap a <cmd>lua require('spider').motion('b')<CR>") -- Move right word
-- vim.api.nvim_command(":noremap f <cmd>require('spider').motion('w')<CR>") -- Move right word
-- vim.api.nvim_command(":noremap a b") -- Move left word
-- vim.api.nvim_command(":noremap A B") -- Move left word
vim.api.nvim_command(":noremap x d") -- Delete
vim.api.nvim_command(":noremap X D") -- Delete
vim.api.nvim_command(":noremap s k") -- Up
vim.api.nvim_command(":noremap D K") -- Up
vim.api.nvim_command(":noremap d j") -- Down
vim.api.nvim_command(":noremap D J") -- Down
vim.api.nvim_command(":noremap e a") -- Down
vim.api.nvim_command(":noremap E A") -- Down
vim.api.nvim_command(":noremap h .") -- Repeat last command
vim.api.nvim_command(":noremap r f") -- To n'th chart to the right
vim.api.nvim_command(":noremap R F") -- To n'th chart to the right
vim.api.nvim_command(":noremap . ;") -- Repeat last f or T
vim.api.nvim_command(":noremap m ^") -- Start line
vim.api.nvim_command(":noremap / $") -- End line
---- Unmap x in favor of xc 
vim.keymap.set({'n'}, 'xc', 'x', { nowait = true }) -- Delete character
---- Unmap r in favor of cx
vim.keymap.set({'n'}, 'cx', 'r', { nowait = true }) -- Replace character

-- Right --
vim.keymap.set({'n', 'v'}, 'k', '5<C-U>', { nowait = true, silent = true }) -- Scroll Up
vim.keymap.set({'n', 'v'}, 'l', '5<C-D>', { nowait = true, silent = true }) -- Scroll Down
--vim.keymap.set({'n', 'v'}, 'k', '5<C-U>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '5<C-D>zz', { nowait = true, silent = true }) -- Scroll Down
--vim.keymap.set({'n', 'v'}, 'k', '<C-u>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '<C-d>zz', { nowait = true, silent = true }) -- Scroll Down
----Enter Modes
vim.keymap.set('n', 'K', 'O', { nowait = true }) -- Enter insert above
vim.keymap.set('n', 'L', 'o', { nowait = true }) -- Enter insert below

vim.keymap.set({'n', 'v'}, 'j', 'h', { nowait = true, silent = true }) -- Left
vim.keymap.set({'n', 'v'}, ';', 'l', { nowait = true, silent = true }) -- Right

-- Search, keeping centered
vim.keymap.set('n', 'n', 'nzzzv', { nowait = true, silent = true })
vim.keymap.set('n', 'n', 'Nzzzv', { nowait = true, silent = true })

-- Text --
---- Remaps ----
vim.keymap.set({'n', 'v', 'i'}, '<C-h>', 'dd', { })
vim.keymap.set({'n', 'v'}, 'U', '<C-r>', { }) -- Redo

vim.keymap.set({'n'}, 'I', 'O<ESC>', { }) -- Insert blank line below
vim.keymap.set({'n'}, 'O', 'o<ESC>', { }) -- Insert blank line above


-------------------------------------------------------------------------------
---- VisualMode ----
-------------------------------------------------------------------------------
-- Move text up or down
vim.keymap.set({'v'}, 'L', ":m '<+1<CR>gv=gv", { }) -- Kill
vim.keymap.set({'v'}, 'K', ":m '>-2<CR>gv=gv", { }) -- Kill

-- Selection
-- vim.keymap.set('v', 'il', 'V', { nowait = true }) -- Select line -> No, just use V?
vim.keymap.set('v', 'y', "y`]", { nowait = true }) -- Copy (Kill-copy),  `] jumps to end
vim.keymap.set('v', 'y', "\"+y", { }) -- Copy to clipboard
vim.keymap.set('v', 'Y', "\"+Y", { }) -- Copy to clipboard
vim.keymap.set({'n', 'v'}, '<leader>p', "\"_dPd", { nowait = true }) -- Paste over (don't add to clipboard)
vim.keymap.set({'n', 'v'}, '<C-p>', "\"*p", { nowait = true }) -- Paste from clipboard, removing ^M
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
vim.keymap.set('n', '<C-k>', "<cmd>cnext<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)
vim.keymap.set('n', '<C-j>', "<cmd>cprev<CR>zz", { nowait = true }) -- Paste over (don't add to clipboard)

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
vim.keymap.set({'n', 'v'}, '<Leader>/', '/\\c', { nowait = true  }) -- Search
vim.keymap.set({'n', 'v'}, 'oc', 'f', { })   -- Goto character in line
vim.keymap.set('n', 'oxc', 'df', { }) -- Goto delete character in line
vim.keymap.set({'n', 'v'}, 'orc', 'F', { })   -- Reverse, Goto character in line
vim.keymap.set('n', 'oxrc', 'dF', { }) -- Goto delete character in line
-- vim.keymap.set({'n', 'v'}, '.', ';', { nowait = true, silent = true }) -- Forward character

-- Lsp --
vim.keymap.set({'n', 'v'}, 'ou',
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

vim.keymap.set({'n', 'v'}, '<C-f>', require("plugins.telescope.occur").occur_in_file, {})
vim.keymap.set({'n', 'v'}, 'of', ":Telescope find_files<CR>", {})
vim.keymap.set({'n', 'v'}, 'oa', ":Telescope file_browser path=%:p:h select_buffer=true<CR><ESC>", {})
vim.keymap.set({'n', 'v'}, 'oo', ":Telescope lsp_document_symbols<CR>", {})
vim.keymap.set({'n', 'v'}, 'ows', ":Telescope lsp_workspace_symbols<CR>", {})
vim.keymap.set({'n', 'v'}, 'opp', vim.lsp.buf.list_workspace_folders, {})
vim.keymap.set({'n', 'v'}, 'oq', function()
      vim.lsp.buf.format {
         filter = function(client) return client.name ~= "tsserver" end
      }
end, {})

vim.keymap.set({'n', 'v'}, 'os', ":Telescope buffers<CR><ESC>", {})
-- vim.keymap.set({'n', 'v'}, 'ot', require("telescope.builtin").help_tags, {})
-- vim.keymap.set({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Common
--vim.keymap.set({'n'}, '<C-g>', '<Esc>:noh<CR>', { nowait = false, unique=true, silent=true }) -- Cancel
--vim.keymap.set({'i', 'v'}, '<C-g>', '<Esc>', { nowait = false, unique=true }) -- Cancel
--vim.keymap.set({'n', 'i', 'v'}, '<Left>', '<Esc>', { nowait = false, unique=true }) -- Cancel
vim.keymap.set({'i', 'v', 'c', 't', 's', 'o', 'x'}, '<C-k>', '<Esc>', { nowait = true }) -- Cancel
vim.keymap.set({'n'}, '<C-k>', '<Esc>:noh<CR>', { nowait = true, silent=true }) -- Cancel

-- vim.keymap.set('n', '<C-->', '<Plug>(comment_toggle_linewise_current)', {}) -- Comment line, maps to C-/
vim.keymap.set('n', '<C-_>', '<Plug>(comment_toggle_linewise_current)j', {}) -- Comment line, maps to C-/
-- vim.keymap.set('n', '<C-/>', '<Plug>(comment_toggle_linewise_current)', {}) -- Comment line, maps to C-/
vim.keymap.set('n', '<C-e>', ':x<CR>', { nowait = true }) -- Save file
vim.keymap.set('n', '<C-s>', ':w<CR>', { nowait = true }) -- Save file
vim.keymap.set('i', '<C-s>', '<C-o>:w<CR>', { nowait = true }) -- Save file
-- vim.keymap.set('n', '<C-r>', ':earlier 10f<CR>', {}) -- Revert file
