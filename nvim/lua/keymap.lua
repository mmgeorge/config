
-- Unmaps -
vim.keymap.set({'n', 'v'}, 'o', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'q', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'xo', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'x', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'c', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'C', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'b', '<Nop>', {})
vim.keymap.set({'n', 'v'}, 'a%', '<Nop>', {})
vim.keymap.set({'n', 'v', 'i'}, '<C-z>', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g> s', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g><C-j>', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g> j', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g><Down>', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g><C-k>', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g>ok', '<Nop>', {})
-- vim.keymap.set({'n', 'v', 'i'}, '<C-g><Up>', '<Nop>', {})
--vim.keymap.set({'n', 'v', 'i'}, '<C-g>u', '<Nop>', {})
--vim.keymap.set({'n', 'v', 'i'}, '<C-g>U', '<Nop>', {})
--vim.keymap.set({'n', 'v'}, 'g', '<Nop>', {})
--vim.keymap.set({'n', 'v'}, 'w', '<Nop>', {})


-- Basic navigation --
--  Left
vim.keymap.set({'n', 'v'}, 'f', 'w', { nowait = true, silent = true }) -- Forward word
vim.keymap.set({'n', 'v'}, 'a', 'b', { nowait = true, silent = true }) -- Backward word
vim.keymap.set({'n', 'v'}, 'd', 'j', { nowait = true, silent = true }) -- Down
vim.keymap.set({'n', 'v'}, 's', 'k', { nowait = true, silent = true }) -- Up

-- Right
vim.keymap.set({'n', 'v'}, 'k', '5<C-U>', { nowait = true, silent = true }) -- Scroll Up
vim.keymap.set({'n', 'v'}, 'l', '5<C-D>', { nowait = true, silent = true }) -- Scroll Down
-- Keeps screen in the center
--vim.keymap.set({'n', 'v'}, 'k', '5<C-U>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '5<C-D>zz', { nowait = true, silent = true }) -- Scroll Down
--vim.keymap.set({'n', 'v'}, 'k', '<C-u>zz', { nowait = true, silent = true }) -- Scroll Up
--vim.keymap.set({'n', 'v'}, 'l', '<C-d>zz', { nowait = true, silent = true }) -- Scroll Down

-- Search, keeping centered
-- vim.keymap.set('n', '<C-;>', 'nzzzv', { nowait = true, silent = true })
-- vim.keymap.set('n', '<C-;>', 'Nzzzv', { nowait = true, silent = true })


vim.keymap.set({'n', 'v'}, 'j', 'h', { nowait = true, silent = true }) -- Left
vim.keymap.set({'n', 'v'}, ';', 'l', { nowait = true, silent = true }) -- Right
vim.keymap.set({'n', 'v'}, 'm', '^', { nowait = true, silent = true }) -- Start Line
vim.keymap.set({'n', 'v'}, '/', '$', { nowait = true, silent = true }) -- End Line

-- Enter Modes
vim.keymap.set('n', '<C-i>', 'i', { nowait = true }) -- Enter insert mode
vim.keymap.set('n', 'v', 'i', { nowait = true }) -- Enter insert mode
vim.keymap.set('n', 'J', 'i', { nowait = true }) -- Enter insert mode

vim.keymap.set('n', 'K', 'O', { nowait = true }) -- Enter insert above
vim.keymap.set('n', 'L', 'o', { nowait = true }) -- Enter insert below

-- Selection
vim.keymap.set('n', 'i', 'v', { nowait = true }) -- Enter selection mode
vim.keymap.set('v', 'il', 'V', { nowait = true }) -- Select line
vim.keymap.set('n', '<A-i>l', 'V', { nowait = true }) -- Select line
vim.keymap.set('v', 'u', 'y`]', { nowait = true }) -- Copy (Kill-copy)
--vim.keymap.set('n', 'u', 'y', { nowait = true }) -- Copy (Kill-copy)
vim.keymap.set({'n', 'v'}, 'y', 'p', { nowait = true }) -- Past / Yank (in emacs lingo)
vim.keymap.set({'i'}, '<C-y>', '<C-o>p', { nowait = true }) -- Past / Yank (in emacs lingo)
vim.keymap.set('v', 'x', 'd', { nowait = true }) -- Cut

-- Window
vim.keymap.set('n', 'wsb', ':split<CR>', { })
vim.keymap.set('n', 'wko', ':only<CR>', { })
vim.keymap.set('n', 'wks', ':close!<CR>', { })
vim.keymap.set('n', 'wo', '<C-w>W', { }) 

-- Goto/Open
-- Character within a line: 
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

vim.keymap.set('n', '.', vim.lsp.buf.definition, { nowait = true }) 
vim.keymap.set('n', ',', '<C-o>' , { nowait = true }) -- Jump back
--vim.keymap.set('n', '<C-,>', 'g;' , { nowait = true }) -- Jump back to last change
vim.keymap.set('n', '=', 'g;' , { nowait = true }) -- Jump back to last change
vim.keymap.set('n', '{', 'g,' , { nowait = true }) -- Jump recent change
-- vim.keymap.set({'i'}, '<Tab>', vim.lsp.buf.completion, { })

vim.keymap.set('n', 'ou', vim.lsp.buf.hover, { nowait = true }) 
vim.keymap.set('n', 'ot', vim.lsp.buf.type_definition, { nowait = true }) 
vim.keymap.set('n', 'ow', vim.lsp.buf.rename, { nowait = true }) 

vim.keymap.set('n', 'ok', 'gg', { }) -- Top of Page
vim.keymap.set('n', 'ol', ']]', { }) -- End of page

vim.keymap.set({'n', 'v'}, '<C-f>', require("plugins.telescope.occur").occur_in_file, {})
vim.keymap.set({'n', 'v'}, 'opf', require("plugins.telescope.find_files").find_files, {})
vim.keymap.set({'n', 'v'}, 'oa', ":Telescope file_browser path=%:p:h select_buffer=true<CR>", {})
vim.keymap.set({'n', 'v'}, 'of', ":Telescope lsp_document_symbols<CR>", {})
vim.keymap.set({'n', 'v'}, 'ops', ":Telescope lsp_workspace_symbols<CR>", {})
vim.keymap.set({'n', 'v'}, 'opp', vim.lsp.buf.list_workspace_folders, {})
vim.keymap.set({'n', 'v'}, 'oq', function()
      vim.lsp.buf.format {
         filter = function(client) return client.name ~= "tsserver" end
      }
end, {})


vim.keymap.set({'n', 'v'}, 'os', require("telescope.builtin").buffers, {})
vim.keymap.set({'n', 'v'}, 'ot', require("telescope.builtin").help_tags, {})
vim.keymap.set({'n', 'v'}, 'or', require("telescope.builtin").resume, { nowait=true })

-- Text --
vim.keymap.set({'n', 'v', 'i'}, '<C-h>', 'dd', { })


-- Insert -- 
vim.keymap.set({'n', 'v'}, '<Tab>', '==', { })

-- Common
vim.keymap.set({'n', 'i', 'v'}, '<C-g>', '<Esc>', { nowait = false, unique=true }) -- Cancel
--vim.keymap.set({'n', 'i', 'v'}, '<Left>', '<Esc>', { nowait = false, unique=true }) -- Cancel
vim.keymap.set({'n', 'i', 'v', 'c', 't', 's', 'o', 'x'}, '<C-k>', '<Esc>', { nowait = true }) -- Cancel

vim.keymap.set('n', '<C-s>', ':w<CR>', { nowait = true }) -- Save file
vim.keymap.set('i', '<C-s>', '<C-o>:w<CR>', { nowait = true }) -- Save file
vim.keymap.set('n', '<C-r>', ':earlier 10f<CR>', {}) -- Revert file
vim.keymap.set('n', '<C-e>', ':q<CR>', { nowait = true }) -- Save file
-- vim.keymap.set('n', '<C-X><83>', ':q<CR>', {}) -- Does not work!
