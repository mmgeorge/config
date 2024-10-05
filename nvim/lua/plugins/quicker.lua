return {
  -- {
  --   'junegunn/fzf', 
  --   config = function ()
  --     vim.fn['fzf#install']() 
  --   end
  -- },
  {
    'kevinhwang91/nvim-bqf', 
    config = function ()
      require('bqf').setup({
        func_map = {
          -- open the item under the cursor 
          open = "", 
          -- open the item, and close quickfix window 
          openc = "",
          -- use drop to open the item, and close quickfix window 
          drop = "",
          -- use tab drop to open the item, and close quickfix window
          tabdrop = "", 
          -- open item in new tab
          tab = "",
          -- open the item in a new tab, but stay in quickfix window, 
          tabb = "",
          -- open the item in a new tab, close qfwindow
          tabc = "",
          -- open the item in horizontal split 	<C-x>
          split = "", 
          -- open the item in vertical split 	<C-v> 
          vsplit = "",  	
          -- go to previous file under the cursor in quickfix window 	<C-p>
          prevfile = "",  	
          -- go to next file under the cursor in quickfix window 	<C-n> 
          nextfile = "", 	
          -- cycle to previous quickfix list in quickfix window 	<
          prevhist = "", 
          -- cycle to next quickfix list in quickfix window 	>
          nexthist = "",	
          -- go to last selected item in quickfix window 	'"
          lastleave = "",
          -- toggle sign and move cursor up 	<S-Tab>
          stoggleup = "",
          -- sign and move cursor down 	<Tab>
          stoggledown = "<Tab>",
          -- scroll back to original position in preview window 	zo
          pscrollorig = "",
          -- toggle preview window between normal and max size 	zp
          ptogglemode = "", 
          -- toggle preview for a quickfix list item 	p
          ptoggleitem = "", 
          -- toggle auto-preview when cursor moves
          ptoggleauto = "",
          -- create new list for signed items 	zn
          filter = "zn", 	
          -- create new list for non-signed items 	zN
          filterr = "",
          -- enter fzf mode 	zf
          fzffilter = "zf" 	        
        }
      })
      
      vim.api.nvim_set_hl(0, 'BqfPreviewBorder', { fg = "#ffffff" })
      -- vim.api.nvim_set_hl(0, 'BqfPreviewSbar', { fg = "#ffffff", bg = "#ffffff" })
    end
  },
  {
    'stevearc/quicker.nvim',

    event = "FileType qf",
    keys = {
      -- {
      --   "<Up>",
      --   function ()
      --     require"dap".step_out() 
      --   end,
      --   mode = { "n", "x" },
      --   desc = "Step over",
      -- },
    },
    config = function ()
      require("quicker").setup({
        -- Local options to set for quickfix
        opts = {
          buflisted = false,
          number = false,
          relativenumber = false,
          signcolumn = "auto",
          winfixheight = true,
          wrap = false,
        },
        -- Set to false to disable the default options in `opts`
        use_default_opts = true,
        -- Keymaps to set for the quickfix buffer
        keys = {
          { ">", "<cmd>lua require('quicker').expand()<CR>", desc = "Expand quickfix content" },
        },
        -- Callback function to run any custom logic or keymaps for the quickfix buffer
        on_qf = function(bufnr) end,
        edit = {
          -- Enable editing the quickfix like a normal buffer
          enabled = true,
          -- Set to true to write buffers after applying edits.
          -- Set to "unmodified" to only write unmodified buffers.
          autosave = "unmodified",
        },
        -- Keep the cursor to the right of the filename and lnum columns
        constrain_cursor = true,
        highlight = {
          -- Use treesitter highlighting
          treesitter = true,
          -- Use LSP semantic token highlighting
          lsp = true,
          -- Load the referenced buffers to apply more accurate highlights (may be slow)
          load_buffers = true,
        },
        -- Trim the leading whitespace from results
        trim_leading_whitespace = true,
        -- Maximum width of the filename column
        max_filename_width = function()
          return math.floor(math.min(95, vim.o.columns / 2))
        end,
        -- How far the header should extend to the right
        header_length = function(type, start_col)
          return vim.o.columns - start_col
        end,
        -- -- Map of quickfix item type to icon
        -- type_icons = {
        --   E = "?? ",
        --   W = "?? ",
        --   I = "? ",
        --   N = "? ",
        --   H = "? ",
        -- },
        -- -- Border characters
        -- borders = {
        --   vert = "?",
        --   -- Strong headers separate results from different files
        --   strong_header = "?",
        --   strong_cross = "?",
        --   strong_end = "?",
        --   -- Soft headers separate results within the same file
        --   soft_header = "?",
        --   soft_cross = "?",
        --   soft_end = "?",
        -- },
      }) 
    end
  }
}
