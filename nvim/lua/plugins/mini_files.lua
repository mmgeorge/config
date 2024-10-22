return {
  {
    'echasnovski/mini.files',
    version = '*',
    keys = {
      {
        "or",
        -- "<cmd>:lua MiniFiles.open()<CR>",
        function ()
          require"mini.files".open(vim.api.nvim_buf_get_name(0), false) 
        end,
        mode = { "n", "x" },
        desc = "Explore Files",
      }, 
    },
    config = function ()
      require"mini.files".setup({
        -- Customization of shown content
        content = {
          -- Predicate for which file system entries to show
          filter = function (fs_entry)
            return not string.find(fs_entry.name, "%.js")
          end,
      
          -- What prefix to show to the left of file system entry
          prefix = nil,
          -- In which order to show file system entries
          sort = nil,
        },

        -- Module mappings created only inside explorer.
        -- Use `''` (empty string) to not create one.
        mappings = {
          -- close       = 'q',
          close       = '<Esc>',
          go_in       = '<CR>',
          go_in_plus  = '',
          go_out      = '<BS>',
          go_out_plus = '',
          mark_goto   = "",
          mark_set    = '',
          reset       = '',
          reveal_cwd  = '@',
          show_help   = 'g?',
          synchronize = '<C-s>',
          trim_left   = '<',
          trim_right  = '>',
        },

        -- General options
        options = {
          -- Whether to delete permanently or move into module-specific trash
          permanent_delete = true,
          -- Whether to use for editing directories
          use_as_default_explorer = true,
        },

        -- Customization of explorer windows
        windows = {
          -- Maximum number of windows to show side by side
          max_number = 5,  --math.huge,
          -- Whether to show preview of file/directory under cursor
          preview = false,
          -- Width of focused window
          width_focus = 25,
          -- Width of non-focused window
          width_nofocus = 25,
          -- Width of preview window
          width_preview = 25,
        },
      }) 
    end
  },
} 

