return {
   {
    'nvim-telescope/telescope.nvim', tag = '0.1.6',
    dependencies = { 'nvim-lua/plenary.nvim' },
    lazy = false, 
    config = function()
      local actions = require "telescope.actions"
      local sorters = require('telescope.sorters')

      -- Sorters in telescope do more than just sorting, they perform the actual filter
      -- and search & highlight as well
      -- See <https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/sorters.lua>
      --
      -- Directly sort results, bypassing any fuzzy matching.
      -- local contains_exactly = sorters.Sorter:new{
      --    scoring_function = function(_, prompt, line)
      --       local prompt_lower = prompt:lower()
      --       local line_lower = line:lower()
      --       local contains_string = line_lower:find(prompt_lower, 1, true)

      --       -- Negative numbers mean we filter the result
      --       if not contains_string then
      --          return -1 
      --       end

      --       return 1
      --    end,

      --    highlighter = function(_, prompt, line)
      --       return { 1, 2, 3, 4, 5, 6, 7, 8, 9 }
      --    end
      -- }
      
      require('telescope').setup({
          defaults = {
             disable_devicons = false,
            winblend = 0,
            layout_strategy = "vertical", 
            layout_config = {
              --height = vim.o.lines, -- Maximally available lines
              --width = vim.o.columns, -- Maximally available columns

              preview_cutoff=1,
            },
            mappings = {
              i = {
                ["<C-g>"] = actions.close, 
                ["<C-k>"] = actions.close, 
              },
              n = {
                ["<C-g>"] = actions.close, 
                ["<C-k>"] = actions.close, 
              }
            }
          },
          pickers = {
            find_files = {
              theme = "ivy"
            },
            current_buffer_fuzzy_find = {
            winblend = 0,
            sorter = sorters.get_substr_matcher(),
              theme = "ivy",
              previewer = false,
              layout_config = {
                height = .3, -- 0.4,
                preview_cutoff=0,
              },        
            },
            buffers = {
              theme = "ivy",
               disable_devicons = true,
              previewer = false,
              layout_config = {
                height = 0.4,
                preview_cutoff=0,
              },        
            }
          }
                                })

    end,
   },

   {   
      "nvim-telescope/telescope-file-browser.nvim",
      lazy = false, 
      dependencies = {
         "nvim-tree/nvim-web-devicons",
         "nvim-telescope/telescope.nvim",
         "nvim-lua/plenary.nvim"
      },
      config = function ()
         local actions = require( "telescope.actions")
         local fb_actions = require( "telescope").extensions.file_browser.actions

         require("telescope").setup ({
            extensions = {
               file_browser = {
                  disable_devicons = false,
                  -- grouped = true, 
                  theme = "ivy",
                  -- -- File tree depth to display, false for unlimited depth
                  depth = 1,
                  auto_depth = false,
                  select_buffer = true,
                  hide_parent_dir = true,
                  hidden = {
                     file_browser = true,
                     folder_browser = true, 
                  },
                  no_ignore = false,
                  follow_symlinks = false,
                  collapse_dirs = true,
                  quiet = true,
                  dir_icon = "",
                  dir_icon_hl = "",
                  display_stat = false, 
                  -- display_stat = {
                  --    date = true,
                  --    size = false,
                  --    mode = false, 
                  -- },
                  git_status = true,
                  prompt_path = false,
                  use_fd = false, 
                  -- disables netrw and use telescope-file-browser in its place
                  hijack_netrw = true,
                  mappings = {
                     ["i"] = {
                        ["<Left>"] = fb_actions.goto_parent_dir,
                        ["<Right>"] = fb_actions.change_cwd,
                        ["<C-r>"] = fb_actions.rename, 
                        ["<C-k>"] = fb_actions.remove, 
                        ["<C-m>"] = fb_actions.move, 
                        ["<C-n>"] = fb_actions.create, 
                        ["<C-p>"] = fb_actions.create_from_prompt, 
                        ["<CR>"] = actions.select_default, 
                        ["<C-g>"] = actions.close, 
                        -- your custom insert mode mappings
                     },
                     ["n"] = {
                        ["<CR>"] = actions.select_default, 
                        -- your custom normal mode mappings
                     },
                  },
               },
            },
             })
            require("telescope").load_extension("file_browser")
      end
   }
}

