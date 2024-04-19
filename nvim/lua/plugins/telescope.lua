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
            -- See <https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/mappings.lua>
            mappings = {
              i = {
                --["<C-g>"] = actions.close, 
                ["<C-k>"] = actions.close, 
              },
              n = {
                --["<C-g>"] = actions.close, 
                ["<C-k>"] = actions.close, 
                ["s"] = actions.move_selection_previous, 
                ["d"] = actions.move_selection_next,
                ["k"] = actions.preview_scrolling_up, 
                ["l"] = actions.preview_scrolling_down, 
                ["ok"] = actions.move_to_top, 
                ["ol"] = actions.move_to_bottom, 
                ["<tab>"] = actions.select_tab, 
              }
            }
          },
          pickers = {
            find_files = {
               theme = "ivy",
               previewer = false,
               layout_config = {
                  height = .3, -- 0.4,
                  preview_cutoff=0,
               },        
            },
            current_buffer_fuzzy_find = {
               sorter = sorters.get_substr_matcher(),
               theme = "ivy",
               previewer = false,
               layout_config = {
                  height = .3, -- 0.4,
                  preview_cutoff=0,
               },        
            },
            buffers = {
               -- Also includes other options, could provide a custom sorting function
              theme = "ivy",
              previewer = false,
              -- sorts current and last buffer to the top and selects the lastused
              sort_lastused = true,
              -- Sorts all buffers after most recent used. Not just the current & last one
              sort_mru = true,
              -- Only show buffers in the current working directory
              only_cwd = false,
              --ignore_current_buffer = true,
              --select_current = false,
              layout_config = {
                height = 0.4,
                preview_cutoff=0,
              },        
            },
            lsp_document_symbols = {
              theme = "ivy",
              previewer = false,
              show_line=true,
              layout_config = {
                height = 0.4,
                preview_cutoff=0,
              },        
            },
            lsp_workspace_symbols = {
               theme = "ivy",
               fname_width= 45,
               symbol_width = 25, 
               symbol_type_width = 8, 
               previewer = false,
               show_line=true,
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
                        ["<C-x>"] = fb_actions.remove, 
                        ["<C-m>"] = fb_actions.move, 
                        ["<C-n>"] = fb_actions.create, 
                        ["<C-p>"] = fb_actions.create_from_prompt, 
                        ["<CR>"] = actions.select_default, 
                        ["<C-g>"] = actions.close, 
                        -- your custom insert mode mappings
                     },
                     ["n"] = {
                        ["<CR>"] = actions.select_default,
                        ["a"] = fb_actions.goto_parent_dir, 
                        ["f"] = fb_actions.change_cwd, 

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

