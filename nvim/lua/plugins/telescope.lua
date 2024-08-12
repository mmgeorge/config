local key = require("../keys").key

return {
  {
    'nvim-telescope/telescope.nvim', -- tag = '0.1.6',
    dependencies = { 
      'nvim-lua/plenary.nvim',
      -- "nvim-tree/nvim-web-devicons",
      -- { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
      "nvim-telescope/telescope-file-browser.nvim",
    },
    -- lazy = false, 
    config = function()
      local actions = require "telescope.actions"
      local sorters = require('telescope.sorters')

      local fb_actions = require( "telescope").extensions.file_browser.actions
      local open_with_trouble = require("trouble.sources.telescope").open
      -- Sorters in telescope do more than just sorting, they perform the actual filter
      -- and search & highlight as well
      -- See <https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/sorters.lua>

      -- Directly sort results, bypassing any fuzzy matching.
      local contains_exactly = sorters.Sorter:new{
        scoring_function = function(_, prompt, line)
          local prompt_lower = prompt:lower()
          local line_lower = line:lower()
          local contains_string = line_lower:find(prompt_lower, 1, true)

          -- Negative numbers mean we filter the result
          if not contains_string then
            return -1 
          end
          --
          return 1
        end,
        --
        highlighter = function(_, prompt, line)
          return { 1, 2, 3, 4, 5, 6, 7, 8, 9 }
        end
      }

      require('telescope').setup({
        defaults = {
          layout_strategy = "vertical", 
          sorting_strategy = "ascending",
          layout_config = {
            prompt_position = "top",
            height = 20,
            --height = vim.o.lines, -- maximally available lines
            --width = vim.o.columns, -- maximally available columns

          },
          -- See <https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/mappings.lua>
          mappings = {
            [ "i" ] = {
              --["<C-g>"] = actions.close
              [key("<C-o>")] = actions.close, 
              [key("<C-k>")] = actions.move_selection_previous, 
              [key("<C-l>")] = actions.move_selection_next,
              ["<C-q>"] = actions.send_to_qflist,
            },
            [ "n" ] = {
              [key("<C-o>")] = actions.close, 
              [key("s")] = actions.move_selection_previous, 
              [key("d")] = actions.move_selection_next,
              [key("k")] = actions.move_selection_previous, 
              [key("l")] = actions.move_selection_next,
              -- ["k"] = actions.preview_scrolling_up, 
              -- ["l"] = actions.preview_scrolling_down, 
              [key("gg")] = actions.move_to_top, 
              [key("G")] = actions.move_to_bottom, 
              ["<C-q>"] = actions.send_to_qflist,
              -- ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
              -- ["<Tab>"] = actions.select_tab, 
            }
          }
        },
        pickers = {
          live_grep = {
            -- theme = "dropdown",
            layout_config = {
              prompt_position = "top",
              height = .8, -- maximally available lines
              width = .8, -- maximally available columns
            },
            previewer = true,
            -- disable_coordinates=true
          },
          find_files = {
            theme = "ivy",
            previewer = false,
            find_command = {
              'rg',
              '--files',
              '--color=never',
              '--no-heading',
              '--line-number',
              '--column',
              '--smart-case',
              '--hidden',
              '--glob',
              '!{.git/*,target/*,node_modules/*}',
              '--path-separator',
              '/',
            },
          },
          current_buffer_fuzzy_find = {
            sorter = sorters.get_substr_matcher(),
            theme = "ivy",
            previewer = false,
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
            ignore_current_buffer = true,
            --select_current = false,
          },
          lsp_document_symbols = {
            theme = "ivy",
            previewer = false,
            show_line=true,
          },
          lsp_workspace_symbols = {
            theme = "ivy",
            fname_width= 45,
            symbol_width = 25, 
            symbol_type_width = 8, 
            previewer = false,
            show_line=true,
          }

        },
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
            use_fd = true, 
            -- disables netrw and use telescope-file-browser in its place
            hijack_netrw = true,
            mappings = {
              ["i"] = {
                ["<Left>"] = fb_actions.goto_parent_dir,
                ["<Right>"] = fb_actions.change_cwd,
                [key("<C-p>")] = fb_actions.create_from_prompt, 
                ["<C-x>"] = fb_actions.remove, 
                ["<C-f>"] = fb_actions.rename, 
                ["<CR>"] = actions.select_default, 
                -- ["<C-p>"] = fb_actions.move, 
                ["<C-p>"] = fb_actions.move, 
                -- ["<C-g>"] = actions.close, 
                -- your custom insert mode mappings
              },
              ["n"] = {
                ["<CR>"] = actions.select_default,
                ["<C-p>"] = fb_actions.move, 
                [key("a")] = fb_actions.goto_parent_dir, 
                [key("f")] = fb_actions.change_cwd, 
                ["<leader>f"] = fb_actions.rename, 
                ["<C-x>"] = fb_actions.remove, 
                -- ["<C-m>"] = fb_actions.move, 
                ["<C-n>"] = fb_actions.create, 

                -- your custom normal mode mappings
              },
            },
          },
        }
      })

      -- require('telescope').load_extension('fzf')
      require("telescope").load_extension("file_browser")
    end,
  },
}

