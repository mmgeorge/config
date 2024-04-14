return {
    'nvim-telescope/telescope.nvim', tag = '0.1.6',
    dependencies = { 'nvim-lua/plenary.nvim' },
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
              previewer = false,
              layout_config = {
                height = 0.4,
                preview_cutoff=0,
              },        
            }
          }
                                })

    end,
  }

