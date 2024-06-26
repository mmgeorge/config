local key = require("../keys").key

return {
  "dnlhc/glance.nvim",
  config = function()
    -- Lua configuration
    local glance = require('glance')
    local actions = glance.actions

    glance.setup({
      height = 18, -- Height of the window
      zindex = 45,

      -- By default glance will open preview "embedded" within your active window
      -- when `detached` is enabled, glance will render above all existing windows
      -- and won't be restiricted by the width of your active window
      detached = false,

      -- Or use a function to enable `detached` only when the active window is too small
      -- (default behavior)
      -- detached = function(winid)
        -- return vim.api.nvim_win_get_width(winid) < 100
      -- end,

      preview_win_opts = { -- Configure preview window options
        cursorline = true,
        number = true,
        wrap = true,
      },
      border = {
        enable = false, -- Show window borders. Only horizontal borders allowed
        -- top_char = '-',
        -- bottom_char = '-',
      },
      list = {
        position = 'right', -- Position of the list window 'left'|'right'
        width = 0.33, -- 33% width relative to the active window, min 0.1, max 0.5
      },
      theme = { -- This feature might not work properly in nvim-0.7.2
        enable = true, -- Will generate colors for the plugin based on your current colorscheme
        mode = 'auto', -- 'brighten'|'darken'|'auto', 'auto' will set mode based on the brightness of your colorscheme
      },
      mappings = {
        list = {
          [key('d')] = actions.next, -- Bring the cursor to the next item in the list
          [key('s')] = actions.previous, -- Bring the cursor to the previous item in the list
          ['<Down>'] = actions.next,
          ['<Up>'] = actions.previous,
          ['<Tab>'] = actions.next_location, -- Bring the cursor to the next location skipping groups in the list
          ['<S-Tab>'] = actions.previous_location, -- Bring the cursor to the previous location skipping groups in the list
          -- ['<C-u>'] = actions.preview_scroll_win(5),
          -- ['<C-d>'] = actions.preview_scroll_win(-5),
          -- ['v'] = actions.jump_vsplit,
          -- ['s'] = actions.jump_split,
          [key('t')] = actions.jump_tab,
          ['<CR>'] = actions.jump,
          -- ['o'] = actions.jump,
          [key('f')] = actions.open_fold,
          [key('a')] = actions.close_fold,
          -- ['<leader>l'] = actions.enter_win('preview'), -- Focus preview window
          [key('q')] = actions.close,
          [key('Q')] = actions.close,
          ['<Esc>'] = actions.close,
          ['<C-k>'] = actions.close,
          ['<C-q>'] = actions.quickfix,
          -- ['<Esc>'] = false -- disable a mapping
        },
        preview = {
          [key('Q')] = actions.close,
          ['<Tab>'] = actions.next_location,
          ['<S-Tab>'] = actions.previous_location,
          ['<leader>' .. key("l")] = actions.enter_win('list'), -- Focus list window
        },
      },
      hooks = {
        -- Don't open glance when there is only one result instead jump to that location
        before_open = function(results, open, jump, method)
          if #results == 1 then
            jump(results[1]) -- argument is optional
          else
            open(results) -- argument is optional
          end
        end,
      },
      -- folds = {
        -- fold_closed = 's',
        -- fold_open = '?',
        -- folded = true, -- Automatically fold list on startup
      -- },
      -- indent_lines = {
        -- enable = true,
        -- icon = '|',
      -- },
      winbar = {
        enable = true, -- Available strating from nvim-0.8+
      },
      use_trouble_qf = false -- Quickfix action will open trouble.nvim instead of built-in quickfix list window
    })
  end,
}
