return {
  {
    'echasnovski/mini.clue',
    version = '*',
    config = function ()
      require"mini.clue".setup({
        -- Array of extra clues to show
        clues = {
          { mode = 'n', keys = '<Leader>d', desc = '+Debug' },
        },

        -- Array of opt-in triggers which start custom key query process.
        -- **Needs to have something in order to show clues**.
        triggers = {
          { mode = 'n', keys = 'o' },
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },
          -- { mode = 'n', keys = 'o' },
        },

        -- Clue window settings
        window = {
          -- Floating window config
          config = {},

          -- Delay before showing clue window
          delay = 200,

          -- Keys to scroll inside the clue window
          scroll_down = '<C-d>',
          scroll_up = '<C-u>',
        },
      })
    end
  },
}
