return {
  {
    'echasnovski/mini.move',
    version = '*',
    opts = {
      -- Module mappings. Use `''` (empty string) to disable one.
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = '<M-n>',
        right = '<M-i>',
        down = '<M-a>',
        up = '<M-e>',

        -- Move current line in Normal mode
        line_left = '<M-n>',
        line_right = '<M-i>',
        line_down = '<M-a>',
        line_up = '<M-e>',
      },

      -- Options which control moving behavior
      options = {
        -- Automatically reindent selection during linewise vertical move
        reindent_linewise = true,
      },
    }
  },
}
