return {
  {
    "uga-rosa/ccc.nvim",
    init = function()
      local ccc = require("ccc")
      local mapping = ccc.mapping

      ccc.setup({
        highlighter = {
          auto_enable = true,
          lsp = true
        },
        preserve = true,
        disable_default_mappings = true,
        -- https://github.com/uga-rosa/ccc.nvim/blob/main/doc/ccc.txt#L522
        mappings = {
          ["<Esc>"] = mapping.quit,
          ["<Tab>"] = mapping.cycle_output_mode,
          ["<S-Tab>"] = mapping.cycle_input_mode,
          ["h"] = mapping.increase10,
          ["r"] = mapping.decrease10,
          ["i"] = mapping.increase1,
          ["n"] = mapping.decrease1,
          ["<CR>"] = mapping.complete,
        }
      })
    end
  }
}
