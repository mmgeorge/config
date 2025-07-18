return {
  'kevinhwang91/nvim-ufo',
  dependencies = { 'kevinhwang91/promise-async' },
  keys = {
    -- { "<leader>k", mode = { "n", "x" }, function() vim.cmd("normal! za") end, desc = "Fold Toggle" },
  },
  config = function()
    require('ufo').setup({
      provider_selector = function(bufnr, filetype, buftype)
        return { 'treesitter', 'indent' }
      end
    })
  end
}
