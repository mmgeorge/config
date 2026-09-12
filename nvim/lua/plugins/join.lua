return {
  'Wansmer/treesj',
  -- keys = { '<space>m', '<space>j', '<space>s' },
  dependencies = { 'nvim-treesitter/nvim-treesitter' }, -- if you install parsers with `nvim-treesitter`
  keys = {
    {
      "G",
      function ()
        if not require('forge.native_syntax').global_parser_allowed(vim.api.nvim_get_current_buf()) then
          vim.cmd('normal! G')
          return
        end
        require('treesj').toggle() 
      end,
      mode = { "n" },
      desc = "Toggle syntax join or go to final native document row",
    }
  }, 
  config = function()
    require('treesj').setup({
      ---@type boolean Use default keymaps (<space>m - toggle, <space>j - join, <space>s - split)
      use_default_keymaps = false,
      ---@type boolean Node with syntax error will not be formatted
      check_syntax_error = false,
      ---If line after join will be longer than max value,
      ---@type number If line after join will be longer than max value, node will not be formatted
      max_join_length = 500,
      ---Cursor behavior:
      ---hold - cursor follows the node/place on which it was called
      ---start - cursor jumps to the first symbol of the node being formatted
      ---end - cursor jumps to the last symbol of the node being formatted
      ---@type 'hold'|'start'|'end'
      cursor_behavior = 'hold',
      ---@type boolean Notify about possible problems or not
      notify = true,
      ---@type boolean Use `dot` for repeat action
      dot_repeat = true,
      ---@type nil|function Callback for treesj error handler. func (err_text, level, ...other_text)
      on_error = nil,
      ---@type table Presets for languages
      -- langs = {}, -- See the default presets in lua/treesj/langs
    })
  end,
}
