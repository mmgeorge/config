return {
  'Bekaboo/dropbar.nvim',
  -- optional, but required for fuzzy finder support
  dependencies = {
    'nvim-telescope/telescope-fzf-native.nvim'
  },
  opts = {
    bar = {
      -- Disable all put path sources
      sources = function(buf, _) 
        local sources = require('dropbar.sources')
        return { sources.path  }
      end
    }
  }
}
