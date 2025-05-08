return {
  {
    'glepnir/template.nvim', 
    dependencies = { 
    },
    cmd = {'Template','TemProject'}, 
    config = function()
      require('template').setup({
        temp_dir = vim.fn.stdpath('config') .. "/templates",
        author = "Matt George",
        email = "mgeorge@esri.com",
      })

      require('template').register('{{_path_}}', function() return vim.fn.expand '%:r~:.' end)
    end
  }
}
