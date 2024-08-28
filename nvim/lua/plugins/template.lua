return {
  {
    'glepnir/template.nvim', 
    dependencies = { 
       'nvim-telescope/telescope.nvim',
    },
    cmd = {'Template','TemProject'}, 
    config = function()
      require('template').setup({
        temp_dir = vim.fn.stdpath('config') .. "/templates",
        author = "Matt George",
        email = "mgeorge@esri.com",
      })

      require('template').register('{{_path_}}', function() return vim.fn.expand '%:r~:.' end)
      require("telescope").load_extension('find_template')
    end
  }
}
