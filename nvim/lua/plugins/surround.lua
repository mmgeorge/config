return {
  "kylechui/nvim-surround",
  version = "*", -- Use for stability; omit to use `main` branch for the latest features
  event = "VeryLazy", -- other results in key configs? C-g
  config = function()
       -- See defaults here:
       -- <https://github.com/kylechui/nvim-surround/blob/main/lua/nvim-surround/config.lua>
    require("nvim-surround").setup({
      keymaps = {
        insert = "<C-g>s",
        insert_line = "<C-g>S",
        normal = "ys",
        normal_cur = "yss",
        normal_line = "yS",
        normal_cur_line = "ySS",
        visual = "S",
        visual_line = "gS",
        delete = "xs",
        change = "cs",
        change_line = "cS",    
      } 
    })
  end
}
