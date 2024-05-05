local key = require("../keys").key

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
        normal = key("us"),
        normal_cur = key("uss"),
        normal_line = key("uS"),
        normal_cur_line = key("uSS"),
        visual = key("S"),
        visual_line = key("gS"),
        delete = key("us"),
        change = key("cs"),
        change_line = key("cS"),    
      }, 
      
    })
  end
}
