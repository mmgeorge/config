return {
   {
      'altermo/ultimate-autopair.nvim',
      event={'InsertEnter','CmdlineEnter'},
      branch='v0.6', --recommended as each new version will have breaking changes
      opts={
         tabout = {
            enable= false,
            --map = "ozz", 
            --cmap = "ozz",
            hopout = true,
            multi = false, 
         }
         --Config goes here
      },
   },

--    {
--     "kylechui/nvim-surround",
--     version = "*", -- Use for stability; omit to use `main` branch for the latest features
--     --event = "VeryLazy", other results in key configs? C-g
--     config = function()
--        -- See defaults here:
--        -- <https://github.com/kylechui/nvim-surround/blob/main/lua/nvim-surround/config.lua>
--        require("nvim-surround").setup({
--              keymaps = {
--                 insert = nil,
--                 insert_line = nil,
--                 normal = nil,
--                 normal_cur = nil,
--                 normal_line = nil,
--                 normal_cur_line = nil,
--                 visual = "qq",
--                 visual_line = nil,
--                 delete = nil,
--                 change = nil,
--                 change_line = nil,
--              },
--             -- Configuration here, or leave empty to use defaults
--                                      })

       
--     end
-- }
}
