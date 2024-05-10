return {
  {
    'altermo/ultimate-autopair.nvim',
    event={'InsertEnter','CmdlineEnter'},
    branch='v0.6', --recommended as each new version will have breaking changes
    opts={
      internal_pairs={-- *ultimate-autopair-pairs-default-pairs*
        {'[',']',fly=true,dosuround=true,newline=true,space=true},
        {'(',')',fly=true,dosuround=true,newline=true,space=true},
        {'{','}',fly=true,dosuround=true,newline=true,space=true},
        {'"','"',suround=true,multiline=false},
        -- {"'","'",suround=true,cond=function(fn) return not fn.in_lisp() or fn.in_string() end,alpha=true,nft={'tex'},multiline=false},
        {'`','`',cond=function(fn) return not fn.in_lisp() or fn.in_string() end,nft={'tex'},multiline=false},
        {'``',"''",ft={'tex'}},
        {'```','```',newline=true,ft={'markdown'}},
        {'<!--','-->',ft={'markdown','html'},space=true},
        {'"""','"""',newline=true,ft={'python'}},
        {"'''","'''",newline=true,ft={'python'}},
      },
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
