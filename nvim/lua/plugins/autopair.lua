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
}
