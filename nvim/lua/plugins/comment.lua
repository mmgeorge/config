return {
  {
    'numToStr/Comment.nvim',
    lazy = false,
    opts = {
      ---Add a space b/w comment and the line
      padding = true,
      ---Whether the cursor should stay at its position
      sticky = true,
      ---Lines to be ignored while (un)comment
      ignore = nil,
      ---LHS of toggle mappings in NORMAL mode
      toggler = {
        ---Line-comment toggle keymap
        line = 'gcc',
        ---Block-comment toggle keymap
        block = 'gbc',
      },
      ---LHS of operator-pending mappings in NORMAL and VISUAL mode
      opleader = {
        ---Line-comment keymap
        line = 'gc',
        ---Block-comment keymap
        block = 'gb',
      },
      ---LHS of extra mappings
      extra = {
        ---Add comment on the line above
        above = 'gcO',
        ---Add comment on the line below
        below = 'gco',
        ---Add comment at the end of line
        eol = 'gcA',
      },
      ---Enable keybindings
      ---NOTE: If given `false` then the plugin won't create any mappings
      mappings = {
        ---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
        basic = true,
        ---Extra mapping; `gco`, `gcO`, `gcA`
        extra = true,
      },
      ---Function to call before (un)comment
      pre_hook = nil,
      ---Function to call after (un)comment
      post_hook = nil,
    }
  }  


  -- { 'echasnovski/mini.comment',
  --   version = '*',
  --   config = function()
  --      require("mini.comment").setup({
  --            -- options which control module behavior
  --            options = {
  --               -- function to compute custom 'commentstring' (optional)
  --               custom_commentstring = nil,
  --               -- whether to ignore blank lines when commenting
  --               ignore_blank_line = false,
  --               -- whether to recognize as comment only lines without indent
  --               start_of_line = false,
  --               -- whether to force single space inner padding for comment parts
  --               pad_comment_parts = true,
  --            },
  --
  --            -- module mappings. use `''` (empty string) to disable one.
  --            mappings = {
  --               -- toggle comment (like `gcip` - comment inner paragraph) for both
  --               -- normal and visual modes
  --               -- comment_line = '<c-_>',
  --               -- toggle comment on current line
  --               -- comment_line = 'i/',
  --               -- toggle comment on visual selection
  --               comment_visual = 'i/',
  --               -- define 'comment' textobject (like `dgc` - delete whole comment block)
  --               -- works also in visual mode if mapping differs from `comment_visual`
  --               textobject = '',
  --            },
  --
  --            -- hook functions to be executed at certain stage of commenting
  --            hooks = {
  --               -- before successful commenting. does nothing by default.
  --               pre = function() end,
  --               -- after successful commenting. does nothing by default.
  --               post = function() end,
  --            },
  --   })
  --   end
  -- },
}
