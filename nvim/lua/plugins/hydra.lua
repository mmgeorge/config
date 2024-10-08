return {
  -- {
  --   -- "nvimtools/hydra.nvim",
  --   "anuvyklack/hydra.nvim",
  --   config = function()
  --     require('hydra').setup({
  --       -- decides what to do when a key which doesn't belong to any head is pressed
  --       --   nil: hydra exits and foreign key behaves normally, as if the hydra wasn't active
  --       --   "warn": hydra stays active, issues a warning and doesn't run the foreign key
  --       --   "run": hydra stays active, runs the foreign key    foreign_keys = nil,
  --       color = "red",
  --       -- timeout after which the hydra is automatically disabled. Calling any head
  --       -- will refresh the timeout
  --       --   true: timeout set to value of 'timeoutlen' (:h 'timeoutlen')
  --       --   5000: set to desired number of milliseconds
  --       timeout = false,
  --       -- when true, summon the hydra after pressing only the `body` keys. Normally a head is
  --       -- required
  --       invoke_on_body = true,
  --       -- see :h hydra-hint-hint-configuration
  --       hint = {
  --         show_name = true,
  --         position = { "bottom" },
  --         offset = 0,
  --         float_opts = { },
  --       },
  --       -- see :h hydra-hooks
  --       on_enter = nil,
  --       on_exit = nil,
  --       on_key = nil,
  --     })
  --
  --     local hydra = require("hydra")
  --     hydra({
  --       -- string? only used in auto-generated hint
  --       name = "Hydra's name",
  --
  --       -- string | string[] modes where the hydra exists, same as `vim.keymap.set()` accepts
  --       mode = "n",
  --
  --       config = {
  --         exit = true
  --       },
  --
  --       -- string? key required to activate the hydra, when excluded, you can use
  --       -- Hydra:activate()
  --       body = "<leader>o",
  --
  --       -- these are explained below
  --       -- hint = [[ ... ]],
  --       -- config = { ... },
  --       heads = { 
  --         { "r", "<cmd>RustLsp debug<CR>" },
  --         { "o", "i" }
  --       },
  --     })
  --
  --   end
  -- }
}
