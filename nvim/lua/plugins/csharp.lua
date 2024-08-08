return {
  {
    -- "jmederosalvarado/roslyn.nvim", 
    -- config = function ()
    --   require("roslyn").setup({
    --     -- dotnet_cmd = "dotnet", -- this is the default
    --     -- roslyn_version = "4.8.0-3.23475.7", -- this is the default
    --     on_attach = {},
    --     capabilities = {}
    --   })  
    -- end
  },
  -- {
  --   "seblj/roslyn.nvim", 
  --   config = function ()
  --     require("roslyn").setup({
  --       config = {
  --         -- Here you can pass in any options that that you would like to pass to `vim.lsp.start`
  --         -- The only options that I explicitly override are, which means won't have any effect of 
  --         -- setting here are:
  --         --     - `name`
  --         --     - `cmd`
  --         --     - `root_dir`
  --         --     - `on_init`
  --       },
  --       exe = {
  --         "dotnet",
  --         vim.fs.joinpath(vim.fn.stdpath("data"), "roslyn", "Microsoft.CodeAnalysis.LanguageServer.dll"),
  --       },
  --       -- NOTE: Set `filewatching` to false if you experience performance problems.
  --       -- Defaults to true, since turning it off is a hack.
  --       -- If you notice that the server is _super_ slow, it is probably because of file watching
  --       -- I noticed that neovim became super unresponsive on some large codebases, and that was because
  --       -- it schedules the file watching on the event loop.
  --       -- This issue went away by disabling that capability. However, roslyn will fallback to its own
  --       -- file watching, which can make the server super slow to initialize.
  --       -- Setting this option to false will indicate to the server that neovim will do the file watching.
  --       -- However, in `hacks.lua` I will also just don't start off any watchers, which seems to make the server
  --       -- a lot faster to initialize.
  --       filewatching = true,
  --     })    
  --   end
  -- }
  -- {
  --   "Hoffs/omnisharp-extended-lsp.nvim",
  -- }
  -- {
  --   "iabdelkareem/csharp.nvim",
  --   dependencies = {
  --     "williamboman/mason.nvim", -- Required, automatically installs omnisharp
  --     "mfussenegger/nvim-dap",
  --     -- "Tastyep/structlog.nvim", -- Optional, but highly recommended for debugging
  --   },
  --   config = function ()
  --     require("mason").setup() -- Mason setup must run before csharp, only if you want to use omnisharp
  --     require("csharp").setup()
  --   end
  -- }
}
