return {
  -- {
  --   'saecki/crates.nvim',
  --   event = { "BufRead Cargo.toml" },
  --   config = function()
  --     require('crates').setup({
  --       lsp = {
  --         enabled = true,
  --         -- on_attach = function(client, bufnr)
  --         --   -- the same on_attach function as for your other language servers
  --         --   -- can be ommited if you're using the `LspAttach` autocmd
  --         -- end,
  --         actions = true,
  --         completion = true,
  --         hover = true,
  --       },
  --     })
  --   end,
  -- },
  -- {
  --   'vxpm/ferris.nvim',
  --   opts = {
  --     -- your options here
  --   }
  -- }
  {
    'mrcjkb/rustaceanvim',
    version = '^6', -- Recommended
    lazy = false,   -- This plugin is already lazy
    config = function()
      -- local cfg = require('rustaceanvim.config')
      -- local codelldb_path = vim.fn.stdpath("data") .. "\\mason\\packages\\codelldb\\extension\\adapter\\codelldb"
      --
      -- vim.notify(vim.inspect(codelldb_path))

      vim.g.rustaceanvim = {
        -- Plugin configuration
        tools = {
          enable_clippy = false,
          test_executor = "background"
        },
        -- LSP configuration
        server = {
          on_attach = function(client, bufnr)
            local format_sync_grp = vim.api.nvim_create_augroup("RustaceanFormat", {})

            -- vim.api.nvim_create_autocmd("BufWritePre", {
            --   buffer = bufnr,
            --   callback = function() vim.lsp.buf.format() end,
            --   group = format_sync_grp,
            -- })
            -- you can also put keymaps in here
          end,
          default_settings = {
            ['rust-analyzer'] = {
              -- checkOnSave = false,
              cargo = {
                -- allTargets = true,
                -- allFeatures = true,
                allTargets = true,
                allFeatures = true,
                extraEnv = {
                  ["CARGO_TARGET_DIR"] = "target/check",
                  -- ["RUSTFLAGS"] = "-Zthreads=8 -Zshare-generics=n"
                },
                -- noDeps = true
              },
              check = {
                -- Avoid re-running on all deps
                workspace = true
              },
              -- numThreads = 8,
              cachePriming = {
                enable = true
              }
              -- Doesn't work... not supported in nvim?
              -- completion = {
              --   snippets = {
              --     custom = {
              --       ["myprint!"] = {
              --         ["postfix"] = "myp",
              --         ["body"] = {
              --           "println!(\"$0\", ${receiver});"
              --         },
              --         ["description"] = "println!()",
              --         ["scope"] = "expr"
              --       }
              --
              --     }
              --   }
              -- },
              -- checkOnSave = false,
            },
          },
        },
        -- DAP configuration
        dap = {
          -- adapter = cfg.get_codelldb_adapter(codelldb_path, codelldb_path),
        },
      }
    end
  }
}
