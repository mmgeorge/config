return {
  {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    lazy = false, -- This plugin is already lazy
    config = function ()
      vim.g.rustaceanvim = {
        -- Plugin configuration
        tools = {
          enable_clippy = false
        },
        -- LSP configuration
        server = {
          on_attach = function(client, bufnr)
            local format_sync_grp = vim.api.nvim_create_augroup("RustaceanFormat", {})

            vim.api.nvim_create_autocmd("BufWritePre", {
              buffer = bufnr,
              callback = function() vim.lsp.buf.format() end,
              group = format_sync_grp,
            }) 
            -- you can also put keymaps in here
          end,
          default_settings = {
            -- rust-analyzer language server configuration
            ['rust-analyzer'] = {
              checkOnSave = false,
              ["cargo"] = {
                ["extraEnv"] = {
                  ["CARGO_TARGET_DIR"] = "target/check",
                  -- ["RUSTFLAGS"] = "-Zthreads=8 -Zshare-generics=n"
                }
              }
            },
          },
        },
        -- DAP configuration
        dap = {
        },
      }
    end 
  }
}
