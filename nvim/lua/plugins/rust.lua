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
      vim.g.rustaceanvim = {
        tools = {
          enable_clippy = false,
          test_executor = "background"
        },
        -- LSP configuration
        server = {
          -- cmd_env = {
          -- CARGO_TARGET_DIR = "target/check",
          -- CARGO_TARGET_DIR = ".rust-analyzer",
          -- },
          on_attach = function(client, bufnr)
            vim.api.nvim_create_autocmd("BufWritePost", {
              pattern = "*.rs",
              callback = function()
                local params = vim.lsp.util.make_range_params()
                params.context = {
                  only = { "quickfix" },
                  diagnostics = vim.tbl_map(function(d)
                    return d.user_data.lsp
                  end, vim.diagnostic.get(0)),
                  triggerKind = 1,
                }
                params.range = {
                  start = { line = 0, character = 0 },
                  ["end"] = { line = #vim.api.nvim_buf_get_lines(0, 0, -1, false), character = 0 },
                }
                vim.lsp.buf_request(0, "textDocument/codeAction", params, function(err, result, context, config)
                  for _, action in ipairs(result or {}) do
                    if action.title == "Remove all unused imports" then
                      local client = vim.lsp.get_client_by_id(context.client_id)
                      client.request("codeAction/resolve", action, function(err_resolve, resolved_action)
                        if err_resolve then
                          vim.notify(err_resolve.code .. ": " .. err_resolve.message, vim.log.levels.ERROR)
                          return
                        end
                        if resolved_action.edit then
                          vim.lsp.util.apply_workspace_edit(resolved_action.edit, client.offset_encoding)
                        end
                      end)
                      return
                    end
                  end
                end)
              end,
            })
          end,
          default_settings = {
            ['rust-analyzer'] = {
              checkOnSave = true,
              cargo = {
                -- allTargets = true,
                -- allFeatures = true,
                allTargets = true,
                allFeatures = true,
                -- extraEnv = {
                --   ["CARGO_TARGET_DIR"] = "target/check",
                --   -- ["RUSTFLAGS"] = "-Zthreads=8 -Zshare-generics=n"
                -- },
                -- noDeps = true
              },
              check = {
                -- Avoid re-running on all deps
                workspace = true,
                -- extraArgs = { "--target-dir", "target/check" },
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
