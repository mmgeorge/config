return {
  -- Lsp package manager
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end
  },

  -- Bridge mason.nvim & lspconfig
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        automatic_installation = true,
        ensure_installed = {
          "jsonls",
          "lua_ls",
          "eslint",
          -- "oxlint",
          "vtsls",
          "terraformls",
          "cssls",
          "html",
          "tailwindcss",
          "taplo"
        }

      })
    end
  },

  -- Install mason tools automatically --
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    config = function()
      require("mason-tool-installer").setup({
        -- ensure_installed = { "stylua", "prettierd" },
        ensure_installed = { "prettierd" },
        auto_update = true,
        run_on_start = true,
        debounce_hours = 5,
      })
    end
  },

  -- Setup servers via lspconfig --
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      'saghen/blink.cmp',
      'yioneko/nvim-vtsls'
    },
    config = function()
      local capabilities = require('blink.cmp').get_lsp_capabilities()

      -- Disable semantic tokens
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })

      vim.lsp.config('terraformls', {
        capabilities = capabilities,
      })

      vim.lsp.config('lua_ls', {
        capabilities = capabilities,
      })

      vim.lsp.config('jsonls', {
        capabilities = capabilities,
      })

      vim.lsp.enable('jsonls')

      vim.lsp.config('html', {
        capabilities = capabilities,
      })

      vim.lsp.enable('html')

      vim.lsp.config('taplo', {
        capabilities = capabilities,
      })

      vim.lsp.enable('taplo')

      vim.lsp.config('rust_analyzer', {
        -- We install this from rustup rather than Mason
        cmd = { "rust-analyzer" },
        -- This slows down saving quite a bit
        -- on_attach = function(client, bufnr)
        --   vim.api.nvim_create_autocmd("BufWritePre", {
        --     buffer = bufnr,
        --     callback = function() vim.lsp.buf.format() end,
        --     group = format_sync_grp,
        --   })
        -- end,
        capabilities = capabilities,
        settings = {
          ["rust-analyzer"] = {
            -- numThreads = 8,
            cargo = {
              allTargets = false,
              -- features = {},
              -- buildScripts = {
              -- enable = false,
              -- rebuildOnSave = false
              -- }
            },
            check = {
              allTargets = false,
              workspace = false,
            },
            cachePriming = {
              enable = false,
              -- numThreads = 8
            },
            procMacro = {
              enable = true,
              ignore = {}
            }

          }
        }
      })

      vim.lsp.enable("rust_analyzer", true)

      require("lspconfig.configs").vtsls = require("vtsls").lspconfig
      vim.lsp.config('vtsls', {
        capabilities = capabilities,
        -- on_attach = function (client, buf)
        --   client.server_capabilities.semanticTokensProvider = nil
        -- end,
        root_markers = { '.git' },
        settings = {
          vtsls = {
            experimental = {
              completion = {
                enableServerSideFuzzyMatch = true,
                entriesLimit = 15
              }
            }
          },
          typescript = {
            suggest = {
              -- This can slow things down
              autoImports = true
            },
            tsserver = {
              -- enableTracing = true,
              -- log = "verbose",
              useSyntaxServer = "never",
              -- To use a memory limit greater than 4 GB, use `#typescript.tsserver.nodePath#
              maxTsServerMemory = 3072,
              experimental = {
                enableProjectDiagnostics = false
              }
            },
            preferences = {
              autoImportSpecifierExcludeRegexes = {
                "node_modules/lucide-solid",
                "lucide-solid$",
                "lucide-solid/icons/index",
                "solid-js/types",
                "solid-js/web/types",
                "solid-js/store/types/server",
              }
            }
          }
        }
      })

      vim.lsp.config('tailwindcss', {
        capabilities = capabilities,
      })

      vim.lsp.config('cssls', {
        capabilities = capabilities,
      })

      -- vim.lsp.config('oxlint', {
      -- capabilities = capabilities,
      -- })

      local configs = require "lspconfig.configs"
      if not configs.slangd then
        configs.slangd = {
          default_config = {
            cmd = { "slangd", "--debug" },
            filetypes = { "slang", "hlsl" },
            root_dir = function(fname)
              return require("lspconfig").util.find_git_ancestor(fname)
            end,
            single_file_support = true,
          },
        }

        vim.lsp.config('slangd', {
          capabilities = capabilities,
        })
      end

      vim.lsp.config("eslint", {
        capabilities = capabilities,
        settings = {
          codeAction = {
            disableRuleComment = {
              enable = true,
              location = "separateLine"
            },
            showDocumentation = {
              enable = true
            }
          },
          -- codeActionOnSave = {
          --   enable = true,
          --   mode = "all"
          -- },
          -- format = true,
          problems = {
            shortenToSingleLine = false
          },
          -- quiet = true,
          rulesCustomizations = {
            {
              rule = 'prettier/prettier',
              severity = 'off',
            },
            {
              rule = '@typescript-eslint/no-unused-vars',
              severity = 'warn',
            }
          },
          run = "onType",
          validate = "on",
          workingDirectory = {
            mode = "location"
          },
        }
      })
    end
  },
  {
    "smjonas/inc-rename.nvim",
    config = function()
      require("inc_rename").setup({
        -- the name of the command
        cmd_name = "IncRename",
        -- the highlight group used for highlighting the identifier's new name
        hl_group = "Substitute",
        -- whether an empty new name should be previewed; if false the command preview will be cancelled instead
        preview_empty_name = false,
        -- whether to display a `Renamed m instances in n files` message after a rename operation
        show_message = true,
        -- whether to save the "IncRename" command in the commandline history (set to false to prevent issues with
        -- navigating to older entries that may arise due to the behavior of command preview)
        save_in_cmdline_history = true,
        -- the type of the external input buffer to use (the only supported value is currently "dressing")
        input_buffer_type = nil,
        -- callback to run after renaming, receives the result table (from LSP handler) as an argument
        post_hook = nil,
      })
    end
  },

  -- Log startup status --
  {
    "j-hui/fidget.nvim",
    opts = {
      -- options
    },
  },
  -- Very cool idea, but visual distracting. Make as a toggle?
  -- Could toggle, see: <https://github.com/ErichDonGubler/lsp_lines.nvim>
  -- {
  --    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  --    config = function()
  --       require("lsp_lines").setup()
  --    end,
  -- }
}
