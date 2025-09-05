
return {
  -- {
  --   "rachartier/tiny-inline-diagnostic.nvim",
  --   event = "VeryLazy", -- Or `LspAttach`
  --   priority = 1000,    -- needs to be loaded in first
  --   config = function()
  --     require('tiny-inline-diagnostic').setup({
  --       preset = "modern",
  --
  --       transparent_bg = false,         -- Set the background of the diagnostic to transparent
  --       transparent_cursorline = false, -- Set the background of the cursorline to transparent (only one the first diagnostic)
  --
  --       hi = {
  --         error = "DiagnosticError", -- Highlight group for error messages
  --         warn = "DiagnosticWarn",   -- Highlight group for warning messages
  --         info = "DiagnosticInfo",   -- Highlight group for informational messages
  --         hint = "DiagnosticHint",   -- Highlight group for hint or suggestion messages
  --         arrow = "NonText",         -- Highlight group for diagnostic arrows
  --
  --         -- Background color for diagnostics
  --         -- Can be a highlight group or a hexadecimal color (#RRGGBB)
  --         background = "CursorLine",
  --
  --         -- Color blending option for the diagnostic background
  --         -- Use "None" or a hexadecimal color (#RRGGBB) to blend with another color
  --         mixing_color = "None",
  --       },
  --
  --       options = {
  --         -- Display the source of the diagnostic (e.g., basedpyright, vsserver, lua_ls etc.)
  --         show_source = {
  --           enabled = false,
  --           if_many = false,
  --         },
  --
  --         -- Use icons defined in the diagnostic configuration
  --         use_icons_from_diagnostic = false,
  --
  --         -- Set the arrow icon to the same color as the first diagnostic severity
  --         set_arrow_to_diag_color = false,
  --
  --         -- Add messages to diagnostics when multiline diagnostics are enabled
  --         -- If set to false, only signs will be displayed
  --         add_messages = true,
  --
  --         -- Time (in milliseconds) to throttle updates while moving the cursor
  --         -- Increase this value for better performance if your computer is slow
  --         -- or set to 0 for immediate updates and better visual
  --         throttle = 20,
  --
  --         -- Minimum message length before wrapping to a new line
  --         softwrap = 30,
  --
  --         -- Configuration for multiline diagnostics
  --         -- Can either be a boolean or a table with the following options:
  --         --  multilines = {
  --         --      enabled = false,
  --         --      always_show = false,
  --         -- }
  --         -- If it set as true, it will enable the feature with this options:
  --         --  multilines = {
  --         --      enabled = true,
  --         --      always_show = false,
  --         -- }
  --         multilines = {
  --           -- Enable multiline diagnostic messages
  --           enabled = true,
  --
  --           -- Always show messages on all lines for multiline diagnostics
  --           always_show = true,
  --         },
  --
  --         -- Display all diagnostic messages on the cursor line
  --         show_all_diags_on_cursorline = true,
  --
  --         -- Enable diagnostics in Insert mode
  --         -- If enabled, it is better to set the `throttle` option to 0 to avoid visual artifacts
  --         enable_on_insert = false,
  --
  --         -- Enable diagnostics in Select mode (e.g when auto inserting with Blink)
  --         enable_on_select = false,
  --
  --         overflow = {
  --           -- Manage how diagnostic messages handle overflow
  --           -- Options:
  --           -- "wrap" - Split long messages into multiple lines
  --           -- "none" - Do not truncate messages
  --           -- "oneline" - Keep the message on a single line, even if it's long
  --           mode = "wrap",
  --
  --           -- Trigger wrapping to occur this many characters earlier when mode == "wrap".
  --           -- Increase this value appropriately if you notice that the last few characters
  --           -- of wrapped diagnostics are sometimes obscured.
  --           padding = 0,
  --         },
  --
  --         -- Configuration for breaking long messages into separate lines
  --         break_line = {
  --           -- Enable the feature to break messages after a specific length
  --           enabled = true,
  --
  --           -- Number of characters after which to break the line
  --           after = 30,
  --         },
  --
  --         -- Custom format function for diagnostic messages
  --         -- Example:
  --         -- format = function(diagnostic)
  --         --     return diagnostic.message .. " [" .. diagnostic.source .. "]"
  --         -- end
  --         format = nil,
  --
  --
  --         virt_texts = {
  --           -- Priority for virtual text display
  --           priority = 2048,
  --         },
  --
  --         -- Filter diagnostics by severity
  --         -- Available severities:
  --         -- vim.diagnostic.severity.ERROR
  --         -- vim.diagnostic.severity.WARN
  --         -- vim.diagnostic.severity.INFO
  --         -- vim.diagnostic.severity.HINT
  --         severity = {
  --           vim.diagnostic.severity.ERROR,
  --           vim.diagnostic.severity.WARN,
  --           -- vim.diagnostic.severity.INFO,
  --           -- vim.diagnostic.severity.HINT,
  --         },
  --
  --         -- Events to attach diagnostics to buffers
  --         -- You should not change this unless the plugin does not work with your configuration
  --         overwrite_events = nil,
  --       },
  --       disabled_ft = {} -- List of filetypes to disable the plugin
  --     })
  --     vim.diagnostic.config({ virtual_text = false })
  --   end
  -- },
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
          -- "ts_ls",
          "vtsls", -- imports don't work correctly...
          "ts_query_ls",
          -- "oxlint",
          "terraformls",
          "cssls",
          "html",
          "tailwindcss",
          "harper_ls"
          -- "taplo"
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
      -- vim.lsp.set_log_level(1)

      vim.diagnostic.config({
        virtual_text = {
          prefix = '●', -- Could be '●', '▎', 'x', '■', , 
        },
        float = { border = "single" },
      })

      local capabilities = require('blink.cmp').get_lsp_capabilities()

      vim.lsp.config("harper_ls", {
        capabilities = capabilities,
        settings = {
          ["harper-ls"] = {
            linters = {
              SpellCheck = true,
              SpelledNumbers = false,
              AnA = true,
              SentenceCapitalization = true,
              UnclosedQuotes = true,
              WrongQuotes = false,
              LongSentences = true,
              RepeatedWords = true,
              Spaces = true,
              Matcher = true,
              CorrectNumberSuffix = true
            },
            -- diagnosticSeverity = "warning",
            dialect = "American",
          }
        }
      })

      vim.lsp.enable('harper_ls')

      -- Disable semantic tokens
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          if client.name ~= "slangd" then
            client.server_capabilities.semanticTokensProvider = nil
          end
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

      vim.lsp.config('wesl_ls', {
        cmd = { 'wesl-ls' },
        filetypes = { 'wesl' },
        root_markers = { 'wesl.toml' },
      })

      vim.lsp.enable("wesl_ls")

      vim.lsp.enable("wgsl_analyzer")

      -- vim.lsp.config('taplo', {
      --   capabilities = capabilities,
      -- })
      --
      -- vim.lsp.enable('taplo')

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
            completion = {
              autoself = {
                enable = false
              }
            },
            -- numThreads = 8,
            cargo = {
              -- allTargets = false,
              features = "all",
              extraEnv = {
                ["CARGO_TARGET_DIR"] = "target/check",
              }
              -- buildScripts = {
              -- enable = false,
              -- rebuildOnSave = false
              -- }
            },
            check = {
              allTargets = true,
              workspace = true,
            },
            cachePriming = {
              enable = false,
              -- numThreads = 8
            },
            diagnostiscs = {
              -- For tsify
              -- https://github.com/madonoharu/tsify/issues/42#issuecomment-2088746822
              disabled = { "non_snake_case" }
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
        root_markers = { 'tsconfig.json' },
        -- root_markers = { 'package.json' },
        settings = {
          vtsls = {
            -- experimental = {
            --   completion = {
            --     enableServerSideFuzzyMatch = true,
            --     entriesLimit = 15
            --   }
            -- }
          },
          typescript = {
            suggest = {
              objectLiteralMethodSnippets = {
                enabled = false
              },
              -- This can slow things down
              autoImports = true
            },
            tsserver = {
              -- enableTracing = true,
              -- log = "verbose",
              -- useSyntaxServer = "never",
              -- To use a memory limit greater than 4 GB, use `#typescript.tsserver.nodePath#
              maxTsServerMemory = 3072,
              experimental = {
                enableProjectDiagnostics = false
              }
            },
            preferences = {
              includeCompletionsForModuleExports = true,
              autoImportSpecifierExcludeRegexes = {
                -- JSSDK
                "@webgis",
                "@amcharts",
                "dist",
                "3d",
                "node_modules",
                "mocha:",
                "mocha",
                "@testing-library",
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
      -- vim.lsp.enable("vtsls", true)


      -- vim.lsp.config('ts_ls', {
      --   capabilities = capabilities,
      --   root_markers = { 'tsconfig.json' },
      --   -- settings = {
      --   --   typescript = {
      --   --     -- suggest = {
      --   --     --   -- This can slow things down
      --   --     --   autoImports = true
      --   --     -- },
      --   --     -- tsserver = {
      --   --     --   -- enableTracing = true,
      --   --     --   -- log = "verbose",
      --   --     --   -- useSyntaxServer = "never",
      --   --     --   -- To use a memory limit greater than 4 GB, use `#typescript.tsserver.nodePath#
      --   --     --   maxTsServerMemory = 3072,
      --   --     --   experimental = {
      --   --     --     enableProjectDiagnostics = false
      --   --     --   }
      --   --     -- },
      --   --     preferences = {
      --   --       includeCompletionsForModuleExports = true,
      --   --       autoImportSpecifierExcludeRegexes = {
      --   --         "node_modules/lucide-solid",
      --   --         "lucide-solid$",
      --   --         "lucide-solid/icons/index",
      --   --         "solid-js/types",
      --   --         "solid-js/web/types",
      --   --         "solid-js/store/types/server",
      --   --       }
      --   --     }
      --   --   }
      --   -- }
      -- })
      -- vim.lsp.enable("ts_ls")

      vim.lsp.config('tailwindcss', {
        capabilities = capabilities,
      })

      vim.lsp.config('cssls', {
        capabilities = capabilities,
      })

      -- vim.lsp.config('oxlint', {
      -- capabilities = capabilities,
      -- })

      -- local configs = require "lspconfig.configs"
      -- if not configs.slangd then
      --   configs.slangd = {
      --     default_config = {
      --       cmd = { "slangd", "--debug" },
      --       filetypes = { "slang", "hlsl" },
      --       root_dir = function(fname)
      --         return require("lspconfig").util.find_git_ancestor(fname)
      --       end,
      --       single_file_support = true,
      --     },
      --   }
      -- end

      vim.lsp.config('slangd', {
        capabilities = capabilities,
        cmd = {
          "slangd",
          -- "--debug",
        },
        filetypes = { "slang", "hlsl" },
        single_file_support = false,
        root_markers = { 'lib.slang' },
        settings = {
          -- The `-style` argument to pass to clang-format
          format = {
            -- 'LLVM'
            -- 'Microsoft'
          },
          -- slangLanguageServer = {
          --   trace = {
          --     server = "messages" -- 'messages', 'verbose'
          --   }
          -- }
        }
      })

      vim.lsp.enable('slangd')

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
}
