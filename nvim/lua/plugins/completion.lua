return {
  {
    "hrsh7th/nvim-cmp",
    -- event = "InsertEnter",
    dependencies = {
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-buffer", 
      "hrsh7th/cmp-path", 
      "hrsh7th/cmp-cmdline", 
      "hrsh7th/cmp-nvim-lsp", 
      "ryo33/nvim-cmp-rust",  
      "saadparwaiz1/cmp_luasnip", 
      "onsails/lspkind.nvim", -- pretty formatting
      "jmederosalvarado/roslyn.nvim" , 
      "f3fora/cmp-spell"
    },
    config = function()
      local cmp = require("cmp"); 

      local mapping = cmp.mapping.preset.insert({
        ["<Tab>"] = cmp.mapping(function(fallback)
          local cmp = require('cmp')
          if cmp.visible() then
            cmp.select_next_item()
          else
            fallback()
          end
        end, {"i","s","c",}),
        ['<C-p>'] = cmp.config.disable, 
        ["<C-k>"] = cmp.mapping.select_prev_item(),
        -- cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
        ["<C-l>"] = cmp.mapping.select_next_item(),
        ['<C-n>'] = cmp.config.disable, 
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-k>'] = cmp.mapping.abort(),
        -- Accept currently selected item. Set `select` to `false`
        -- to only confirm explicitly selected items.
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
      });

      cmp.setup({
        formatting = {
          format = require("lspkind").cmp_format({
            -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
            mode = 'symbol_text', -- show only symbol annotations
            maxwidth = 40, 
            ellipsis_char = '...', 
            show_labelDetails = true,
            -- The function below will be called before any actual modifications from lspkind
            -- so that you can provide more controls on popup customization.
            -- (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
            before = function (entry, vim_item)
              return vim_item
            end
          })
        },
        snippet = {
          expand = function(args)
            require'luasnip'.lsp_expand(args.body)
            --vim.snippet.expand(args.body)
          end
        },
        completion = {
          scrollbar = false,
          completeopt = "menu, menuone, preview, noinsert",
          -- completeopt = "menu, menuone, preview",
          keyword_length = 2, -- # of characters to trigger auto completion
        },
        window = {
          -- completion = cmp.config.window.bordered(),
          -- documentation = cmp.config.window.bordered(),   
        },
        view = {
          docs = {
            -- auto_open = false,
          } ,
          -- entries = {
          --    follow_cursor = false, 
          -- }
          --entries = 'native'
        },
        mapping = mapping, 
        sources = cmp.config.sources({
          -- { name = 'luasnip' },
          { name = 'nvim_lsp' },
          { 
            name = 'spell', 
            option = {
              keep_all_entries = false,
              enable_in_context = function(params)
                return require('cmp.config.context').in_treesitter_capture('spell')
              end,
              preselect_correct_word = true,
            },
          }
        })
      })

      cmp.setup.cmdline({ '/', '?' }, {
        mapping = mapping, 
        -- mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.insert({
          ["<Tab>"] = cmp.mapping(function(fallback)
            local cmp = require('cmp')
            if cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, {"i","s","c",}),
          ['<C-p>'] = cmp.config.disable, 
          ['<Down>'] = {
            c = cmp.mapping.select_next_item(),
          },
          ['<Up>'] = {
            c = cmp.mapping.select_prev_item(),
          }, 
          ['<C-k>'] = { 
            c = cmp.mapping.abort(), 
          },
        }),
        -- mapping = cmp.mapping.preset.cmdline(),
        completion = {
          completeopt = "menu, menuone, preview, noselect, noinsert",
        },
        sources = cmp.config.sources(
          {
            { name = 'path' }
          }, 
          {
            { name = 'cmdline' }
          }),
        matching = { disallow_symbol_nonprefix_matching = false }
      })

      local compare = require("cmp.config.compare");
      cmp.setup.filetype({ "rust" }, {
        sorting = {
          priority_weight = 2,
          comparators = {
            -- deprioritize `.box`, `.mut`, etc.
            require("cmp-rust").deprioritize_postfix,
            -- deprioritize `Borrow::borrow` and `BorrowMut::borrow_mut`
            require("cmp-rust").deprioritize_borrow,
            -- deprioritize `Deref::deref` and `DerefMut::deref_mut`
            require("cmp-rust").deprioritize_deref,
            -- deprioritize `Into::into`, `Clone::clone`, etc.
            require("cmp-rust").deprioritize_common_traits,
            compare.offset,
            compare.exact,
            compare.score,
            compare.recently_used,
            compare.locality,
            compare.sort_text,
            compare.length,
            compare.order,
          },
        },
      })

      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      -- require('lspconfig')['rust_analyzer'].setup({
        -- capabilities = capabilities
      -- })

      local function organize_imports()
        local params = {
          command = "_typescript.organizeImports",
          arguments = {vim.api.nvim_buf_get_name(0)},
          title = ""
        }
        vim.lsp.buf.execute_command(params)
      end
      
      -- require('lspconfig')['tsserver'].setup({
        -- capabilities = capabilities,
        -- commands = {
          -- OrganizeImports = {
            -- organize_imports,
            -- description = "Organize Imports"
          -- }
        -- }
      -- })
      -- require('lspconfig')['csharp_ls'].setup({
      --   capabilities = capabilities
      -- })
    
      require("roslyn").setup({
        -- dotnet_cmd = "dotnet", -- this is the default
        -- roslyn_version = "4.8.0-3.23475.7", -- this is the default
        on_attach = function () end,
        capabilities = capabilities
      })  

      require('lspconfig')['lua_ls'].setup({
        capabilities = capabilities
      })

      require('lspconfig')['eslint'].setup({
        capabilities = capabilities,
        on_attach = function(client, bufnr)
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            command = "EslintFixAll",
          })
        end,
        -- cmd = { "npx", "eslint", "--stdio" },
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
          codeActionOnSave = {
            enable = true,
            mode = "all"
          },
          -- experimental = {
            -- useFlatConfig = true
          -- },
          format = true,
          -- nodePath = "",
          -- onIgnoredFiles = "off",
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
          -- useESLintClass = false,
          validate = "on",
          workingDirectory = {
            mode = "location"
          },
          -- root_dir = lspconfig.util.find_git_ancestor,
        }
      })
    end

  }}
