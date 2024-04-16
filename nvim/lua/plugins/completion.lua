return {
   {
      "hrsh7th/nvim-cmp",
      event = "InsertEnter",
      dependencies = {
         "neovim/nvim-lspconfig",
         "hrsh7th/cmp-buffer", 
         "hrsh7th/cmp-path", 
         "hrsh7th/cmp-cmdline", 
         "hrsh7th/cmp-nvim-lsp", 
         "onsails/lspkind.nvim", -- pretty formatting
      },
      config = function()
         local cmp = require("cmp"); 

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
                     vim.snippet.expand(args.body)
                  end
               },
               completion = {
                  scrollbar = false,
                  -- completeopt = "menu, menuone, preview, noselect",
                  -- keyword_length = 5, -- # of characters to trigger auto completion
                  
               },
               window = {
                  -- completion = cmp.config.window.bordered(),
                  -- documentation = cmp.config.window.bordered(),   
               },
               view = {
                  docs = {
                     auto_open = false,
                  } ,
                  -- entries = {
                  --    follow_cursor = false, 
                  -- }
                  --entries = 'native'
               },
               mapping = cmp.mapping.preset.insert({
                     -- Intellij-like mapping
                     --   If no completion is selected, insert the first one in the list.
                     --   If a completion is selected, insert this one.
                     ["<Tab>"] = cmp.mapping(function(fallback)
                           -- This little snippet will confirm with tab, and if no entry is selected,
                           -- will confirm the first item
                           if cmp.visible() then
                              local entry = cmp.get_selected_entry()
                              if not entry then
                                 cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                              end
                              cmp.confirm()
                           else
                              fallback()
                           end
                     end, {"i","s","c",}),
                     ['<C-b>'] = cmp.mapping.scroll_docs(-4),
                     ['<C-f>'] = cmp.mapping.scroll_docs(4),
                     ['<C-Space>'] = cmp.mapping.complete(),
                     ['<C-e>'] = cmp.mapping.abort(),
                     -- Accept currently selected item. Set `select` to `false`
                     -- to only confirm explicitly selected items.
                     ['<CR>'] = cmp.mapping.confirm({ select = true }),
               }),
               sources = cmp.config.sources({
                     { name = 'nvim_lsp' },
                     { name = 'path' },
                     { name = 'cmdline' },
                     { name = 'buffer' },
               })
         })

         local capabilities = require('cmp_nvim_lsp').default_capabilities()
         require('lspconfig')['rust_analyzer'].setup({
               capabilities = capabilities
                                                    })
      end
      
}}
