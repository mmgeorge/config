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
      "onsails/lspkind.nvim", -- pretty formatting
      "jmederosalvarado/roslyn.nvim" , 
      "f3fora/cmp-spell"
    },
    config = function()
      local cmp = require("cmp"); 
      -----------------------------------------------------------------------------
      --- NATIVE AUTOCMDS SNIPPETS -- 
      -----------------------------------------------------------------------------
        -- snippet("la ", text"<- "),
        -- snippet("ra ", text"-> "),
        -- snippet("udef ", text"self.useDefaults?.(arguments)"),
        -- snippet("sf ", {
        --   text({"<script>", 
        --     "  var esriConfig = {",
        --     "    has: {",
        --     "      \"esri-2d-update-debug\": 1,",
        --     "      \"esri-2d-debug\": 1,",
        --     "      \"esri-tiles-debug\": 1,",
        --     "      \"featurelayer-pbf\": 1,",
        --     "    }",
        --     "  }",
        --     "</script>"}), 
        -- }) ,
     
      local autosnippets = {
        {
          trigger = "la ", 
          body = "<- "
        },
        {
          trigger = "todo: ", 
          body = "TODO "
        },
        {
          trigger = "perf: ", 
          body = "PERF: "
        },
        {
          trigger = "bug: ", 
          body = "BUG: "
        },
        {
          trigger = "warn: ", 
          body = "WARN: "
        },
        {
          trigger = "hack: ", 
          body = "HACK: "
        },
        {
          trigger = "note: ", 
          body = "NOTE: "
        },
        {
          trigger = "ra ", 
          body = "-> "
        },
        {
          trigger = "udef ", 
          body = "self.useDefaults?.(arguments)"
        },
        {
          trigger = "sf", 
          body = [[<script> 
var esriConfig = {
  has: {
    "esri-2d-update-debug": 1,
    "esri-2d-debug": 1,
    "esri-tiles-debug": 1,
    "featurelayer-pbf": 1,
  },
},
</script>]]
        },
        {
          trigger = "hh ", 
          body = [[//--------------------------------------------------------------------------" 
//
//  $1 
//
//--------------------------------------------------------------------------"
]]
        }
      }
      
      vim.api.nvim_create_autocmd("TextChangedI", {
        pattern = "*",
        callback = function()
          local line = vim.api.nvim_get_current_line()
          local col = vim.api.nvim_win_get_cursor(0)[2]  -- Current column

          -- Extract the last entered token
          local start_pos, end_pos = line:sub(1, col):find("([^%s]+)%s?$")
    
          if start_pos and end_pos then
            local last_token = line:sub(start_pos, end_pos)
            
            -- OPTIMIZE: Inefficent. Use map as snippets list grows?
            for index, item in ipairs(autosnippets) do 
              if last_token == item.trigger then
                local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
                local row = pos[1] - 1; 
                local buf = vim.api.nvim_get_current_buf() 
                -- Remove match token
                vim.api.nvim_buf_set_text(buf, row, start_pos - 1, row, end_pos, {})
                -- Insert text at cursor
                -- vim.api.nvim_put(vim.split(item.body, "\n"), "", true, true)
                vim.snippet.expand(item.body)
                break
              end
            end
          end
        end,
      })

      -----------------------------------------------------------------------------
      --- !NATIVE AUTOCMDS SNIPPETS -- 
      -----------------------------------------------------------------------------
      
      -----------------------------------------------------------------------------
      --- NATIVE SNIPPETS -- 
      -----------------------------------------------------------------------------

      local function get_position_before_postfix()
        local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
        local row = pos[1] - 1 -- must convert to 0 indexed
        local line = vim.api.nvim_get_current_line()
        local dot_position = line:find("%.") 

        if dot_position then 
          return { row, dot_position - 2 } -- Before dot + 1 based index
        end

        return nil
      end

      local function type_node()
        local node = vim.treesitter.get_node({
          pos = get_position_before_postfix()
        })

        local function is_type(ty)
          if 
            ty == "primitive_type" or 
            ty == "scoped_type_identifier" or 
            ty == "generic_type" or 
            ty == "type_identifier"  then
            return true
          end 
         
          return false
        end

        while node do 
          local ty = node:type()
          
          local parent = node:parent(); 
          local parent_ty = parent and parent:type()

          if is_type(ty) and not is_type(parent_ty) then
            return node
          end
         
          node = parent
        end 

        return nil
      end 

      local function expr_node()
        local node = vim.treesitter.get_node({
          pos = get_position_before_postfix()
        })

        while node do 
          local ty = node:type()
          if 
            ty == "call_expression" or 
            ty == "integer_literal" or 
            ty == "string_literal" or 
            ty == "float_literal" then 
            return node
          end 

          node = node:parent()
        end 

        return nil
      end

      function postfix(options)
        return { 
          trigger = options.trigger, 
          execute = function ()
            local node = options.node()
            if node == nil then
              return nil
            end

            local row, col = node:start()
            local text = vim.treesitter.get_node_text(node, 0):gsub('%.%w*$', '')  -- remove postfix
            local pos = vim.api.nvim_win_get_cursor(0)
            return {
              body = options.body(text), 
              clear_region = {
                from = { row, col },
                to = { pos[1] - 1, pos[2] } 
              }
            } 
          end, 
        }  
      end
      
      local global_snippets = {
        -- { 
        -- trigger = 'fun', body = 'function ${1:name}(${2:args}) $0 end',
        -- }
      }

      local snippets_by_filetype = {
        rust = {
          postfix({ 
            trigger = 'rcl', 
            node = type_node, 
            body = function (text)
              return "Rc<RefCell<" .. text .. ">>" 
            end 
          }),
          postfix({ 
            trigger = 'rcl', 
            node = expr_node, 
            body = function (text)
              return "Rc::new(RefCell::new(" .. text .. "))" 
            end 
          }),
          postfix({ 
            trigger = 'rfl', 
            node = type_node, 
            body = function (text)
              return "RefCell<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'rfl', 
            node = expr_node, 
            body = function (text)
              return "RefCell::new(" .. text .. ")" 
            end 
          }),
          postfix({ 
            trigger = 'arc', 
            node = type_node, 
            body = function (text)
              return "Arc<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'rf', 
            node = type_node, 
            body = function (text)
              return "RefCell<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'rf', 
            node = expr_node, 
            body = function (text)
              return "RefCell::new(" .. text .. ")" 
            end 
          }),
          postfix({ 
            trigger = 'rl', 
            node = type_node, 
            body = function (text)
              return "RwLock<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'rl', 
            node = expr_node, 
            body = function (text)
              return "RwLock::new(" .. text .. ")" 
            end 
          }),
          postfix({ 
            trigger = 'mut', 
            node = type_node, 
            body = function (text)
              return "Mutex<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'mut', 
            node = expr_node, 
            body = function (text)
              return "Mutex::new(" .. text .. ")" 
            end 
          }),
          postfix({ 
            trigger = 'amut', 
            node = type_node, 
            body = function (text)
              return "Arc<Mutex<" .. text .. ">>" 
            end 
          }),
          postfix({ 
            trigger = 'amut', 
            node = expr_node, 
            body = function (text)
              return "Arc::new(Mutex::new(" .. text .. "))" 
            end 
          }),
          postfix({ 
            trigger = 'lboxfut', 
            node = type_node, 
            body = function (text)
              return "LocalBoxFuture<'static, " .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'boxfut', 
            node = type_node, 
            body = function (text)
              return "BoxFuture<'static, " .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'ifut', 
            node = type_node, 
            body = function (text)
              return "impl Future<Output = " .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'opt', 
            node = type_node, 
            body = function (text)
              return "Option<" .. text .. ">" 
            end 
          }),
          postfix({ 
            trigger = 'vec', 
            node = type_node, 
            body = function (text)
              return "Vec<" .. text .. ">" 
            end 
          }),
        }
      }

      local function get_buf_snips()
        local ft = vim.bo.filetype
        local snips = vim.list_slice(global_snippets)

        if ft and snippets_by_filetype[ft] then
          vim.list_extend(snips, snippets_by_filetype[ft])
        end

        return snips
      end

      local source = {}

      ---Return whether this source is available in the current context or not (optional).
      ---@return boolean
      function source:is_available()
        return true
      end

      ---Return the debug name of this source (optional).
      ---@return string
      function source:get_debug_name()
        return 'debug name'
      end

      ---Return LSP's PositionEncodingKind.
      ---@NOTE: If this method is omitted, the default value will be `utf-16`.
      ---@return lsp.PositionEncodingKind
      function source:get_position_encoding_kind()
        return 'utf-16'
      end

      ---Return the keyword pattern for triggering completion (optional).
      ---If this is omitted, nvim-cmp will use a default keyword pattern. See |cmp-config.completion.keyword_pattern|.
      ---@return string
      function source:get_keyword_pattern()
        return [[\k\+]]
      end

      ---Return trigger characters for triggering completion (optional).
      function source:get_trigger_characters()
        return { '.' }
      end

      ---Invoke completion (required).
      ---@param params cmp.SourceCompletionApiParams
      ---@param callback fun(response: lsp.CompletionResponse|nil)
      function source:complete(params, callback)
        local bufnr = vim.api.nvim_get_current_buf()
        local completion_items = {}
        
        vim.tbl_map(function(s)
          local executed = s.execute();
          if executed ~= nil then 
            ---@type lsp.CompletionItem
            local item = {
              word = s.trigger,
              label = s.trigger,
              kind = vim.lsp.protocol.CompletionItemKind.Snippet,
              insertText = executed.body,
              insertTextFormat = vim.lsp.protocol.InsertTextFormat.Snippet,
              clear_region = executed.clear_region
            }
            table.insert(completion_items, item)
          end
        end, get_buf_snips())

        callback(completion_items)
      end

      ---Resolve completion item (optional). This is called right before the completion is about to be displayed.
      ---Useful for setting the text shown in the documentation window (`completion_item.documentation`).
      ---@param completion_item lsp.CompletionItem
      ---@param callback fun(completion_item: lsp.CompletionItem|nil)
      function source:resolve(completion_item, callback)
        callback(completion_item)
      end

      ---Executed after the item was selected.
      ---@param completion_item lsp.CompletionItem
      ---@param callback fun(completion_item: lsp.CompletionItem|nil)
      function source:execute(completion_item, callback)
        local clear = completion_item.clear_region 
        if clear ~= nil then
          local buf = vim.api.nvim_get_current_buf() 
         
          vim.api.nvim_buf_set_text(buf, clear.from[1], clear.from[2], clear.to[1], clear.to[2], {})
        end 
        
        callback(completion_item)
      end

      ---Register your source to nvim-cmp.
      require('cmp').register_source('snp', source)
      -----------------------------------------------------------------------------
      --- !NATIVE SNIPPETS -- 
      -----------------------------------------------------------------------------

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
            vim.snippet.expand(args.body)
          end
        },
        completion = {
          scrollbar = false,
          completeopt = "menu, menuone, preview, noinsert",
          -- completeopt = "menu, menuone, preview",
          -- keyword_length = 2, -- # of characters to trigger auto completion
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
          { name = 'snp' },
          { name = 'nvim_lsp' },
          -- { name = 'luasnip' },
          { 
            name = 'spell', 
            option = {
              keep_all_entries = false,
              enable_in_context = function(params)
                return require('cmp.config.context').in_treesitter_capture('spell')
              end,
              preselect_correct_word = true,
            },
          },
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
      
      require('lspconfig')['tailwindcss'].setup({
        capabilities = capabilities
      })
      
      require('lspconfig')['cssls'].setup({
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
