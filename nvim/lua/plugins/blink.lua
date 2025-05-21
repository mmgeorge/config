local priorities = {
  snippets = 4,
}

local completionItemPriorities = {
  Variable = 1,

  -- Text = 1,
  -- Method = 2,
  -- Function = 3,
  -- Constructor = 4,
  -- Field = 5,
  -- Variable = 6,
  -- Class = 7,
  -- Interface = 8,
  -- Module = 9,
  -- Property = 10,
  -- Unit = 11,
  -- Value = 12,
  -- Enum = 13,
  -- Keyword = 14,
  -- Snippet = 15,
  -- Color = 16,
  -- File = 17,
  -- Reference = 18,
  -- Folder = 19,
  -- EnumMember = 20,
  -- Constant = 21,
  -- Struct = 22,
  -- Event = 23,
  -- Operator = 24,
  -- TypeParameter = 25,
}

local function deprioritize_common_rust_traits(a, b)
  local function is_common_trait(entry)
    local labelDetails = entry.labelDetails
    if not labelDetails then
      return
    end

    local label = labelDetails.detail
    if label == nil then
      return nil
    end

    -- find `(as Trait)` in the label
    local trait = label:match "%(as ([^)]+)%)"
    if trait == nil then
      return nil
    end
    return vim
        .iter({
          "Clone",
          "Copy",
          "Deref",
          "DerefMut",
          "Borrow",
          "BorrowMut",
          "Drop",
          "ToString",
          "ToOwned",
          "PartialEq",
          "PartialOrd",
          "AsRef",
          "AsMut",
          "From",
          "Into",
          "TryFrom",
          "TryInto",
          "Default",
        })
        :find(function(x)
          return x == trait
        end)
  end

  local is_common_1 = is_common_trait(a) or false
  local is_common_2 = is_common_trait(b) or false
  if is_common_1 ~= is_common_2 then
    return not is_common_1
  end
end

return {
  {
    'saghen/blink.cmp',
    -- optional: provides snippets for the snippet source
    dependencies = {
      -- 'rafamadriz/friendly-snippets',
      'ribru17/blink-cmp-spell'
    },

    -- use a release tag to download pre-built binaries
    version = '1.*',

    init = function()
      vim.cmd([[highlight BlinkCmpMenu             guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpLabel            guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpLabelDescription guibg=#242529 guifg=#3ec5ff]])
      vim.cmd([[highlight BlinkCmpKind             guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpMenuSelection	   guibg=White   guifg=Black]])
      vim.cmd([[highlight BlinkCmpScrollBarThumb   guibg=White   guifg=Black]])
      vim.cmd([[highlight BlinkCmpScrollBarGutter  guifg=White              ]])
      vim.cmd([[highlight BlinkCmpMenuSelection	   guibg=White   guifg=Black]])
    end,

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      fuzzy = {
        use_frecency = true,
        use_proximity = true,
        max_typos = function(str)
          if #str < 4 then
            return 0
          end
          return 2
        end,
        implementation = "prefer_rust_with_warning",
        sorts = {
          -- function(a, b)
          --   local a_priority = priorities[a.source_id] or 0
          --   local b_priority = priorities[b.source_id] or 0
          --   if a_priority ~= b_priority then return a_priority > b_priority end
          -- end,
          'exact',
          -- function(a, b)
          --   local a_priority = completionItemPriorities[a.kind] or 100
          --   local b_priority = completionItemPriorities[b.kind] or 100
          --
          --   if a_priority ~= b_priority then return a_priority > b_priority end
          -- end,
          deprioritize_common_rust_traits,
          'score',
          'sort_text',
          function(a, b)
            return #a.label < #b.label
          end,
          -- 'label'
        }
      },
      keymap = {
        ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
        ['<C-e>'] = { 'hide', 'fallback' },
        ['<CR>'] = { 'accept', 'fallback' },

        ['<Tab>'] = { 'snippet_forward', 'fallback' },
        ['<S-Tab>'] = { 'snippet_backward', 'fallback' },

        ['<Up>'] = { 'select_prev', 'fallback' },
        ['<Down>'] = { 'select_next', 'fallback' },
        ['<C-p>'] = { 'select_prev', 'fallback_to_mappings' },
        ['<C-n>'] = { 'select_next', 'fallback_to_mappings' },

        ['<C-b>'] = { 'scroll_documentation_up', 'fallback' },
        ['<C-f>'] = { 'scroll_documentation_down', 'fallback' },

        ['<C-k>'] = { 'show_signature', 'hide_signature', 'fallback' },
      },

      appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono'
      },

      cmdline = {
        enabled = true,
        keymap = { preset = 'enter' },
        completion = {
          trigger = {
            show_on_blocked_trigger_characters = {},
            show_on_x_blocked_trigger_characters = {},
          },
          list = {
            selection = {
              preselect = false,
              auto_insert = true,
            },
          },
          menu = {
            auto_show = true,
          },
          ghost_text = {
            enabled = true,
          },
        },
      },

      -- (Default) Only show the documentation popup when manually triggered
      completion = {
        accept = {
          auto_brackets = {
            enabled = false
          }
        },
        documentation = {
          auto_show = false,
        },
        ghost_text = {
          enabled = true,
          show_with_menu = true
        },
        menu = {
          draw = {
            columns = {
              { "kind_icon" }, { "label", "label_description", gap = 1 },
              -- { "kind_icon", "label", "label_description", gap = 1 },
              -- {  "kind" },
            }
          }
        }
      },

      signature = {
        enabled = false
      },

      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { 'lsp', 'path', 'custom_snippets', 'spell' },
        -- default = { 'snippets', 'lsp', 'path', 'custom_snippets', 'spell' },

        -- min_keyword_length = 3,
        providers = {
          snippets = {
            score_offset = 20
          },
          lsp = {
            transform_items = function(ctx, items)
              local function should_filter(label)
                -- if ctx.bounds.length < 4 and #label >= 8 then
                --   return true
                -- end

                if ctx.bounds.length < 5 and #label >= 18 then
                  return true
                end

                return string.sub(label, 1, 2) == "__"
              end

              local out = {}
              for _, item in ipairs(items) do
                if not should_filter(item.sortText) then
                  table.insert(out, item)
                end
              end

              -- print(vim.inspect(ctx))
              -- local out = {}
              -- local ftype = vim.bo.filetype
              -- if ftype == "typescriptreact" or ftype =="typescript" then
              --   for _, item in ipairs(items) do
              --    if item.label ~= "class" then
              --      table.insert(out, item)
              --    end
              --   end
              -- else
              --   out = items
              -- end

              return out
            end
          },
          custom_snippets = {
            name = "custom_snippets",
            module = "snippet_source",
          },
          spell = {
            name = 'Spell',
            min_keyword_length = 5,
            module = 'blink-cmp-spell',
            opts = {
              use_cmp_spell_storting = true,
              -- EXAMPLE: Only enable source in `@spell` captures, and disable it
              -- in `@nospell` captures.
              enable_in_context = function()
                local curpos = vim.api.nvim_win_get_cursor(0)
                local captures = vim.treesitter.get_captures_at_pos(
                  0,
                  curpos[1] - 1,
                  curpos[2] - 1
                )
                local in_spell_capture = false
                for _, cap in ipairs(captures) do
                  if cap.capture == 'spell' then
                    in_spell_capture = true
                  elseif cap.capture == 'nospell' then
                    return false
                  end
                end
                return in_spell_capture
              end,
            },
          },
        }
      },

    },
    opts_extend = { "sources.default" }
  }
}
