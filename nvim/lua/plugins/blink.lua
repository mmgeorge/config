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
 
    init = function ()
      vim.cmd([[highlight BlinkCmpMenu           guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpLabel          guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpKind           guibg=#242529 guifg=White]])
      vim.cmd([[highlight BlinkCmpMenuSelection	 guibg=White   guifg=Black]])
      vim.cmd([[highlight BlinkCmpScrollBarThumb guibg=White   guifg=Black]])
      vim.cmd([[highlight BlinkCmpMenuSelection	 guibg=White   guifg=Black]])
    end,

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
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
              { "kind_icon", "label", "label_description", gap = 1 },
              {  "kind" },
            }
          }
        }
      },

      signature = {
        enabled = true
      },

      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { 'lsp', 'path', 'custom_snippets', 'spell' },
        -- min_keyword_length = 3, 
        providers = {
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

      -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
      -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
      -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
      --
      -- See the fuzzy documentation for more information
      fuzzy = { implementation = "prefer_rust_with_warning" }
    },
    opts_extend = { "sources.default" }
  }
}
