local no_preview = {
  layout = {
    box = "vertical",
    backdrop = false,
    row = -1,
    width = 0,
    height = 0.4,
    border = "top",
    title = " {title} {live} {flags}",
    title_pos = "left",
    { win = "input", height = 1, border = "bottom" },
    {
      box = "horizontal",
      { win = "list", border = "none" },
    },
  },
}

-- k.keymap('n', '.', '<CMD>Glance definitions<CR>')
-- k.keymap('n', 'or', '<CMD>Glance references<CR>')
-- k.keymap('n', 'ot', '<CMD>Glance type_definitions<CR>')

return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    init = function ()
      vim.api.nvim_create_autocmd("BufEnter", {
        callback = function()
          local root = Snacks.git.get_root()
          if root then
            vim.fn.chdir(root)
          end
        end,
      })

      vim.api.nvim_create_user_command('Notifications', function(_opts)
        Snacks.notifier.show_history()
      end, { desc = 'Get notification history (on)', nargs = '*' })

      -- LSP notifier:
      ---@type table<number, {token:lsp.ProgressToken, msg:string, done:boolean}[]>
      local progress = vim.defaulttable()
      vim.api.nvim_create_autocmd("LspProgress", {
        ---@param ev {data: {client_id: integer, params: lsp.ProgressParams}}
        callback = function(ev)
          local client = vim.lsp.get_client_by_id(ev.data.client_id)
          local value = ev.data.params.value --[[@as {percentage?: number, title?: string, message?: string, kind: "begin" | "report" | "end"}]]
          if not client or type(value) ~= "table" then
            return
          end
          local p = progress[client.id]

          for i = 1, #p + 1 do
            if i == #p + 1 or p[i].token == ev.data.params.token then
              p[i] = {
                token = ev.data.params.token,
                msg = ("[%3d%%] %s%s"):format(
                  value.kind == "end" and 100 or value.percentage or 100,
                  value.title or "",
                  value.message and (" **%s**"):format(value.message) or ""
                ),
                done = value.kind == "end",
              }
              break
            end
          end

          local msg = {} ---@type string[]
          progress[client.id] = vim.tbl_filter(function(v)
            return table.insert(msg, v.msg) or not v.done
          end, p)

          local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
          vim.notify(table.concat(msg, "\n"), "info", {
            id = "lsp_progress",
            title = client.name,
            opts = function(notif)
              notif.icon = #progress[client.id] == 0 and " "
              or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
            end,
          })
        end,
      })

    end,
    keys = {
      -- Top Pickers & Explorer
      -- { "<leader><space>", function() Snacks.picker.smart() end, desc = "Smart Find Files" },
      -- {
      --   '.',
      --   function()
      --     Snacks.picker.lsp_definitions({
      --       focus = "list",
      --       formatters = {
      --         file = {
      --           -- filename_only = true,
      --           truncate = 40
      --         }
      --       }
      --     })
      --   end,
      --   desc = "Lsp definitions"
      -- },
     {
        "on",
        function()
          Snacks.notifier.show_history()
        end,
        desc = "Notifications"
      } ,

      {
        'of',
        function()
          Snacks.picker.lsp_references({
            on_show = function()
              vim.api.nvim_command('stopinsert')
            end,
            -- focus = "list",
            formatters = {
              file = {
                -- filename_only = true,
                truncate = 40
              }
            },
            transform = function(item, ctx)
              -- Filter out definintions
              if item.line and item.line:find("import") then
                -- if item.line and item.line:find("import") or item.line:find("class") or item.line:find("fn") or item.line:find("function") then
                return false
              end

              return true
            end
          })
        end,
        desc = "Lsp references"
      },
      {
        "oh",
        function()
          Snacks.picker.files({
            finder = "files",
            format = "file",
            show_empty = true,
            hidden = false,
            ignored = false,
            follow = false,
            supports_live = true,
            exclude = {
              -- For the JS SDK
              "smartMapping",
              "webscene",
              -- "3d",
              -- "3D",
              "tests/data",
              "dojo",
              "pkg",
              "mjs",
              "widgets"
            }
          })
        end,
        desc = "Files"
      },
      {
        "ot",
        function()
          -- Can use -- at then end to pass flags, eg:
          -- [token] -- -g=*.el    only el files
          -- [token] -- -ig=*.el   ignore case, only el
          -- [token] -- -ig=!*.el  ignore case, not el files
          -- -F search exact
          Snacks.picker.grep({
            finder = "grep",
            format = "file",
            -- search = function(picker)
            --   return picker:word()
            -- end,
            live = true,
            supports_live = true,
            -- layout = no_preview,
            exclude = { "refs" },
          })
        end,
        desc = "Grep"
      },
      {
        "og",
        function()
          Snacks.picker.git_diff()
        end,
        desc = "Git diff"
      },
      {
        "Sr",
        function()
          Snacks.picker.spelling({
            focus = "list",
            layout = no_preview,
          })
        end,
        desc = "Spelling"
      },
      -- Currently I prefer treesitter over this as we don't need to wait for the lsp
      -- to startup
      -- {
      --   "oo",
      --   function ()
      --     Snacks.picker.lsp_symbols({
      --       tree = true,
      --       sort = { fields = { "idx" } },
      --       filter = {
      --         default = {
      --           "Class",
      --           "Constructor",
      --           "Enum",
      --           -- "Field",
      --           "Function",
      --           "Interface",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           "Package",
      --           -- "Property",
      --           "Struct",
      --           "Trait",
      --           "Variable",
      --           -- "Type",
      --           -- "Object",
      --         },
      --         typescriptreact = {
      --           "Class",
      --           "Constructor",
      --           "Enum",
      --           -- "Field",
      --           "Function",
      --           "Interface",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           "Package",
      --           -- "Property",
      --           "Struct",
      --           "Trait",
      --           "Variable",
      --           -- "Type",
      --           -- "Object",
      --         },
      --         rust = {
      --           "Class",
      --           "Constructor",
      --           "Enum",
      --           -- "Field",
      --           "Function",
      --           "Interface",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           "Package",
      --           -- "Property",
      --           "Struct",
      --           "Trait",
      --           "Object",
      --         },
      --         typescript = {
      --           "Class",
      --           "Constructor",
      --           "Enum",
      --           "Field",
      --           "Function",
      --           "Interface",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           "Package",
      --           "Property",
      --           "Struct",
      --           "Trait",
      --         },
      --         lua = {
      --           "Class",
      --           "Constructor",
      --           "Enum",
      --           "Field",
      --           "Function",
      --           "Interface",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           -- "Package", -- remove package since luals uses it for control flow structures
      --           "Property",
      --           "Struct",
      --           "Trait",
      --         },
      --       }
      --     })
      --   end,
      --   desc = "Symbols"
      -- },
      -- {
      --   "oo",
      --   function ()
      --     Snacks.picker.treesitter({
      --       finder = "treesitter_symbols",
      --       format = "lsp_symbol",
      --       tree = true,
      --       filter = {
      --         default = {
      --           "Class",
      --           "Enum",
      --           "Field",
      --           "Function",
      --           "Method",
      --           "Module",
      --           "Namespace",
      --           "Struct",
      --           "Trait",
      --           "Constant",
      --           "Variable"
      --         },
      --         -- set to `true` to include all symbols
      --         markdown = true,
      --         help = true,
      --       },
      --     })
      --   end,
      --   desc = "Symbols"
      -- },
      {
        "<leader>h",
        function()
          Snacks.picker.explorer({
            finder = "explorer",
            sort = { fields = { "sort" } },
            supports_live = true,
            tree = true,
            watch = true,
            diagnostics = true,
            diagnostics_open = false,
            git_status = true,
            git_status_open = false,
            git_untracked = true,
            follow_file = true,
            -- focus = "list",
            focus = "input",
            auto_close = true,
            jump = { close = true },
            -- layout = { preset = "sidebar", preview = false },
            -- to show the explorer to the right, add the below to
            -- your config under `opts.picker.sources.explorer`
            -- layout = { layout = { position = "right" } },
            formatters = {
              file = { filename_only = true },
              severity = { pos = "right" },
            },
            matcher = { sort_empty = false, fuzzy = false },
            config = function(opts)
              return require("snacks.picker.source.explorer").setup(opts)
            end,
            win = {
              list = {
                keys = {
                  ["<BS>"] = "explorer_up",
                  ["l"] = "confirm",
                  ["h"] = "explorer_close", -- close directory
                  ["a"] = "explorer_add",
                  ["d"] = "explorer_del",
                  ["r"] = "explorer_rename",
                  ["c"] = "explorer_copy",
                  ["m"] = "explorer_move",
                  ["o"] = "explorer_open", -- open with system application
                  ["P"] = "toggle_preview",
                  ["y"] = { "explorer_yank", mode = { "n", "x" } },
                  ["p"] = "explorer_paste",
                  ["u"] = "explorer_update",
                  ["<c-c>"] = "tcd",
                  ["<leader>/"] = "picker_grep",
                  ["<c-t>"] = "terminal",
                  ["."] = "explorer_focus",
                  ["I"] = "toggle_ignored",
                  ["H"] = "toggle_hidden",
                  ["Z"] = "explorer_close_all",
                  ["]g"] = "explorer_git_next",
                  ["[g"] = "explorer_git_prev",
                  ["]d"] = "explorer_diagnostic_next",
                  ["[d"] = "explorer_diagnostic_prev",
                  ["]w"] = "explorer_warn_next",
                  ["[w"] = "explorer_warn_prev",
                  ["]e"] = "explorer_error_next",
                  ["[e"] = "explorer_error_prev",
                },
              },
            },
          })
        end,
        desc = "Explorer"
      },
      {
        "os",
        function()
          Snacks.picker.buffers({
            finder = "buffers",
            format = "buffer",
            hidden = false,
            show_empty = true,
            unloaded = true,
            current = false,
            sort_lastused = true,
            layout = no_preview,
            win = {
              input = {
                keys = {
                  ["<c-j>"] = { "bufdelete", mode = { "n", "i" } },
                },
              },
              list = {
                keys = {
                  ["jj"] = "bufdelete",
                },
              },
            },
          })
        end,
        desc = "Buffers"
      },
      {
        "<C-h>",
        function()
          Snacks.picker.lines({
            finder = "lines",
            format = "lines",
            show_empty = true,
            -- layout = no_preview,
            jump = { match = true },
            -- allow any window to be used as the main window
            main = { current = true },
            ---@param picker snacks.Picker
            on_show = function(picker)
              local cursor = vim.api.nvim_win_get_cursor(picker.main)
              local info = vim.api.nvim_win_call(picker.main, vim.fn.winsaveview)
              picker.list:view(cursor[1], info.topline)
              picker:show_preview()
            end,
            -- sort = { fields = { "score:desc", "idx" } },
            sort = { fields = { "idx" } },
            matcher = {
              fuzzy = false
            }
          })
        end,
        desc = "Search lines"
      }
    },
    ---@type snacks.Config
    opts = {
      bigfile = { enabled = false },
      dashboard = { enabled = false },
      explorer = { enabled = true },
      image = {},
      indent = {
        enabled = false,
        only_scope = true,
        only_current = true
      },
      gitbrowse = {
        notify = true
      },
      gh = { },
      input = {
        enabled = false
      },
      quickfile = { enabled = false },
      scope = {
        enabled = false,
      },
      rename = {
        enabled = true
      },
      scroll = { enabled = false },
      statuscolumn = { enabled = false },
      words = { enabled = false },
      notifier = {
        enabled = true,
        timeout = 3000,
        style = "minimal",
        top_down = false,
        filter = function(notif)
          return
            not string.find(notif.msg, "warning: multiple different client offset_encodings")
            and not string.find(notif.msg, "position_encoding param is required")
        end
      },
      picker = {
        sources = {
          gh_diff = {
            auto_close = false,
            focus = "list",
            win = {
              preview = {
                keys = {
                  ["go"] = "focus_list",
                }
              },
              input = {
                keys = {
                  ["<CR>"] = {"focus_preview", mode = { "n", "i" }},
                },
              },
              list = {
                keys = {
                  ["<CR>"] = "focus_preview",
                  ["u"] = "focus_input",
                },
              },
            },
          },
          git_diff = {
            auto_close = false,
            focus = "list",
            win = {
              preview = {
                keys = {
                  ["go"] = "focus_list",
                }
              },
              input = {
                keys = {
                  ["<CR>"] = {"focus_preview", mode = { "n", "i" }},
                },
              },
              list = {
                keys = {
                  ["<CR>"] = "focus_preview",
                  ["u"] = "focus_input",
                },
              },
            },
          },
        },
        previewers = {
          diff = {
            native = true,
            -- fancy: Snacks fancy diff (borders, multi-column line numbers, syntax highlighting)
            -- syntax: Neovim's built-in diff syntax highlighting
            -- terminal: external command (git's pager for git commands, `cmd` for other diffs)
            -- style = "fancy", ---@type "fancy"|"syntax"|"terminal"
            style = "terminal", ---@type "fancy"|"syntax"|"terminal"
            cmd = { "delta" },
            wo = {
              breakindent = true,
              wrap = true,
              linebreak = true,
              showbreak = "",
            },
          },
        },
        formatters = {
          file = {
            truncate = 70
          }
        },
        win = {
          -- input window
          input = {
            keys = {
              -- to close the picker on ESC instead of going to normal mode,
              -- add the following keymap to your config
              -- ["<Esc>"] = { "close", mode = { "n", "i" } },
              ["/"] = "toggle_focus",
              ["<C-Down>"] = { "history_forward", mode = { "i", "n" } },
              ["<C-Up>"] = { "history_back", mode = { "i", "n" } },
              ["<C-c>"] = { "cancel", mode = "i" },
              ["<C-w>"] = { "<c-s-w>", mode = { "i" }, expr = true, desc = "delete word" },
              ["<CR>"] = { "confirm", mode = { "n", "i" } },
              ["<Down>"] = { "list_down", mode = { "i", "n" } },
              ["<Esc>"] = "cancel",
              ["<S-CR>"] = { { "pick_win", "jump" }, mode = { "n", "i" } },
              ["<S-Tab>"] = { "select_and_prev", mode = { "i", "n" } },
              ["<Tab>"] = { "select_and_next", mode = { "i", "n" } },
              ["<Up>"] = { "list_up", mode = { "i", "n" } },
              ["<a-d>"] = { "inspect", mode = { "n", "i" } },
              ["<a-f>"] = { "toggle_follow", mode = { "i", "n" } },
              ["<a-h>"] = { "toggle_hidden", mode = { "i", "n" } },
              ["<a-i>"] = { "toggle_ignored", mode = { "i", "n" } },
              ["<a-m>"] = { "toggle_maximize", mode = { "i", "n" } },
              ["<a-p>"] = { "toggle_preview", mode = { "i", "n" } },
              ["<a-w>"] = { "cycle_win", mode = { "i", "n" } },
              ["<c-a>"] = { "select_all", mode = { "n", "i" } },
              ["<c-b>"] = { "preview_scroll_up", mode = { "i", "n" } },
              -- ["<c-d>"] = { "list_scroll_down", mode = { "i", "n" } },
              ["<c-f>"] = { "preview_scroll_down", mode = { "i", "n" } },
              ["<c-g>"] = { "toggle_live", mode = { "i", "n" } },
              ["<c-j>"] = { "list_down", mode = { "i", "n" } },
              ["s"] = { "list_up", mode = { "n" } },
              ["t"] = { "list_down", mode = { "n" } },
              ["e"] = { "list_scroll_up", mode = { "n" } },
              ["a"] = { "list_scroll_down", mode = { "n" } },
              ["<c-q>"] = { "qflist", mode = { "i", "n" } },
              ["<c-s>"] = { "edit_split", mode = { "i", "n" } },
              ["<c-t>"] = { "tab", mode = { "n", "i" } },
              ["<c-u>"] = { "list_scroll_up", mode = { "i", "n" } },
              ["<c-v>"] = { "edit_vsplit", mode = { "i", "n" } },
              ["<c-r>#"] = { "insert_alt", mode = "i" },
              ["<c-r>%"] = { "insert_filename", mode = "i" },
              ["<c-r><c-a>"] = { "insert_cWORD", mode = "i" },
              ["<c-r><c-f>"] = { "insert_file", mode = "i" },
              ["<c-r><c-l>"] = { "insert_line", mode = "i" },
              ["<c-r><c-p>"] = { "insert_file_full", mode = "i" },
              ["<c-r><c-w>"] = { "insert_cword", mode = "i" },
              ["<c-w>H"] = "layout_left",
              ["<c-w>J"] = "layout_bottom",
              ["<c-w>K"] = "layout_top",
              ["<c-w>L"] = "layout_right",
              ["?"] = "toggle_help_input",
              ["G"] = "list_bottom",
              ["gg"] = "list_top",
              -- ["j"] = "",
              -- ["k"] = "list_up",
              -- ["q"] = "close",
            },
            b = {
              minipairs_disable = true,
            },
          },
          -- result list window
          list = {
            keys = {
              ["/"] = "toggle_focus",
              ["<2-LeftMouse>"] = "confirm",
              ["<CR>"] = "confirm",
              ["<Down>"] = "list_down",
              ["<Esc>"] = "cancel",
              ["<S-CR>"] = { { "pick_win", "jump" } },
              ["<S-Tab>"] = { "select_and_prev", mode = { "n", "x" } },
              ["<Tab>"] = { "select_and_next", mode = { "n", "x" } },
              ["<Up>"] = "list_up",
              ["<a-d>"] = "inspect",
              ["<a-f>"] = "toggle_follow",
              ["<a-h>"] = "toggle_hidden",
              ["<a-i>"] = "toggle_ignored",
              ["<a-m>"] = "toggle_maximize",
              ["<a-p>"] = "toggle_preview",
              ["<a-w>"] = "cycle_win",
              ["<c-a>"] = "select_all",
              ["<c-b>"] = "preview_scroll_up",
              ["u"] = { "focus_input"},
              ["e"] = { "list_scroll_up", mode = { "n" } },
              ["a"] = { "list_scroll_down", mode = { "n" } },
              ["<c-f>"] = "preview_scroll_down",
              ["<c-j>"] = "list_down",
              ["<c-k>"] = "list_up",
              ["<c-n>"] = "list_down",
              ["<c-p>"] = "list_up",
              ["<c-q>"] = "qflist",
              ["<c-s>"] = "edit_split",
              ["<c-t>"] = "tab",
              ["<c-v>"] = "edit_vsplit",
              ["<c-w>H"] = "layout_left",
              ["<c-w>J"] = "layout_bottom",
              ["<c-w>K"] = "layout_top",
              ["<c-w>L"] = "layout_right",
              ["?"] = "toggle_help_list",
              ["G"] = "list_bottom",
              ["gg"] = "list_top",
              -- ["i"] = "focus_input",
              -- ["j"] = "list_down",
              -- ["k"] = "list_up",
              -- ["q"] = "close",
              -- ["zb"] = "list_scroll_bottom",
              -- ["zt"] = "list_scroll_top",
              -- ["zz"] = "list_scroll_center",
            },
            wo = {
              conceallevel = 2,
              concealcursor = "nvc",
            },
          },
          -- preview window
          preview = {
            keys = {
              ["<Esc>"] = "cancel",
              ["q"] = "close",
              ["i"] = "focus_input",
              ["<a-w>"] = "cycle_win",
            },
          },
        },
        -- https://github.com/folke/snacks.nvim/blob/main/docs/picker.md
        enabled = true,
        -- layout = {
        --   layout = {
        --     box = "vertical",
        --     backdrop = false,
        --     row = -1,
        --     width = 0,
        --     height = 0.4,
        --     border = "top",
        --     title = " {title} {live} {flags}",
        --     title_pos = "left",
        --     { win = "input", height = 1, border = "bottom" },
        --     {
        --       box = "horizontal",
        --       { win = "list", border = "none" },
        --       { win = "preview", title = "{preview}", width = 0.5, border = "left" },
        --     },
        --   },
        -- },
        layout = {
          -- Ivy split
          preview = "main",
          layout = {
            box = "vertical",
            backdrop = false,
            width = 0,
            height = 0.4,
            position = "bottom",
            border = "top",
            title = " {title} {live} {flags}",
            title_pos = "left",
            { win = "input", height = 1, border = "bottom" },
            {
              box = "horizontal",
              { win = "list",    border = "none" },
              { win = "preview", title = "{preview}", width = 0.6, border = "left" },
            },
          },
        },
      },
      styles = {
        -- notification = {
        --     border = false,
        --     zindex = 100,
        --     ft = "markdown",
        --     wo = {
        --       winblend = 5,
        --       wrap = false,
        --       conceallevel = 2,
        --       colorcolumn = "",
        --     },
        --     bo = { filetype = "snacks_notif" },
        --   }
        -- wo = { wrap = true } -- Wrap notifications
        notification_history = {
          -- zindex = 100,
          width = 0.8,
          -- height = 0.8,
          -- minimal = false,
        }
      }
    },
  }
}
