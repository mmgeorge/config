return {
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    -- branch = "dev",
    keys = {
      {
        "ge",
        '<CMD>Trouble diagnostics toggle filter.severity=vim.diagnostic.severity.ERROR focus=true <CR>',
        mode = { 'n' }
      },
      {
        "gd",
        -- '<CMD>Trouble diagnostics toggle focus=true filter.buf=0 <CR>',
        '<CMD>Trouble diagnostics toggle focus=true<CR>',
        mode = { 'n' }
      },
      {
        "gq",
        '<CMD>Trouble quickfix toggle focus=true <CR>',
        mode = { 'n' }
      }
    },
    -- config = function ()
    --   -- Open Trouble qflist when qf list opens
    --   vim.api.nvim_create_autocmd("BufRead", {
    --     callback = function(ev)
    --       if vim.bo[ev.buf].buftype == "quickfix" then
    --         vim.schedule(function()
    --           vim.cmd([[cclose]])
    --           vim.cmd([[Trouble qflist open]])
    --         end)
    --       end
    --     end,
    --   })
    -- end,
    opts = {
      -- warn_no_results = false,
      -- open_no_results = true,
      auto_close = false,   -- auto close when there are no items
      -- auto_open = true, -- auto open when there are items
      auto_preview = true,  -- automatically open preview when on an item
      auto_refresh = true,  -- auto refresh when open
      focus = true,         -- Focus the window when opened
      restore = true,       -- restores the last location in the list when opening
      follow = true,        -- Follow the current item
      indent_guides = true, -- show indent guides
      max_items = 200,      -- limit number of items that can be displayed per section
      multiline = false,    -- render multi-line messages
      pinned = false,       -- When pinned, the opened trouble window will be bound to the current buffer
      ---@type trouble.Window.opts
      win = {},             -- window options for the results window. Can be a split or a floating window.
      warn_no_results = false,
      open_no_results = false,
      -- Window options for the preview window. Can be a split, floating window,
      -- or `main` to show the preview in the main editor window.
      ---@type trouble.Window.opts
      preview = { type = "main" },
      -- Throttle/Debounce settings. Should usually not be changed.
      ---@type table<string, number|{ms:number, debounce?:boolean}>
      throttle = {
        refresh = 20,                            -- fetches new data when needed
        update = 10,                             -- updates the window
        render = 10,                             -- renders the window
        follow = 10,                             -- follows the current item
        preview = { ms = 100, debounce = true }, -- shows the preview for the current item
      },
      -- Key mappings can be set to the name of a builtin action,
      -- or you can define your own custom action.
      ---@type table<string, string|trouble.Action>
      keys = {
        ["?"] = "help",
        r = "refresh",
        R = "toggle_refresh",
        -- q = "close",
        o = "jump_close",
        -- ["<esc>"] = "cancel",
        ["<esc>"] = "close",
        ["<cr>"] = "jump",
        ["<2-leftmouse>"] = "jump",
        -- ["<c-s>"] = "jump_split",
        -- ["<c-v>"] = "jump_vsplit",
        -- go down to next item (accepts count)
        -- j = "next",
        -- ["]]"] = "next",
        -- go up to prev item (accepts count)
        -- k = "prev",
        ["s"] = "prev",
        ["t"] = "next",
        -- ["[["] = "prev",
        -- i = "inspect",
        -- p = "preview",
        P = "toggle_preview",
        zo = "fold_open",
        zO = "fold_open_recursive",
        zc = "fold_close",
        zC = "fold_close_recursive",
        za = "fold_toggle",
        zA = "fold_toggle_recursive",
        zm = "fold_more",
        zM = "fold_close_all",
        zr = "fold_reduce",
        zR = "fold_open_all",
        zx = "fold_update",
        zX = "fold_update_all",
        zn = "fold_disable",
        zN = "fold_enable",
        zi = "fold_toggle_enable",
        ["F"] = { -- example of a custom action that toggles the active view filter
          action = function(view)
            view:filter({ buf = 0 }, { toggle = true })
          end,
          desc = "Toggle Current Buffer Filter",
        },
        ["f"] = { -- example of a custom action that toggles the severity
          action = function(view)
            local f = view:get_filter("severity")
            local severity = ((f and f.filter.severity or 0) + 1) % 5
            view:filter({ severity = severity }, {
              id = "severity",
              template = "{hl:Title}Filter:{hl} {severity}",
              del = severity == 0,
            })
          end,
          desc = "Toggle Severity Filter",
        },
      },
      ---@type table<string, trouble.Mode>
      modes = {
        diff_review = {
          desc = "Diff Review",
          source = "diff_review",
          events = { "BufWritePost" },
          groups = {
            { "filename", format = "{item.file_check} {file_icon} {basename} {item.stats}" },
          },
          sort = { "filename", "pos" },
          format = "{item.check} {item.hunk_header} {item.context_text}",
          auto_preview = false,
          preview = { type = "main", scratch = false },
          focus = true,
          keys = {
            -- Override s/t to use raw cursor movement so group headers
            -- (file rows) are reachable, and show diff preview on file rows
            s = {
              action = function(self)
                local cursor = vim.api.nvim_win_get_cursor(self.win.win)
                if cursor[1] > 1 then
                  vim.api.nvim_win_set_cursor(self.win.win, { cursor[1] - 1, cursor[2] })
                end
                require("trouble.sources.diff_review").auto_preview(self)
              end,
              desc = "Move up",
            },
            t = {
              action = function(self)
                local cursor = vim.api.nvim_win_get_cursor(self.win.win)
                local max = vim.api.nvim_buf_line_count(self.win.buf)
                if cursor[1] < max then
                  vim.api.nvim_win_set_cursor(self.win.win, { cursor[1] + 1, cursor[2] })
                end
                require("trouble.sources.diff_review").auto_preview(self)
              end,
              desc = "Move down",
            },
            ["<Tab>"] = "fold_toggle",
            ["<cr>"] = {
              action = function(self, ctx)
                if ctx.item then
                  -- Hunk: jump to real file
                  self:jump(ctx.item)
                elseif ctx.node and ctx.node.item then
                  -- File group header: show diff in main and focus it
                  local dr = require("trouble.sources.diff_review")
                  dr.auto_preview(self)
                  local win = dr.get_main_win(self)
                  if win and vim.api.nvim_win_is_valid(win) then
                    vim.api.nvim_set_current_win(win)
                  end
                end
              end,
              desc = "Jump to file/hunk",
            },
            S = {
              action = function(view, ctx)
                local dr = require("trouble.sources.diff_review")
                local affected_file = ctx.item and ctx.item.filename
                    or (ctx.node and ctx.node.item and ctx.node.item.filename)
                if ctx.item then
                  local it = ctx.item.item
                  if it.staged then return end
                  if it.diff then
                    local result = vim.fn.system({ "git", "apply", "--cached", "--unidiff-zero", "-" }, it.diff .. "\n")
                    if vim.v.shell_error ~= 0 then
                      vim.notify("Stage failed: " .. result, vim.log.levels.ERROR)
                    end
                  end
                elseif ctx.node then
                  local filename = ctx.node.item and ctx.node.item.filename
                  if filename then
                    vim.fn.system({ "git", "add", "--", filename })
                  end
                  view:fold(ctx.node, { action = "close" })
                end
                view:refresh()
                -- Sync open diff buffer
                if affected_file then
                  dr.refresh_open_diff_buffer(affected_file)
                end
              end,
              desc = "Stage hunk/file",
            },
            U = {
              action = function(view, ctx)
                local dr = require("trouble.sources.diff_review")
                local affected_file = ctx.item and ctx.item.filename
                    or (ctx.node and ctx.node.item and ctx.node.item.filename)
                if ctx.item then
                  local it = ctx.item.item
                  if not it.staged then return end
                  if it.diff then
                    local result = vim.fn.system({ "git", "apply", "--cached", "--reverse", "--unidiff-zero", "-" }, it.diff .. "\n")
                    if vim.v.shell_error ~= 0 then
                      vim.notify("Unstage failed: " .. result, vim.log.levels.ERROR)
                    end
                  end
                elseif ctx.node then
                  local filename = ctx.node.item and ctx.node.item.filename
                  if filename then
                    vim.fn.system({ "git", "restore", "--staged", "--", filename })
                  end
                end
                view:refresh()
                if affected_file then
                  dr.refresh_open_diff_buffer(affected_file)
                end
              end,
              desc = "Unstage hunk/file",
            },
          },
        },
        symbols = {
          desc = "document symbols",
          mode = "lsp_document_symbols",
          focus = false,
          win = { position = "right" },
          filter = {
            -- remove Package since luals uses it for control flow structures
            ["not"] = { ft = "lua", kind = "Package" },
            any = {
              -- all symbol kinds for help / markdown files
              ft = { "help", "markdown" },
              -- default set of symbol kinds
              kind = {
                "Class",
                "Constructor",
                "Enum",
                "Field",
                "Function",
                "Interface",
                "Method",
                "Module",
                "Namespace",
                "Package",
                "Property",
                "Struct",
                "Trait",
              },
            },
          },
        },
      },
    }
  }
}
