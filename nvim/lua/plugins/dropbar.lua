local custom_path = {
  get_symbols = function(buff, win, cursor)
    local symbols = require("dropbar.sources").path.get_symbols(buff, win, cursor)

    for _, symbol in ipairs(symbols) do
      symbol.name_hl = "DropBarPath"
    end

    symbols[#symbols].name_hl = "DropBarFileName"
    if vim.bo[buff].modified then
      symbols[#symbols].name = symbols[#symbols].name .. "*"
      -- symbols[#symbols].name_hl = "DiffAdded"
    end
    return symbols
  end,
}

return {
  {
    'Bekaboo/dropbar.nvim',
    opts = {
      sources = {
        lsp = {
          valid_symbols = {
            'File',
            'Module',
            'Namespace',
            'Package',
            'Class',
            -- 'Method',
            -- 'Property',
            -- 'Field',
            'Constructor',
            'Enum',
            'Interface',
            -- 'Function',
            -- 'Variable',
            -- 'Constant',
            -- 'String',
            -- 'Number',
            -- 'Boolean',
            -- 'Array',
            'Object',
            -- 'Keyword',
            -- 'Null',
            -- 'EnumMember',
            'Struct',
            -- 'Event',
            -- 'Operator',
            -- 'TypeParameter',
          }
        }
      },
      bar = {
        sources = function(buf, _)
          local sources = require "dropbar.sources"
          local utils = require "dropbar.utils"
          if vim.bo[buf].ft == "markdown" then
            return {
              custom_path,
              sources.markdown,
            }
          end
          if vim.bo[buf].buftype == "terminal" then
            return {
              sources.terminal,
            }
          end
          return {
            custom_path,
            utils.source.fallback {
              -- sources.lsp,
              -- sources.treesitter,
            },
          }
        end,
        enable = function(buf, win, _)
          -- Seems like with snacks this somehow gets added twice?
          if vim.b[buf].snacks_previewed then
            vim.wo[win].winbar = ''
          end

          if
              not vim.api.nvim_buf_is_valid(buf)
              or not vim.api.nvim_win_is_valid(win)
              or vim.fn.win_gettype(win) ~= ''
              or vim.wo[win].winbar ~= ''
              or vim.bo[buf].ft == 'help'
              -- or vim.bo[buf].ft == 'snacks_layout_box'
              -- or vim.bo[buf].ft == 'snacks_picker_list'
              -- or vim.bo[buf].ft == 'snacks_picker_input'
              or vim.bo[buf].ft:find('snacks')
              or vim.bo[buf].ft == 'NeogitStatus'
          -- or vim.b[buf].snacks_previewed
          then
            return false
          end

          local stat = vim.uv.fs_stat(vim.api.nvim_buf_get_name(buf))
          if stat and stat.size > 1024 * 1024 then
            return false
          end

          return vim.bo[buf].ft == 'markdown'
              or pcall(vim.treesitter.get_parser, buf)
              or not vim.tbl_isempty(vim.lsp.get_clients({
                bufnr = buf,
                method = 'textDocument/documentSymbol',
              }))
        end
      }
    }
  }
}
