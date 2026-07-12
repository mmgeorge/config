local M = {}

---@param row0 integer
---@param spans table
---@param ephemeral? boolean
---@return table[]
function M.extmark_list(row0, spans, ephemeral)
  local extmark_list = {}
  if spans.bg then
    extmark_list[#extmark_list + 1] = {
      row = row0,
      col = 0,
      options = {
        end_row = row0 + 1,
        end_col = 0,
        hl_group = spans.bg.hl_group,
        priority = spans.bg.priority or 60,
        hl_eol = true,
        ephemeral = ephemeral or nil,
      },
    }
  end
  for _, highlight in ipairs(spans.highlights or {}) do
    extmark_list[#extmark_list + 1] = {
      row = row0,
      col = highlight.start_col,
      options = {
        end_col = highlight.end_col,
        hl_group = highlight.hl_group,
        priority = highlight.priority or 90,
        ephemeral = ephemeral or nil,
      },
    }
  end
  return extmark_list
end

---@param buf integer
---@param namespace integer
---@param row0 integer
---@param spans table
---@param ephemeral? boolean
function M.emit(buf, namespace, row0, spans, ephemeral)
  for _, extmark in ipairs(M.extmark_list(row0, spans, ephemeral)) do
    pcall(vim.api.nvim_buf_set_extmark, buf, namespace, extmark.row, extmark.col, extmark.options)
  end
end

return M
