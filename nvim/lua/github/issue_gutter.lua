local M = {}
local namespace = vim.api.nvim_create_namespace("ForgeIssueContinuationGutter")
local hidden_group = "ForgeIssueContinuationNumber"

local function configure_highlight()
  local line_number = vim.api.nvim_get_hl(0, { name = "LineNr", link = false })
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  vim.api.nvim_set_hl(0, hidden_group, { fg = line_number.bg or normal.bg or 0 })
end

function M.refresh(replica)
  if not replica or not vim.api.nvim_buf_is_valid(replica.buffer) then return end
  configure_highlight()
  vim.api.nvim_buf_clear_namespace(replica.buffer, namespace, 0, -1)
  for id, block in pairs(replica.block) do
    if id:match("^issue:comment:%d+:label$") and block.row_count > 1 then
      local _, start_row = replica.sequence:position(id)
      for offset = 1, block.row_count - 1 do
        vim.api.nvim_buf_set_extmark(replica.buffer, namespace, start_row + offset, 0, {
          number_hl_group = hidden_group,
        })
      end
    end
  end
end

function M.clear(replica)
  if replica and vim.api.nvim_buf_is_valid(replica.buffer) then
    vim.api.nvim_buf_clear_namespace(replica.buffer, namespace, 0, -1)
  end
end

return M
