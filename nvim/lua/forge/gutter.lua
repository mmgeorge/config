local M = {}

---@param session table
---@param row integer One-based buffer row.
---@return ForgeRowLayout?
function M.bounds(session, row)
  local location = session.decoration_location and session.decoration_location(row - 1)
  if session.decoration_location and not location then return nil end
  local node = location and session.sequence.node[location.block] or session.sequence:locate(row - 1)
  if not node then return nil end
  local _, start = session.sequence:position(node.id)
  local relative = location and location.position.row or row - 1 - start
  return node.entry.row_layout and node.entry.row_layout[relative]
end

function M.normalize(session, selection)
  if session.status ~= "Applied" or session.applying or session.editable.suspended
    or vim.api.nvim_get_current_buf() ~= session.buffer then return end
  local mode = vim.api.nvim_get_mode().mode
  if mode ~= "n" and not selection then return end
  local position = vim.fn.getcurpos()
  if selection then
    local anchor = vim.fn.getpos("v")[2]
    session.gutter_selection = { window = vim.api.nvim_get_current_win(),
      first = math.min(anchor, position[2]), last = math.max(anchor, position[2]) }
  end
  local bounds = M.bounds(session, position[2])
  if not bounds then return end
  local text = vim.api.nvim_buf_get_lines(session.buffer, position[2] - 1, position[2], true)[1]
  local column, offset = position[3] - 1, position[4]
  if selection then
    column, offset = bounds.column, 0
  elseif session.materialized_prefix then
    column, offset = math.max(column, bounds.column + bounds.width), 0
  elseif #text <= bounds.column or column <= bounds.column then
    column, offset = bounds.column, bounds.width
  elseif column >= #text then
    column, offset = #text - 1, 0
    while column > 0 and text:byte(column + 1) >= 128 and text:byte(column + 1) < 192 do column = column - 1 end
  else
    offset = 0
  end
  if column ~= position[3] - 1 or offset ~= position[4] then
    vim.fn.setpos(".", { 0, position[2], column + 1, offset })
  end
end

function M.highlight(session, window, row, namespace)
  local selection = session.gutter_selection
  if not selection or selection.window ~= window or row + 1 < selection.first or row + 1 > selection.last then return end
  vim.api.nvim_buf_set_extmark(session.buffer, namespace, row, 0, {
    end_row = row + 1, end_col = 0, hl_group = "Visual", line_hl_group = "Visual",
    hl_eol = true, priority = 65535, ephemeral = true,
  })
  local bounds = M.bounds(session, row + 1)
  local origin = vim.api.nvim_win_get_position(window)[2] + 1
  for _, entry in ipairs(bounds and bounds.insertion or {}) do
    local chunks = {}
    for _, chunk in ipairs(entry.chunk) do
      chunks[#chunks + 1] = { chunk.text, "Visual" }
    end
    local location = session.decoration_location and session.decoration_location(row)
    local prefix = location and location.display_indent or 0
    local physical_column = entry.position.column == 0 and 0 or entry.position.column + prefix
    local screen = vim.fn.screenpos(window, row + 1, physical_column + 1)
    local column = entry.position.column == 0 and 0
      or screen.col - origin - vim.fn.getwininfo(window)[1].textoff
    if screen.row > 0 and column >= 0 then
      vim.api.nvim_buf_set_extmark(session.buffer, namespace, row, physical_column, {
        virt_text = chunks, virt_text_win_col = column, priority = 65535, hl_mode = "replace", ephemeral = true,
      })
    end
  end
  local location = session.decoration_location and session.decoration_location(row)
    or require("forge.buffer").locate(session, row, 0)
  local node = location and session.sequence.node[location.block]
  if not location or not node then return end
  if location.position.row ~= 0 then return end
  ---@type ForgeContentLayout?
  local layout = node.entry.metadata.layout
  local marker = layout and require("forge.content_layout").marker(node.entry)
  if layout and marker and layout.indent > 2 then
    vim.api.nvim_buf_set_extmark(session.buffer, namespace, row, 0, {
      virt_text = { { marker.text, "Visual" } }, virt_text_win_col = layout.indent - 4,
      priority = 65535, hl_mode = "replace", ephemeral = true,
    })
  end
end

return M
