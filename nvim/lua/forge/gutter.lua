local M = {}

function M.bounds(session, row)
  local node = session.sequence:locate(row - 1)
  if not node then return nil end
  local _, start = session.sequence:position(node.id)
  local gutter = node.entry.gutter_row and node.entry.gutter_row[row - 1 - start]
  if not gutter then return nil end
  local column, width = nil, 0
  for _, entry in ipairs(gutter) do
    if column == nil or entry.position.column < column then column, width = entry.position.column, 0 end
    if entry.position.column == column then
      for _, chunk in ipairs(entry.chunk) do width = width + vim.fn.strdisplaywidth(chunk.text) end
    end
  end
  if not column then return nil end
  return { column = column, width = width, gutter = gutter }
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
  local bounds = M.bounds(session, row + 1)
  if not bounds then return end
  local origin = vim.api.nvim_win_get_position(window)[2] + 1
  for _, entry in ipairs(bounds.gutter) do
    local chunks = {}
    for _, chunk in ipairs(entry.chunk) do
      chunks[#chunks + 1] = { chunk.text, "Visual" }
    end
    local screen = vim.fn.screenpos(window, row + 1, entry.position.column + 1)
    local column = screen.col - origin
    if screen.row > 0 and column >= 0 then
      vim.api.nvim_buf_set_extmark(session.buffer, namespace, row, entry.position.column, {
        virt_text = chunks, virt_text_win_col = column, priority = 65535, hl_mode = "replace", ephemeral = true,
      })
    end
  end
end

return M
