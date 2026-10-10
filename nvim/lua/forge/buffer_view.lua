local M = {}

---@class ForgeRetainedViewport
---@field window integer
---@field top? {block: string, position: {row: integer, column: integer}}
---@field view table Native winsaveview state before the projection changes.

---@param session table
---@param window integer
---@param destination {block: string, position: {row: integer, column: integer}}
---@return ForgeRetainedViewport?
function M.capture_viewport(session, window, destination)
  local _, start = session.sequence:position(destination.block)
  if not start then return nil end
  local row = require("forge.buffer").physical_row(session, start + destination.position.row) + 1
  return vim.api.nvim_win_call(window, function()
    if row < vim.fn.line("w0") or row > vim.fn.line("w$") then return nil end
    local view = vim.fn.winsaveview()
    return { window = window, view = view,
      top = require("forge.buffer").locate(session, view.topline - 1, 0) }
  end)
end

---@param session table
---@param retained ForgeRetainedViewport
function M.restore_viewport(session, retained)
  if not vim.api.nvim_win_is_valid(retained.window)
    or vim.api.nvim_win_get_buf(retained.window) ~= session.buffer then return end
  vim.api.nvim_win_call(retained.window, function()
    local view = vim.fn.winsaveview()
    local top = retained.top
    -- Resolve the source identity after publication instead of retaining a shifted physical row.
    if top and session.sequence.node[top.block] then
      local _, start = session.sequence:position(top.block)
      view.topline = require("forge.buffer").physical_row(session, start + top.position.row) + 1
    else
      view.topline = retained.view.topline
    end
    view.topfill, view.leftcol, view.skipcol = retained.view.topfill, retained.view.leftcol, retained.view.skipcol
    vim.fn.winrestview(view)
  end)
end

---@class ForgeRetainedView
---@field window integer
---@field block? string
---@field row integer Offset inside the retained block.
---@field original_row integer Zero-based native cursor row before the update.
---@field column integer Zero-based byte column.
---@field view table Native winsaveview state.

---@param session table Native buffer replica.
---@param retained fun(id: string): boolean Accepts identities present after the update.
---@return ForgeRetainedView[]
function M.capture(session, retained)
  if not session.preserve_view or session.row_count == 0 then return {} end
  local result = {}
  for _, window in ipairs(vim.fn.win_findbuf(session.buffer)) do
    local cursor = vim.api.nvim_win_get_cursor(window)
    local original_row = cursor[1] - 1
    local node = session.sequence:locate(original_row)
    if node then
      local index, start = session.sequence:position(node.id)
      local block, offset = node.id, original_row - start
      if not retained(block) then
        block, offset = nil, 0
        local ancestor_size
        for _, record in pairs(session.fold and session.fold.record or {}) do
          local _, first = session.sequence:position(record.owner)
          local _, last = session.sequence:position(record.fold["end"].block)
          first = first + record.fold.start.row
          last = last + record.fold["end"].position.row
          if original_row >= first and original_row < last and retained(record.owner)
            and (not ancestor_size or last - first < ancestor_size) then
            block, offset, ancestor_size = record.owner, record.fold.start.row, last - first
          end
        end
        local distance
        for _, direction in ipairs(block and {} or { -1, 1 }) do
          local candidate_index = index + direction
          while candidate_index >= 0 and candidate_index < session.sequence:count() do
            local candidate = session.sequence:at(candidate_index)
            if retained(candidate.id) then
              local _, candidate_start = session.sequence:position(candidate.id)
              local candidate_row = math.max(candidate_start,
                math.min(original_row, candidate_start + candidate.entry.row_count - 1))
              local candidate_distance = math.abs(original_row - candidate_row)
              if not distance or candidate_distance < distance then
                block, offset, distance = candidate.id, candidate_row - candidate_start, candidate_distance
              end
              break
            end
            candidate_index = candidate_index + direction
          end
        end
      end
      result[#result + 1] = {
        window = window, block = block, row = offset, original_row = original_row, column = cursor[2],
        source_row = block and session.projection and not session.project_source
          and require("forge.node_projection").source_position(session, block, {row=offset,column=0}).row or nil,
        view = vim.api.nvim_win_call(window, vim.fn.winsaveview),
      }
    end
  end
  return result
end

---@param session table Native buffer replica after publication.
---@param retained ForgeRetainedView[]
function M.restore(session, retained)
  for _, saved in ipairs(retained) do
    if vim.api.nvim_win_is_valid(saved.window) and vim.api.nvim_win_get_buf(saved.window) == session.buffer then
      local row = math.min(saved.original_row, math.max(0, session.row_count - 1))
      local node = saved.block and session.sequence.node[saved.block]
      if node then
        local _, start = session.sequence:position(saved.block)
        local offset = saved.row
        if saved.source_row and session.next_projection_mapping then
          offset = require("forge.node_projection").visible_position(session.next_projection_mapping, saved.block, saved.source_row) or 0
        end
        row = start + math.min(offset, node.entry.row_count - 1)
      end
      vim.api.nvim_win_call(saved.window, function()
        local column = saved.column
        local line = vim.api.nvim_buf_get_lines(session.buffer, row, row + 1, true)[1] or ""
        local insertion = vim.api.nvim_get_current_win() == saved.window and vim.fn.mode(1):match("^[iR]") ~= nil
        column = math.min(column, math.max(0, #line - (insertion and 0 or 1)))
        while column > 0 do
          local byte = line:byte(column + 1)
          if not byte or byte < 128 or byte >= 192 then break end
          column = column - 1
        end
        local view = vim.deepcopy(saved.view)
        view.lnum, view.col = row + 1, column
        view.topline = math.max(1, math.min(session.row_count,
          saved.view.topline + row - saved.original_row))
        vim.fn.winrestview(view)
      end)
    end
  end
end

return M
