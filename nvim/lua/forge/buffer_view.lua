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

---@class ForgeRetainedPosition
---@field block? string
---@field row integer Offset inside the retained block.
---@field original_row integer Zero-based native cursor row before the update.
---@field column integer Zero-based byte column.
---@field source_column? boolean Column is canonical and requires projection on restore.
---@field projection_offset? integer Physical offset within an attached annotation.
---@field source_row? integer Row in the canonical source before projection.
---@field column_offset? integer Virtual screen-column offset inside a character.
---@field source_anchor? {owner: string, before?: integer, after?: integer} Canonical source location when its display block disappears.

---@param session table Native buffer replica.
---@param cursor integer[] One-based row and zero-based byte column.
---@param retained fun(id: string): boolean Accepts identities present after the update.
---@return ForgeRetainedPosition
local function capture_position(session, cursor, retained)
  local original_row = cursor[1] - 1
  local source = session.source_projected and session.locate and session.locate(original_row, cursor[2]) or nil
  local node = source and session.sequence.node[source.block] or session.sequence:locate(original_row)
  if node then
    local index, start = session.sequence:position(node.id)
    local canonical_row = source and start + source.position.row or original_row
    local block, offset = node.id, canonical_row - start
    local source_anchor
    local represented = node.entry.metadata.source_anchor
    if not retained(block) and represented and represented ~= vim.NIL then
      source_anchor = { owner = represented.owner }
      for _, side in ipairs({ "before", "after" }) do
        local range = represented[side]
        if range and range ~= vim.NIL then
          source_anchor[side] = math.min(range["end"] - 1, range.start + offset)
        end
      end
    end
    local projection_offset = source and session.physical_row
      and original_row - session.physical_row(canonical_row) or nil
    if not retained(block) and session.retain_removed_position then
      block, offset, projection_offset = nil, 0, nil
    elseif not retained(block) then
      projection_offset = nil
      block, offset = nil, 0
      local ancestor_size
      for _, record in pairs(session.fold and session.fold.record or {}) do
        local _, first = session.sequence:position(record.owner)
        local _, last = session.sequence:position(record.fold["end"].block)
        first = first + record.fold.start.row
        last = last + record.fold["end"].position.row
        if canonical_row >= first and canonical_row < last and retained(record.owner)
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
              math.min(canonical_row, candidate_start + candidate.entry.row_count - 1))
            local candidate_distance = math.abs(canonical_row - candidate_row)
            if not distance or candidate_distance < distance then
              block, offset, distance = candidate.id, candidate_row - candidate_start, candidate_distance
            end
            break
          end
          candidate_index = candidate_index + direction
        end
      end
    end
    return {
      block = block, row = offset, original_row = original_row,
      source_anchor = source_anchor,
      column = source and projection_offset == 0 and source.position.column or cursor[2],
      source_column = source ~= nil and projection_offset == 0,
      projection_offset = projection_offset,
      source_row = block and session.projection and not session.project_source
        and require("forge.node_projection").source_position(session, block, {row=offset,column=0}).row or nil,
    }
  end
  return { row = 0, original_row = original_row, column = cursor[2] }
end

---@class ForgeRetainedView: ForgeRetainedPosition
---@field window integer
---@field view table Native winsaveview state.
---@field selection? {mode: string, anchor: ForgeRetainedPosition}

---@param session table Native buffer replica.
---@param retained fun(id: string): boolean Accepts identities present after the update.
---@return ForgeRetainedView[]
function M.capture(session, retained)
  if not session.preserve_view then return {} end
  local result = {}
  local active_window, mode = vim.api.nvim_get_current_win(), vim.fn.mode(1)
  local selecting = mode == "v" or mode == "V" or mode == "\22"
    or mode == "s" or mode == "S" or mode == "\19"
  for _, window in ipairs(vim.fn.win_findbuf(session.buffer)) do
    ---@type ForgeRetainedView
    local saved = vim.tbl_extend("force", capture_position(session, vim.api.nvim_win_get_cursor(window), retained), {
      window = window,
      view = vim.api.nvim_win_call(window, vim.fn.winsaveview),
    })
    if window == active_window and selecting then
      local position = vim.fn.getpos("v")
      local anchor = capture_position(session, { position[2], position[3] - 1 }, retained)
      anchor.column_offset = position[4]
      saved.selection = { mode = mode, anchor = anchor }
    end
    result[#result + 1] = saved
  end
  return result
end

---@param session table Native buffer replica after publication.
---@param saved ForgeRetainedPosition
---@param allow_eol boolean Whether the current mode permits the end-of-line byte boundary.
---@return integer row
---@return integer column
---@return integer row_count
local function resolve_position(session, saved, allow_eol)
  local row = math.min(saved.original_row, math.max(0, session.row_count - 1))
  local node = saved.block and session.sequence.node[saved.block]
  local source_anchor = saved.source_anchor
  local source_offset, nearest_distance, nearest_extent
  if source_anchor then
    for index = 0, session.sequence:count() - 1 do
      local candidate = session.sequence:at(index)
      local represented = candidate.entry.metadata.source_anchor
      if represented and represented ~= vim.NIL and represented.owner == source_anchor.owner then
        for _, side in ipairs({ "after", "before" }) do
          local line, range = source_anchor[side], represented[side]
          if line and range and range ~= vim.NIL then
            local distance = math.max(range.start - line, line - range["end"] + 1, 0)
            local extent = range["end"] - range.start
            if not nearest_distance or distance < nearest_distance
              or distance == nearest_distance and extent < nearest_extent then
              node, nearest_distance, nearest_extent = candidate, distance, extent
              local offset = math.max(0, line - range.start)
              source_offset = offset < candidate.entry.row_count and offset or 0
            end
          end
        end
      end
    end
  end
  if node then
    local _, start = session.sequence:position(node.id)
    local offset = source_offset or saved.row
    if source_offset == nil and saved.source_row and session.next_projection_mapping then
      offset = require("forge.node_projection").visible_position(session.next_projection_mapping, saved.block, saved.source_row) or 0
    end
    row = start + math.min(offset, node.entry.row_count - 1)
  end
  local row_count = session.row_count
  local canonical_row = row
  if session.source_projected and session.physical_row then
    row_count = vim.api.nvim_buf_line_count(session.buffer)
    row = math.min(row_count - 1, session.physical_row(row) + (saved.projection_offset or 0))
  end
  local column = saved.source_column and require("forge.buffer").physical_column(session, canonical_row, saved.column) or saved.column
  local line = vim.api.nvim_buf_get_lines(session.buffer, row, row + 1, true)[1] or ""
  column = math.min(column, math.max(0, #line - (allow_eol and 0 or 1)))
  while column > 0 do
    local byte = line:byte(column + 1)
    if not byte or byte < 128 or byte >= 192 then break end
    column = column - 1
  end
  return row, column, row_count
end

---@param session table Native buffer replica after publication.
---@param retained ForgeRetainedView[]
function M.restore(session, retained)
  local active_window = vim.api.nvim_get_current_win()
  for _, saved in ipairs(retained) do
    if vim.api.nvim_win_is_valid(saved.window) and vim.api.nvim_win_get_buf(saved.window) == session.buffer then
      vim.api.nvim_win_call(saved.window, function()
        local mode = vim.fn.mode(1)
        local selection = saved.selection
        local selecting = selection ~= nil and active_window == saved.window and mode == selection.mode
        local allow_eol = (active_window == saved.window and mode:match("^[iR]") ~= nil)
          or (selecting and vim.o.selection == "exclusive")
        local row, column, row_count = resolve_position(session, saved, allow_eol)
        local select_mode = selecting and (mode == "s" or mode == "S" or mode == "\19")
        if selection and selecting then
          local anchor_row, anchor_column = resolve_position(session, selection.anchor, allow_eol)
          local offset = anchor_column == selection.anchor.column and selection.anchor.column_offset or 0
          if select_mode then vim.cmd.normal({ args = { "\7" }, bang = true }) end
          -- Swap endpoints to restore the live Visual anchor without leaving Visual mode.
          vim.cmd.normal({ args = { "o" }, bang = true })
          vim.fn.setpos(".", { 0, anchor_row + 1, anchor_column + 1, offset })
          vim.cmd.normal({ args = { "o" }, bang = true })
        end
        local view = vim.deepcopy(saved.view)
        view.lnum, view.col = row + 1, column
        if column ~= saved.column then view.coladd = 0 end
        view.topline = math.max(1, math.min(row_count,
          saved.view.topline + row - saved.original_row))
        vim.fn.winrestview(view)
        if select_mode then vim.cmd.normal({ args = { "\7" }, bang = true }) end
      end)
    end
  end
end

return M
