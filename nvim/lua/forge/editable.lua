local M = {}

local MAX_COUNTER = 9007199254740991

---@class ForgeEditableAnchor
---@field start {row: integer, column: integer}
---@field finish {row: integer, column: integer}
---@field layout_revision? integer

local function valid_counter(value)
  return type(value) == "number" and value >= 0 and value <= MAX_COUNTER and value == math.floor(value)
end

local function copy_rows(rows)
  assert(type(rows) == "table", "region text must contain rows")
  local result = {}
  for index, row in ipairs(rows) do
    assert(type(row) == "string" and not row:find("[\n%z]"), "invalid region row")
    result[index] = row
  end
  assert(vim.tbl_count(rows) == #result, "region text must be a dense array")
  return result
end

function M.new(document)
  assert(type(document) == "string" and document ~= "", "document identity is required")
  return { document = document, sequence = 0, region = {}, suspended = false }
end

function M.register(state, region, revision)
  assert(type(region) == "string" and region ~= "", "region identity is required")
  assert(valid_counter(revision), "invalid region revision")
  assert(not state.region[region], "region already registered")
  state.region[region] = { revision = revision }
end

function M.record(state, region, rows)
  local entry = assert(state.region[region], "unknown editable region")
  local text = copy_rows(rows)
  assert(entry.revision < MAX_COUNTER, "region revision exhausted")
  assert(state.sequence < MAX_COUNTER, "edit sequence exhausted")
  state.sequence = state.sequence + 1
  entry.pending = { sequence = state.sequence, text = text }
  state.suspended = true
  return state.sequence
end

function M.suspend_generated_text(state)
  return state.suspended or state.native and state.native.rejecting or false
end

function M.recoverable_text(state, region)
  local entry = assert(state.region[region], "unknown editable region")
  return entry.pending and copy_rows(entry.pending.text) or nil
end

local function before_or_equal(left, right)
  return left.row < right.row or (left.row == right.row and left.column <= right.column)
end

local function native_rows(buffer, anchor)
  local count = vim.api.nvim_buf_line_count(buffer)
  for _, position in ipairs({ anchor.start, anchor.finish }) do
    assert(valid_counter(position.row) and valid_counter(position.column), "invalid native position")
    assert(position.row <= count, "native position is outside buffer")
    if position.row == count then
      assert(position.column == 0, "invalid end-of-buffer column")
    else
      local row = vim.api.nvim_buf_get_lines(buffer, position.row, position.row + 1, true)[1]
      local byte = row:byte(position.column + 1)
      assert(position.column <= #row and (not byte or byte < 128 or byte >= 192), "native position splits UTF-8")
    end
  end
  if anchor.finish.row == count then
    local rows = vim.api.nvim_buf_get_lines(buffer, anchor.start.row, count, true)
    if #rows > 0 then
      rows[1] = rows[1]:sub(anchor.start.column + 1)
    end
    rows[#rows + 1] = ""
    return rows
  end
  return vim.api.nvim_buf_get_text(buffer, anchor.start.row, anchor.start.column,
    anchor.finish.row, anchor.finish.column, {})
end

local function refresh_anchor(native, region)
  local anchor = native.anchor[region]
  if native.resolve and anchor.layout_revision ~= native.layout_revision then
    anchor = native.resolve(region)
    anchor.layout_revision = native.layout_revision
    native.anchor[region] = anchor
  end
  return anchor
end

function M.guard_region(state, start, finish)
  if not state.native or not state.native.active then
    return nil, "editable buffer attachment is not active"
  end
  local owner
  for region in pairs(state.native.anchor) do
    local anchor = refresh_anchor(state.native, region)
    if before_or_equal(anchor.start, start) and before_or_equal(finish, anchor.finish) then
      if owner then
        return nil, "edit lies on an ambiguous editable boundary"
      end
      owner = region
    end
  end
  if owner then return owner end
  return nil, "edit crosses a read-only boundary"
end

function M.capture(state, region)
  local native = assert(state.native, "native editing is not attached")
  local anchor = assert(refresh_anchor(native, region), "unknown native region")
  return M.record(state, region, native_rows(native.buffer, anchor))
end

---Remove trailing line breaks from a native input and retain the normalized text for dispatch.
---@param state {native: {buffer: integer, active: boolean, anchor: table<string, ForgeEditableAnchor>}, fault?: string}
---@param region string
---@return string? text
---@return string? failure
function M.trim_trailing_newlines(state, region)
  local native = state.native
  if not native or not native.active or state.fault then return nil, state.fault or "Native input is unavailable" end
  local anchor = refresh_anchor(native, region)
  if not anchor then return nil, "Unknown native input: " .. region end
  local text = table.concat(native_rows(native.buffer, anchor), "\n")
  local trimmed = text:gsub("[\r\n]+$", "")
  if trimmed == text then return text end
  local rows = vim.split(trimmed, "\n", { plain = true })
  local row = anchor.start.row + #rows - 1
  local column = #rows[#rows] + (#rows == 1 and anchor.start.column or 0)
  local finish = anchor.finish
  local count = vim.api.nvim_buf_line_count(native.buffer)
  if finish.row == count then
    finish = { row = count - 1, column = #vim.api.nvim_buf_get_lines(native.buffer, count - 1, count, false)[1] }
  end
  local modifiable = vim.bo[native.buffer].modifiable
  vim.bo[native.buffer].modifiable = true
  local ok, failure = pcall(vim.api.nvim_buf_set_text, native.buffer, row, column, finish.row, finish.column, { "" })
  vim.bo[native.buffer].modifiable = modifiable
  if not ok or state.fault then return nil, state.fault or tostring(failure) end
  if anchor.finish.row == vim.api.nvim_buf_line_count(native.buffer) then
    anchor.finish = { row = row, column = column }
    M.capture(state, region)
  end
  return trimmed
end



---@param state table
---@param selected? table<string, boolean>
---@return table[]
function M.capture_draft(state, selected)
  assert(not state.fault, state.fault)
  local captured = {}
  for region, entry in pairs(state.region) do
    if (not selected or selected[region]) and entry.pending then
      captured[#captured + 1] = { document = state.document, region = region,
        base = entry.revision, sequence = entry.pending.sequence,
        text = table.concat(entry.pending.text, "\n") }
    end
  end
  table.sort(captured, function(left, right) return left.region < right.region end)
  return captured
end

---@param state table
---@param captured table[]
function M.saved_capture(state, captured)
  for _, capture in ipairs(captured) do
    local entry = state.region[capture.region]
    if entry then
      entry.revision = capture.base + 1
      if entry.pending and entry.pending.sequence == capture.sequence then entry.pending = nil end
    end
  end
  state.suspended = false
  for _, entry in pairs(state.region) do
    if entry.pending then state.suspended = true end
  end
end

local function native_fault(state, message)
  state.suspended = true
  state.fault = message
  local native = state.native
  if native.notice then
    vim.schedule(function()
      if native.active then
        native.notice(message)
      end
    end)
  end
end

---Restore the last permitted text and pre-splice marks after a rejected native edit.
---@param state table
---@param message string
---@param start {row: integer, column: integer}
---@param finish {row: integer, column: integer}
local function reject_edit(state, message, start, finish)
  local native = state.native
  local marks = vim.api.nvim_buf_get_extmarks(native.buffer, -1, 0, -1, { details = true })
  local cursor = {}
  for _, window in ipairs(vim.fn.win_findbuf(native.buffer)) do
    cursor[window] = vim.api.nvim_win_get_cursor(window)
    local region = M.guard_region(state, finish, finish) or M.guard_region(state, start, start)
    if region then
      local anchor = refresh_anchor(native, region)
      local position = { row = cursor[window][1] - 1, column = cursor[window][2] }
      if not before_or_equal(anchor.start, position) then
        cursor[window] = { anchor.start.row + 1, anchor.start.column }
      elseif not before_or_equal(position, anchor.finish) then
        cursor[window] = { anchor.finish.row + 1, anchor.finish.column }
      end
    end
  end
  native.rejecting = true
  vim.schedule(function()
    if not native.active or state.native ~= native or not vim.api.nvim_buf_is_valid(native.buffer) then return end
    local modifiable = vim.bo[native.buffer].modifiable
    local modified = native.modified
    local ok, failure = pcall(function()
      vim.bo[native.buffer].modifiable = true
      vim.api.nvim_buf_call(native.buffer, function() pcall(vim.cmd, "undojoin") end)
      vim.api.nvim_buf_set_lines(native.buffer, 0, -1, false, native.shadow)
      for _, mark in ipairs(marks) do
        local details = mark[4]
        local namespace = details.ns_id
        details.ns_id, details.invalid = nil, nil
        details.id = mark[1]
        vim.api.nvim_buf_set_extmark(native.buffer, namespace, mark[2], mark[3], details)
      end
      for window, position in pairs(cursor) do
        if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == native.buffer then
          position[1] = math.min(position[1], #native.shadow)
          position[2] = math.min(position[2], #native.shadow[position[1]])
          vim.api.nvim_win_set_cursor(window, position)
        end
      end
      vim.bo[native.buffer].modified = modified
      if native.restored then native.restored() end
    end)
    vim.bo[native.buffer].modifiable = modifiable
    native.rejecting = nil
    if not ok then native_fault(state, tostring(failure))
    else
      vim.api.nvim_exec_autocmds("TextChanged", { buffer = native.buffer })
      if native.notice then native.notice(message) end
    end
  end)
end

function M.detach(state)
  local native = state.native
  if not native then
    return
  end
  native.active = false
  native.generation = native.generation + 1
  state.native = nil
end

---@param state table
---@param active boolean
function M.applying(state, active)
  if state.native then state.native.applying = active end
end

---@param state table
---@param anchor table<string, ForgeEditableAnchor>
---@param region_state table<string, {revision: integer}>
---@param removed table<string, boolean>
---@param resolve fun(region: string): ForgeEditableAnchor
function M.update_regions(state, anchor, region_state, removed, resolve)
  assert(not state.suspended, "cannot replace regions during local editing")
  local native = assert(state.native, "native editing is not attached")
  native.layout_revision = (native.layout_revision or 0) + 1
  native.resolve = resolve
  for region in pairs(removed) do
    state.region[region], native.anchor[region] = nil, nil
  end
  for region, positions in pairs(anchor) do
    native_rows(native.buffer, positions)
    positions.layout_revision = native.layout_revision
    state.region[region], native.anchor[region] = region_state[region], positions
  end
end

---@param state table
---@param anchor table<string, ForgeEditableAnchor>
function M.reanchor(state, anchor)
  local native = assert(state.native, "native editing is not attached")
  assert(native.applying, "local presentation must guard buffer mutations")
  local replacement = {}
  local layout_revision = (native.layout_revision or 0) + 1
  for region, positions in pairs(anchor) do
    assert(state.region[region], "presentation references an unknown draft region")
    native_rows(native.buffer, positions)
    replacement[region] = vim.deepcopy(positions)
    replacement[region].layout_revision = layout_revision
  end
  native.layout_revision = layout_revision
  native.anchor = replacement
end

function M.attach(state, buffer, region_ranges, options)
  assert(not state.native, "native editing is already attached")
  assert(vim.api.nvim_buf_is_valid(buffer), "invalid native buffer")
  local anchor = {}
  for region, positions in pairs(region_ranges) do
    assert(state.region[region], "native range has no registered region")
    local start, finish = positions.start, positions.finish
    assert(before_or_equal(start, finish), "reversed editable region")
    native_rows(buffer, positions)
    for _, previous in pairs(anchor) do
      assert(before_or_equal(finish, previous.start) or before_or_equal(previous.finish, start), "editable regions overlap")
    end
    anchor[region] = vim.deepcopy(positions)
  end
  local native = {
    buffer = buffer, anchor = anchor, notice = options.notice, restored = options.restored,
    active = true, generation = 0,
    shadow = vim.api.nvim_buf_get_lines(buffer, 0, -1, false), modified = vim.bo[buffer].modified,
  }
  state.native = native
  local attached = vim.api.nvim_buf_attach(buffer, false, {
    on_changedtick = function()
      if not native.active then return true end
      if not native.applying and not native.rejecting and native.restored then native.restored() end
    end,
    on_bytes = function(_, _, _, row, column, _, old_rows, old_column, _, new_rows, new_column)
      if not native.active then
        return true
      end
      if native.rejecting then return end
      local function retain_rows()
        local replacement = vim.api.nvim_buf_get_lines(buffer, row, row + new_rows + 1, false)
        if #replacement == old_rows + 1 then
          for index, text in ipairs(replacement) do native.shadow[row + index] = text end
        else
          for _ = 1, old_rows + 1 do
            if row + 1 <= #native.shadow then table.remove(native.shadow, row + 1) end
          end
          for index = #replacement, 1, -1 do table.insert(native.shadow, row + 1, replacement[index]) end
        end
        native.modified = vim.bo[buffer].modified
      end
      if native.applying then retain_rows() return end
      if state.fault then
        return
      end
      local start = { row = row, column = column }
      local finish = { row = row + old_rows, column = old_rows == 0 and column + old_column or old_column }
      local new_finish = { row = row + new_rows, column = new_rows == 0 and column + new_column or new_column }
      local owner, message = M.guard_region(state, start, finish)
      if not owner and finish.row == #native.shadow and finish.column == 0
        and new_finish.row == vim.api.nvim_buf_line_count(buffer) and new_finish.column == 0 then
        local last_row = vim.api.nvim_buf_get_lines(buffer, -2, -1, false)[1]
        local text_finish = { row = #native.shadow - 1, column = #native.shadow[#native.shadow] }
        local text_new_finish = { row = new_finish.row - 1, column = #last_row }
        owner, message = M.guard_region(state, start, text_finish)
        if owner then
          finish, new_finish = text_finish, text_new_finish
        end
      end
      if not owner then
        reject_edit(state, message, start, finish)
        return
      end
      retain_rows()
      state.suspended = true
      local function shift(position)
        if position.row == finish.row then
          position.column = new_finish.column + position.column - finish.column
        end
        position.row = new_finish.row + position.row - finish.row
      end
      for region, positions in pairs(native.anchor) do
        if region == owner then
          shift(positions.finish)
        elseif before_or_equal(finish, positions.start) then
          shift(positions.start)
          shift(positions.finish)
        end
      end
      local ok, failure = pcall(M.capture, state, owner)
      if not ok then
        native_fault(state, tostring(failure))
        return
      end
    end,
    on_reload = function()
      if not native.active then
        return true
      end
      native_fault(state, "native buffer reloaded and requires explicit reconciliation")
    end,
    on_detach = function()
      if native.active then
        M.detach(state)
      end
    end,
  })
  if not attached then
    M.detach(state)
    error("native buffer attachment failed")
  end
end

return M
