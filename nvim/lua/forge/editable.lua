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

function M.take_pending(state, region)
  local entry = assert(state.region[region], "unknown editable region")
  if entry.sent or entry.conflict or not entry.pending then
    return nil
  end
  entry.sent = {
    sequence = entry.pending.sequence,
    base = entry.revision,
    text = entry.pending.text,
  }
  return {
    document = state.document,
    region = region,
    base = entry.revision,
    sequence = entry.pending.sequence,
    text = copy_rows(entry.pending.text),
  }
end

function M.acknowledge(state, acknowledgement)
  local entry = state.region[acknowledgement.region]
  if acknowledgement.document ~= state.document or not entry or not entry.sent then
    return false, "acknowledgement has no matching in-flight edit"
  end
  if acknowledgement.sequence ~= entry.sent.sequence
    or not valid_counter(acknowledgement.revision)
    or acknowledgement.revision ~= entry.sent.base + 1
  then
    return false, "acknowledgement does not match the accepted edit"
  end
  entry.revision = acknowledgement.revision
  entry.accepted_text = entry.sent.text
  if entry.pending.sequence == entry.sent.sequence then
    entry.pending = nil
  end
  entry.sent = nil
  local native = state.native
  if entry.pending and native then
    vim.schedule(function()
      if state.native == native and native.active then
        M.flush(state)
      end
    end)
  end
  return true
end

function M.conflict(state, response)
  local entry = state.region[response.region]
  if response.document ~= state.document or not entry or not entry.sent
    or response.sequence ~= entry.sent.sequence
  then
    return false, "conflict has no matching in-flight edit"
  end
  entry.conflict = true
  entry.sent = nil
  return true
end

function M.suspend_generated_text(state)
  return state.suspended or state.native and state.native.rejecting or false
end

function M.ready_to_reconcile(state)
  if not state.suspended or state.fault or state.native and state.native.rejecting then
    return false
  end
  for _, entry in pairs(state.region) do
    if entry.pending or entry.sent or entry.conflict then
      return false
    end
  end
  return true
end

function M.reconciled(state, revision_by_region)
  if not M.ready_to_reconcile(state) then
    return false, "local edits still require acknowledgement"
  end
  for region, entry in pairs(state.region) do
    if revision_by_region[region] ~= entry.revision then
      return false, "snapshot does not contain the acknowledged region revision"
    end
  end
  state.suspended = false
  return true
end

function M.disconnect(state)
  for _, entry in pairs(state.region) do
    entry.sent = nil
    if entry.pending then
      entry.conflict = true
    end
  end
  state.suspended = true
end

function M.resolve(state, region, revision, rows)
  local entry = assert(state.region[region], "unknown editable region")
  assert(entry.conflict, "region has no unresolved conflict")
  assert(valid_counter(revision), "invalid region revision")
  assert(revision < MAX_COUNTER, "region revision exhausted")
  local sequence = M.record(state, region, rows)
  entry.revision = revision
  entry.conflict = nil
  return sequence
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

function M.flush(state)
  local native = state.native
  if not native or not native.active or state.fault then
    return false
  end
  native.timer:stop()
  native.deadline = nil
  for region in pairs(state.region) do
    local request = M.take_pending(state, region)
    if request then
      local ok, sent = pcall(native.send, request)
      if not ok or sent ~= true then
        M.disconnect(state)
        return false
      end
    end
  end
  return true
end

local function schedule_flush(state)
  local native = state.native
  local now = vim.uv.hrtime() / 1000000
  native.deadline = native.deadline or (now + native.max_delay)
  native.generation = native.generation + 1
  local generation = native.generation
  native.timer:start(math.max(1, math.floor(math.min(native.delay, native.deadline - now))), 0,
    vim.schedule_wrap(function()
      if state.native == native and native.active and native.generation == generation then
        M.flush(state)
      end
    end))
end

local function native_fault(state, message)
  state.suspended = true
  state.fault = message
  local native = state.native
  native.timer:stop()
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
  native.timer:stop()
  native.timer:close()
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

function M.attach(state, buffer, region_ranges, options)
  assert(not state.native, "native editing is already attached")
  assert(vim.api.nvim_buf_is_valid(buffer), "invalid native buffer")
  assert(type(options.send) == "function", "local edit sender is required")
  local delay, max_delay = options.delay or 120, options.max_delay or 500
  assert(delay >= 1 and delay <= max_delay and max_delay <= 1000, "invalid edit debounce bounds")
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
    buffer = buffer, anchor = anchor, send = options.send, notice = options.notice, restored = options.restored,
    delay = delay, max_delay = max_delay, timer = assert(vim.uv.new_timer()), active = true, generation = 0,
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
      schedule_flush(state)
    end,
    on_reload = function()
      if not native.active then
        return true
      end
      native_fault(state, "native buffer reloaded and requires explicit reconciliation")
    end,
    on_detach = function()
      if native.active then
        M.disconnect(state)
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
