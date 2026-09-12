local M = {}
local sessions = setmetatable({}, { __mode = "v" })
local window_state = {}

local function fold_start(session, record)
  local _, start = session.sequence:position(record.owner)
  return start + record.fold.start.row + 1
end

local function close_open_fold(row)
  -- foldclosed() also returns -1 when the row has no native fold.
  if vim.fn.foldlevel(row) > 0 and vim.fn.foldclosed(row) < 0 then
    vim.cmd(tostring(row) .. "foldclose")
  end
end

local function capture_window(session, window)
  return vim.api.nvim_win_call(window, function()
    local view, state, opened = vim.fn.winsaveview(), {}, {}
    local record_list = {}
    for id, record in pairs(session.fold and session.fold.record or {}) do
      local _, finish = session.sequence:position(record.fold["end"].block)
      finish = finish + record.fold["end"].position.row
        + (record.fold["end"].position.column > 0 and 1 or 0)
      record_list[#record_list + 1] = { id = id, row = fold_start(session, record), finish = finish }
    end
    table.sort(record_list, function(left, right)
      if left.row == right.row then return left.finish > right.finish end
      return left.row < right.row
    end)
    for _, record in ipairs(record_list) do
      local closed = vim.fn.foldclosed(record.row)
      while closed >= 0 and (closed < record.row or vim.fn.foldclosedend(record.row) > record.finish) do
        opened[#opened + 1] = closed
        vim.cmd(tostring(record.row) .. "foldopen")
        closed = vim.fn.foldclosed(record.row)
      end
      if vim.fn.foldlevel(record.row) > 0 then
        state[record.id] = closed == record.row and vim.fn.foldclosedend(record.row) == record.finish
      end
    end
    for index = #opened, 1, -1 do
      vim.cmd(tostring(opened[index]) .. "foldclose")
    end
    vim.fn.winrestview(view)
    return state
  end)
end

local function option_with_pair(option, name, value)
  local pair_list = vim.split(option, ",", { plain = true, trimempty = true })
  local prefix = name .. ":"
  local output = {}
  for _, pair in ipairs(pair_list) do
    if pair:sub(1, #prefix) ~= prefix then output[#output + 1] = pair end
  end
  output[#output + 1] = prefix .. value
  return table.concat(output, ",")
end
vim.api.nvim_create_autocmd("WinClosed", {
  group = vim.api.nvim_create_augroup("ForgeDocumentFolds", { clear = true }),
  callback = function(event) window_state[tonumber(event.match)] = nil end,
})

function M.validate(sequence, changed, retired, state, read_row)
  local seen, affected = {}, {}
  local function position(block, value)
    local node = assert(sequence.node[block], "fold endpoint block is missing")
    assert(type(value) == "table" and type(value.row) == "number" and type(value.column) == "number",
      "invalid fold position")
    assert(value.row >= 0 and value.row == math.floor(value.row) and value.row <= node.entry.row_count
      and value.column >= 0 and value.column == math.floor(value.column), "invalid fold position")
    local _, start = sequence:position(block)
    if value.row == node.entry.row_count then
      assert(value.column == 0, "invalid fold end column")
    else
      local text = assert(read_row(start + value.row), "fold row is missing")
      local byte = text:byte(value.column + 1)
      assert(value.column <= #text and (not byte or byte < 128 or byte >= 192), "fold splits UTF-8")
    end
    return start + value.row, value.column
  end
  local function validate(owner, fold)
    assert(type(fold.id) == "string" and #fold.id > 0 and #fold.id <= 256 and not fold.id:find("%c"),
      "invalid fold identity")
    assert(not seen[fold.id], "duplicate fold identity")
    seen[fold.id] = true
    assert(type(fold.closed) == "boolean", "invalid fold closed state")
    local previous = state and state.record[fold.id]
    assert(not previous or previous.owner == owner or changed[previous.owner] or retired[previous.owner],
      "duplicate fold identity")
    local start_row, start_column = position(owner, fold.start)
    local end_row, end_column = position(fold["end"].block, fold["end"].position)
    assert(start_row < end_row or (start_row == end_row and start_column < end_column), "empty or reversed fold")
  end
  for owner in pairs(changed) do
    local list = sequence.node[owner].entry.metadata.fold or {}
    assert(type(list) == "table" and vim.tbl_count(list) == #list, "expected fold array")
    for _, fold in ipairs(list) do validate(owner, fold) end
  end
  if state then
    for _, set in ipairs({ changed, retired }) do
      for endpoint in pairs(set) do
        for id in pairs(state.endpoint[endpoint] or {}) do affected[id] = true end
      end
    end
    for id in pairs(affected) do
      local record = state.record[id]
      if not changed[record.owner] and not retired[record.owner] then validate(record.owner, record.fold) end
    end
  end
end

local function remove_boundary(session, record)
  session.sequence:fold_boundary(record.owner, "start:" .. record.fold.id)
  session.sequence:fold_boundary(record.fold["end"].block, "end:" .. record.fold.id)
end

local function add_boundary(session, record)
  local fold = record.fold
  local finish = fold["end"].position
  session.sequence:fold_boundary(record.owner, "start:" .. fold.id, fold.start.row, 1)
  session.sequence:fold_boundary(fold["end"].block, "end:" .. fold.id,
    finish.row + (finish.column > 0 and 1 or 0), -1)
end

---@param session table
---@param prepared table
---@param replace_all boolean
function M.update(session, prepared, replace_all)
  if replace_all or not session.fold then session.fold = { record = {}, owner = {}, endpoint = {} } end
  local state, affected = session.fold, {}
  for _, changed in ipairs({ prepared.changed, prepared.retired }) do
    for owner in pairs(changed) do
      for id in pairs(state.owner[owner] or {}) do affected[id] = true end
      for id in pairs(state.endpoint[owner] or {}) do affected[id] = true end
    end
  end
  for id in pairs(affected) do
    local record = state.record[id]
    remove_boundary(session, record)
    if prepared.changed[record.owner] or prepared.retired[record.owner] then
      state.owner[record.owner][id] = nil
      state.endpoint[record.fold["end"].block][id] = nil
      if not next(state.owner[record.owner]) then state.owner[record.owner] = nil end
      if not next(state.endpoint[record.fold["end"].block]) then state.endpoint[record.fold["end"].block] = nil end
      state.record[id] = nil
    end
  end
  for owner in pairs(prepared.changed) do
    for _, fold in ipairs(prepared.block[owner].metadata.fold or {}) do
      assert(not state.record[fold.id], "duplicate fold identity")
      state.record[fold.id] = { owner = owner, fold = fold }
      state.owner[owner] = state.owner[owner] or {}
      state.owner[owner][fold.id] = true
      state.endpoint[fold["end"].block] = state.endpoint[fold["end"].block] or {}
      state.endpoint[fold["end"].block][fold.id] = true
      affected[fold.id] = true
    end
  end
  for id in pairs(affected) do
    if state.record[id] then add_boundary(session, state.record[id]) end
  end
  state.changed = affected
  if not session.fragment then sessions[session.buffer] = session end
end

local function apply_defaults(session, window, saved, changed)
  local opened, closed = {}, {}
  for id in pairs(saved.fold) do
    if not session.fold.record[id] then saved.fold[id] = nil end
  end
  for id in pairs(changed) do
    local record = session.fold.record[id]
    if not record then
      saved.fold[id] = nil
    elseif not saved.fold[id] then
      local _, start = session.sequence:position(record.owner)
      local _, finish = session.sequence:position(record.fold["end"].block)
      start = start + record.fold.start.row
      finish = finish + record.fold["end"].position.row + (record.fold["end"].position.column > 0 and 1 or 0)
      if finish > start + 1 then
        saved.fold[id] = true
        if record.fold.closed then closed[start + 1] = true else opened[start + 1] = true end
      end
    end
  end
  local opening = vim.tbl_keys(opened)
  table.sort(opening)
  vim.api.nvim_win_call(window, function()
    if next(opened) or next(closed) then vim.wo[window].foldexpr = vim.wo[window].foldexpr end
    local view = vim.fn.winsaveview()
    for _, row in ipairs(opening) do
      local ancestor = vim.fn.foldclosed(row)
      while ancestor >= 0 do
        if ancestor ~= row and not opened[ancestor] then closed[ancestor] = true end
        vim.cmd(tostring(row) .. "foldopen")
        local next_ancestor = vim.fn.foldclosed(row)
        assert(next_ancestor ~= ancestor, "native fold did not open")
        ancestor = next_ancestor
      end
    end
    local closing = vim.tbl_keys(closed)
    table.sort(closing, function(left, right) return left > right end)
    for _, row in ipairs(closing) do
      close_open_fold(row)
    end
    vim.fn.winrestview(view)
  end)
end

---@param session table
function M.register(session)
  sessions[session.buffer] = session
end

---@param session table
---@return table<integer, table<string, boolean>>
function M.capture(session)
  local captured = {}
  if session.status ~= "Applied" then return captured end
  for window, saved in pairs(window_state) do
    if saved.session == session and vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == session.buffer then
      captured[window] = capture_window(session, window)
    end
  end
  return captured
end

---@param session table
---@param captured table<integer, table<string, boolean>>
function M.restore(session, captured)
  for window, state in pairs(captured) do
    if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == session.buffer then
      local opened, closed = {}, {}
      for id, was_closed in pairs(state) do
        local record = session.fold.record[id]
        if record then
          local row = fold_start(session, record)
          local target = was_closed and closed or opened
          target[#target + 1] = row
        end
      end
      table.sort(opened)
      table.sort(closed, function(left, right) return left > right end)
      vim.api.nvim_win_call(window, function()
        local view = vim.fn.winsaveview()
        vim.wo[window].foldexpr = vim.wo[window].foldexpr
        for _, row in ipairs(opened) do
          if vim.fn.foldclosed(row) >= 0 then vim.cmd("silent! " .. row .. "foldopen") end
        end
        for _, row in ipairs(closed) do
          close_open_fold(row)
        end
        vim.fn.winrestview(view)
      end)
    end
  end
end

function M.refresh(session)
  for window, saved in pairs(window_state) do
    if saved.session == session and vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == session.buffer then
      apply_defaults(session, window, saved, session.fold.changed or {})
    end
  end
end

---@return string|table[]
function M.text()
  local buffer = vim.api.nvim_get_current_buf()
  local text = vim.api.nvim_buf_get_lines(buffer, vim.v.foldstart - 1, vim.v.foldstart, true)[1] or ""
  local session = sessions[buffer]
  if session and session.header_text then
    local chunks = session.header_text(vim.v.foldstart - 1)
    if chunks then return chunks end
  end
  local node = session and session.sequence:locate(vim.v.foldstart - 1)
  if not node then return text end
  local _, start = session.sequence:position(node.id)
  local relative = vim.v.foldstart - 1 - start
  local boundary, spans = { [0] = true, [#text] = true }, {}
  local decoration = vim.list_extend({}, node.entry.metadata.decoration or {})
  vim.list_extend(decoration, node.entry.metadata.visible_decoration or {})
  for _, span in ipairs(decoration) do
    if span.range.start.row <= relative and (span.range["end"].row > relative
      or span.range["end"].row == relative and span.range["end"].column > 0) then
      local first = span.range.start.row == relative and span.range.start.column or 0
      local last = span.range["end"].row == relative and span.range["end"].column or #text
      first, last = math.min(first, #text), math.min(last, #text)
      boundary[first], boundary[last] = true, true
      spans[#spans + 1] = { first = first, last = last, capture = span.capture, priority = span.priority }
    end
  end
  local conceal = {}
  if vim.wo.conceallevel > 0 then
    for _, span in ipairs(node.entry.metadata.conceal or {}) do
      if span.range.start.row <= relative and (span.range["end"].row > relative
        or span.range["end"].row == relative and span.range["end"].column > 0) then
        local first = span.range.start.row == relative and span.range.start.column or 0
        local last = span.range["end"].row == relative and span.range["end"].column or #text
        first, last = math.min(first, #text), math.min(last, #text)
        boundary[first], boundary[last] = true, true
        conceal[#conceal + 1] = { first = first, last = last, replacement = span.replacement, priority = span.priority }
      end
    end
  end
  local column = vim.tbl_keys(boundary)
  table.sort(column)
  local chunks = {}
  for index = 1, #column - 1 do
    local first, last = column[index], column[index + 1]
    local capture, priority = "Normal", -1
    for _, span in ipairs(spans) do
      if span.first <= first and span.last >= last and span.priority >= priority then
        capture, priority = span.capture, span.priority
      end
    end
    local hidden
    for _, span in ipairs(conceal) do
      if span.first <= first and span.last >= last and (not hidden or span.priority >= hidden.priority) then hidden = span end
    end
    if not hidden then chunks[#chunks + 1] = { text:sub(first + 1, last), capture }
    elseif hidden.first == first and vim.wo.conceallevel < 3 then
      local replacement = hidden.replacement
      if replacement == "" and vim.wo.conceallevel == 1 then replacement = " " end
      if replacement ~= "" then chunks[#chunks + 1] = { replacement, capture } end
    end
  end
  return chunks
end

---@return integer|string
function M.expression()
  local session = sessions[vim.api.nvim_get_current_buf()]
  if not session or (session.status ~= "Applied" and not session.applying) then return 0 end
  if session.editable.suspended and not session.applying then return 0 end
  local row, delta = vim.v.lnum - 1, 0
  for index = #(session.fold_pending or {}), session.fold_pending_index or 1, -1 do
    local edit = session.fold_pending[index]
    if row >= edit.start_row + edit.removed_rows then
      delta = delta + #edit.text - edit.removed_rows
    elseif row >= edit.start_row then
      if #edit.text == 0 then return 0 end
      row = edit.start_row + math.min(row - edit.start_row, #edit.text - 1)
      break
    end
  end
  local level, starts = session.sequence:fold_level(row + delta)
  return starts and level > 0 and (">" .. level) or level
end

---@param session table
---@param window integer
function M.attach(session, window)
  assert(vim.api.nvim_win_get_buf(window) == session.buffer, "fold window has another document")
  if window_state[window] and window_state[window].session == session then return end
  M.release(window)
  local retained = session.fold_view and session.fold_view[window]
  local saved = { session = session, fold = retained and vim.deepcopy(retained.fold) or {} }
  for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars", "winhighlight" }) do saved[name] = vim.wo[window][name] end
  window_state[window] = saved
  vim.wo[window].foldmethod = "expr"
  vim.wo[window].foldexpr = "v:lua.require'forge.folds'.expression()"
  vim.wo[window].foldenable = true
  vim.wo[window].foldlevel = 99
  vim.wo[window].foldtext = "v:lua.require'forge.folds'.text()"
  local fillchars = saved.fillchars:gsub("^fold:[^,]*,?", ""):gsub(",fold:[^,]*", "")
  vim.wo[window].fillchars = fillchars .. (fillchars == "" and "" or ",") .. "fold: "
  vim.wo[window].winhighlight = option_with_pair(saved.winhighlight, "Folded", "Normal")
  if session.fold then apply_defaults(session, window, saved, session.fold.record) end
  if retained then M.restore(session, { [window] = retained.state }) end
end

function M.release(window)
  local saved = window_state[window]
  if not saved then return end
  local session = saved.session
  if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == session.buffer then
    local retained = { fold = vim.deepcopy(saved.fold), state = capture_window(session, window) }
    session.fold_view = session.fold_view or {}
    session.fold_view[window] = retained
  end
  window_state[window] = nil
  if vim.api.nvim_win_is_valid(window) then
    for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars", "winhighlight" }) do
      vim.wo[window][name] = saved[name]
    end
  end
end

function M.restore_inherited(origin, target)
  local saved = window_state[origin]
  if not saved then return end
  if vim.wo[target].foldexpr == "v:lua.require'forge.folds'.expression()" then
    for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars" }) do
      vim.wo[target][name] = saved[name]
    end
  end
  if origin == target then window_state[origin] = nil end
end

---@param session table
function M.detach(session)
  sessions[session.buffer] = nil
  session.fold_view = nil
  for window, saved in pairs(window_state) do
    if saved.session == session then M.release(window) end
  end
end

return M
