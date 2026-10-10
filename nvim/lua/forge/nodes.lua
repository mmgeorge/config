local M = {}
local sessions = setmetatable({}, { __mode = "v" })
local window_state = {}
local cooperative = require("forge.cooperative")
local heading_display, full_display = { display = "heading" }, { display = "full" }

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
    assert(fold.collapsed_suffix == nil or (type(fold.collapsed_suffix) == "string"
      and #fold.collapsed_suffix <= 1024 and not fold.collapsed_suffix:find("%c")), "invalid fold summary suffix")
    assert(fold.collapse_children == nil or type(fold.collapse_children) == "boolean", "invalid fold child policy")
    assert(fold.expand_children == nil or type(fold.expand_children) == "boolean", "invalid fold expansion policy")
    assert(not (fold.collapse_children and fold.expand_children), "conflicting fold child policies")
    local previous = state and state.record[fold.id]
    assert(not previous or previous.owner == owner or changed[previous.owner] or retired[previous.owner],
      "duplicate fold identity")
    local start_row, start_column = position(owner, fold.start)
    if fold.heading_start then
      local heading_row, heading_column = position(fold.heading_start.block, fold.heading_start.position)
      assert(heading_row < start_row or (heading_row == start_row and heading_column <= start_column),
        "fold heading follows its start")
    end
    local end_row, end_column = position(fold["end"].block, fold["end"].position)
    assert(start_row < end_row or (start_row == end_row and start_column < end_column), "empty or reversed fold")
  end
  for owner in pairs(changed) do
    local list = sequence.node[owner].entry.metadata.fold or {}
    assert(type(list) == "table" and vim.tbl_count(list) == #list, "expected fold array")
    for _, fold in ipairs(list) do validate(owner, fold) cooperative.checkpoint() end
    cooperative.checkpoint()
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
  local retained = {}
  for owner in pairs(prepared.changed) do
    for _, fold in ipairs(prepared.block[owner].metadata.fold or {}) do
      local previous = state.record[fold.id]
      if prepared.retain_folds and previous and previous.owner == owner and vim.deep_equal(previous.fold, fold) then
        retained[fold.id] = true
      end
    end
    cooperative.checkpoint()
  end
  for _, changed in ipairs({ prepared.changed, prepared.retired }) do
    for owner in pairs(changed) do
      for id in pairs(state.owner[owner] or {}) do affected[id] = true end
      for id in pairs(state.endpoint[owner] or {}) do affected[id] = true end
    end
  end
  for id in pairs(affected) do
    local record = state.record[id]
    if prepared.retain_folds and (retained[id] or not prepared.changed[record.owner] and not prepared.retired[record.owner]) then
      affected[id] = nil
    end
  end
  for id in pairs(affected) do
    local record = state.record[id]
    remove_boundary(session, record)
    if prepared.changed[record.owner] or prepared.retired[record.owner] then
      state.owner[record.owner][id] = nil
      if record.fold.heading_start then
        local heading = record.fold.heading_start.block
        state.endpoint[heading][id] = nil
        if not next(state.endpoint[heading]) then state.endpoint[heading] = nil end
      end
      local endpoint = record.fold["end"].block
      if state.endpoint[endpoint] then
        state.endpoint[endpoint][id] = nil
        if not next(state.endpoint[endpoint]) then state.endpoint[endpoint] = nil end
      end
      if not next(state.owner[record.owner]) then state.owner[record.owner] = nil end
      state.record[id] = nil
    end
  end
  for owner in pairs(prepared.changed) do
    for _, fold in ipairs(prepared.block[owner].metadata.fold or {}) do
      if not retained[fold.id] then
        assert(not state.record[fold.id], "duplicate fold identity")
        state.record[fold.id] = { owner = owner, fold = fold }
        state.owner[owner] = state.owner[owner] or {}
        state.owner[owner][fold.id] = true
        if fold.heading_start then
          local heading = fold.heading_start.block
          state.endpoint[heading] = state.endpoint[heading] or {}
          state.endpoint[heading][fold.id] = true
        end
        state.endpoint[fold["end"].block] = state.endpoint[fold["end"].block] or {}
        state.endpoint[fold["end"].block][fold.id] = true
        affected[fold.id] = true
      end
    end
    cooperative.checkpoint()
  end
  for id in pairs(affected) do
    if state.record[id] then add_boundary(session, state.record[id]) end
    cooperative.checkpoint()
  end
  state.changed = affected
  if not session.fragment then sessions[session.buffer] = session end
end

function M.display(entry)
  local state = entry.metadata.node
  if state and state ~= vim.NIL then return state end
  for _, range in ipairs(entry.metadata.fold or {}) do
    if range.start.row == 0 and range.closed then return heading_display end
  end
  return full_display
end

function M.sign()
  if vim.v.virtnum ~= 0 then return "  " end
  local window = tonumber(vim.g.statusline_winid) or vim.api.nvim_get_current_win()
  local session = sessions[vim.api.nvim_win_get_buf(window)]
  if not session or session.status ~= "Applied" or session.applying then return "  " end
  local location = require("forge.buffer").locate(session, vim.v.lnum - 1, 0)
  local node = location and session.sequence.node[location.block]
  local status = node and node.entry.metadata.status
  if status and status ~= vim.NIL and location.position.row == status.row then return "%s" end
  local layout = node and location.position.row == 0 and node.entry.metadata.layout
  local marker = layout and layout.marker
  if not marker or marker == vim.NIL or layout.indent ~= 2 then return "  " end
  local icon = marker.text
  if icon == "▸" then
    icon = M.display(node.entry).display == "heading" and "▸" or "▾"
  end
  return "%#" .. marker.capture .. "#" .. icon .. " %*"
end


function M.register(session) sessions[session.buffer] = session end

function M.closed(session, id)
  local choice = session.projection and session.projection.choice[id]
  if choice ~= nil then return choice end
  local record = session.fold and session.fold.record[id]
  if not record then return false end
  return record.fold.closed
end

function M.at(session, row, include_body)
  local selected, extent
  for _, record in pairs(session.fold and session.fold.record or {}) do
    local body = include_body == true or type(include_body) == "function" and include_body(record.fold.id)
    local visible = not session.project_source or require("forge.node_projection").contains(
      session.projection, record.owner, record.fold.start.row)
    local _, start = session.sequence:position(record.owner)
    local _, finish = session.sequence:position(record.fold["end"].block)
    if visible and start and finish then
      start = start + record.fold.start.row
      finish = finish + record.fold["end"].position.row
      local heading = record.fold.heading_start
      local first = heading and select(2, session.sequence:position(heading.block)) + heading.position.row or start
      if session.physical_row then
        first = session.physical_row(first)
        start = session.physical_row(start)
        finish = session.physical_row(finish)
      end
      if row >= first and row <= (body and finish - 1 or start)
        and (not extent or finish - start < extent) then
        selected, extent = record, finish - start
      end
    end
  end
  return selected
end

function M.set_open(session, window, id, opening)
  if session.status ~= "Applied" or session.applying or session.update_pending then return false end
  if session.node_action then return session.node_action(id, opening, window) end
  return require("forge.buffer").set_expansion(session, id, opening)
end

function M.toggle_heading(session, window, options)
  if session.status ~= "Applied" or session.applying or session.update_pending then return false end
  local cursor = vim.api.nvim_win_get_cursor(window)
  local location = require("forge.buffer").locate(session, cursor[1] - 1, cursor[2])
  local entry = location and session.block[location.block]
  if options and options.on_projected and entry and #(entry.metadata.collapse or {}) > 0 then
    options.on_projected()
    return true
  end
  local record = M.at(session, cursor[1] - 1, options and options.include_body)
  if not record then return false end
  local opening = M.closed(session, record.fold.id)
  if options and options.before_toggle and options.before_toggle(record.fold.id, opening) == false then return true end
  local result = M.set_open(session, window, record.fold.id, opening)
  if result and options and options.on_toggled then options.on_toggled(record.fold.id, not opening) end
  return result
end

function M.collapse_parent(session, window)
  local record = M.at(session, vim.api.nvim_win_get_cursor(window)[1] - 1, true)
  if not record then return false end
  return M.set_open(session, window, record.fold.id, false)
end

function M.attach(session, window)
  M.register(session)
  if window_state[window] and window_state[window].session == session then return end
  M.release(window)
  local saved = { session = session }
  for _, name in ipairs({ "foldmethod", "foldenable", "foldcolumn" }) do saved[name] = vim.wo[window][name] end
  window_state[window] = saved
  vim.wo[window].foldenable = false
  vim.wo[window].foldmethod = "manual"
  vim.wo[window].foldcolumn = "0"
end

function M.release(window)
  local saved = window_state[window]
  if not saved then return end
  window_state[window] = nil
  if vim.api.nvim_win_is_valid(window) then
    for _, name in ipairs({ "foldmethod", "foldenable", "foldcolumn" }) do vim.wo[window][name] = saved[name] end
  end
end

function M.restore_inherited(origin, target)
  local saved = window_state[origin]
  if not saved then return end
  if vim.api.nvim_win_is_valid(target) then
    for _, name in ipairs({ "foldmethod", "foldenable", "foldcolumn" }) do vim.wo[target][name] = saved[name] end
  end
  if origin == target then M.release(origin) end
end

function M.detach(session)
  sessions[session.buffer] = nil
  for window, saved in pairs(window_state) do if saved.session == session then M.release(window) end end
end

return M
