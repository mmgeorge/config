local M = {}
local sessions = setmetatable({}, { __mode = "v" })
local window_state = {}
local cooperative = require("forge.cooperative")

---@alias ForgeFoldPreference 'auto'|'open'|'closed'
---@class ForgeFoldView
---@field applied boolean
---@field preference ForgeFoldPreference
---@field native? boolean

---@param saved table
---@param state table<string, boolean>
local function retain_preferences(saved, state)
  for id, closed in pairs(state) do
    local view = saved.fold[id]
    if view and not view.controlled and closed ~= view.applied then
      view.preference = closed and "closed" or "open"
      view.applied = closed
    end
  end
end

local function fold_start(session, record)
  local _, start = session.sequence:position(record.owner)
  return require("forge.buffer").physical_row(session, start + record.fold.start.row) + 1
end

local function fold_end(session, record)
  local _, finish = session.sequence:position(record.fold["end"].block)
  return require("forge.buffer").physical_row(session, finish + record.fold["end"].position.row
    + (record.fold["end"].position.column > 0 and 1 or 0))
end

local function close_open_fold(row)
  -- foldclosed() also returns -1 when the row has no native fold.
  if vim.fn.foldlevel(row) > 0 and vim.fn.foldclosed(row) < 0 then
    vim.cmd(tostring(row) .. "foldclose")
  end
end

local function capture_window(session, window, selected)
    local state = {}
    local record_list = {}
    local records = session.fold and session.fold.record or {}
    for id in pairs(selected or records) do
      local record = records[id]
      if record then
        local finish = fold_end(session, record)
        record_list[#record_list + 1] = { id = id, row = fold_start(session, record), finish = finish }
      end
      cooperative.checkpoint()
    end
    table.sort(record_list, function(left, right)
      if left.row == right.row then return left.finish > right.finish end
      return left.row < right.row
    end)
    for _, record in ipairs(record_list) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return state end
      vim.api.nvim_win_call(window, function()
      local view, opened = vim.fn.winsaveview(), {}
      local closed = vim.fn.foldclosed(record.row)
      while closed >= 0 and (closed < record.row or vim.fn.foldclosedend(record.row) > record.finish) do
        opened[#opened + 1] = closed
        vim.cmd(tostring(record.row) .. "foldopen")
        closed = vim.fn.foldclosed(record.row)
      end
      if vim.fn.foldlevel(record.row) > 0 then
        state[record.id] = closed == record.row and vim.fn.foldclosedend(record.row) == record.finish
      end
      for index = #opened, 1, -1 do
        vim.cmd(tostring(opened[index]) .. "foldclose")
      end
      vim.fn.winrestview(view)
      end)
      cooperative.checkpoint()
    end
    return state
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
  callback = function(event)
    local window = tonumber(event.match)
    M.release(window)
    for _, session in pairs(sessions) do
      if session.fold_view then session.fold_view[window] = nil end
    end
  end,
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
  for id in pairs(prepared.native_fold_changed or {}) do affected[id] = true end
  state.changed = affected
  if not session.fragment then sessions[session.buffer] = session end
end

local function apply_defaults(session, window, saved, changed)
  local opened, closed = {}, {}
  local created = {}
  for id in pairs(changed) do
    local record = session.fold.record[id]
    if not record then
      saved.fold[id] = nil
    else
      local _, start = session.sequence:position(record.owner)
      local _, finish = session.sequence:position(record.fold["end"].block)
      start = start + record.fold.start.row
      finish = finish + record.fold["end"].position.row + (record.fold["end"].position.column > 0 and 1 or 0)
      if finish > start then
        local view = saved.fold[id] or { preference = "auto" }
        local source = session.block[record.owner]
        local node = source and source.metadata.node
        view.controlled = node and node ~= vim.NIL and node.id == id or false
        local target
        if view.controlled then target = node.display == "heading"
        else target = view.preference == "closed" or view.preference == "auto" and record.fold.closed end
        if not view.native then created[#created + 1] = { id = id, start = start + 1, finish = finish } end
        if view.applied == nil or view.applied ~= target or changed[id] then
          if target then closed[start + 1] = true else opened[start + 1] = true end
        end
        view.applied = target
        saved.fold[id] = view
      end
    end
    cooperative.checkpoint()
  end
  local opening = vim.tbl_keys(opened)
  table.sort(opening)
  table.sort(created, function(left, right)
    if left.start == right.start then return left.finish > right.finish end
    return left.start < right.start
  end)
  for _, range in ipairs(created) do
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return end
    vim.api.nvim_win_call(window, function()
      local view = vim.fn.winsaveview()
      -- A closed ancestor expands an Ex range to its entire fold.
      local ancestor = vim.fn.foldclosed(range.start)
      for _ = 1, vim.fn.foldlevel(range.start) do
        if ancestor < 0 then break end
        if not opened[ancestor] then closed[ancestor] = true end
        vim.cmd(tostring(range.start) .. "foldopen")
        ancestor = vim.fn.foldclosed(range.start)
      end
      vim.cmd(("%d,%dfold"):format(range.start, range.finish))
      saved.fold[range.id].native = true
      vim.cmd(tostring(range.start) .. "foldopen!")
      vim.fn.winrestview(view)
    end)
    cooperative.checkpoint()
  end
  for _, row in ipairs(opening) do
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return end
    vim.api.nvim_win_call(window, function()
      local view = vim.fn.winsaveview()
      local ancestor = vim.fn.foldclosed(row)
      for _ = 1, vim.fn.foldlevel(row) do
        if ancestor < 0 then break end
        if ancestor ~= row and not opened[ancestor] then closed[ancestor] = true end
        vim.cmd(tostring(row) .. "foldopen")
        local next_ancestor = vim.fn.foldclosed(row)
        ancestor = next_ancestor
      end
      vim.fn.winrestview(view)
    end)
    cooperative.checkpoint()
  end
  local closing = vim.tbl_keys(closed)
  table.sort(closing, function(left, right) return left > right end)
  for _, row in ipairs(closing) do
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return end
    vim.api.nvim_win_call(window, function()
      local view = vim.fn.winsaveview()
      close_open_fold(row)
      vim.fn.winrestview(view)
    end)
    cooperative.checkpoint()
  end
end

function M.reset(session)
  M.capture(session)
  for window, saved in pairs(window_state) do
    if saved.session == session and vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == session.buffer then
      vim.api.nvim_win_call(window, function() vim.cmd("silent! normal! zE") end)
      for _, fold in pairs(saved.fold) do fold.native = false end
    end
  end
end

---@param session table
---@param prepared table
local function remove_native(session, affected)
  for window, saved in pairs(window_state) do
    if saved.session == session and vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == session.buffer and next(affected) then
      retain_preferences(saved, capture_window(session, window, affected))
      for id in pairs(affected) do if saved.fold[id] then saved.fold[id].native = false end end
      local removed = {}
      for id in pairs(affected) do
        local record = session.fold.record[id]
        removed[#removed + 1] = { start = fold_start(session, record), finish = fold_end(session, record) }
      end
      table.sort(removed, function(left, right)
        if left.start == right.start then return left.finish > right.finish end
        return left.start < right.start
      end)
      local removed_end = -1
      for _, range in ipairs(removed) do
        if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then break end
        vim.api.nvim_win_call(window, function()
          local view = vim.fn.winsaveview()
          if range.finish > removed_end and range.finish >= range.start and vim.fn.foldlevel(range.start) > 0 then
            local minimum_lines = vim.wo.foldminlines
            vim.wo.foldminlines = 0
            local ancestors = {}
            local ok, failure = pcall(function()
              local closed = vim.fn.foldclosed(range.start)
              while closed >= 0 and (closed < range.start or vim.fn.foldclosedend(range.start) > range.finish) do
                ancestors[#ancestors + 1] = closed
                vim.cmd(range.start .. "foldopen")
                closed = vim.fn.foldclosed(range.start)
              end
              local depth = vim.fn.foldlevel(range.start)
              vim.cmd(range.start .. "foldopen!")
              -- Select the outermost removed fold before deleting its descendants.
              for _ = 1, depth do
                vim.cmd(range.start .. "foldclose")
                if vim.fn.foldclosed(range.start) < range.start
                  or vim.fn.foldclosedend(range.start) > range.finish then
                  vim.cmd(range.start .. "foldopen")
                  break
                end
              end
              assert(vim.fn.foldclosed(range.start) == range.start
                and vim.fn.foldclosedend(range.start) == range.finish,
                "native fold range differs from the removed subtree")
              vim.api.nvim_win_set_cursor(window, { range.start, 0 })
              vim.cmd("normal! zD")
            end)
            local restored, restore_failure = pcall(function()
              for index = #ancestors, 1, -1 do close_open_fold(ancestors[index]) end
            end)
            vim.wo.foldminlines = minimum_lines
            vim.fn.winrestview(view)
            if not ok then error(failure, 0) end
            if not restored then error(restore_failure, 0) end
            removed_end = range.finish
          end
          vim.fn.winrestview(view)
        end)
        cooperative.checkpoint()
      end
    end
  end
end

function M.prepare_records(session, selected)
  if not session.fold or not next(selected) then return {} end
  local affected = {}
  local ranges = {}
  for id in pairs(selected) do
    local record = session.fold.record[id]
    if record then
      affected[id] = true
      ranges[#ranges + 1] = { first = fold_start(session, record), last = fold_end(session, record) }
    end
  end
  for id, record in pairs(session.fold.record) do
    local first, last = fold_start(session, record), fold_end(session, record)
    for _, range in ipairs(ranges) do
      if first >= range.first and last <= range.last then affected[id] = true break end
    end
  end
  remove_native(session, affected)
  return affected
end

function M.prepare(session, prepared)
  if not session.fold then return end
  local affected = {}
  for _, changed in ipairs({ prepared.changed, prepared.retired }) do
    for owner in pairs(changed) do
      for id in pairs(session.fold.owner[owner] or {}) do
        local record = session.fold.record[id]
        local retained = false
        if prepared.retain_folds and not prepared.retired[owner] then
          for _, fold in ipairs(prepared.block[owner].metadata.fold or {}) do
            if fold.id == id and vim.deep_equal(record.fold, fold) then retained = true break end
          end
        end
        if not retained then affected[id] = true end
      end
      if not prepared.retain_folds then
        for id in pairs(session.fold.endpoint[owner] or {}) do affected[id] = true end
      end
    end
  end
  local root = vim.tbl_keys(affected)
  for _, id in ipairs(root) do
    local record = session.fold.record[id]
    local first = session.sequence:position(record.owner)
    local last = session.sequence:position(record.fold["end"].block)
    for index = first, last do
      for child in pairs(session.fold.owner[session.sequence:at(index).id] or {}) do affected[child] = true end
    end
  end
  prepared.native_fold_changed = affected
  remove_native(session, affected)
end

---@param session table
function M.register(session)
  sessions[session.buffer] = session
end

---@class ForgeFoldToggleOptions
---@field include_body? fun(id: string): boolean
---@field on_toggled? fun(id: string, closed: boolean)
---@field on_projected? fun()
---@field before_toggle? fun(id: string, opening: boolean): boolean

---Set a stable native fold without moving the cursor to its heading.
---@param session table
---@param window integer
---@param id string
---@param opening boolean
---@return boolean
function M.set_open(session, window, id, opening)
  if not session or session.status ~= "Applied" or session.update_pending or session.applying
    or not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return false end
  local record = session.fold and session.fold.record[id]
  if not record then return false end
  local start = fold_start(session, record)
  vim.api.nvim_win_call(window, function()
    local cursor = vim.api.nvim_win_get_cursor(window)
    local closed = vim.fn.foldclosed(start) == start
    if opening == closed then
      vim.cmd(tostring(start) .. (opening and (record.fold.expand_children and "foldopen!" or "foldopen") or "foldclose"))
    end
    if not opening and cursor[1] > start then
      local text = vim.api.nvim_buf_get_lines(session.buffer, start - 1, start, false)[1] or ""
      vim.api.nvim_win_set_cursor(window, { start, math.min(cursor[2], math.max(0, #text - 1)) })
    end
    if opening and record.fold.collapse_children then
      local finish, children = fold_end(session, record), {}
      for _, child in pairs(session.fold.record) do
        local child_start = fold_start(session, child)
        if child_start > start and fold_end(session, child) <= finish then children[#children + 1] = child_start end
      end
      table.sort(children, function(left, right) return left > right end)
      for _, child_start in ipairs(children) do close_open_fold(child_start) end
    end
  end)
  M.capture(session, window)
  return true
end

---@param session table
---@param window integer
---@param options? ForgeFoldToggleOptions
---@return boolean
function M.toggle_heading(session, window, options)
  if not session or session.status ~= "Applied" or session.update_pending or session.applying or not vim.api.nvim_win_is_valid(window)
    or vim.api.nvim_win_get_buf(window) ~= session.buffer then return false end
  local row = vim.api.nvim_win_get_cursor(window)[1]
  if options and options.on_projected then
    local located = require("forge.buffer").locate(session, row - 1, vim.api.nvim_win_get_cursor(window)[2])
    local index = located and session.sequence:position(located.block)
    local node = index and session.sequence:at(index)
    if node and node.entry.metadata.collapse and #node.entry.metadata.collapse > 0
      and vim.api.nvim_win_call(window, function() return vim.fn.foldclosed(row) == -1 end) then
      options.on_projected()
      return true
    end
  end
  local selected, selected_start, selected_finish
  for id, record in pairs(session.fold and session.fold.record or {}) do
    local start = fold_start(session, record)
    local finish = fold_end(session, record)
    local heading = record.fold.heading_start
    local first = heading and (require("forge.buffer").physical_row(session,
      select(2, session.sequence:position(heading.block)) + heading.position.row) + 1) or start
    local last = options and options.include_body and options.include_body(id) and finish or start
    if row >= first and row <= last and (not selected or finish - start < selected_finish - selected_start) then
      selected, selected_start, selected_finish = record, start, finish
    end
  end
  if selected then
    local record, start = selected, selected_start
    local opening = vim.api.nvim_win_call(window, function() return vim.fn.foldclosed(start) == start end)
    if options and options.before_toggle and options.before_toggle(record.fold.id, opening) == false then return true end
    M.set_open(session, window, record.fold.id, opening)
    if options and options.on_toggled then options.on_toggled(record.fold.id, not opening) end
    return true
  end
  return false
end

---@param session table
---@param selected_window? integer
---@return table<integer, table<string, boolean>>
function M.capture(session, selected_window)
  local captured = {}
  if session.status ~= "Applied" then return captured end
  for window, saved in pairs(window_state) do
    if (not selected_window or window == selected_window) and saved.session == session and vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == session.buffer then
      captured[window] = capture_window(session, window)
      retain_preferences(saved, captured[window])
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
          local saved = window_state[window]
          local view = saved and saved.fold[id]
          if view then
            if view.controlled then was_closed = record.fold.closed
            else was_closed = view.preference == "closed"
              or view.preference == "auto" and record.fold.closed end
            view.applied = was_closed
          end
          local row = fold_start(session, record)
          local target = was_closed and closed or opened
          target[#target + 1] = row
        end
      end
      table.sort(opened)
      table.sort(closed, function(left, right) return left > right end)
      vim.api.nvim_win_call(window, function()
        local view = vim.fn.winsaveview()
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
  session.fold.changed = {}
end

function M.sign()
  if vim.v.virtnum ~= 0 then return "  " end
  local window = tonumber(vim.g.statusline_winid) or vim.api.nvim_get_current_win()
  local session = sessions[vim.api.nvim_win_get_buf(window)]
  if not session or session.status ~= "Applied" or session.applying then return "  " end
  local location = require("forge.buffer").locate(session, vim.v.lnum - 1, 0)
  local node = location and session.sequence.node[location.block]
  local layout = node and location.position.row == 0 and node.entry.metadata.layout
  local marker = layout and layout.marker
  if not marker or marker == vim.NIL or layout.indent ~= 2 then return "  " end
  local icon = marker.text
  if icon == "▸" then
    local state = node.entry.metadata.node
    icon = state and (state.display == "heading" and "▸" or "▾")
      or "%{foldlevel(v:lnum) == 0 ? ' ' : (foldclosed(v:lnum) == v:lnum ? '▸' : '▾')}"
  end
  return "%#" .. marker.capture .. "#" .. icon .. " %*"
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
  local gutter = {}
  if node.entry.metadata.layout then
    gutter[0] = require("forge.content_layout").prefix(node.entry.metadata.layout, relative == 0)
  end
  for _, item in ipairs(node.entry.metadata.gutter or {}) do
    if item.placement ~= "sign" and item.position.row == relative then
      local column = math.min(item.position.column, #text)
      boundary[column] = true
      gutter[column] = gutter[column] or {}
      for _, chunk in ipairs(item.chunk) do
        gutter[column][#gutter[column] + 1] = { chunk.text, chunk.capture }
      end
    end
  end
  local decoration = vim.list_extend({}, node.entry.metadata.decoration or {})
  vim.list_extend(decoration, node.entry.metadata.visible_decoration or {})
  local background, background_priority = nil, -1
  for order, span in ipairs(decoration) do
    if span.range.start.row <= relative and (span.range["end"].row > relative
      or span.range["end"].row == relative and span.range["end"].column > 0) then
      local first = span.range.start.row == relative and span.range.start.column or 0
      local last = span.range["end"].row == relative and span.range["end"].column or #text
      first, last = math.min(first, #text), math.min(last, #text)
      boundary[first], boundary[last] = true, true
      spans[#spans + 1] = { first = first, last = last, capture = span.capture, priority = span.priority, order = order }
      if require("forge.decorations").full_width(span.capture) and span.priority >= background_priority then
        background, background_priority = span.capture, span.priority
      end
    end
  end
  table.sort(spans, function(left, right)
    if left.priority == right.priority then return left.order < right.order end
    return left.priority < right.priority
  end)
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
    vim.list_extend(chunks, gutter[first] or {})
    local capture = {}
    for _, span in ipairs(spans) do
      if span.first <= first and span.last >= last then
        capture[#capture + 1] = span.capture
      end
    end
    if #capture == 0 then capture[1] = background or "Normal" end
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
  vim.list_extend(chunks, gutter[#text] or {})
  for _, record in pairs(session.fold and session.fold.record or {}) do
    if fold_start(session, record) == vim.v.foldstart
      and fold_end(session, record) == vim.v.foldend then
      local loading = session.fold_loading and session.fold_loading[vim.api.nvim_get_current_win()]
      local suffix = loading and loading[record.fold.id] and " Loading…" or record.fold.collapsed_suffix
      if suffix then chunks[#chunks + 1] = { suffix, "Comment" } end
      break
    end
  end
  if background then
    local width = 0
    for _, chunk in ipairs(chunks) do
      local capture = type(chunk[2]) == "table" and chunk[2] or { chunk[2] }
      local layered = { background }
      for _, group in ipairs(capture) do
        if group ~= "Normal" and group ~= background then layered[#layered + 1] = group end
      end
      chunk[2] = layered
      width = width + vim.fn.strdisplaywidth(chunk[1], width)
    end
    local window = vim.api.nvim_get_current_win()
    local available = vim.api.nvim_win_get_width(window) - vim.fn.getwininfo(window)[1].textoff
    if width < available then chunks[#chunks + 1] = { string.rep(" ", available - width), background } end
  end
  return chunks
end

---@param session table
---@param window integer
function M.attach(session, window)
  assert(vim.api.nvim_win_get_buf(window) == session.buffer, "fold window has another document")
  if window_state[window] and window_state[window].session == session then return end
  M.release(window)
  local retained = session.fold_view and session.fold_view[window] or session.last_fold_view
  local saved = { session = session, fold = retained and vim.deepcopy(retained.fold) or {} }
  for id in pairs(saved.fold) do
    if not session.fold or not session.fold.record[id] then saved.fold[id] = nil end
  end
  for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars", "winhighlight" }) do saved[name] = vim.wo[window][name] end
  window_state[window] = saved
  vim.wo[window].foldmethod = "manual"
  vim.wo[window].foldexpr = "0"
  vim.wo[window].foldenable = true
  vim.wo[window].foldlevel = 99
  vim.wo[window].foldtext = "v:lua.require'forge.folds'.text()"
  local fillchars = saved.fillchars:gsub("^fold:[^,]*,?", ""):gsub(",fold:[^,]*", "")
  vim.wo[window].fillchars = fillchars .. (fillchars == "" and "" or ",") .. "fold: "
  vim.wo[window].winhighlight = option_with_pair(saved.winhighlight, "Folded", "Normal")
  vim.api.nvim_win_call(window, function() vim.cmd("silent! normal! zE") end)
  for _, fold in pairs(saved.fold) do fold.native = false end
  if session.fold then apply_defaults(session, window, saved, session.fold.record) end
end

function M.release(window)
  local saved = window_state[window]
  if not saved then return end
  local session = saved.session
  if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == session.buffer then
    local state = capture_window(session, window)
    retain_preferences(saved, state)
  end
  local retained = { fold = vim.deepcopy(saved.fold) }
  session.fold_view = session.fold_view or {}
  session.fold_view[window] = retained
  session.last_fold_view = retained
  window_state[window] = nil
  if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == session.buffer then
    for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars", "winhighlight" }) do
      vim.wo[window][name] = saved[name]
    end
  end
end

function M.restore_inherited(origin, target)
  local saved = window_state[origin]
  if not saved then return end
  if vim.wo[target].foldtext == "v:lua.require'forge.folds'.text()" then
    for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars" }) do
      vim.wo[target][name] = saved[name]
    end
  end
  if origin == target then M.release(origin) end
end

---@param session table
function M.detach(session)
  sessions[session.buffer] = nil
  for window, saved in pairs(window_state) do
    if saved.session == session then M.release(window) end
  end
  session.fold_view, session.last_fold_view = nil, nil
end

return M
