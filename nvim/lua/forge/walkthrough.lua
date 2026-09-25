local M = {}
local buffer = require("forge.buffer")
local input = require("forge.input")
local serial = 0

local function request(state, params, callback)
  state.queue[#state.queue + 1] = { params = params, callback = callback }
  local function pump()
    if state.pending or #state.queue == 0 then return end
    local item = table.remove(state.queue, 1)
    if not state.active and item.params.operation ~= "close" then pump() return end
    state.pending = true
    require("forge.client").request_host("walkthrough", item.params, function(result, failure)
      vim.schedule(function()
        state.pending = false
        if state.active or item.params.operation == "close" then item.callback(result, failure) end
        pump()
      end)
    end)
  end
  if #state.queue > 64 and params.operation ~= "close" then state.notice("Walkthrough request admission is full") table.remove(state.queue) return end
  pump()
end

local function patch(state, value, failure)
  if failure then state.notice(failure) return end
  if value and value ~= vim.NIL then buffer.apply_patch(state.replica, value) end
end

local function close_view(state, window)
  local view = state.view[window]
  if not view then return end
  if vim.api.nvim_win_is_valid(window) then
    vim.wo[window].wrap, vim.wo[window].linebreak = view.original_wrap, view.original_linebreak
  end
  input.close(view)
  state.view[window] = nil
  if state.active then request(state, { operation = "close_view", document = state.document, view = view.id }, function(value, failure) patch(state, value, failure) end) end
end

local function attach(state, window)
  local view = state.view[window]
  if not view then
    view = input.open(state.replica, window)
    view.original_wrap, view.original_linebreak = vim.wo[window].wrap, vim.wo[window].linebreak
    state.view[window] = view
  end
  vim.wo[window].wrap, vim.wo[window].linebreak = true, true
  request(state, { operation = "view", document = state.document, view = view.id, width = require("forge.width").capture(window) }, function(value, failure) patch(state, value, failure) end)
  return view
end

local function current(state, view, captured)
  if not state.active or not view.active or view.sequence ~= captured.sequence or state.replica.revision ~= captured.revision
    or not vim.api.nvim_win_is_valid(view.window) or vim.api.nvim_win_get_buf(view.window) ~= state.replica.buffer then return false end
  local cursor = vim.api.nvim_win_get_cursor(view.window)
  local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
  return location and location.block == captured.block and location.position.row == captured.position.row
    and location.position.column == captured.position.column and location.target == captured.target
end

function M.open_change(state)
  state.source = vim.tbl_filter(function(source) return source.active end, state.source)
  if #state.source >= 8 then state.notice("Close a Walkthrough source before opening another") return end
  if state.pending or #state.queue > 0 then state.notice("Walkthrough layout is updating") return end
  local window = vim.api.nvim_get_current_win()
  local view = state.view[window]
  if not view then attach(state, window) return end
  local captured, failure = input.capture(state.replica, view, "open")
  if not captured or not captured.target then state.notice(failure or "This row has no source annotation") return end
  serial = serial + 1
  local annotation_id = state.document .. ":annotation:" .. serial
  state.opening = true
  local function settled_source()
    if not state.opening then return end
    state.opening = false
    vim.schedule(function()
      if state.active then
        for _, attached in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do attach(state, attached) end
      end
    end)
  end
  vim.cmd("vsplit")
  local source_window = vim.api.nvim_get_current_win()
  vim.api.nvim_set_current_win(window)
  local owner = { active = true, review = true, window = source_window, annotation_view = {}, annotation_name = "ForgeWalkthroughReview:" .. serial }
  state.source[#state.source + 1] = owner
  owner.group = vim.api.nvim_create_augroup("ForgeWalkthroughSource" .. serial, { clear = true })
  function owner.close()
    if not owner.active then return end
    owner.active = false
    vim.api.nvim_del_augroup_by_id(owner.group)
    if owner.commands then owner.commands.close() end
    if owner.annotation_commands then owner.annotation_commands.close() end
    for _, annotation_view in pairs(owner.annotation_view) do input.close(annotation_view) end
    owner.annotation_view = {}
    for _, owned_window in ipairs({ owner.window, owner.annotation_window }) do
      if owned_window and vim.api.nvim_win_is_valid(owned_window) then
        local displayed = vim.api.nvim_win_get_buf(owned_window)
        if displayed == state.replica.buffer or (owner.annotation and displayed == owner.annotation.buffer)
          or (owner.source and displayed == owner.source.replica.buffer) then
          local normal_windows = 0
          for _, candidate in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_win_get_tabpage(owned_window))) do
            if vim.api.nvim_win_get_config(candidate).relative == "" then normal_windows = normal_windows + 1 end
          end
          if normal_windows > 1 then vim.api.nvim_win_close(owned_window, true) end
        end
      end
    end
    if owner.annotation then buffer.close(owner.annotation) end
    if state.active then
      request(state, { operation = "annotation_close", document = state.document, annotation = annotation_id }, function(_, failure)
        if failure then state.notice(failure) end
      end)
    end
    if owner.source then require("forge.source_document").close(owner.source) end
    settled_source()
    if state.active and vim.api.nvim_win_is_valid(window) then vim.api.nvim_set_current_win(window) end
  end
  vim.api.nvim_create_autocmd("WinClosed", { group = owner.group, callback = function(event)
    local closed = tonumber(event.match)
    if closed == owner.window or closed == owner.annotation_window then vim.schedule(owner.close) end
  end })
  function owner.resize_annotation()
    if not owner.active or not owner.annotation then return end
    for attached, annotation_view in pairs(owner.annotation_view) do
      if not vim.api.nvim_win_is_valid(attached) or vim.api.nvim_win_get_buf(attached) ~= owner.annotation.buffer then
        input.close(annotation_view)
        owner.annotation_view[attached] = nil
        request(state, { operation = "annotation_close_view", document = state.document, annotation = annotation_id, view = annotation_view.id },
          function(value, failure)
            if failure then state.notice(failure)
            elseif owner.active and value and value ~= vim.NIL then buffer.apply_patch(owner.annotation, value) end
          end)
      end
    end
    for _, attached in ipairs(vim.fn.win_findbuf(owner.annotation.buffer)) do
      local annotation_view = owner.annotation_view[attached]
      if not annotation_view then annotation_view = input.open(owner.annotation, attached) owner.annotation_view[attached] = annotation_view end
      vim.wo[attached].wrap, vim.wo[attached].linebreak = true, true
      local width = require("forge.width").capture(attached)
      if not vim.deep_equal(annotation_view.width, width) then
        annotation_view.width = width
        request(state, { operation = "annotation_view", document = state.document, annotation = annotation_id, view = annotation_view.id, width = width },
          function(value, failure)
            if failure then annotation_view.width = nil state.notice(failure)
            elseif owner.active and value and value ~= vim.NIL then buffer.apply_patch(owner.annotation, value) end
          end)
      end
    end
  end
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized", "WinEnter" }, { group = owner.group, callback = owner.resize_annotation })
  function owner.show_review()
    if not owner.active or not owner.annotation or not vim.api.nvim_win_is_valid(owner.window) then return end
    local attached = vim.api.nvim_get_current_win()
    if not owner.source or vim.api.nvim_win_get_buf(attached) ~= owner.source.replica.buffer then attached = owner.window end
    vim.api.nvim_win_set_buf(attached, owner.annotation.buffer)
    owner.resize_annotation()
  end
  function owner.open_source()
    local attached = vim.api.nvim_get_current_win()
    local annotation_view = owner.annotation_view[attached]
    if not annotation_view or not owner.source or not owner.source.active then return end
    local captured_source, capture_failure = input.capture(owner.annotation, annotation_view, "open")
    if not captured_source or not captured_source.target then state.notice(capture_failure or "This row has no captured source coordinate") return end
    request(state, { operation = "annotation_source", document = state.document, input = captured_source }, function(result, failure)
      if failure then state.notice(failure) return end
      if not result or not owner.active or not owner.source.active
        or not current({ active = owner.active, replica = owner.annotation }, annotation_view, captured_source) then return end
      owner.source.window, owner.source.requested_row = attached, result.source_row + 1
      vim.api.nvim_win_set_buf(attached, owner.source.replica.buffer)
      require("forge.source_document").demand(owner.source)
    end)
  end
  owner.source = require("forge.source_document").open_walkthrough({ workspace = state.workspace, window = source_window,
    input = captured, annotation_document = annotation_id, review = true,
    is_current = function()
      local valid = owner.active and current(state, view, captured)
      if not valid then owner.close() end
      return valid
    end,
    on_error = function(message) owner.close() state.notice(message) end,
    on_ready = function(source)
      owner.source = source
      owner.commands = require("forge.document_commands").attach(source.replica, { view = "diff", title = "Walkthrough source", winbar = false,
        handler = { close = owner.show_review } })
      vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = source.replica.buffer,
        callback = function() vim.schedule(owner.close) end })
      settled_source()
      owner.show_review()
      vim.api.nvim_set_current_win(owner.window)
    end,
    on_annotation = function(snapshot, _, review)
      if not owner.active or not current(state, view, captured) then return end
      if review ~= true then owner.close() state.notice("Missing Walkthrough review projection") return end
      owner.annotation = buffer.open(annotation_id, { notice = state.notice, recover = function()
        request(state, { operation = "annotation_snapshot", document = state.document, annotation = annotation_id }, function(snapshot, failure)
          if failure then state.notice(failure)
          elseif owner.active and snapshot then buffer.apply_snapshot(owner.annotation, snapshot) end
        end)
      end })
      buffer.apply_snapshot(owner.annotation, snapshot)
      vim.bo[owner.annotation.buffer].buftype = "nowrite"
      vim.bo[owner.annotation.buffer].readonly = true
      vim.api.nvim_buf_set_name(owner.annotation.buffer, owner.annotation_name)
      owner.annotation_commands = require("forge.document_commands").attach(owner.annotation, { view = "diff", title = "Walkthrough review",
        handler = { close = owner.close, open = owner.open_source } })
      vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = owner.annotation.buffer,
        callback = function() vim.schedule(owner.close) end })
      vim.api.nvim_create_autocmd({ "BufWinEnter", "BufWinLeave" }, { group = owner.group, buffer = owner.annotation.buffer,
        callback = function() vim.schedule(owner.resize_annotation) end })
      owner.annotation_window = source_window
    end })
end

function M.open(options)
  options = options or {}
  serial = serial + 1
  local window = options.window or vim.api.nvim_get_current_win()
  local state = { document = "walkthrough:" .. vim.uv.hrtime() .. ":" .. serial, workspace = options.workspace or vim.fn.getcwd(),
    active = true, pending = false, queue = {}, view = {}, source = {}, window = window, origin = vim.api.nvim_win_get_buf(window) }
  state.notice = options.on_error or function(message) vim.notify(message, vim.log.levels.ERROR, { title = "Forge walkthrough" }) end
  state.replica = buffer.open(state.document, { notice = state.notice, recover = function()
    request(state, { operation = "snapshot", document = state.document }, function(snapshot, failure)
      if failure then state.notice(failure) elseif snapshot then buffer.apply_snapshot(state.replica, snapshot) end
    end)
  end })
  vim.bo[state.replica.buffer].buftype = "nowrite"
  vim.bo[state.replica.buffer].buflisted = true
  vim.bo[state.replica.buffer].bufhidden = "hide"
  vim.bo[state.replica.buffer].readonly = true
  vim.bo[state.replica.buffer].filetype = "ForgeWalkthrough"
  vim.api.nvim_buf_set_name(state.replica.buffer, "ForgeWalkthrough:" .. serial)
  state.group = vim.api.nvim_create_augroup("ForgeWalkthrough" .. serial, { clear = true })
  vim.api.nvim_create_autocmd("BufWipeout", { group = state.group, buffer = state.replica.buffer,
    callback = function() vim.schedule(function() M.close(state) end) end })
  vim.api.nvim_create_autocmd("BufWinLeave", { group = state.group, buffer = state.replica.buffer, callback = function() close_view(state, vim.api.nvim_get_current_win()) end })
  local function refresh_views()
    if state.active and not state.opening and state.replica.status == "Applied" then
      for _, attached in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do attach(state, attached) end
    end
  end
  vim.api.nvim_create_autocmd({ "BufWinEnter", "WinEnter" }, { group = state.group, buffer = state.replica.buffer, callback = refresh_views })
  vim.api.nvim_create_autocmd({ "VimResized", "WinResized" }, { group = state.group, callback = refresh_views })
  state.commands = require("forge.document_commands").attach(state.replica, { view = "diff", title = "Walkthrough",
    handler = { open = function() M.open_change(state) end, close = function() M.close(state) end } })
  request(state, { operation = "open", document = state.document, workspace = state.workspace,
    inventory = require("forge.infra.config").options.walkthrough_inventory ~= false }, function(opened, failure)
    if failure or not opened then state.notice(failure or "Missing Walkthrough document") M.close(state) return end
    buffer.apply_snapshot(state.replica, opened.snapshot)
    if opened.inventory_state == "unavailable" and opened.inventory_diagnostic then
      state.notice(opened.inventory_diagnostic)
    end
    if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == state.origin then
      vim.api.nvim_win_set_buf(window, state.replica.buffer)
      attach(state, window)
    end
  end)
  return state
end

function M.close(state)
  if not state or not state.active then return end
  state.active = false
  local attached = {}
  for window in pairs(state.view) do attached[#attached + 1] = window end
  for _, window in ipairs(attached) do close_view(state, window) end
  for _, source in ipairs(state.source) do source.close() end
  if state.commands then state.commands.close() end
  if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
  request(state, { operation = "close", document = state.document }, function() end)
  buffer.close(state.replica)
end

return M
