local M = {}
local buffer = require("forge.status_render")
local input = require("forge.input")
local next_document = 0
local runner_for_test
local host_unavailable_message = "Forge host stopped or restarted; refresh or reopen this document"

---@class ForgeNativeStatusOptions
---@field workspace? string
---@field window? integer
---@field started_at? integer Monotonic command-entry time in nanoseconds.
---@field keymaps? table<string, string|false>
---@field filetype? string
---@field name? string
---@field bind? boolean
---@field filename? string
---@field comparison? {reference: string, worktree: boolean, path?: string}
---@field handler? table<string, function>
---@field before_open? fun(start: fun())
---@field on_ready? fun(state: ForgeNativeStatus)
---@field open_commit? function
---@field open_pr? function
---@field open_about? function
---@field edit_issues? function

---@class ForgeStatusView: ForgeInputView

---@class ForgeStatusNavigation
---@field window integer
---@field forward boolean
---@field block string
---@field position {row: integer, column: integer}
---@field sequence? integer

---@class ForgeNativeStatus
---@field document string
---@field replica table
---@field view table<integer, ForgeStatusView>
---@field active boolean
---@field pending boolean
---@field scheduled boolean
---@field done table<integer, boolean>
---@field recovering? boolean
---@field body_started? table<integer, integer>
---@field width_owner? integer
---@field host_generation? integer
---@field group integer
---@field request_queue? function[]
---@field request_active? boolean
---@field opening_failed? boolean Initial request failed before a usable snapshot arrived.
---@field finish_open? fun(failure?: string) Completes startup timing on ready, failure, or close.
---@field fold_open? {window: table<integer, boolean>, command: string}
---@field navigation? ForgeStatusNavigation
---@field commands? {close: fun()}
---@field native_context? boolean
---@field context? table
---@field ready? boolean
---@field refresh_epoch? integer
---@field refresh_active? boolean
---@field refresh_recover? boolean
---@field refresh_callback? table<integer, fun(success: boolean, failure?: string)>
---@field unsubscribe_document? fun()
---@field applied_operation? table<integer, integer>

---@param message string
local function notice(message)
  vim.notify(message, vim.log.levels.ERROR, { title = "Forge status" })
end

local function host_current(state)
  if runner_for_test or not state.host_generation then return true end
  local client = require("forge.client")
  return state.host_generation == client.host_generation() and client.host_accepting()
end

---@param state ForgeNativeStatus
---@param params table
---@param callback fun(result: table?, failure: string?)
local function request(state, params, callback, method)
  local queued_at = vim.uv.hrtime()
  state.request_queue = state.request_queue or {}
  local function start()
    local request_started = vim.uv.hrtime()
    require("forge.startup_log").write("status.request", { document = state.document, method = method or "status", operation = params.operation,
      queue_wait_us = math.floor((request_started - queued_at) / 1000) })
    state.request_active = true
    local delivered = false
    local function receive(result, failure)
      if delivered then return end
      delivered = true
      local received_at = vim.uv.hrtime()
      require("forge.startup_log").write("status.response", { document = state.document, operation = params.operation,
        error = failure, elapsed_ms = math.floor((vim.uv.hrtime() - request_started) / 1e6) })
      if result and not failure and not runner_for_test and not state.host_generation then
        state.host_generation = require("forge.client").host_generation()
      end
      vim.schedule(function()
        local callback_started = vim.uv.hrtime()
        require("forge.startup_log").write("status.callback", { document = state.document, operation = params.operation,
          queue_wait_us = math.floor((vim.uv.hrtime() - received_at) / 1000) })
        if not host_current(state) then
          result = nil
          failure = params.operation ~= "close" and host_unavailable_message or nil
        end
        if state.active or params.operation == "close" then callback(result, failure) end
        state.request_active = false
        local next_request = table.remove(state.request_queue, 1)
        if next_request then next_request()
        elseif state.active then
          if state.navigation then M.navigate_step(state) else M.demand(state) end
        end
        if params.operation == "open" or params.operation == "comparison" or params.operation == "local" then
          require("forge.startup_log").write("status.callback.finished", { document = state.document, operation = params.operation,
            elapsed_us = math.floor((vim.uv.hrtime() - callback_started) / 1000) })
        end
      end)
    end
    if not host_current(state) then receive(nil) return end
    local ok, failure = pcall(function()
      if runner_for_test then runner_for_test(method or "status", params, receive)
      else require("forge.client").request_host(method or "status", params, receive, function(progress)
        require("forge.startup_log").write("status.native.timing", {
          document = state.document, operation = params.operation, timing = progress,
        })
      end) end
    end)
    if not ok then receive(nil, tostring(failure)) end
  end
  if state.request_active then
    if #state.request_queue >= 64 and params.operation ~= "close" then
      vim.schedule(function() if state.active then callback(nil, "Status request admission is full") end end)
    else state.request_queue[#state.request_queue + 1] = start end
  else start() end
end

---@param state ForgeNativeStatus
local function recover(state)
  if state.recovering then return end
  state.recovering = true
  request(state, { operation = "snapshot", document = state.document }, function(result, failure)
    if failure or not result then notice(failure or "Missing status snapshot") return end
    if buffer.apply_snapshot(state.replica, result).kind ~= "Applied" then return end
    state.recovering = nil
    state.done = {}
    M.demand(state)
  end)
end

local function close_view(state, window, view)
  input.close(view)
  state.view[window] = nil
  if state.width_owner == window then
    state.width_owner = next(state.view)
    if state.width_owner and vim.api.nvim_win_is_valid(state.width_owner) then buffer.resize(state.replica, state.width_owner) end
  end
  if state.active and host_current(state) then
    request(state, { operation = "close_view", document = state.document, view = view.id }, function(patch, failure)
      if failure then notice(failure) return end
      if patch and patch ~= vim.NIL then buffer.apply_patch(state.replica, patch) end
    end)
  end
end

---@param state ForgeNativeStatus
---@param window integer
---@return ForgeStatusView
local function view_for(state, window)
  local view = state.view[window]
  if not view or not view.active then
    local attached = input.open(state.replica, window)
    ---@cast attached ForgeStatusView
    view = attached
    state.view[window] = view
    if not state.width_owner then state.width_owner = window buffer.resize(state.replica, window) end
  end
  return view
end

---@param state ForgeNativeStatus
---@return ForgeStatusInput?
local function visible_demand(state)
  for window, view in pairs(state.view) do
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then
      close_view(state, window, view)
    end
  end
  for _, window in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do
    local view = view_for(state, window)
    local bounds = vim.api.nvim_win_call(window, function()
      return { vim.fn.line("w0") - 1, vim.fn.line("w$") - 1, vim.api.nvim_win_get_height(window) }
    end)
    local last = state.fold_open and state.replica.row_count - 1
      or math.min(state.replica.row_count - 1, bounds[2] + bounds[3])
    local row = bounds[1]
    while row <= last do
      local folded = vim.api.nvim_win_call(window, function() return vim.fn.foldclosedend(row + 1) end)
      local location = buffer.locate(state.replica, row, 0)
      local deferred_open = state.fold_open and location and location.block:match("^file:")
      local semantic = location and location.location
      if (folded < row + 1 or deferred_open) and semantic and (semantic.kind == "file" or semantic.kind == "body") then
        local file = semantic.kind == "file" and semantic.id or semantic.file
        if not state.done[file] then
          view.sequence = view.sequence + 1
          return { document = state.document, revision = state.replica.revision,
            view = view.id, sequence = view.sequence, action = "demand", location = { kind = "file", id = file } }
        end
      end
      row = not state.fold_open and folded >= row + 1 and folded or row + 1
    end
  end
end

---@param state ForgeNativeStatus
function M.demand(state)
  if not host_current(state) then return end
  if not state.active or state.navigation or state.request_active or state.pending or state.scheduled or state.replica.status ~= "Applied" then return end
  state.scheduled = true
  vim.schedule(function()
    state.scheduled = false
    if not host_current(state) then return end
    if not state.active or state.navigation or state.request_active or state.pending or state.replica.status ~= "Applied" then return end
    local demand = visible_demand(state)
    if not demand then
      local fold_open = state.fold_open
      state.fold_open = nil
      if fold_open then
        for window in pairs(fold_open.window) do
          if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == state.replica.buffer then
            vim.api.nvim_win_call(window, function() vim.cmd("normal! " .. fold_open.command) end)
          end
        end
      end
      return
    end
    if state.request_active then return end
    state.pending = true
    request(state, { operation = "demand", input = demand }, function(result, failure)
      state.pending = false
      if failure or not result then
        state.done[demand.location.id] = true
        notice(failure or "Missing status body delivery")
        return
      end
      local applied = buffer.apply_body(state.replica, result)
      if applied.kind == "Desynchronized" then return end
      if applied.kind == "Discarded" then M.demand(state) return end
      local started = state.body_started and state.body_started[result.file]
      if started and applied.kind == "Applied" then
        state.body_started[result.file] = nil
        require("forge.startup_log").watch_redraw(state.replica.buffer, state.document, started,
          "status.body.first_redraw", { file = result.file, generation = result.generation })
      end
      state.done[result.file] = result.more ~= true
      if result.state and result.state.state == "failed" then notice(result.state.diagnostic) end
      M.demand(state)
    end)
  end)
end

---@param state ForgeNativeStatus
function M.refresh(state, callback)
  if not state.active then if callback then callback(false, "Status document is closed") end return end
  state.refresh_epoch = (state.refresh_epoch or 0) + 1
  state.refresh_callback = state.refresh_callback or {}
  if callback and #state.refresh_callback >= 64 then
    callback(false, "Status refresh callback admission is full")
    return
  end
  if callback then state.refresh_callback[#state.refresh_callback + 1] = callback end
  if state.refresh_active then return end
  state.refresh_active = true
  state.navigation = nil
  local run
  run = function()
    local epoch = state.refresh_epoch
    local function finish(result, failure, snapshot)
      if epoch ~= state.refresh_epoch then
        state.refresh_recover = true
        run()
        return
      end
      state.refresh_active = false
      local callbacks = state.refresh_callback or {}
      state.refresh_callback = {}
      if failure then
        state.refresh_recover = true
        notice(failure)
      else
        if snapshot then
          buffer.apply_snapshot(state.replica, result)
          state.refresh_recover = false
        elseif result and result ~= vim.NIL then buffer.apply_patch(state.replica, result) end
        if snapshot or (result and result ~= vim.NIL) then state.done = {} end
        if state.context then state.context.refresh() end
        M.demand(state)
      end
      for _, complete in ipairs(callbacks) do complete(not failure, failure) end
    end
    request(state, { operation = "refresh", document = state.document }, function(result, failure)
      if epoch == state.refresh_epoch and not failure and state.refresh_recover then
        request(state, { operation = "snapshot", document = state.document }, function(snapshot, snapshot_failure)
          finish(snapshot, snapshot_failure or (not snapshot and "Missing status snapshot" or nil), true)
        end)
      else finish(result, failure, false) end
    end)
  end
  run()
end

---@param state ForgeNativeStatus
---@param forward boolean
function M.navigate(state, forward)
  if not host_current(state) then notice(host_unavailable_message) return end
  local window = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(window)
  local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
  if not location then return end
  state.navigation = { window = window, forward = forward, block = location.block, position = location.position }
  M.navigate_step(state)
end

function M.navigate_step(state)
  local navigation = state.navigation
  if not navigation or not state.active or state.request_active then return end
  local window = navigation.window
  if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then
    state.navigation = nil
    return
  end
  local view = view_for(state, window)
  local cursor = vim.api.nvim_win_get_cursor(window)
  local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
  if not location or location.block ~= navigation.block or location.position.row ~= navigation.position.row
      or location.position.column ~= navigation.position.column or (navigation.sequence and view.sequence ~= navigation.sequence) then
    state.navigation = nil
    M.demand(state)
    return
  end
  if state.request_active then return end
  local captured, failure = input.capture(state.replica, view, "navigate")
  if not captured then state.navigation = nil notice(failure or "Cannot capture hunk navigation") return end
  navigation.sequence = captured.sequence
  request(state, { input = captured, forward = navigation.forward }, function(result, request_failure)
    if result and result.body and result.body ~= vim.NIL then buffer.apply_body(state.replica, result.body) end
    if state.navigation ~= navigation then return end
    if request_failure or not result then
      state.navigation = nil
      notice(request_failure or "Missing hunk navigation response")
      return
    end
    if result.effect and result.effect ~= vim.NIL then
      local current = vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_cursor(window)
      local position = current and buffer.locate(state.replica, current[1] - 1, current[2])
      if position and position.block == navigation.block and position.position.row == navigation.position.row
          and position.position.column == navigation.position.column then
        buffer.apply_effect(state.replica, view, result.effect)
      end
    end
    if not result.more then state.navigation = nil end
  end, "status.navigate")
end

function M.action(state, action, visual)
  local action_started = vim.uv.hrtime()
  if not host_current(state) then notice(host_unavailable_message) return end
  state.navigation = nil
  local window = vim.api.nvim_get_current_win()
  local view = view_for(state, window)
  local selection
  if visual then
    local anchor = vim.fn.getpos("v")[2] - 1
    local cursor = vim.api.nvim_win_get_cursor(window)[1] - 1
    local ok, result = pcall(buffer.selection, state.replica, math.min(anchor, cursor), math.max(anchor, cursor))
    if not ok then notice(result) return end
    selection = result
  end
  local captured, failure = buffer.capture(state.replica, view, action, selection)
  if not captured then notice(failure or "Cannot capture status input") return end
  if action == "open" and state.context and captured.location.kind == "context" then
    state.context.activate("status:context:" .. captured.location.role)
    return
  end
  if action == "open" then
    request(state, { operation = "open_target", input = captured }, function(effect, request_failure)
      if request_failure or not effect then notice(request_failure or "Missing file target") return end
      require("forge.effects").apply(state.replica, view, effect)
    end)
    return
  end
  if visual then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  end
  local function submit()
    request(state, { operation = "input", input = captured, selection = selection }, function(result, request_failure)
      if request_failure or not result then notice(request_failure or "Missing Git write outcome") return end
      if result.update then
        M.apply_update(state, result.update)
        local visible = state.applied_operation and state.applied_operation[result.operation_id] or vim.uv.hrtime()
        require("forge.startup_log").write("status.action.visible", { operation = result.operation_id, action = action,
          elapsed_us = math.floor((visible - action_started) / 1000), native_us = result.update.elapsed_us })
        if state.applied_operation then state.applied_operation[result.operation_id] = nil end
        return
      end
      if not result.success then
        for _, target in ipairs(result.target or {}) do
          if target.diagnostic then notice(target.diagnostic) end
        end
      end
      if result.settlement_diagnostic then notice(result.settlement_diagnostic) end
      if result.patch and result.patch ~= vim.NIL then
        buffer.apply_patch(state.replica, result.patch)
        state.done = {}
        M.demand(state)
      else M.refresh(state) end
      if result.operation_id then
        require("forge.client").request_host("repository.write", { operation = "acknowledge", operation_id = result.operation_id }, function(_, ack_failure)
          if ack_failure then notice(ack_failure) end
        end)
      end
    end)
  end
  if action == "discard" then
    local replica, generation = state.replica, state.host_generation
    local message = require("forge.views.status.dialogs").discard_message(replica, captured, selection)
    if not message then return end
    require("forge.infra.confirm").open(message, function()
      if not host_current(state) or state.host_generation ~= generation then notice(host_unavailable_message) return end
      if not state.active or state.replica ~= replica or not view.active or state.view[window] ~= view then
        notice("input view is no longer current")
        return
      end
      if view.sequence >= 9007199254740991 then notice("input sequence exhausted") return end
      view.sequence = view.sequence + 1
      captured.sequence = view.sequence
      submit()
    end)
  else submit() end
end

---@param state ForgeNativeStatus
---@param update table
function M.apply_update(state, update)
  if not state.active or not host_current(state) then return end
  if update.resync then
    if update.phase == "accepted" then
      for _, diagnostic in ipairs(update.diagnostic or {}) do notice(diagnostic) end
    end
    recover(state)
    return
  end
  local result = buffer.apply_update(state.replica, update)
  if result.kind == "Desynchronized" then recover(state) return end
  if result.kind ~= "Applied" then return end
  state.applied_operation = state.applied_operation or {}
  if update.phase == "accepted" then state.applied_operation[update.operation_id] = vim.uv.hrtime() end
  state.done = {}
  for _, delivery in ipairs(update.body or {}) do state.done[delivery.file] = delivery.more ~= true end
  M.demand(state)
end

---@param options? ForgeNativeStatusOptions
---@return ForgeNativeStatus
function M.open(options)
  options = options or {}
  local open_started = options.started_at or vim.uv.hrtime()
  local opening_window = options.window or vim.api.nvim_get_current_win()
  local opening_buffer = vim.api.nvim_win_get_buf(opening_window)
  local opening_view = vim.api.nvim_win_call(opening_window, vim.fn.winsaveview)
  next_document = next_document + 1
  local document = "status:" .. tostring(vim.uv.hrtime()) .. ":" .. next_document
  ---@type ForgeNativeStatus
  local state = { document = document, replica = {}, view = {}, active = true,
    pending = false, scheduled = false, done = {}, group = 0 }
  state.finish_open = require("forge.startup_log").span("status.open", { document = document,
    workspace = options.workspace or vim.fn.getcwd(), profile = vim.g.forge_build_profile or "release",
    command_started_us = math.floor(open_started / 1000),
    entry_to_open_us = math.floor((vim.uv.hrtime() - open_started) / 1000) })
  state.replica = buffer.open(document, { filetype = options.filetype or "forge", notice = notice, recover = function() recover(state) end })
  state.replica.width = math.max(1, vim.api.nvim_win_get_width(opening_window))
  state.replica.recover_body = function(file, generation)
    request(state, { operation = "body_snapshot", document = document, file = file, generation = generation }, function(result, failure)
      if failure or not result then notice(failure or "Missing status body snapshot") return end
      local applied = buffer.apply_body(state.replica, result)
      if applied.kind ~= "Applied" then return end
      local model = state.replica.file[file]
      if model and model.record.generation == generation then model.recovering = nil end
      state.done[file] = result.more ~= true
      M.demand(state)
    end)
  end
  require("forge.startup_log").write("status.buffer.opened", { document = document,
    elapsed_ms = math.floor((vim.uv.hrtime() - open_started) / 1e6) })
  vim.b[state.replica.buffer].forge_native_document = true
  local loading_namespace = vim.api.nvim_create_namespace("ForgeStatusLoading")
  vim.api.nvim_buf_set_extmark(state.replica.buffer, loading_namespace, 0, 0, {
    virt_text = { { "Loading Forge status…", "Comment" } },
    virt_text_pos = "overlay",
  })
  local name = options.name or ("ForgeStatus://" .. document)
  if not pcall(vim.api.nvim_buf_set_name, state.replica.buffer, name) then
    vim.api.nvim_buf_set_name(state.replica.buffer, name .. "#" .. state.replica.buffer)
  end
  if options.bind ~= false then
    vim.api.nvim_win_set_buf(options.window or vim.api.nvim_get_current_win(), state.replica.buffer)
    require("forge.startup_log").write("status.loading.presented", { document = document,
      elapsed_ms = math.floor((vim.uv.hrtime() - open_started) / 1e6) })
  end
  state.group = vim.api.nvim_create_augroup("ForgeNativeStatus" .. next_document, { clear = true })
  if not runner_for_test then
    state.unsubscribe_document = require("forge.client").subscribe_document(document, function(event, update, generation)
      vim.schedule(function()
        if not state.active or (state.host_generation and generation ~= state.host_generation) then return end
        if event == "status.resync" then recover(state)
        elseif event == "status.update" then M.apply_update(state, update) end
      end)
    end)
  end
  local refresh_root = vim.fs.normalize(options.workspace or (options.filename and vim.fs.dirname(options.filename)) or vim.fn.getcwd())
  if vim.fn.has("win32") == 1 then refresh_root = refresh_root:lower() end
  refresh_root = refresh_root:gsub("/$", "") .. "/"
  vim.api.nvim_create_autocmd("BufWritePost", { group = state.group, callback = function(event)
    if not state.ready or not state.active or not host_current(state) then return end
    local filename = vim.fs.normalize(event.file)
    if vim.fn.has("win32") == 1 then filename = filename:lower() end
    if filename:sub(1, #refresh_root) == refresh_root then M.refresh(state) end
  end })
  vim.api.nvim_create_autocmd({ "BufEnter", "FocusGained" }, { group = state.group, callback = function()
    if require("forge.infra.popup_window").restoring_origin then return end
    if state.ready and state.active and host_current(state) and vim.api.nvim_get_current_buf() == state.replica.buffer then
      M.refresh(state)
    end
  end })
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled", "BufWinEnter" }, {
    group = state.group, buffer = state.replica.buffer, callback = function(event)
      if event.event == "BufWinEnter" and state.active and state.replica.status == "Applied" then
        local window = vim.api.nvim_get_current_win()
        if vim.api.nvim_win_get_buf(window) == state.replica.buffer then view_for(state, window) end
      end
      M.demand(state)
    end,
  })
  vim.api.nvim_create_autocmd("WinClosed", {
    group = state.group, callback = function() M.demand(state) end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", { group = state.group, buffer = state.replica.buffer,
    once = true, callback = function() M.close(state) end })
  local handler = {
    open = function(visual) M.action(state, "open", visual) end,
    next_hunk = function() M.navigate(state, true) end,
    previous_hunk = function() M.navigate(state, false) end,
    refresh = function() M.refresh(state) end,
    close = function() M.close(state) end,
    stage = function(visual) M.action(state, "stage", visual) end,
    unstage = function(visual) M.action(state, "unstage", visual) end,
    discard = function(visual) M.action(state, "discard", visual) end,
    ignore = function(visual) M.action(state, "ignore", visual) end,
  }
  local function bind_commands()
  if state.commands then state.commands.close() end
  local comparison_title = options.comparison and (options.name or "ForgeBranchDiff") or "ForgeStatus"
  state.commands = require("forge.document_commands").attach(state.replica, {
    view = options.comparison and "diff" or "status", title = comparison_title,
    narrow_title = comparison_title,
    keymaps = options.keymaps, handler = vim.tbl_extend("force", handler, options.handler or {}), changed = function()
      local started = vim.uv.hrtime()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local located = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
      local location = located and located.location
      if location and location.kind == "file" and not state.done[location.id] and vim.fn.foldclosed(cursor[1]) == -1 then
        state.body_started = state.body_started or {}
        state.body_started[location.id] = started
        require("forge.startup_log").write("status.body.requested", { document = state.document, file = location.id }, started)
      end
      M.demand(state)
    end,
  })
  vim.keymap.set("n", "zR", function()
    state.fold_open = state.fold_open or { window = {}, command = "zR" }
    state.fold_open.window[vim.api.nvim_get_current_win()] = true
    M.demand(state)
  end, { buffer = state.replica.buffer, nowait = true, desc = "Open loaded Forge Status folds" })
  end
  bind_commands()
  require("forge.startup_log").write("status.buffer.prepared", { document = document,
    elapsed_ms = math.floor((vim.uv.hrtime() - open_started) / 1e6) })
  vim.api.nvim_create_autocmd("BufWinLeave", { group = state.group, buffer = state.replica.buffer, callback = function()
    local window = vim.api.nvim_get_current_win()
    if state.view[window] then close_view(state, window, state.view[window]) end
  end })
  local opening = { operation = "open", document = document, workspace = options.workspace or vim.fn.getcwd() }
  if options.comparison then
    opening.operation = "comparison"
    opening.reference, opening.worktree = options.comparison.reference, options.comparison.worktree == true
    opening.path = options.comparison.path and vim.base64.encode(options.comparison.path) or nil
  elseif options.filename then
    opening.operation, opening.filename, opening.workspace = "local", options.filename, nil
  end
  local function start_open()
  require("forge.startup_log").write("status.open.dispatch", { document = document })
  if not state.active then return end
  request(state, opening, function(result, failure)
    if vim.api.nvim_buf_is_valid(state.replica.buffer) then
      vim.api.nvim_buf_clear_namespace(state.replica.buffer, loading_namespace, 0, -1)
    end
    if failure or not result then
      state.finish_open(failure or "Missing status document")
      state.opening_failed = true
      if options.bind ~= false and vim.api.nvim_win_is_valid(opening_window)
        and vim.api.nvim_win_get_buf(opening_window) == state.replica.buffer
        and vim.api.nvim_buf_is_valid(opening_buffer) then
        vim.api.nvim_win_set_buf(opening_window, opening_buffer)
        vim.api.nvim_win_call(opening_window, function() vim.fn.winrestview(opening_view) end)
      end
      M.close(state)
      notice(failure or "Missing status document")
      return
    end
    local snapshot_apply_started = vim.uv.hrtime()
    require("forge.startup_log").write("status.snapshot.apply", { document = document, files = #result.file })
    buffer.apply_snapshot(state.replica, result, function(applied)
    if not state.active or applied.kind ~= "Applied" then return end
    require("forge.startup_log").write("status.snapshot.applied", { document = document, status = state.replica.status,
      files = #result.file, rows = state.replica.row_count,
      elapsed_ms = math.floor((vim.uv.hrtime() - snapshot_apply_started) / 1e6) })
    local post_apply_started = vim.uv.hrtime()
    if result.context and result.context ~= vim.NIL then
      state.native_context = true
      local displayed_window = vim.fn.win_findbuf(state.replica.buffer)
      local window = displayed_window[1]
      for _, attached in ipairs(displayed_window) do view_for(state, attached) end
      vim.api.nvim_create_autocmd("WinResized", { group = state.group, callback = function()
        if not host_current(state) then return end
        local owner = state.width_owner
        if owner and vim.api.nvim_win_is_valid(owner) then buffer.resize(state.replica, owner) end
      end })
      local function current(captured)
        local window
        for attached, view in pairs(state.view) do
          if view.id == captured.view then window = attached break end
        end
        if not window then return false end
        local view = state.view[window]
        local alive = state.active and view and view.active and view.id == captured.view and view.sequence == captured.sequence
          and state.replica.revision == captured.revision and vim.api.nvim_win_is_valid(window)
          and vim.api.nvim_win_get_buf(window) == state.replica.buffer
        if not alive then return false end
        local cursor = vim.api.nvim_win_get_cursor(window)
        local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
        return location and vim.deep_equal(location.location, captured.location)
      end
      state.context = require("forge.views.status.status_context").attach({
        document_id = document, workspace = options.workspace or vim.fn.getcwd(), window = window,
        is_alive = function() return state.active and host_current(state) end,
        get_info = function() return state.replica.inventory.context end,
        ignored_paths = function()
          local paths = {}
          for _, file in ipairs(state.replica.inventory.file) do
            if file.section == "ignored" then paths[#paths + 1] = file.path end
          end
          return paths
        end,
        present = function(presentation) buffer.present_context(state.replica, presentation) end,
        request = function(params, callback) request(state, params, callback, "status.context") end,
        is_input_current = current,
        input_window = function(captured)
          for attached, view in pairs(state.view) do if view.id == captured.view then return attached end end
        end,
        capture_input = function(target, original)
          local window = vim.api.nvim_get_current_win()
          if original then
            window = nil
            for attached, view in pairs(state.view) do if view.id == original.view then window = attached break end end
            if not window then return nil, "Context view closed" end
          end
          if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then return nil, "Context view is not attached" end
          local captured, failure = input.capture(state.replica, view_for(state, window), "context")
          if captured and (captured.location.kind ~= "context" or "status:context:" .. captured.location.role ~= target) then return nil, "Context target changed" end
          return captured, failure
        end,
        refresh_status = function() M.refresh(state) end,
        open_commit = options.open_commit,
        open_pr = options.open_pr, open_about = options.open_about,
        edit_issues = options.edit_issues or function(_, _, captured)
          for attached, view in pairs(state.view) do
            if view.id == captured.view then state.replica.issues_editor.focus(attached) return end
          end
        end,
      })
      require("forge.views.status.issues_editor").attach(state.replica, function(text, callback)
        local window = vim.api.nvim_get_current_win()
        local captured, failure = buffer.capture(state.replica, view_for(state, window), "context",
          { target = { { kind = "context", role = "issues" } } })
        if not captured then notice(failure or "Issues input is unavailable") callback(false) return end
        state.context.save_issues(captured, text, callback)
      end)
      handler = vim.tbl_extend("force", handler, require("forge.views.status.status_context").producer_handlers({
        workspace = options.workspace or vim.fn.getcwd(), window = vim.api.nvim_get_current_win,
        is_alive = function() return state.active and host_current(state) end,
        refresh_status = function() M.refresh(state) end,
        context_info = function() return state.context and state.context.info end,
        context = function() return state.context end,
      }))
      handler.walkthrough = function()
        require("forge.walkthrough").open({ workspace = options.workspace or vim.fn.getcwd(), window = vim.api.nvim_get_current_win() })
      end
      bind_commands()
      state.context.refresh()
    end
    M.demand(state)
    require("forge.startup_log").write("status.ready", { document = document,
      post_apply_ms = math.floor((vim.uv.hrtime() - post_apply_started) / 1e6),
      elapsed_ms = math.floor((vim.uv.hrtime() - open_started) / 1e6) })
    state.finish_open()
    state.ready = true
    if options.bind ~= false then
      require("forge.startup_log").watch_redraw(state.replica.buffer, document, open_started)
    end
    if options.on_ready then options.on_ready(state) end
    end)
  end)
  end
  if options.before_open then options.before_open(start_open) else start_open() end
  return state
end

function M.open_local(options)
  local state = M.open(options)
  vim.api.nvim_buf_set_name(state.replica.buffer, "diff://" .. options.filename .. "#" .. state.replica.buffer)
  vim.api.nvim_create_autocmd("BufEnter", { group = state.group, buffer = state.replica.buffer, callback = function()
    if require("forge.infra.popup_window").restoring_origin then return end
    if state.replica.status == "Applied" then M.refresh(state) end
  end })
  return state
end

function M.open_comparison(options)
  local keymaps = vim.tbl_extend("force", options.keymaps or {}, { stage = false, unstage = false, discard = false })
  return M.open(vim.tbl_extend("force", options, { keymaps = keymaps, comparison = {
    reference = options.reference, worktree = options.worktree == true, path = options.path,
  } }))
end

---@param state ForgeNativeStatus
function M.close(state, on_closed)
  if state.finish_open then state.finish_open("view closed") end
  if not state.active then return end
  state.active = false
  if state.unsubscribe_document then state.unsubscribe_document() state.unsubscribe_document = nil end
  for _, complete in ipairs(state.refresh_callback or {}) do complete(false, "Status document is closed") end
  state.refresh_callback = {}
  if state.commands then state.commands.close() end
  if state.context then state.context.close() end
  for window, view in pairs(state.view) do close_view(state, window, view) end
  if state.group ~= 0 then vim.api.nvim_del_augroup_by_id(state.group) end
  request(state, { operation = "close", document = state.document }, function(_, failure)
    if failure and not (state.opening_failed and failure == "unknown status document") then notice(failure) end
    if on_closed then on_closed(failure) end
  end)
  buffer.close(state.replica)
end

---@param runner? fun(method: string, params: table, callback: fun(result: table?, failure: string?))
function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
