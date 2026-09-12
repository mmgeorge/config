local M = {}
local client = require("forge.client")
local buffer = require("forge.buffer")
local input = require("forge.input")
local editable = require("forge.editable")

---@param window integer
---@return table
local function review_view_options(window)
  local columns = require("forge.window_presentation").capture(window)
  columns.number, columns.relativenumber = true, false
  columns.statuscolumn = vim.go.statuscolumn
  return { columns = columns, conceal = { level = 3, cursor = "" },
    wrapping = { indent = columns.breakindent, options = columns.breakindentopt } }
end

function M.attach(options, callback)
  local owner = { document = "plan:review:" .. tostring(vim.uv.hrtime()), generation = client.host_generation(), closed = false, views = {} }
  local action_tick = setmetatable({}, { __mode = "k" })
  local function alive()
    return not owner.closed and owner.generation == client.host_generation() and client.host_accepting()
      and vim.api.nvim_buf_is_valid(options.buffer)
  end
  local queue, active = {}, false
  local function dispatch()
    if active or #queue == 0 then return end
    active = true
    local next_request = table.remove(queue, 1)
    local function receive(result, failure)
      local accepted, callback_error = pcall(next_request.receive, result, failure)
      active = false
      dispatch()
      if not accepted and options.notice then options.notice(tostring(callback_error)) end
    end
    if owner.generation ~= client.host_generation() then
      receive(nil, "Plan review host generation changed")
    else client.request_for(options.session_id, "harness.document", next_request.params, receive) end
  end
  local function request(params, receive)
    if params.operation == "plan_edit" then
      client.request_for(options.session_id, "harness.document", params, receive)
      return
    end
    if #queue >= 63 and params.operation ~= "plan_close" then receive(nil, "Plan review request admission is full") return end
    queue[#queue + 1] = { params = params, receive = receive }
    dispatch()
  end
  local submit_ready
  function owner.close()
    if owner.closed then return true end
    if owner.replica then
      if owner.generation ~= client.host_generation() or not vim.api.nvim_buf_is_valid(options.buffer) then buffer.invalidate(owner.replica)
      else
        local closed = buffer.close(owner.replica, { preserve_buffer = true })
        if closed and closed.kind == "Deferred" then return false end
      end
    end
    owner.closed = true
    if owner.pending_submit then
      local pending = owner.pending_submit
      owner.pending_submit = nil
      pending.callback(nil, "Plan review closed before submission admission")
    end
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    for _, view in pairs(owner.views) do input.close(view) end
    if owner.generation == client.host_generation() and client.host_accepting() then
      request({ operation = "plan_close", document = owner.document }, function(_, failure)
        if failure and options.notice then options.notice(failure) end
      end)
    end
    return true
  end
  local function finish_save()
    if submit_ready then submit_ready() end
    if not owner.pending_save or editable.suspend_generated_text(owner.replica.editable) then return end
    local tick = owner.pending_save
    owner.pending_save = nil
    if tick == vim.api.nvim_buf_get_changedtick(options.buffer) then vim.bo[options.buffer].modified = false end
  end
  submit_ready = function()
    local pending = owner.pending_submit
    if not pending or not alive() or editable.suspend_generated_text(owner.replica.editable) then return end
    owner.pending_submit = nil
    if pending.tick ~= vim.api.nvim_buf_get_changedtick(options.buffer) then
      pending.callback(nil, "Plan review changed while awaiting saved annotation acknowledgement") return
    end
    local view = owner.current_view()
    if not view then pending.callback(nil, "Plan review view is closed") return end
    local captured, failure = input.capture(owner.replica, view, pending.method)
    if not captured then pending.callback(nil, failure) return end
    pending.params.review = captured
    client.request_for(options.session_id, pending.method, pending.params, pending.callback)
  end
  function owner.submit(method, params, receive)
    if not alive() or not owner.ready or owner.pending_submit then receive(nil, "Plan review is not ready for submission") return end
    owner.pending_submit = { method = method, params = params or {}, callback = receive,
      tick = vim.api.nvim_buf_get_changedtick(options.buffer) }
    editable.flush(owner.replica.editable)
    submit_ready()
  end
  function owner.recovery()
    local state = owner.replica and owner.replica.editable
    if not state or state.fault then return nil, "Plan review contains text outside its editable annotation regions" end
    local draft = {}
    for region in pairs(state.region) do
      local captured, failure = pcall(editable.capture, state, region)
      if not captured then return nil, tostring(failure) end
      draft[region] = editable.recoverable_text(state, region)
    end
    return { draft = draft, saved_source_digest = owner.saved_source_digest }
  end
  owner.replica = buffer.open(owner.document, { buffer = options.buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(options.buffer), notice = options.notice,
    editable = { notice = options.notice, send = function(edit)
      if not alive() then return false end
      request({ operation = "plan_edit", edit = edit }, function(result, failure)
        if not alive() then return end
        if failure or not result or not result.accepted then
          if options.notice then options.notice(failure or "Plan annotation edit conflicted with its saved source") end
          if owner.pending_submit then
            local pending = owner.pending_submit
            owner.pending_submit = nil
            pending.callback(nil, failure or "Plan annotation edit was not saved")
          end
          return
        end
        local adopted = buffer.acknowledge_edit(owner.replica, result.acknowledgement, result.patch)
        if adopted.kind == "Applied" then
          vim.bo[options.buffer].modifiable = true
          if not editable.suspend_generated_text(owner.replica.editable) then vim.bo[options.buffer].modified = false end
        end
        finish_save()
      end)
      return true
    end } })
  owner.view = input.open(owner.replica, options.window, review_view_options(options.window))
  owner.views[options.window] = owner.view
  local function adopt_view(result, failure)
    if not alive() then return end
    if failure then if options.notice then options.notice(failure) end return end
    if result.patch and result.patch ~= vim.NIL then
      local adopted = buffer.apply_patch(owner.replica, result.patch)
      if adopted.kind == "Applied" then vim.bo[options.buffer].modifiable = true end
    end
  end
  local function view_for(window)
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= options.buffer then return nil end
    if owner.views[window] then return owner.views[window] end
    local view = input.open(owner.replica, window, review_view_options(window))
    owner.views[window] = view
    if options.configure_view then options.configure_view(view, owner) end
    request({ operation = "plan_view", document = owner.document, view = view.id,
      width = require("forge.width").capture(window) }, adopt_view)
    return view
  end
  function owner.current_view()
    return view_for(vim.api.nvim_get_current_win()) or view_for(vim.fn.win_findbuf(options.buffer)[1] or -1)
  end
  function owner.refresh_views()
    if not alive() or not owner.ready then return end
    for window, view in pairs(owner.views) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= options.buffer then
        input.close(view)
        owner.views[window] = nil
        request({ operation = "plan_view", document = owner.document, view = view.id }, adopt_view)
      end
    end
    for _, window in ipairs(vim.fn.win_findbuf(options.buffer)) do view_for(window) end
  end
  function owner.is_current(captured)
    if not alive() or owner.replica.revision ~= captured.revision then return false end
    if action_tick[captured] and action_tick[captured] ~= vim.api.nvim_buf_get_changedtick(options.buffer) then return false end
    for _, view in pairs(owner.views) do
      if view.id == captured.view then
        return view.active and view.sequence == captured.sequence and vim.api.nvim_win_is_valid(view.window)
          and vim.api.nvim_win_get_buf(view.window) == options.buffer
          and vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor)
      end
    end
    return false
  end
  request({ operation = "plan_open", document = owner.document, view = owner.view.id,
    plan_id = options.plan.id, digest = options.plan.review_digest,
    saved_source_digest = options.recovery and options.recovery.saved_source_digest or nil,
    width = require("forge.width").capture(options.window) }, function(opened, failure)
    if not alive() then owner.close() return end
    if failure then owner.close() callback(nil, failure) return end
    if vim.fs.normalize(vim.api.nvim_buf_get_name(options.buffer)) ~= vim.fs.normalize(opened.path) then
      owner.close() callback(nil, "Physical plan review path changed before attachment") return
    end
    if options.recovery then
      local region = {}
      for _, block in ipairs(opened.snapshot.block) do
        for _, editable_region in ipairs(block.metadata.editable_region or {}) do region[editable_region.id] = true end
      end
      for id in pairs(options.recovery.draft) do
        if not region[id] then owner.close() callback(nil, "A retained plan annotation is missing from saved storage") return end
      end
    end
    local adopted = buffer.apply_snapshot(owner.replica, opened.snapshot)
    if adopted.kind ~= "Applied" then
      owner.close() callback(nil, "Physical plan review changed before attachment: " .. adopted.kind) return
    end
    if options.configure_view then options.configure_view(owner.view, owner) end
    owner.saved_source_digest, owner.version = opened.saved_source_digest, opened.version
    owner.ready = true
    vim.bo[options.buffer].modified = false
    vim.bo[options.buffer].modifiable = true
    vim.bo[options.buffer].buftype = "acwrite"
    if options.recovery then
      local restore = {}
      for id, text in pairs(options.recovery.draft) do
        restore[#restore + 1] = { anchor = vim.deepcopy(owner.replica.editable.native.anchor[id]), text = text }
      end
      table.sort(restore, function(left, right) return left.anchor.start.row > right.anchor.start.row end)
      for _, retained in ipairs(restore) do
        local start, finish = retained.anchor.start, retained.anchor.finish
        if not vim.deep_equal(vim.api.nvim_buf_get_text(options.buffer, start.row, start.column, finish.row, finish.column, {}), retained.text) then
          vim.api.nvim_buf_set_text(options.buffer, start.row, start.column, finish.row, finish.column, retained.text)
        end
      end
    end
    owner.group = vim.api.nvim_create_augroup("ForgePlanDocument" .. options.buffer, { clear = true })
    vim.api.nvim_create_autocmd({ "BufWinEnter", "BufWinLeave", "WinClosed" }, { group = owner.group,
      callback = function() vim.schedule(owner.refresh_views) end })
    vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, { group = owner.group, callback = function()
      if owner.resize_scheduled then return end
      owner.resize_scheduled = true
      vim.schedule(function()
        owner.resize_scheduled = false
        if not alive() then return end
        owner.refresh_views()
        for window, view in pairs(owner.views) do
          request({ operation = "plan_view", document = owner.document, view = view.id,
            width = require("forge.width").capture(window) }, adopt_view)
        end
      end)
    end })
    vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = options.buffer,
      callback = function() vim.schedule(owner.close) end })
    vim.api.nvim_create_autocmd("BufWriteCmd", { group = owner.group, buffer = options.buffer, callback = function()
      if not alive() then return end
      owner.pending_save = vim.api.nvim_buf_get_changedtick(options.buffer)
      editable.flush(owner.replica.editable)
      finish_save()
    end })
    callback(owner)
  end)
  function owner.action(action, receive)
    if not alive() or not owner.ready then return end
    local view = owner.current_view()
    if not view then return end
    local captured, failure = input.capture(owner.replica, view, action)
    if not captured then if options.notice then options.notice(failure) end return end
    local tick = vim.api.nvim_buf_get_changedtick(options.buffer)
    action_tick[captured] = tick
    request({ operation = action == "comment" and "plan_add_annotation" or "plan_action", input = captured }, function(anchor, action_error)
      if not owner.is_current(captured)
          or tick ~= vim.api.nvim_buf_get_changedtick(options.buffer) then return end
      if not action_error and action == "comment" then
        local adopted = buffer.apply_patch(owner.replica, anchor.patch)
        if adopted.kind ~= "Applied" then if options.notice then options.notice("Plan annotation layout requires reconciliation: " .. adopted.kind) end return end
        vim.bo[options.buffer].modifiable = true
      end
      receive(anchor, action_error, captured)
    end)
  end
  return owner
end

return M
