local M = {}
local client = require("forge.client")
local replica = require("forge.buffer")
local input = require("forge.input")
local editable = require("forge.editable")

function M.open(options, callback)
  local identity = "harness:" .. options.session_id .. ":" .. tostring(vim.uv.hrtime())
  local owner = { document = identity, composer_id = identity .. ":composer", closed = false, syncing = false, pending = false, output = {},
    session_id = options.session_id, host_generation = client.host_generation(), views = {} }
  local function alive()
    return not owner.closed and owner.host_generation == client.host_generation()
      and client.host_accepting() and options.is_alive()
  end
  local function notice(message)
    if options.notice then options.notice(message) end
  end
  local queued, active = {}, false
  local function dispatch_next()
    if active or #queued == 0 then return end
    active = true
    local current = table.remove(queued, 1)
    local function receive(result, failure)
      local accepted, callback_error = pcall(current.done, result, failure)
      active = false
      dispatch_next()
      if not accepted then notice(tostring(callback_error)) end
    end
    if owner.host_generation ~= client.host_generation() then
      receive(current.params.operation == "close" and {} or nil, current.params.operation ~= "close" and "Harness host generation changed" or nil)
    else client.request_for(options.session_id, "harness.document", current.params, receive) end
  end
  local function request(params, done)
    if owner.host_generation ~= client.host_generation() then
      done(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
    if params.operation == "edit_composer" or (params.operation == "snapshot" and params.document == owner.composer_id) then
      client.request_for(options.session_id, "harness.document", params, done)
      return
    end
    if #queued >= 63 and params.operation ~= "close" then done(nil, "Harness presentation request capacity is full") return end
    queued[#queued + 1] = { params = params, done = done }
    dispatch_next()
  end
  local function view_for(window)
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= owner.transcript.buffer then return nil end
    local view = owner.views[window]
    if view then return view end
    view = input.open(owner.transcript, window, { margin = 0 })
    owner.views[window] = view
    request({ operation = "open_view", document = identity, view = view.id,
      width = require("forge.width").capture(window) }, function(_, failure)
      if not alive() then return end
      if failure then notice(failure) else owner.sync() end
    end)
    return view
  end
  local function action_view(captured)
    if captured then
      for _, view in pairs(owner.views) do if view.id == captured.view then return view end end
      return nil
    end
    return view_for(vim.api.nvim_get_current_win()) or view_for(options.transcript_window)
  end
  local function submit_ready()
    local pending = owner.pending_submit
    if not pending or not alive() or editable.suspend_generated_text(owner.composer.editable) then return end
    owner.pending_submit = nil
    if vim.api.nvim_buf_get_changedtick(options.composer_buffer) ~= pending.changedtick then
      pending.callback(nil, "Composer changed while awaiting its edit acknowledgement")
      return
    end
    local region = owner.composer.editable.region.composer
    client.request_for(options.session_id, "prompt.submit", { composer = { document = owner.composer_id, revision = region.revision } }, pending.callback)
  end
  local function recovery(document)
    request({ operation = "snapshot", document = document.document }, function(snapshot, failure)
      if not alive() then return end
      if failure then notice(failure) return end
      local result = replica.apply_snapshot(document, snapshot)
      if result.kind ~= "Applied" then notice("Harness snapshot could not be adopted: " .. tostring(result.kind)) end
    end)
  end
  local transcript_tick = vim.api.nvim_buf_get_changedtick(options.transcript_buffer)
  local composer_tick = vim.api.nvim_buf_get_changedtick(options.composer_buffer)
  local function reject_open(message)
    owner.closed = true
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    if owner.view then input.close(owner.view) end
    if owner.transcript and not owner.transcript.generated_owned then replica.close(owner.transcript) end
    if owner.composer and not owner.composer.generated_owned then replica.close(owner.composer) end
    request({ operation = "close", document = identity }, function() end)
    callback(nil, message)
  end
  owner.transcript = replica.open(identity, { buffer = options.transcript_buffer, generated = true,
    expected_changedtick = transcript_tick, filetype = "ForgeHarness", notice = notice,
    recover = function() recovery(owner.transcript) end })
  owner.composer = replica.open(owner.composer_id, { buffer = options.composer_buffer, generated = true,
    expected_changedtick = composer_tick, filetype = "ForgeHarnessInput", notice = notice,
    editable = { notice = notice, send = function(edit)
      if not alive() then return false end
      request({ operation = "edit_composer", edit = edit }, function(result, failure)
        if not alive() then return end
        if failure or not result or not result.accepted then
          notice(failure or "Harness composer changed before edit acknowledgement")
          if owner.pending_submit then
            local pending = owner.pending_submit
            owner.pending_submit = nil
            pending.callback(nil, failure or "Composer edit was not accepted")
          end
          return
        end
        local applied = replica.acknowledge_edit(owner.composer, result.acknowledgement, result.patch)
        if applied.kind == "Applied" then vim.bo[options.composer_buffer].modifiable = true end
        if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then notice("Harness composer acknowledgement failed: " .. tostring(applied.kind)) end
        submit_ready()
      end)
      return true
    end } })
  owner.view = input.open(owner.transcript, options.transcript_window, { margin = 0 })
  owner.views[options.transcript_window] = owner.view
  request({ operation = "open", document = identity, composer = owner.composer_id, view = owner.view.id,
    width = require("forge.width").capture(options.transcript_window),
    initial = vim.api.nvim_buf_get_lines(options.composer_buffer, 0, -1, false) }, function(opened, failure)
    if not alive() then
      request({ operation = "close", document = identity }, function() end)
      return
    end
    if failure then reject_open(failure) return end
    if vim.api.nvim_buf_get_changedtick(options.transcript_buffer) ~= transcript_tick
        or vim.api.nvim_buf_get_changedtick(options.composer_buffer) ~= composer_tick then
      reject_open("Harness startup text changed before native adoption")
      return
    end
    local transcript = replica.apply_snapshot(owner.transcript, opened.transcript)
    local composer = replica.apply_snapshot(owner.composer, opened.composer)
    if transcript.kind ~= "Applied" or composer.kind ~= "Applied" then
      reject_open("Harness native documents could not be adopted")
      return
    end
    owner.ready = true
    vim.bo[options.composer_buffer].modifiable = true
    callback(owner)
  end)

  function owner.sync()
    if not alive() or not owner.ready then return end
    owner.pending = true
    if owner.syncing then return end
    owner.pending, owner.syncing = false, true
    request({ operation = "sync", document = identity, revision = owner.transcript.revision }, function(result, failure)
      owner.syncing = false
      if not alive() then return end
      if failure then notice(failure) return end
      local follow = {}
      local previous_rows = vim.api.nvim_buf_line_count(options.transcript_buffer)
      for window in pairs(owner.views) do
        if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer
          and vim.api.nvim_win_get_cursor(window)[1] >= previous_rows then follow[#follow + 1] = window end
      end
      if result.snapshot then replica.apply_snapshot(owner.transcript, result.snapshot)
      else
        for _, patch in ipairs(result.patch or {}) do
          local applied = replica.apply_patch(owner.transcript, patch)
          if applied.kind ~= "Applied" then break end
        end
      end
      for _, window in ipairs(follow) do
        if vim.api.nvim_buf_line_count(options.transcript_buffer) > previous_rows
          and vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer then
          vim.api.nvim_win_set_cursor(window, { vim.api.nvim_buf_line_count(options.transcript_buffer), 0 })
        end
      end
      if options.on_update then options.on_update() end
      if owner.pending then owner.sync() end
    end)
  end

  function owner.activate(callback)
    if not alive() or not owner.ready then return end
    local view = action_view()
    if not view then return end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return end
    request({ operation = "input", input = captured }, function(action, action_error)
      if not alive() then return end
      if action_error then notice(action_error) return end
      if owner.transcript.status ~= "Applied" or owner.transcript.revision ~= captured.revision or view.sequence ~= captured.sequence
          or not vim.api.nvim_win_is_valid(view.window)
          or vim.api.nvim_win_get_buf(view.window) ~= owner.transcript.buffer then return end
      if not vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor) then return end
      callback(action, captured)
    end)
  end

  function owner.refresh_views()
    if not alive() or not owner.ready then return end
    for _, window in ipairs(vim.fn.win_findbuf(owner.transcript.buffer)) do view_for(window) end
    for window, view in pairs(owner.views) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= owner.transcript.buffer then
        input.close(view)
        owner.views[window] = nil
        if owner.pending_width then owner.pending_width[view.id] = nil end
        request({ operation = "close_view", document = identity, view = view.id }, function(_, failure)
          if not alive() then return end
          if failure then notice(failure) else owner.sync() end
        end)
      end
    end
  end

  function owner.resize()
    if not alive() or not owner.ready then return end
    owner.refresh_views()
    owner.pending_width = owner.pending_width or {}
    for window, view in pairs(owner.views) do
      owner.pending_width[view.id] = { view = view.id, width = require("forge.width").capture(window) }
    end
    if owner.resizing then return end
    local function send_width()
      local view, pending = next(owner.pending_width)
      if not view or not pending then owner.sync() return end
      owner.pending_width[view], owner.resizing = nil, true
      request({ operation = "resize", document = identity, view = pending.view, width = pending.width }, function(_, failure)
        owner.resizing = false
        if not alive() then return end
        if failure then notice(failure) return end
        send_width()
      end)
    end
    send_width()
  end

  function owner.open_output(action, previous_input)
    if not alive() or not owner.ready or (action.kind ~= "tool" and action.kind ~= "diff") then return false end
    local view = action_view(previous_input)
    if not view then return false end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return false end
    local function current()
      return alive() and owner.transcript.status == "Applied" and owner.transcript.revision == captured.revision
        and view.sequence == captured.sequence and vim.api.nvim_win_is_valid(view.window)
        and vim.api.nvim_win_get_buf(view.window) == owner.transcript.buffer
        and vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor)
    end
    local output_options = { session_id = options.session_id, input = captured, window = view.window,
      is_current = current, notice = notice, on_error = notice }
    if action.kind == "diff" then
      require("forge.source_document").open_harness_diff(output_options)
    else
      local retained = {}
      for _, output in ipairs(owner.output) do if not output.closed then retained[#retained + 1] = output end end
      retained[#retained + 1] = require("forge.views.harness.tool_output").open(output_options)
      owner.output = retained
    end
    return true
  end

  function owner.navigate_prompt(previous)
    if not alive() or not owner.ready then return end
    local view = action_view()
    if not view then return end
    local captured, failure = input.capture(owner.transcript, view, "navigate_prompt")
    if not captured then notice(failure) return end
    request({ operation = "navigate_prompt", input = captured, previous = previous }, function(result, navigation_error)
      if not alive() then return end
      if navigation_error then notice(navigation_error) return end
      if result.anchor and result.anchor ~= vim.NIL then
        require("forge.effects").apply(owner.transcript, view, {
          id = "prompt", document = captured.document, revision = captured.revision, view = captured.view,
          sequence = captured.sequence, kind = "cursor", block = result.anchor.block, position = result.anchor.position,
        })
      end
    end)
  end

  function owner.select_agent(run_id)
    if not alive() or not owner.ready then return end
    request({ operation = "select_agent", document = identity, run_id = run_id or vim.NIL }, function(_, failure)
      if not alive() then return end
      if failure then notice(failure) return end
      owner.sync()
    end)
  end

  function owner.submit(callback)
    if not alive() or not owner.ready then callback(nil, "Harness documents are not ready") return end
    if owner.pending_submit then callback(nil, "Composer submission already awaits its edit acknowledgement") return end
    owner.pending_submit = { changedtick = vim.api.nvim_buf_get_changedtick(options.composer_buffer), callback = callback }
    if not editable.flush(owner.composer.editable) then
      owner.pending_submit = nil
      callback(nil, "Composer edit could not be sent")
      return
    end
    submit_ready()
  end

  function owner.receive(event)
    if not alive() or not owner.ready or event.kind ~= "composer_patch" then return false end
    local patch = event.data
    if type(patch) ~= "table" or patch.document ~= owner.composer_id then return false end
    local applied = replica.apply_patch(owner.composer, patch)
    if applied.kind == "Applied" then vim.bo[options.composer_buffer].modifiable = true
    elseif applied.kind ~= "Deferred" then notice("Harness composer transition failed: " .. tostring(applied.kind)) end
    submit_ready()
    return true
  end

  function owner.close(close_options)
    if owner.closed then return true end
    local collected = owner.host_generation ~= client.host_generation()
      or not vim.api.nvim_buf_is_valid(options.composer_buffer) or not vim.api.nvim_buf_is_valid(options.transcript_buffer)
    if collected then replica.invalidate(owner.composer)
    else
      local composer = replica.close(owner.composer, close_options)
      if composer and composer.kind == "Deferred" then return false end
    end
    owner.closed = true
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    for _, output in ipairs(owner.output) do output.close() end
    owner.output = {}
    for _, view in pairs(owner.views) do input.close(view) end
    owner.views = {}
    if collected then replica.invalidate(owner.transcript) else replica.close(owner.transcript, close_options) end
    request({ operation = "close", document = identity }, function(_, failure)
      if failure then notice(failure) end
    end)
    return true
  end
  owner.group = vim.api.nvim_create_augroup("ForgeHarnessPresentation" .. tostring(vim.uv.hrtime()), { clear = true })
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, { group = owner.group, callback = owner.resize })
  vim.api.nvim_create_autocmd({ "BufWinEnter", "BufWinLeave", "WinClosed" }, {
    group = owner.group, callback = function() vim.schedule(owner.refresh_views) end,
  })
  for _, buffer in ipairs({ options.transcript_buffer, options.composer_buffer }) do
    vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = buffer, once = true,
      callback = function() vim.schedule(function() owner.close() end) end })
  end
  return owner
end

return M
