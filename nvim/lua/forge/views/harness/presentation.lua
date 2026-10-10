local M = {}
local client = require("forge.client")
local replica = require("forge.buffer")
local input = require("forge.input")
local perf = require("forge.infra.perf")
local transcript_options = {
  margin = 0,
  scrolloff = 3,
  conceal = { level = 3, cursor = "nvic" },
  columns = { signcolumn = "yes:1", statuscolumn = "%s" },
  wrapping = { indent = true, options = "shift:0" },
}

function M.open(options, callback)
  local identity = "harness:" .. options.session_id .. ":" .. tostring(vim.uv.hrtime())
  local owner = { document = identity, submission_sequence = 0, closed = false, syncing = false, pending = false, output = {},
    session_id = options.session_id, host_generation = client.host_generation(), views = {},
    timeline_key = "main", timeline_view = {} }
  local function alive()
    return not owner.closed and owner.host_generation == client.host_generation()
      and client.host_accepting() and options.is_alive()
  end
  local function notice(message)
    if options.notice then options.notice(message) end
  end
  local queued, active = {}, false
  local markdown = require("forge.render.harness.markdown")
  local function render_markdown(force)
    if not alive() or owner.transcript.status ~= "Applied" or owner.transcript.applying or owner.transcript.update_pending then return end
    local range_list, retained, window_list = {}, {}, {}
    for window in pairs(owner.views) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer then
        window_list[#window_list + 1] = window
        for _, range in ipairs(markdown.viewport(owner.transcript, window)) do
          if not retained[range.id] then
            retained[range.id] = true
            range_list[#range_list + 1] = range
          end
        end
      end
    end
    table.sort(range_list, function(left, right) return left.first0 < right.first0 end)
    table.sort(window_list)
    if not force and vim.deep_equal(owner.markdown_ranges, range_list)
      and vim.deep_equal(owner.markdown_windows, window_list) then return end
    owner.markdown_ranges, owner.markdown_windows = range_list, window_list
    if #window_list == 0 then markdown.render(options.transcript_buffer, nil, {}) return end
    for _, window in ipairs(window_list) do
      perf.trace("harness", "ui.markdown", { session_id = options.session_id,
        revision = owner.transcript.revision, buf = options.transcript_buffer,
        line_count = vim.api.nvim_buf_line_count(options.transcript_buffer), count = #range_list }, function()
        markdown.render(options.transcript_buffer, window, range_list)
      end)
    end
  end
  local function dispatch_next()
    if active or owner.applying or #queued == 0 then return end
    active = true
    local current = table.remove(queued, 1)
    local started = perf.now()
    perf.event("harness", "ui.request", { phase = "begin", session_id = options.session_id,
      operation = current.params.operation, revision = current.params.revision,
      queue_count = #queued, elapsed_ms = perf.elapsed_ms(current.queued_at) })
    local function receive(result, failure)
      perf.event("harness", "ui.request", { phase = "end", session_id = options.session_id,
        operation = current.params.operation, revision = current.params.revision,
        elapsed_ms = perf.elapsed_ms(started), status = failure and "error" or "ok" })
      local accepted, callback_error = pcall(perf.trace, "harness", "ui.response", {
        session_id = options.session_id, operation = current.params.operation,
        revision = current.params.revision }, function() current.done(result, failure) end)
      active = false
      dispatch_next()
      if not accepted then notice(tostring(callback_error)) end
    end
    if owner.host_generation ~= client.host_generation() or not client.host_accepting() then
      receive(current.params.operation == "close" and {} or nil, current.params.operation ~= "close" and "Harness host generation changed" or nil)
    else client.request_for(options.session_id, "harness.document", current.params, receive) end
  end
  local function request(params, done)
    if owner.host_generation ~= client.host_generation() or not client.host_accepting() then
      done(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
    if #queued >= 63 and params.operation ~= "close" then done(nil, "Harness presentation request capacity is full") return end
    queued[#queued + 1] = { params = params, done = done, queued_at = perf.now() }
    dispatch_next()
  end
  local function view_for(window)
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= owner.transcript.buffer then return nil end
    local view = owner.views[window]
    if view then return view end
    view = input.open(owner.transcript, window, transcript_options)
    owner.views[window] = view
    if owner.ready then render_markdown(true) end
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
  local function recovery(document)
    request({ operation = "snapshot", document = document.document }, function(snapshot, failure)
      if not alive() then return end
      if failure then notice(failure) return end
      owner.applying = true
      replica.apply_async(document, snapshot, alive, function(result)
        owner.applying = false
        if not alive() then return end
        if result.kind ~= "Applied" then
          notice("Harness snapshot could not be adopted: " .. tostring(result.kind))
        elseif vim.api.nvim_get_current_buf() ~= options.transcript_buffer then
          owner.follow_tail()
        end
        dispatch_next()
      end)
    end)
  end
  local transcript_tick = vim.api.nvim_buf_get_changedtick(options.transcript_buffer)
  local function reject_open(message)
    owner.closed = true
    markdown.clear(options.transcript_buffer)
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    if owner.view then input.close(owner.view) end
    if owner.transcript and not owner.transcript.generated_owned then replica.close(owner.transcript) end
    request({ operation = "close", document = identity }, function() end)
    callback(nil, message)
  end
  owner.transcript = replica.open(identity, { buffer = options.transcript_buffer, generated = true, preserve_view = true,
    expected_changedtick = transcript_tick, filetype = "ForgeHarness", notice = notice,
    recover = function() recovery(owner.transcript) end })
  owner.view = input.open(owner.transcript, options.transcript_window, transcript_options)
  owner.views[options.transcript_window] = owner.view
  request({ operation = "open", document = identity, view = owner.view.id,
    width = require("forge.width").capture(options.transcript_window) }, function(opened, failure)
    if not alive() then
      request({ operation = "close", document = identity }, function() end)
      return
    end
    if failure then reject_open(failure) return end
    if vim.api.nvim_buf_get_changedtick(options.transcript_buffer) ~= transcript_tick then
      reject_open("Harness startup text changed before native adoption")
      return
    end
    owner.applying = true
    replica.apply_async(owner.transcript, opened.transcript, alive, function(transcript)
    owner.applying = false
    if not alive() then return end
    if transcript.kind ~= "Applied" then
      reject_open("Harness native documents could not be adopted")
      return
    end
    owner.ready = true
    render_markdown()
    owner.terminals = require("forge.views.harness.terminals").watch({
      session_id = options.session_id, alive = alive, notice = notice,
      update = function(snapshot)
        owner.transcript.background_terminals = snapshot
        if options.on_update then options.on_update() end
      end,
    })
    vim.bo[options.composer_buffer].modifiable = true
    callback(owner)
    if vim.api.nvim_get_current_buf() ~= options.transcript_buffer then owner.follow_tail() end
    if opened.syntax_pending then vim.schedule(function() owner.highlight() end) end
    dispatch_next()
    end)
  end)

  function owner.sync()
    if not alive() or not owner.ready then return end
    owner.pending = true
    if owner.syncing or owner.selecting then return end
    if owner.next_timeline and not owner.restore_timeline then
      owner.select_agent(owner.next_timeline)
      return
    end
    if not owner.refresh_ready then
      if not owner.refresh_timer then
        owner.refresh_timer = vim.defer_fn(function()
          owner.refresh_timer = nil
          if not alive() then return end
          owner.refresh_ready = true
          owner.sync()
        end, 67)
      end
      return
    end
    owner.refresh_ready = nil
    owner.pending, owner.syncing = false, true
    request({ operation = "sync", document = identity, revision = owner.transcript.revision }, function(result, failure)
      if not alive() then owner.syncing = false return end
      if failure then
        owner.syncing = false
        if owner.sync_failure ~= failure then
          owner.sync_failure = failure
          notice(failure)
        end
        return
      end
      owner.sync_failure = nil
      owner.applying = true
      local function complete()
        owner.applying, owner.syncing = false, false
        local switched_timeline = owner.restore_timeline ~= nil
        if owner.restore_timeline then
          for window, saved in pairs(owner.timeline_view[owner.restore_timeline] or {}) do
            if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer then
              vim.api.nvim_win_call(window, function() vim.fn.winrestview(saved) end)
            end
          end
          owner.restore_timeline = nil
        end
        if vim.api.nvim_get_current_buf() ~= options.transcript_buffer then owner.follow_tail() end
        render_markdown(switched_timeline)
        if options.on_update then options.on_update() end
        if result.syntax_pending then owner.highlight() end
        if owner.pending then owner.sync() end
        dispatch_next()
      end
      local snapshot = type(result.snapshot) == "table" and result.snapshot or nil
      local update = snapshot or { patch = result.patch or {} }
      if not snapshot and #update.patch == 0 then complete() return end
      if not snapshot and #update.patch == 1 then update = update.patch[1] end
      replica.apply_async(owner.transcript, update, alive, function(applied)
          if not alive() then owner.applying, owner.syncing = false, false return end
          if applied.kind ~= "Applied" then
            owner.applying, owner.syncing = false, false
            notice("Harness transcript update failed: " .. tostring(applied.diagnostic or applied.kind))
            dispatch_next()
            return
          end
          complete()
      end)
    end)
  end

  function owner.highlight()
    if not alive() or not owner.ready or owner.highlighting then return end
    owner.highlighting = true
    client.request_for(options.session_id, "harness.document", { operation = "highlight", document = identity }, function(_, failure)
      owner.highlighting = false
      if not alive() then return end
      if failure then notice(failure) end
      owner.sync()
    end)
  end

  ---@return boolean
  function owner.toggle_tool()
    if not alive() or not owner.ready then return false end
    local view = action_view()
    if not view then return false end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return true end
    if not captured.target or not captured.target:match(":tool$") then return false end
    request({ operation = "toggle_tool", input = captured }, function(_, action_error)
      if not alive() then return end
      if action_error then notice(action_error) else owner.sync() end
    end)
    return true
  end

  function owner.activate(callback)
    if not alive() or not owner.ready then return end
    local view = action_view()
    if not view then return end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return end
    if not captured.target then return end
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
    render_markdown(true)
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

  ---@param run_id string?
  function owner.select_agent(run_id)
    if not alive() or not owner.ready then return end
    local target = run_id or "main"
    owner.next_timeline, owner.pending = target, true
    if owner.syncing or owner.selecting then return end
    owner.next_timeline = nil
    if target == owner.timeline_key then owner.sync() return end
    ---@type table<integer, table>
    local saved = {}
    for window in pairs(owner.views) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer then
        saved[window] = vim.api.nvim_win_call(window, vim.fn.winsaveview)
      end
    end
    owner.timeline_view[owner.timeline_key] = saved
    owner.selecting = true
    request({ operation = "select_agent", document = identity, run_id = target ~= "main" and target or vim.NIL }, function(_, failure)
      owner.selecting = false
      if not alive() then return end
      if failure then notice(failure) return end
      owner.timeline_key, owner.restore_timeline = target, target
      owner.sync()
    end)
  end

  ---Resume transcript tail following for an explicit user action without changing focus.
  function owner.follow_tail()
    if not alive() or not owner.ready then return end
    local last_row = vim.api.nvim_buf_line_count(owner.transcript.buffer)
    for window in pairs(owner.views) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == owner.transcript.buffer then
        vim.api.nvim_win_set_cursor(window, { last_row, 0 })
        vim.api.nvim_win_call(window, function() vim.cmd("normal! $zb") end)
      end
    end
  end

  function owner.submit(callback)
    if not alive() or not owner.ready then callback(nil, "Harness documents are not ready") return end
    if owner.submission then callback(nil, "Composer submission is already active") return end
    local row_count = vim.api.nvim_buf_line_count(options.composer_buffer)
    if row_count > 4096 or vim.api.nvim_buf_get_offset(options.composer_buffer, row_count) - 1 > 65536 then
      callback(nil, "Prompt exceeds 64 KiB or 4096 rows")
      return
    end
    local source = vim.api.nvim_buf_get_lines(options.composer_buffer, 0, -1, false)
    local text = table.concat(source, "\n")
    if not text:find("%S") then
      callback(nil, "Prompt requires nonempty text within 64 KiB and 4096 rows")
      return
    end
    owner.submission_sequence = owner.submission_sequence + 1
    local submission = { token = owner.submission_sequence, source = source,
      changedtick = vim.api.nvim_buf_get_changedtick(options.composer_buffer) }
    owner.submission = submission
    owner.follow_tail()
    client.request_for(options.session_id, "prompt.submit", {
      text = text, submission = { document = identity, token = submission.token },
    }, function(result, failure, error_detail)
      if owner.closed or owner.host_generation ~= client.host_generation() or owner.submission ~= submission then return end
      owner.submission = nil
      callback(result, failure, error_detail)
    end)
  end

  function owner.receive(event)
    if not alive() or not owner.ready or event.kind ~= "prompt_submission" then return false end
    local transition = event.data
    local submission = owner.submission
    if type(transition) ~= "table" or transition.document ~= identity or not submission
        or transition.token ~= submission.token or not vim.api.nvim_buf_is_valid(options.composer_buffer) then return false end
    local changedtick = vim.api.nvim_buf_get_changedtick(options.composer_buffer)
    if transition.state == "accepted" and not submission.accepted then
      submission.accepted = true
      if changedtick == submission.changedtick then
        vim.api.nvim_buf_set_lines(options.composer_buffer, 0, -1, false, { "" })
        submission.cleared_tick = vim.api.nvim_buf_get_changedtick(options.composer_buffer)
      end
    elseif transition.state == "retracted" and submission.cleared_tick then
      if changedtick == submission.cleared_tick then
        vim.api.nvim_buf_set_lines(options.composer_buffer, 0, -1, false, submission.source)
      end
      submission.cleared_tick = nil
    end
    return true
  end

  function owner.close(close_options)
    if owner.closed then return true end
    local collected = owner.host_generation ~= client.host_generation()
      or not vim.api.nvim_buf_is_valid(options.composer_buffer) or not vim.api.nvim_buf_is_valid(options.transcript_buffer)
    owner.closed = true
    if owner.refresh_timer then
      owner.refresh_timer:stop()
      owner.refresh_timer:close()
      owner.refresh_timer = nil
    end
    if owner.terminals then owner.terminals.close() end
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    for _, output in ipairs(owner.output) do output.close() end
    require("forge.views.harness.status_hint").clear(options.transcript_buffer)
    owner.output = {}
    for _, view in pairs(owner.views) do input.close(view) end
    owner.views = {}
    if collected then replica.invalidate(owner.transcript) else replica.close(owner.transcript, close_options) end
    if not collected and not (close_options and close_options.preserve_buffer) then
      vim.api.nvim_buf_delete(options.composer_buffer, { force = true })
    end
    request({ operation = "close", document = identity }, function(_, failure)
      if failure then notice(failure) end
    end)
    return true
  end
  owner.group = vim.api.nvim_create_augroup("ForgeHarnessPresentation" .. tostring(vim.uv.hrtime()), { clear = true })
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, { group = owner.group, callback = owner.resize })
  local markdown_scheduled = false
  vim.api.nvim_create_autocmd({ "WinScrolled", "CursorMoved", "CursorMovedI", "BufWinEnter", "WinClosed" }, {
    group = owner.group, callback = function()
      if markdown_scheduled then return end
      markdown_scheduled = true
      vim.schedule(function() markdown_scheduled = false render_markdown(false) end)
    end,
  })
  vim.api.nvim_create_autocmd({ "BufWinEnter", "BufWinLeave", "WinClosed" }, {
    group = owner.group, callback = function() vim.schedule(owner.refresh_views) end,
  })
  vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
    group = owner.group, callback = function()
      vim.schedule(function()
        if alive() and owner.ready and vim.api.nvim_get_current_buf() ~= options.transcript_buffer then
          owner.follow_tail()
        end
      end)
    end,
  })
  for _, buffer in ipairs({ options.transcript_buffer, options.composer_buffer }) do
    vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = buffer, once = true,
      callback = function() vim.schedule(function() owner.close() end) end })
  end
  return owner
end

return M
