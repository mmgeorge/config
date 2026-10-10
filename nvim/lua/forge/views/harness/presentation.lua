local M = {}
local client = require("forge.client")
local replica = require("forge.buffer")
local input = require("forge.input")
local perf = require("forge.infra.perf")
local transcript_options = {
  margin = 0,
  scrolloff = 3,
    fold_markers = true,
  conceal = { level = 3, cursor = "nvic" },
  columns = { signcolumn = "yes:1", statuscolumn = "%s" },
  wrapping = { indent = true, options = "shift:0" },
}

function M.open(options, callback)
  local loading_namespace = vim.api.nvim_create_namespace("ForgeHarnessLoading" .. options.transcript_buffer)
  local loading_mark = {}
  local function clear_loading()
    if vim.api.nvim_buf_is_valid(options.transcript_buffer) then
      vim.api.nvim_buf_clear_namespace(options.transcript_buffer, loading_namespace, 0, -1)
    end
    loading_mark = {}
  end
  local identity = "harness:" .. options.session_id .. ":" .. tostring(vim.uv.hrtime())
  local owner = { document = identity, submission_sequence = 0, closed = false, syncing = false, pending = false, output = {},
    session_id = options.session_id, host_generation = client.host_generation(), views = {},
    timeline_key = "main", timeline_view = {}, epoch = 0 }
  local function alive()
    return not owner.closed and owner.host_generation == client.host_generation()
      and client.host_accepting() and options.is_alive()
  end
  local function notice(message)
    if options.notice then options.notice(message) end
  end
  local queued, active = {}, false
  local function fail(message)
    local previous = owner.failure
    owner.failure = tostring(message) .. ". Reopen Harness to retry."
    owner.pending, owner.syncing, owner.applying, owner.highlighting, owner.selecting = false, false, false, false, false
    owner.section_inflight = {}
    clear_loading()
    if owner.transcript then owner.transcript.fold_loading = {} end
    if previous ~= owner.failure then notice(owner.failure) end
    if options.on_update then pcall(options.on_update) end
    if not owner.ready and not owner.open_failed then
      owner.open_failed = true
      pcall(callback, nil, owner.failure)
      vim.schedule(function() if owner.close then owner.close() end end)
    end
  end
  local function guard(callback)
    local epoch = owner.epoch
    return function(...)
      if not alive() or owner.epoch ~= epoch then return end
      local ok, failure = pcall(callback, ...)
      if not ok then fail("Presentation callback failed: " .. tostring(failure)) end
    end
  end
  local function cleanup_request(params)
    return params.operation == "close" or params.operation == "close_view"
  end
  local function lifetime_request(params)
    return cleanup_request(params) or params.operation == "open_view" or params.operation == "resize"
  end
  local function valid_request(current)
    if cleanup_request(current.params) then return true end
    if not alive() or owner.failure then return false end
    if current.epoch ~= owner.epoch and not lifetime_request(current.params) then return false end
    if current.valid and not current.valid() then return false end
    local view_id = current.params.view or (current.params.input and current.params.input.view)
    if view_id then
      for _, view in pairs(owner.views) do
        if view.id == view_id and view.active and vim.api.nvim_win_is_valid(view.window)
          and vim.api.nvim_win_get_buf(view.window) == options.transcript_buffer then return true end
      end
      return false
    end
    return true
  end
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
    while #queued > 0 and not valid_request(queued[1]) do
      local discarded = table.remove(queued, 1)
      if discarded.cancel then discarded.cancel() end
    end
    if #queued == 0 then return end
    active = true
    local current = table.remove(queued, 1)
    local completed = false
    local deadline
    local started = perf.now()
    perf.event("harness", "ui.request", { phase = "begin", session_id = options.session_id,
      operation = current.params.operation, revision = current.params.revision,
      queue_count = #queued, elapsed_ms = perf.elapsed_ms(current.queued_at) })
    local function receive(result, failure)
      if completed then return end
      completed = true
      if deadline then deadline:stop() deadline:close() deadline = nil end
      perf.event("harness", "ui.request", { phase = "end", session_id = options.session_id,
        operation = current.params.operation, revision = current.params.revision,
        elapsed_ms = perf.elapsed_ms(started), status = failure and "error" or "ok" })
      local accepted, callback_error = pcall(perf.trace, "harness", "ui.response", {
        session_id = options.session_id, operation = current.params.operation,
        revision = current.params.revision }, function()
          if valid_request(current) then current.done(result, failure)
          elseif current.cancel then current.cancel() end
        end)
      active = false
      if not accepted and alive() then fail("Presentation callback failed: " .. tostring(callback_error)) end
      dispatch_next()
    end
    deadline = vim.defer_fn(function()
      if completed then return end
      if alive() and current.epoch == owner.epoch then
        fail("Presentation request timed out (" .. current.params.operation .. "); its outcome is unknown")
      end
      receive(nil, "Presentation request timed out")
    end, 30000)
    if owner.host_generation ~= client.host_generation() or not client.host_accepting() then
      receive(current.params.operation == "close" and {} or nil, current.params.operation ~= "close" and "Harness host generation changed" or nil)
    else client.request_for(options.session_id, "harness.document", current.params, receive) end
  end
  local function request(params, done, valid, cancel)
    if owner.host_generation ~= client.host_generation() or not client.host_accepting() then
      done(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
    if #queued >= 63 and params.operation ~= "close" then done(nil, "Harness presentation request capacity is full") return end
    queued[#queued + 1] = { params = params, done = done, queued_at = perf.now(), epoch = owner.epoch,
      valid = valid, cancel = cancel }
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
  local function current_input(view, captured, semantic)
    if not alive() or owner.failure or owner.transcript.status ~= "Applied"
      or owner.views[view.window] ~= view or view.sequence ~= captured.sequence
      or not vim.api.nvim_win_is_valid(view.window)
      or vim.api.nvim_win_get_buf(view.window) ~= owner.transcript.buffer then return false end
    local cursor = vim.api.nvim_win_get_cursor(view.window)
    local location = replica.locate(owner.transcript, cursor[1] - 1, cursor[2])
    if not location or location.block ~= captured.block or location.target ~= captured.target then return false end
    return semantic or (owner.transcript.revision == captured.revision
      and vim.deep_equal(location.position, captured.position))
  end
  local function recovery(document)
    request({ operation = "snapshot", document = document.document }, function(snapshot, failure)
      if not alive() then return end
      if failure then fail("Transcript recovery failed: " .. tostring(failure)) return end
      owner.applying = true
      replica.apply_async(document, snapshot, alive, guard(function(result)
        owner.applying = false
        if not alive() then return end
        if result.kind ~= "Applied" then
          fail("Harness snapshot could not be adopted: " .. tostring(result.kind))
        elseif vim.api.nvim_get_current_buf() ~= options.transcript_buffer then
          owner.follow_tail()
        end
        dispatch_next()
        if owner.next_timeline then owner.sync() end
      end))
    end)
  end
  local transcript_tick = vim.api.nvim_buf_get_changedtick(options.transcript_buffer)
  local function reject_open(message)
    owner.closed = true
    if owner.selection then owner.selection.close() end
    markdown.clear(options.transcript_buffer)
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    if owner.view then input.close(owner.view) end
    if owner.transcript and not owner.transcript.generated_owned then replica.close(owner.transcript) end
    request({ operation = "close", document = identity }, function() end)
    callback(nil, message)
  end
  owner.transcript = replica.open(identity, { buffer = options.transcript_buffer, generated = true, preserve_view = true, source_projected = true,
    expected_changedtick = transcript_tick, filetype = "ForgeHarness", notice = notice,
    before_commit = function()
      if not owner.save_timeline then return end
      local saved = {}
      for _, window in ipairs(vim.fn.win_findbuf(options.transcript_buffer)) do
        saved[window] = vim.api.nvim_win_call(window, vim.fn.winsaveview)
      end
      owner.timeline_view[owner.save_timeline] = saved
      owner.save_timeline = nil
    end,
    recover = function() recovery(owner.transcript) end })
  owner.selection = require("forge.document_commands").attach_selection(owner.transcript, { normalize = false })
  owner.transcript.fold_loading = {}
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
    replica.apply_async(owner.transcript, opened.transcript, alive, guard(function(transcript)
    owner.applying = false
    if not alive() then return end
    if transcript.kind ~= "Applied" then
      reject_open("Harness native documents could not be adopted")
      return
    end
    owner.ready = true
    render_markdown()
    vim.schedule(function() if alive() then owner.observe_sections() end end)
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
    end))
  end)

  local function section_state(id)
    local header = owner.transcript.node_owner[id]
    local block = header and owner.transcript.block[header]
    local node = block and block.metadata.node
    if node and node ~= vim.NIL then return node end
  end

  local function clear_opening(window, id)
    local opening = owner.transcript.fold_loading[window]
    if opening then
      opening[id] = nil
      if not next(opening) then owner.transcript.fold_loading[window] = nil end
    end
    if loading_mark[id] then
      vim.api.nvim_buf_del_extmark(options.transcript_buffer, loading_namespace, loading_mark[id])
      loading_mark[id] = nil
    end
  end

  local function page_boundary(id)
    local boundary_id = id .. ":deferred-body"
    local block = owner.transcript.block[boundary_id]
    if not block then return nil end
    local more = false
    for _, section in ipairs(block.metadata.section or {}) do
      if section.id == id and section.more then more = true break end
    end
    if not more then return nil end
    local _, row = owner.transcript.sequence:position(boundary_id)
    if not row or row == 0 then return id .. ":empty" end
    local previous = replica.locate(owner.transcript, row - 1, 0)
    if not previous then return id .. ":empty" end
    local text = vim.api.nvim_buf_get_lines(owner.transcript.buffer, row - 1, row, false)[1] or ""
    return previous.block .. ":" .. previous.position.row .. ":" .. text
  end

  local function section_pending(id)
    for _, pending in pairs(owner.section_inflight or {}) do
      if pending[id] then return true end
    end
    return false
  end

  local function section_failed(id, message)
    owner.section_failure = owner.section_failure or {}
    local changed = owner.section_failure[id] ~= tostring(message)
    owner.section_failure[id] = tostring(message)
    owner.section_error = "Section could not be loaded: " .. tostring(message)
    if changed then notice(owner.section_error) end
    if options.on_update then options.on_update() end
  end

  local function publish_openings()
    for _, requests in pairs(owner.section_inflight or {}) do
      for id, pending in pairs(requests) do
        if pending.ready then
          if pending.more and pending.boundary and page_boundary(id) == pending.boundary then
            section_failed(id, "Loading made no progress. Close and reopen the section to retry.")
          end
          requests[id] = nil
        end
      end
    end
    for window, opening in pairs(owner.transcript.fold_loading) do
      local view = owner.views[window]
      for id, pending in pairs(opening) do
        local section = section_state(id)
        if not view or view.id ~= pending.view or not vim.api.nvim_win_is_valid(window)
          or vim.api.nvim_win_get_buf(window) ~= options.transcript_buffer or not section then
          clear_opening(window, id)
        elseif pending.ready and section.display ~= "heading" then
          clear_opening(window, id)
        elseif pending.ready then
          clear_opening(window, id)
          section_failed(id, "The requested content was not published. Reopen the section to retry.")
        end
      end
    end
  end

  local function reject_openings()
    owner.section_inflight = {}
    for window, opening in pairs(owner.transcript.fold_loading) do
      for id in pairs(opening) do clear_opening(window, id) end
    end
  end

  function owner.sync()
    if not alive() or not owner.ready or owner.failure then return end
    owner.pending = true
    if owner.syncing or owner.selecting then return end
    if owner.next_timeline and not owner.restore_timeline then
      owner.select_agent(owner.next_timeline)
      return
    end
    if not owner.refresh_ready then
      if not owner.refresh_timer then
        local deadline = vim.uv.hrtime() + 67000000
        local function publish()
          owner.refresh_timer = nil
          if not alive() then return end
          local remaining = math.ceil((deadline - vim.uv.hrtime()) / 1000000)
          if remaining > 0 then
            vim.uv.update_time()
            owner.refresh_timer = vim.defer_fn(publish, remaining)
            return
          end
          owner.refresh_ready = true
          owner.sync()
        end
        vim.uv.update_time()
        owner.refresh_timer = vim.defer_fn(publish, 67)
      end
      return
    end
    owner.refresh_ready = nil
    owner.pending, owner.syncing = false, true
    request({ operation = "sync", document = identity, revision = owner.transcript.revision }, function(result, failure)
      if not alive() then owner.syncing = false return end
      if failure then
        owner.syncing = false
        reject_openings()
        fail("Transcript synchronization failed: " .. tostring(failure))
        return
      end
      owner.sync_failure = nil
      owner.applying = true
      local function complete()
        owner.applying, owner.syncing = false, false
        publish_openings()
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
        owner.observe_sections()
        if options.on_update then options.on_update() end
        if result.syntax_pending then owner.highlight() end
        if owner.pending then owner.sync() end
        dispatch_next()
      end
      local snapshot = type(result.snapshot) == "table" and result.snapshot or nil
      local update = snapshot or { patch = result.patch or {} }
      if not snapshot and #update.patch == 0 then
        owner.transcript.before_commit()
        complete()
        return
      end
      if not snapshot and #update.patch == 1 then update = update.patch[1] end
      replica.apply_async(owner.transcript, update, alive, guard(function(applied)
          if not alive() then owner.applying, owner.syncing = false, false return end
          if applied.kind ~= "Applied" then
            owner.applying, owner.syncing = false, false
            reject_openings()
            fail("Harness transcript update failed: " .. tostring(applied.diagnostic or applied.kind))
            dispatch_next()
            return
          end
          complete()
      end))
    end)
  end

  function owner.set_section(section, expanded, more, selected_view)
    if not alive() or not owner.ready or owner.failure then return end
    local view = selected_view or action_view()
    if not view then return end
    local node = section_state(section)
    if not node then return end
    owner.section_failure = owner.section_failure or {}
    local retry = expanded and not more and owner.section_failure[section] ~= nil
    if expanded and not more then
      owner.section_failure[section] = nil
      if owner.section_page then owner.section_page[section] = nil end
      local _, failure = next(owner.section_failure)
      owner.section_error = failure and ("Section could not be loaded: " .. failure) or nil
    elseif more and owner.section_failure[section] then return end
    owner.section_intent = owner.section_intent or {}
    local intent = owner.section_intent[view.id] or {}
    owner.section_intent[view.id] = intent
    if not more then intent[section] = expanded end
    owner.section_sequence = (owner.section_sequence or 0) + 1
    local sequence = owner.section_sequence
    owner.section_request = owner.section_request or {}
    local requests = owner.section_request[view.id] or {}
    owner.section_request[view.id] = requests
    requests[section] = sequence
    owner.section_inflight = owner.section_inflight or {}
    local pending = owner.section_inflight[view.id] or {}
    owner.section_inflight[view.id] = pending
    pending[section] = { sequence = sequence, ready = false, view = view.id,
      more = more, boundary = more and page_boundary(section) or nil }
    request({ operation = "node", document = identity, view = view.id,
      sequence = sequence, node = section, generation = node.generation,
      action = more and "load_more" or (retry and "retry_loading" or "set_expansion"), expanded = expanded,
      rows = 2 * vim.api.nvim_win_get_height(view.window),
      width = require("forge.width").capture(view.window) }, function(_, failure)
      if not alive() then return end
      local attached = false
      for _, candidate in pairs(owner.views) do if candidate.id == view.id then attached = true break end end
      if not attached then return end
      if requests[section] ~= sequence then return end
      local active = pending[section]
      if active and active.sequence ~= sequence then return end
      if failure then
        pending[section] = nil
        clear_opening(view.window, section)
        intent[section] = nil
        section_failed(section, failure)
      else
        if active then active.ready = true end
        local opening = owner.transcript.fold_loading[view.window]
        if opening and opening[section] then opening[section].ready = true end
        owner.sync()
      end
    end, function() return requests[section] == sequence end)
  end

  function owner.defer_open(window, id)
    local view = view_for(window)
    if not view then return false end
    local loading = owner.transcript.fold_loading
    local opening = loading[window]
    if opening and opening[id] then
      clear_opening(window, id)
      owner.set_section(id, false, false, view)
      return true
    end
    local section = section_state(id)
    if not section or section.display ~= "heading" then return false end
    opening = opening or {}
    loading[window] = opening
    opening[id] = { view = view.id, ready = false }
    local header = owner.transcript.node_owner[id]
    local _, row = owner.transcript.sequence:position(header)
    if row then
      loading_mark[id] = vim.api.nvim_buf_set_extmark(options.transcript_buffer, loading_namespace, row, 0,
        { virt_text = { { " Loading…", "Comment" } }, virt_text_pos = "eol" })
    end
    owner.set_section(id, true, false, view)
    return true
  end

  function owner.observe_sections()
    if not alive() or not owner.ready or owner.applying or owner.transcript.update_pending or owner.failure then return end
    if vim.api.nvim_get_current_buf() ~= options.transcript_buffer then return end
    owner.section_page = owner.section_page or {}
    local admitted = 0
    for window, view in pairs(owner.views) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == options.transcript_buffer then
        vim.api.nvim_win_call(window, function()
          local first, last = vim.fn.line("w0"), vim.fn.line("w$")
          local height = vim.api.nvim_win_get_height(window)
          local count = vim.api.nvim_buf_line_count(owner.transcript.buffer)
          local row, visited = first, {}
          while row <= count do
            if row > last then
              local distance = vim.api.nvim_win_text_height(window, {
                start_row = last - 1, end_row = row - 1,
              }).all
              if distance > height + 1 then break end
            end
            local closed = -1
            local located = replica.locate(owner.transcript, row - 1, 0)
            local block = located and owner.transcript.block[located.block]
            if block and not visited[located.block] then
              visited[located.block] = true
              for _, section in ipairs(block.metadata.section or {}) do
                if admitted >= 8 then return end
                local loading = owner.transcript.fold_loading[window]
                local active = section_pending(section.id)
                local failed = owner.section_failure and owner.section_failure[section.id]
                if not active and not (loading and loading[section.id]) then
                  local expanded = closed == -1
                  local node = section_state(section.id)
                  if expanded and section.more and node and node.display ~= "heading"
                    and not failed
                    and owner.section_page[section.id] ~= page_boundary(section.id) then
                    owner.section_page[section.id] = page_boundary(section.id)
                    admitted = admitted + 1
                    owner.set_section(section.id, true, true, view)
                  end
                end
              end
            end
            row = row + 1
          end
        end)
      end
    end
  end

  function owner.toggle_heading(window)
    if not alive() or not owner.ready or owner.failure then return false end
    local cursor = vim.api.nvim_win_get_cursor(window)
    local location = replica.locate(owner.transcript, cursor[1] - 1, cursor[2])
    local block = location and owner.transcript.block[location.block]
    local node = block and block.metadata.node
    if (not node or node == vim.NIL) and block and block.metadata.content_node then
      node = section_state(block.metadata.content_node)
    end
    if not node or node == vim.NIL or node.kind == "message" then return false end
    local view = view_for(window)
    if not view then return false end
    local desired = node.display ~= "full"
    if section_pending(node.id) then
      local intent = owner.section_intent and owner.section_intent[view.id]
      if intent and intent[node.id] ~= nil then desired = not intent[node.id] end
    end
    if desired and owner.defer_open(window, node.id) then return true end
    clear_opening(window, node.id)
    owner.set_section(node.id, desired, false, view)
    return true
  end

  function owner.highlight()
    if not alive() or not owner.ready or owner.highlighting or owner.failure then return end
    owner.highlighting = true
    request({ operation = "highlight", document = identity }, function(_, failure)
      owner.highlighting = false
      if not alive() then return end
      if failure then notice(failure) end
      owner.sync()
    end)
  end


  function owner.activate(callback)
    if not alive() or not owner.ready or owner.failure then return end
    local view = action_view()
    if not view then return end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return end
    if not captured.target then return end
    request({ operation = "input", input = captured }, function(action, action_error)
      if not alive() then return end
      if action_error then notice(action_error) return end
      if not current_input(view, captured, action.kind == "tool") then return end
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
        owner.transcript.fold_loading[window] = nil
        if owner.section_intent then owner.section_intent[view.id] = nil end
        if owner.section_request then owner.section_request[view.id] = nil end
        if owner.section_inflight then owner.section_inflight[view.id] = nil end
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
      end, nil, function() owner.resizing = false end)
    end
    send_width()
  end

  function owner.open_output(action, previous_input)
    if not alive() or not owner.ready or (action.kind ~= "tool" and action.kind ~= "diff") then return false end
    local view = action_view(previous_input)
    if not view then return false end
    local captured, failure = input.capture(owner.transcript, view, "activate")
    if not captured then notice(failure) return false end
    local epoch = owner.epoch
    local function current()
      return owner.epoch == epoch and current_input(view, captured, action.kind == "tool")
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
    if not alive() or not owner.ready or owner.failure then return end
    local target = run_id or "main"
    owner.next_timeline, owner.pending = target, true
    if owner.syncing or owner.selecting or owner.applying then return end
    owner.next_timeline = nil
    if target == owner.timeline_key then owner.sync() return end
    owner.epoch = owner.epoch + 1
    owner.section_inflight, owner.section_request, owner.section_intent = {}, {}, {}
    owner.section_failure, owner.section_page = {}, {}
    owner.section_error, owner.highlighting = nil, false
    clear_loading()
    owner.transcript.fold_loading = {}
    owner.selecting = true
    request({ operation = "select_agent", document = identity, run_id = target ~= "main" and target or vim.NIL }, function(_, failure)
      owner.selecting = false
      if not alive() then return end
      if failure then notice(failure) return end
      owner.save_timeline = owner.timeline_key
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
    if owner.failure then callback(nil, owner.failure) return end
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
    owner.selection.close()
    owner.epoch = owner.epoch + 1
    owner.applying = false
    owner.section_inflight = {}
    clear_loading()
    owner.transcript.fold_loading = {}
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
      vim.schedule(function() markdown_scheduled = false render_markdown(false) owner.observe_sections() end)
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
