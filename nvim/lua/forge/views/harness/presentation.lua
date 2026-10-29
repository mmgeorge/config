local M = {}
local client = require("forge.client")
local replica = require("forge.buffer")
local input = require("forge.input")
local perf = require("forge.infra.perf")
local diagnostic = require("forge.views.harness.diagnostic")
local refresh_interval_ns = 1e9 / 60
local transcript_options = {
  margin = 0,
  scrolloff = 3,
    fold_markers = true,
  conceal = { level = 3, cursor = "nvic" },
  columns = { signcolumn = "yes:1", statuscolumn = "%s" },
  wrapping = { indent = true, options = "shift:0" },
}

function M.open(options, callback)
  local startup = options.startup_trace
  if startup then startup.mark("document.open.begin") end
  local loading_namespace = vim.api.nvim_create_namespace("ForgeHarnessLoading" .. options.transcript_buffer)
  local loading_mark = {}
  local function clear_loading()
    if vim.api.nvim_buf_is_valid(options.transcript_buffer) then
      vim.api.nvim_buf_clear_namespace(options.transcript_buffer, loading_namespace, 0, -1)
    end
    loading_mark = {}
  end
  local identity = "harness:" .. options.session_id .. ":" .. tostring(vim.uv.hrtime())
  ---@class HarnessPresentation
  ---@field status_view HarnessStatusView
  local owner = { document = identity, closed = false, syncing = false, pending = false, output = {},
    session_id = options.session_id, host_generation = client.host_get_generation(), views = {},
    timeline_key = "main", timeline_view = {}, epoch = 0,
    staging = { queue = {}, steering = {} }, applied_staging = { queue = {}, steering = {} } }
  local function alive()
    return not owner.closed and owner.host_generation == client.host_get_generation()
      and client.host_is_accepting() and options.is_alive()
  end
  local function notice(message)
    if options.notice then options.notice(message) end
  end
  local queued, active = {}, false
  local flush_staging
  local staging_reveal, staging_revealed
  local function cancel_staging_reveal()
    local pending = staging_reveal
    staging_reveal = nil
    if pending then pending.timer:stop() pending.timer:close() end
  end
  local function fail(message)
    if startup then startup.finish("failed", { error = tostring(message) }) end
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
    local changedtick = vim.api.nvim_buf_get_changedtick(options.transcript_buffer)
    -- Appending outside a message can extend Tree-sitter's included region at its boundary.
    if not force and owner.markdown_tick == changedtick and vim.deep_equal(owner.markdown_ranges, range_list)
      and vim.deep_equal(owner.markdown_windows, window_list) then return end
    owner.markdown_ranges, owner.markdown_windows = range_list, window_list
    owner.markdown_tick = changedtick
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
    local function receive(result, failure, detail)
      if completed then return end
      completed = true
      if deadline then deadline:stop() deadline:close() deadline = nil end
      local operation = current.params.operation
      local code = detail and detail.code
      local read = operation == "sync" or operation == "snapshot" or operation == "open"
      local retryable = code == "not_admitted"
        or (read and code ~= "invalid_state" and code ~= "invalid_request" and code ~= -32602)
        or ((not detail or code == "outcome_unknown")
          and (operation == "staging" or operation == "node" or operation == "select_agent"))
      if failure and retryable and valid_request(current) then
        diagnostic.set(owner, "sync_failure", "Retrying transcript " .. operation .. ": " .. tostring(failure),
          { owner = identity, category = "presentation", severity = "info" })
        if options.on_update then options.on_update() end
        table.insert(queued, 1, current)
        vim.defer_fn(function() active = false dispatch_next() end, 1000)
        return
      end
      if not failure then diagnostic.set(owner, "sync_failure", nil) end
      perf.event("harness", "ui.request", { phase = "end", session_id = options.session_id,
        operation = current.params.operation, revision = current.params.revision,
        elapsed_ms = perf.elapsed_ms(started), status = failure and "error" or "ok" })
      local accepted, callback_error = pcall(perf.trace, "harness", "ui.response", {
        session_id = options.session_id, operation = current.params.operation,
        revision = current.params.revision }, function()
          if valid_request(current) then current.done(result, failure, detail)
          elseif current.cancel then current.cancel() end
        end)
      active = false
      if not accepted and alive() then fail("Presentation callback failed: " .. tostring(callback_error)) end
      if failure and code == "outcome_unknown" and alive() and owner.ready then owner.sync() end
      dispatch_next()
    end
    deadline = vim.defer_fn(function()
      if completed then return end
      receive(nil, "Presentation response is delayed", { code = "outcome_unknown" })
    end, 30000)
    if owner.host_generation ~= client.host_get_generation() or not client.host_is_accepting() then
      receive(current.params.operation == "close" and {} or nil, current.params.operation ~= "close" and "Harness host generation changed" or nil)
    elseif options.request then options.request(current.params, receive)
    else client.request({ session_id = options.session_id, method = "session.document", params = current.params }, receive) end
  end
  local function request(params, done, valid, cancel)
    if owner.host_generation ~= client.host_get_generation() or not client.host_is_accepting() then
      done(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
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
      if document == owner.status_view.native then
        owner.status_view:apply(snapshot, function(applied)
          if applied and options.on_update then options.on_update() end
        end)
        return
      end
      owner.applying = true
      replica.apply_async(document, snapshot, alive, guard(function(result)
        owner.applying = false
        if not alive() then return end
        if result.kind ~= "Applied" then
          fail("Harness snapshot could not be adopted: " .. tostring(result.kind))
        else
          flush_staging()
          if options.on_update then options.on_update() end
        end
        dispatch_next()
        if owner.next_timeline then owner.sync() end
      end))
    end)
  end
  local transcript_tick = vim.api.nvim_buf_get_changedtick(options.transcript_buffer)
  local function reject_open(message)
    owner.closed = true
    if owner.status_view then owner.status_view:close() end
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
  owner.status_view = require("forge.views.harness.status_view").new({
    transcript = owner.transcript, transcript_window = options.transcript_window,
    composer_window = options.composer_window, alive = alive, notice = notice, recover = recovery,
  })
  owner.selection = require("forge.document_commands").attach_selection(owner.transcript, { normalize = false })
  owner.transcript.fold_loading = {}
  owner.view = input.open(owner.transcript, options.transcript_window, transcript_options)
  owner.views[options.transcript_window] = owner.view
  flush_staging = function()
    if not alive() or not owner.ready or owner.failure or owner.selecting or owner.applying
      or staging_reveal or owner.staging_inflight or vim.deep_equal(owner.staging, owner.applied_staging) then return end
    local pending = vim.deepcopy(owner.staging)
    owner.staging_inflight = true
    owner.staging_sequence = (owner.staging_sequence or 0) + 1
    request({ operation = "staging", document = identity, sequence = owner.staging_sequence,
      queue = pending.queue, steering = pending.steering }, function(_, failure)
      owner.staging_inflight = false
      if failure then fail("Staging update failed: " .. tostring(failure)) return end
      owner.applied_staging = pending
      flush_staging()
      owner.sync()
    end, nil, function()
      owner.staging_inflight = false
      owner.applied_staging = nil
    end)
  end

  ---Replace transient staged messages without changing the composer or durable history.
  ---@param queue (string|{id?: string, text: string, failed?: boolean})[]?
  ---@param steering {id?: string, text: string, failed?: boolean}[]?
  function owner.set_staging(queue, steering)
    if not alive() then return end
    local staged = { queue = {}, steering = {} }
    for _, entry in ipairs(queue or {}) do
      staged.queue[#staged.queue + 1] = type(entry) == "table" and entry.id
        and { id = entry.id, text = entry.text, failed = entry.failed == true } or type(entry) == "table" and entry.text or entry
    end
    for _, entry in ipairs(steering or {}) do
      staged.steering[#staged.steering + 1] = entry.id
        and { id = entry.id, text = entry.text, failed = entry.failed == true } or entry.text or ""
    end
    owner.staging = staged
    local immediate = #staged.queue > 0 or #staged.steering == 0
    for _, entry in ipairs(staged.steering) do
      if type(entry) == "table" and entry.failed then immediate = true end
    end
    if immediate then
      cancel_staging_reveal()
      staging_revealed = #staged.queue > 0 or #staged.steering > 0
    elseif not staging_revealed and not staging_reveal then
      local pending = {}
      staging_reveal = pending
      pending.timer = vim.defer_fn(function()
        if staging_reveal ~= pending then return end
        staging_reveal = nil
        if not alive() then return end
        staging_revealed = true
        flush_staging()
      end, 100)
    end
    flush_staging()
  end

  request({ operation = "open", document = identity, view = owner.view.id,
    width = require("forge.width").capture(options.transcript_window) }, function(opened, failure)
    if startup then startup.mark("document.open.received", { error = failure, rust = opened and opened.startup_timing }) end
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
    if startup then startup.mark("document.apply.end", { result = transcript.kind }) end
    owner.applying = false
    if not alive() then return end
    if transcript.kind ~= "Applied" then
      reject_open("Harness native documents could not be adopted")
      return
    end
    owner.status_view:apply(opened.status, guard(function(applied)
    if not applied then reject_open("Harness status could not be adopted") return end
    owner.ready = true
    flush_staging()
    render_markdown()
    vim.schedule(function() if alive() then owner.observe_sections() end end)
    if not options.read_only then owner.terminals = require("forge.views.harness.terminals").watch({
      session_id = options.session_id, alive = alive, notice = notice,
      update = function(snapshot)
        owner.status_view.context.background_terminals = snapshot
        owner.status_view:render()
        if options.on_update then options.on_update() end
      end,
    }) end
    vim.bo[options.composer_buffer].modifiable = not options.read_only
    callback(owner)
    if vim.api.nvim_get_current_buf() ~= options.transcript_buffer then owner.follow_tail() end
    if opened.syntax_pending then vim.schedule(function() owner.highlight() end) end
    if startup then startup.mark("document.ready") startup.watch(options.transcript_buffer) end
    dispatch_next()
    end))
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
    local node = section_state(id)
    local cursor = node and table.concat({ tostring(node.generation), tostring(node.loaded_rows), tostring(node.loaded_bytes) }, ":") or id
    local _, row = owner.transcript.sequence:position(boundary_id)
    if not row or row == 0 then return cursor .. ":empty" end
    local previous = replica.locate(owner.transcript, row - 1, 0)
    if not previous then return cursor .. ":empty" end
    local text = vim.api.nvim_buf_get_lines(owner.transcript.buffer, row - 1, row, false)[1] or ""
    return cursor .. ":" .. previous.block .. ":" .. previous.position.row .. ":" .. text
  end

  local function section_pending(id)
    for _, pending in pairs(owner.section_inflight or {}) do
      if pending[id] then return true end
    end
    return false
  end

  local function section_failed(id, message)
    owner.section_failure = owner.section_failure or {}
    owner.section_failure[id] = tostring(message)
    local node = section_state(id)
    owner.section_failure_revision = owner.section_failure_revision or {}
    owner.section_failure_revision[id] = node and tostring(node.generation) .. ":" .. tostring(node.content_revision)
    local header = owner.transcript.node_owner[id]
    local _, row = owner.transcript.sequence:position(header)
    if row then
      if loading_mark[id] then vim.api.nvim_buf_del_extmark(options.transcript_buffer, loading_namespace, loading_mark[id]) end
      loading_mark[id] = vim.api.nvim_buf_set_extmark(options.transcript_buffer, loading_namespace, row, 0,
        { virt_text = { { " Section unavailable · reopen to retry", "Comment" } }, virt_text_pos = "eol" })
    end
    if options.on_update then options.on_update() end
  end

  local function publish_openings()
    for _, requests in pairs(owner.section_inflight or {}) do
      for id, pending in pairs(requests) do
        if pending.ready then
          local node = section_state(id)
          if node and node.generation ~= pending.generation then
            requests[id] = nil
            local intent = owner.section_intent and owner.section_intent[pending.view]
            for window, view in pairs(owner.views) do
              if view.id == pending.view and (not intent or intent[id] ~= false) then
                local opening = owner.transcript.fold_loading[window]
                if opening and opening[id] then opening[id].ready = false end
                vim.schedule(function()
                  local current = owner.section_request and owner.section_request[view.id]
                  if alive() and current and current[id] == pending.sequence then owner.set_section(id, true, false, view) end
                end)
              end
            end
          elseif pending.more and pending.boundary and page_boundary(id) == pending.boundary then
            local node = section_state(id)
            local observation = node and tostring(node.generation) .. ":" .. tostring(node.content_revision)
            if observation and pending.observation ~= observation then
              pending.observation = observation
              owner.pending = true
            else
              section_failed(id, "The current page is unchanged. Reopen the section to retry.")
              requests[id] = nil
            end
          else
            requests[id] = nil
          end
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
          local intent = owner.section_intent and owner.section_intent[view.id]
          local observation = tostring(section.generation) .. ":" .. tostring(section.content_revision)
          if not intent or not intent[id] then clear_opening(window, id)
          elseif pending.observation ~= observation then
            pending.observation = observation
            owner.pending = true
          else
            clear_opening(window, id)
            section_failed(id, "Section content is unavailable. Reopen the section to retry.")
          end
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
        local deadline = vim.uv.hrtime() + refresh_interval_ns
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
        owner.refresh_timer = vim.defer_fn(publish, math.ceil(refresh_interval_ns / 1e6))
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
        render_markdown(switched_timeline)
        owner.observe_sections()
        if options.on_update then options.on_update() end
        if result.syntax_pending then owner.highlight() end
        if owner.pending then owner.sync() end
        dispatch_next()
      end
      local function complete_status()
        owner.status_view:apply(result.status, guard(function() complete() end))
      end
      local snapshot = type(result.snapshot) == "table" and result.snapshot or nil
      local update = snapshot or { patch = result.patch or {} }
      if not snapshot and #update.patch == 0 then
        owner.transcript.before_commit()
        complete_status()
        return
      end
      if not snapshot and #update.patch == 1 then update = update.patch[1] end
      replica.apply_async(owner.transcript, update, alive, guard(function(applied)
          if not alive() then owner.applying, owner.syncing = false, false return end
          if applied.kind ~= "Applied" then
            owner.applying, owner.syncing = false, false
            reject_openings()
            recovery(owner.transcript)
            dispatch_next()
            return
          end
          complete_status()
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
      if retry and loading_mark[section] then
        vim.api.nvim_buf_del_extmark(options.transcript_buffer, loading_namespace, loading_mark[section])
        loading_mark[section] = nil
      end
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
      generation = node.generation, more = more, boundary = more and page_boundary(section) or nil }
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
            if located and block and not visited[located.block] then
              visited[located.block] = true
              for _, section in ipairs(block.metadata.section or {}) do
                if admitted >= 8 then return end
                local loading = owner.transcript.fold_loading[window]
                local active = section_pending(section.id)
                local failed = owner.section_failure and owner.section_failure[section.id]
                local current_node = section_state(section.id)
                if failed and current_node and owner.section_failure_revision
                  and owner.section_failure_revision[section.id] ~= tostring(current_node.generation) .. ":" .. tostring(current_node.content_revision) then
                  owner.section_failure[section.id], owner.section_page[section.id] = nil, nil
                  failed = false
                end
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
    if not block or not node or node == vim.NIL then return false end
    if #(block.metadata.fold or {}) == 0
      and (node.kind == "message" or node.kind == "thought" and node.default_display == "full") then
      return false
    end
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
    owner.status_view:resize()
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

  function owner.status_action(callback)
    if not alive() or not owner.ready then return end
    local view = action_view()
    local snapshot = owner.status_view.snapshot
    if not view or not snapshot then return end
    local target
    for _, block in ipairs(snapshot.block) do
      local status = block.metadata.status
      if type(status) == "table" and status.hint == "review" then
        local candidate = block.metadata.target and block.metadata.target[1]
        if candidate then target = candidate.id end
      end
    end
    if not target then return end
    view.sequence = view.sequence + 1
    request({ operation = "status_action", document = identity, status = snapshot.document,
      revision = snapshot.revision, target = target, view = view.id, sequence = view.sequence }, function(action, failure)
      if failure then notice(failure) return end
      callback(action)
    end)
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
    owner.selection_sequence = (owner.selection_sequence or 0) + 1
    request({ operation = "select_agent", document = identity, sequence = owner.selection_sequence,
      run_id = target ~= "main" and target or vim.NIL }, function(_, failure)
      owner.selecting = false
      if not alive() then return end
      if failure then notice(failure) flush_staging() return end
      owner.save_timeline = owner.timeline_key
      owner.timeline_key, owner.restore_timeline = target, target
      flush_staging()
      owner.sync()
    end)
  end

  ---Position the transcript at its tail once for an explicit action without changing focus.
  function owner.follow_tail()
    if not alive() or not owner.ready then return end
    for window in pairs(owner.views) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == owner.transcript.buffer then
        vim.api.nvim_win_set_cursor(window, { vim.api.nvim_buf_line_count(owner.transcript.buffer), 0 })
        vim.api.nvim_win_call(window, function() vim.cmd("normal! $zb") end)
      end
    end
  end

  ---Capture the current draft's view identity without owning its request lifetime.
  ---@return ForgePromptContext? context
  ---@return string? failure
  function owner.capture_prompt()
    if options.read_only then return nil, "This timeline is read-only" end
    if not alive() or not owner.ready then return nil, "Harness documents are not ready" end
    if owner.failure then return nil, owner.failure end
    owner.follow_tail()
    return { document = identity, buffer = options.composer_buffer, is_current = alive }
  end

  function owner.close(close_options)
    if owner.closed then return true end
    if startup then startup.finish("closed_before_redraw") end
    cancel_staging_reveal()
    local collected = owner.host_generation ~= client.host_get_generation()
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
    owner.status_view:close()
    owner.output = {}
    for _, view in pairs(owner.views) do input.close(view) end
    owner.views = {}
    if collected then replica.invalidate(owner.transcript) else replica.close(owner.transcript, close_options) end
    if not collected and not (close_options and close_options.preserve_buffer) then
      vim.api.nvim_buf_delete(options.composer_buffer, { force = true })
    end
    request({ operation = "close", document = identity }, function(_, failure)
      if failure and owner.host_generation == client.host_get_generation() and client.host_is_accepting() then notice(failure) end
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
  for _, buffer in ipairs({ options.transcript_buffer, options.composer_buffer }) do
    vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = buffer, once = true,
      callback = function() vim.schedule(function() owner.close() end) end })
  end
  return owner
end

return M
