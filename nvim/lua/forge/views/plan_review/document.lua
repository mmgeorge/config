local M = {}
local client = require("forge.client")
local buffer = require("forge.buffer")
local input = require("forge.input")
local comments = require("forge.draft_comments")
local perf = require("forge.infra.perf")

---@param options table
---@param callback fun(owner: table?, failure: string?)
---@return table
function M.attach(options, callback)
  local owner = { document = "plan:review:" .. vim.uv.hrtime(), generation = client.host_generation(), views = {} }
  local function alive()
    return not owner.closed and owner.generation == client.host_generation() and client.host_accepting()
      and vim.api.nvim_buf_is_valid(options.buffer)
  end
  local function request(params, receive)
    if not alive() then receive(nil, "Plan host is unavailable. The local draft remains in its buffer.") return end
    client.request_for(options.session_id, "harness.document", params, function(result, failure)
      if owner.closed or not vim.api.nvim_buf_is_valid(options.buffer) then return end
      if not alive() then receive(nil, "Plan host was collected. The local draft remains in its buffer.")
      else receive(result, failure) end
    end)
  end
  function owner.attached() return alive() and owner.ready == true end
  function owner.current_view(window)
    if window then return owner.views[window] end
    local current_window = vim.api.nvim_get_current_win()
    return owner.views[current_window] or owner.views[vim.fn.win_findbuf(options.buffer)[1]]
  end
  function owner.is_current(captured, cursor_bound)
    local view
    for _, candidate in pairs(owner.views) do
      if captured and candidate.id == captured.view then view = candidate break end
    end
    return captured and alive() and view and captured.revision == owner.replica.revision
      and captured.document == owner.document and view.active
      and vim.api.nvim_win_is_valid(view.window) and vim.api.nvim_win_get_buf(view.window) == options.buffer
      and view.changedtick == vim.api.nvim_buf_get_changedtick(options.buffer)
      and view.sequence == captured.sequence
      and (cursor_bound == false or vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor))
  end
  function owner.sync_editability()
    if not owner.ready or not owner.comment_state then return end
    local view = owner.current_view()
    if view then
      local position = vim.api.nvim_win_get_cursor(view.window)
      local editable = false
      for _, range in ipairs(owner.comment_state.range_list) do
        if not range.compact and not range.readonly then
          local header = vim.api.nvim_buf_get_extmark_by_id(options.buffer, owner.comment_state.namespace, range.header_mark, {})
          local footer = vim.api.nvim_buf_get_extmark_by_id(options.buffer, owner.comment_state.namespace, range.footer_mark, {})
          editable = editable or #header == 2 and #footer == 2 and position[1] - 1 > header[1] and position[1] - 1 < footer[1]
        end
      end
      vim.bo[options.buffer].modifiable = editable and not options.plan.historical_revision
    end
  end
  function owner.recovery()
    if not owner.ready then return nil, "Plan review is not ready" end
    return { annotation = comments.capture(options.buffer), saved_source_digest = owner.saved_source_digest,
      pending_operation = owner.pending_operation and vim.deepcopy(owner.pending_operation) }
  end
  function owner.close()
    if owner.saving or owner.submission_pending then return false end
    if vim.bo[options.buffer].modified then return true end
    if owner.closed then return true end
    owner.closed = true
    comments.detach(options.buffer, false)
    if owner.gutter_selection then owner.gutter_selection.close() end
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    for _, view in pairs(owner.views) do input.close(view) end
    buffer.close(owner.replica, { preserve_buffer = true })
    client.request_for(options.session_id, "harness.document", { operation = "plan_close", document = owner.document }, function(_, failure)
      if failure and options.notice then options.notice(failure) end
    end)
    return true
  end
  local dispatch_operation
  owner.reply_by_id = {}
  owner.pending_reply_by_id = {}
  local function show_answers(annotation)
    local updates = {}
    for _, item in ipairs(annotation or {}) do
      if item.kind == "question" then
        local id = tostring(item.id)
        local reply = item.reply ~= vim.NIL and item.reply or nil
        reply = reply or owner.reply_by_id[id]
        if reply and reply.question_body ~= item.source.body then reply = nil end
        owner.reply_by_id[id] = reply
        local pending = owner.pending_reply_by_id[id]
        local duration_ms = reply and tonumber(reply.duration_ms)
        local seconds = duration_ms and math.floor(duration_ms / 1000)
        local heading = seconds and ("Thought for %d %s"):format(seconds, seconds == 1 and "second" or "seconds") or "Answered"
        updates[#updates + 1] = reply and { id = item.id, replies_body = reply.question_body,
          replies = { { heading = heading, body_lines = vim.split(reply.body, "\n", { plain = true }) } } }
          or pending and pending.replies_body == item.source.body and pending or { id = item.id, replies = {} }
      end
    end
    if #updates > 0 then comments.update(options.buffer, updates) end
  end
  local function enqueue_operation(operation)
    if owner.saving or owner.submission_pending or owner.completing then
      local previous = owner.pending_operation
      owner.pending_operation = operation
      if previous and previous.receive then previous.receive(nil, "A newer explicit plan operation replaced this queued operation") end
    else dispatch_operation(operation) end
  end
  dispatch_operation = function(operation)
    local function complete(result, failure)
      owner.saving, owner.submission_pending = false, false
      if not failure then comments.saved(options.buffer, operation.capture) end
      owner.completing = true
      local received, callback_failure = pcall(function()
        if operation.receive then operation.receive(result, failure)
        elseif failure and options.notice then options.notice(failure) end
      end)
      owner.completing = false
      local pending = owner.pending_operation
      if pending and alive() then
        owner.pending_operation = nil
        dispatch_operation(pending)
      end
      if not received then error(callback_failure, 0) end
    end
    if operation.method then
      owner.submission_pending = true
      client.request_for(options.session_id, operation.method, operation.params, function(result, failure)
        if owner.generation ~= client.host_generation() or not client.host_accepting() then
          complete(nil, "Plan host is unavailable. The local draft remains in its buffer.")
        else complete(result, failure) end
      end)
    else
      owner.saving = true
      local pending = {}
      for _, annotation in ipairs(operation.capture) do
        local reply = owner.reply_by_id[tostring(annotation.id)]
        if annotation.kind == "question" and vim.trim(annotation.source.body) ~= ""
          and (not reply or reply.question_body ~= annotation.source.body) then
          pending[#pending + 1] = { id = annotation.id, replies_body = annotation.source.body,
            replies = { { heading = "Plan answer", body_lines = { "Answering…" } } } }
        end
      end
      if #pending == 0 then
        request({ operation = "plan_save_annotations", document = owner.document,
          saved_source_digest = owner.saved_source_digest, annotation = operation.capture }, complete)
      else
        local captured, failure = input.capture(owner.replica, owner.current_view(), "plan.questions.answer")
        if not captured then complete(nil, failure) return end
        for _, update in ipairs(pending) do owner.pending_reply_by_id[tostring(update.id)] = update end
        comments.update(options.buffer, pending)
        client.request_for(options.session_id, "plan.questions.answer", { review = captured,
          draft_source_digest = owner.saved_source_digest, draft_annotation = operation.capture }, function(result, error)
          if not alive() then complete(nil, "Plan host is unavailable. The local draft remains in its buffer.") return end
          for _, update in ipairs(pending) do owner.pending_reply_by_id[tostring(update.id)] = nil end
          if error then
            for _, update in ipairs(pending) do update.replies = {} end
            comments.update(options.buffer, pending)
          else show_answers(result.annotation) end
          complete(result, error)
        end)
      end
    end
  end
  function owner.submit(method, params, receive)
    if not owner.attached() then receive(nil, "Plan review is not ready") return false end
    local capture = comments.capture(options.buffer)
    local view = owner.current_view()
    local captured, failure = input.capture(owner.replica, view, method)
    if not captured then receive(nil, failure) return false end
    params = vim.deepcopy(params or {})
    params.review, params.draft_annotation, params.draft_source_digest = captured, capture, owner.saved_source_digest
    enqueue_operation({ method = method, params = params, capture = capture, receive = receive })
    return true
  end
  local function save(capture)
    if not owner.attached() then return end
    enqueue_operation({ capture = capture or comments.capture(options.buffer) })
  end
  local function attach_projection(opened)
    local recovered = options.recovery ~= nil
    local captured = options.recovery and options.recovery.annotation or opened.annotation or {}
    local baseline = {}
    for _, item in ipairs(opened.annotation or {}) do
      baseline[#baseline + 1] = { id = tostring(item.id), kind = item.kind, parent_id = item.parent_id, source = vim.deepcopy(item.source) }
    end
    options.recovery = nil
    local annotation = {}
    for _, item in ipairs(captured) do
      annotation[#annotation + 1] = { id = item.id, source_line = item.source.start_line,
        end_source_line = item.source.end_line, body = item.source.body, kind = item.kind, parent_id = item.parent_id,
        heading = item.kind == "question" and "Plan question" or nil }
    end
    local source = assert(opened.source_row, "Plan review source rows are missing")
    owner.source = source
    local source_lines = {}
    for _, row in ipairs(source) do
      source_lines[#source_lines + 1] = row.text
      row.annotation_anchor = row.target ~= nil and row.target ~= vim.NIL and row.source_line > 0
    end
    local namespace = vim.api.nvim_create_namespace("ForgePlanDraftSource" .. options.buffer)
    local retained_folds, projection_attached
    local function paint(_, _, projection)
      owner.source_generation = (owner.source_generation or 0) + 1
      vim.api.nvim_buf_clear_namespace(options.buffer, namespace, 0, -1)
      owner.projection = projection
      for _, record in ipairs(projection.source_record_list) do
        local row = source[record.source_index]
        local metadata = row.metadata or {}
        for _, decoration_list in ipairs({ metadata.decoration or {} }) do
          for _, decoration in ipairs(decoration_list) do
            local span = decoration.range
            if span.start.row <= row.position.row and span["end"].row >= row.position.row then
              local start = span.start.row == row.position.row and span.start.column or 0
              local finish = span["end"].row == row.position.row and span["end"].column or #row.text
              if finish > start then vim.api.nvim_buf_set_extmark(options.buffer, namespace, record.row, start,
                { end_col = math.min(finish, #row.text), hl_group = decoration.capture, priority = decoration.priority }) end
            end
          end
        end
      end
      if owner.comment_state and options.configure_view then options.configure_view(owner.view, owner) end
      if retained_folds then require("forge.folds").restore(owner.replica, retained_folds) retained_folds = nil end
      local ranges = require("forge.views.plan_review.markdown").ranges(source, projection)
      for _, window in ipairs(vim.fn.win_findbuf(options.buffer)) do
        require("forge.render.harness.markdown").render(options.buffer, window, ranges)
      end
    end
    owner.replica.physical_row = nil
    owner.comment_state = comments.attach(options.buffer, options.window, source_lines, annotation, {
      source_provider = function() return source end, after_render = paint,
      before_render = function()
        if projection_attached then retained_folds = require("forge.folds").capture(owner.replica) end
      end,
      baseline = recovered and baseline or nil,
      readonly = options.plan.historical_revision ~= nil,
      guard_source = true,
    })
    require("forge.draft_source").attach(owner.replica, owner.comment_state, source)
    projection_attached = true
    show_answers(opened.annotation)
  end
  owner.replica = buffer.open(owner.document, { buffer = options.buffer, filetype = "ForgePlan", generated = true, preserve_view = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(options.buffer), notice = options.notice })
  local function open_view(window)
    local columns = require("forge.window_presentation").capture(window)
    columns.number, columns.relativenumber, columns.statuscolumn = false, false, ""
    local view = input.open(owner.replica, window, { columns = columns, virtualedit = "", conceal = { level = 3, cursor = "" },
      wrapping = { indent = columns.breakindent, options = columns.breakindentopt } })
    owner.views[window] = view
    if owner.projection then
      require("forge.render.harness.markdown").render(options.buffer, window,
        require("forge.views.plan_review.markdown").ranges(owner.source, owner.projection))
    end
    if owner.ready and alive() then
      request({ operation = "plan_view", document = owner.document, view = view.id, width = owner.source_width }, function(_, failure)
        if failure and options.notice then options.notice(failure) end
      end)
    end
    return view
  end
  owner.view = open_view(options.window)
  owner.source_width = require("forge.width").capture(options.window)
  local started = perf.now()
  request({ operation = "plan_open", document = owner.document, view = owner.view.id,
    plan_id = options.plan.id, digest = options.plan.review_digest, revision = options.plan.historical_revision,
    saved_source_digest = options.recovery and options.recovery.saved_source_digest,
    width = owner.source_width }, function(opened, failure)
    perf.event("harness", "plan.review.open_response", { elapsed_ms = perf.elapsed_ms(started), status = failure and "error" or "ok" })
    if failure then callback(nil, failure) return end
    if options.recovery_provider then
      local recovered, recovery_failure = options.recovery_provider()
      if not recovered then callback(nil, recovery_failure) return end
      if recovered.saved_source_digest ~= opened.saved_source_digest then
        callback(nil, "Plan source changed while recovering the draft") return
      end
      options.recovery = recovered
      owner.replica.expected_changedtick = vim.api.nvim_buf_get_changedtick(options.buffer)
    end
    local adopted = buffer.apply_snapshot(owner.replica, opened.snapshot)
    if adopted.kind ~= "Applied" then callback(nil, adopted.kind) return end
    owner.public_only, owner.saved_source_digest, owner.version = opened.public_only, opened.saved_source_digest, opened.version
    local pending = options.recovery and options.recovery.pending_operation
    attach_projection(opened)
    owner.ready = true
    if options.configure_view then options.configure_view(owner.view, owner) end
    vim.bo[options.buffer].buftype = "acwrite"
    if options.plan.historical_revision then vim.bo[options.buffer].modifiable = false end
    owner.gutter_selection = require("forge.document_commands").attach_selection(owner.replica, { normalize = false })
    owner.group = vim.api.nvim_create_augroup("ForgePlanDocument" .. options.buffer, { clear = true })
    vim.api.nvim_create_autocmd("BufWriteCmd", { group = owner.group, buffer = options.buffer, callback = function() save() end })
    vim.api.nvim_create_autocmd("BufWinEnter", { group = owner.group, buffer = options.buffer, callback = function()
      local window = vim.api.nvim_get_current_win()
      if not owner.views[window] then open_view(window) end
    end })
    callback(owner)
    if pending then
      if pending.method then
        local captured = pending.params.review
        captured.document, captured.revision, captured.view = owner.document, owner.replica.revision, owner.view.id
        owner.view.sequence = owner.view.sequence + 1
        captured.sequence = owner.view.sequence
      end
      enqueue_operation(pending)
    end
  end)
  function owner.refresh_views()
    for window, view in pairs(owner.views) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= options.buffer then
        input.close(view)
        owner.views[window] = nil
      end
    end
    for _, window in ipairs(vim.fn.win_findbuf(options.buffer)) do
      if not owner.views[window] then open_view(window) end
    end
  end
  function owner.action(action, receive, window)
    if not owner.attached() then return false end
    if action == "comment" or action == "question" then
      comments.add_at_cursor(options.buffer, false, { kind = action == "question" and "question" or nil,
        heading = action == "question" and "Plan question" or nil })
      receive({ local_draft = true }, nil) return true
    end
    if action == "delete" then comments.delete_at_cursor(options.buffer) receive({}, nil) return true end
    if (action == "toggle_public" or action == "toggle_declaration" or action:find("reveal_reference:", 1, true) == 1) and vim.bo[options.buffer].modified then
      if options.notice then options.notice("Save plan annotations before changing the source projection") end
      return false
    end
    local view = owner.current_view(window)
    if not view then receive(nil, "Plan review window is no longer available") return false end
    local captured, failure = input.capture(owner.replica, view, action)
    if not captured then receive(nil, failure) return false end
    local viewport
    if action == "toggle_declaration" then
      local node = owner.replica.sequence.node[captured.block]
      local collapse = node and node.entry.metadata.collapse and node.entry.metadata.collapse[1]
      if collapse then
        viewport = require("forge.buffer_view").capture_viewport(owner.replica,
          view.window, collapse.opening)
      end
    end
    request({ operation = "plan_action", input = captured }, function(result, error)
      local reference_preview = action:find("reveal_reference:", 1, true) == 1
      if not owner.is_current(captured, not reference_preview) then
        if reference_preview then receive({ cancelled = true }, nil, captured) end
        return
      end
      if not error and result.patch and result.patch ~= vim.NIL then
        comments.detach(options.buffer, false)
        owner.replica.locate, owner.replica.physical_row = nil, nil
        owner.replica.prepare_source, owner.replica.decoration_location, owner.replica.fold_location = nil, nil, nil
        local applied = buffer.apply_snapshot(owner.replica, result.snapshot)
        if applied.kind ~= "Applied" then receive(nil, applied.kind, captured) return end
        attach_projection(result)
        captured = vim.tbl_extend("force", captured, { revision = owner.replica.revision })
        if view and view.id == captured.view then
          view.cursor = vim.api.nvim_win_get_cursor(view.window)
          view.changedtick = vim.api.nvim_buf_get_changedtick(options.buffer)
        end
      end
      if not error and viewport then result.viewport = viewport end
      receive(result, error, captured)
    end)
    return true
  end
  ---@return {file: string, name: string}[]?, ForgeDocumentInput?, string?
  function owner.selected_tests()
    if not owner.attached() then return nil, nil, "Plan review is not ready" end
    local view = owner.current_view()
    if not view then return nil, nil, "Plan review window is no longer available" end
    local mode = vim.fn.mode(1)
    if mode ~= "v" and mode ~= "V" and mode ~= "\22" then return nil, nil, "Select the planned tests first" end
    local first, last = vim.fn.getpos("v")[2] - 1, vim.api.nvim_win_get_cursor(view.window)[1] - 1
    if first > last then first, last = last, first end
    local selected, seen = {}, {}
    for row = first, last do
      local metadata = owner.projection.line_meta_list[row + 1]
      local test = metadata and metadata.test
      if type(test) == "table" then
        local identity = vim.json.encode({ test.file, test.name })
        if not seen[identity] then selected[#selected + 1] = vim.deepcopy(test) seen[identity] = true end
      end
    end
    vim.cmd("normal! " .. string.char(27))
    if #selected == 0 then return nil, nil, "The selection contains no planned tests" end
    local captured, failure = input.capture(owner.replica, view, "delete_tests")
    return selected, captured, failure
  end
  return owner
end

return M
