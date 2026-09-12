local M = {}
local buffer = require("forge.buffer")
local input = require("forge.input")
local next_document = 0
local runner_for_test
local cache = {}
local window_presentation = require("forge.window_presentation")

local function host_current(state)
  if runner_for_test or not state.host_generation then return true end
  local client = require("forge.client")
  return state.host_generation == client.host_generation() and client.host_accepting()
end

local function request(state, params, callback)
  local function receive(result, failure)
    if result and not failure and not runner_for_test and not state.host_generation then
      state.host_generation = require("forge.client").host_generation()
    end
    vim.schedule(function()
      if not host_current(state) then
        result = nil
        failure = params.operation ~= "close" and "Forge host stopped or restarted; reopen this source document" or nil
      end
      if state.active or params.operation == "close" then callback(result, failure) end
    end)
  end
  if not host_current(state) then receive(nil) return end
  local ok, failure = pcall(function()
    local method = params.operation == "change" and "walkthrough" or params.operation == "open_diff" and "harness.document"
      or (params.operation == "commit_message" or params.operation == "about_message") and "status.context" or "source.document"
    if runner_for_test then runner_for_test(method, params, receive)
    elseif params.operation == "open_diff" then require("forge.client").request_for(state.session_id, method, params, receive)
    else require("forge.client").request_host(method, params, receive) end
  end)
  if not ok then receive(nil, tostring(failure)) end
end

local function close_view(state, window, view)
  input.close(view.input)
  local owned = window_presentation.release(window, view)
  if owned and vim.api.nvim_win_is_valid(window) and vim.wo[window].winbar == view.header then
    vim.wo[window].winbar = view.winbar
    for name, value in pairs(view.presentation) do
      if vim.wo[window][name] == view.applied[name] then vim.wo[window][name] = value end
    end
  end
  state.view[window] = nil
  if state.active and host_current(state) then
    request(state, { operation = "close_view", document = state.document, view = view.input.id }, function(_, failure)
      if failure then state.notice(failure) end
    end)
  end
end

local function view_for(state, window)
  local view = state.view[window]
  if not view then
    local original_winbar = vim.wo[window].winbar
    local header = state.kind == "commit_message" and ("%%#WinBar# 󰈔 %%*%%#DropBarFileName#%s%%*"):format(state.display_revision)
      or "%#ForgeFileRevisionHeader# " .. (state.title or "Historical source"):gsub("%%", "%%%%"):gsub("[\r\n]", " ") .. " — read-only revision %*"
    view = { input = input.open(state.replica, window, { exact_source = true }), winbar = original_winbar,
      header = header, presentation = {}, applied = {} }
    local baseline = state.presentation[window] or window_presentation.capture(window)
    state.presentation[window] = nil
    for name, value in pairs(baseline) do
      view.presentation[name] = vim.wo[window][name]
      view.applied[name] = value
      vim.wo[window][name] = view.applied[name]
    end
    window_presentation.retain(window, view, baseline)
    state.view[window] = view
    vim.wo[window].winbar = header
  end
  return view.input
end

local function position_requested(state)
  if not state.requested_row then return end
  if state.replica.row_count < state.requested_row and state.more then return end
  if vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.replica.buffer then
    vim.api.nvim_win_set_cursor(state.window, { math.max(1, math.min(state.requested_row, state.replica.row_count)), 0 })
    vim.api.nvim_win_call(state.window, function() vim.cmd("normal! zz") end)
  end
  state.requested_row = nil
end

local function display_source(state, window)
  local origin = vim.api.nvim_win_get_buf(window)
  if origin ~= state.replica.buffer then
    state.presentation[window] = window_presentation.capture(window)
    state.return_view[window] = { buffer = origin,
      view = vim.api.nvim_win_call(window, function() return vim.fn.winsaveview() end) }
    vim.api.nvim_win_set_buf(window, state.replica.buffer)
  end
  view_for(state, window)
  position_requested(state)
end

function M.demand(state)
  if not host_current(state) then return end
  if not state.active or state.pending or state.scheduled or state.replica.status ~= "Applied" then return end
  state.scheduled = true
  vim.schedule(function()
    state.scheduled = false
    if not host_current(state) then return end
    if not state.active or state.pending then return end
    for window in pairs(state.return_view) do
      if not vim.api.nvim_win_is_valid(window) then state.return_view[window] = nil end
    end
    for window, view in pairs(state.view) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then
        close_view(state, window, view)
      end
    end
    position_requested(state)
    for _, window in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do
      local view = view_for(state, window)
      local last = vim.api.nvim_win_call(window, function() return vim.fn.line("w$") + vim.api.nvim_win_get_height(window) end)
      if state.more and (state.replica.row_count <= last or state.requested_row and state.requested_row > state.replica.row_count) then
        local captured, failure = input.capture(state.replica, view, "demand")
        if not captured then state.notice(failure) return end
        state.pending = true
        request(state, { operation = "demand", input = captured }, function(delivery, request_failure)
          state.pending = false
          if request_failure or not delivery then state.notice(request_failure or "Missing source delivery") return end
          if delivery.patch and delivery.patch ~= vim.NIL then buffer.apply_patch(state.replica, delivery.patch) end
          state.more = delivery.more == true
          if delivery.state and (delivery.state.state == "failed" or delivery.state.state == "unavailable") then
            state.notice(delivery.state.diagnostic)
          end
          M.demand(state)
        end)
        return
      end
    end
  end)
end

function M.open(options)
  next_document = next_document + 1
  local state = { document = "source:" .. vim.uv.hrtime() .. ":" .. next_document,
    active = true, pending = false, scheduled = false, more = false, view = {}, return_view = {}, presentation = {},
    window = options.window or vim.api.nvim_get_current_win(), requested_row = options.line, used = next_document, session_id = options.session_id }
  state.origin = vim.api.nvim_win_get_buf(state.window)
  state.notice = options.on_error or function(message) vim.notify(message, vim.log.levels.ERROR, { title = "Forge source" }) end
  state.replica = buffer.open(state.document, { notice = state.notice, recover = function()
    request(state, { operation = "snapshot", document = state.document }, function(snapshot, failure)
      if failure or not snapshot then state.notice(failure or "Missing source snapshot") return end
      buffer.apply_snapshot(state.replica, snapshot)
      M.demand(state)
    end)
  end })
  vim.bo[state.replica.buffer].buflisted = true
  vim.bo[state.replica.buffer].buftype = "nowrite"
  vim.bo[state.replica.buffer].bufhidden = "hide"
  vim.bo[state.replica.buffer].readonly = true
  state.group = vim.api.nvim_create_augroup("ForgeSourceDocument" .. next_document, { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled", "BufWinEnter" }, {
    group = state.group, buffer = state.replica.buffer, callback = function() M.demand(state) end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", { group = state.group, buffer = state.replica.buffer, callback = function()
    local window = vim.api.nvim_get_current_win()
    if state.view[window] then close_view(state, window, state.view[window]) end
  end })
  vim.api.nvim_create_autocmd("WinClosed", { group = state.group, callback = function() M.demand(state) end })
  vim.api.nvim_create_autocmd("BufWipeout", { group = state.group, buffer = state.replica.buffer, once = true,
    callback = function() vim.schedule(function() M.close(state) end) end })
  vim.keymap.set("n", "q", function() M.close(state) end, { buffer = state.replica.buffer, silent = true, desc = "Close file revision" })
  local function open_native()
  local params = options.walkthrough_input and { operation = "change", document = state.document, annotation_document = options.annotation_document, input = options.walkthrough_input, review = options.walkthrough_review }
    or options.harness_input and { operation = "open_diff", document = state.document, input = options.harness_input }
    or options.oid and { operation = "commit_message", document = state.document, workspace = options.workspace, oid = options.oid }
    or options.status_document and { operation = "about_message", document = state.document, text = assert(options.message, "About message is missing") }
    or { operation = "open", document = state.document, workspace = options.workspace,
      revision = options.revision, path = vim.base64.encode(options.path) }
  state.kind = params.operation
  request(state, params, function(opened, failure)
    if failure or not opened then
      state.notice(failure or "Missing source document")
      M.close(state)
      return
    end
    if options.is_current and not options.is_current() then M.close(state) return end
    if opened.source_row then state.requested_row = opened.source_row + 1 end
    if options.on_annotation and opened.annotation then options.on_annotation(opened.annotation, opened.stale, opened.review) end
    local key = options.workspace .. "\0" .. options.path .. "\0" .. opened.title .. "\0" .. opened.object
    if options.annotation_document then key = key .. "\0" .. options.annotation_document end
    local existing = cache[key]
    if existing and existing.active and host_current(existing) then
      existing.window, existing.requested_row = state.window, state.requested_row
      existing.used = next_document
      if (not options.is_current or options.is_current()) and vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.origin then
        display_source(existing, state.window)
      end
      M.close(state)
      M.demand(existing)
      if options.on_ready then options.on_ready(existing) end
      return
    end
    cache[key], state.cache_key = state, key
    local revision = params.operation == "commit_message" and options.oid or opened.revision or opened.object
    local display_revision = revision:match("^%x+$") and revision:sub(1, 7) or revision
    state.display_revision = display_revision
    state.title, state.more = opened.title, opened.more == true
    if display_revision ~= revision then
      state.title = state.title:gsub(" @ " .. revision .. "$", " @ " .. display_revision)
    end
    buffer.apply_snapshot(state.replica, opened.snapshot)
    if options.is_current and not options.is_current() then M.close(state) return end
    if params.operation == "commit_message" then
      vim.api.nvim_buf_set_name(state.replica.buffer, "GitCommit://" .. display_revision)
      vim.bo[state.replica.buffer].filetype = "gitcommit"
    else
      vim.api.nvim_buf_set_name(state.replica.buffer, "ForgeFileRevision://" .. options.path .. "@" .. display_revision .. "#" .. next_document)
    end
    if params.operation == "open" then
      local filetype = vim.filetype.match({ filename = options.path, buf = state.replica.buffer }) or ""
      vim.b[state.replica.buffer].forge_source_markdown = filetype == "markdown"
      vim.bo[state.replica.buffer].filetype = filetype
      vim.treesitter.stop(state.replica.buffer)
    end
    if (not options.is_current or options.is_current()) and vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.origin then
      display_source(state, state.window)
    end
    if opened.state and (opened.state.state == "failed" or opened.state.state == "unavailable") then state.notice(opened.state.diagnostic) end
    M.demand(state)
    if options.on_ready then options.on_ready(state) end
  end)
  end
  local count, oldest = 0, nil
  for _, cached in pairs(cache) do
    if cached.active then
      count = count + 1
      if #vim.fn.win_findbuf(cached.replica.buffer) == 0 and (not oldest or cached.used < oldest.used) then oldest = cached end
    end
  end
  if count >= 7 and oldest then M.close(oldest, open_native) else open_native() end
  return state
end

function M.open_commit(options)
  return M.open(vim.tbl_extend("force", options, { path = "commit/" .. options.oid }))
end

function M.open_about(options)
  return M.open(vim.tbl_extend("force", options, { path = "about/" .. options.status_document }))
end

function M.open_walkthrough(options)
  return M.open({ workspace = options.workspace, path = "walkthrough", window = options.window,
    walkthrough_input = options.input, annotation_document = options.annotation_document, walkthrough_review = options.review,
    is_current = options.is_current, on_annotation = options.on_annotation, on_error = options.on_error, on_ready = options.on_ready })
end

function M.open_harness_diff(options)
  return M.open(vim.tbl_extend("force", options, { workspace = options.session_id, path = "tool-diff", harness_input = options.input }))
end

function M.close(state, on_closed)
  if not state.active then return end
  state.active = false
  if state.cache_key and cache[state.cache_key] == state then cache[state.cache_key] = nil end
  for window, view in pairs(state.view) do close_view(state, window, view) end
  vim.api.nvim_del_augroup_by_id(state.group)
  for window, origin in pairs(state.return_view) do
    if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == state.replica.buffer
      and vim.api.nvim_buf_is_valid(origin.buffer) then
      vim.api.nvim_win_set_buf(window, origin.buffer)
      vim.api.nvim_win_call(window, function() vim.fn.winrestview(origin.view) end)
    end
  end
  state.return_view = {}
  request(state, { operation = "close", document = state.document }, function(_, failure)
    if failure then state.notice(failure) end
    if on_closed then on_closed() end
  end)
  buffer.close(state.replica)
end

function M._set_runner_for_test(runner) runner_for_test = runner end

return M
