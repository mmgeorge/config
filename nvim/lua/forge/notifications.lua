local M = {}
local buffer = require("forge.buffer")
local input = require("forge.input")
local serial = 0
local current_state
local demand

local function request(state, params, callback)
  if #state.queue >= 64 and params.operation ~= "close" then state.notice("Notification request admission is full") return end
  state.queue[#state.queue + 1] = { params = params, callback = callback }
  local function pump()
    if state.pending or #state.queue == 0 then return end
    local item = table.remove(state.queue, 1)
    if not state.active and item.params.operation ~= "close" then pump() return end
    state.pending = true
    state.request(item.params, function(result, failure)
      vim.schedule(function()
        state.pending = false
        if state.active or item.params.operation == "close" then item.callback(result, failure) end
        pump()
        if state.active and not state.pending and #state.queue == 0 and demand then demand(state) end
      end)
    end)
  end
  pump()
end

local function apply(state, patch, failure)
  if failure then state.notice(failure) return end
  if patch and patch ~= vim.NIL then
    local result = buffer.apply_patch(state.replica, patch)
    if result.kind ~= "Applied" and result.kind ~= "Deferred" and result.kind ~= "Closed" then
      state.notice(result.diagnostic or result.reason or "Invalid notification patch")
    end
  end
end

local function current(state, view, captured)
  return state.active and view.active and state.replica.revision == captured.revision
    and view.sequence == captured.sequence and vim.api.nvim_win_is_valid(view.window)
    and vim.api.nvim_win_get_buf(view.window) == state.replica.buffer
    and vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor)
end

local function update(state, result, failure, view, captured)
  if failure then state.notice(failure) return end
  if not result then state.notice("Missing notification result") return end
  local effect = result.effect ~= vim.NIL and result.effect or nil
  if effect and view and captured and type(effect.id) == "string" and #effect.id > 0 and #effect.id <= 256
    and effect.document == captured.document and effect.revision == captured.revision
    and effect.view == captured.view and effect.sequence == captured.sequence
    and effect.block == captured.block and effect.target == captured.target
    and (effect.kind == "open" or effect.kind == "browse")
    and current(state, view, captured) and not view.effect[effect.id] then
    view.effect[effect.id] = true
    state.open_effect(effect, view.window, function() return current(state, view, captured) end)
  end
  apply(state, result.patch)
  state.more = result.more == true
  if result.diagnostic and result.diagnostic ~= vim.NIL then state.notice(result.diagnostic) end
  local recovery = result.recovery ~= vim.NIL and result.recovery or nil
  if recovery and recovery.state and recovery.state.phase ~= "confirmed" then
    state.notice(recovery.state.diagnostic or ("Notification remote outcome: " .. tostring(recovery.state.phase)))
  end
end

local function attach(state, window)
  if not state.active or not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then return end
  local view = state.view[window]
  if not view then
    local linebreak = vim.wo[window].linebreak
    local columns = {}
    for _, name in ipairs({ "number", "relativenumber", "signcolumn", "foldcolumn", "statuscolumn" }) do
      columns[name] = vim.wo[window][name]
    end
    view = input.open(state.replica, window, { columns = columns })
    view.winbar = vim.wo[window].winbar
    view.header = "%#WinBar# 󰈔 %*%#DropBarFileName#notifications%*"
    vim.wo[window].winbar = view.header
    vim.wo[window].linebreak = linebreak
    state.view[window] = view
  end
  request(state, { operation = "view", document = state.document, view = view.id, width = require("forge.width").capture(window) }, function(patch, failure) apply(state, patch, failure) end)
  return view
end

local function close_view(state, window)
  local view = state.view[window]
  if not view then return end
  input.close(view)
  if vim.api.nvim_win_is_valid(window) and vim.wo[window].winbar == view.header then
    vim.wo[window].winbar = view.winbar
  end
  state.view[window] = nil
  if state.active then request(state, { operation = "close_view", document = state.document, view = view.id }, function(patch, failure) apply(state, patch, failure) end) end
end

local function act(state, action, location, quiet)
  if not state or not state.active then return end
  if state.pending or #state.queue > 0 then if not quiet then state.notice("Notification layout is updating") end return end
  local window = vim.api.nvim_get_current_win()
  local view = state.view[window]
  if not view then attach(state, window) return end
  local captured, failure = input.capture(state.replica, view, action)
  if not captured then if not quiet then state.notice(failure) end return end
  if location then captured.block, captured.position, captured.target = location.block, location.position, location.target end
  if not captured.target then if not quiet then state.notice("This row has no notification") end return end
  if action == "refresh" then state.count = {} end
  request(state, { operation = "act", input = captured }, function(result, reason) update(state, result, reason, view, captured) end)
end

demand = function(state)
  if not state.active or state.pending or #state.queue > 0 or state.replica.status ~= "Applied" then return end
  local window = vim.api.nvim_get_current_win()
  if not state.view[window] or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then return end
  local first, last = vim.fn.line("w0", window), vim.fn.line("w$", window)
  for row = first - 1, math.min(last - 1, first + 255) do
    local location = buffer.locate(state.replica, row, 0)
    if location and location.target then
      if location.target == "notifications:page" then
        if state.more then act(state, "more", location, true) return end
      elseif not state.count[location.target] then
        state.count[location.target] = true
        act(state, "count", location, true)
        return
      end
    end
  end
end

function M.close(state)
  state = state or current_state
  if not state or not state.active then return end
  state.active = false
  for window in pairs(state.view) do close_view(state, window) end
  if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
  request(state, { operation = "close", document = state.document }, function() end)
  buffer.close(state.replica)
  if current_state == state then current_state = nil end
end

function M.open(options)
  options = options or {}
  if current_state and current_state.active and options.hostname and options.hostname ~= current_state.hostname then M.close(current_state) end
  if current_state and current_state.active then
    vim.api.nvim_set_current_buf(current_state.replica.buffer)
    M.refresh()
    return current_state
  end
  serial = serial + 1
  local window = options.window or vim.api.nvim_get_current_win()
  local state = { document = "notifications:" .. vim.uv.hrtime() .. ":" .. serial, workspace = options.workspace or vim.fn.getcwd(),
    active = true, pending = false, queue = {}, view = {}, count = {}, more = false, hostname = options.hostname or "github.com", origin = vim.api.nvim_win_get_buf(window) }
  current_state = state
  state.notice = options.on_error or function(message) vim.notify(tostring(message), vim.log.levels.ERROR, { title = "GitHub notifications" }) end
  state.request = options.request or function(params, callback) return require("forge.client").request_host("notifications.document", params, callback) end
  state.open_effect = options.open_effect or function(effect, effect_window, is_current)
    if not is_current() then return end
    if effect.kind == "browse" or not effect.number or effect.number == vim.NIL then
      if effect.url and effect.url ~= vim.NIL then vim.ui.open(effect.url) end
    else
      vim.api.nvim_win_call(effect_window, function()
        if effect.subject_kind == "PullRequest" then
          require("forge").open_pr_number(effect.number, { repo = effect.repository, cwd = state.workspace })
        else
          require("github.issue_document").open({ kind = "issue", number = effect.number, repository = effect.repository,
            cwd = state.workspace, is_current = is_current })
        end
      end)
    end
  end
  state.replica = buffer.open(state.document, { notice = state.notice, recover = function()
    request(state, { operation = "snapshot", document = state.document }, function(snapshot, failure)
      if failure then state.notice(failure) elseif snapshot then buffer.apply_snapshot(state.replica, snapshot) end
    end)
  end })
  vim.bo[state.replica.buffer].buftype = "nowrite"
  vim.bo[state.replica.buffer].readonly = true
  vim.bo[state.replica.buffer].buflisted = true
  vim.bo[state.replica.buffer].bufhidden = "hide"
  vim.bo[state.replica.buffer].filetype = "ForgeGithubNotifications"
  pcall(vim.api.nvim_buf_set_name, state.replica.buffer, "github://notifications")
  state.group = vim.api.nvim_create_augroup("ForgeNotifications" .. serial, { clear = true })
  vim.api.nvim_create_autocmd("BufWipeout", { group = state.group, buffer = state.replica.buffer, callback = function() M.close(state) end })
  vim.api.nvim_create_autocmd("BufWinLeave", { group = state.group, buffer = state.replica.buffer, callback = function() close_view(state, vim.api.nvim_get_current_win()) end })
  vim.api.nvim_create_autocmd({ "BufWinEnter", "WinResized", "VimResized" }, { group = state.group, callback = function()
    if state.active and state.replica.status == "Applied" then for _, attached in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do attach(state, attached) end end
  end })
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled" }, { group = state.group, buffer = state.replica.buffer, callback = function() demand(state) end })
  for key, action in pairs({ ["<Tab>"] = "expand", ["<CR>"] = "open", b = "browse", S = "save", U = "unread", D = "done", r = "refresh" }) do
    vim.keymap.set("n", key, function() act(state, action) end, { buffer = state.replica.buffer, nowait = true, desc = "GitHub notification " .. action })
  end
  vim.keymap.set("n", "q", function() M.close(state) end, { buffer = state.replica.buffer, nowait = true, desc = "Close GitHub notifications" })
  request(state, { operation = "open", document = state.document, workspace = state.workspace, hostname = state.hostname }, function(opened, failure)
    if failure or not opened then state.notice(failure or "Missing notifications document") M.close(state) return end
    buffer.apply_snapshot(state.replica, opened.snapshot)
    state.more = opened.more == true
    if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == state.origin then
      vim.api.nvim_win_set_buf(window, state.replica.buffer)
      attach(state, window)
    end
  end)
  return state
end

function M.toggle_expand() act(current_state, "expand") end
function M.open_current() act(current_state, "open") end
function M.browse_current() act(current_state, "browse") end
function M.save_current() act(current_state, "save") end
function M.unread_current() act(current_state, "unread") end
function M.done_current() act(current_state, "done") end
function M.refresh()
  local state = current_state
  if not state or not state.active then return end
  local footer = state.replica.sequence.node["notifications:page"]
  if not footer then return end
  act(state, "refresh", { block = "notifications:page", position = { row = 0, column = 0 }, target = "notifications:page" })
end
function M._reset_for_tests() M.close() end
return M
