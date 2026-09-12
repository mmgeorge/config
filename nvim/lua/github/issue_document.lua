local M = {}
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local input = require("forge.input")
local effects = require("forge.effects")
local choice_popup = require("forge.infra.choice_popup")
local markdown = require("github.issue_markdown")
local issue_gutter = require("github.issue_gutter")
local runner_for_test
local apply_patch

local command = {
  { key = "<Tab>", label = "Toggle fold", hint = "toggle" },
  { key = "b", label = "Browse source", hint = "browse" },
  { key = "R", label = "Refresh issue", hint = "refresh" },
  { key = "<C-s>", label = "Save edits", hint = "sync" },
  { key = "X", label = "Resolve save recovery" },
  { key = "q", label = "Close issue", hint = "close" },
}

local function report(state, failure)
  state.failure = tostring(failure)
  state.notice(state.failure)
end

local function current(state)
  return state.active and (not state.is_current or state.is_current())
end

local function render_markdown(state)
  if not state.replica then return end
  issue_gutter.refresh(state.replica)
  for window in pairs(state.view) do
    if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == state.replica.buffer then
      markdown.render(state.replica, window)
    end
  end
end

local function adopt_save_state(state, result)
  state.fields = result.fields
  state.recovery = result.recovery ~= vim.NIL and result.recovery or nil
  state.fresh_required = result.fresh_required == true
end

local function recovery_operation_id(state)
  local capture = state.recovery and state.recovery.capture or nil
  local operation_id = capture and capture.operation_id or nil
  if type(operation_id) ~= "string" or operation_id == "" or #operation_id > 256 or operation_id:find("%c") then
    return nil
  end
  return operation_id
end

local function request(state, operation, params, callback)
  params.operation = operation
  local delivered = false
  local function receive(result, failure)
    if delivered then return end
    delivered = true
    vim.schedule(function() callback(result, failure) end)
  end
  if not runner_for_test and state.generation then
    local client = require("forge.client")
    if state.generation ~= client.host_generation() or not client.host_accepting() then
      if state.replica then buffer.invalidate(state.replica) end
      receive(nil, "Issue host was collected. Local text remains available in this buffer.")
      return
    end
  end
  local accepted, failure = pcall(function()
    if runner_for_test then runner_for_test("issue.document", params, receive)
    else require("forge.client").request_host("issue.document", params, receive) end
  end)
  if not accepted then receive(nil, tostring(failure)) end
end

local function apply_window_options(state, window)
  if not state.replica or not vim.api.nvim_win_is_valid(window)
    or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then
    return
  end
  vim.wo[window].wrap = true
  vim.wo[window].linebreak = true
  vim.wo[window].breakindent = true
end

local function attach_view(state, window)
  if not state.active or not state.replica or not vim.api.nvim_win_is_valid(window)
    or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then
    return nil
  end
  local view = state.view[window]
  if not view then
    view = input.open(state.replica, window, { columns = state.columns })
    view.issue_winbar = { previous = vim.wo[window].winbar }
    state.view[window] = view
  end
  apply_window_options(state, window)
  issue_gutter.refresh(state.replica)
  markdown.render(state.replica, window)
  local hint = {}
  for _, spec in ipairs(command) do
    if spec.hint then hint[#hint + 1] = spec.key .. " " .. spec.hint end
  end
  hint[#hint + 1] = "? help"
  local title = "GitHub Issue #" .. state.number
  local text = table.concat(hint, " | ")
  if vim.fn.strdisplaywidth(title .. "  " .. text) > vim.api.nvim_win_get_width(window) then title = "" end
  local winbar = title:gsub("%%", "%%%%") .. "%=" .. text:gsub("%%", "%%%%")
  view.issue_winbar.value = winbar
  vim.wo[window].winbar = winbar
  request(state, "view", {
    document = state.document,
    view = view.id,
    width = require("forge.width").capture(window),
  }, function(patch, failure)
    if failure then report(state, failure) return end
    apply_patch(state, patch)
  end)
  return view
end

local function toggle_fold(state)
  local window = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(window)
  local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
  local block = location and state.replica.block[location.block]
  if location and location.block:match("^issue:comment:%d+:label$")
    and location.position.row == 0 and block and block.row_count > 1 then
    vim.api.nvim_win_set_cursor(window, { cursor[1] + 1, 0 })
    vim.cmd("normal! za")
    vim.api.nvim_win_set_cursor(window, cursor)
    return
  end
  vim.cmd("normal! za")
end

local function close_view(state, window, callback)
  local view = state.view[window]
  if not view then
    if callback then callback() end
    return
  end
  state.view[window] = nil
  if vim.api.nvim_win_is_valid(window) and vim.wo[window].winbar == view.issue_winbar.value then
    vim.wo[window].winbar = view.issue_winbar.previous
  end
  input.close(view)
  request(state, "close_view", { document = state.document, view = view.id }, function(_, failure)
    if failure then report(state, failure) end
    if callback then callback() end
  end)
end

local function show_commands()
  choice_popup.open({
    title = "GitHub Issue Commands",
    options = vim.tbl_map(function(spec)
      return { key = spec.key, label = spec.label, value = false }
    end, command),
    cancel_label = "close help",
    on_choice = function() end,
  })
end

local function finish_close(state)
  if state.closing_request then return end
  state.closing_request = true
  local function close_document()
    request(state, "close", { document = state.document }, function(result, failure)
    state.closing_request = false
      if failure or type(result) ~= "table" or result.collected ~= true then
        report(state, failure or "Native issue close did not confirm collection")
        return
      end
      state.active = false
      if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
      if state.replica then
        issue_gutter.clear(state.replica)
        markdown.clear(state.replica)
        buffer.close(state.replica)
        if vim.api.nvim_win_is_valid(state.window)
          and vim.api.nvim_win_get_buf(state.window) == state.replica.buffer
          and vim.api.nvim_buf_is_valid(state.origin) then
          vim.api.nvim_win_set_buf(state.window, state.origin)
        end
        if vim.api.nvim_buf_is_valid(state.replica.buffer) then
          pcall(vim.api.nvim_buf_delete, state.replica.buffer, { force = true })
        end
      end
    end)
  end
  local windows = {}
  for window in pairs(state.view) do windows[#windows + 1] = window end
  if #windows == 0 then close_document() return end
  local pending = #windows
  for _, window in ipairs(windows) do
    close_view(state, window, function()
      pending = pending - 1
      if pending == 0 then close_document() end
    end)
  end
end

local function settled(state)
  if state.replica and editable.suspend_generated_text(state.replica.editable) then return end
  if state.closing then finish_close(state)
  elseif state.save_pending then M.save(state) end
  if state.refresh_pending then M.refresh(state) end
end

function M.save(state)
  if not current(state) or not state.replica or state.saving then return false end
  if state.fresh_required then
    report(state, "Issue requires a fresh remote observation before saving. Press R to refresh.")
    return false
  end
  state.save_pending = true
  if not editable.flush(state.replica.editable) then
    report(state, "Issue edits could not be submitted")
    return false
  end
  if editable.suspend_generated_text(state.replica.editable) then return true end
  state.save_pending, state.saving = false, true
  request(state, "save", { document = state.document }, function(result, failure)
    state.saving = false
    if failure or type(result) ~= "table" then
      report(state, failure or "Missing native issue save result")
    else
      adopt_save_state(state, result)
      if result.diagnostic and result.diagnostic ~= vim.NIL then report(state, result.diagnostic) end
      for _, field in ipairs(result.fields or {}) do
        if field.uncertain then report(state, "Issue save outcome requires reconciliation") break end
      end
    end
    settled(state)
  end)
  return true
end

function M.refresh(state)
  if not current(state) or not state.replica or state.refreshing then return false end
  if not editable.flush(state.replica.editable) then
    report(state, "Issue edits could not be submitted before refresh")
    return false
  end
  if editable.suspend_generated_text(state.replica.editable) then
    state.refresh_pending = true
    return true
  end
  state.refresh_pending, state.refreshing = false, true
  request(state, "refresh", { document = state.document }, function(result, failure)
    state.refreshing = false
    if failure or type(result) ~= "table" then
      report(state, failure or "Missing native issue refresh result")
    else
      adopt_save_state(state, result)
      if result.patch and result.patch ~= vim.NIL then apply_patch(state, result.patch) end
    end
    settled(state)
  end)
  return true
end

---@param state table
---@param resolution {resolution: "close_unknown"|"not_dispatched"}
---@return boolean
function M.resolve(state, resolution)
  if not current(state) or not state.replica or state.resolving then return false end
  local operation_id = recovery_operation_id(state)
  if not operation_id then
    if state.fresh_required then
      report(state, "Issue requires a fresh remote observation before saving. Press R to refresh.")
    else
      report(state, "Issue has no unresolved save operation")
    end
    return false
  end
  state.resolving = true
  request(state, "resolve", {
    document = state.document,
    operation_id = operation_id,
    resolution = resolution,
  }, function(result, failure)
    state.resolving = false
    if failure or type(result) ~= "table" then
      report(state, failure or "Missing native issue recovery result")
    else
      adopt_save_state(state, result)
      if result.diagnostic and result.diagnostic ~= vim.NIL then report(state, result.diagnostic) end
      if state.fresh_required then
        report(state, "Issue outcome was closed without confirmation. Press R to refresh before saving.")
      end
    end
    settled(state)
  end)
  return true
end

---@param state table
function M.recover(state)
  if not current(state) or state.resolving then return end
  local operation_id = recovery_operation_id(state)
  if not operation_id then
    if state.fresh_required then
      report(state, "Issue requires a fresh remote observation before saving. Press R to refresh.")
    else
      report(state, "Issue has no unresolved save operation")
    end
    return
  end
  local phase = state.recovery.state and state.recovery.state.phase or "unknown"
  local options = {
    {
      key = "c",
      value = { resolution = "close_unknown" },
      label = "Close unknown outcome and require a fresh remote observation",
    },
  }
  if phase == "prepared" then
    table.insert(options, 1, {
      key = "n",
      value = { resolution = "not_dispatched" },
      label = "Confirm the operation was not dispatched",
    })
  end
  choice_popup.open({
    title = "Issue Save Recovery",
    options = options,
    cancel_label = "keep pending",
    on_choice = function(resolution)
      if resolution then M.resolve(state, resolution) end
    end,
  })
end

function M.close(state)
  if not state.active or state.closing then return end
  state.closing = true
  if not state.opened then return end
  if state.replica then
    if not editable.flush(state.replica.editable) then
      report(state, "Issue edits remain in this buffer because draft publication failed")
      return
    end
    if editable.suspend_generated_text(state.replica.editable) then return end
  end
  if not state.saving then finish_close(state) end
end

apply_patch = function(state, patch)
  if not patch or patch == vim.NIL then return true end
  local applied = buffer.apply_patch(state.replica, patch)
  if applied.kind == "Applied" or applied.kind == "Deferred" then
    render_markdown(state)
    return true
  end
  report(state, applied.reason or applied.diagnostic or applied.kind)
  return false
end

function M.activate(state)
  if not current(state) or not state.replica then return false end
  local window = vim.api.nvim_get_current_win()
  local view = state.view[window] or attach_view(state, window)
  if not view then
    report(state, "Issue view is not ready for input")
    return false
  end
  local captured, failure = input.capture(state.replica, view, "browse")
  if not captured then report(state, failure) return false end
  request(state, "act", { input = captured }, function(delivery, request_failure)
    if request_failure or type(delivery) ~= "table" then
      report(state, request_failure or "Missing native issue action result")
      return
    end
    if delivery.diagnostic and delivery.diagnostic ~= vim.NIL then report(state, delivery.diagnostic) return end
    if not current(state) or state.view[window] ~= view then return end
    if delivery.effect and delivery.effect ~= vim.NIL then
      local outcome, effect_failure = effects.apply(state.replica, view, delivery.effect)
      if outcome == "Failed" then report(state, effect_failure) end
    end
  end)
  return true
end

local function attach(state, result)
  state.replica = buffer.open(state.document, {
    filetype = "ForgeGithubIssue", notice = state.notice,
    recover = function() report(state, "Issue presentation requires explicit recovery") end,
    editable = { notice = state.notice, send = function(edit)
      request(state, "edit", { edit = {
        document = edit.document, region = edit.region, base = edit.base,
        sequence = edit.sequence, text = table.concat(edit.text, "\n"),
      } }, function(acknowledgement, failure)
        if failure or type(acknowledgement) ~= "table" or type(acknowledgement.patch) ~= "table" then
          report(state, failure or "Missing native issue edit acknowledgement")
          return
        end
        if not state.active then return end
        local applied = buffer.acknowledge_edit(state.replica, acknowledgement, acknowledgement.patch)
        if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then
          report(state, applied.reason or applied.kind)
          return
        end
        render_markdown(state)
        settled(state)
      end)
      return true
    end },
  })
  local applied = buffer.apply_snapshot(state.replica, result.snapshot)
  if applied.kind ~= "Applied" then report(state, applied.reason or applied.kind) return end
  local native_buffer = state.replica.buffer
  vim.bo[native_buffer].buftype = "acwrite"
  vim.bo[native_buffer].buflisted = true
  require("github.repo_cache").enable_user_completion(
    native_buffer,
    state.repository.owner .. "/" .. state.repository.name
  )
  state.group = vim.api.nvim_create_augroup("ForgeIssueDocument" .. native_buffer, { clear = true })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = state.group, buffer = native_buffer, callback = function() M.save(state) end,
  })
  vim.api.nvim_create_autocmd("BufUnload", {
    group = state.group, buffer = native_buffer, callback = function() M.close(state) end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", {
    group = state.group, buffer = native_buffer, callback = function()
      close_view(state, vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd({ "BufWinEnter", "WinEnter", "WinResized", "VimResized" }, {
    group = state.group, buffer = native_buffer, callback = function()
      if state.active then
        for _, window in ipairs(vim.fn.win_findbuf(native_buffer)) do attach_view(state, window) end
      end
    end,
  })
  vim.keymap.set("n", "q", function() M.close(state) end, { buffer = native_buffer, desc = "Close issue" })
  vim.keymap.set({ "n", "i" }, "<C-s>", function() M.save(state) end, { buffer = native_buffer, desc = "Save issue" })
  vim.keymap.set("n", "b", function() M.activate(state) end, { buffer = native_buffer, desc = "Browse issue source" })
  vim.keymap.set("n", "R", function() M.refresh(state) end, { buffer = native_buffer, desc = "Refresh issue" })
  vim.keymap.set("n", "X", function() M.recover(state) end, { buffer = native_buffer, desc = "Resolve issue save recovery" })
  vim.keymap.set("n", "<Tab>", function()
    if vim.api.nvim_get_current_buf() == native_buffer then toggle_fold(state) end
  end, { buffer = native_buffer, desc = "Toggle issue fold" })
  vim.keymap.set("n", "?", show_commands, { buffer = native_buffer, desc = "Show issue commands" })
  vim.keymap.set("i", "<CR>", function()
    local window = vim.api.nvim_get_current_win()
    if vim.api.nvim_win_get_buf(window) ~= native_buffer then return "\n" end
    local cursor = vim.api.nvim_win_get_cursor(window)
    local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
    if location and (location.block == "region:title" or location.block == "region:assignees") then return "" end
    return "\n"
  end, { buffer = native_buffer, expr = true, desc = "Issue title newline guard" })
  if current(state) and vim.api.nvim_win_is_valid(state.window)
    and vim.api.nvim_win_get_buf(state.window) == state.origin then
    vim.api.nvim_win_set_buf(state.window, native_buffer)
    state.shown = true
    attach_view(state, state.window)
    if state.on_open then state.on_open(state) end
  else M.close(state) end
end

function M.open(options)
  local repository = vim.deepcopy(assert(options.repository, "Issue repository is required"))
  if options.kind == "pr" then
    return require("forge.review_document").open({
      directory = options.cwd or vim.fn.getcwd(), repository = repository, number = options.number,
      is_current = options.is_current, window = options.window,
      on_open = options.on_open, on_error = options.on_error,
    })
  end
  local window = options.window or vim.api.nvim_get_current_win()
  local baseline = require("forge.window_presentation").capture(window)
  local columns = {}
  for _, name in ipairs({ "number", "relativenumber", "signcolumn", "foldcolumn", "statuscolumn" }) do
    columns[name] = baseline[name]
  end
  local state = {
    active = true, document = "issue:" .. tostring(vim.uv.hrtime()),
    repository = repository, number = assert(options.number),
    directory = options.cwd or vim.fn.getcwd(), window = window,
    origin = vim.api.nvim_win_get_buf(window), is_current = options.is_current,
    on_open = options.on_open, view = {}, columns = columns,
    notice = options.on_error or function(message)
      vim.notify(message, vim.log.levels.ERROR, { title = "Forge issue" })
    end,
  }
  local repository_name = repository.owner .. "/" .. repository.name
  local database = options.database or vim.fs.joinpath(
    require("github.repo_cache").repo_dir(repository_name, repository.hostname), "issues", "issues.redb")
  request(state, "open", { document = state.document, directory = state.directory,
    database = database, repository = repository, number = state.number,
  }, function(result, failure)
    if failure or type(result) ~= "table" or type(result.snapshot) ~= "table"
      or result.snapshot.document ~= state.document then
      report(state, failure or "Missing native issue document")
      state.active = false
      return
    end
    state.opened = true
    if not runner_for_test then state.generation = require("forge.client").host_generation() end
    adopt_save_state(state, result)
    if state.closing or not current(state) then finish_close(state) return end
    attach(state, result)
  end)
  return state
end

function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
