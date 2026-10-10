local M = {}
local client = require("forge.client")
local buffer = require("forge.buffer")

function M.open(options)
  local owner = { document = "harness:tool:" .. tostring(vim.uv.hrtime()), closed = false, more = true, pending = false,
    host_generation = client.host_generation() }
  local window = options.window
  local origin = vim.api.nvim_win_get_buf(window)
  local function notice(message)
    if options.notice then options.notice(message) else vim.notify(message, vim.log.levels.ERROR, { title = "Forge tool output" }) end
  end
  local function alive()
    return not owner.closed and owner.host_generation == client.host_generation()
      and owner.replica and vim.api.nvim_buf_is_valid(owner.replica.buffer)
  end
  local function failed(message)
    owner.pending, owner.blocked = false, true
    if owner.failure ~= message then notice(message) end
    owner.failure = message
    if not owner.closed and owner.replica and vim.api.nvim_buf_is_valid(owner.replica.buffer) then
      vim.b[owner.replica.buffer].forge_output_error = message
      for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do
        vim.wo[attached].winbar = "Output unavailable · r retry · E export"
      end
    end
  end
  local function request(params, callback)
    if owner.host_generation ~= client.host_generation() then
      callback(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
    local completed = false
    local deadline
    local function receive(result, failure)
      if completed then return end
      completed = true
      if deadline then deadline:stop() deadline:close() deadline = nil end
      if owner.host_generation ~= client.host_generation() then
        if not owner.closed then failed("Forge host changed. Reopen tool output to reconnect.") end
        return
      end
      local ok, callback_error = pcall(callback, result, failure)
      if not ok and alive() then failed("Tool output callback failed: " .. tostring(callback_error)) end
    end
    deadline = vim.defer_fn(function()
      receive(nil, "Tool output request timed out; press r to refresh before retrying")
    end, 30000)
    client.request_for(options.session_id, "harness.document", params, receive)
  end
  function owner.close()
    if owner.closed then return end
    owner.closed = true
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    if owner.replica then
      for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do
        if vim.api.nvim_buf_is_valid(origin) then require("forge.views.harness.workspace").set_buffer(attached, origin) end
      end
      buffer.close(owner.replica)
    end
    request({ operation = "close", document = owner.document }, function(_, failure)
      if failure then notice(failure) end
    end)
  end
  function owner.demand()
    if not alive() or not owner.ready or owner.pending or owner.blocked or not owner.more then return end
    local visible = false
    for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do
      local last = vim.api.nvim_win_get_cursor(attached)[1] + vim.api.nvim_win_get_height(attached)
      if last + 64 >= vim.api.nvim_buf_line_count(owner.replica.buffer) then visible = true break end
    end
    if not visible then return end
    owner.pending = true
    local previous_rows = vim.api.nvim_buf_line_count(owner.replica.buffer)
    request({ operation = "tool_demand", document = owner.document, revision = owner.replica.revision }, function(result, failure)
      owner.pending = false
      if not alive() then return end
      if failure then failed(failure) return end
      if type(result) ~= "table" then failed("Tool output response is missing") return end
      if type(result.patch) == "table" then
        local applied = buffer.apply_patch(owner.replica, result.patch)
        if applied.kind ~= "Applied" then failed("Tool output delivery failed: " .. tostring(applied.kind)) return end
      end
      owner.more = result.more == true
      if owner.more and vim.api.nvim_buf_line_count(owner.replica.buffer) <= previous_rows then
        failed("Tool output loading made no progress")
        return
      end
      if owner.more then vim.schedule(owner.demand) end
    end)
  end
  function owner.retry()
    if not alive() or not owner.ready or owner.pending then return end
    owner.pending = true
    request({ operation = "snapshot", document = owner.document }, function(snapshot, failure)
      if not alive() then return end
      owner.pending = false
      if failure then failed(failure) return end
      local applied = buffer.apply_snapshot(owner.replica, snapshot)
      if applied.kind ~= "Applied" then failed("Tool output snapshot could not be adopted") return end
      owner.failure, owner.blocked, owner.more = nil, false, true
      vim.b[owner.replica.buffer].forge_output_error = nil
      for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do vim.wo[attached].winbar = "" end
      owner.demand()
    end)
  end
  function owner.export(callback)
    if not alive() or not owner.ready then return end
    request({ operation = "tool_export", document = owner.document }, function(result, failure)
      if not alive() then return end
      if failure then notice(failure) return end
      if callback then callback(result.path)
      else vim.api.nvim_echo({ { "Complete tool output: " .. result.path } }, false, {}) end
    end)
  end
  owner.replica = buffer.open(owner.document, { filetype = "ForgeHarnessTool", notice = notice })
  vim.bo[owner.replica.buffer].bufhidden = "wipe"
  owner.group = vim.api.nvim_create_augroup("ForgeHarnessTool" .. tostring(vim.uv.hrtime()), { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled", "BufWinEnter" }, {
    group = owner.group, buffer = owner.replica.buffer, callback = owner.demand,
  })
  vim.api.nvim_create_autocmd("BufWipeout", { group = owner.group, buffer = owner.replica.buffer, once = true, callback = owner.close })
  vim.keymap.set("n", "q", owner.close, { buffer = owner.replica.buffer, silent = true, desc = "Close tool output" })
  vim.keymap.set("n", "E", function() owner.export() end, { buffer = owner.replica.buffer, silent = true, desc = "Export complete tool output" })
  vim.keymap.set("n", "r", owner.retry, { buffer = owner.replica.buffer, silent = true, desc = "Retry tool output delivery" })
  request({ operation = "tool_open", document = owner.document, input = options.input }, function(opened, failure)
    if owner.closed then
      if opened then request({ operation = "close", document = owner.document }, function(_, close_error) if close_error then notice(close_error) end end) end
      return
    end
    if failure then notice(failure) owner.close() return end
    if not options.is_current() or not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= origin then owner.close() return end
    local applied = buffer.apply_snapshot(owner.replica, opened.snapshot)
    if applied.kind ~= "Applied" then notice("Tool output could not be adopted: " .. tostring(applied.kind)) owner.close() return end
    owner.ready, owner.more = true, opened.more == true
    vim.api.nvim_buf_set_name(owner.replica.buffer, "ForgeToolOutput://" .. owner.document)
    require("forge.views.harness.workspace").set_buffer(window, owner.replica.buffer)
    owner.demand()
    if options.on_open then options.on_open(owner) end
  end)
  return owner
end

return M
