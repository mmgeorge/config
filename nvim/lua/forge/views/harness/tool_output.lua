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
  local function request(params, callback)
    if owner.host_generation ~= client.host_generation() then
      callback(params.operation == "close" and {} or nil, params.operation ~= "close" and "Harness host generation changed" or nil)
      return
    end
    client.request_for(options.session_id, "harness.document", params, callback)
  end
  function owner.close()
    if owner.closed then return end
    owner.closed = true
    if owner.group then vim.api.nvim_del_augroup_by_id(owner.group) end
    if owner.replica then
      for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do
        if vim.api.nvim_buf_is_valid(origin) then vim.api.nvim_win_set_buf(attached, origin) end
      end
      buffer.close(owner.replica)
    end
    request({ operation = "close", document = owner.document }, function(_, failure)
      if failure then notice(failure) end
    end)
  end
  function owner.demand()
    if owner.closed or not owner.ready or owner.pending or owner.blocked or not owner.more then return end
    local visible = false
    for _, attached in ipairs(vim.fn.win_findbuf(owner.replica.buffer)) do
      local last = vim.api.nvim_win_get_cursor(attached)[1] + vim.api.nvim_win_get_height(attached)
      if last + 64 >= vim.api.nvim_buf_line_count(owner.replica.buffer) then visible = true break end
    end
    if not visible then return end
    owner.pending = true
    request({ operation = "tool_demand", document = owner.document, revision = owner.replica.revision }, function(result, failure)
      owner.pending = false
      if owner.closed then return end
      if failure then owner.blocked = true notice(failure) return end
      if result.patch then
        local applied = buffer.apply_patch(owner.replica, result.patch)
        if applied.kind ~= "Applied" then notice("Tool output delivery failed: " .. tostring(applied.kind)) return end
      end
      owner.more = result.more == true
      if owner.more then vim.schedule(owner.demand) end
    end)
  end
  function owner.export(callback)
    if owner.closed or not owner.ready then return end
    request({ operation = "tool_export", document = owner.document }, function(result, failure)
      if owner.closed then return end
      if failure then notice(failure) return end
      if callback then callback(result.path)
      else vim.notify("Complete tool output: " .. result.path, vim.log.levels.INFO, { title = "Forge tool output" }) end
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
  vim.keymap.set("n", "r", function() owner.blocked = false owner.demand() end, { buffer = owner.replica.buffer, silent = true, desc = "Retry tool output delivery" })
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
    vim.api.nvim_win_set_buf(window, owner.replica.buffer)
    owner.demand()
    if options.on_open then options.on_open(owner) end
  end)
  return owner
end

return M
