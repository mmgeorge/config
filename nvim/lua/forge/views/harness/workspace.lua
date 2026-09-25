local M = {}

local owner_by_state = {}
local protected_command = {
  close = true, only = true, bdelete = true, bwipeout = true, bunload = true,
  quit = true, wq = true, exit = true, tabclose = true, tabonly = true,
}

local function blocked(command)
  local ok, parsed = pcall(vim.api.nvim_parse_cmd, command, {})
  return ok and protected_command[parsed.cmd] == true
end

local function notice()
  vim.api.nvim_echo({ { "Harness panes are locked. Press q to exit Harness.", "WarningMsg" } }, false, {})
end

---@param state ForgeHarnessPresentationState
function M.release(state)
  local owner = owner_by_state[state]
  if not owner then return end
  owner_by_state[state] = nil
  vim.api.nvim_del_augroup_by_id(owner.group)
  for window, previous in pairs(owner.window) do
    if vim.api.nvim_win_is_valid(window) then vim.wo[window].winfixbuf = previous end
  end
  for buffer, mappings in pairs(owner.mapping) do
    if vim.api.nvim_buf_is_valid(buffer) then
      vim.api.nvim_buf_call(buffer, function()
        for key, previous in pairs(mappings) do
          pcall(vim.keymap.del, "n", key, { buffer = buffer })
          if previous.buffer == 1 then vim.fn.mapset("n", false, previous) end
        end
      end)
    end
  end
end

---@param state ForgeHarnessPresentationState
function M.attach(state)
  if owner_by_state[state] then return end
  local owner = { window = {}, mapping = {} }
  owner.group = vim.api.nvim_create_augroup("ForgeHarnessWorkspace" .. state.transcript_buf, { clear = true })
  owner_by_state[state] = owner
  local keys = { "ZZ", "ZQ" }
  for _, mapping in ipairs(vim.api.nvim_get_keymap("n")) do
    local command = (mapping.rhs or ""):match("^<[Cc][Mm][Dd]>(.-)<[Cc][Rr]>$")
    if command and blocked(command) then keys[#keys + 1] = mapping.lhs end
  end
  for _, window in ipairs({ state.transcript_win, state.composer_win }) do
    if window and vim.api.nvim_win_is_valid(window) then
      owner.window[window] = vim.wo[window].winfixbuf
      vim.wo[window].winfixbuf = true
    end
  end
  for _, buffer in ipairs({ state.transcript_buf, state.composer_buf }) do
    owner.mapping[buffer] = {}
    for _, key in ipairs(keys) do
      if owner.mapping[buffer][key] == nil then
        vim.api.nvim_buf_call(buffer, function()
          owner.mapping[buffer][key] = vim.fn.maparg(key, "n", false, true)
        end)
      end
      vim.keymap.set("n", key, notice, { buffer = buffer, silent = true, desc = "Keep Harness panes open" })
    end
    vim.api.nvim_create_autocmd({ "BufUnload", "BufWipeout" }, {
      group = owner.group, buffer = buffer,
      callback = function()
        if owner.exiting then return end
        error("Harness buffer is locked. Press q to exit Harness.", 0)
      end,
    })
  end
  vim.api.nvim_create_autocmd("CmdlineLeave", {
    group = owner.group,
    callback = function()
      if vim.fn.getcmdtype() ~= ":" or vim.v.event.abort then return end
      local window = state.transcript_win
      if not vim.api.nvim_win_is_valid(window)
        or vim.api.nvim_get_current_tabpage() ~= vim.api.nvim_win_get_tabpage(window) then return end
      if blocked(vim.fn.getcmdline()) then
        vim.cmd("let v:event.abort = v:true")
        vim.schedule(notice)
      end
    end,
  })
  vim.api.nvim_create_autocmd("ExitPre", {
    group = owner.group,
    callback = function()
      owner.exiting = true
      vim.schedule(function()
        if owner_by_state[state] == owner then owner.exiting = false end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = owner.group, callback = function() M.release(state) end,
  })
  vim.api.nvim_create_autocmd("WinClosed", {
    group = owner.group,
    callback = function()
      vim.schedule(function()
        if owner_by_state[state] ~= owner then return end
        for window in pairs(owner.window) do
          if vim.api.nvim_win_is_valid(window) then return end
        end
        M.release(state)
      end)
    end,
  })
end

---@param window integer
---@param buffer integer
function M.set_buffer(window, buffer)
  local pinned = vim.wo[window].winfixbuf
  vim.wo[window].winfixbuf = false
  local ok, failure = pcall(vim.api.nvim_win_set_buf, window, buffer)
  if vim.api.nvim_win_is_valid(window) then vim.wo[window].winfixbuf = pinned end
  if not ok then error(failure, 0) end
end

return M
