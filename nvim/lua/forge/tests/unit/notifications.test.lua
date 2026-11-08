-- Unit tests for `forge.notifications` (notifications.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["adapter"] = function()
  local fixture = "nvim/lua/forge/tests/fixtures/protocol/notification.json"
  local snapshot = vim.json.decode(table.concat(vim.fn.readfile(fixture), "\n"))
  local notifications = require("forge.notifications")
  vim.wo.number = true
  vim.wo.relativenumber = false
  vim.wo.signcolumn = "yes"
  vim.wo.foldcolumn = "1"
  vim.wo.statuscolumn = "%l %=%s"
  vim.wo.linebreak = false
  vim.wo.winbar = "origin notification winbar"
  local notices, writes, closed, calls = {}, {}, {}, {}
  local delayed, delayed_open, delayed_invalid, open_input
  local opened_effect = 0
  local function requester(params, callback)
    calls[#calls + 1] = params
    if params.operation == "open" then
      local copied = vim.deepcopy(snapshot)
      copied.document = params.document
      callback({ snapshot = copied, more = false })
    elseif params.operation == "act" then
      if params.input.action == "open" or params.input.action == "browse" then
        delayed_open, open_input = callback, params.input
      elseif params.input.action == "save" then
        writes[#writes + 1] = params.input
        delayed = callback
      elseif params.input.action == "unread" then
        delayed_invalid = callback
      else callback({ more = false }) end
    elseif params.operation == "close" then closed[params.document] = true callback(true)
    else callback(nil) end
  end
  local state = notifications.open({ request = requester, open_effect = function() opened_effect = opened_effect + 1 end, on_error = function(message) notices[#notices + 1] = message end })
  wait_until(function() return not state.pending and #state.queue == 0 end, "notification view did not finish opening",
    { timeout_ms = 3000, errors = notices })
  assert(#notices == 0, vim.inspect(notices))
  assert(vim.bo[state.replica.buffer].readonly)
  assert(vim.bo[state.replica.buffer].filetype == "ForgeGithubNotifications")
  assert(vim.wo.number and not vim.wo.relativenumber, "notification view did not retain number columns")
  assert(vim.wo.signcolumn == "yes" and vim.wo.foldcolumn == "1", "notification view did not retain gutters")
  assert(vim.wo.statuscolumn == "%l %=%s", "notification view did not retain the status column")
  assert(not vim.wo.linebreak, "notification view did not retain line wrapping")
  assert(vim.wo.winbar == "%#WinBar# 󰈔 %*%#DropBarFileName#notifications%*", "notification view lost legacy header")
  local buffer = require("forge.buffer")
  local _, row = state.replica.sequence:position("notification:9007199254740993")
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  keymap.invoke("b")
  wait_until(function() return delayed_open ~= nil end, "notification b did not request the browse action")
  assert(open_input.action == "browse", "notification b invoked a different action")
  local browse_effect = vim.deepcopy(open_input)
  browse_effect.id, browse_effect.kind = "captured-browse", "browse"
  delayed_open({ more = false, effect = browse_effect })
  wait_until(function() return not state.pending and #state.queue == 0 end, "notification b browse operation did not settle")
  assert(opened_effect == 1, "notification b did not deliver its browser effect")
  opened_effect, delayed_open = 0, nil
  notifications.open_current()
  wait_until(function() return delayed_open ~= nil end, "open_current did not request the open action")
  vim.api.nvim_win_set_cursor(0, { row + 2, 0 })
  local effect = vim.deepcopy(open_input)
  effect.id, effect.kind = "captured-open", "open"
  delayed_open({ more = false, effect = effect })
  wait_until(function() return not state.pending and #state.queue == 0 end, "stale open operation did not settle")
  assert(opened_effect == 0, "stale open effect followed a moved cursor")
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  delayed_open = nil
  notifications.open_current()
  wait_until(function() return delayed_open ~= nil end, "open_current did not request the open action for the foreign effect")
  local foreign = vim.deepcopy(open_input)
  foreign.id, foreign.kind, foreign.view = "foreign-view", "open", "another-view"
  delayed_open({ more = false, effect = foreign })
  wait_until(function() return not state.pending and #state.queue == 0 end, "foreign effect operation did not settle")
  assert(opened_effect == 0, "foreign effect envelope opened a subject")
  notifications.save_current()
  wait_until(function() return delayed ~= nil end, "save_current did not request the save action")
  assert(#writes == 1)
  assert(writes[1].target == buffer.locate(state.replica, row, 0).target)
  assert(writes[1].block == "notification:9007199254740993")
  delayed({ more = false })
  wait_until(function() return not state.pending and #state.queue == 0 end, "save operation did not settle")
  local original_apply_patch = buffer.apply_patch
  buffer.apply_patch = function() return { kind = "Desynchronized", diagnostic = "notification patch rejected" } end
  notifications.unread_current()
  wait_until(function() return delayed_invalid ~= nil end, "unread_current did not request the unread action")
  delayed_invalid({ more = false, patch = {} })
  wait_until(function() return #notices == 1 end, "rejected notification patch was not reported")
  assert(notices[1] == "notification patch rejected", "invalid notification patch did not report the result diagnostic")
  buffer.apply_patch = original_apply_patch
  local original = state.replica.buffer
  notifications.close(state)
  assert(not state.active)
  assert(closed[state.document], "close did not collect after admitted operations settled")
  assert(not vim.api.nvim_buf_is_valid(original))
  assert(vim.wo.winbar == "origin notification winbar", "notification close leaked its header")
  local open_callback, open_document, close_count
  close_count = 0
  local origin = vim.api.nvim_get_current_buf()
  local late = notifications.open({ on_error = function(message) error(message) end, request = function(params, callback)
    if params.operation == "open" then open_callback, open_document = callback, params.document
    elseif params.operation == "close" then close_count = close_count + 1 callback(true)
    else callback(nil) end
  end })
  notifications.close(late)
  local copied = vim.deepcopy(snapshot)
  copied.document = open_document
  open_callback({ snapshot = copied, more = false })
  wait_until(function() return close_count == 1 end, "late open response did not close its document")
  assert(vim.api.nvim_get_current_buf() == origin, "late open stole the user's window")
  assert(#notices == 1, vim.inspect(notices))
end

return T
