-- Unit tests for `forge.views.permissions` (permissions.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["permissions"] = function()
  local request_list = {}
  local save_error = "line 3: invalid permission decision"
  local permission_path = vim.fn.tempname() .. ".json"
  local permission_source = '{\n  "read": { "*": "allow" }\n}\n'
  local defer_save, pending_save, defer_open, pending_open

  package.loaded["forge.client"] = {
    session_initialize = function(callback) callback({}, nil) end,
    request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

      request_list[#request_list + 1] = { method = method, params = params }
      if method == "permissions.document.open" then
        if defer_open then pending_open = callback return end
        callback({ path = permission_path, source = permission_source }, nil)
      elseif defer_save then
        pending_save = callback
      elseif params.source:find('"invalid"', 1, true) then
        callback(nil, save_error)
      else
        callback({ path = permission_path, source = params.source }, nil)
      end
    end,
  }
  local notification_list = notify.capture({ notifications = true })

  require("forge.views.permissions").open()
  local buffer = vim.api.nvim_get_current_buf()
  eq(vim.api.nvim_buf_get_name(buffer), permission_path, { fail_reason = "permissions should expose the broker-owned path" })
  eq(vim.bo[buffer].buftype, "acwrite", { fail_reason = "permissions should save through broker validation" })
  eq(vim.bo[buffer].filetype, "json", { fail_reason = "permissions should use JSON editing" })
  assert(not vim.bo[buffer].modified, "opening permissions should begin clean")

  buffer_support.set(buffer, { "{", '  "read": { "*": "invalid" }', "}" })
  vim.bo[buffer].modified = true
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  assert(vim.bo[buffer].modified, "a rejected document should remain modified")
  eq(notification_list[#notification_list].level, vim.log.levels.ERROR, { fail_reason = "a rejected document should report an error" })
  eq(notification_list[#notification_list].message, save_error, { fail_reason = "the broker validation should reach the user" })

  buffer_support.set(buffer, { "{", '  "read": { "*": "allow" }', "}" })
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  assert(not vim.bo[buffer].modified, "an accepted document should become clean")
  eq(notification_list[#notification_list].level, vim.log.levels.INFO, { fail_reason = "an accepted document should report success" })
  eq(request_list[1].method, "permissions.document.open", { fail_reason = "the view should load through the broker" })
  eq(request_list[2].method, "permissions.document.save", { fail_reason = "the view should save through the broker" })
  eq(request_list[3].method, "permissions.document.save", { fail_reason = "the corrected document should be resubmitted" })

  local buffer_count = #vim.api.nvim_list_bufs()
  require("forge.views.permissions").open()
  eq(vim.api.nvim_get_current_buf(), buffer, { fail_reason = "reopening permissions should reuse its editor" })
  eq(#vim.api.nvim_list_bufs(), buffer_count, { fail_reason = "reopening permissions leaked a buffer" })
  eq(#vim.api.nvim_get_autocmds({ event = "BufWriteCmd", buffer = buffer }), 1, { fail_reason = "reopening duplicated save handlers" })
  buffer_support.set(buffer, { '{"draft":true}' })
  require("forge.views.permissions").open()
  eq(buffer_support.line(buffer, 1), '{"draft":true}', { fail_reason = "reopening erased an unsaved draft" })
  assert(vim.bo[buffer].modified, "reopening cleared the draft's modified state")

  defer_save = true
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  buffer_support.set(buffer, { '{"newer":true}' })
  pending_save({}, nil)
  assert(vim.bo[buffer].modified, "save completion marked newer edits saved")
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  vim.api.nvim_buf_delete(buffer, { force = true })
  pending_save({}, nil)

  defer_open = true
  require("forge.views.permissions").open()
  local unrelated = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_set_current_buf(unrelated)
  buffer_count = #vim.api.nvim_list_bufs()
  pending_open({ path = permission_path, source = permission_source }, nil)
  eq(vim.api.nvim_get_current_buf(), unrelated, { fail_reason = "delayed permissions replaced the newly selected buffer" })
  eq(#vim.api.nvim_list_bufs(), buffer_count, { fail_reason = "abandoned permissions request allocated an editor" })
end

return T
