local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual))
  end
end

local function assert_true(value, message)
  if not value then error(message or "expected truthy value") end
end

local ok, failure = pcall(function()
  local request_list = {}
  local notification_list = {}
  local save_error = "line 3: invalid permission decision"
  local permission_path = vim.fn.tempname() .. ".json"
  local permission_source = '{\n  "read": { "*": "allow" }\n}\n'

  package.loaded["diff_review.harness.client"] = {
    start = function(callback) callback({}, nil) end,
    request = function(method, params, callback)
      request_list[#request_list + 1] = { method = method, params = params }
      if method == "permissions.open" then
        callback({ path = permission_path, source = permission_source }, nil)
      elseif params.source:find('"invalid"', 1, true) then
        callback(nil, save_error)
      else
        callback({ path = permission_path, source = params.source }, nil)
      end
    end,
  }
  package.loaded["diff_review.infra.notifications"] = {
    error = function(message, title)
      notification_list[#notification_list + 1] = { level = "error", message = message, title = title }
    end,
    info = function(message, title)
      notification_list[#notification_list + 1] = { level = "info", message = message, title = title }
    end,
  }
  package.loaded["diff_review.views.permissions"] = nil

  require("diff_review.views.permissions").open()
  local buffer = vim.api.nvim_get_current_buf()
  assert_equals(vim.api.nvim_buf_get_name(buffer), permission_path, "permissions should expose the broker-owned path")
  assert_equals(vim.bo[buffer].buftype, "acwrite", "permissions should save through broker validation")
  assert_equals(vim.bo[buffer].filetype, "json", "permissions should use JSON editing")
  assert_true(not vim.bo[buffer].modified, "opening permissions should begin clean")

  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "{", '  "read": { "*": "invalid" }', "}" })
  vim.bo[buffer].modified = true
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  assert_true(vim.bo[buffer].modified, "a rejected document should remain modified")
  assert_equals(notification_list[#notification_list].level, "error", "a rejected document should report an error")
  assert_equals(notification_list[#notification_list].message, save_error, "the broker validation should reach the user")

  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "{", '  "read": { "*": "allow" }', "}" })
  vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = buffer })
  assert_true(not vim.bo[buffer].modified, "an accepted document should become clean")
  assert_equals(notification_list[#notification_list].level, "info", "an accepted document should report success")
  assert_equals(request_list[1].method, "permissions.open", "the view should load through the broker")
  assert_equals(request_list[2].method, "permissions.save", "the view should save through the broker")
  assert_equals(request_list[3].method, "permissions.save", "the corrected document should be resubmitted")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
