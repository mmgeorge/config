package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local commit = require("forge.integrations.commit")
local git_write = require("forge.git.write")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function run()
  local original_notify = vim.notify
  local original_system = vim.system
  local original_execute = git_write.execute
  local notification_list = {}
  local process_request_list = {}

  local function restore()
    vim.notify = original_notify
    vim.system = original_system
    git_write.execute = original_execute
    commit._admission_pending = false
    if commit._active and commit._active.console and vim.api.nvim_buf_is_valid(commit._active.console) then
      pcall(vim.api.nvim_buf_delete, commit._active.console, { force = true })
    end
    commit._active = nil
  end

  local ok, error_message = xpcall(function()
    vim.notify = function(message, level, opts)
      notification_list[#notification_list + 1] = {
        message = tostring(message),
        level = level,
        opts = opts,
      }
    end
    vim.system = function(command, opts, callback)
      process_request_list[#process_request_list + 1] = {
        command = vim.deepcopy(command),
        opts = opts,
        callback = callback,
      }
      return {}
    end
    git_write.execute = function(workspace, action, callback, progress)
      assert_true(workspace == "D:/commit-lifecycle", "native commit lost its workspace")
      assert_true(action.kind == "commit_editor", "commit bypassed native editor admission")
      assert_true(type(action.command) == "string" and action.command ~= "", "native commit lost editor command")
      assert_true(type(action.nvim_server) == "string" and action.nvim_server ~= "", "native commit lost RPC server")
      process_request_list[#process_request_list + 1] = {
        command = { "native", "commit" }, callback = callback, progress = progress,
      }
      return function() end
    end

    local function saw_notification(message)
      for _, notification in ipairs(notification_list) do
        if notification.message == message then return true end
      end
      return false
    end

    local function command_count(expected_command_key)
      local count = 0
      for _, request in ipairs(process_request_list) do
        if command_key(request.command) == expected_command_key then count = count + 1 end
      end
      return count
    end

    local function command_request(expected_command_key, occurrence)
      local matched = 0
      for _, request in ipairs(process_request_list) do
        if command_key(request.command) == expected_command_key then
          matched = matched + 1
          if matched == occurrence then return request end
        end
      end
      return nil
    end

    local function wait_for(condition, message)
      assert_true(vim.wait(1000, condition, 10), message)
    end

    local root = "D:/commit-lifecycle"
    local win = vim.api.nvim_get_current_win()
    local original_buf = vim.api.nvim_win_get_buf(win)

    commit._active = nil
    commit._admission_pending = false
    commit.commit({ win = win })
    commit.commit({ win = win })

    local root_command_key = "git\trev-parse\t--show-toplevel"
    local commit_command_key = "native\tcommit"
    assert_true(command_count(root_command_key) == 1, "duplicate root lookup started while commit admission was pending")
    assert_true(saw_notification("A commit is already in progress"), "duplicate starting commit was not rejected")

    command_request(root_command_key, 1).callback({ code = 0, stdout = root .. "\n", stderr = "" })
    wait_for(function() return command_count(commit_command_key) == 1 end, "git commit did not start after root resolution")
    assert_true(commit._active ~= nil, "active commit state was not installed")
    assert_true(not commit._admission_pending, "root admission remained pending after commit start")
    command_request(commit_command_key, 1).progress("native pre-commit progress\n", "stderr")
    assert_true(
      table.concat(vim.api.nvim_buf_get_lines(commit._active.console, 0, -1, false), "\n"):find("native pre-commit progress", 1, true) ~= nil,
      "native commit progress was not rendered in the commit console"
    )

    commit.commit({ win = win })
    assert_true(command_count(commit_command_key) == 1, "duplicate git commit process started while a commit was active")

    commit._active.aborted = true
    command_request(commit_command_key, 1).callback({ ok = false, code = 1, output = "" })
    wait_for(function() return commit._active == nil end, "active commit state did not clear after exit")
    assert_true(vim.api.nvim_win_get_buf(win) == original_buf, "commit exit did not restore the borrowed window")

    vim.cmd('vsplit')
    local closed_window = vim.api.nvim_get_current_win()
    commit.commit({ win = closed_window })
    vim.api.nvim_win_close(closed_window, true)
    command_request(root_command_key, 2).callback({ code = 0, stdout = root })
    wait_for(function() return not commit._admission_pending end, 'closed window left admission pending')
    assert_true(command_count(commit_command_key) == 1, 'native commit started with a closed host window')
    assert_true(saw_notification('No diff window to host the commit'), 'closed host window was not reported')
  end, debug.traceback)

  restore()
  if not ok then error(error_message, 0) end
end

local ok, error_message = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit")
end
vim.cmd("qa!")
