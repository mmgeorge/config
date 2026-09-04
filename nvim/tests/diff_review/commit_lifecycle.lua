package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local commit = require("diff_review.integrations.commit")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function run()
  local original_notify = vim.notify
  local original_system = vim.system
  local original_pending = mutation_coordinator.pending
  local notification_list = {}
  local process_request_list = {}

  local function restore()
    vim.notify = original_notify
    vim.system = original_system
    mutation_coordinator.pending = original_pending
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

    local mutation_pending = false
    mutation_coordinator.pending = function() return mutation_pending end

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
    local commit_command_key = "git\tcommit"
    assert_true(command_count(root_command_key) == 1, "duplicate root lookup started while commit admission was pending")
    assert_true(saw_notification("A commit is already in progress"), "duplicate starting commit was not rejected")

    command_request(root_command_key, 1).callback({ code = 0, stdout = root .. "\n", stderr = "" })
    wait_for(function() return command_count(commit_command_key) == 1 end, "git commit did not start after root resolution")
    assert_true(commit._active ~= nil, "active commit state was not installed")
    assert_true(not commit._admission_pending, "root admission remained pending after commit start")

    commit.commit({ win = win })
    assert_true(command_count(commit_command_key) == 1, "duplicate git commit process started while a commit was active")

    commit._active.aborted = true
    command_request(commit_command_key, 1).callback({ code = 1, stdout = "", stderr = "" })
    wait_for(function() return commit._active == nil end, "active commit state did not clear after exit")
    assert_true(vim.api.nvim_win_get_buf(win) == original_buf, "commit exit did not restore the borrowed window")

    mutation_pending = true
    commit.commit({ win = win })
    assert_true(command_count(root_command_key) == 2, "pending-mutation commit did not resolve its repository root")
    command_request(root_command_key, 2).callback({ code = 0, stdout = root .. "\n", stderr = "" })
    wait_for(function() return not commit._admission_pending end, "pending-mutation rejection did not release admission")
    assert_true(command_count(commit_command_key) == 1, "git commit started while index mutations were pending")
    assert_true(commit._active == nil, "pending-mutation rejection installed active commit state")
    assert_true(
      saw_notification("Commit is unavailable while Git index changes are pending"),
      "pending-mutation commit rejection was not reported"
    )
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
