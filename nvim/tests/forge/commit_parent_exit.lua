vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")

local repository = vim.fn.tempname()
vim.fn.mkdir(repository, "p")
local config_root = vim.fn.getcwd() .. "/nvim"
local parent_process, git_process, parent_channel

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments),
    { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end

local function run()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Test" })
  git({ "config", "user.email", "forge@example.test" })
  vim.fn.writefile({ "committed" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  git({ "commit", "--quiet", "-m", "original" })
  local original_head = git({ "rev-parse", "HEAD" })
  vim.fn.writefile({ "staged" }, repository .. "/tracked.txt")
  git({ "add", "tracked.txt" })
  local original_index = git({ "write-tree" })

  for ordinal, shutdown in ipairs({ "quit", "terminate", "abort" }) do
    local server = vim.fn.has("win32") == 1
      and ("\\\\.\\pipe\\forge-commit-parent-%d-%d"):format(vim.fn.getpid(), ordinal)
      or vim.fn.tempname()
    local setup = ([[
      require("forge.git.write").execute = function(_, action)
        vim.g.commit_parent_test_action = action
        return function() end
      end
      require("forge.integrations.commit").commit({
        win = vim.api.nvim_get_current_win(), workspace = %q, amend = true,
      })
    ]]):format(repository)
    parent_process = vim.system({ vim.v.progpath, "--headless", "--clean", "--noplugin",
      "--listen", server, "-c", "set runtimepath^=" .. vim.fn.fnameescape(config_root),
      "-c", "lua " .. setup }, { text = true, stdout = true, stderr = true })
    parent_channel = nil
    assert(vim.wait(5000, function()
      local connected, channel = pcall(vim.fn.sockconnect, "pipe", server, { rpc = true })
      if connected and channel ~= 0 then parent_channel = channel return true end
      return false
    end, 50), "parent editor server did not start")
    local action
    assert(vim.wait(5000, function()
      action = vim.rpcrequest(parent_channel, "nvim_exec_lua", "return vim.g.commit_parent_test_action", {})
      return type(action) == "table"
    end, 50), "parent did not prepare commit editor")

    local result
    git_process = vim.system({ "git", "-C", repository, "commit", "--amend", "--only" }, {
      text = true, stdout = true, stderr = true, timeout = 15000,
      env = { GIT_EDITOR = action.command, NVIM = action.nvim_server },
    }, function(outcome) result = outcome end)
    assert(vim.wait(5000, function()
      return vim.rpcrequest(parent_channel, "nvim_exec_lua",
        "return vim.b.forge_commit_buffer == true", {})
    end, 50), "commit editor did not open")
    assert(vim.uv.fs_stat(repository .. "/.git/index.lock"), "amend did not hold index lock")

    local started = vim.uv.hrtime()
    if shutdown == "terminate" then
      parent_process:kill(9)
    elseif shutdown == "quit" then
      vim.rpcnotify(parent_channel, "nvim_command", "qa!")
    else
      vim.rpcrequest(parent_channel, "nvim_exec_lua",
        "vim.fn.maparg('<C-q>', 'n', false, true).callback()", {})
    end
    assert(vim.wait(5000, function() return result ~= nil end, 20), shutdown .. " left Git waiting")
    assert(result.code ~= 0 and result.code ~= 124, shutdown .. " did not abort Git promptly")
    assert(not vim.uv.fs_stat(repository .. "/.git/index.lock"), shutdown .. " retained index.lock")
    assert(git({ "rev-parse", "HEAD" }) == original_head, shutdown .. " changed HEAD")
    assert(git({ "write-tree" }) == original_index, shutdown .. " changed staged content")
    print(("commit parent %s released Git and lock in %.0f ms"):format(shutdown,
      (vim.uv.hrtime() - started) / 1e6))
    if shutdown == "abort" then vim.rpcnotify(parent_channel, "nvim_command", "qa!") end
    parent_process:wait(2000)
    pcall(vim.fn.chanclose, parent_channel)
    parent_process, git_process, parent_channel = nil, nil, nil
  end
end

local ok, failure = xpcall(run, debug.traceback)
if parent_process then parent_process:kill(9) parent_process:wait(2000) end
if git_process then git_process:kill(9) git_process:wait(2000) end
if parent_channel then pcall(vim.fn.chanclose, parent_channel) end
vim.fn.delete(repository, "rf")
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("commit_parent_exit OK")
vim.cmd("qa!")
