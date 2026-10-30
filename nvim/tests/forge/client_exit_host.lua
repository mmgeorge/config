vim.loader.enable(false)

local fixture = vim.g.forge_exit_fixture
local test_path = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
local runtime_path = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(test_path)))

local function write_record(path, record)
  vim.fn.writefile({ vim.json.encode(record) }, path)
end

local function child_command(config)
  return {
    vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE",
    "--cmd", "set rtp+=" .. vim.fn.fnameescape(runtime_path),
    "--cmd", "lua vim.g.forge_exit_fixture = " .. vim.inspect(config),
    "-l", test_path,
  }
end

if fixture and fixture.mode == "host" then
  local protocol = require("forge.protocol")
  write_record(fixture.root .. "/host-started.json", { pid = vim.fn.getpid() })
  for line in io.lines() do
    local request = vim.json.decode(line)
    if request.method == "host.initialize" then
      io.write(vim.json.encode({ id = request.id, result = { protocol_version = protocol.VERSION } }) .. "\n")
      io.flush()
    elseif request.method == "host.shutdown" then
      if fixture.case ~= "unresponsive" then
        vim.wait(350, function() return false end, 10)
        write_record(fixture.root .. "/host-drained.json", { pid = vim.fn.getpid(), drained = true })
        io.write(vim.json.encode({ id = request.id, result = vim.empty_dict() }) .. "\n")
        io.flush()
        vim.cmd("qa!")
      end
    end
  end
  vim.cmd("qa!")
  return
end

if fixture and fixture.mode == "editor" then
  local original_stdpath = vim.fn.stdpath
  vim.fn.stdpath = function(kind)
    if kind == "data" or kind == "config" or kind == "cache" or kind == "state" then
      return fixture.root .. "/" .. kind
    end
    return original_stdpath(kind)
  end
  package.loaded["forge.builder"] = {
    ensure = function(callback)
      if fixture.case == "startup" then return function() end end
      vim.schedule(function() callback({ ok = true, path = vim.v.progpath }) end)
      return function() end
    end,
  }
  local client = require("forge.client")
  local host_process
  local warning_list = {}
  vim.notify = function(message) warning_list[#warning_list + 1] = tostring(message) end
  client._host_set_launcher_for_test(function(_, options, on_exit)
    host_process = vim.system(child_command({ mode = "host", root = fixture.root, case = fixture.case }), options, on_exit)
    return host_process
  end)
  if fixture.case ~= "empty" then
    local started, failure = false, nil
    client.host_start(function(_, error) started, failure = true, error end)
    if fixture.case ~= "startup" then
      assert(vim.wait(5000, function() return started end, 10), "host did not initialize")
      assert(not failure, failure)
    end
  end
  if fixture.case == "draining" then client.host_stop("already draining") end
  vim.api.nvim_create_autocmd("VimLeavePre", { callback = function()
    local collected = client._client == nil or client._client.process == nil
    local result = collected and host_process and host_process:wait(0) or nil
    write_record(fixture.root .. "/editor-leaving.json", {
      collected = collected,
      host_exit_code = result and result.code or nil,
      host_exit_signal = result and result.signal or nil,
      warning_list = warning_list,
    })
  end })
  vim.cmd("qa!")
  return
end

local roots = {}
local ok, failure = xpcall(function()
  for _, case in ipairs({ "graceful", "draining", "empty", "startup", "unresponsive" }) do
    local root = vim.fn.tempname() .. "-forge-client-exit-" .. case
    assert(vim.fn.mkdir(root, "p") == 1)
    roots[#roots + 1] = root
    local result = vim.system(child_command({ mode = "editor", root = root, case = case }), { text = true }):wait(10000)
    assert(result.code == 0, case .. " editor failed: " .. (result.stderr or ""))
    local leaving = vim.json.decode(table.concat(vim.fn.readfile(root .. "/editor-leaving.json"), "\n"))
    assert(leaving.collected, case .. " editor left before host collection")
    if case == "graceful" or case == "draining" then
      assert(vim.fn.filereadable(root .. "/host-drained.json") == 1,
        case .. " editor exited before the delayed host persisted its shutdown sentinel")
      assert(leaving.host_exit_code == 0, case .. " host did not exit and collect normally")
    elseif case == "unresponsive" then
      assert(vim.fn.filereadable(root .. "/host-drained.json") == 0,
        "unresponsive host falsely reported graceful persistence")
      assert(leaving.host_exit_code ~= nil and
        (leaving.host_exit_code ~= 0 or leaving.host_exit_signal ~= 0),
        "unresponsive host was not forcibly collected")
      assert(table.concat(leaving.warning_list, "\n"):find("Forge drain deadline exceeded", 1, true),
        "forced host collection must report the existing drain failure")
    else
      assert(vim.fn.filereadable(root .. "/host-started.json") == 0,
        case .. " unexpectedly launched a host")
    end
  end
end, debug.traceback)
for _, root in ipairs(roots) do
  local started_path = root .. "/host-started.json"
  if vim.fn.filereadable(started_path) == 1 then
    local started = vim.json.decode(table.concat(vim.fn.readfile(started_path), "\n"))
    if vim.uv.kill(started.pid, 0) then vim.uv.kill(started.pid, 15) end
  end
  vim.fn.delete(root, "rf")
end
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("client_exit_host: real VimLeavePre graceful and forced collection; empty and startup exits passed")
vim.cmd("qa!")
