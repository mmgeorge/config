-- The real Forge host's lifecycle in an integration case: redirect `stdpath("data")` and optionally
-- `stdpath("config")` to temporary directories, make `forge.builder` hand out the pinned executable
-- lease, launch through `vim.system`, and stop the host and wait until its process is collected.
--
--   local host = dofile("nvim/lua/forge/tests/support/host.lua")
--   local started = host.start({ config = true })
--   require("forge").setup({ harness = { provider = "mock" } })
--   ...
--   assert(host.stop(), "host process was not collected")
--
-- `start` registers a stop with `MiniTest.finally` before it creates any temporary directory, so a
-- failing case stops its host before the host's data directory is deleted. A case that asserts
-- collection calls `host.stop()` itself before it ends.
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local host_executable = dofile("nvim/lua/forge/tests/support/host_executable.lua")

local host = {}

local collection_timeout_ms = 5000

---@class HostStartOptions
---@field data? string data directory; a temporary directory by default
---@field config? string|boolean config directory, or true for a temporary one; unchanged by default
---@field schedule_ensure? boolean deliver the builder result on the next event-loop turn, as the real builder does; defaults to true

---@class StartedHost
---@field data string
---@field config string?
---@field executable string
---@field client table the `forge.client` module

---@param options? HostStartOptions
---@return StartedHost
function host.start(options)
  options = options or {}
  MiniTest.finally(function() pcall(host.stop) end)
  local data = options.data or temp.directory("data")
  local config = options.config == true and temp.directory("config") or options.config or nil
  local executable = host_executable()
  local original_stdpath
  original_stdpath = stub(vim.fn, "stdpath", function(kind)
    if kind == "data" then return data end
    if kind == "config" and config then return config end
    return original_stdpath(kind)
  end)
  local schedule_ensure = options.schedule_ensure ~= false
  stub(package.loaded, "forge.builder", { ensure = function(callback)
    local result = { ok = true, path = executable }
    if schedule_ensure then vim.schedule(function() callback(result) end) else callback(result) end
    return function() end
  end })
  local client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
  return { data = data, config = config, executable = executable, client = client }
end

---Stops the host if one is running and waits for its process to be collected.
---@param options? { client?: table, timeout_ms?: integer } `client` defaults to the loaded `forge.client`
---@return boolean collected whether no host process remains
function host.stop(options)
  options = options or {}
  local client = options.client or package.loaded["forge.client"]
  if not client or not client._client then return true end
  client.host_stop()
  return vim.wait(options.timeout_ms or collection_timeout_ms, function()
    return client._client == nil or client._client.process == nil
  end, 5)
end

return host
