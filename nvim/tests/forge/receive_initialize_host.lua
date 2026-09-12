vim.loader.enable(false)

local root = vim.fs.normalize(vim.fn.getcwd())
local executable = require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host before running this fixture")

local protocol = require("forge.protocol")
local receive = require("forge.receive")
local data_root = vim.fn.tempname()
assert(vim.fn.mkdir(data_root, "p") == 1)

local function initialize_host(attempt)
  local process, initialized, exited = nil, false, nil
  local consumed_bytes, consumed_frames = 0, 0
  local receiver = receive.new({
    decode = protocol.decode_message,
    dispatch = function(message)
      if message.id == 1 then
        assert(not message.error, vim.inspect(message.error))
        initialized = true
        process:write(protocol.encode_request(2, "shutdown", {}))
        process:write(nil)
      end
    end,
    consumed = function(bytes, frames)
      consumed_bytes, consumed_frames = consumed_bytes + bytes, consumed_frames + frames
      process:write(protocol.encode_request(0, "transport.consumed", {
        bytes = consumed_bytes,
        frames = consumed_frames,
      }))
    end,
    finished = function(result) exited = result end,
  })
  process = vim.system({ executable }, {
    text = true,
    stdin = true,
    stdout = function(_, chunk)
      if chunk then receive.push(receiver, chunk) end
    end,
    stderr = function() end,
  }, function(result)
    receive.finish(receiver, result)
  end)
  process:write(protocol.encode_request(1, "initialize", {
    protocol_version = protocol.VERSION,
    recovery_directory = vim.fs.joinpath(data_root, "forge", "recovery", "github", "v1"),
    status_ignored_directory = vim.fs.joinpath(data_root, "forge", "status-ignored"),
  }))
  assert(vim.wait(3000, function() return initialized and exited ~= nil end, 200),
    "receive lifecycle timed out on attempt " .. attempt)
  assert(exited.code == 0, "receive lifecycle exited unexpectedly: " .. vim.inspect(exited))
end

local ok, err = xpcall(function()
  for attempt = 1, 10 do initialize_host(attempt) end
end, debug.traceback)
vim.fn.delete(data_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit 1")
end
print("receive_initialize_host: ten framed handshakes passed")
vim.cmd("qa!")
