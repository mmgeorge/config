vim.loader.enable(false)
local receive = require("forge.receive")
local native_schedule = vim.schedule
local scheduled = {}
vim.schedule = function(callback) scheduled[#scheduled + 1] = callback end

local function run_one()
  assert(#scheduled == 1, "receive path queued multiple drain callbacks")
  table.remove(scheduled, 1)()
end

local ok, failure = xpcall(function()
  local messages, decoded, credits, finished = {}, 0, 0, false
  local state = receive.new({
    decode = function(line) decoded = decoded + 1 return vim.json.decode(line) end,
    dispatch = function(message) messages[#messages + 1] = message end,
    consumed = function(_, frames) credits = credits + frames end,
    finished = function(result) assert(#messages == 4096 and result.code == 0) finished = true end,
  })
  for sequence = 1, 4096 do assert(receive.push(state, tostring(sequence) .. "\n")) end
  assert(decoded == 0 and #scheduled == 1)
  receive.finish(state, { code = 0 })
  run_one()
  assert(#messages == 16 and credits == 16 and not finished)
  while #scheduled > 0 do run_one() end
  assert(finished and credits == 4096 and state.bytes == 0)
  for sequence, message in ipairs(messages) do assert(sequence == message) end

  local failures = {}
  local function new_receiver()
    return receive.new({
      decode = vim.json.decode, dispatch = function() error("invalid data reached dispatch") end,
      failed = function(message) failures[#failures + 1] = message end,
    })
  end
  state = new_receiver()
  assert(not receive.push(state, string.rep("x", 16 * 1024 * 1024 + 1)))
  run_one()
  assert(state.closed and state.bytes == 0 and #failures == 1)
  state = new_receiver()
  for _ = 1, 4096 do assert(receive.push(state, "1\n")) end
  assert(not receive.push(state, "2\n"))
  run_one()
  assert(state.closed and #failures == 2)
  local large_bytes = 0
  state = receive.new({ decode = function(line) return #line end,
    dispatch = function(length) large_bytes = large_bytes + length end })
  local large = string.rep("x", 1024 * 1024) .. "\n"
  for _ = 1, 32 do assert(receive.push(state, large)) end
  assert(state.bytes > 8 * 1024 * 1024)
  while #scheduled > 0 do run_one() end
  assert(large_bytes == 32 * 1024 * 1024 and state.bytes == 0 and not state.closed)
  state = new_receiver()
  receive.push(state, '{"partial":')
  receive.finish(state, { code = 0 })
  run_one()
  assert(state.closed and #failures == 3)

  local boundary_bytes = 0
  state = receive.new({ decode = function(line) return #line end,
    dispatch = function(length) boundary_bytes = length end })
  assert(receive.push(state, string.rep("x", 16 * 1024 * 1024 - 1) .. "\n"))
  run_one()
  assert(boundary_bytes == 16 * 1024 * 1024 - 1 and state.bytes == 0)
  state = new_receiver()
  assert(not receive.push(state, string.rep("x", 16 * 1024 * 1024) .. "\n"))
  run_one()
  assert(state.closed and #failures == 4)

  local value
  state = receive.new({ decode = vim.json.decode, dispatch = function(message) value = message end })
  local encoded = vim.json.encode({ text = "λ🙂" }) .. "\n"
  for index = 1, #encoded do assert(receive.push(state, encoded:sub(index, index))) end
  run_one()
  assert(value.text == "λ🙂" and state.bytes == 0)
  state = receive.new({ active = function() return false end, decode = vim.json.decode,
    dispatch = function() error("old process frame dispatched") end })
  receive.push(state, "1\n")
  run_one()
  assert(state.closed)
end, debug.traceback)

vim.schedule = native_schedule
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
