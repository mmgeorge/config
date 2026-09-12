local M = {}
local MAX_FRAME_BYTES = 512 * 1024
local MAX_PENDING_BYTES = 8 * 1024 * 1024
local MAX_PENDING_FRAMES = 128
local MAX_BATCH_BYTES = 256 * 1024
local MAX_BATCH_FRAMES = 16

local drain

local function schedule(state)
  if state.scheduled or state.closed then return end
  state.scheduled = true
  vim.schedule(function() drain(state) end)
end

local function fail(state, message)
  state.failure = message
  state.queue, state.parts = {}, {}
  state.head, state.tail, state.bytes, state.partial_bytes = 1, 0, 0, 0
  schedule(state)
end

function M.new(options)
  assert(type(options.dispatch) == "function" and type(options.decode) == "function", "receive handlers are required")
  return {
    options = options, queue = {}, parts = {}, head = 1, tail = 0,
    bytes = 0, partial_bytes = 0, scheduled = false, closed = false,
  }
end

function M.push(state, chunk)
  if state.closed or state.failure then return false end
  if state.ending then fail(state, "stdout arrived after process completion") return false end
  if type(chunk) ~= "string" then return true end
  local offset = 1
  while offset <= #chunk do
    local newline = chunk:find("\n", offset, true)
    local finish = newline and newline - 1 or #chunk
    local length = finish - offset + 1
    if state.partial_bytes + length > MAX_FRAME_BYTES
      or state.bytes + length + (newline and 1 or 0) > MAX_PENDING_BYTES
      or state.tail - state.head + 1 >= MAX_PENDING_FRAMES
    then
      fail(state, "Forge receive admission limit exceeded")
      return false
    end
    if length > 0 then
      state.parts[#state.parts + 1] = chunk:sub(offset, finish)
      state.partial_bytes = state.partial_bytes + length
      state.bytes = state.bytes + length
      if #state.parts >= 128 then state.parts = { table.concat(state.parts) } end
    end
    if not newline then break end
    state.tail = state.tail + 1
    state.queue[state.tail] = { line = table.concat(state.parts), bytes = state.partial_bytes + 1,
      received_at = vim.uv.hrtime() }
    state.bytes = state.bytes + 1
    state.parts, state.partial_bytes = {}, 0
    offset = newline + 1
    schedule(state)
  end
  return true
end

function M.finish(state, result)
  if state.closed or state.failure then return end
  state.ending = { result = result }
  if state.partial_bytes > 0 then
    fail(state, "Forge stdout ended inside a frame")
  else
    schedule(state)
  end
end

function M.close(state)
  state.closed = true
  state.queue, state.parts = {}, {}
  state.head, state.tail, state.bytes, state.partial_bytes = 1, 0, 0, 0
  state.ending = nil
end

drain = function(state)
  state.scheduled = false
  if state.closed then return end
  if state.options.active and not state.options.active() then M.close(state) return end
  if state.failure then
    local failure = state.failure
    M.close(state)
    if state.options.failed then state.options.failed(failure) end
    return
  end
  local frames, bytes = 0, 0
  while state.head <= state.tail and frames < MAX_BATCH_FRAMES do
    local frame = state.queue[state.head]
    if frames > 0 and bytes + frame.bytes > MAX_BATCH_BYTES then break end
    local decode_started = vim.uv.hrtime()
    local ok, message, decode_error = pcall(state.options.decode, frame.line)
    local timing = { received_at = frame.received_at, bytes = frame.bytes,
      queue_wait_us = math.floor((decode_started - frame.received_at) / 1000),
      decode_us = math.floor((vim.uv.hrtime() - decode_started) / 1000) }
    if not ok or not message then
      fail(state, tostring(decode_error or message or "Invalid Forge frame"))
      return
    end
    local dispatched, failure = pcall(state.options.dispatch, message, timing)
    if not dispatched then fail(state, tostring(failure)) return end
    if state.closed then return end
    if state.failure then return end
    state.queue[state.head] = nil
    state.head = state.head + 1
    state.bytes = state.bytes - frame.bytes
    frames, bytes = frames + 1, bytes + frame.bytes
  end
  if frames > 0 and state.options.consumed then
    local ok, failure = pcall(state.options.consumed, bytes, frames)
    if not ok then fail(state, tostring(failure)) return end
  end
  if state.closed or state.failure then return end
  if state.head <= state.tail then
    schedule(state)
  else
    state.queue, state.head, state.tail = {}, 1, 0
    if state.ending then
      local result = state.ending.result
      M.close(state)
      if state.options.finished then state.options.finished(result) end
    end
  end
end

return M
