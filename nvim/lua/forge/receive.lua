local M = {}
local MAX_FRAME_BYTES = 16 * 1024 * 1024
local MAX_PENDING_FRAMES = 4096
local MAX_BATCH_BYTES = 256 * 1024
local MAX_BATCH_FRAMES = 16

-- One drain callback stops after this long so redraw and input run between batches.
local BATCH_BUDGET_NS = 4 * 1000 * 1000

local drain

local function schedule(state)
  if state.scheduled or state.closed then return end
  state.scheduled = true
  vim.schedule(function() drain(state) end)
end

--- Continues a slow backlog from a timer: callbacks queued with `vim.schedule` run back to back,
--- so a rescheduled drain would hold the main loop until the queue emptied. Cheap batches keep
--- the scheduled path, because a timer tick per batch would cap throughput.
local function continue_later(state)
  if state.scheduled or state.closed then return end
  state.scheduled = true
  vim.defer_fn(function() drain(state) end, 1)
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
    if state.partial_bytes + length + (newline and 1 or 0) > MAX_FRAME_BYTES
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
    -- A failure after `finish` means the process has already exited, so `failed` receives its
    -- exit result and the owner completes the exit path instead of stopping a dead process.
    local failure, ending = state.failure, state.ending
    M.close(state)
    if state.options.failed then state.options.failed(failure, ending and ending.result) end
    return
  end
  local frames, bytes = 0, 0
  local batch_started, over_budget = vim.uv.hrtime(), false
  while state.head <= state.tail and frames < MAX_BATCH_FRAMES do
    local frame = state.queue[state.head]
    if frames > 0 and bytes + frame.bytes > MAX_BATCH_BYTES then break end
    if frames > 0 and vim.uv.hrtime() - batch_started >= BATCH_BUDGET_NS then
      over_budget = true
      break
    end
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
    if over_budget then continue_later(state) else schedule(state) end
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
