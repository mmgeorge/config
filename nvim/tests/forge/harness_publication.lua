vim.loader.enable(false)
local client = require("forge.client")
local requests = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  requests[#requests + 1] = { params = params, callback = callback, time = vim.uv.hrtime() }
end
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(transcript)
local publications = 0
local owner = require("forge.views.harness.presentation").open({
  session_id = "publication", transcript_buffer = transcript, composer_buffer = composer,
  transcript_window = vim.api.nvim_get_current_win(), is_alive = function() return true end,
  notice = function(message) error(message) end, on_update = function() publications = publications + 1 end,
}, function(value, failure) assert(value and not failure, failure) end)
requests[1].callback({ transcript = { document = owner.document, revision = 0, block = {
  { id = "body", text = { "frame 0" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
} } })
local function burst(expected)
  local started = vim.uv.hrtime()
  for _ = 1, 100 do owner.sync() end
  assert(#requests == expected - 1, "burst published immediately")
  assert(vim.wait(1000, function() return #requests == expected end, 1), "burst did not publish")
  assert((requests[expected].time - started) / 1e6 >= 60, "refresh exceeded the 15 fps cap")
end
burst(2)
for _ = 1, 100 do owner.sync() end
local patch = {}
for revision = 1, 64 do
  patch[revision] = { document = owner.document, base = revision - 1, next = revision,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    block_edit = {}, removed_block = {}, metadata_edit = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "frame " .. revision } } },
  }
end
local observations = 0
local timer = vim.uv.new_timer()
timer:start(0, 1, vim.schedule_wrap(function()
  observations = observations + 1
  local text = vim.api.nvim_buf_get_lines(transcript, 0, -1, false)
  assert(#text == 1 and (text[1] == "frame 0" or text[1] == "frame 64"), "partial batch reached the main loop")
end))
local completed = vim.uv.hrtime()
local initial_publications = publications
requests[2].callback({ patch = patch })
assert(owner.transcript.revision == 64)
assert(publications == initial_publications + 1, "batch notified before its final revision")
assert(vim.wait(1000, function() return #requests == 3 end, 1), "in-flight burst lost its trailing update")
assert((requests[3].time - completed) / 1e6 >= 60, "trailing publication exceeded the frame cap")
requests[3].callback({ patch = {} })
timer:stop()
timer:close()
assert(observations > 0)
owner.sync()
assert(owner.close())
local closed_count = #requests
vim.wait(100, function() return false end, 1)
assert(#requests == closed_count, "closed presentation dispatched a pending refresh")
print("harness_publication: passed, atomic 64-patch batch and 15 fps trailing updates")
vim.cmd("qa!")
