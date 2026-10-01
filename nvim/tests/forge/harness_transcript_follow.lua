vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting = client.request_for, client.host_accepting
client.host_accepting = function() return true end
local requests = {}
client.request_for = function(_, method, params, callback)
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end

local function snapshot(document, revision, lines, composer)
  local blocks = {}
  for row, line in ipairs(lines) do
    blocks[#blocks + 1] = { id = "row:" .. row, text = { line }, metadata = {
      target = {}, decoration = {}, editable_region = composer and {
        { id = "composer", revision = 0, range = {
          start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #line },
        } },
      } or {},
    } }
  end
  return { document = document, revision = revision, block = blocks }
end

local owner
local ok, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  vim.wo[window].wrap = true
  vim.cmd("belowright 3new")
  local composer = vim.api.nvim_get_current_buf()
  local composer_window = vim.api.nvim_get_current_win()
  local lines = {}
  for row = 1, 60 do lines[row] = "response row " .. row end
  owner = require("forge.views.harness.presentation").open({
    session_id = "follow-test", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local opening = requests[1]
  opening.callback({ transcript = snapshot(opening.params.document, 0, lines),
    composer = snapshot(opening.params.composer, 0, { "" }, true) })
  assert(vim.api.nvim_get_current_win() == composer_window, "opening stole composer focus")
  assert(vim.api.nvim_win_get_cursor(window)[1] == #lines, "unfocused initial transcript did not follow")

  local revision = 0
  local function refresh(updated, before_response)
    owner.sync()
    local pending = requests[#requests]
    assert(pending.params.operation == "sync")
    if before_response then before_response() end
    revision = revision + 1
    pending.callback({ snapshot = snapshot(opening.params.document, revision, updated) })
  end

  vim.api.nvim_win_set_cursor(window, { 5, 2 })
  lines[#lines + 1] = "new output"
  refresh(lines)
  assert(vim.api.nvim_win_get_cursor(window)[1] == #lines,
    "composer focus did not resume following from an old transcript cursor")
  assert(vim.api.nvim_get_current_win() == composer_window, "following stole focus")

  vim.api.nvim_win_set_cursor(window, { 8, 0 })
  lines[#lines] = string.rep("wrapped output ", 80) .. "end"
  owner.sync()
  local streaming = requests[#requests]
  assert(streaming.params.operation == "sync")
  revision = revision + 1
  streaming.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = #lines, next_rows = #lines, base_blocks = #lines, next_blocks = #lines,
    block_edit = {}, removed_block = {},
    text_edit = { { start_row = #lines - 1, removed_rows = 1, text = { lines[#lines] } } },
    metadata_edit = { { block = "row:" .. #lines, row_count = 1,
      metadata = { target = {}, decoration = {}, editable_region = {} } } },
  } } })
  local tail_cursor = vim.api.nvim_win_get_cursor(window)
  assert(tail_cursor[1] == #lines and tail_cursor[2] == #lines[#lines] - 1,
    "same-row streaming did not reveal the end of a wrapped response")
  local shorter = { "short response", "last output" }
  refresh(shorter)
  assert(vim.api.nvim_win_get_cursor(window)[1] == 2, "shrinking output did not follow the new tail")

  refresh(lines)
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  vim.cmd("normal! zt")
  local saved = vim.fn.winsaveview()
  lines[#lines + 1] = "background output while reading"
  refresh(lines)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 12, 4 }), "focused reader cursor moved")
  assert(vim.fn.winsaveview().topline == saved.topline, "focused reader viewport moved")
  vim.api.nvim_win_set_cursor(window, { #lines, 3 })
  local former_tail = #lines
  lines[#lines + 1] = "output after the focused final row"
  refresh(lines)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { former_tail, 3 }),
    "focused reader at the former tail was automatically scrolled")

  vim.api.nvim_set_current_win(composer_window)
  assert(vim.wait(100, function() return vim.api.nvim_win_get_cursor(window)[1] == #lines end, 5),
    "leaving the transcript did not resume following without new output")

  vim.api.nvim_win_set_cursor(window, { 7, 2 })
  refresh(lines, function()
    vim.api.nvim_set_current_win(window)
    vim.api.nvim_win_set_cursor(window, { 15, 5 })
  end)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 15, 5 }),
    "delayed response ignored focus and cursor changes while the request was pending")

  vim.api.nvim_win_set_cursor(window, { 9, 1 })
  owner.sync()
  local pending = requests[#requests]
  assert(pending.params.operation == "sync")
  vim.api.nvim_set_current_win(composer_window)
  pending.callback({ patch = {} })
  assert(vim.api.nvim_win_get_cursor(window)[1] == #lines, "empty refresh did not resume following")
  assert(owner.close())
end, debug.traceback)
if owner and not owner.closed then owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1")
else print("harness_transcript_follow OK") vim.cmd("qa!") end
