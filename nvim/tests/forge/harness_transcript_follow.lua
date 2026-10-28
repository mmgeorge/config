vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request_for
local original_accepting = client.host_accepting
local request_list = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  callback = dofile("nvim/tests/forge/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  request_list[#request_list + 1] = { params = params, callback = callback }
end

local function snapshot(document, revision, line_list)
  local block_list = {}
  for row, line in ipairs(line_list) do
    block_list[#block_list + 1] = { id = "row:" .. row, text = { line },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
    if line:find("Working", 1, true) == 1 then
      block_list[#block_list].metadata.status = { row = 0, animated = true }
    end
  end
  return { document = document, revision = revision, block = block_list }
end

local owner
local success, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  vim.wo[window].wrap = true
  vim.cmd("belowright 3new")
  local composer = vim.api.nvim_get_current_buf()
  local composer_window = vim.api.nvim_get_current_win()
  local line_list = {}
  for row = 1, 60 do line_list[row] = "response row " .. row end
  line_list[#line_list + 1] = "Working · 1s"
  line_list[#line_list + 1] = "Inspecting the current thought"
  owner = require("forge.views.harness.presentation").open({
    session_id = "follow-test", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local opening = table.remove(request_list, 1)
  opening.callback({ transcript = snapshot(opening.params.document, 0, line_list) })
  assert(vim.wait(1000, function() return owner.ready end, 1), "opening did not finish")
  assert(vim.api.nvim_get_current_win() == composer_window, "opening stole composer focus")
  assert(vim.api.nvim_win_get_cursor(window)[1] == #line_list, "opening did not position the initial tail")

  local revision = 0
  local function take_refresh()
    owner.sync()
    assert(vim.wait(1000, function()
      return #request_list > 0 and request_list[#request_list].params.operation == "sync"
    end, 1), "missing timeline refresh")
    return table.remove(request_list)
  end
  local function refresh()
    local pending = take_refresh()
    revision = revision + 1
    pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  end
  local function expect_position(target_window, expected, label)
    local actual = vim.api.nvim_win_get_cursor(target_window)
    assert(vim.deep_equal(actual, expected), label .. ": " .. vim.inspect(actual))
  end

  vim.api.nvim_win_set_cursor(window, { #line_list, 4 })
  local thought_cursor = vim.api.nvim_win_get_cursor(window)
  line_list[#line_list] = "Inspecting the current thought and its updated status"
  refresh()
  expect_position(window, thought_cursor, "thought snapshot moved the cursor to its end")
  assert(vim.api.nvim_get_current_win() == composer_window, "thought snapshot stole composer focus")

  local pending = take_refresh()
  line_list[#line_list] = string.rep("wrapped thought ", 80) .. "end"
  revision = revision + 1
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = #line_list, next_rows = #line_list, base_blocks = #line_list, next_blocks = #line_list,
    block_edit = {}, removed_block = {},
    text_edit = { { start_row = #line_list - 1, removed_rows = 1, text = { line_list[#line_list] } } },
    metadata_edit = { { block = "row:" .. #line_list, row_count = 1,
      metadata = { target = {}, decoration = {}, editable_region = {} } } },
  } } })
  expect_position(window, thought_cursor, "wrapped thought patch moved the cursor")

  owner.follow_tail()
  local previous_tail = vim.api.nvim_win_get_cursor(window)
  line_list[#line_list + 1] = "new output after the thought"
  refresh()
  expect_position(window, previous_tail, "append resumed automatic following")

  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  vim.cmd("normal! zt")
  local reading_view = vim.fn.winsaveview()
  line_list[#line_list] = "updated background output"
  refresh()
  expect_position(window, { 12, 4 }, "focused history cursor moved")
  assert(vim.fn.winsaveview().topline == reading_view.topline, "history viewport moved")

  pending = take_refresh()
  vim.api.nvim_win_set_cursor(window, { 15, 5 })
  vim.api.nvim_set_current_win(composer_window)
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  expect_position(window, { 15, 5 }, "delayed response lost movement before publication")

  local second_window = vim.api.nvim_open_win(transcript, false, { split = "right" })
  owner.refresh_views()
  assert(vim.wait(1000, function() return #request_list > 0 end, 1), "missing second view request")
  local view_request = table.remove(request_list)
  assert(view_request.params.operation == "open_view")
  view_request.callback({})
  vim.api.nvim_win_set_cursor(second_window, { #line_list, 3 })
  line_list[#line_list] = "updated output shown in both windows"
  refresh()
  expect_position(window, { 15, 5 }, "sibling publication moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "sibling publication moved the tail cursor")
  pending = take_refresh()
  pending.callback({ patch = {} })
  expect_position(window, { 15, 5 }, "empty refresh moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "empty refresh moved the tail cursor")
  assert(vim.api.nvim_get_current_win() == composer_window, "refresh changed focused window")

  pending = take_refresh()
  revision = revision + 1
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = #line_list, next_rows = #line_list, base_blocks = #line_list, next_blocks = #line_list,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = "row:61", row_count = 1,
      metadata = { target = {}, decoration = {}, editable_region = {}, status = { row = 0, animated = false } } } },
  } } })
  expect_position(window, { 15, 5 }, "status metadata moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "status metadata moved the tail cursor")

  vim.api.nvim_win_close(second_window, true)
  assert(owner.close())
end, debug.traceback)
if owner and not owner.closed then owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1")
else print("harness_transcript_follow OK") vim.cmd("qa!") end
