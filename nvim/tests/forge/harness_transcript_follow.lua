vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request
local original_accepting = client.host_is_accepting
local request_list = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/tests/forge/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  if params.operation == "resize" then callback({}) return end
  request_list[#request_list + 1] = { params = params, callback = callback }
end

local completion_blocks = {}
local function snapshot(document, revision, line_list)
  local block_list = {}
  for row, line in ipairs(line_list) do
    block_list[#block_list + 1] = { id = "row:" .. row, text = { line },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
    if line:find("Working", 1, true) == 1 then
      block_list[#block_list].metadata.status = { row = 0, animated = true }
    end
  end
  vim.list_extend(block_list, vim.deepcopy(completion_blocks))
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
    transcript_window = window, composer_window = composer_window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local opening = table.remove(request_list, 1)
  opening.callback({ transcript = snapshot(opening.params.document, 0, line_list) })
  assert(vim.wait(1000, function() return owner.ready end, 1), "opening did not finish")
  assert(vim.api.nvim_get_current_win() == composer_window, "opening stole composer focus")
  assert(vim.api.nvim_win_get_cursor(window)[1] == #line_list, "opening did not position the initial tail")

  local revision = 0
  local function await_publication()
    assert(vim.wait(1000, function()
      return owner.failure or owner.transcript.revision == revision and not owner.applying
    end, 1), owner.transcript.diagnostic or "transcript publication did not finish")
    assert(not owner.failure, owner.failure)
    assert(owner.transcript.status == "Applied", owner.transcript.diagnostic)
  end
  local function take_refresh()
    owner.sync()
    assert(vim.wait(1000, function()
      return #request_list > 0 and request_list[#request_list].params.operation == "sync"
    end, 1), "missing timeline refresh: " .. vim.inspect({ failure = owner.failure, requests = request_list,
      applying = owner.applying, syncing = owner.syncing }))
    return table.remove(request_list)
  end
  local function refresh()
    local pending = take_refresh()
    revision = revision + 1
    pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
    await_publication()
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

  local layout = require("forge.views.harness.layout")
  vim.api.nvim_win_set_config(second_window, { relative = "editor", row = 0, col = 0,
    width = 30, height = 3, style = "minimal" })
  layout.configure_transcript_window(window)
  vim.wo[window].winbar = "Harness"
  layout.attach_scroll_boundary(transcript, window)
  vim.api.nvim_win_set_height(window, 10)

  ---@param identity string
  ---@param lines string[]
  local function begin_response(identity, lines)
    completion_blocks = {
      { id = identity .. ":prompt", text = { "Prompt for " .. identity }, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity .. ":prompt", kind = "message", lifecycle = "settled", display = "full",
          default_display = "full", generation = 1, more = false, content_revision = 1,
          loaded_rows = 1, loaded_bytes = 24, order = 0 },
        layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessPrompt" } },
      } },
      { id = identity .. ":summary", text = { "Exchange activity" }, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity, kind = "exchange", lifecycle = "live", display = "full", generation = 1,
          default_display = "full", more = false, content_revision = 1, loaded_rows = 1, loaded_bytes = 17, order = 0 },
      } },
      { id = identity .. ":response", text = lines, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity .. ":response", kind = "message", lifecycle = "live",
          display = "full", default_display = "full", generation = 1, more = false, content_revision = 1,
          loaded_rows = #lines, loaded_bytes = #table.concat(lines, "\n"), order = 1 },
        layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessResponse" } },
      } },
    }
    refresh()
    vim.api.nvim_win_set_cursor(window, { 12, 4 })
    vim.api.nvim_win_call(window, function() vim.cmd("normal! zt") end)
  end

  local function finish_response()
    completion_blocks[2].metadata.node.lifecycle = "settled"
    refresh()
  end

  local function expect_prompt_at_top(target_window, label)
    vim.cmd("redraw")
    vim.api.nvim_win_call(target_window, function()
      local view = vim.fn.winsaveview()
      assert(view.topline == #line_list + 1 and view.skipcol == 0, label .. ": " .. vim.inspect(view))
    end)
    assert(vim.api.nvim_get_current_win() == composer_window, label .. ": focus changed")
  end

  local function expect_revealed(label)
    local first = #line_list + 2
    local last = vim.api.nvim_buf_line_count(transcript)
    vim.cmd("redraw")
    vim.api.nvim_win_call(window, function()
      assert(vim.fn.line("w0") <= first and vim.fn.line("w$") == last, label)
      local start = vim.fn.winsaveview().topline - 1
      local measured = vim.api.nvim_win_text_height(window, { start_row = start, end_row = last - 1 })
      assert(measured.all <= vim.api.nvim_win_get_height(window) - 1, label .. ": wrapped tail clipped")
    end)
    assert(vim.api.nvim_get_current_win() == composer_window, label .. ": focus changed")
  end

  begin_response("live-progress", { "Running tool" })
  completion_blocks[3].metadata.node.kind = "tool"
  completion_blocks[3].metadata.node.parent = completion_blocks[2].metadata.node.id
  completion_blocks[3].metadata.layout = nil
  refresh()
  expect_revealed("live tool without a response did not follow from the composer")
  expect_position(window, { vim.api.nvim_buf_line_count(transcript), #"Running tool" - 1 },
    "live tool did not position the tail")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  pending = take_refresh()
  pending.callback({ patch = {} })
  expect_position(window, { 12, 4 }, "unchanged publication replayed live following")

  pending = take_refresh()
  vim.api.nvim_set_current_win(window)
  vim.cmd("normal! vll")
  local progress_cursor, progress_anchor = vim.api.nvim_win_get_cursor(window), vim.fn.getpos("v")
  local progress_view = vim.fn.winsaveview()
  completion_blocks[3].text = { "Running tool with more output" }
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_position(window, progress_cursor, "live update ignored transcript focus at publication")
  assert(vim.fn.mode() == "v" and vim.deep_equal(vim.fn.getpos("v"), progress_anchor),
    "live following changed the reading selection")
  assert(vim.fn.winsaveview().topline == progress_view.topline, "live following moved the focused viewport")
  vim.cmd("normal! \27")

  pending = take_refresh()
  vim.api.nvim_set_current_win(composer_window)
  completion_blocks[3].text = { "Tool finished", "Next tool is running", string.rep("output ", 20) .. "end" }
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_revealed("live following used request-time rather than publication-time focus")
  assert(vim.api.nvim_win_get_cursor(second_window)[1] == vim.api.nvim_buf_line_count(transcript),
    "live following omitted the sibling transcript window")

  begin_response("short", { "First response line", "Last response line" })
  expect_position(window, { 12, 4 }, "streaming response moved history")
  finish_response()
  expect_revealed("completion outside transcript did not reveal the response from history")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  refresh()
  expect_position(window, { 12, 4 }, "repeated completion replayed reveal")

  begin_response("focused", { "Response while selecting" })
  pending = take_refresh()
  vim.api.nvim_set_current_win(window)
  vim.cmd("normal! vll")
  local selected_cursor, selected_anchor = vim.api.nvim_win_get_cursor(window), vim.fn.getpos("v")
  local selected_view = vim.fn.winsaveview()
  completion_blocks[2].metadata.node.lifecycle = "settled"
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_position(window, selected_cursor, "completion moved focused selection")
  assert(vim.fn.mode() == "v" and vim.deep_equal(vim.fn.getpos("v"), selected_anchor),
    "completion changed the selection anchor or mode")
  assert(vim.fn.winsaveview().topline == selected_view.topline, "completion moved focused viewport")
  vim.cmd("normal! \27")
  vim.api.nvim_set_current_win(composer_window)
  refresh()
  expect_position(window, selected_cursor, "leaving transcript retried a skipped reveal")

  begin_response("focus-left", { "Focus changed before publication" })
  vim.api.nvim_set_current_win(window)
  pending = take_refresh()
  vim.api.nvim_set_current_win(composer_window)
  completion_blocks[2].metadata.node.lifecycle = "settled"
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_revealed("completion used request-time focus instead of publication-time focus")

  local height = vim.api.nvim_win_get_height(window) - 1
  local exact = {}
  for row = 1, height - 2 do exact[row] = "Response line " .. row end
  begin_response("exact-fit", exact)
  finish_response()
  expect_revealed("exact-height response did not fit")
  expect_prompt_at_top(second_window, "small sibling window did not reveal its prompt")

  exact[#exact + 1] = "One row beyond the available height"
  begin_response("one-row-over", exact)
  finish_response()
  expect_prompt_at_top(window, "exchange one row taller than the window hid its prompt")

  local long_response = {}
  for row = 1, 100 do long_response[row] = "Response line " .. row end
  vim.wo[window].scrolloff = 3
  begin_response("long", long_response)
  finish_response()
  expect_prompt_at_top(window, "100-line response did not start at its prompt")
  expect_prompt_at_top(second_window, "100-line response did not respect sibling geometry")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  refresh()
  expect_position(window, { 12, 4 }, "repeated long completion replayed reveal")

  begin_response("too-tall", { string.rep("wrapped response ", vim.api.nvim_win_get_width(window) * height) })
  finish_response()
  expect_prompt_at_top(window, "oversized wrapped response did not start at its prompt")

  begin_response("folded", { "Preview of a longer response" })
  completion_blocks[3].metadata.node.display = "preview"
  completion_blocks[3].metadata.node.more = true
  finish_response()
  expect_position(window, { 12, 4 }, "folded response was mistaken for a complete response")

  begin_response("metadata-only", { "Response was already streamed" })
  completion_blocks[2].metadata.node.lifecycle = "settled"
  pending = take_refresh()
  revision = revision + 1
  local count = vim.api.nvim_buf_line_count(transcript)
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = count, next_rows = count, base_blocks = #line_list + 3, next_blocks = #line_list + 3,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = completion_blocks[2].id, row_count = 1, metadata = completion_blocks[2].metadata } },
  } } })
  await_publication()
  expect_revealed("metadata-only completion did not reveal the streamed answer")

  begin_response("switched", { "Historical response in another timeline" })
  owner.select_agent("child")
  assert(vim.wait(1000, function()
    local request = request_list[#request_list]
    if request and request.params.operation == "select_agent" then return true end
    if request and request.params.operation == "sync" then
      table.remove(request_list).callback({ patch = {} })
    end
    return false
  end, 1), "missing timeline selection")
  local selection_request = table.remove(request_list)
  assert(selection_request.params.operation == "select_agent")
  selection_request.callback({})
  finish_response()
  expect_position(window, { 12, 4 }, "switching timelines replayed historical completion")
  refresh()
  expect_position(window, { 12, 4 }, "switched history was not baselined")

  vim.api.nvim_win_close(second_window, true)
  assert(owner.close())
end, debug.traceback)
if owner and not owner.closed then owner.close() end
client.request, client.host_is_accepting = original_request, original_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1")
else print("harness_transcript_follow OK") vim.cmd("qa!") end
