-- Unit tests for `forge.views.harness.status_view` (status_view.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harness_status_view"] = function()
  vim.o.columns, vim.o.lines = 120, 42
  require("forge.infra.highlights").setup()
  local forge_buffer = require("forge.buffer")
  local layout = require("forge.views.harness.layout")
  local StatusView = require("forge.views.harness.status_view")
  local transcript_buffer, transcript_window, composer_buffer, composer_window = layout.open("status-test")
  local transcript = forge_buffer.open("status-test", { buffer = transcript_buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(transcript_buffer), preserve_view = true })
  assert(forge_buffer.apply_snapshot(transcript, { document = "status-test", revision = 0, block = {
    { id = "history", text = vim.tbl_map(tostring, vim.fn.range(1, 100)), metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }).kind == "Applied")
  local commands = require("forge.shared.view_command_set").new()
  require("forge.shared.view_command_set").register(commands, "cancel", function() end)
  require("forge.shared.view_command_set").register(commands, "reopen_question", function() end)
  local view = StatusView.new({ transcript = transcript, transcript_window = transcript_window,
    composer_window = composer_window, alive = function() return true end,
    notice = function(message) error(message) end, recover = function() error("unexpected recovery") end })
  view:render({}, commands)
  local revision = 0
  local function publish(lines, animated)
    revision = revision + 1
    local blocks = #lines > 0 and { { id = "status", text = lines, metadata = {
      target = {}, decoration = {}, editable_region = {}, status = { row = 0, animated = animated or false, hint = "working" },
    } } } or {}
    local applied
    view:apply({ document = "status-test:status", revision = revision, block = blocks }, function(ok) applied = ok end)
    wait_until(function() return applied ~= nil end, "status view did not settle its publication", { timeout_ms = 3000 })
    assert(applied)
  end
  publish({})
  eq(view.window, nil)
  vim.api.nvim_set_current_win(transcript_window)
  vim.api.nvim_win_set_cursor(transcript_window, { 20, 0 })
  vim.api.nvim_win_call(transcript_window, function() vim.fn.winrestview({ topline = 10 }) end)
  local cursor = vim.api.nvim_win_get_cursor(transcript_window)
  vim.api.nvim_set_current_win(composer_window)
  local separator = vim.wo[transcript_window].fillchars
  publish({ "Working · 3s", "Current thought" }, true)
  eq(vim.api.nvim_get_current_win(), composer_window)
  eq(cursor, vim.api.nvim_win_get_cursor(transcript_window))
  assert(vim.api.nvim_win_get_position(view.window)[1] < vim.api.nvim_win_get_position(composer_window)[1])
  eq(vim.api.nvim_win_get_config(view.window).focusable, false)
  eq(vim.api.nvim_win_get_height(view.window), 2)
  vim.cmd("wincmd w")
  assert(vim.api.nvim_get_current_win() ~= view.window)
  vim.cmd("wincmd w")
  assert(vim.api.nvim_get_current_win() == composer_window, vim.inspect({current=vim.api.nvim_get_current_win(),status=view.window,composer=composer_window,windows=vim.api.nvim_tabpage_list_wins(0)}))
  assert(table.concat(view.content.lines, "\n"):find("<C%-c> interrupt"))
  local window = view.window
  local height = vim.api.nvim_win_get_height(window)
  vim.api.nvim_win_call(window, function()
    vim.fn.winrestview({ lnum = 2, col = 3, topline = 2 })
  end)
  vim.api.nvim_exec_autocmds("WinScrolled", { pattern = tostring(window) })
  local pinned = vim.api.nvim_win_call(window, vim.fn.winsaveview)
  assert(pinned.topline == 1 and pinned.lnum == 1 and pinned.col == 0 and pinned.skipcol == 0
    and pinned.leftcol == 0 and pinned.topfill == 0, "status viewport did not stay pinned")
  assert(vim.api.nvim_get_current_win() == composer_window, "pinning status changed focus")
  assert(vim.deep_equal(cursor, vim.api.nvim_win_get_cursor(transcript_window)), "pinning status moved transcript")
  vim.wait(260, function() return false end, 20)
  assert(view.window == window and vim.api.nvim_win_get_height(window) == height)
  publish({ "Working · 4s" })
  eq(vim.api.nvim_win_get_height(view.window), 1)
  vim.api.nvim_set_current_win(transcript_window)
  vim.cmd("normal! 20GV25G")
  local anchor, selected = vim.fn.getpos("v"), vim.api.nvim_win_get_cursor(transcript_window)
  publish({ "Working · 5s", "Selection remains in history" })
  vim.api.nvim_win_call(view.window, function() vim.fn.winrestview({ lnum = 2, col = 2, topline = 2 }) end)
  vim.api.nvim_exec_autocmds("WinScrolled", { pattern = tostring(view.window) })
  eq(anchor, vim.fn.getpos("v"))
  eq(selected, vim.api.nvim_win_get_cursor(transcript_window))
  eq(vim.fn.mode(), "V")
  vim.cmd("normal! \27")
  vim.api.nvim_set_current_win(composer_window)
  view:render({ message_notice = { text = ("long λ notice\n"):rep(200) .. "last notice", failed = true } }, commands)
  assert(vim.api.nvim_buf_get_lines(view.buffer, -2, -1, false)[1]:find("last notice", 1, true))
  assert(vim.api.nvim_win_get_height(transcript_window) >= 1)
  view:open_details()
  eq(vim.api.nvim_get_current_buf(), view.details.buffer)
  assert(vim.api.nvim_buf_line_count(view.details.buffer) > 200)
  keymap.invoke("q")
  eq(vim.api.nvim_get_current_win(), composer_window)
  view:render({}, commands)
  view:suspend(true)
  assert(view.window == nil and vim.wo[transcript_window].fillchars == separator)
  view:suspend(false)
  assert(view.window ~= nil)
  publish({})
  assert(view.window == nil and vim.wo[transcript_window].fillchars == separator)
  view:render({ rename_status = "Renaming" }, commands)
  assert(view.window ~= nil)
  view:render({}, commands)
  eq(view.window, nil)
  -- Question previews reflow in the real status window without stealing composer focus.
  local question_context = { questions = { revision = 1, questions = {
    { content = { question = ("Choose the function behavior "):rep(12) }, resolution = { kind = "pending" } },
    { content = { question = ("Choose the empty input behavior "):rep(12) }, resolution = { kind = "pending" } },
  } } }
  local preview_cursor = vim.api.nvim_win_get_cursor(transcript_window)
  vim.cmd("topleft vnew")
  local preview_neighbor = vim.api.nvim_get_current_win()
  local preview_buffer = vim.api.nvim_get_current_buf()
  vim.api.nvim_win_set_width(preview_neighbor, 52)
  vim.api.nvim_set_current_win(composer_window)
  view:render(question_context, commands)
  assert(view.window ~= nil and #view.content.lines == 5)
  eq(view.content.lines[3]:find("  │  ", 1, true), 1)
  eq(view.content.lines[5]:find("     ", 1, true), 1)
  local narrow_preview = view.content.lines[2]
  vim.api.nvim_win_set_width(preview_neighbor, 20)
  view:resize()
  assert(view.content.lines[2] ~= narrow_preview, "resizing did not reflow the question preview: " .. vim.inspect({
    columns = vim.o.columns, width = vim.api.nvim_win_get_width(view.window), previous = narrow_preview,
    current = view.content.lines[2],
  }))
  eq(view.content.lines[5]:sub(-#"…"), "…")
  for index = 2, 5 do
    local line_width = vim.api.nvim_win_call(view.window, function()
      return vim.fn.strdisplaywidth(view.content.lines[index])
    end)
    assert(line_width <= vim.api.nvim_win_get_width(view.window), view.content.lines[index])
  end
  assert(vim.api.nvim_win_text_height(view.window, { start_row = 1 }).all == 4,
    "question branches wrapped into more than two display lines each")
  assert(vim.api.nvim_get_current_win() == composer_window, "question preview changed focus")
  assert(vim.deep_equal(preview_cursor, vim.api.nvim_win_get_cursor(transcript_window)), "question preview moved transcript cursor")
  view:render({}, commands)
  assert(view.window == nil, "resolved questions left the status window open")
  vim.api.nvim_win_close(preview_neighbor, true)
  vim.api.nvim_buf_delete(preview_buffer, { force = true })
  vim.api.nvim_buf_delete(view.buffer, { force = true })
  assert(view.closed)
  view:close()
  assert(not vim.api.nvim_buf_is_valid(view.buffer) and view.timer == nil)
  forge_buffer.close(transcript)
  vim.api.nvim_buf_delete(composer_buffer, { force = true })
  dofile("nvim/lua/forge/tests/support/status_scroll.lua")
end

T["timeline_status"] = function()
  local forge_buffer = require("forge.buffer")
  local health = require("forge.views.harness.health")
  local spinner = require("forge.render.harness.timeline_status")
  local commands = require("forge.shared.view_command_set").new()
  local transcript_buffer, transcript_window, composer_buffer, composer_window = require("forge.views.harness.layout").open("status-transitions")
  local transcript = forge_buffer.open("status-transitions", { buffer = transcript_buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(transcript_buffer), preserve_view = true })
  eq(forge_buffer.apply_snapshot(transcript, { document = transcript.document, revision = 0, block = {} }).kind, "Applied")
  local notices = {}
  local status = require("forge.views.harness.status_view").new({ transcript = transcript,
    transcript_window = transcript_window, composer_window = composer_window, alive = function() return true end,
    notice = function(message) notices[#notices + 1] = message end, recover = function() end })
  status:render({}, commands)
  local revision = 0
  local function publish(text, animated)
    revision = revision + 1
    local result
    status:apply({ document = transcript.document .. ":status", revision = revision, block = {
      { id = "status", text = { text }, metadata = { target = {}, decoration = {}, editable_region = {},
        status = { row = 0, animated = animated } } },
    } }, function(ok) result = ok end)
    wait_until(function() return result ~= nil end, "status view did not settle its publication", { timeout_ms = 3000 })
    assert(result)
  end
  local function mark()
    return vim.api.nvim_buf_get_extmark_by_id(status.buffer, status.animation_namespace, 1, { details = true })
  end
  assert(spinner.frame_at(0) == "⠋" and spinner.frame_at(120) == "⠙")
  for _, phase in ipairs({ "Working", "Planning", "Revising plan", "Implementing", "Verifying", "Resolving", "Saving exchange", "Waiting for 2 agents" }) do
    publish(phase, true)
    assert(status.timer and mark()[3].virt_text[1][1] ~= "◷")
  end
  for _, phase in ipairs({ "Paused", "Waiting for your answer", "Waiting for plan review", "Planning stopped", "Saving exchange failed" }) do
    publish(phase, false)
    assert(status.timer == nil and mark()[3].virt_text[1][1] == "◷")
  end
  publish("Working", true)
  local state = { status = { kind = "working" } }
  status:render({ status_notice = { text = "Waiting for your approval", waiting = true, animated = false } }, commands)
  assert(status.timer == nil and mark()[3].virt_text[1][1] == "◷")
  assert(table.concat(status.content.lines):find("Waiting for your approval", 1, true))
  state.connection_error = "Waiting for provider or tool · 30s without update"
  status:render({ status_notice = health.notice(state) }, commands)
  assert(table.concat(status.content.lines):find("30s without update", 1, true))
  state.host_error = "Stopped · lost connection"
  status:render({ status_notice = health.notice(state) }, commands)
  eq(status.timer, nil)
  status:render({}, commands)
  local first = mark()[3].virt_text[1][1]
  local text = buffer.lines(status.buffer)
  local calls = { clear = 0, read = 0 }
  local clear, read = vim.api.nvim_buf_clear_namespace, vim.api.nvim_buf_get_lines
  vim.api.nvim_buf_clear_namespace = function(...) calls.clear = calls.clear + 1 return clear(...) end
  vim.api.nvim_buf_get_lines = function(...) calls.read = calls.read + 1 return read(...) end
  wait_until(function() return mark()[3].virt_text[1][1] ~= first end, "status spinner did not advance", { timeout_ms = 600 })
  vim.api.nvim_buf_clear_namespace, vim.api.nvim_buf_get_lines = clear, read
  assert(calls.clear == 0 and calls.read == 0)
  eq(text, buffer.lines(status.buffer))
  local result
  status:apply({ document = transcript.document .. ":status", revision = revision + 1, block = {
    { id = "bad", text = { "bad" }, metadata = { target = {}, decoration = {}, editable_region = {}, status = { row = 2, animated = true } } },
  } }, function(ok) result = ok end)
  wait_until(function() return result ~= nil end, "status view did not settle the invalid publication", { timeout_ms = 3000 })
  assert(not result)
  eq(text, buffer.lines(status.buffer))
  status:close()
  forge_buffer.close(transcript)
  vim.api.nvim_buf_delete(composer_buffer, { force = true })
end

return T
