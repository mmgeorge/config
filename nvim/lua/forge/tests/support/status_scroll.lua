local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
assert(child > 0, "could not start isolated status-scroll editor")
local success, failure = xpcall(function()
  vim.rpcrequest(child, "nvim_ui_attach", 100, 30, { rgb = true })
  local target = vim.rpcrequest(child, "nvim_exec_lua", [[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    vim.o.mouse = "a"
    local layout = require("forge.views.harness.layout")
    local buffer = require("forge.buffer")
    local transcript_buffer, transcript_window, composer_buffer, composer_window = layout.open("mouse-status")
    local transcript = buffer.open("mouse-status", { buffer = transcript_buffer, generated = true, preserve_view = true,
      expected_changedtick = vim.api.nvim_buf_get_changedtick(transcript_buffer) })
    assert(buffer.apply_snapshot(transcript, { document = "mouse-status", revision = 0, block = {
      { id = "history", text = vim.tbl_map(tostring, vim.fn.range(1, 100)),
        metadata = { target = {}, decoration = {}, editable_region = {} } },
    } }).kind == "Applied")
    local status = require("forge.views.harness.status_view").new({
      transcript = transcript, transcript_window = transcript_window, composer_window = composer_window,
      alive = function() return true end, notice = function(message) error(message) end,
      recover = function() error("unexpected recovery") end,
    })
    status:render({ message_notice = { text = "Status first row\nStatus second row\nStatus final row", failed = false } })
    vim.api.nvim_win_set_cursor(transcript_window, { 20, 0 })
    vim.api.nvim_win_call(transcript_window, function() vim.cmd("normal! zt") end)
    vim.cmd("redraw!")
    _G.status_scroll_fixture = { status = status, transcript = transcript, transcript_window = transcript_window,
      composer_window = composer_window, composer_buffer = composer_buffer }
    return { position = vim.api.nvim_win_get_position(status.window), window = status.window }
  ]], {})
  for _, direction in ipairs({ "down", "up", "right", "left" }) do
    vim.rpcrequest(child, "nvim_input_mouse", "wheel", direction, "", 0, target.position[1], target.position[2] + 4)
    vim.rpcrequest(child, "nvim_command", "redraw!")
    vim.rpcrequest(child, "nvim_exec_lua", [[
      local fixture = _G.status_scroll_fixture
      local view = vim.api.nvim_win_call(fixture.status.window, vim.fn.winsaveview)
      assert(view.topline == 1 and view.skipcol == 0 and view.leftcol == 0 and view.topfill == 0,
        "mouse wheel scrolled status: " .. vim.inspect(view))
      assert(vim.api.nvim_get_current_win() == fixture.composer_window, "status wheel moved focus")
      assert(vim.api.nvim_win_get_cursor(fixture.transcript_window)[1] == 20, "status wheel moved transcript")
    ]], {})
  end
  local details = vim.rpcrequest(child, "nvim_exec_lua", [[
    local fixture = _G.status_scroll_fixture
    fixture.status:render({ message_notice = { text = ("Status overflow\n"):rep(80), failed = false } })
    fixture.status:open_details()
    vim.cmd("redraw!")
    return vim.api.nvim_win_get_position(fixture.status.details.window)
  ]], {})
  vim.rpcrequest(child, "nvim_input_mouse", "wheel", "down", "", 0, details[1] + 2, details[2] + 4)
  vim.rpcrequest(child, "nvim_command", "redraw!")
  vim.rpcrequest(child, "nvim_exec_lua", [[
    local fixture = _G.status_scroll_fixture
    assert(vim.api.nvim_win_call(fixture.status.details.window, vim.fn.winsaveview).topline > 1,
      "status details lost mouse scrolling")
    fixture.status:close()
    require("forge.buffer").close(fixture.transcript)
    vim.api.nvim_buf_delete(fixture.composer_buffer, { force = true })
  ]], {})
end, debug.traceback)
vim.fn.jobstop(child)
assert(success, failure)
