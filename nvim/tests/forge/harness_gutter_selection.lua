vim.loader.enable(false)
local runtime = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2))))
local host = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
assert(host > 0)
---@param source string
---@param arguments? table
local function run(source, arguments)
  return vim.rpcrequest(host, "nvim_exec_lua", source, arguments or {})
end
local success, failure = xpcall(function()
  vim.rpcrequest(host, "nvim_ui_attach", 72, 28, { rgb = true })
  run([[vim.api.nvim__inspect_cell(1, 0, 0)]])
  run([[
    local runtime = ...
    vim.opt.rtp:prepend(runtime)
    vim.loader.enable(false)
    vim.o.laststatus, vim.o.showtabline, vim.o.showmode, vim.o.ruler = 0, 0, false, false
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0x000000 })
    vim.api.nvim_set_hl(0, "Visual", { bg = 0x123456 })
    vim.api.nvim_set_hl(0, "DiffAdd", { fg = 0x00ff00, bg = 0x002800 })
    vim.api.nvim_set_hl(0, "DiffDelete", { fg = 0xff0000, bg = 0x280000 })
    local config = require("forge.infra.config")
    config.setup({})
    local client = require("forge.client")
    client.host_accepting = function() return true end
    client.host_generation = function() return 1 end
    client.subscribe = function() return function() end end
    _G.open_response = nil
    client.request_for = function(_, method, params, callback)
      if method == "harness.document" and params.operation == "open" then
        open_response = { document = params.document, callback = callback }
      elseif method == "harness.document" and params.operation == "background_terminals" then
        callback({ supported = false })
      elseif method == "health.get" then callback({ responsive = true })
      else callback({ patch = {} }) end
    end
    _G.state = require("forge.session").harness
    state.session = { id = "gutter-selection", backend = "mock", model = "mock", execution_mode = "read" }
    state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
      require("forge.views.harness.layout").open("gutter-selection")
    _G.controller = require("forge.views.harness.controller")
    controller.attach()
    vim.api.nvim_set_current_win(state.transcript_win)
    _G.presentation = require("forge.views.harness.presentation").open({
      session_id = state.session.id, transcript_buffer = state.transcript_buf,
      transcript_window = state.transcript_win, composer_buffer = state.composer_buf,
      is_alive = function() return true end, notice = function(message) error(message) end,
    }, function(owner, message) assert(owner, message) end)
    state.presentation = presentation
    _G.source = {
      "Modified src/presentation.rs +14 -7", "@@ -6,2 +6,3 @@",
      "use bevy::prelude::*;", "use bevy::text::FontSize;",
      "use bevy::transform::TransformSystems;", "",
      "    a long source line that wraps across the narrow transcript window and continues onto another screen row",
    }
    local metadata = { target = {}, editable_region = {}, decoration = {}, gutter = {}, layout = { indent = 4 } }
    for row = 2, 6 do
      local capture = row == 3 and "DiffDelete" or row == 4 and "DiffAdd" or "LineNr"
      local sign = row == 3 and "-" or row == 4 and "+" or " "
      metadata.gutter[#metadata.gutter + 1] = { position = { row = row, column = 0 }, priority = 100,
        chunk = { { text = "  6", capture = capture }, { text = "  ", capture = capture },
          { text = "7 " .. sign .. " ", capture = capture } } }
    end
    open_response.callback({ transcript = { document = open_response.document, revision = 0,
      block = { { id = "diff", text = source, metadata = metadata } } } })
    assert(presentation.ready)
    _G.prior_copy_count = 0
    vim.keymap.set("x", "<Space>l", function() prior_copy_count = prior_copy_count + 1 end,
      { buffer = state.transcript_buf })
    vim.g.clipboard = { name = "Harness fixture", copy = {
      ["+"] = function(lines, kind) _G.clipboard = { lines = lines, kind = kind } end,
      ["*"] = function() end,
    }, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }
    _G.move = function(row)
      vim.api.nvim_win_set_cursor(state.transcript_win, { row, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.transcript_buf })
    end
    _G.escape = function()
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
      vim.api.nvim_exec_autocmds("ModeChanged", {})
    end
    _G.start = function(row)
      move(row)
      vim.fn.maparg("W", "n", false, true).callback()
      assert(vim.api.nvim_get_mode().mode == "V")
    end
    vim.wo[state.transcript_win].winbar = ""
    move(3)
    assert(vim.fn.getcurpos()[4] == 0, "ordinary timeline cursor gained a gutter offset")
    start(4)
    move(6)
    assert(presentation.transcript.gutter_selection.first == 4 and presentation.transcript.gutter_selection.last == 6)
    move(3)
    assert(presentation.transcript.gutter_selection.first == 3 and presentation.transcript.gutter_selection.last == 4)
    escape()
    start(3)
    move(5)
  ]], { runtime })
  run([[
    assert(vim.api.nvim_get_mode().mode == "V", "selection mode changed between RPC calls: " .. vim.inspect(vim.api.nvim_get_mode()))
    vim.cmd("redraw!")
    local selection = presentation.transcript.gutter_selection
    for row = selection.first, selection.last do
      local position = vim.fn.screenpos(state.transcript_win, row, 1)
      local cursor_column = position.col + require("forge.gutter").bounds(presentation.transcript, row).width
      for column = position.col, cursor_column + #source[row] - 1 do
        if row ~= vim.fn.line(".") or column ~= cursor_column then
          assert(vim.api.nvim__inspect_cell(1, position.row - 1, column - 1)[2].background == 0x123456,
            "selected text or gutter cell lost Visual at " .. row .. ":" .. column)
        end
      end
    end
    escape()
    assert(presentation.transcript.gutter_selection == nil)
    vim.fn.maparg("<Space>l", "x", false, true).callback()
    assert(prior_copy_count == 1)
    start(3)
    move(6)
    vim.fn.maparg("<Space>l", "x", false, true).callback()
    assert(clipboard.kind == "V")
    for index = 1, 4 do
      local layout = require("forge.gutter").bounds(presentation.transcript, index + 2)
      local prefix = {}
      for _, insertion in ipairs(layout.insertion) do
        for _, chunk in ipairs(insertion.chunk) do prefix[#prefix + 1] = chunk.text end
      end
      assert(clipboard.lines[index] == table.concat(prefix) .. source[index + 2], "copied gutter differs from display")
    end
    assert(presentation.transcript.gutter_selection == nil and vim.api.nvim_get_mode().mode == "n")
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, true), source))
    start(6)
    vim.cmd("redraw!")
    local position = vim.fn.screenpos(state.transcript_win, 6, 1)
    assert(vim.api.nvim__inspect_cell(1, position.row - 1, position.col - 1)[2].background == 0x123456,
      "empty row gutter lost Visual")
    escape()
    start(7)
    vim.cmd("redraw!")
    position = vim.fn.screenpos(state.transcript_win, 7, 1)
    assert(vim.api.nvim__inspect_cell(1, position.row, position.col + 11)[2].background == 0x123456,
      "wrapped continuation lost Visual")
    escape()
    vim.cmd("vsplit")
    _G.other_window = vim.api.nvim_get_current_win()
    vim.api.nvim_set_current_win(state.transcript_win)
    start(4)
    vim.cmd("redraw!")
    local other = vim.fn.screenpos(other_window, 4, 1)
    assert(vim.api.nvim__inspect_cell(1, other.row - 1, other.col - 1)[2].background ~= 0x123456,
      "selection leaked into another split")
    escape()
    vim.api.nvim_win_close(other_window, true)
    start(4)
    vim.api.nvim_set_current_win(state.composer_win)
    assert(presentation.transcript.gutter_selection == nil, "buffer leave retained selection")
    assert(vim.fn.maparg("W", "n", false, true).buffer ~= 1, "composer binds gutter selection")
    escape()
    vim.api.nvim_set_current_win(state.transcript_win)
    vim.fn.maparg("?", "n", false, true).callback()
    local help = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, true), "\n")
    assert(help:find("W", 1, true) and help:find("including the diff gutter", 1, true), "help omitted W: " .. help)
    vim.fn.maparg("q", "n", false, true).callback()
    local config = require("forge.infra.config")
    config.setup({ keymaps = { harness = { visual_line_with_gutter = "gW" } } })
    vim.keymap.del("n", "W", { buffer = state.transcript_buf })
    controller.attach_transcript(state.transcript_buf)
    assert(vim.fn.maparg("gW", "n", false, true).buffer == 1)
    config.setup({ keymaps = { harness = { visual_line_with_gutter = false } } })
    vim.keymap.del("n", "gW", { buffer = state.transcript_buf })
    controller.attach_transcript(state.transcript_buf)
    assert(vim.fn.maparg("gW", "n", false, true).buffer ~= 1 and vim.fn.maparg("W", "n", false, true).buffer ~= 1)
    config.setup({})
    controller.attach_transcript(state.transcript_buf)
    start(4)
    presentation.close({ preserve_buffer = true })
    assert(presentation.selection.closed and presentation.transcript.gutter_selection == nil)
    vim.fn.maparg("<Space>l", "x", false, true).callback()
    assert(prior_copy_count == 2, "close did not restore prior clipboard mapping")
    escape()
    local rejected = false
    local pending = require("forge.views.harness.presentation").open({
      session_id = state.session.id, transcript_buffer = state.transcript_buf,
      transcript_window = state.transcript_win, composer_buffer = state.composer_buf,
      is_alive = function() return true end, notice = function(message) error(message) end,
    }, function(owner, message) rejected = owner == nil and message == "fixture rejection" end)
    state.presentation = pending
    vim.fn.maparg("W", "n", false, true).callback()
    assert(vim.api.nvim_get_mode().mode == "n", "pending presentation admitted selection")
    open_response.callback(nil, "fixture rejection")
    assert(rejected and pending.selection.closed, "rejected startup retained selection owner")
    assert(vim.fn.exists("#ForgeGutterSelection" .. state.transcript_buf) == 0,
      "rejected startup retained selection callbacks")
  ]])
end, debug.traceback)
vim.fn.jobstop(host)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_gutter_selection OK")
vim.cmd("qa!")
