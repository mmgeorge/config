local fixture = assert(arg[1], "pass the snapshot emitted by the narrative_symbols_prefer Rust test")
local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
assert(child > 0)
local success, failure = xpcall(function()
  vim.rpcrequest(child, "nvim_ui_attach", 100, 32, { rgb = true })
  vim.rpcrequest(child, "nvim_exec_lua", [=[
    local root, fixture = ...
    vim.opt.runtimepath:prepend(root .. "/nvim")
    local snapshot = vim.json.decode(table.concat(vim.fn.readfile(fixture), "\n"))
    local buffer = require("forge.buffer")
    local native = vim.api.nvim_get_current_buf()
    local replica = buffer.open(snapshot.document, { buffer = native, generated = true, filetype = "ForgePlan",
      expected_changedtick = vim.api.nvim_buf_get_changedtick(native) })
    assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
    vim.api.nvim_set_hl(0, "@type", { fg = "#00ff00" })
    vim.api.nvim_set_hl(0, "@function", { fg = "#0000ff" })
    local row, text
    for index, line in ipairs(vim.api.nvim_buf_get_lines(native, 0, -1, false)) do
      if line:find("Use `Health`", 1, true) then row, text = index, line break end
    end
    assert(row)
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.cmd("normal! zt")
    vim.cmd("redraw!")
    for name, capture in pairs({ Health = "@type", configure_game = "@function" }) do
      local column = assert(text:find(name, 1, true)) - 1
      local location = assert(buffer.locate(replica, row - 1, column))
      assert(location.target:find("plan:symbol:", 1, true), "paragraph target masked the inline symbol")
      local found = false
      local node = replica.sequence.node[location.block]
      for _, decoration in ipairs(node.entry.metadata.decoration) do
        if decoration.capture == capture and decoration.range.start.column <= column
          and decoration.range["end"].column > column then found = true end
      end
      assert(found, "missing rendered declaration-kind decoration for " .. name)
      vim.api.nvim_win_set_cursor(0, { row, column })
      vim.cmd("redraw!")
      local screen = vim.fn.screenpos(0, row, column + 1)
      assert(screen.row > 0, "symbol is not visible")
      local cell = vim.api.nvim__inspect_cell(1, screen.row - 1, screen.col - 1)
      local expected = capture == "@type" and 65280 or 255
      assert(cell and cell[2] and cell[2].foreground == expected, "rendered symbol lost its declaration-kind color")
    end
    local column = assert(text:find("External", 1, true)) - 1
    local location = assert(buffer.locate(replica, row - 1, column))
    assert(location.target:find("plan:symbol:", 1, true), "unresolved inline reference has no definition target")
    buffer.close(replica)
  ]=], { vim.fn.getcwd(), fixture })
end, debug.traceback)
vim.fn.jobstop(child)
assert(success, failure)
print("plan_review_narrative_render: passed at 100x32")
vim.cmd("qa!")
