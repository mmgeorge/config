local fixture = assert(arg[1], "pass the snapshot emitted by the call_rows_highlight Rust test")
local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
assert(child > 0)
local success, failure = xpcall(function()
  vim.rpcrequest(child, "nvim_ui_attach", 100, 40, { rgb = true })
  vim.rpcrequest(child, "nvim_exec_lua", [=[
    local root, fixture = ...
    vim.opt.runtimepath:prepend(root .. "/nvim")
    local snapshot = vim.json.decode(table.concat(vim.fn.readfile(fixture), "\n"))
    for _, block in ipairs(snapshot.block) do
      for _, fold in ipairs(block.metadata.fold or {}) do fold.closed = false end
    end
    local buffer = require("forge.buffer")
    local native = vim.api.nvim_get_current_buf()
    local replica = buffer.open(snapshot.document, { buffer = native, generated = true, filetype = "ForgePlan",
      expected_changedtick = vim.api.nvim_buf_get_changedtick(native) })
    assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
    require("forge.decorations").attach(replica)
    require("forge.infra.highlights").setup()
    vim.api.nvim_set_hl(0, "@variable", { fg = "#ffffff" })
    for name, expected in pairs({ Projectile = 0x5bff94, projectile_alias = 0x5bff94, MAX_SHOTS = 16777215 }) do
      local row, column
      for index, text in ipairs(vim.api.nvim_buf_get_lines(native, 0, -1, false)) do
        if vim.trim(text) == name then row, column = index, text:find(name, 1, true) - 1 break end
      end
      assert(row, "Accesses reference is missing: " .. name)
      vim.api.nvim_win_set_cursor(0, { row, column })
      vim.cmd("normal! zt")
      vim.cmd("redraw!")
      local screen = vim.fn.screenpos(0, row, column + 1)
      local cell = vim.api.nvim__inspect_cell(1, screen.row - 1, screen.col - 1)
      assert(cell and cell[2] and cell[2].foreground == expected, name .. " has the wrong rendered color: " .. vim.inspect(cell))
    end
    require("forge.decorations").detach(replica)
    buffer.close(replica)
  ]=], { vim.fn.getcwd(), fixture })
end, debug.traceback)
vim.fn.jobstop(child)
assert(success, failure)
print("plan_review_type_access_render: passed at 100x40")
vim.cmd("qa!")
