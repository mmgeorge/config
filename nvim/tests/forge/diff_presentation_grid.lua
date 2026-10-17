local path = debug.getinfo(1, "S").source:sub(2)
if not vim.g.forge_diff_grid_child then
  for _, width in ipairs({ 40, 120 }) do
    for _, indent in ipairs({ 2, 6 }) do
      local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
      assert(child > 0)
      local success, failure = xpcall(function()
        vim.rpcrequest(child, "nvim_ui_attach", width, 30, { rgb = true })
        vim.rpcrequest(child, "nvim_exec_lua", [[
          local path, indent = ...
          vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
          vim.g.forge_diff_grid_child = true
          vim.g.forge_diff_grid_indent = indent
          dofile(path)
        ]], { path, indent })
      end, debug.traceback)
      vim.fn.jobstop(child)
      assert(success, failure)
    end
  end
  print("diff_presentation_grid: 40/120 columns and 2/6 cell layouts passed")
  return
end

local buffer = require("forge.buffer")
local owner = buffer.open("diff-grid")
vim.api.nvim_win_set_buf(0, owner.buffer)
vim.wo.number, vim.wo.relativenumber = false, false
vim.wo.signcolumn, vim.wo.statuscolumn, vim.wo.foldcolumn = "yes:1", "%s", "0"
vim.wo.wrap, vim.wo.winbar = false, ""
vim.o.showtabline, vim.o.laststatus = 0, 0
local indent = vim.g.forge_diff_grid_indent
local function metadata()
  return { target = {}, decoration = {}, editable_region = {}, layout = { indent = indent } }
end
local body = metadata()
body.gutter = { { position = { row = 0, column = 0 }, priority = 100,
  chunk = { { text = "  9   19  + ", capture = "Normal" } } } }
assert(buffer.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
  { id = "heading", text = { "@@ -10,2 +20,2 @@ fn run" }, metadata = metadata() },
  { id = "body", text = { "new value" }, metadata = body },
} }).kind == "Applied")
vim.cmd("redraw!")
local function screen(row)
  local result = {}
  for column = 1, vim.o.columns do result[#result + 1] = vim.fn.screenstring(row, column) end
  return table.concat(result)
end
assert(screen(1):find("@@ -10,2 +20,2 @@ fn run", 1, true) == indent + 1, screen(1))
assert(screen(2):find("  9   19  + new value", 1, true) == indent + 1, screen(2))
buffer.close(owner)
