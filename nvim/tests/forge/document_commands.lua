vim.loader.enable(false)
local replica = require("forge.buffer")
local input = require("forge.input")
local config = require("forge.infra.config")
local commands = require("forge.document_commands")
local session = replica.open("command-fixture")
local owner, view
local clipboard
vim.g.clipboard = { name = "Forge fixture", copy = {
  ["+"] = function(lines, kind) clipboard = { lines = lines, kind = kind } end,
  ["*"] = function() end,
}, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }
local ok, failure = xpcall(function()
  config.setup({ keymaps = { status = { open = { "go", "gO" }, refresh = false, discard = "X" } } })
  vim.api.nvim_set_current_buf(session.buffer)
  local pending_closed = false
  local pending_owner = commands.attach(session, { view = "diff", title = "Pending comparison", handler = {
    close = function() pending_closed = true end,
    open = function() error("pending document admitted a content action") end,
  } })
  vim.fn.maparg("go", "n", false, true).callback()
  vim.fn.maparg("?", "n", false, true).callback()
  assert(vim.bo.filetype == "ForgeHelp", "pending document did not expose help")
  vim.fn.maparg("q", "n", false, true).callback()
  vim.fn.maparg("q", "n", false, true).callback()
  assert(pending_closed, "pending document trapped its close command")
  pending_owner.close()
  assert(replica.apply_snapshot(session, { document = session.document, revision = 0, block = {
    { id = "source", text = { "first", "second" }, metadata = { decoration = {}, target = {}, editable_region = {},
      fold = { { id = "source-fold", start = { row = 0, column = 0 },
        ["end"] = { block = "source", position = { row = 2, column = 0 } }, closed = false } },
      gutter = {
        { position = { row = 0, column = 0 }, chunk = { { text = "1 │ ", capture = "LineNr" } }, priority = 100 },
        { position = { row = 1, column = 0 }, chunk = { { text = "2 │ ", capture = "LineNr" } }, priority = 100 },
      },
    } },
  } }).kind == "Applied")
  vim.api.nvim_set_current_buf(session.buffer)
  view = input.open(session, 0)
  local opened, refreshed, old_yank, old_open, newer_open = 0, 0, 0, 0, 0
  vim.keymap.set("n", "go", function() old_open = old_open + 1 end, { buffer = session.buffer })
  vim.keymap.set("x", "<Space>l", function() old_yank = old_yank + 1 end, { buffer = session.buffer })
  owner = commands.attach(session, { view = "diff", title = "Forge comparison", handler = {
    open = function() opened = opened + 1 end,
    refresh = function() refreshed = refreshed + 1 end,
    close = function() end,
    discard = function() error("read-only comparison exposed discard") end,
  } })
  local function local_map(key, mode)
    local mapping = vim.fn.maparg(key, mode or "n", false, true)
    return mapping.buffer == 1 and mapping or nil
  end
  assert(local_map("go") and local_map("gO"), "configured open list was not bound")
  assert(not local_map("o") and not local_map("<CR>") and not local_map("."), "default open keys survived list replacement")
  assert(not local_map("R") and not local_map("X"), "disabled or unavailable command was bound")
  local_map("go").callback()
  assert(opened == 1 and refreshed == 0)
  local_map("<Tab>").callback()
  assert(vim.fn.foldclosed(1) == 1, "native fold did not close")
  local_map("<Tab>").callback()
  assert(vim.fn.foldclosed(1) == -1, "native fold did not reopen")
  local_map("W").callback()
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  local_map("<Space>l", "x").callback()
  assert(clipboard and vim.deep_equal(clipboard.lines, { "1 │ first", "2 │ second", "" }), "clipboard omitted native gutters: " .. vim.inspect(clipboard))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "first", "second" }), "selection changed source rows")
  local_map("<Space>l", "x").callback()
  assert(old_yank == 1, "temporary clipboard mapping was not restored")
  local_map("W").callback()
  local newer_yank = 0
  vim.keymap.set("x", "<Space>l", function() newer_yank = newer_yank + 1 end, { buffer = session.buffer })
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  vim.api.nvim_exec_autocmds("ModeChanged", {})
  local_map("<Space>l", "x").callback()
  assert(newer_yank == 1, "selection cleanup replaced a newer clipboard mapping")
  local_map("?").callback()
  local help = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, true), "\n")
  assert(help:find("go, gO", 1, true) and not help:find("Discard", 1, true), "help differs from live command bindings")
  assert(not help:find("Forge comparison", 1, true), "help duplicates its title in the command rows")
  local help_width = 1
  for _, line in ipairs(vim.api.nvim_buf_get_lines(0, 0, -1, true)) do
    help_width = math.max(help_width, vim.fn.strdisplaywidth(line) + 2)
  end
  assert(vim.api.nvim_win_get_width(0) == math.min(help_width, vim.o.columns - 4), "help ignores command content width")
  vim.fn.maparg("q", "n", false, true).callback()
  vim.keymap.set("n", "gO", function() newer_open = newer_open + 1 end, { buffer = session.buffer })
  owner.close()
  local_map("go").callback()
  local_map("gO").callback()
  assert(old_open == 1 and newer_open == 1, "close replaced prior or newer user mappings")
  assert(not local_map("?"), "close retained owned help binding")
end, debug.traceback)
if owner then owner.close() end
if view then input.close(view) end
replica.close(session)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("document_commands OK")
vim.cmd("qa!")
