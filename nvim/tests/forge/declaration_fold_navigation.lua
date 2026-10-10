vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.nodes")
local replica = buffer.open("declaration-fold-navigation", {})
local window = vim.api.nvim_get_current_win()
local previous_foldopen = vim.o.foldopen
vim.o.foldopen = "hor,search"
require("vim_options")
assert(vim.o.foldopen == "search", "horizontal fold opening remained enabled")
local declaration_id = "plan:design:file:test:declaration"
assert(buffer.apply_snapshot(replica, { document = replica.document, revision = 0, block = {
  { id = "source", text = { "File", "#[derive(Debug)]", "pub enum ConfigError {", "  /// Rejects invalid dimensions.", "  InvalidArena,", "}", "tail" },
    metadata = { target = {}, decoration = {}, editable_region = {}, fold = {
      { id = "plan:design:file:test", start = { row = 0, column = 0 }, ["end"] = { block = "source", position = { row = 7, column = 0 } }, closed = false },
      { id = declaration_id, start = { row = 2, column = 0 }, heading_start = { block = "source", position = { row = 1, column = 0 } },
        ["end"] = { block = "source", position = { row = 6, column = 0 } }, closed = true,
        text = { { text = "pub enum ConfigError { ... }", capture = "Normal" } } },
    } } },
} }).kind == "Applied")
vim.api.nvim_win_set_buf(window, replica.buffer)
folds.attach(replica, window)
local function toggle()
  assert(folds.toggle_heading(replica, window, {
    include_body = function(id) return id:match(":declaration$") ~= nil end,
    on_toggled = function(id, closed) _G.declaration_choice = { id = id, closed = closed } end,
  }))
end
vim.keymap.set("n", "<Tab>", toggle, { buffer = replica.buffer })
vim.api.nvim_win_set_cursor(window, { 3, 4 })
assert(folds.closed(replica, declaration_id))
vim.api.nvim_feedkeys("l", "xt", false)
assert(folds.closed(replica, declaration_id), "rightward movement automatically opened the enum")
assert(vim.o.foldopen == "search", "fold attachment changed the user's global option")
assert(vim.fn.maparg("l", "n") == "" and vim.fn.maparg("h", "n") == "", "fold attachment installed movement mappings")
toggle()
assert(not folds.closed(replica, declaration_id))
vim.api.nvim_win_set_cursor(window, { 5, 2 })
toggle()
assert(folds.closed(replica, declaration_id) and not folds.closed(replica, "plan:design:file:test"), "member Tab closed the file instead of its enum")
assert(vim.api.nvim_win_get_cursor(window)[1] == 3, "closing an enum retained a hidden member cursor")
assert(_G.declaration_choice.id == declaration_id and _G.declaration_choice.closed)
vim.api.nvim_win_set_cursor(window, { 2, 0 })
toggle()
assert(not folds.closed(replica, declaration_id), "attribute Tab did not reopen its declaration")
vim.api.nvim_win_set_cursor(window, { 7, 0 })
assert(not folds.toggle_heading(replica, window, { include_body = function(id) return id:match(":declaration$") ~= nil end }),
  "ordinary source Tab closed its enclosing file")
buffer.close(replica)
vim.o.foldopen = previous_foldopen
print("declaration_fold_navigation: passed")
