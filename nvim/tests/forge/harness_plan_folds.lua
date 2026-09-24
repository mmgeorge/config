vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local replica = buffer.open("plan-fold-regression", {})
local function block(id, rows, indent, endpoint)
  local metadata = { target = {}, decoration = {}, editable_region = {}, fold = {}, gutter = {} }
  for row = 0, #rows - 1 do
    metadata.gutter[#metadata.gutter + 1] = { position = { row = row, column = 0 },
      chunk = { { text = string.rep(" ", indent), capture = "Normal" } }, priority = 200 }
  end
  if endpoint then
    metadata.fold[1] = { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = endpoint, position = { row = 1, column = 0 } }, closed = false }
  end
  return { id = id, text = rows, metadata = metadata }
end
local snapshot = { document = replica.document, revision = 0, block = {
  block("prompt", { "▸ Request plan changes" }, 0),
  block("summary", { "▸ Planned for 21s, 4 tools called" }, 0, "last-tool"),
  block("plan", { "▸ Plan changes requested: Migrate cloud diagnostics to Rust · revision 1" }, 2, "comment"),
  block("comment", { "CloudServiceSettings rename to CloudServiceConfig" }, 4),
  block("commentary", { "↳ Replacing the declaration and its owning task." }, 2),
  block("tools", { "▸ Ran 4 tools" }, 2, "last-tool"),
  block("first-tool", { "• harness_plan_read", "  └ Plan read accepted" }, 4),
  block("last-tool", { "• harness_plan_submit" }, 4),
  block("response", { "Resolved the configuration name." }, 0),
} }
snapshot.block[4].metadata.gutter[1].chunk[1].text = "    ◦ "
assert(snapshot.block[4].metadata.gutter[1].chunk[1].capture == "Normal")
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, replica.buffer)
folds.attach(replica, window)
state.transcript_buf, state.transcript_win = replica.buffer, window
state.presentation = { transcript = replica, toggle_tool = function() return false end }
local opened = 0
state.presentation.activate = function() opened = opened + 1 end
local function tab(row)
  vim.api.nvim_win_set_cursor(window, { row, 0 })
  controller.toggle_activity()
end
tab(3)
assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosedend(3) == 4, "plan fold crossed its details")
assert(vim.fn.foldtextresult(3):sub(1, 5) == "  ▸", "collapsed plan lost virtual indentation")
assert(vim.fn.foldclosed(5) == -1, "plan fold swallowed following commentary")
tab(3)
assert(vim.fn.foldclosed(3) == -1, "plan heading did not reopen its own fold")
tab(4)
tab(5)
assert(vim.fn.foldclosed(2) == -1, "detail or commentary Tab closed the enclosing exchange")
tab(6)
assert(vim.fn.foldclosed(6) == 6 and vim.fn.foldclosedend(6) == 9, "tool group range differs")
assert(vim.fn.foldtextresult(6) == "  ▸ Ran 4 tools", "collapsed tools lost indentation")
tab(2)
assert(vim.fn.foldclosed(2) == 2 and vim.fn.foldclosedend(2) == 9, "exchange range differs")
assert(vim.fn.foldclosed(10) == -1, "exchange swallowed final response")
tab(2)
local saved = folds.capture(replica)
tab(10)
assert(opened == 0, "Tab activated an unfolded timeline row")
controller.open_timeline_entry()
assert(opened == 1, "Enter action did not activate the selected timeline row")
snapshot.revision = 1
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
folds.restore(replica, saved)
assert(vim.fn.foldclosed(6) == 6, "refresh lost nested fold choice")
vim.cmd("normal! zR")
vim.api.nvim_win_set_cursor(window, { 3, 0 })
vim.keymap.set("n", "<Tab>", controller.toggle_activity, { buffer = replica.buffer })
print("harness_plan_folds: passed")
