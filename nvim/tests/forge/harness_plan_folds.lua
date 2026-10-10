vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.nodes")
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
  block("commentary", { "○ Replacing the declaration and its owning task." }, 2),
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
state.presentation = { transcript = replica, toggle_heading = function(window)
  return folds.toggle_heading(replica, window)
end, open_output = function() return false end }
local opened = 0
state.presentation.activate = function() opened = opened + 1 end
local function tab(row)
  vim.api.nvim_win_set_cursor(window, { row, 0 })
  controller.toggle_activity()
end
local function row(id) return select(2, replica.sequence:position(id)) + 1 end
tab(row("plan"))
assert(folds.closed(replica, "plan") and not replica.sequence.node.comment)
assert(replica.sequence.node.commentary, "plan hid unrelated commentary")
tab(row("plan"))
assert(not folds.closed(replica, "plan") and replica.sequence.node.comment)
tab(row("comment"))
tab(row("commentary"))
assert(not folds.closed(replica, "summary"), "body Tab collapsed parent exchange")
tab(row("tools"))
assert(folds.closed(replica, "tools") and not replica.sequence.node["last-tool"])
tab(row("summary"))
assert(folds.closed(replica, "summary") and replica.sequence.node.response)
tab(row("summary"))
tab(row("response"))
assert(opened == 0, "Tab activated a non-expandable timeline row")
controller.open_timeline_entry()
assert(opened == 1, "Enter did not activate selected row")
snapshot.revision = 1
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
assert(folds.closed(replica, "tools") and not replica.sequence.node["last-tool"], "snapshot lost child choice")
print("harness_plan_folds: passed")
