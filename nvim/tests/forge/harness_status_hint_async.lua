vim.loader.enable(false)
local buffer = require("forge.buffer")
local hint = require("forge.views.harness.status_hint")
local commands = require("forge.shared.view_command_set").new()
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local notices = {}
local replica = buffer.open("status-hint-async", { notice = function(message) notices[#notices + 1] = message end })
local function status(text)
  return { id = "status", text = { text }, metadata = {
    decoration = {}, fold = {}, editable_region = {}, target = {
      { id = "status:working", range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } },
    },
  } }
end
local history = {}
for index = 1, 3000 do history[index] = "History row " .. index end
local function snapshot(revision)
  return { document = replica.document, revision = revision, block = {
    { id = "history", text = history, metadata = { target = {}, decoration = {}, fold = {}, editable_region = {} } },
    status("Working"),
  } }
end
assert(buffer.apply_snapshot(replica, snapshot(1)).kind == "Applied")
hint.render(replica, commands, 100)
local first = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, {})
assert(first[1] == 3000)

local failures, preparation_ticks = {}, 0
local timer = vim.uv.new_timer()
timer:start(0, 1, vim.schedule_wrap(function()
  if replica.update_pending then
    preparation_ticks = preparation_ticks + 1
    local ok, failure = pcall(hint.render, replica, commands, 100)
    if not ok then failures[#failures + 1] = failure end
    if vim.api.nvim_buf_line_count(replica.buffer) ~= 3001 then
      failures[#failures + 1] = "snapshot exposed a partial buffer"
    end
    if #vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, {}) == 0 then
      failures[#failures + 1] = "preparation removed the committed working spinner"
    end
  end
end))
local result
buffer.apply_async(replica, snapshot(2), function() return true end, function(applied) result = applied end)
assert(vim.wait(10000, function() return result ~= nil end, 1), "snapshot did not finish")
timer:stop()
timer:close()
assert(result.kind == "Applied", result.diagnostic)
assert(preparation_ticks > 0, "test did not render during preparation")
assert(#failures == 0, table.concat(failures, "\n"))
hint.render(replica, commands, 100)
local resumed = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, {})
assert(resumed[1] == 3000, "committed snapshot did not restore the spinner")

assert(buffer.apply_snapshot(replica, { document = replica.document, revision = 2, block = { status("Working again") } }).kind == "Applied")
hint.render(replica, commands, 100)
assert(vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, {})[1] == 0,
  "same-revision replacement reused an obsolete physical row")

local offset = 0
replica.physical_row = function(row) return row + offset end
vim.bo[replica.buffer].modifiable = true
vim.api.nvim_buf_set_lines(replica.buffer, 0, 0, true, { "Local insertion" })
vim.bo[replica.buffer].modifiable = false
offset = 1
hint.render(replica, commands, 100)
assert(vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, {})[1] == 1,
  "local row movement reused an obsolete physical row")

offset = 100
hint.render(replica, commands, 100)
assert(replica.status == "Desynchronized" and #notices == 1,
  "invalid committed status location did not request recovery")
assert(notices[1]:find("outside the committed buffer", 1, true))
replica.execution_notice = "Presentation stopped. Recovering transcript."
hint.render(replica, commands, 100)
local stopped = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, 1, { details = true })
assert(stopped[3].virt_lines and not stopped[3].sign_text, "failure retained a working spinner")
hint.clear(replica.buffer)
buffer.close(replica)
print("harness_status_hint_async: passed, " .. preparation_ticks .. " preparation renders")
vim.cmd("qa!")
