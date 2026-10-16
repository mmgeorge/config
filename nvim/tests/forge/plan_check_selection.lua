vim.loader.enable(false)
local client = require("forge.client")
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
local state = require("forge.session").harness
state.session = { id = "checks-session" }
state.transcript_win = vim.api.nvim_get_current_win()
local notices = {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end }
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "Plan checks" }, path)
local version, enabled, pending = 1, true, nil
local function presentation(document)
  local rows = { { text = "Planned checks" } }
  if enabled then rows[#rows + 1] = { text = "Test  cargo test  All  Will run", check = "tests" } end
  local source, blocks = {}, {}
  for index, row in ipairs(rows) do
    local id = "row:" .. index
    local metadata = { decoration = {}, editable_region = {}, target = {{ id = id, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } }} }
    source[#source + 1] = { id = id, text = row.text, check = row.check, source_line = index,
      block = id, target = id, position = { row = 0, column = 0 }, metadata = metadata }
    blocks[#blocks + 1] = { id = id, text = { row.text }, metadata = metadata }
  end
  return { version = version, saved_source_digest = "saved:" .. version, annotation = {}, source_row = source,
    checks = {{ id = "tests", name = "Package tests", stage = "test", enabled = enabled, available = true }},
    snapshot = { document = document, revision = version - 1, block = blocks } }
end
client.request_for = function(_, method, params, callback)
  if method == "plan.checks.update" then pending = { params = params, callback = callback } return end
  assert(method == "harness.document", method)
  callback(params.operation == "plan_open" and presentation(params.document) or {})
end
local native = require("forge.views.plan_review.native_controller")
local picker = require("forge.views.picker")
local ok, failure = xpcall(function()
  native.open({ id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:" .. version })
  local review = assert(state.plan_review, table.concat(notices, "\n"))
  assert(type(vim.fn.maparg("oc", "n", false, true).callback) == "function", "oc mapping is missing")
  review.command_set.action_by_id.checks.run({})
  local text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(text:find("● Package tests", 1, true), text)
  vim.fn.maparg("<Space>", "n", false, true).callback()
  assert(pending and pending.params.enabled.tests == false, "picker did not disable the selected check")
  local active = pending
  vim.fn.maparg("<Space>", "n", false, true).callback()
  assert(pending == active, "rapid toggles submitted concurrent plan edits")
  pending.callback(nil, "fixture rejection")
  assert(not state.busy and review.owner.checks[1].enabled, "failed toggle changed canonical state")
  vim.fn.maparg("<Space>", "n", false, true).callback()
  version, enabled = 2, false
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:2" },
    review = presentation(review.owner.document) })
  pending = nil
  text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(text:find("○ Package tests", 1, true), text)
  vim.fn.maparg("<Space>", "n", false, true).callback()
  assert(pending.params.enabled.tests == true)
  version, enabled = 3, true
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:3" },
    review = presentation(review.owner.document) })
  pending = nil
  picker.close()
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { 2, 0 })
  vim.cmd("normal! V")
  vim.fn.maparg("j", "x", false, true).callback()
  assert(vim.bo.filetype == "ForgeConfirm", "selected check did not open deletion confirmation")
  vim.fn.maparg("y", "n", false, true).callback()
  assert(pending and pending.params.enabled.tests == false and #pending.params.tests == 0,
    "visual deletion did not become a plan-only disable")
  version, enabled = 4, false
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:4" },
    review = presentation(review.owner.document) })
  assert(#vim.api.nvim_buf_get_lines(review.buf, 0, -1, false) == 1, "disabled check remains in the plan")
  assert(not state.busy and review.owner.version == 4)
  pending = nil
  review.plan.state = "accepted"
  review.command_set.action_by_id.checks.run({})
  vim.fn.maparg("<Space>", "n", false, true).callback()
  assert(pending == nil, "accepted plan allowed a check mutation")
end, debug.traceback)
picker.close()
vim.fn.delete(path)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("plan_check_selection passed")
vim.cmd("qa!")
