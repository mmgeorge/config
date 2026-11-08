-- Unit tests for `forge.views.plan_review.checks` (checks.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local picker = package.loaded["forge.views.picker"]
      if picker then picker.close() end
    end,
  }),
})

T["plan_check_selection"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  local state = require("forge.session").harness
  state.session = { id = "checks-session" }
  state.transcript_win = vim.api.nvim_get_current_win()
  local notices = notify.capture()
  package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end }
  local path = temp.file("plan.md", { "Plan checks" })
  local version, enabled, pending = 1, true, nil
  local function presentation(document)
    local rows = { { text = "Checks" } }
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
  mock_client.respond({
    ["session.plan.check.update"] = function(spec, callback)
      pending = { params = spec.params or {}, callback = callback }
    end,
    ["session.document"] = function(spec, callback)
      local params = spec.params or {}
      callback(params.operation == "plan_open" and presentation(params.document) or {})
    end,
  }, { wrap_callback = encode_delivery })
  local native = require("forge.views.plan_review.native_controller")
  local picker = require("forge.views.picker")
  native.open({ id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:" .. version })
  local review = assert(state.plan_review, table.concat(notices:messages(), "\n"))
  eq(type(keymap.get("oc").callback), "function", { fail_reason = "oc mapping is missing" })
  review.command_set.action_by_id.checks.run({})
  local text = buffer.text(0)
  assert(text:find("● Package tests", 1, true), text)
  keymap.invoke("<Space>")
  assert(pending and pending.params.enabled.tests == false, "picker did not disable the selected check")
  local active = pending
  keymap.invoke("<Space>")
  assert(pending == active, "rapid toggles submitted concurrent plan edits")
  pending.callback(nil, "fixture rejection")
  assert(not require("forge.views.harness.activity").busy(state) and review.owner.checks[1].enabled, "failed toggle changed canonical state")
  keymap.invoke("<Space>")
  version, enabled = 2, false
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:2" },
    review = presentation(review.owner.document) })
  pending = nil
  text = buffer.text(0)
  assert(text:find("○ Package tests", 1, true), text)
  keymap.invoke("<Space>")
  eq(pending.params.enabled.tests, true)
  version, enabled = 3, true
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:3" },
    review = presentation(review.owner.document) })
  pending = nil
  picker.close()
  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_cursor(review.win, { 2, 0 })
  vim.cmd("normal! V")
  keymap.invoke("j", { mode = "x" })
  eq(vim.bo.filetype, "ForgeConfirm", { fail_reason = "selected check did not open deletion confirmation" })
  keymap.invoke("y")
  assert(pending and pending.params.enabled.tests == false and #pending.params.tests == 0,
    "visual deletion did not become a plan-only disable")
  version, enabled = 4, false
  pending.callback({ plan = { id = "plan", state = "awaiting_review", working_path = path, review_digest = "digest:4" },
    review = presentation(review.owner.document) })
  eq(#buffer.lines(review.buf), 1, { fail_reason = "disabled check remains in the plan" })
  assert(not require("forge.views.harness.activity").busy(state) and review.owner.version == 4)
  pending = nil
  review.plan.state = "accepted"
  review.command_set.action_by_id.checks.run({})
  keymap.invoke("<Space>")
  eq(pending, nil, { fail_reason = "accepted plan allowed a check mutation" })
end

return T
