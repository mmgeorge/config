-- Unit tests for `forge.editable` (editable.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["native"] = function()
  local editable = require("forge.editable")
  local buffer = buffer_support.scratch({ "prefix body suffix", "other" })
  local state = editable.new("native")
  MiniTest.finally(function() pcall(editable.detach, state) end)
  local sent = {}

  editable.register(state, "body", 0)
  editable.register(state, "other", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 7 }, finish = { row = 0, column = 11 } },
    other = { start = { row = 1, column = 0 }, finish = { row = 1, column = 5 } },
  }, {
    delay = 10, max_delay = 30,
    send = function(request)
      sent[#sent + 1] = request
      return true
    end,
  })
  vim.api.nvim_buf_set_text(buffer, 0, 7, 0, 11, { "λ", "🙂" })
  assert(editable.suspend_generated_text(state), "callback failed to suspend synchronously")
  assert(not state.fault, state.fault)
  eq(editable.recoverable_text(state, "body"), { "λ", "🙂" })
  assert(state.native.anchor.other.start.row == 2)
  local captured = editable.capture_draft(state)
  assert(#sent == 0 and captured[1].text == "λ\n🙂", "typing must remain local")
  vim.api.nvim_buf_set_text(buffer, 1, 4, 1, 4, { "new" })
  eq(editable.recoverable_text(state, "body"), { "λ", "🙂new" })
  editable.saved_capture(state, captured)
  assert(state.region.body.revision == 1 and state.region.body.pending)
  assert(captured[1].text == "λ\n🙂")
  assert(editable.capture_draft(state)[1].base == 1)
  vim.api.nvim_buf_set_text(buffer, 2, 0, 2, 5, { "last" })
  eq(editable.recoverable_text(state, "other"), { "last" })
  assert(#editable.capture_draft(state) == 2 and #sent == 0)
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "changed readonly" })
  assert(editable.suspend_generated_text(state))
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(not state.fault, state.fault)
  assert(buffer_support.line(buffer, 1) == "prefix λ")
  assert(#sent == 0, "submitted a cross-boundary edit")
  local namespace = vim.api.nvim_create_namespace("protected-heading-test")
  local heading = vim.api.nvim_buf_set_extmark(buffer, namespace, 2, 0,
    { virt_text = { { "Other: " } }, virt_text_pos = "inline", right_gravity = false })
  local before = buffer_support.lines(buffer)
  local mark = vim.api.nvim_buf_get_extmark_by_id(buffer, namespace, heading, { details = true })
  vim.api.nvim_buf_set_text(buffer, 1, #before[2], 2, 0, {})
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(not state.fault, state.fault)
  eq(buffer_support.lines(buffer), before, { fail_reason = "boundary join changed layout" })
  assert(vim.deep_equal(vim.api.nvim_buf_get_extmark_by_id(buffer, namespace, heading, { details = true }), mark),
    "rejected join displaced the field heading")
  vim.api.nvim_buf_set_text(buffer, 2, 0, 2, 4, { "still editable" })
  eq(editable.recoverable_text(state, "other"), { "still editable" }, { fail_reason = "rejected edit locked the input" })

  editable.detach(state)
  state = editable.new("end-of-buffer")
  buffer_support.set(buffer, { "last" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 0 }, finish = { row = 1, column = 0 } },
  }, { delay = 10, max_delay = 30, send = function() return false end })
  vim.api.nvim_buf_set_text(buffer, 0, 4, 0, 4, { "", "new" })
  assert(not state.fault, state.fault)
  eq(editable.recoverable_text(state, "body"), { "last", "new", "" })
  assert(editable.capture_draft(state)[1].text == "last\nnew\n")
  local sequence = state.sequence
  editable.detach(state)
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "detached" })
  assert(state.sequence == sequence, "detached callback captured text")

  state = editable.new("whole-line-delete")
  buffer_support.set(buffer, { "draft" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 0 }, finish = { row = 0, column = 5 } },
  }, { delay = 10, max_delay = 30, send = function() return true end })
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(buffer_support.lines(buffer), { "" }),
    "whole-line deletion restored the editable draft")
  eq(editable.recoverable_text(state, "body"), { "" })
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 0, { "λ", "second", "🙂" })
  assert(not state.fault, state.fault)
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "" }),
    "multiline deletion failed to capture the empty draft")

  editable.detach(state)
  state = editable.new("protected-last-line")
  buffer_support.set(buffer, { "Label: draft" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 7 }, finish = { row = 0, column = 12 } },
  }, { delay = 10, max_delay = 30, send = function() return true end })
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(vim.deep_equal(buffer_support.lines(buffer), { "Label: draft" }),
    "whole-line deletion removed a protected prefix")

  editable.detach(state)
  state = editable.new("rollback-cursor")
  vim.api.nvim_set_current_buf(buffer)
  buffer_support.set(buffer, { "Label: first", "second" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 7 }, finish = { row = 1, column = 6 } },
  }, { send = function() return true end })
  vim.api.nvim_win_set_cursor(0, { 1, 8 })
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "X" })
  assert(state.native.rejecting, "protected edit did not schedule rollback")
  vim.api.nvim_win_set_cursor(0, { 2, 3 })
  wait_until(function() return not state.native.rejecting end, "native rejection rollback did not settle")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), { 2, 3 }),
    "scheduled rollback restored a cursor captured before the reader moved")
end

T["ordering"] = function()
  local editable = require("forge.editable")
  local state = editable.new("document")
  editable.register(state, "body", 7)
  editable.register(state, "title", 0)
  editable.record(state, "body", { "first" })
  local captured = editable.capture_draft(state)
  editable.record(state, "body", { "newer", "text" })
  editable.saved_capture(state, captured)
  assert(state.region.body.revision == 8 and state.region.body.pending)
  assert(captured[1].text == "first", "new typing mutated an explicit capture")
  editable.record(state, "title", { "Title" })
  local body = editable.capture_draft(state, { body = true })
  assert(#body == 1 and body[1].text == "newer\ntext" and body[1].base == 8)
  editable.saved_capture(state, body)
  assert(editable.suspend_generated_text(state), "saving one field cleared another field")
  editable.saved_capture(state, editable.capture_draft(state))
  assert(not editable.suspend_generated_text(state))
  local before = vim.deepcopy(state)
  assert(not pcall(editable.record, state, "body", { "bad\nrow" }))
  eq(before, state, { fail_reason = "invalid text changed draft state" })
  local racing = editable.new("racing")
  editable.register(racing, "body", 0)
  for revision = 0, 999 do
    editable.record(racing, "body", { tostring(revision) })
    local original = editable.capture_draft(racing)
    for index = 1, 10 do
      editable.record(racing, "body", { tostring(revision), tostring(index) })
    end
    editable.saved_capture(racing, original)
    assert(editable.suspend_generated_text(racing))
    local latest = editable.capture_draft(racing)
    assert(latest[1].base == revision * 2 + 1)
    assert(latest[1].text == tostring(revision) .. "\n10")
    editable.saved_capture(racing, latest)
    assert(not editable.suspend_generated_text(racing))
  end
end

return T
