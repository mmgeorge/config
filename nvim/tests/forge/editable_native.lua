vim.loader.enable(false)
local editable = require("forge.editable")
local buffer = vim.api.nvim_create_buf(false, true)
local state = editable.new("native")
local sent = {}

local ok, failure = xpcall(function()
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "prefix body suffix", "other" })
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
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "λ", "🙂" }))
  assert(state.native.anchor.other.start.row == 2)
  local captured = editable.capture_draft(state)
  assert(#sent == 0 and captured[1].text == "λ\n🙂", "typing must remain local")
  vim.api.nvim_buf_set_text(buffer, 1, 4, 1, 4, { "new" })
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "λ", "🙂new" }))
  editable.saved_capture(state, captured)
  assert(state.region.body.revision == 1 and state.region.body.pending)
  assert(captured[1].text == "λ\n🙂")
  assert(editable.capture_draft(state)[1].base == 1)
  vim.api.nvim_buf_set_text(buffer, 2, 0, 2, 5, { "last" })
  assert(vim.deep_equal(editable.recoverable_text(state, "other"), { "last" }))
  assert(#editable.capture_draft(state) == 2 and #sent == 0)
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "changed readonly" })
  assert(editable.suspend_generated_text(state))
  assert(vim.wait(200, function() return not state.native.rejecting end, 1))
  assert(not state.fault, state.fault)
  assert(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1] == "prefix λ")
  assert(#sent == 0, "submitted a cross-boundary edit")
  local namespace = vim.api.nvim_create_namespace("protected-heading-test")
  local heading = vim.api.nvim_buf_set_extmark(buffer, namespace, 2, 0,
    { virt_text = { { "Other: " } }, virt_text_pos = "inline", right_gravity = false })
  local before = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  local mark = vim.api.nvim_buf_get_extmark_by_id(buffer, namespace, heading, { details = true })
  vim.api.nvim_buf_set_text(buffer, 1, #before[2], 2, 0, {})
  assert(vim.wait(200, function() return not state.native.rejecting end, 1))
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), before), "boundary join changed layout")
  assert(vim.deep_equal(vim.api.nvim_buf_get_extmark_by_id(buffer, namespace, heading, { details = true }), mark),
    "rejected join displaced the field heading")
  vim.api.nvim_buf_set_text(buffer, 2, 0, 2, 4, { "still editable" })
  assert(vim.deep_equal(editable.recoverable_text(state, "other"), { "still editable" }), "rejected edit locked the input")

  editable.detach(state)
  state = editable.new("end-of-buffer")
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "last" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 0 }, finish = { row = 1, column = 0 } },
  }, { delay = 10, max_delay = 30, send = function() return false end })
  vim.api.nvim_buf_set_text(buffer, 0, 4, 0, 4, { "", "new" })
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "last", "new", "" }))
  assert(editable.capture_draft(state)[1].text == "last\nnew\n")
  local sequence = state.sequence
  editable.detach(state)
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "detached" })
  assert(state.sequence == sequence, "detached callback captured text")

  state = editable.new("whole-line-delete")
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "draft" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 0 }, finish = { row = 0, column = 5 } },
  }, { delay = 10, max_delay = 30, send = function() return true end })
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  assert(vim.wait(200, function() return not state.native.rejecting end, 1))
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), { "" }),
    "whole-line deletion restored the editable draft")
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "" }))
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 0, { "λ", "second", "🙂" })
  assert(not state.fault, state.fault)
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  assert(vim.wait(200, function() return not state.native.rejecting end, 1))
  assert(not state.fault, state.fault)
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "" }),
    "multiline deletion failed to capture the empty draft")

  editable.detach(state)
  state = editable.new("protected-last-line")
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "Label: draft" })
  editable.register(state, "body", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 7 }, finish = { row = 0, column = 12 } },
  }, { delay = 10, max_delay = 30, send = function() return true end })
  vim.api.nvim_buf_call(buffer, function() vim.cmd("normal! ggdG") end)
  assert(vim.wait(200, function() return not state.native.rejecting end, 1))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), { "Label: draft" }),
    "whole-line deletion removed a protected prefix")
end, debug.traceback)

editable.detach(state)
vim.api.nvim_buf_delete(buffer, { force = true })
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
