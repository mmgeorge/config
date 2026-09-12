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
  assert(vim.wait(200, function() return #sent == 1 end, 1), "debounce did not flush")
  vim.api.nvim_buf_set_text(buffer, 1, 4, 1, 4, { "new" })
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "λ", "🙂new" }))
  assert(editable.flush(state))
  assert(#sent == 1, "sent a second request while the first was in flight")
  assert(editable.acknowledge(state, { document = "native", region = "body", sequence = sent[1].sequence, revision = 1 }))
  assert(vim.wait(200, function() return #sent == 2 end, 1), "acknowledgement stranded newer text")
  assert(#sent == 2 and sent[2].base == 1)
  assert(vim.deep_equal(sent[2].text, { "λ", "🙂new" }))
  vim.api.nvim_buf_set_text(buffer, 2, 0, 2, 5, { "last" })
  assert(vim.deep_equal(editable.recoverable_text(state, "other"), { "last" }))
  editable.flush(state)
  assert(#sent == 3 and sent[3].region == "other")
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "changed readonly" })
  assert(state.fault and editable.suspend_generated_text(state))
  assert(not editable.ready_to_reconcile(state))
  assert(not editable.flush(state))
  assert(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1]:find("changed readonly", 1, true))
  assert(#sent == 3, "submitted a cross-boundary edit")

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
  assert(not editable.flush(state), "failed transport admission was reported as successful")
  assert(state.region.body.conflict and not state.region.body.sent)
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "last", "new", "" }))
  local sequence = state.sequence
  editable.detach(state)
  vim.api.nvim_buf_set_text(buffer, 0, 0, 0, 1, { "detached" })
  assert(state.sequence == sequence, "detached callback captured text")
end, debug.traceback)

editable.detach(state)
vim.api.nvim_buf_delete(buffer, { force = true })
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
