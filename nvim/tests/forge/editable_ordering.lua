vim.loader.enable(false)
local editable = require("forge.editable")

local function acknowledge(state, region, sequence, revision)
  return editable.acknowledge(state, {
    document = state.document, region = region, sequence = sequence, revision = revision,
  })
end

local ok, failure = xpcall(function()
  local state = editable.new("document")
  editable.register(state, "body", 7)
  editable.register(state, "title", 0)
  local first = editable.record(state, "body", { "first" })
  local request = editable.take_pending(state, "body")
  assert(request.sequence == first and request.base == 7)
  request.text[1] = "external mutation"
  assert(editable.recoverable_text(state, "body")[1] == "first")
  local second = editable.record(state, "body", { "newer", "text" })
  assert(editable.take_pending(state, "body") == nil)
  assert(not acknowledge(state, "body", second, 8))
  assert(not acknowledge(state, "body", first, 10))
  assert(acknowledge(state, "body", first, 8))
  assert(editable.suspend_generated_text(state))
  assert(not editable.ready_to_reconcile(state))
  assert(vim.deep_equal(editable.recoverable_text(state, "body"), { "newer", "text" }))
  request = editable.take_pending(state, "body")
  assert(request.sequence == second and request.base == 8)
  assert(not acknowledge(state, "body", first, 8))
  local title = editable.record(state, "title", { "title" })
  editable.take_pending(state, "title")
  assert(acknowledge(state, "body", second, 9))
  assert(not editable.ready_to_reconcile(state))
  assert(acknowledge(state, "title", title, 1))
  assert(editable.ready_to_reconcile(state))
  assert(editable.suspend_generated_text(state))
  assert(not editable.reconciled(state, { body = 8, title = 1 }))
  assert(editable.reconciled(state, { body = 9, title = 1 }))
  assert(not editable.suspend_generated_text(state))

  local pending = editable.record(state, "body", { "unsent" })
  editable.take_pending(state, "body")
  editable.disconnect(state)
  assert(not acknowledge(state, "body", pending, 10))
  assert(editable.take_pending(state, "body") == nil)
  assert(editable.recoverable_text(state, "body")[1] == "unsent")
  local resolved = editable.resolve(state, "body", 12, { "resolved" })
  request = editable.take_pending(state, "body")
  assert(request.base == 12 and request.sequence == resolved)
  assert(editable.conflict(state, { document = "document", region = "body", sequence = resolved }))
  assert(not editable.ready_to_reconcile(state))
  assert(editable.recoverable_text(state, "body")[1] == "resolved")
  assert(editable.take_pending(state, "body") == nil)

  local independent = editable.new("independent")
  assert(not editable.suspend_generated_text(independent))
  local before = vim.deepcopy(state)
  assert(not pcall(editable.record, state, "body", { "bad\nrow" }))
  assert(vim.deep_equal(before, state))

  local racing = editable.new("racing")
  editable.register(racing, "body", 0)
  for revision = 0, 999 do
    editable.record(racing, "body", { tostring(revision) })
    local sent = editable.take_pending(racing, "body")
    for index = 1, 10 do
      editable.record(racing, "body", { tostring(revision), tostring(index) })
    end
    assert(acknowledge(racing, "body", sent.sequence, revision * 2 + 1))
    assert(not editable.reconciled(racing, { body = revision * 2 + 1 }))
    sent = editable.take_pending(racing, "body")
    assert(vim.deep_equal(sent.text, { tostring(revision), "10" }))
    assert(acknowledge(racing, "body", sent.sequence, revision * 2 + 2))
    assert(editable.reconciled(racing, { body = revision * 2 + 2 }))
  end
  editable.record(racing, "body", { "snapshot raced with new typing" })
  assert(not editable.reconciled(racing, { body = 2000 }))
  assert(editable.suspend_generated_text(racing))
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
