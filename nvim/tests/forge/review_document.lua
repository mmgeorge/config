vim.opt.runtimepath:append("nvim")
local adapter = require("forge.review_document")
local editable = require("forge.editable")
local pending, closed, save_count = {}, {}, 0
local revision, current = 0, "initial"
local restored_sequence = 100
local comment_request = {}
local action_request = {}
local lifecycle_request = {}
local hold_action, delayed_action = false, nil
local function metadata(text, accepted)
  return { target = { { id = "body-action", range = {
    start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } },
    decoration = {}, visible_decoration = {}, fold = { { id = "fixture:body", start = { row = 0, column = 0 },
      ["end"] = { block = "body", position = { row = 1, column = 0 } }, closed = true } }, gutter = {},
    editable_region = { { id = "body", revision = accepted, sequence = restored_sequence,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } }
end
local function snapshot(document)
  return { document = document, revision = revision,
    block = { { id = "body", text = { current }, metadata = metadata(current, revision) } } }
end
local delayed_open
local delayed_discovery, discovery_request
local hold_materialize, delayed_materialize, materialize_count = false, nil, 0
local section_request = {}
local delayed_header, header_request
local hold_header = true
adapter._set_runner_for_test(function(method, params, callback)
  if method == "review.open_pr" then
    if params.target.number == 8 then delayed_open = callback
    else callback({ document = "review-adapter" }) end
  elseif method == "github.actor" then callback({ login = "viewer" })
  elseif method == "review.open" then
    discovery_request = params
    if params.number == 9 then delayed_discovery = callback
    else callback({ document = "review-discovered" }) end
  elseif method == "review.header" then
    header_request = params
    if hold_header then hold_header = false delayed_header = callback else callback({ ready = true }) end
  elseif method == "review.materialize" then
    materialize_count = materialize_count + 1
    if hold_materialize then
      hold_materialize = false
      delayed_materialize = function() callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
    else callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
  elseif method == "review.load" then
    section_request[#section_request + 1] = params.document
    callback({ diagnostic = {} })
  elseif method == "review.region_edit" then error("typing sent a per-edit request")
  elseif method == "review.view" then callback(vim.NIL)
  elseif method == "review.thread" then
    local previous = revision
    revision = revision + 1
    callback({ thread = { node_id = params.thread_node_id, loaded_comments = 2, total_comments = 2 },
      patch = { document = params.document, base = previous, next = revision,
        base_rows = 1, next_rows = 2, base_blocks = 1, next_blocks = 2, removed_block = {},
        text_edit = { { start_row = 1, removed_rows = 0, text = { "next thread comment" } } },
        metadata_edit = { { block = "thread-continuation", row_count = 1, metadata = {
          target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {}, editable_region = {},
        } } },
        block_edit = { { start_block = 1, removed_blocks = 0, inserted = { "thread-continuation" } } } } })
  elseif method == "review.act" then
    action_request[#action_request + 1] = params.input
    if hold_action then
      hold_action = false
      delayed_action = function()
        local effect = vim.deepcopy(params.input)
        effect.id, effect.kind, effect.position = "review-test-focus", "cursor", { row = 0, column = 0 }
        callback({ patch = {}, effect = effect, diagnostic = vim.NIL })
      end
    else callback({ patch = {}, effect = vim.NIL, diagnostic = vim.NIL }) end
  elseif method == "review.transition" then
    lifecycle_request[#lifecycle_request + 1] = params.desired
    callback({ lifecycle = { state = params.desired == "CLOSED" and "CLOSED" or "OPEN", is_draft = params.desired == "DRAFT" },
      recovery = vim.NIL, fresh_required = false })
  elseif method == "review.lifecycle_reconcile" then
    lifecycle_request[#lifecycle_request + 1] = "reconcile"
    callback({ lifecycle = { state = "OPEN", is_draft = false }, recovery = vim.NIL, fresh_required = false })
  elseif method == "review.begin_batched" then
    callback({ mode = "batched", viewed_file = {}, snapshot = {} })
  elseif method == "review.set_viewed" then
    callback({ mode = "batched", viewed_file = params.viewed and { params.path } or {}, snapshot = {} })
  elseif method == "review.submit_batched" then
    callback({ mode = "batched", viewed_file = {}, snapshot = {}, outcome = "outcome_unknown", operation_id = "submission-1" })
  elseif method == "review.submit_batched_recover" then
    assert(params.operation_id == "submission-1")
    callback({ submission = { mode = "batched", viewed_file = {}, snapshot = {}, outcome = "confirmed", operation_id = vim.NIL }, fresh_required = false })
  elseif method == "review.save" then
    save_count = save_count + 1
    pending[#pending + 1] = { capture = vim.deepcopy(params.capture), callback = callback }
  elseif method == "review.comment" then
    comment_request[#comment_request + 1] = params.command
    if params.capture and params.capture[1] then current = params.capture[1].text end
    local parent = params.command.parent
    callback({ snapshot = { comment = parent and 2 or 1, parent = parent, region = "body", text = current }, patch = vim.NIL })
  elseif method == "review.close" then closed[#closed + 1] = params.document callback({ closed = true })
  else error("unexpected route " .. method) end
end)
local errors = {}
local origin = vim.api.nvim_get_current_buf()
local state = adapter.open({ directory = vim.fn.getcwd(), target = { number = 7 },
  on_error = function(message) errors[#errors + 1] = message end })
assert(vim.wait(1000, function() return delayed_header ~= nil end))
assert(vim.api.nvim_get_current_buf() == origin and not state.shown and not state.replica,
  "review displayed before required header metadata arrived")
delayed_header({ ready = true })
assert(vim.wait(1000, function() return state.shown end), "native presentation did not open")
assert(vim.wo.wrap and vim.wo.linebreak and not vim.wo.breakindent and vim.wo.statuscolumn == "",
  "PR header did not retain native wrapping without a margin")
assert(vim.fn.maparg("S", "n", false, true).buffer ~= 1, "overview exposed batched viewed action")
assert(vim.fn.maparg("or", "n", false, true).buffer ~= 1, "review shortcut delays native o in editable text")
assert(not state.replica.physical and not state.replica.generated)
for _, mode in ipairs({ "n", "x" }) do
  local browse = vim.fn.maparg("b", mode, false, true)
  assert(browse.buffer == 1, "editable PR field lost its browse mapping")
  browse.callback()
  assert(vim.wait(1000, function() return not state.action_running end))
  assert(action_request[#action_request].action == "browse", "browse invoked generic row activation")
end
action_request = {}
assert(vim.wait(1000, function() return #section_request == 1 end),
  "native review did not request Rust-owned initial loading")
local shared_snapshot = { id = "PR_shared", number = 7, title = "Refreshed title" }
adapter.refresh_snapshot(state, shared_snapshot)
assert(vim.wait(1000, function() return not state.snapshot_refreshing and not state.rendering end))
assert(vim.deep_equal(header_request.initial, shared_snapshot), "status refresh did not supply its shared snapshot")
for _, key in ipairs({ "<C-S>", "<Tab>", "gR", "q" }) do
  assert(vim.fn.maparg(key, "n", false, true).buffer == 1, "native review omitted " .. key .. " action")
end
state.comment_by_region = { body = { comment = 1, uncertain = true } }
vim.fn.maparg("gR", "n", false, true).callback()
assert(vim.bo.filetype == "ForgeReviewRecovery")
assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == "o  Observe remote state")
vim.fn.maparg("q", "n", false, true).callback()
assert(#comment_request == 0 and state.comment_by_region.body.uncertain)
vim.fn.maparg("gR", "n", false, true).callback()
vim.fn.maparg("r", "n", false, true).callback()
assert(vim.wait(1000, function() return not state.comment_running end))
assert(comment_request[1].operation == "recover" and comment_request[1].comment == 1)
assert(comment_request[1].resolution.resolution == "not_dispatched")
comment_request = {}
state.submission_recovery = "unresolved-submission"
vim.fn.maparg("gB", "n", false, true).callback()
assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == "l  Link confirmed review ID")
vim.fn.maparg("<Esc>", "n", false, true).callback()
assert(state.submission_recovery == "unresolved-submission")
state.submission_recovery = nil
assert(vim.fn.maparg("l", "n", false, true).buffer ~= 1, "l must retain cursor movement, not open a lifecycle picker")
assert(state.replica.fold.record["fixture:body"].fold.closed,
  "native review did not retain the stable default fold metadata")
local batched = false
assert(adapter.begin_batched(state, function(delivery, failure)
  assert(not failure and delivery.mode == "batched")
  batched = true
end))
assert(vim.wait(1000, function() return batched end), "batched review mode did not settle")
assert(vim.fn.maparg("S", "n", false, true).buffer ~= 1, "viewed action overrides native editing")
assert(vim.fn.maparg("or", "n", false, true).buffer ~= 1, "batched review retained overview start action")
local viewed = false
assert(adapter.set_viewed(state, "src/lib.rs", true, function(delivery, failure)
  assert(not failure and vim.deep_equal(delivery.viewed_file, { "src/lib.rs" }))
  viewed = true
end))
assert(vim.wait(1000, function() return viewed and state.mode == "batched" end), "viewed state did not settle")
local submitted = false
state.notice = function() end
local review_window = vim.api.nvim_get_current_win()
assert(adapter.submit_batched(state), "review verdict popup did not open")
local verdict_buffer = vim.api.nvim_get_current_buf()
assert(vim.bo[verdict_buffer].filetype == "ForgeChoicePopup", "review verdict bypassed the shared choice presentation")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(verdict_buffer, 0, -1, false), {
  "", "  [c]  Comment (no verdict)", "  [a]  Approve", "  [r]  Request changes", "  [q]  cancel", "",
}), "review verdict lost the legacy option order or labels")
vim.api.nvim_feedkeys("q", "x", false)
assert(vim.wait(1000, function() return vim.api.nvim_get_current_win() == review_window end), "review verdict cancel lost its origin")
state.verdict_provider = function(callback) callback("comment") end
assert(adapter.submit_batched(state, function(delivery, failure)
  assert(not failure and delivery.outcome == "outcome_unknown")
  submitted = true
end))
assert(vim.wait(1000, function() return submitted and state.submission_recovery == "submission-1" end), "unknown submission did not retain recovery identity")
local recovered = false
assert(adapter.recover_batched_submission(state, { resolution = "not_dispatched" }, function(delivery, failure)
  assert(not failure and delivery.submission.outcome == "confirmed")
  recovered = true
end))
assert(vim.wait(1000, function() return recovered and state.submission_recovery == nil end), "submission recovery did not settle")
local transitioned = false
assert(adapter.transition(state, "draft", function(delivery, failure)
  assert(not failure and delivery.lifecycle.is_draft)
  transitioned = true
end))
assert(vim.wait(1000, function() return transitioned end), "native lifecycle transition did not settle")
assert(lifecycle_request[1] == "DRAFT", "lifecycle transition changed the selected target")
local reconciled = false
assert(adapter.reconcile_lifecycle(state, function(delivery, failure)
  assert(not failure and delivery.lifecycle.state == "OPEN")
  reconciled = true
end))
assert(vim.wait(1000, function() return reconciled end), "native lifecycle reconciliation did not settle")
assert(lifecycle_request[2] == "reconcile")
hold_materialize = true
local before_refresh = materialize_count
adapter.refresh(state)
adapter.refresh(state)
assert(materialize_count == before_refresh + 1, "overlapping refresh was not coalesced")
delayed_materialize()
assert(vim.wait(1000, function() return materialize_count == before_refresh + 2 and not state.rendering end),
  "newer section refresh was lost behind an unchanged presentation")
local function type_text(text)
  vim.bo[state.replica.buffer].modifiable = true
  local previous = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1]
  vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, #previous, { text })
  editable.capture(state.replica.editable, "body")
end
local function confirm(index)
  local selected = assert(pending[index])
  local captured = assert(selected.capture[1])
  current, revision = captured.text, revision + 1
  selected.callback({ snapshot = { uncertain = false, field = {
    { region = "body", baseline = current, text = current, revision = captured.base + 1 },
  } }, remote = { outcome = "confirmed" } })
end
state.fields = { { region = "body", baseline = current, text = current, revision = 0 } }
type_text("first")
assert(#pending == 0, "typing sent a per-edit request")
assert(editable.capture_draft(state.replica.editable)[1].sequence > 100,
  "restored durable capture sequence was not adopted")
adapter.save(state)
assert(save_count == 1 and #pending == 1)
type_text("queued")
adapter.save(state)
assert(save_count == 1, "queued save overlapped publication")
type_text("newer")
confirm(1)
assert(vim.wait(1000, function() return save_count == 2 end))
assert(pending[2].capture[1].text == "queued", "queued save recaptured later typing")
confirm(2)
assert(vim.wait(1000, function() return not state.saving end))
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "newer")
assert(vim.bo[state.replica.buffer].modified, "queued completion cleared newer typing")
adapter.save(state)
confirm(3)
assert(vim.wait(1000, function() return not state.saving and not state.rendering end))
assert(not vim.bo[state.replica.buffer].modified)
type_text("comment capture")
vim.api.nvim_win_set_cursor(state.window, { 1, 0 })
assert(not adapter.add_comment(state), "absent local comment view created a host draft")
local comment_delivered = false
state.comment_by_region = { body = { comment = 1, region = "body" } }
assert(adapter.comment(state, { operation = "save_draft", region = "body", action = "save" },
  function(delivery, failure)
    assert(not failure and delivery.snapshot.text == "comment capture")
    comment_delivered = true
  end))
assert(vim.wait(1000, function() return comment_delivered end))
assert(#action_request == 0, "comment save dispatched generic activation")
assert(comment_request[1].action == "save")
hold_action = true
assert(adapter.activate(state))
assert(delayed_action)
vim.api.nvim_win_set_cursor(state.window, { 1, 2 })
delayed_action()
assert(vim.wait(1000, function() return not state.action_running end))
assert(vim.api.nvim_win_get_cursor(state.window)[2] == 2, "late native focus moved a newer user cursor")
type_text("close with accepted text")
assert(#pending == 3, "typing sent a request before close")
adapter.close(state)
assert(state.active and state.hidden and #closed == 0, "close discarded a local draft")
assert(editable.capture_draft(state.replica.editable)[1].text == "close with accepted text")
vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
assert(vim.wait(1000, function() return not state.active end))
assert(closed[1] == "review-adapter")
assert(#errors == 0, table.concat(errors, "\n"))
local closing = adapter.open({ directory = vim.fn.getcwd(), target = { number = 8 }, on_error = error })
assert(vim.api.nvim_get_current_buf() == closing.origin and not closing.shown,
  "review displayed before the remote response")
adapter.close(closing)
delayed_open({ document = "late-review" })
assert(vim.wait(1000, function() return closed[2] == "late-review" end), "late native open leaked its document")
local repository = { hostname = "github.example", owner = "owner", name = "repo" }
local cache = require("github.repo_cache")
local previous_hostname = cache.hostname
local discovered = adapter.open({ directory = vim.fn.getcwd(), repository = repository, number = 7, on_error = error })
repository.hostname = "changed.example"
cache.hostname = function() return "unrelated.example" end
assert(vim.wait(1000, function() return discovered.shown end), "captured host was replaced by ambient hostname")
assert(discovery_request.repository.hostname == "github.example")
local before_thread = materialize_count
local thread_loaded = false
adapter.read_thread(discovered, "THREAD_1", "next", function(delivery, failure)
  assert(not failure and delivery.thread.loaded_comments == 2)
  thread_loaded = true
end)
assert(vim.wait(1000, function() return thread_loaded end))
assert(materialize_count == before_thread, "thread continuation re-materialized the entire document")
assert(vim.api.nvim_buf_get_lines(discovered.replica.buffer, 1, 2, false)[1] == "next thread comment")
vim.api.nvim_win_set_cursor(discovered.window, { 2, 0 })
local browse_count = #action_request
vim.fn.maparg("b", "n", false, true).callback()
assert(vim.wait(1000, function() return #action_request == browse_count + 1 end))
assert(action_request[#action_request].action == "browse" and not action_request[#action_request].target,
  "browse on plain text required an interactive row target")
cache.hostname = previous_hostname
adapter.close(discovered)
assert(discovered.hidden and discovered.active and not closed[3], "close discarded cached PR state")
assert(vim.api.nvim_get_current_buf() == discovered.origin)
local cached_view_id = discovered.closed_view.id
local cached_buffer = discovered.replica.buffer
local old_request = discovery_request
hold_header = true
local reopened = adapter.open({ directory = vim.fn.getcwd(),
  repository = { hostname = "github.example", owner = "owner", name = "repo" }, number = 7, on_error = error })
assert(reopened == discovered and vim.api.nvim_get_current_buf() == cached_buffer,
  "cached PR did not reopen synchronously")
assert(discovery_request == old_request, "cached PR repeated document discovery")
assert(reopened.view.id == cached_view_id, "cache reopen leaked a native view")
assert(vim.api.nvim_buf_get_lines(cached_buffer, 1, 2, false)[1] == "next thread comment",
  "cached PR lost loaded sections")
assert(vim.wait(1000, function() return reopened.revalidating end))
delayed_header({ ready = true })
assert(vim.wait(1000, function() return not reopened.revalidating end))
adapter.close(reopened)
local refresh_failure
hold_header = true
local stale = adapter.open({ directory = vim.fn.getcwd(),
  repository = { hostname = "github.example", owner = "owner", name = "repo" }, number = 7,
  on_error = function(message) refresh_failure = message end })
assert(stale == reopened and vim.api.nvim_get_current_buf() == cached_buffer)
assert(vim.wait(1000, function() return not hold_header end))
delayed_header(nil, "refresh unavailable")
assert(vim.wait(1000, function() return not stale.revalidating end))
assert(refresh_failure == "refresh unavailable" and stale.active,
  "refresh failure discarded cached PR or omitted notification")
adapter.close(stale)
vim.api.nvim_buf_delete(cached_buffer, { force = true })
assert(vim.wait(1000, function() return closed[3] == "review-discovered" end))
local current = true
local late = adapter.open({ directory = vim.fn.getcwd(), repository = repository, number = 9,
  is_current = function() return current end, on_error = error })
current = false
delayed_discovery({ document = "late-discovered" })
assert(vim.wait(1000, function() return closed[4] == "late-discovered" end), "superseded native discovery leaked its document")
assert(not late.active and not late.shown)
adapter._set_runner_for_test(nil)
print("review_document: native projection, captured saves, dirty close, and late-open cleanup passed")
