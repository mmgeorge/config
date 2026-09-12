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
adapter._set_runner_for_test(function(method, params, callback)
  if method == "review.open_pr" then
    if params.target.number == 8 then delayed_open = callback
    else callback({ document = "review-adapter" }) end
  elseif method == "review.open" then
    discovery_request = params
    if params.number == 9 then delayed_discovery = callback
    else callback({ document = "review-discovered" }) end
  elseif method == "review.materialize" then
    materialize_count = materialize_count + 1
    if hold_materialize then
      hold_materialize = false
      delayed_materialize = function() callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
    else callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
  elseif method == "review.section" then
    section_request[#section_request + 1] = params.section
    callback({ complete = true })
  elseif method == "review.region_edit" then pending[#pending + 1] = { request = params, callback = callback }
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
    callback({ snapshot = { uncertain = false }, remote = { outcome = "confirmed" } })
  elseif method == "review.comment" then
    comment_request[#comment_request + 1] = params.command
    local parent = params.command.parent
    callback({ snapshot = { comment = parent and 2 or 1, parent = parent, region = "body", text = current }, patch = vim.NIL })
  elseif method == "review.close" then closed[#closed + 1] = params.document callback({ closed = true })
  else error("unexpected route " .. method) end
end)
local errors = {}
local state = adapter.open({ directory = vim.fn.getcwd(), target = { number = 7 },
  on_error = function(message) errors[#errors + 1] = message end })
assert(vim.wait(1000, function() return state.shown end), "native presentation did not open")
assert(vim.fn.maparg("S", "n", false, true).buffer ~= 1, "overview exposed batched viewed action")
assert(vim.fn.maparg("or", "n", false, true).buffer == 1, "overview omitted start review action")
assert(not state.replica.physical and not state.replica.generated)
assert(vim.wait(1000, function() return vim.deep_equal(section_request, { "overview", "requested_reviewers", "files", "checks", "conversation", "commits" }) end),
  "native review did not load the initial overview sections in order")
for _, key in ipairs({ "<C-S>", "<Tab>", "b", "<CR>", "C", "J", "R", "gR", "q" }) do
  assert(vim.fn.maparg(key, "n", false, true).buffer == 1, "native review omitted " .. key .. " action")
end
assert(vim.fn.maparg("l", "n", false, true).buffer ~= 1, "l must retain cursor movement, not open a lifecycle picker")
assert(state.replica.fold.record["fixture:body"].fold.closed,
  "native review did not retain the stable default fold metadata")
local batched = false
assert(adapter.begin_batched(state, function(delivery, failure)
  assert(not failure and delivery.mode == "batched")
  batched = true
end))
assert(vim.wait(1000, function() return batched end), "batched review mode did not settle")
assert(vim.fn.maparg("S", "n", false, true).buffer == 1, "batched review omitted viewed action after rebinding")
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
  editable.flush(state.replica.editable)
end
local function confirm(index)
  local selected = assert(pending[index])
  local edit = selected.request
  local previous = revision
  current, revision = edit.text, revision + 1
  selected.callback({ document = edit.document, region = edit.region, sequence = edit.sequence, revision = edit.base + 1,
    patch = { document = edit.document, base = previous, next = revision, base_rows = 1, next_rows = 1,
      base_blocks = 1, next_blocks = 1, removed_block = {}, block_edit = {},
      text_edit = { { start_row = 0, removed_rows = 1, text = { current } } },
      metadata_edit = { { block = "body", row_count = 1, metadata = metadata(current, revision) } } } })
end
type_text("first")
assert(#pending == 1)
assert(pending[1].request.sequence > 100, "restored durable edit sequence was not adopted before typing")
type_text("newer")
adapter.save(state)
assert(save_count == 0, "save bypassed local acknowledgements")
confirm(1)
assert(vim.wait(1000, function() return #pending == 2 end))
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "newer")
confirm(2)
assert(vim.wait(1000, function() return save_count == 1 end))
assert(state.replica.revision == 2)
type_text("comment capture")
vim.api.nvim_win_set_cursor(state.window, { 1, 0 })
assert(adapter.add_comment(state))
assert(#action_request == 0, "inline comment activation bypassed local acknowledgement")
local command = { operation = "save", comment = 1, action = "save" }
local comment_delivered = false
assert(adapter.comment(state, command, function(delivery, failure)
  assert(not failure and delivery.snapshot.text == "comment capture")
  comment_delivered = true
end))
command.action = "delete"
assert(#comment_request == 0, "comment bypassed local acknowledgement")
assert(not adapter.comment(state, { operation = "draft_conversation" }), "queued comment was overwritten")
confirm(3)
assert(vim.wait(1000, function() return comment_delivered end))
assert(#action_request == 1 and action_request[1].target == "body-action")
assert(action_request[1].revision == state.replica.revision, "action captured a pre-acknowledgement revision")
assert(comment_request[1].action == "save", "queued command did not retain its capture")
local drafted_comment = false
assert(adapter.comment(state, { operation = "draft_conversation" }, function(delivery, failure)
  assert(not failure and delivery.snapshot.region == "body")
  drafted_comment = true
end))
assert(vim.wait(1000, function() return drafted_comment and state.comment_focus and state.comment_focus.comment == 1 end),
  "drafted conversation comment did not focus its editable region")
local saved_comment = false
assert(adapter.save_comment(state, "save", function(delivery, failure)
  assert(not failure and delivery.snapshot.comment == 1)
  saved_comment = true
end))
assert(vim.wait(1000, function() return saved_comment end), "focused comment did not save")
local replied_comment = false
assert(adapter.reply_comment(state, function(delivery, failure)
  assert(not failure and delivery.snapshot.parent == 1)
  replied_comment = true
end))
assert(vim.wait(1000, function() return replied_comment and state.comment_focus and state.comment_focus.comment == 2 end),
  "reply did not focus the created draft")
local deleted_comment = false
assert(adapter.save_comment(state, "delete", function(_, failure)
  assert(not failure)
  deleted_comment = true
end))
assert(vim.wait(1000, function() return deleted_comment and state.comment_focus == nil end),
  "deleted comment retained focus")
assert(comment_request[#comment_request - 2].operation == "save" and comment_request[#comment_request - 2].action == "save")
assert(comment_request[#comment_request - 1].operation == "draft_reply" and comment_request[#comment_request - 1].parent == 1)
assert(comment_request[#comment_request].operation == "save" and comment_request[#comment_request].action == "delete")
hold_action = true
assert(adapter.activate(state))
assert(delayed_action)
vim.api.nvim_win_set_cursor(state.window, { 1, 2 })
delayed_action()
assert(vim.wait(1000, function() return not state.action_running end))
assert(vim.api.nvim_win_get_cursor(state.window)[2] == 2, "late native focus moved a newer user cursor")
type_text("close with accepted text")
assert(#pending == 4)
adapter.close(state)
assert(state.active and #closed == 0, "close discarded an in-flight edit")
confirm(4)
assert(vim.wait(1000, function() return not state.active end))
assert(closed[1] == "review-adapter")
assert(#errors == 0, table.concat(errors, "\n"))
local closing = adapter.open({ directory = vim.fn.getcwd(), target = { number = 8 }, on_error = error })
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
cache.hostname = previous_hostname
adapter.close(discovered)
assert(vim.wait(1000, function() return closed[3] == "review-discovered" end))
local current = true
local late = adapter.open({ directory = vim.fn.getcwd(), repository = repository, number = 9,
  is_current = function() return current end, on_error = error })
current = false
delayed_discovery({ document = "late-discovered" })
assert(vim.wait(1000, function() return closed[4] == "late-discovered" end), "superseded native discovery leaked its document")
assert(not late.active and not late.shown)
adapter._set_runner_for_test(nil)
print("review_document: native projection, incremental edit/save/close, and late-open cleanup passed")
