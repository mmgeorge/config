local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["review_capture"] = function()
  local review = require("forge.review_document")
  local requests = {}
  review._set_runner_for_test(function(method, params, callback)
    requests[#requests + 1] = { method = method, params = params, callback = callback }
  end)
  review.sync_editing = function() end
  review.sync_dirty = function() end
  review.refresh = function() end

  local function field(text)
    return { revision = 0, pending = { sequence = 1, text = { text } } }
  end
  local state = {
    active = true,
    shown = true,
    explicit_repository = true,
    document = "review-capture",
    notice = function(message) error(message) end,
    replica = {
      editable = {
        document = "review-capture",
        suspended = true,
        region = {
          title = field("Unsaved title"),
          body = field("Unsaved description"),
          reviewers = field("Unsaved reviewers"),
          summary = field("Review summary"),
          ["comment-1/body"] = field("Selected comment\r\n\n"),
          ["comment-2/body"] = field("Other comment"),
        },
      },
    },
    comment_by_region = {
      ["comment-1/body"] = { comment = 1 },
      ["comment-2/body"] = { comment = 2 },
    },
  }

  assert(review.comment(state, { operation = "save", comment = 1, action = "save" }))
  local saved = requests[1]
  eq(saved.method, "github.review.document.comment")
  assert(#saved.params.capture == 1 and saved.params.capture[1].region == "comment-1/body",
    "saving one comment must capture only that comment")
  assert(saved.params.capture[1].text == "Selected comment\r\n\n",
    "explicit comment saves must preserve raw body bytes")
  saved.callback({})
  wait_until(function() return not state.comment_running end, "comment save did not settle")
  assert(state.replica.editable.region.title.pending, "comment save must retain unsaved PR fields")
  assert(state.replica.editable.region["comment-2/body"].pending,
    "comment save must retain other comment drafts")

  state.verdict_provider = function(callback) callback("comment") end
  assert(review.submit_batched(state))
  local submitted = requests[2]
  eq(submitted.method, "github.review.document.batch.submit")
  assert(#submitted.params.capture == 2, "review submission must capture the summary and remaining comment")
  for _, captured in ipairs(submitted.params.capture) do
    assert(captured.region ~= "title" and captured.region ~= "body" and captured.region ~= "reviewers",
      "review submission must retain unrelated PR fields")
  end
  state.submission_running = false
  state.replica.editable.region["draft-comment/client-1/body"] = field("Local body\r\n\n")
  local anchor = { revision = string.rep("a", 40), path = "src/lib.rs", side = "right", first_line = 4, last_line = 4 }
  state.comment_by_region["draft-comment/client-1/body"] = {
    region = "draft-comment/client-1/body", local_draft = true, anchor = anchor,
  }
  assert(review.comment(state, { operation = "save_draft", region = "draft-comment/client-1/body", action = "save" }))
  local created = requests[3]
  assert(#created.params.capture == 1 and created.params.capture[1].text == "Local body\r\n\n")
  eq(#created.params.draft_comment, 1)
  eq(created.params.draft_comment[1].region, "draft-comment/client-1/body")
  assert(vim.deep_equal(created.params.draft_comment[1].anchor, anchor))
  anchor.last_line = 9
  assert(created.params.draft_comment[1].anchor.last_line == 4,
    "later metadata changes must not mutate a submitted comment definition")
  local editable = require("forge.editable")
  state.replica.editable.sequence = 10
  local region = "draft-comment/client-1/body"
  editable.record(state.replica.editable, region, { "queued explicit body" })
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  assert(#requests == 3, "queued comment save bypassed active operation")
  editable.record(state.replica.editable, region, { "newer unsaved body" })
  created.callback({})
  wait_until(function() return #requests == 4 end, "completion stranded the queued explicit capture")
  assert(requests[4].params.capture[1].text == "queued explicit body",
    "queued comment save recaptured later unsaved typing")
  eq(requests[4].params.capture[1].base, 1)
  requests[4].callback({})
  wait_until(function() return not state.comment_running end, "queued comment save did not settle")
  eq(editable.capture_draft(state.replica.editable, { [region] = true })[1].text, "newer unsaved body")
  assert(state.replica.editable.region.title.pending, "queued saves cleared unrelated fields")
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  eq(#requests, 5)
  editable.record(state.replica.editable, "summary", { "queued summary" })
  assert(review.submit_batched(state))
  assert(#requests == 5, "submission overlapped the active comment save")
  editable.record(state.replica.editable, "summary", { "newer unsaved summary" })
  requests[5].callback({})
  wait_until(function() return #requests == 6 end, "comment completion stranded submission")
  eq(requests[6].method, "github.review.document.batch.submit")
  local captured_summary
  for _, capture in ipairs(requests[6].params.capture) do
    if capture.region == "summary" then captured_summary = capture.text end
  end
  assert(captured_summary == "queued summary", "submission recaptured newer unsaved text")
  requests[6].callback({ outcome = "confirmed" })
  wait_until(function() return not state.submission_running end, "review submission did not settle")
  eq(editable.capture_draft(state.replica.editable, { summary = true })[1].text, "newer unsaved summary")
  review.read_section = function() end
  editable.record(state.replica.editable, region, { "lifecycle interleave body" })
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  eq(#requests, 7)
  assert(review.transition(state, "draft"))
  assert(#requests == 7, "lifecycle transition overlapped a comment save")
  requests[7].callback({})
  wait_until(function() return #requests == 8 end, "comment completion did not dispatch the queued lifecycle transition")
  assert(requests[8].method == "github.review.document.transition" and requests[8].params.desired == "DRAFT")
  editable.record(state.replica.editable, region, { "captured during lifecycle" })
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  assert(#requests == 8, "comment save overlapped a lifecycle transition")
  editable.record(state.replica.editable, region, { "newer lifecycle typing" })
  requests[8].callback({ state = "OPEN", is_draft = true })
  wait_until(function() return #requests == 9 end, "lifecycle completion did not dispatch the queued comment save")
  eq(requests[9].params.capture[1].text, "captured during lifecycle")
  requests[9].callback({})
  wait_until(function() return not state.comment_running end, "comment save after lifecycle did not settle")
  eq(editable.capture_draft(state.replica.editable, { [region] = true })[1].text, "newer lifecycle typing")
  local failure_notice
  state.notice = function(message) failure_notice = message end
  assert(review.transition(state, "open"))
  eq(#requests, 10)
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  eq(#requests, 10)
  requests[10].callback(nil, "lifecycle fixture failure")
  wait_until(function() return #requests == 11 end, "lifecycle failure stranded queued capture")
  eq(failure_notice, "lifecycle fixture failure")
  requests[11].callback({})
  wait_until(function() return not state.comment_running end, "comment save after lifecycle failure did not settle")
  state.view = {}
  state.comment_focus = { comment = 1 }
  assert(not review.add_comment(state))
  assert(not review.reply_comment(state))
  assert(#requests == 11, "missing local presentation fell back to host draft creation")
  local client = require("forge.client")
  local original_generation = client.host_get_generation
  local generation = original_generation()
  client.host_get_generation = function() return generation end
  editable.record(state.replica.editable, region, { "retained across restart" })
  local accepted_revision = state.replica.editable.region[region].revision
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  eq(#requests, 12)
  editable.record(state.replica.editable, region, { "queued before restart" })
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  editable.record(state.replica.editable, region, { "retained across restart" })
  generation = generation + 1
  requests[12].callback({})
  wait_until(function() return not state.comment_running end, "obsolete host response did not settle the comment save")
  assert(state.replica.editable.region[region].revision == accepted_revision,
    "obsolete host response advanced the draft revision")
  eq(editable.capture_draft(state.replica.editable, { [region] = true })[1].text, "retained across restart")
  assert(failure_notice:find("host changed", 1, true))
  assert(#requests == 12, "host loss dispatched a queued capture against the stale document")
  eq(state.pending_operation.capture[1].text, "queued before restart")
  state.host_lost, state.pending_operation = false, nil
  state.generation = generation
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  eq(#requests, 13)
  generation = generation + 1
  state.generation = generation
  state.comment_running = "replacement operation"
  requests[13].callback({})
  vim.wait(50, function() return false end, 5)
  assert(not state.host_lost, "obsolete response invalidated the replacement host binding")
  assert(state.comment_running == "replacement operation", "obsolete response settled a replacement operation")
  eq(state.replica.editable.region[region].revision, accepted_revision)
  state.host_lost, state.comment_running = false, true
  state.submission_recovery = "recovery-operation"
  local request_count = #requests
  assert(review.reconcile_save(state) == false, "save recovery overlapped comment publication")
  assert(not review.recover_batched_submission(state, { resolution = "not_dispatched" }),
    "submission recovery overlapped comment publication")
  eq(#requests, request_count)
  state.comment_running, state.save_recovering = false, true
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  assert(#requests == request_count and state.pending_operation, "comment bypassed active save recovery")
  assert(review.transition(state, "CLOSED"))
  assert(#requests == request_count and state.pending_operation, "lifecycle bypassed active save recovery")
  state.save_recovering, state.save_uncertain = false, true
  assert(review.transition(state, "OPEN"))
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  assert(#requests == request_count, "unresolved save outcome dispatched a later mutation")
  eq(state.pending_operation.kind, "comment")
  eq(state.pending_operation.capture[1].text, "retained across restart")
  state.save_uncertain, state.rebinding = false, true
  assert(review.transition(state, "CLOSED"))
  assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
  assert(#requests == request_count, "rebinding dispatched a mutation against the obsolete document")
  eq(state.pending_operation.kind, "comment")
end

T["review_operation_queue"] = function()
  local review = require("forge.review_document")
  local editable = require("forge.editable")
  local request = {}
  review._set_runner_for_test(function(method, params, callback)
    request[#request + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
  end)
  review.sync_editing = function() end
  review.sync_dirty = function() end
  review.refresh = function() end
  review.read_section = function() end
  local function field(text)
    return { revision = 0, pending = { sequence = 1, text = { text } } }
  end
  local comment_region = "comment-1/body"
  local state = {
    active = true, explicit_repository = true, document = "queue-review", fields = {}, notice = error,
    replica = { buffer = buffer.scratch(), editable = {
      document = "queue-review", sequence = 1, native = { anchor = {} }, suspended = true,
      region = { title = field("Title"), body = field("Description"), summary = field("Summary"),
        [comment_region] = field("Comment") },
    } },
    comment_by_region = { [comment_region] = { comment = 1 } },
    verdict_provider = function(callback) callback("comment") end,
  }
  review.save(state)
  assert(#request == 1 and request[1].method == "github.review.document.save")
  local superseded_comment, superseded_submission, superseded_lifecycle
  review.comment(state, { operation = "save", comment = 1, action = "save" }, function(result, failure)
    assert(not result) superseded_comment = failure
  end)
  review.submit_batched(state, function(result, failure)
    assert(not result) superseded_submission = failure
  end)
  review.transition(state, "CLOSED", function(result, failure)
    assert(not result) superseded_lifecycle = failure
  end)
  review.save(state)
  assert(superseded_comment and superseded_submission and superseded_lifecycle)
  assert(state.pending_operation.kind == "save" and #request == 1)
  editable.record(state.replica.editable, comment_region, { "Frozen comment" })
  local callback_ran
  review.comment(state, { operation = "save", comment = 1, action = "save" }, function(result, failure)
    assert(result and not failure, failure)
    review.transition(state, "OPEN")
    assert(#request == 2, "completion callback dispatched a concurrent operation")
    callback_ran = true
  end)
  editable.record(state.replica.editable, comment_region, { "Newer unsaved comment" })
  assert(state.pending_operation.kind == "comment" and #request == 1)
  request[1].callback({ snapshot = { uncertain = false, field = {} }, remote = { outcome = "confirmed" } })
  wait_until(function() return #request == 2 end, "save completion did not dispatch the queued comment")
  eq(request[2].method, "github.review.document.comment")
  assert(request[2].params.capture[1].text == "Frozen comment", "dispatch recaptured later typing")
  local superseded_close
  review.transition(state, "CLOSED", function(result, failure)
    assert(not result) superseded_close = failure
  end)
  request[2].callback({})
  wait_until(function() return #request == 3 end, "comment completion did not dispatch the queued lifecycle transition")
  assert(callback_ran and superseded_close)
  assert(request[3].method == "github.review.document.transition" and request[3].params.desired == "OPEN")
  eq(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text, "Newer unsaved comment")
  request[3].callback({ lifecycle = { state = "OPEN" } })
  wait_until(function() return not state.lifecycle_running end, "lifecycle transition did not settle")
  state.submission_recovery = "unknown-review-submission"
  review.transition(state, "CLOSED")
  assert(#request == 3 and state.pending_operation.kind == "lifecycle",
    "unresolved review submission admitted a later publication")
  local recovery_callback_ran
  assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
    assert(result and not failure, failure)
    review.transition(state, "OPEN")
    assert(#request == 4, "recovery callback dispatched before completion settled")
    recovery_callback_ran = true
  end))
  assert(#request == 4 and request[4].method == "github.review.document.batch.recover")
  request[4].callback({ submission = {}, fresh_required = false })
  wait_until(function() return #request == 5 end, "recovery completion did not dispatch the queued lifecycle transition")
  assert(not state.submission_recovery and request[5].method == "github.review.document.transition")
  assert(recovery_callback_ran and request[5].params.desired == "OPEN")
  request[5].callback({ lifecycle = { state = "OPEN" } })
  wait_until(function() return not state.lifecycle_running end, "lifecycle transition after recovery did not settle")
  assert(not state.pending_operation)
  state.submission_recovery = "failed-review-recovery"
  state.notice = function() end
  review.transition(state, "CLOSED")
  local failed_recovery_callback
  assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
    failed_recovery_callback = failure
  end))
  eq(#request, 6)
  request[6].callback({})
  wait_until(function() return failed_recovery_callback ~= nil end, "malformed recovery delivery did not fail its callback")
  eq(failed_recovery_callback, "Missing native review recovery delivery")
  eq(state.submission_recovery, "failed-review-recovery")
  assert(state.pending_operation.kind == "lifecycle" and #request == 6)
  eq(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text, "Newer unsaved comment")
  failed_recovery_callback = nil
  assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
    failed_recovery_callback = failure
  end))
  request[7].callback(nil, "Recovery transport failed")
  wait_until(function() return failed_recovery_callback ~= nil end, "recovery transport failure did not reach its callback")
  eq(failed_recovery_callback, "Recovery transport failed")
  assert(state.submission_recovery and state.pending_operation and #request == 7)
  state.submission_recovery = nil
  state.comment_by_region[comment_region].uncertain = true
  review.transition(state, "OPEN")
  assert(#request == 7 and state.pending_operation.params.desired == "OPEN")
  local retained_operation = state.pending_operation
  assert(review.comment(state, { operation = "recover", comment = 1 }))
  assert(#request == 8 and state.pending_operation == retained_operation)
  request[8].callback(nil, "Comment recovery failed")
  wait_until(function() return not state.comment_running end, "failed comment recovery did not settle")
  assert(#request == 8 and state.pending_operation == retained_operation)
  local malformed_recovery_failure
  assert(review.comment(state, { operation = "recover", comment = 1 }, function(result, failure)
    malformed_recovery_failure = failure
  end))
  request[9].callback({})
  wait_until(function() return malformed_recovery_failure ~= nil end, "malformed comment recovery did not fail its callback")
  eq(malformed_recovery_failure, "Missing native comment recovery snapshot")
  assert(#request == 9 and state.pending_operation == retained_operation)
  assert(review.comment(state, { operation = "recover", comment = 1 }))
  request[10].callback({ snapshot = { region = comment_region, comment = 1, uncertain = false } })
  wait_until(function() return #request == 11 end, "comment recovery did not dispatch the retained lifecycle transition")
  assert(request[11].method == "github.review.document.transition" and request[11].params.desired == "OPEN")
  eq(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text, "Newer unsaved comment")
  request[11].callback({ lifecycle = { state = "OPEN" } })
  wait_until(function() return not state.lifecycle_running end, "retained lifecycle transition did not settle")
end

T["document"] = function()
  local adapter = require("forge.review_document")
  local editable = require("forge.editable")
  local pending, closed, save_count = {}, {}, 0
  local revision, current = 0, "initial"
  local restored_sequence = 100
  local comment_request = {}
  local action_request = {}
  local lifecycle_request = {}
  local hold_action, delayed_action = false, nil
  local diff_view = { visibility = "all", presentation = "full" }
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
    if method == "github.review.document.open" then
      if not params.target then
        discovery_request = params
        if params.number == 9 then delayed_discovery = callback
        else callback({ document = "review-discovered" }) end
      elseif params.target.number == 8 then delayed_open = callback
      else callback({ document = "review-adapter" }) end
    elseif method == "github.actor.get" then callback({ login = "viewer" })
    elseif method == "github.review.document.header.load" then
      header_request = params
      if hold_header then hold_header = false delayed_header = callback else callback({ ready = true }) end
    elseif method == "github.review.document.render" then
      materialize_count = materialize_count + 1
      if hold_materialize then
        hold_materialize = false
        delayed_materialize = function() callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
      else callback({ snapshot = snapshot(params.document), patch = vim.NIL }) end
    elseif method == "github.review.document.load" then
      section_request[#section_request + 1] = params.document
      callback({ diagnostic = {} })
    elseif method == "review.region_edit" then error("typing sent a per-edit request")
    elseif method == "github.review.document.view.set" then callback(vim.NIL)
    elseif method == "github.review.document.thread.get" then
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
    elseif method == "github.review.document.input.apply" then
      action_request[#action_request + 1] = params.input
      if hold_action then
        hold_action = false
        delayed_action = function()
          local effect = vim.deepcopy(params.input)
          effect.id, effect.kind, effect.position = "review-test-focus", "cursor", { row = 0, column = 0 }
          callback({ patch = {}, effect = effect, diagnostic = vim.NIL })
        end
      else
        if params.input.action == "toggle_visibility" then
          diff_view.visibility = diff_view.visibility == "all" and "public" or "all"
        elseif params.input.action == "toggle_presentation" then
          diff_view.presentation = diff_view.presentation == "full" and "semantic" or "full"
        end
        callback({ patch = {}, effect = vim.NIL, diagnostic = vim.NIL, diff_view = vim.deepcopy(diff_view) })
      end
    elseif method == "github.review.document.transition" then
      lifecycle_request[#lifecycle_request + 1] = params.desired
      callback({ lifecycle = { state = params.desired == "CLOSED" and "CLOSED" or "OPEN", is_draft = params.desired == "DRAFT" },
        recovery = vim.NIL, fresh_required = false })
    elseif method == "github.review.document.lifecycle.reconcile" then
      lifecycle_request[#lifecycle_request + 1] = "reconcile"
      callback({ lifecycle = { state = "OPEN", is_draft = false }, recovery = vim.NIL, fresh_required = false })
    elseif method == "github.review.document.batch.begin" then
      callback({ mode = "batched", viewed_file = {}, snapshot = {} })
    elseif method == "github.review.document.file.viewed.set" then
      callback({ mode = "batched", viewed_file = params.viewed and { params.path } or {}, snapshot = {} })
    elseif method == "github.review.document.batch.submit" then
      callback({ mode = "batched", viewed_file = {}, snapshot = {}, outcome = "outcome_unknown", operation_id = "submission-1" })
    elseif method == "github.review.document.batch.recover" then
      eq(params.operation_id, "submission-1")
      callback({ submission = { mode = "batched", viewed_file = {}, snapshot = {}, outcome = "confirmed", operation_id = vim.NIL }, fresh_required = false })
    elseif method == "github.review.document.save" then
      save_count = save_count + 1
      pending[#pending + 1] = { capture = vim.deepcopy(params.capture), callback = callback }
    elseif method == "github.review.document.comment" then
      comment_request[#comment_request + 1] = params.command
      if params.capture and params.capture[1] then current = params.capture[1].text end
      local parent = params.command.parent
      callback({ snapshot = { comment = parent and 2 or 1, parent = parent, region = "body", text = current }, patch = vim.NIL })
    elseif method == "github.review.document.close" then closed[#closed + 1] = params.document callback({ closed = true })
    else error("unexpected route " .. method) end
  end)
  local errors = {}
  local origin = vim.api.nvim_get_current_buf()
  local state = adapter.open({ directory = vim.fn.getcwd(), target = { number = 7 },
    on_error = function(message) errors[#errors + 1] = message end })
  wait_until(function() return delayed_header ~= nil end, "review open did not request header metadata")
  assert(vim.api.nvim_get_current_buf() == origin and not state.shown and not state.replica,
    "review displayed before required header metadata arrived")
  delayed_header({ ready = true })
  wait_until(function() return state.shown end, "native presentation did not open")
  assert(vim.wo.wrap and vim.wo.linebreak and not vim.wo.breakindent and vim.wo.statuscolumn == "",
    "PR header did not retain native wrapping without a margin")
  assert(vim.fn.maparg("S", "n", false, true).buffer ~= 1, "overview exposed batched viewed action")
  assert(vim.fn.maparg("or", "n", false, true).buffer ~= 1, "review shortcut delays native o in editable text")
  assert(not state.replica.physical and not state.replica.generated)
  for _, mode in ipairs({ "n", "x" }) do
    local browse = keymap.get("b", { mode = mode })
    assert(browse.buffer == 1, "editable PR field lost its browse mapping")
    browse.callback()
    wait_until(function() return not state.action_running end, "browse action did not settle")
    assert(action_request[#action_request].action == "browse", "browse invoked generic row activation")
  end
  action_request = {}
  local visibility_map = keymap.get("ov")
  local presentation_map = keymap.get("ox")
  assert(visibility_map.buffer == 1 and presentation_map.buffer == 1,
    "editable PR fields omitted independent diff controls")
  visibility_map.callback()
  wait_until(function() return not state.action_running end, "visibility toggle did not settle")
  assert(state.diff_view.visibility == "public" and state.diff_view.presentation == "full")
  presentation_map.callback()
  wait_until(function() return not state.action_running end, "presentation toggle did not settle")
  assert(state.diff_view.visibility == "public" and state.diff_view.presentation == "semantic")
  state.action_running = true
  visibility_map.callback()
  visibility_map.callback()
  presentation_map.callback()
  assert(#action_request == 2, "busy controls started overlapping requests")
  state.action_running = false
  assert(adapter.drain_diff_controls(state))
  wait_until(function() return not state.action_running end, "coalesced diff controls did not settle")
  assert(#action_request == 3 and action_request[3].action == "toggle_presentation",
    "busy controls did not coalesce independent toggle parity")
  assert(state.diff_view.visibility == "public" and state.diff_view.presentation == "full")
  state.lifecycle_running = true
  visibility_map.callback()
  visibility_map.callback()
  presentation_map.callback()
  assert(#action_request == 3, "diff controls overlapped a native mutation")
  state.lifecycle_running = false
  assert(adapter.drain_diff_controls(state))
  wait_until(function() return not state.action_running end, "diff controls deferred behind a mutation did not settle")
  assert(#action_request == 4 and state.diff_view.presentation == "semantic")
  action_request = {}
  wait_until(function() return #section_request == 1 end,
    "native review did not request Rust-owned initial loading")
  local shared_snapshot = { id = "PR_shared", number = 7, title = "Refreshed title" }
  adapter.refresh_snapshot(state, shared_snapshot)
  wait_until(function() return not state.snapshot_refreshing and not state.rendering end, "shared snapshot refresh did not settle")
  assert(vim.deep_equal(header_request.initial, shared_snapshot), "status refresh did not supply its shared snapshot")
  for _, key in ipairs({ "<C-S>", "<Tab>", "gR", "q" }) do
    assert(keymap.get(key).buffer == 1, "native review omitted " .. key .. " action")
  end
  state.comment_by_region = { body = { comment = 1, uncertain = true } }
  keymap.invoke("gR")
  eq(vim.bo.filetype, "ForgeReviewRecovery")
  eq(buffer.line(0, 1), "o  Observe remote state")
  keymap.invoke("q")
  assert(#comment_request == 0 and state.comment_by_region.body.uncertain)
  keymap.invoke("gR")
  keymap.invoke("r")
  wait_until(function() return not state.comment_running end, "comment recovery did not settle")
  assert(comment_request[1].operation == "recover" and comment_request[1].comment == 1)
  eq(comment_request[1].resolution.resolution, "not_dispatched")
  comment_request = {}
  state.submission_recovery = "unresolved-submission"
  keymap.invoke("gB")
  eq(buffer.line(0, 1), "l  Link confirmed review ID")
  keymap.invoke("<Esc>")
  eq(state.submission_recovery, "unresolved-submission")
  state.submission_recovery = nil
  assert(vim.fn.maparg("l", "n", false, true).buffer ~= 1, "l must retain cursor movement, not open a lifecycle picker")
  assert(state.replica.fold.record["fixture:body"].fold.closed,
    "native review did not retain the stable default fold metadata")
  local batched = false
  assert(adapter.begin_batched(state, function(delivery, failure)
    assert(not failure and delivery.mode == "batched")
    batched = true
  end))
  wait_until(function() return batched end, "batched review mode did not settle")
  assert(vim.fn.maparg("S", "n", false, true).buffer ~= 1, "viewed action overrides native editing")
  assert(vim.fn.maparg("or", "n", false, true).buffer ~= 1, "batched review retained overview start action")
  local viewed = false
  assert(adapter.set_viewed(state, "src/lib.rs", true, function(delivery, failure)
    assert(not failure and vim.deep_equal(delivery.viewed_file, { "src/lib.rs" }))
    viewed = true
  end))
  wait_until(function() return viewed and state.mode == "batched" end, "viewed state did not settle")
  local submitted = false
  state.notice = function() end
  local review_window = vim.api.nvim_get_current_win()
  assert(adapter.submit_batched(state), "review verdict popup did not open")
  local verdict_buffer = vim.api.nvim_get_current_buf()
  assert(vim.bo[verdict_buffer].filetype == "ForgeChoicePopup", "review verdict bypassed the shared choice presentation")
  assert(vim.deep_equal(buffer.lines(verdict_buffer), {
    "", "  [c]  Comment (no verdict)", "  [a]  Approve", "  [r]  Request changes", "  [q]  cancel", "",
  }), "review verdict lost the legacy option order or labels")
  keymap.feed("q", "x")
  wait_until(function() return vim.api.nvim_get_current_win() == review_window end, "review verdict cancel lost its origin")
  state.verdict_provider = function(callback) callback("comment") end
  assert(adapter.submit_batched(state, function(delivery, failure)
    assert(not failure and delivery.outcome == "outcome_unknown")
    submitted = true
  end))
  wait_until(function() return submitted and state.submission_recovery == "submission-1" end, "unknown submission did not retain recovery identity")
  local recovered = false
  assert(adapter.recover_batched_submission(state, { resolution = "not_dispatched" }, function(delivery, failure)
    assert(not failure and delivery.submission.outcome == "confirmed")
    recovered = true
  end))
  wait_until(function() return recovered and state.submission_recovery == nil end, "submission recovery did not settle")
  local transitioned = false
  assert(adapter.transition(state, "draft", function(delivery, failure)
    assert(not failure and delivery.lifecycle.is_draft)
    transitioned = true
  end))
  wait_until(function() return transitioned end, "native lifecycle transition did not settle")
  assert(lifecycle_request[1] == "DRAFT", "lifecycle transition changed the selected target")
  local reconciled = false
  assert(adapter.reconcile_lifecycle(state, function(delivery, failure)
    assert(not failure and delivery.lifecycle.state == "OPEN")
    reconciled = true
  end))
  wait_until(function() return reconciled end, "native lifecycle reconciliation did not settle")
  eq(lifecycle_request[2], "reconcile")
  hold_materialize = true
  local before_refresh = materialize_count
  adapter.refresh(state)
  adapter.refresh(state)
  assert(materialize_count == before_refresh + 1, "overlapping refresh was not coalesced")
  delayed_materialize()
  wait_until(function() return materialize_count == before_refresh + 2 and not state.rendering end,
    "newer section refresh was lost behind an unchanged presentation")
  local function type_text(text)
    vim.bo[state.replica.buffer].modifiable = true
    local previous = buffer.line(state.replica.buffer, 1)
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
  wait_until(function() return save_count == 2 end, "save completion did not dispatch the queued save")
  assert(pending[2].capture[1].text == "queued", "queued save recaptured later typing")
  confirm(2)
  wait_until(function() return not state.saving end, "queued save did not settle")
  eq(buffer.line(state.replica.buffer, 1), "newer")
  assert(vim.bo[state.replica.buffer].modified, "queued completion cleared newer typing")
  adapter.save(state)
  confirm(3)
  wait_until(function() return not state.saving and not state.rendering end, "explicit save did not settle")
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
  wait_until(function() return comment_delivered end, "comment save did not deliver")
  assert(#action_request == 0, "comment save dispatched generic activation")
  eq(comment_request[1].action, "save")
  hold_action = true
  assert(adapter.activate(state), vim.inspect({ active = state.active, view_ready = state.view_ready,
    running = state.action_running, pending = state.activation_pending, cursor = vim.api.nvim_win_get_cursor(state.window),
    errors = errors }))
  assert(delayed_action)
  vim.api.nvim_win_set_cursor(state.window, { 1, 2 })
  delayed_action()
  wait_until(function() return not state.action_running end, "late native focus action did not settle")
  assert(vim.api.nvim_win_get_cursor(state.window)[2] == 2, "late native focus moved a newer user cursor")
  type_text("close with accepted text")
  assert(#pending == 3, "typing sent a request before close")
  adapter.close(state)
  assert(state.active and state.hidden and #closed == 0, "close discarded a local draft")
  eq(editable.capture_draft(state.replica.editable)[1].text, "close with accepted text")
  vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
  wait_until(function() return not state.active end, "deleting the hidden review buffer did not close the document")
  eq(closed[1], "review-adapter")
  assert(#errors == 0, table.concat(errors, "\n"))
  local closing = adapter.open({ directory = vim.fn.getcwd(), target = { number = 8 }, on_error = error })
  assert(vim.api.nvim_get_current_buf() == closing.origin and not closing.shown,
    "review displayed before the remote response")
  adapter.close(closing)
  delayed_open({ document = "late-review" })
  wait_until(function() return closed[2] == "late-review" end, "late native open leaked its document")
  local repository = { hostname = "github.example", owner = "owner", name = "repo" }
  local cache = require("github.repo_cache")
  local previous_hostname = cache.hostname
  local discovered = adapter.open({ directory = vim.fn.getcwd(), repository = repository, number = 7, on_error = error })
  repository.hostname = "changed.example"
  cache.hostname = function() return "unrelated.example" end
  wait_until(function() return discovered.shown end, "captured host was replaced by ambient hostname")
  eq(discovery_request.repository.hostname, "github.example")
  local before_thread = materialize_count
  local thread_loaded = false
  adapter.read_thread(discovered, "THREAD_1", "next", function(delivery, failure)
    assert(not failure and delivery.thread.loaded_comments == 2)
    thread_loaded = true
  end)
  wait_until(function() return thread_loaded end, "thread continuation did not load")
  assert(materialize_count == before_thread, "thread continuation re-materialized the entire document")
  eq(buffer.line(discovered.replica.buffer, 2), "next thread comment")
  vim.api.nvim_win_set_cursor(discovered.window, { 2, 0 })
  local browse_count = #action_request
  keymap.invoke("b")
  wait_until(function() return #action_request == browse_count + 1 end, "browse on plain text did not dispatch")
  assert(action_request[#action_request].action == "browse" and not action_request[#action_request].target,
    "browse on plain text required an interactive row target")
  cache.hostname = previous_hostname
  adapter.close(discovered)
  assert(discovered.hidden and discovered.active and not closed[3], "close discarded cached PR state")
  eq(vim.api.nvim_get_current_buf(), discovered.origin)
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
  assert(buffer.line(cached_buffer, 2) == "next thread comment",
    "cached PR lost loaded sections")
  wait_until(function() return reopened.revalidating end, "cached PR reopen did not start revalidation")
  delayed_header({ ready = true })
  wait_until(function() return not reopened.revalidating end, "cached PR revalidation did not settle")
  adapter.close(reopened)
  local refresh_failure
  hold_header = true
  local stale = adapter.open({ directory = vim.fn.getcwd(),
    repository = { hostname = "github.example", owner = "owner", name = "repo" }, number = 7,
    on_error = function(message) refresh_failure = message end })
  assert(stale == reopened and vim.api.nvim_get_current_buf() == cached_buffer)
  wait_until(function() return not hold_header end, "stale reopen did not request header metadata")
  delayed_header(nil, "refresh unavailable")
  wait_until(function() return not stale.revalidating end, "failed revalidation did not settle")
  assert(refresh_failure == "refresh unavailable" and stale.active,
    "refresh failure discarded cached PR or omitted notification")
  adapter.close(stale)
  vim.api.nvim_buf_delete(cached_buffer, { force = true })
  wait_until(function() return closed[3] == "review-discovered" end, "deleting the cached buffer did not close the discovered document")
  local current = true
  local late = adapter.open({ directory = vim.fn.getcwd(), repository = repository, number = 9,
    is_current = function() return current end, on_error = error })
  current = false
  delayed_discovery({ document = "late-discovered" })
  wait_until(function() return closed[4] == "late-discovered" end, "superseded native discovery leaked its document")
  assert(not late.active and not late.shown)
end

T["editing"] = function()
  local adapter = require("forge.review_document")
  local cache = require("github.repo_cache")
  local index = require("github.issue_index")
  local warmed_users, warmed_issues, saves, actions = 0, 0, 0, 0
  cache.ensure_metadata = function(_, repo) assert(repo == "owner/repo") warmed_users = warmed_users + 1 end
  index.ensure_repo = function(_, repo) assert(repo == "owner/repo") warmed_issues = warmed_issues + 1 end
  cache.contributors = function(repo) assert(repo == "owner/repo") return { { login = "alice", name = "Alice" }, { login = "viewer" } } end
  index.search = function(repo, query)
    assert(repo == "owner/repo" and query == "issue")
    return { { number = 42, title = "Issue search", repo = repo, state = "OPEN" } }
  end
  local field = {
    { region = "title", text = "PR title", baseline = "PR title", revision = 0, sequence = 0 },
    { region = "reviewers", text = "@alice", baseline = "@alice", revision = 0, sequence = 0 },
    { region = "body", text = "Description text", baseline = "Description text", revision = 0, sequence = 0 },
  }
  local revision = 0
  local function block(identity, text, metadata)
    return { id = identity, text = vim.split(text, "\n", { plain = true }), metadata = vim.tbl_extend("force", {
      target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {}, editable_region = {},
    }, metadata or {}) }
  end
  local function field_block(value)
    local rows = vim.split(value.text, "\n", { plain = true })
    local label = ({ title = "Title:  ", reviewers = "Review: " })[value.region]
    return block("region:" .. value.region, value.text, { gutter = label and {
      { position = { row = 0, column = 0 }, chunk = { { text = label, capture = "ForgeStatusLabel" } }, priority = 110 },
    } or {}, editable_region = { {
      id = value.region, revision = value.revision, sequence = value.sequence,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = #rows - 1, column = #rows[#rows] } },
    } } })
  end
  local function snapshot()
    return { document = "editing", revision = revision, block = {
      field_block(field[1]), block("repo", "Repo:   owner/repo"), field_block(field[2]),
      block("label:body", "Description:"), field_block(field[3]), block("checks", "Checks:"),
    } }
  end
  local save_callback, reconcile_callback
  adapter._set_runner_for_test(function(method, params, callback)
    if method == "github.review.document.open" then callback({ document = "editing", field = vim.deepcopy(field) })
    elseif method == "github.actor.get" then callback({ login = "viewer" })
    elseif method == "github.review.document.reconcile" then reconcile_callback = callback
    elseif method == "github.review.document.header.load" then callback({ ready = true })
    elseif method == "github.review.document.render" then callback({ snapshot = snapshot(), field = vim.deepcopy(field) })
    elseif method == "github.review.document.view.set" or method == "github.review.document.close" then callback({})
    elseif method == "github.review.document.load" then callback({ diagnostic = {} })
    elseif method == "github.review.document.input.apply" then actions = actions + 1 error("editable text dispatched an action")
    elseif method == "review.region_edit" then error("typing sent a per-edit request")
    elseif method == "github.review.document.save" then
      saves = saves + 1
      assert(#(params.capture or {}) > 0, "explicit save omitted its field capture")
      local before_capture = vim.deepcopy(field)
      for _, captured in ipairs(params.capture or {}) do
        local selected
        for _, value in ipairs(field) do if value.region == captured.region then selected = value end end
        assert(selected and captured.base == selected.revision)
        selected.text, selected.sequence, selected.revision = captured.text, captured.sequence, captured.base + 1
      end
      local captured = vim.deepcopy(field)
      save_callback = function(failure, outcome)
        if failure then field = before_capture callback(nil, failure) return end
        if outcome == "missing" then field = before_capture callback({}) return end
        outcome = outcome or "confirmed"
        if outcome == "confirmed" then
          for position, value in ipairs(field) do value.baseline = captured[position].text end
        end
        callback({ snapshot = { field = vim.deepcopy(field), uncertain = outcome == "outcome_unknown" },
          remote = { outcome = outcome } })
      end
    else error(method) end
  end)
  local errors = {}
  local state = adapter.open({ directory = vim.fn.getcwd(), number = 7,
    repository = { hostname = cache.hostname(), owner = "owner", name = "repo" },
    on_error = function(message) errors[#errors + 1] = message end })
  wait_until(function() return state.shown and state.view_ready end, "editing review did not open its view")
  assert(warmed_users == 1 and warmed_issues == 1 and cache.user_completion_enabled(state.replica.buffer))
  local function cursor(row, column)
    vim.api.nvim_win_set_cursor(0, { row, column or 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.replica.buffer })
  end
  local function keys(value)
    keymap.feed(value)
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = state.replica.buffer })
  end
  local function marker_rows()
    local result = {}
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.replica.buffer,
      vim.api.nvim_get_namespaces().ForgeReviewDirty, 0, -1, { details = true })) do
      local line = buffer.line(state.replica.buffer, mark[2] + 1)
      if line == "Description:" then
        assert(mark[3] == #"Description" and mark[4].virt_text_pos == "inline")
        assert(mark[4].virt_text[1][1] == "*", "description marker must render before its colon")
      else
        eq(mark[4].virt_text[1][1], " *")
      end
      result[#result + 1] = mark[2] + 1
    end
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.replica.buffer,
      state.replica.namespace, 0, -1, { details = true })) do
      if mark[4].virt_text and mark[4].virt_text[1][1]:find("*: ", 1, true) then
        assert(mark[4].virt_text_pos == "inline" and mark[3] == 0)
        result[#result + 1] = mark[2] + 1
      end
    end
    table.sort(result)
    return result
  end
  assert(vim.wo.virtualedit == "", "review enabled cursor movement past field text")
  for _, entry in ipairs({ { row = 1, label = "Title" }, { row = 3, label = "Review" } }) do
    cursor(entry.row)
    keys("<End>")
    eq(vim.api.nvim_win_get_cursor(0)[2], #vim.api.nvim_get_current_line() - 1)
    assert(vim.fn.getcurpos()[4] == 0, "End moved into virtual whitespace")
    local entered, insert_failure = false, nil
    vim.api.nvim_create_autocmd("InsertEnter", { buffer = state.replica.buffer, once = true, callback = function()
      entered = true
      local success, failure = pcall(function()
        assert(vim.api.nvim_win_get_cursor(0)[2] == 0, "i did not move to the input start")
        assert(vim.deep_equal(marker_rows(), { entry.row }), "editing marker missing")
        local marks = vim.api.nvim_buf_get_extmarks(state.replica.buffer, state.replica.namespace,
          { entry.row - 1, 0 }, { entry.row - 1, -1 }, { details = true })
        local heading
        for _, mark in ipairs(marks) do
          if mark[4].virt_text then
            for _, chunk in ipairs(mark[4].virt_text) do
              if chunk[1] == entry.label .. "*: " then heading = chunk[1] end
            end
          end
        end
        assert(heading, "editable heading did not show its active marker")
        assert(not vim.bo.modified, "entering insert mode dirtied the value")
      end)
      if not success then insert_failure = failure end
    end })
    keys("i<Esc>")
    assert(not insert_failure, insert_failure)
    assert(entered and #marker_rows() == 0, "unchanged field retained editing marker")
  end
  cursor(1)
  assert(vim.bo.modifiable)
  vim.opt.backspace = { "indent", "eol", "start" }
  for _, row in ipairs({ 3, 5 }) do
    cursor(row)
    local before = buffer.lines(state.replica.buffer)
    keys("i<BS><Esc>")
    wait_until(function() return not state.replica.editable.native.rejecting end, "rejected backspace did not settle")
    assert(not state.replica.editable.fault, state.replica.editable.fault)
    assert(vim.deep_equal(buffer.lines(state.replica.buffer), before),
      "backspace moved a heading into the preceding row")
    assert(vim.api.nvim_win_get_cursor(0)[1] == row, "rejected backspace left the cursor on protected metadata")
    assert(#marker_rows() == 0 and not vim.bo.modified, "rejected edit left an unchanged field dirty")
    assert(table.remove(errors) == "edit crosses a read-only boundary", vim.inspect(errors))
  end
  cursor(1)
  keys("A changed<CR><Esc>")
  assert(vim.api.nvim_get_current_line() == "PR title changed", "title accepted a newline or remained locked")
  cursor(3)
  keys("A @bob<CR><Esc>")
  assert(vim.api.nvim_get_current_line() == "@alice @bob", "reviewer editing failed")
  cursor(5)
  for _, key in ipairs({ "C", "J", "R", "<CR>", "or" }) do
    assert(vim.fn.maparg(key, "n", false, true).buffer ~= 1, "native editing was overridden: " .. key)
  end
  keys("oSecond line<Esc>")
  assert(not state.replica.editable.fault, state.replica.editable.fault)
  assert(vim.api.nvim_get_current_line() == "Second line", "native o did not extend the description")
  assert(vim.deep_equal(marker_rows(), { 1, 3, 4 }), vim.inspect(marker_rows()))
  assert(vim.bo.modified)
  assert(require("forge.editable").suspend_generated_text(state.replica.editable), "local typing lost its pending capture")
  cursor(2)
  assert(not vim.bo.modifiable, "read-only metadata was unlocked")
  cursor(6)
  keys("<C-S>")
  assert(not vim.bo.modified and #marker_rows() == 0, "save waited for GitHub before clearing markers")
  wait_until(function() return saves == 1 end, "explicit save was not dispatched")
  assert(state.saving and not state.pending_operation, "in-flight save was not retained locally")
  adapter.refresh(state)
  assert(state.refresh_pending and not state.rendering, "refresh raced a pending save")
  save_callback("injected save rejection")
  wait_until(function() return not state.saving and not state.rendering end, "rejected save did not settle")
  assert(vim.bo.modified and #marker_rows() == 3, "rejection cleared unsaved state")
  keys("<C-S>")
  assert(not vim.bo.modified and #marker_rows() == 0)
  wait_until(function() return saves == 2 end, "second save was not dispatched")
  cursor(1)
  keys("A newer<Esc>")
  assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "new typing stayed optimistically clean")
  save_callback()
  wait_until(function() return not state.saving and not state.rendering end, "second save did not settle")
  assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), vim.inspect({
    message = "save cleared text typed after submission", modified = vim.bo.modified,
    marker = marker_rows(), text = buffer.lines(state.replica.buffer), field = state.fields, errors = errors,
  }))
  keys("A<C-S><Esc>")
  wait_until(function() return saves == 3 end, "insert-mode save was not dispatched")
  save_callback()
  wait_until(function() return not vim.bo.modified and #marker_rows() == 0 end, "insert-mode save did not clear markers")
  keys("A write<Esc>")
  vim.cmd("write")
  assert(not vim.bo.modified and #marker_rows() == 0, ":w waited for its response")
  wait_until(function() return saves == 4 end, ":write save was not dispatched")
  save_callback()
  wait_until(function() return not vim.bo.modified and #marker_rows() == 0 end, ":write save did not clear markers")
  wait_until(function() return not state.saving and not state.reconciling end, ":write save did not settle")
  keys("A delayed<Esc>")
  adapter.save(state)
  assert(not vim.bo.modified and #marker_rows() == 0, "explicit capture did not clear its submitted markers")
  assert(state.saving and not state.pending_operation)
  wait_until(function() return saves == 5 end, "delayed save was not dispatched")
  adapter.save(state)
  assert(saves == 5 and not state.pending_operation, "unchanged save duplicated an in-flight request")
  keys("A queued<Esc>")
  adapter.save(state)
  assert(state.pending_operation and saves == 5 and not vim.bo.modified and #marker_rows() == 0)
  keys("A latest<Esc>")
  adapter.save(state)
  assert(state.pending_operation and saves == 5 and not vim.bo.modified)
  save_callback()
  wait_until(function() return saves == 6 end, "queued save was dropped")
  assert(not vim.bo.modified and #marker_rows() == 0, "older completion restored queued markers")
  keys("A unsaved<Esc>")
  save_callback()
  wait_until(function() return not state.saving and not state.reconciling end, "queued save did not settle")
  assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "queued completion cleared newer typing")
  for _, outcome in ipairs({ "rejected", "outcome_unknown", "missing" }) do
    adapter.save(state)
    assert(not vim.bo.modified and #marker_rows() == 0)
    wait_until(function() return state.saving ~= nil end, "retry save was not dispatched")
    keys("A retry<Esc>")
    adapter.save(state)
    assert(state.pending_operation and not vim.bo.modified)
    local before = saves
    save_callback(nil, outcome)
    if outcome == "outcome_unknown" then
      wait_until(function() return reconcile_callback ~= nil end, "uncertain save did not start reconciliation")
      assert(state.save_recovering and vim.bo.modifiable, "recovery disabled native editing")
      reconcile_callback(nil, "injected observation failure")
      wait_until(function() return not state.save_recovering end, "failed reconciliation did not settle")
      assert(state.save_uncertain and vim.bo.modifiable, "failed recovery disabled editing")
      reconcile_callback = nil
      keys("A pending recovery<Esc>")
      local recovery_capture = vim.api.nvim_get_current_line()
      adapter.save(state)
      assert(state.pending_operation.text.title == recovery_capture, "save during recovery lost its explicit capture")
      wait_until(function() return reconcile_callback ~= nil end, "explicit save did not retry field recovery")
      reconcile_callback({ field = vim.deepcopy(field), uncertain = false })
      wait_until(function() return not state.save_recovering end, "retried reconciliation did not settle")
    end
    wait_until(function() return saves == before + 1 end, "explicit queued capture was discarded")
    assert(state.saving and not state.pending_operation)
    keys("A retained<Esc>")
    save_callback()
    wait_until(function() return not state.saving and not state.reconciling end, "retried save did not settle")
    assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "failure lost unsaved state")
  end
  keys("A rollback<Esc>")
  local retained_recovery_text = vim.api.nvim_get_current_line()
  adapter.save(state)
  wait_until(function() return state.saving ~= nil end, "rollback save was not dispatched")
  reconcile_callback = nil
  save_callback(nil, "outcome_unknown")
  wait_until(function() return reconcile_callback ~= nil end, "rollback save did not start reconciliation")
  reconcile_callback({ field = vim.deepcopy(field), uncertain = false })
  wait_until(function() return not state.save_recovering end, "rollback reconciliation did not settle")
  assert(vim.api.nvim_get_current_line() == retained_recovery_text, "save recovery replaced locally owned text")
  assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }))
  cursor(6)
  keys("A<CR><CR><Esc>")
  local description_anchor = state.replica.editable.native.anchor.body
  local description = table.concat(vim.api.nvim_buf_get_text(state.replica.buffer,
    description_anchor.start.row, description_anchor.start.column,
    description_anchor.finish.row, description_anchor.finish.column, {}), "\n")
  assert(description:sub(-2) == "\n\n", "fixture omitted trailing empty description rows")
  local before_save_text = buffer.lines(state.replica.buffer)
  adapter.save(state)
  assert(not vim.bo.modified and #marker_rows() == 0, "explicit capture delayed submitted markers")
  assert(vim.deep_equal(buffer.lines(state.replica.buffer), before_save_text),
    "save transformed locally owned buffer text")
  local anchor = state.replica.editable.native.anchor.body
  assert(table.concat(vim.api.nvim_buf_get_text(state.replica.buffer, anchor.start.row, anchor.start.column,
    anchor.finish.row, anchor.finish.column, {}), "\n") == description, "save changed trailing description rows")
  wait_until(function() return state.saving ~= nil end, "trailing-row save was not dispatched")
  assert(field[3].text == description and description:find("\nSecond line", 1, true), "save changed internal paragraphs")
  save_callback()
  wait_until(function() return not state.saving and not state.reconciling end, "trailing-row save did not settle")
  local function completion(row, text, source)
    cursor(row)
    keys("A " .. text .. " <Esc>")
    local line = vim.api.nvim_get_current_line()
    vim.api.nvim_win_set_cursor(0, { row, #line - 1 })
    assert(source:enabled(), "completion source disabled in editable field")
    local result
    source:get_completions({}, function(value) result = value end)
    wait_until(function() return result ~= nil end, "completion source did not answer")
    return result.items
  end
  local users = require("forge.views.pr.reviewer_source").new()
  eq(completion(1, "@al", users)[1].textEdit.newText, "@alice")
  assert(#completion(1, "@vi", users) == 2, "self mention was hidden outside reviewer input")
  assert(#completion(3, "@al", users) == 1, "self reviewer remained selectable")
  cursor(3)
  keys("A @ViEwEr<Esc>")
  local before_self = saves
  adapter.save(state)
  assert(saves == before_self and not state.pending_operation and vim.bo.modified, "self reviewer was submitted")
  eq(completion(6, "@al", users)[1].textEdit.newText, "@alice")
  local issues = require("github.issue_source").new()
  eq(completion(6, "#issue", issues)[1].textEdit.newText, "#42")
  assert(warmed_users == 1 and warmed_issues == 1, "completion performed another remote refresh")
  assert(actions == 0 and vim.deep_equal(errors, { "injected save rejection", "Review save was rejected",
    "injected observation failure. Press gR to retry recovery. Your edits are retained.",
    "Missing native review save result", "You cannot request a review from yourself (@viewer)" }), vim.inspect(errors))
  assert(require("forge.editable").suspend_generated_text(state.replica.editable), "unsaved completion text lost its local capture")
  vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
end

T["rebind"] = function()
  local forge_buffer = require("forge.buffer")
  local comments = require("forge.review_comments")
  local review = require("forge.review_document")
  local editable = require("forge.editable")
  local request = {}
  review._set_runner_for_test(function(method, params, callback)
    request[#request + 1] = { method = method, params = params, callback = callback }
  end)
  local function metadata()
    return { target = {}, decoration = {}, visible_decoration = {}, gutter = {}, fold = {}, editable_region = {} }
  end
  local title = metadata()
  title.editable_region = { { id = "title", revision = 0, sequence = 0,
    range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 8 } } } }
  local code = metadata()
  code.target = { { id = "inline:assets", range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } }
  local snapshot = { document = "review-local-comments", revision = 0, block = {
    { id = "region:title", text = { "Original" }, metadata = title },
    { id = "diff:assets", text = { "+ pub mod assets;" }, metadata = code },
  } }
  local replica = forge_buffer.open(snapshot.document, { editable = {} })
  eq(forge_buffer.apply_snapshot(replica, snapshot).kind, "Applied")
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, replica.buffer)
  local state = { active = true, shown = true, explicit_repository = true, document = snapshot.document,
    window = window, replica = replica, view = { id = "local-view" },
    notice = function(message) error(message) end }
  local anchor = { revision = string.rep("a", 40), path = "src/assets.rs", side = "right", first_line = 44, last_line = 44 }
  comments.attach(state, { snapshot = snapshot, comment = {}, inline_anchor = { ["inline:assets"] = anchor } })
  vim.api.nvim_win_set_cursor(window, { 2, 0 })
  assert(review.add_comment(state), "inline comment creation must use the local renderer")
  vim.wait(20, function() return false end, 5)
  vim.cmd("stopinsert")
  assert(#request == 0, "creation must not send a host request")
  local region = state.comment_focus.region
  local bounds = replica.editable.native.anchor[region]
  assert(bounds and editable.capture_draft(replica.editable)[1].text == "")
  vim.api.nvim_buf_set_text(replica.buffer, bounds.start.row, 0, bounds.finish.row, bounds.finish.column,
    { "Sp", "second line αβ", "" })
  vim.api.nvim_buf_set_text(replica.buffer, 0, 0, 0, 8, { "Unsaved title" })
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = replica.buffer })
  assert(#request == 0, "collapse must not send a host request")
  assert(buffer.line(replica.buffer, 1) == "Unsaved title",
    "comment transitions must preserve other unsaved fields")

  state.directory, state.number = vim.fn.getcwd(), 7
  state.repository = { hostname = "github.com", owner = "owner", name = "repo" }
  state.fields = { { region = "title", baseline = "Original", revision = 0 } }
  state.host_lost = true
  state.mode = "batched"
  state.actor = "viewer"
  state.pending_operation = { kind = "save", text = { title = "Unsaved title" },
    capture = editable.capture_draft(state.replica.editable, { title = true }) }
  local delayed, replacement_document, closed
  local queued_save, complete_save
  local entered_batched, transitioned
  local replacement_anchor = anchor
  local uncertain = false
  local baseline = "Original"
  state.notice = function() end
  review._set_runner_for_test(function(method, params, callback)
    if method == "github.review.document.open" then
      entered_batched = false
      replacement_document = "replacement-" .. tostring(vim.uv.hrtime())
      callback({ document = replacement_document, uncertain = uncertain, field = {
        { region = "title", baseline = baseline, revision = 0, uncertain = uncertain } } })
    elseif method == "github.review.document.header.load" or method == "github.review.document.load" or method == "github.review.document.view.set" or method == "github.review.document.section.get" then callback({})
    elseif method == "github.review.document.render" then
      assert(entered_batched, "replacement presentation omitted batched-review restoration")
      delayed = function()
        local replacement = vim.deepcopy(snapshot)
        replacement.document = replacement_document
        callback({ snapshot = replacement, field = {
          { region = "title", baseline = baseline, revision = 0, uncertain = uncertain } },
          comment = {}, inline_anchor = { ["inline:assets"] = replacement_anchor } })
      end
    elseif method == "github.review.document.close" then closed = params.document callback({})
    elseif method == "github.review.document.batch.begin" then entered_batched = true callback({ mode = "batched" })
    elseif method == "github.review.document.transition" then
      assert(params.document == state.document, "queued lifecycle retained the obsolete host identity")
      transitioned = params.document
      callback({ lifecycle = { state = "CLOSED", is_draft = false }, fresh_required = false })
    elseif method == "github.review.document.save" then queued_save, complete_save = vim.deepcopy(params), callback
    else error("unexpected recovery request " .. method) end
  end)
  local retained_buffer = state.replica.buffer
  local recovered
  assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
  wait_until(function() return delayed ~= nil end, "rebind did not render the replacement document")
  vim.bo[retained_buffer].modifiable = true
  vim.api.nvim_buf_set_text(retained_buffer, 0, 0, 0, #"Unsaved title", { "Latest title" })
  delayed()
  wait_until(function() return recovered ~= nil end, "rebind did not complete")
  assert(state.replica.buffer == retained_buffer and state.document == replacement_document)
  assert(not state.host_lost and not state.rebinding)
  wait_until(function() return state.view_ready end, "replacement host view did not attach")
  eq(buffer.line(retained_buffer, 1), "Latest title")
  local capture_by_region = {}
  for _, capture in ipairs(editable.capture_draft(state.replica.editable)) do capture_by_region[capture.region] = capture end
  eq(capture_by_region.title.text, "Latest title")
  eq(capture_by_region[region].text, "Sp\nsecond line αβ\n")
  assert(vim.bo[retained_buffer].modified)
  wait_until(function() return queued_save ~= nil end, "replacement host did not resume the queued capture")
  assert(queued_save.capture[1].text == "Unsaved title", "recovery recaptured newer typing for the queued save")
  assert(queued_save.document == state.document and queued_save.capture[1].document == state.document)
  assert(queued_save.capture[1].sequence < capture_by_region.title.sequence)
  complete_save({ snapshot = { uncertain = false, field = {
    { region = "title", baseline = "Unsaved title", revision = 1 } } }, remote = { outcome = "confirmed" } })
  wait_until(function() return not state.saving end, "resumed save did not settle")
  state.host_lost, delayed, recovered = true, nil, nil
  baseline = "Unsaved title"
  state.pending_operation = { kind = "lifecycle", method = "github.review.document.transition",
    params = { document = state.document, desired = "CLOSED" } }
  assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
  wait_until(function() return delayed ~= nil end, "lifecycle rebind did not render the replacement document")
  delayed()
  wait_until(function() return recovered and transitioned == state.document end, "rebind did not resume the queued lifecycle transition")
  eq(editable.capture_draft(state.replica.editable, { title = true })[1].text, "Latest title")
  assert(vim.bo[retained_buffer].modified)
  local previous_document = state.document
  local previous_text = buffer.lines(retained_buffer)
  baseline, delayed = "Remote changed", nil
  local conflict
  assert(review.rebind(state, function(value, failure) assert(not value) conflict = failure end))
  wait_until(function() return delayed ~= nil end, "conflicting rebind did not render the replacement document")
  delayed()
  wait_until(function() return conflict ~= nil end, "baseline conflict did not fail the rebind")
  assert(closed == replacement_document and state.document == previous_document)
  assert(vim.deep_equal(buffer.lines(retained_buffer), previous_text))
  assert(vim.bo[retained_buffer].modified)
  baseline, delayed = "Unsaved title", nil
  replacement_anchor = vim.deepcopy(anchor)
  replacement_anchor.revision = string.rep("b", 40)
  conflict = nil
  assert(review.rebind(state, function(value, failure) assert(not value) conflict = failure end))
  wait_until(function() return delayed ~= nil end, "inline-revision rebind did not render the replacement document")
  delayed()
  wait_until(function() return conflict ~= nil end, "inline revision conflict did not fail the rebind")
  assert(conflict:find("inline revision", 1, true))
  assert(closed == replacement_document and state.document == previous_document)
  assert(vim.deep_equal(buffer.lines(retained_buffer), previous_text))
  replacement_anchor, uncertain, delayed, queued_save = anchor, true, nil, nil
  state.pending_operation = { kind = "save", text = { title = "Latest title" },
    capture = editable.capture_draft(state.replica.editable, { title = true }) }
  recovered = nil
  assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
  wait_until(function() return delayed ~= nil end, "uncertain rebind did not render the replacement document")
  delayed()
  wait_until(function() return recovered and state.view_ready end, "uncertain rebind did not attach its view")
  assert(state.save_uncertain and state.pending_operation and not queued_save,
    "uncertain replacement host published a queued capture")
  eq(state.pending_operation.capture[1].text, "Latest title")
  assert(vim.bo[retained_buffer].modified)
  local original_attach = comments.attach
  local protected_document = state.document
  local protected_text = buffer.lines(retained_buffer)
  vim.wo[window].number, vim.wo[window].relativenumber = false, true
  local protected_cursor = vim.api.nvim_win_get_cursor(window)
  comments.attach = function(candidate, delivery, recovery)
    if candidate.replica.buffer ~= retained_buffer then
      original_attach(candidate, delivery, recovery)
      error("replacement comment renderer rejected metadata")
    end
    return original_attach(candidate, delivery, recovery)
  end
  delayed, conflict = nil, nil
  assert(review.rebind(state, function(value, failure) assert(not value) conflict = failure end))
  wait_until(function() return delayed ~= nil end, "renderer-failure rebind did not render the replacement document")
  delayed()
  wait_until(function() return conflict ~= nil end, "renderer failure did not fail the rebind")
  assert(state.document == protected_document and state.replica.buffer == retained_buffer)
  assert(vim.deep_equal(buffer.lines(retained_buffer), protected_text))
  assert(vim.bo[retained_buffer].modified)
  assert(not vim.wo[window].number and vim.wo[window].relativenumber)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), protected_cursor))
end

T["semantic"] = function()
  local commit_fixture = vim.g.forge_commit_semantic_fixture == true
  local fixture_path = "nvim/lua/forge/tests/fixtures/protocol/"
    .. (commit_fixture and "review-commit-semantic.json" or "review-semantic.json")
  local fixture = vim.json.decode(table.concat(vim.fn.readfile(fixture_path), "\n"))
  local adapter = require("forge.review_document")
  local nodes = require("forge.nodes")
  local stage = 0
  local current = fixture.initial
  local errors = {}
  adapter._set_runner_for_test(function(method, params, callback)
    if method == "github.review.document.open" then
      callback({ document = current.snapshot.document, diff_view = current.diff_view })
    elseif method == "github.actor.get" then callback({ login = "viewer" })
    elseif method == "github.review.document.header.load" then callback({ ready = true })
    elseif method == "github.review.document.render" then callback(current)
    elseif method == "github.review.document.view.set" then callback(vim.NIL)
    elseif method == "github.review.document.load" then callback({ diagnostic = {} })
    elseif method == "github.review.document.input.apply" then
      local transition = assert(fixture.transition[stage + 1], "unexpected source action")
      assert(params.input.action == transition.action, params.input.action .. " differs from " .. transition.action)
      stage, current = stage + 1, transition.expected
      callback(transition.delivery)
    elseif method == "github.review.document.close" then callback({ closed = true })
    else error("unexpected route " .. method) end
  end)
  local state = adapter.open({ directory = vim.fn.getcwd(), target = { number = 7 },
    on_error = function(message) errors[#errors + 1] = message end })
  wait_until(function() return state.shown and state.view_ready end, "semantic review did not open its view")
  for identity, record in pairs(state.replica.fold.record) do
    if record.owner:match("section:Files:.*:title$") or (commit_fixture and record.owner == "section:Commits:heading") then
      assert(nodes.set_open(state.replica, state.window, identity, true))
    end
  end
  wait_until(function() return not state.replica.update_pending end, "opened file sections did not render")
  local function source_cursor()
    local fallback
    for row, text in ipairs(buffer.lines(state.replica.buffer)) do
      local location = require("forge.buffer").locate(state.replica, row - 1, 0)
      local desired = not commit_fixture or (location and location.block:find("commit:" .. string.rep("f", 40) .. ":", 1, true) == 1)
      if desired and text:find("pub fn visible() {", 1, true) then
        vim.api.nvim_win_set_cursor(state.window, { row, 0 })
        return
      end
      if desired and text:find(commit_fixture and "first_commit()" or "updated()", 1, true) then fallback = row end
    end
    if fallback then vim.api.nvim_win_set_cursor(state.window, { fallback, 0 }) return end
    error("public declaration is absent " .. vim.inspect({ folds = state.replica.fold.record,
      choices = state.replica.projection.choice,
      rendered = buffer.lines(state.replica.buffer) }))
  end
  local function run_source_action(key, expected)
    local mapping = keymap.get(key)
    assert(mapping.buffer == 1 and mapping.callback, "missing " .. key)
    mapping.callback()
    wait_until(function() return stage == expected and not state.action_running end, "source action did not settle")
    assert(state.replica.status == "Applied" and #errors == 0, table.concat(errors, "\n"))
    assert(vim.deep_equal(state.diff_view, current.diff_view))
  end
  source_cursor()
  run_source_action("ox", 1)
  assert(buffer.text(state.replica.buffer):find("{ … 1 change }", 1, true),
    "comment projection omitted the shared semantic summary")
  local visible_heading
  for identity, record in pairs(state.replica.fold.record) do
    local entry = state.replica.block[record.owner]
    if entry and entry.metadata.content_node == identity and nodes.closed(state.replica, identity) then
      visible_heading = true
    end
  end
  assert(visible_heading, "semantic declarations did not start closed")
  local first_window = state.window
  vim.cmd("vsplit")
  wait_until(function() return state.window == vim.api.nvim_get_current_win() and state.view_ready end, "second window did not acquire a native input view")
  assert(state.window ~= first_window and vim.tbl_count(state.window_view) == 2,
    "a second window did not acquire its own native input view")
  source_cursor()
  run_source_action("<Tab>", 2)
  assert(vim.api.nvim_get_current_line():find("pub fn visible() {", 1, true),
    "source expansion moved the cursor away from its retained declaration")
  run_source_action("ov", 3)
  -- The winbar no longer pins the visibility toggle, so the shared diff view carries the state.
  assert(state.diff_view.visibility == "public", "visibility did not switch to public declarations")
  assert(#vim.fn.win_findbuf(state.replica.buffer) == 2, "visibility change dropped a document window")
  assert(not buffer.text(state.replica.buffer):find("hidden", 1, true))
  run_source_action("ox", 4)
  run_source_action("ov", 5)
  local rendered = buffer.text(state.replica.buffer)
  assert(rendered:find(commit_fixture and "first_commit()" or "updated()", 1, true)
    and rendered:find(commit_fixture and "first_private()" or "new_private()", 1, true))
  if commit_fixture then
    assert(rendered:find("second_commit()", 1, true) and rendered:find("pr_current()", 1, true))
  end
  adapter.close(state)
  assert(state.hidden and not vim.bo[state.replica.buffer].modified)
  vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
  wait_until(function() return not state.active end, "deleting the review buffer did not close the document")
end

return T
