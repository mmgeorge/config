vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request_for
local original_accepting = client.host_accepting
client.host_accepting = function() return true end
local requests = {}
client.request_for = function(session, method, params, callback)
  assert(session == "test-session" and (method == "harness.document" or method == "prompt.submit"))
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
local function snapshot(document, text, editable)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = editable and {
      { id = "composer", revision = 0, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } },
    } or {} } } } }
end
local owner
local prompt_submission = require("forge.views.harness.prompt_submission")
local session_state = { session = { id = "test-session" } }
local function submit(callback)
  local context, failure = owner.capture_prompt()
  assert(context, failure)
  prompt_submission.start(session_state, { text = "", context = context }, callback)
end
local ok, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "draft" })
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local attached
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, error_message) assert(not error_message, error_message) attached = value end)
  local opened = requests[1].params
  assert(opened.initial == nil and opened.composer == nil, "opening transmitted an unsent draft")
  local history = snapshot(opened.document, "native transcript")
  history.block[1].text = { "native transcript", "previous exchange", "previous answer" }
  history.block[1].metadata.node = {
    id = "body", kind = "thought", order = 0, lifecycle = "live", more = false,
    default_display = "full", display = "full", generation = 1, content_revision = 1,
    loaded_rows = 3, loaded_bytes = #table.concat(history.block[1].text, "\n"),
  }
  requests[1].callback({ transcript = history })
  assert(attached == owner and owner.ready)
  assert(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)[1] == "native transcript")
  assert(vim.bo[composer].modifiable and not vim.bo[transcript].modifiable)
  owner.activate(function() error("plain transcript text was activated") end)
  assert(#requests == 1, "plain transcript text dispatched an action without a target")
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(not owner.toggle_heading(window), "a thought without activity offered a fold")
  assert(#requests == 1, "a thought without activity dispatched an expansion")
  owner.sync()
  owner.sync()
  assert(#requests == 1, "refresh bypassed the publication interval")
  assert(vim.wait(500, function() return #requests == 2 end, 1))
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  requests[2].callback({ snapshot = vim.NIL, patch = {} })
  assert(owner.transcript.status == "Applied", "null snapshot desynchronized incremental transcript updates")
  assert(#requests == 2, "one burst dispatched redundant refreshes")
  assert(vim.api.nvim_win_get_cursor(window)[1] == 1, "native patch publication moved a reader away from history")
  vim.api.nvim_buf_set_text(composer, 0, 0, 0, 5, { "first typed prompt" })
  assert(#requests == 2, "typing sent a composer request")
  owner.follow_tail()
  assert(vim.api.nvim_win_get_cursor(window)[1] == 3, "explicit agent action did not follow the tail")
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  submit(function() end)
  assert(vim.api.nvim_win_get_cursor(window)[1] == 3, "explicit submission did not resume following the transcript tail")
  local submission = requests[3]
  assert(submission.method == "prompt.submit" and submission.params.text == "first typed prompt")
  assert(submission.params.submission.document == opened.document and not submission.params.composer)
  local function transition(state, token)
    return prompt_submission.receive(session_state, { kind = "prompt_submission", data = { document = opened.document,
      token = token or submission.params.submission.token, state = state } })
  end
  assert(not transition("accepted", 99), "stale acceptance cleared a draft")
  assert(transition("accepted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "")
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "first typed prompt")
  submission.callback(nil, "turn_retracted")

  submit(function() end)
  submission = requests[#requests]
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "newer draft", "λ" })
  assert(transition("accepted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "acceptance overwrote newer typing")
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "retraction overwrote newer typing")
  submission.callback(nil, "turn_retracted")

  submit(function() end)
  submission = requests[#requests]
  assert(submission.params.text == "newer draft\nλ", "submission lost multiline Unicode text")
  assert(transition("accepted"))
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "typed after acceptance" })
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance")
  submission.callback(nil, "turn_retracted")

  local cancellation
  submit(function(_, _, detail) cancellation = detail end)
  submission = requests[#requests]
  submission.callback(nil, "interrupted", { code = "turn_cancelled" })
  assert(cancellation and cancellation.code == "turn_cancelled", "composer dropped structured cancellation")
  submit(function() end)
  submission = requests[#requests]
  submission.callback(nil, "admission failed")
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance", "failure lost the draft")
  local request_count = #requests
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { " " })
  local rejected
  submit(function(result, submission_error) rejected = not result and submission_error end)
  assert(rejected and #requests == request_count, "blank draft reached Rust")
  for _, source in ipairs({ { string.rep("x", 65537) }, vim.fn["repeat"]({ "row" }, 4097) }) do
    vim.api.nvim_buf_set_lines(composer, 0, -1, false, source)
    submit(function() end)
    local large_submission = requests[#requests]
    assert(large_submission.params.text == table.concat(source, "\n"), "valid draft was truncated")
    large_submission.callback({})
  end
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "draft at close" })
  local completed_after_close = 0
  submit(function() completed_after_close = completed_after_close + 1 end)
  submission = requests[#requests]
  assert(owner.close())
  assert(requests[#requests].params.operation == "close")
  assert(not transition("accepted"), "closed presentation accepted a late submission event")
  submission.callback({})
  submission.callback({})
  assert(completed_after_close == 1 and not session_state.prompt_submission,
    "presentation close discarded or duplicated request completion")
  assert(not vim.api.nvim_buf_is_valid(composer), "closing the workspace retained its owned composer buffer")

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  local rejected
  require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end,
  }, function(value, error_message) assert(value and not error_message) rejected = false end)
  local delayed = requests[#requests]
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "new startup draft" })
  local changedtick = vim.api.nvim_buf_get_changedtick(composer)
  delayed.callback({ transcript = snapshot(delayed.params.document, "stale transcript") })
  assert(rejected == false, "typing during startup rejected the transcript")
  assert(vim.api.nvim_buf_is_valid(composer) and vim.api.nvim_buf_get_changedtick(composer) == changedtick)
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "new startup draft")

  vim.api.nvim_buf_delete(transcript, { force = true })
  vim.api.nvim_buf_delete(composer, { force = true })

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  local notices = {}
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) notices[#notices + 1] = message end,
  }, function(value, error_message) assert(value and not error_message) end)
  local initial = requests[#requests]
  local stream = snapshot(initial.params.document, "retained history")
  for row = 1, vim.api.nvim_win_get_height(window) * 2 do
    stream.block[1].text[#stream.block[1].text + 1] = "history row " .. row
  end
  initial.callback({ transcript = stream })
  local function refresh()
    local count = #requests
    owner.sync()
    assert(vim.wait(500, function()
      return #requests > count and requests[#requests].params.operation == "sync"
    end, 1))
    return requests[#requests]
  end
  owner.follow_tail()
  local before_stream = vim.api.nvim_win_get_cursor(window)
  stream.revision = 1
  stream.block[1].text[#stream.block[1].text + 1] = "streamed at bottom"
  refresh().callback({ snapshot = vim.deepcopy(stream) })
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), before_stream),
    "snapshot publication moved a window already at the bottom")
  local delayed = refresh()
  vim.api.nvim_win_call(window, function() vim.cmd("normal! ggzt") end)
  local reading_view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
  local composer_window = vim.api.nvim_open_win(composer, true, {
    relative = "editor", row = 0, col = 0, width = 20, height = 3, style = "minimal",
  })
  stream.revision = 2
  stream.block[1].text[#stream.block[1].text + 1] = "streamed while reading"
  delayed.callback({ snapshot = vim.deepcopy(stream) })
  assert(vim.api.nvim_win_get_cursor(window)[1] == 1,
    "an in-flight snapshot ignored navigation before publication")
  assert(vim.api.nvim_win_call(window, vim.fn.winsaveview).topline == reading_view.topline,
    "composer focus resumed following while the transcript was scrolled up")
  vim.api.nvim_win_close(composer_window, true)
  local retry = refresh()
  local revision = owner.transcript.revision
  retry.callback(nil, "temporary admission contention", { code = "not_admitted" })
  assert(not owner.failure and owner.transcript.revision == revision)
  local previous = #requests
  assert(vim.wait(2000, function() return #requests > previous end, 1), "transcript read did not retry")
  requests[#requests].callback({ patch = {} })
  assert(not owner.failure and not owner.sync_failure, "successful retry retained its diagnostic")
  owner.set_staging({ "queued draft" }, {})
  local staging = requests[#requests]
  assert(staging.params.operation == "staging" and staging.params.sequence)
  previous = #requests
  staging.callback(nil, "response lost after application", { code = "outcome_unknown" })
  assert(vim.wait(2000, function() return #requests > previous end, 1))
  assert(requests[#requests].params.sequence == staging.params.sequence, "staging retry changed mutation identity")
  requests[#requests].callback({})
  assert(not owner.failure)
  refresh().callback(nil, "duplicate block identity", { code = "invalid_state" })
  local failed_count = #requests
  for _ = 1, 3 do owner.sync() end
  assert(#requests == failed_count, "failed presentation retried a request requiring reopening")
  assert(owner.failure and owner.failure:find("Reopen Harness to retry", 1, true))
  assert(#notices == 1, "refresh failure flooded notifications: " .. vim.inspect(notices))
  assert(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)[1] == "retained history")
  assert(owner.close())
  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local opening = requests[#requests]
  local collapsed = snapshot(opening.params.document, "Thought with deferred tools")
  collapsed.block[1].metadata.node = vim.deepcopy(history.block[1].metadata.node)
  collapsed.block[1].metadata.node.default_display = "heading"
  collapsed.block[1].metadata.node.display = "heading"
  opening.callback({ transcript = collapsed })
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(owner.toggle_heading(window), "collapsed thought without materialized fold ranges did not open")
  local expansion = requests[#requests].params
  assert(expansion.operation == "node" and expansion.node == "body"
    and expansion.action == "set_expansion" and expansion.expanded,
    "deferred thought did not request its owned activity")
  assert(owner.close())
end, debug.traceback)
client.request_for = original_request
client.host_accepting = original_accepting
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_presentation OK") vim.cmd("qa!") end
