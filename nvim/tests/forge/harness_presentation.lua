vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request
local original_accepting = client.host_is_accepting
local markdown = require("forge.render.harness.markdown")
local original_markdown_render = markdown.render
local original_defer = vim.defer_fn
client.host_is_accepting = function() return true end
local requests = {}
local input_state = require("forge.views.harness.input_state")
local session_state = { session = { id = "test-session" } }
local host = dofile("nvim/tests/forge/support/input.lua").new(session_state)
client.request = function(request_specification, callback)
  local session = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  local receive = callback
  callback = function(result, failure, detail)
    if type(result) == "table" and (params.operation == "open" or params.operation == "sync") then
      result.status = result.status or { document = params.document .. ":status", revision = 0, block = {} }
    end
    receive(result, failure, detail)
  end
  assert(session == "test-session")
  if host.request(method, params, callback) then return end
  assert(method == "session.document")
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
local function submit(callback)
  local context, failure = owner.capture_prompt()
  assert(context, failure)
  prompt_submission.start(session_state, { text = "", context = context }, callback)
end
local ok, failure = xpcall(function()
  input_state.attach(session_state, function() end)
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
  local submission = host.requests[#host.requests]
  assert(submission.parameters.action.message.text == "first typed prompt")
  local function retract(id)
    host.revision = host.revision + 1
    input_state.receive(session_state, { revision = host.revision, suspended = true, upsert = {}, removed = {},
      retracted = id or submission.parameters.action.message.id })
  end
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "")
  host.complete(submission)
  retract("stale-message")
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "", "stale retraction restored a draft")
  retract()
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "first typed prompt")
  host.flush()

  submit(function() end)
  submission = host.requests[#host.requests]
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "newer draft", "λ" })
  host.complete(submission)
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "acceptance overwrote newer typing")
  retract()
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "retraction overwrote newer typing")
  host.flush()

  submit(function() end)
  submission = host.requests[#host.requests]
  assert(submission.parameters.action.message.text == "newer draft\nλ", "submission lost multiline Unicode text")
  host.complete(submission)
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "typed after acceptance" })
  retract()
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance")
  host.flush()

  local cancellation
  submit(function(_, message) cancellation = message end)
  host.complete(nil, "interrupted")
  assert(cancellation == "interrupted", "composer dropped submission failure")
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance")
  submit(function() end)
  host.complete(nil, "admission failed")
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance", "failure lost the draft")
  local request_count = #host.requests
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { " " })
  local rejected
  submit(function(result, submission_error) rejected = not result and submission_error end)
  assert(rejected and #host.requests == request_count, "blank draft reached Rust")
  for _, source in ipairs({ { string.rep("x", 65537) }, vim.fn["repeat"]({ "row" }, 4097) }) do
    vim.api.nvim_buf_set_lines(composer, 0, -1, false, source)
    submit(function() end)
    local large_submission = host.requests[#host.requests]
    assert(large_submission.parameters.action.message.text == table.concat(source, "\n"), "valid draft was truncated")
    host.complete(large_submission)
  end
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "draft at close" })
  local completed_after_close = 0
  submit(function() completed_after_close = completed_after_close + 1 end)
  submission = host.requests[#host.requests]
  assert(owner.close())
  assert(requests[#requests].params.operation == "close")
  host.complete(submission)
  host.complete(submission)
  retract()
  assert(completed_after_close == 1,
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
  requests[#requests].callback({ patch = {}, status = { document = owner.document .. ":status", revision = 0, block = {} } })
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
  local retained_lines = vim.api.nvim_buf_get_lines(transcript, 0, -1, false)
  local retained_cursor = vim.api.nvim_win_get_cursor(window)
  local retained_view = vim.api.nvim_win_call(window, vim.fn.winsaveview)
  local retained_revision = owner.transcript.revision
  refresh().callback(nil, "duplicate block identity", { code = "invalid_state" })
  local failed_count = #requests
  for _ = 1, 3 do owner.sync() end
  assert(#requests == failed_count, "failed presentation retried a request requiring reopening")
  assert(owner.failure and owner.failure:find("Reopen Harness to retry", 1, true))
  assert(not owner.pending and not owner.syncing, "permanent projection failure left publication pending")
  assert(#notices == 1, "refresh failure flooded notifications: " .. vim.inspect(notices))
  assert(owner.transcript.status == "Applied" and owner.transcript.revision == retained_revision,
    "permanent projection failure invalidated the usable transcript revision")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), retained_lines),
    "permanent projection failure replaced usable transcript content")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), retained_cursor)
    and vim.deep_equal(vim.api.nvim_win_call(window, vim.fn.winsaveview), retained_view),
    "permanent projection failure moved the transcript cursor or viewport")
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

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  local markdown_publication = {}
  markdown.render = function(buffer, rendered_window, ranges)
    markdown_publication[#markdown_publication + 1] = {
      buffer = buffer, window = rendered_window, ranges = vim.deepcopy(ranges),
    }
  end
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local markdown_opening = requests[#requests]
  local markdown_history = snapshot(markdown_opening.params.document, "**retained response**")
  markdown_history.block[1].metadata.markdown = true
  markdown_history.block[2] = { id = "tail", text = { "" },
    metadata = { target = {}, decoration = {}, editable_region = {} } }
  markdown_opening.callback({ transcript = markdown_history })
  local rendered_ranges = vim.deepcopy(owner.markdown_ranges)
  local render_count = #markdown_publication
  assert(render_count > 0 and rendered_ranges[1].id == "body", "initial response Markdown was not published")
  refresh().callback({ patch = { {
    document = owner.document, base = 0, next = 1, base_rows = 2, next_rows = 4,
    base_blocks = 2, next_blocks = 2,
    text_edit = { { start_row = 1, removed_rows = 1, text = { "", "new prompt", "Working" } } },
    block_edit = {}, removed_block = {},
    metadata_edit = { { block = "tail", row_count = 3, metadata = markdown_history.block[2].metadata } },
  } } })
  assert(vim.wait(500, function() return owner.transcript.revision == 1 end, 1),
    "prompt insertion did not publish")
  assert(vim.deep_equal(owner.markdown_ranges, rendered_ranges),
    "appending a prompt changed the retained response's Markdown ownership: "
      .. vim.inspect({ before = rendered_ranges, after = owner.markdown_ranges }))
  assert(#markdown_publication > render_count,
    "appending non-Markdown text did not restore retained response parser boundaries")
  render_count = #markdown_publication
  refresh().callback({ patch = {}, status = { document = owner.document .. ":status", revision = 0, block = {} } })
  assert(#markdown_publication == render_count, "unchanged publication rerendered Markdown")
  refresh().callback({ patch = { {
    document = owner.document, base = 1, next = 2, base_rows = 4, next_rows = 4,
    base_blocks = 2, next_blocks = 2, text_edit = {}, block_edit = {}, removed_block = {},
    metadata_edit = { { block = "tail", row_count = 3, metadata = markdown_history.block[2].metadata } },
  } } })
  assert(vim.wait(500, function() return owner.transcript.revision == 2 end, 1),
    "metadata-only publication did not settle")
  assert(#markdown_publication == render_count, "non-Markdown metadata-only publication rerendered Markdown")
  assert(owner.close())
  markdown.render = original_markdown_render

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  vim.cmd("belowright 3split")
  local composer_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(composer_window, composer)
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  opening = requests[#requests]
  local idle = snapshot(opening.params.document, "")
  idle.block[1].text = {}
  idle.block[1].metadata.status = nil
  opening.callback({ transcript = idle })
  local status_revision = owner.transcript.revision
  local status_cursor = vim.api.nvim_win_get_cursor(window)
  for _, resize in ipairs({
    { height = 7, event = "WinResized" },
    { height = 3, event = "VimResized" },
    { height = 5, event = "WinResized" },
  }) do
    vim.api.nvim_win_set_height(composer_window, resize.height)
    vim.api.nvim_exec_autocmds(resize.event, {})
    local height = vim.api.nvim_win_get_height(window) - (vim.wo[window].winbar ~= "" and 1 or 0)
    local measured = vim.api.nvim_win_text_height(window, { start_row = 0 }).all
    assert(owner.status_view.window == nil,
      ("resize left idle status using %d rows in a %d-row pane"):format(measured, height))
    assert(owner.transcript.revision == status_revision, "resize required a new transcript revision")
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), { "" }),
      "resize inserted spacing into transcript text")
    assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), status_cursor), "resize moved the transcript cursor")
    assert(vim.api.nvim_get_current_win() == composer_window, "resize stole composer focus")
  end
  assert(owner.close())

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local staged_request, reveal_timer = {}, {}
  local elapsed = 0
  vim.defer_fn = function(callback, delay)
    if delay ~= 100 then return original_defer(callback, delay) end
    local timer = { callback = callback, deadline = elapsed + delay }
    function timer:stop() self.stopped = true end
    function timer:close() self.closed = true end
    reveal_timer[#reveal_timer + 1] = timer
    return timer
  end
  local function advance(milliseconds)
    elapsed = elapsed + milliseconds
    for _, timer in ipairs(reveal_timer) do
      if not timer.stopped and not timer.closed and timer.deadline <= elapsed then
        timer:close()
        timer.callback()
      end
    end
  end
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, read_only = true,
    request = function(params, callback)
      if params.operation == "open" then
        callback({ transcript = snapshot(params.document, "Working"), status = { document = params.document .. ":status", revision = 0, block = {} } })
      else
        if params.operation == "staging" then staged_request[#staged_request + 1] = params end
        callback({ patch = {}, status = { document = params.document .. ":status", revision = 0, block = {} } })
      end
    end,
  }, function(value, error_message) assert(value and not error_message) end)
  owner.sync = function() end
  local first = { id = "first", text = "first direction" }
  local second = { id = "second", text = "second direction" }
  owner.set_staging({}, { first })
  advance(99)
  assert(#staged_request == 0, "hidden steering was published before its reveal deadline")
  owner.set_staging({}, {})
  local cancelled = reveal_timer[1]
  assert(cancelled.stopped and cancelled.closed, "accepted steering retained its reveal timer")
  owner.set_staging({}, { second })
  cancelled.callback()
  advance(1)
  assert(#staged_request == 0, "stale reveal published a newer steering message early")
  owner.set_staging({}, { second, first })
  assert(#reveal_timer == 2, "steering updates postponed the original reveal deadline")
  advance(99)
  assert(#staged_request == 1 and #staged_request[1].steering == 2,
    "reveal did not publish the latest steering messages at 100 ms")
  owner.set_staging({}, { first })
  assert(#staged_request == 2 and staged_request[2].steering[1].id == first.id,
    "visible steering updates acquired another delay")
  owner.set_staging({}, {})
  assert(#staged_request == 3 and #staged_request[3].steering == 0,
    "accepted visible steering was not removed immediately")
  owner.set_staging({}, { second })
  local before_queue = reveal_timer[#reveal_timer]
  owner.set_staging({ first }, { second })
  assert(#staged_request == 4 and before_queue.stopped,
    "explicit queue did not reveal immediately and cancel the steering delay")
  owner.set_staging({}, {})
  owner.set_staging({}, { first })
  local before_failure = reveal_timer[#reveal_timer]
  owner.set_staging({}, { { id = first.id, text = "failed direction", failed = true } })
  assert(#staged_request == 6 and staged_request[6].steering[1].failed and before_failure.stopped,
    "failure was hidden behind the steering delay")
  owner.set_staging({}, {})
  owner.set_staging({}, { second })
  local before_close = reveal_timer[#reveal_timer]
  assert(owner.close())
  assert(before_close.stopped and before_close.closed, "closing retained the steering reveal timer")
  before_close.callback()
  owner.set_staging({}, { first })
  advance(100)
  assert(#staged_request == 7, "closed presentation published delayed steering")
end, debug.traceback)
vim.defer_fn = original_defer
client.request = original_request
client.host_is_accepting = original_accepting
markdown.render = original_markdown_render
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_presentation OK") vim.cmd("qa!") end
