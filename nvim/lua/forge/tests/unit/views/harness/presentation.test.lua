-- Unit tests for `forge.views.harness.presentation` (presentation.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "harness_file_paging", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local expanded, loaded, revision = false, 0, 0
local requests, notices, pending = {}, {}, {}
local total, hold = 300, false
local owner
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.o.lines, vim.o.columns = 24, 100
vim.api.nvim_win_set_buf(window, transcript)

local function snapshot(document)
  local body = {}
  for row = 1, loaded do body[row] = ("source row %03d"):format(row) end
  local last = expanded and "file:deferred-body" or "empty"
  local blocks = {
    { id = "file", text = { "Modified source.rs" }, metadata = {
      node = { id = "file", kind = "file", lifecycle = "settled", generation = 1,
        content_revision = revision, loaded_rows = loaded, loaded_bytes = 0, order = 0, more = false,
        display = expanded and "full" or "heading", default_display = "heading", expansion = expanded },
      section = { { id = "file", revision = revision, open = expanded, more = false } },
      fold = { { id = "file", start = { row = 0, column = 0 },
        ["end"] = { block = last, position = { row = 1, column = 0 } },
        closed = true, expand_children = true } },
    } },
  }
  if expanded then
    blocks[#blocks + 1] = { id = "hunk", text = { "@@ -1 +1 @@" }, metadata = {
      section = { { id = "hunk", revision = revision, open = true, more = false } },
      fold = { { id = "hunk", start = { row = 0, column = 0 },
        ["end"] = { block = "rows", position = { row = #body, column = 0 } }, closed = false } },
    } }
    blocks[#blocks + 1] = { id = "rows", text = body, metadata = {} }
    blocks[#blocks + 1] = { id = "file:deferred-body", text = { loaded < total and "More content available" or "End" }, metadata = {
      section = loaded < total and { { id = "file", revision = revision, open = true, more = true } } or {},
    } }
  else
    blocks[#blocks + 1] = { id = "empty", text = { "" }, metadata = {} }
  end
  for _, block in ipairs(blocks) do
    block.metadata.target = {}
    block.metadata.decoration = {}
    block.metadata.editable_region = {}
  end
  return { document = document, revision = revision, block = blocks }
end

local function deliver(params, callback)
  if params.operation == "node" then
    assert(params.node == "file", "hunk body caused a separate request")
    assert(params.rows == 2 * vim.api.nvim_win_get_height(window))
    assert(params.width.columns > 0)
    expanded = params.expanded
    if expanded then loaded = math.min(total, (params.action == "load_more") and loaded + params.rows or math.max(loaded, params.rows)) end
    revision = revision + 1
    callback({})
  elseif params.operation == "sync" then callback({ snapshot = snapshot(params.document) })
  else callback({}) end
end

client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false })
  elseif params.operation == "open" then
    vim.schedule(function() callback({ transcript = snapshot(params.document) }) end)
  else
    requests[#requests + 1] = params
    if hold and params.operation == "node" and (params.action == "load_more") then
      pending[#pending + 1] = { params = params, callback = callback }
    else deliver(params, callback) end
  end
end

local function settled()
  return not owner.syncing and not owner.applying and not owner.pending and not owner.transcript.update_pending
end
local function page_count()
  local count = 0
  for _, request in ipairs(requests) do
    if request.operation == "node" and (request.action == "load_more") then count = count + 1 end
  end
  return count
end
local ok, failure = xpcall(function()
  owner = require("forge.views.harness.presentation").open({
    session_id = "file-paging", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) notices[#notices + 1] = message end,
  }, function() end)
  assert(vim.wait(1000, function() return owner.ready end, 1), table.concat(notices, "\n"))
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return expanded and settled() and owner.transcript.block.file.metadata.node.display == "full" end, 1))
  local initial = loaded
  assert(initial == 2 * vim.api.nvim_win_get_height(window))
  for _ = 1, 5 do owner.observe_sections() end
  assert(page_count() == 0, "visible heading fetched another page")
  assert(owner.transcript.block.hunk ~= nil, "initial hunk was not opened with its file")

  hold = true
  vim.api.nvim_win_set_cursor(window, { math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  local picker = vim.api.nvim_open_win(composer, true, {
    relative = "editor", row = 2, col = 2, width = 30, height = 4, style = "minimal",
  })
  owner.observe_sections()
  assert(#pending == 0, "approval picker triggered background paging")
  vim.api.nvim_win_close(picker, true)
  vim.api.nvim_set_current_win(window)
  owner.observe_sections()
  assert(#pending == 1, "one-screen lead did not request the next page")
  for _ = 1, 8 do owner.observe_sections() end
  assert(#pending == 1, "duplicate page requests while delivery was pending")
  local cursor = vim.api.nvim_win_get_cursor(window)
  local top = vim.fn.line("w0")
  local delivery = table.remove(pending, 1)
  deliver(delivery.params, delivery.callback)
  assert(vim.wait(1000, settled, 1))
  assert(loaded == initial + 2 * vim.api.nvim_win_get_height(window))
  assert(vim.deep_equal(cursor, vim.api.nvim_win_get_cursor(window)), "page delivery moved cursor")
  assert(top == vim.fn.line("w0"), "page delivery moved viewport")
  assert(page_count() == 1, "publication fetched another page away from the boundary")

  vim.api.nvim_win_set_cursor(window, { loaded - math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  owner.observe_sections()
  assert(#pending == 1)
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, settled, 1))
  delivery = table.remove(pending, 1)
  -- A stale page response cannot reopen a fold the user has closed.
  delivery.callback({})
  assert(vim.wait(1000, settled, 1))
  assert(owner.transcript.block.file.metadata.node.display == "heading")
  assert(#notices == 0, table.concat(notices, "\n"))

  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return settled() and owner.transcript.block.file.metadata.node.display == "full" end, 1))
  vim.api.nvim_win_set_cursor(window, { loaded - math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  owner.observe_sections()
  assert(#pending == 1)
  delivery = table.remove(pending, 1)
  -- The host rejects an oversized page with a typed error; a failure without detail is an
  -- unknown outcome that the presentation retries.
  delivery.callback(nil, "section exceeds the 16 MiB loaded-body limit", { code = "invalid_request" })
  local failed_pages = page_count()
  for _ = 1, 20 do owner.observe_sections() end
  assert(page_count() == failed_pages and #pending == 0, "failed page cascaded into automatic retries")
  -- A failed section reports inline on its heading rather than through a notification.
  assert(#notices == 0, table.concat(notices, "\n"))
  local failure_marks = vim.api.nvim_buf_get_extmarks(transcript,
    vim.api.nvim_create_namespace("ForgeHarnessLoading" .. transcript), 0, -1, { details = true })
  assert(#failure_marks == 1 and failure_marks[1][4].virt_text[1][1]:find("Section unavailable", 1, true),
    "failed page did not mark its section unavailable")
  owner.set_section("file", true, false)
  assert(vim.wait(1000, settled, 1), "explicit retry was blocked")
end, debug.traceback)
if owner then owner.close() end
assert(ok, failure)
print("harness_file_paging: passed")
vim.cmd("qa!")
  end },
  { name = "presentation", run = function()
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
local host = dofile("nvim/lua/forge/tests/support/input.lua").new(session_state)
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

  -- A deleted session is terminal: the retry loop must stop instead of polling every second.
  local deferred = {}
  vim.defer_fn = function(callback, delay)
    local timer = { callback = callback, delay = delay }
    function timer:stop() self.stopped = true end
    function timer:close() self.closed = true end
    deferred[#deferred + 1] = timer
    return timer
  end
  local function pending_deferred(matches)
    local pending = {}
    for _, timer in ipairs(deferred) do
      if matches(timer.delay) and not timer.stopped and not timer.closed then pending[#pending + 1] = timer end
    end
    return pending
  end
  local function fire_deferred(matches)
    local pending = pending_deferred(matches)
    for _, timer in ipairs(pending) do
      timer.closed = true
      timer.callback()
    end
    return #pending
  end
  local function is_retry(delay) return delay == 1000 end
  local function is_publication(delay) return delay ~= 1000 and delay ~= 30000 end
  local deleted_notices = {}
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end,
    notice = function(message) deleted_notices[#deleted_notices + 1] = message end,
  }, function(value, error_message) assert(value and not error_message) end)
  requests[#requests].callback({ transcript = snapshot(requests[#requests].params.document, "deleted history") })
  assert(owner.ready, "deleted-session fixture did not open")
  local before_sync = #requests
  owner.sync()
  while #requests == before_sync and fire_deferred(is_publication) > 0 do end
  assert(#requests == before_sync + 1 and requests[#requests].params.operation == "sync",
    "sync was not dispatched")
  requests[#requests].callback(nil, "temporary admission contention", { code = "not_admitted" })
  assert(owner.sync_failure and #pending_deferred(is_retry) == 1, "transient failure did not schedule a retry")
  local before_retry = #requests
  assert(fire_deferred(is_retry) == 1)
  assert(#requests == before_retry + 1 and requests[#requests].params.operation == "sync",
    "retry did not reissue the transcript read")
  requests[#requests].callback(nil, "session not found", { code = "session_not_found" })
  assert(#pending_deferred(is_retry) == 0, "deleted session scheduled another transcript retry")
  assert(not owner.sync_failure, "deleted session retained the retry footer")
  assert(owner.failure and owner.failure:find("no longer exists", 1, true)
    and not owner.failure:find("Reopen", 1, true),
    "deleted session was not marked terminal: " .. tostring(owner.failure))
  local terminal_count = #requests
  for _ = 1, 3 do owner.sync() end
  fire_deferred(is_publication)
  fire_deferred(is_retry)
  assert(#requests == terminal_count, "deleted session dispatched further transcript reads")
  assert(#deleted_notices == 1, "deleted session flooded notifications: " .. vim.inspect(deleted_notices))
  assert(owner.close())
  vim.defer_fn = original_defer
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
  end },
  { name = "publication", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local state = { session = { id = "publication" } }
local host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
local requests = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  if host.request(request_specification.method, params, callback) then return end
  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  requests[#requests + 1] = { params = params, callback = callback, time = vim.uv.hrtime() }
end
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(transcript)
local publications = 0
local owner = require("forge.views.harness.presentation").open({
  session_id = "publication", transcript_buffer = transcript, composer_buffer = composer,
  transcript_window = vim.api.nvim_get_current_win(), is_alive = function() return true end,
  notice = function(message) error(message) end, on_update = function() publications = publications + 1 end,
}, function(value, failure) assert(value and not failure, failure) end)
requests[1].callback({ transcript = { document = owner.document, revision = 0, block = {
  { id = "body", text = { "frame 0" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
} } })
local function burst(expected)
  local started = vim.uv.hrtime()
  local defer = vim.defer_fn
  local scheduled_delay
  vim.defer_fn = function(callback, delay)
    scheduled_delay = delay
    return defer(callback, delay)
  end
  for _ = 1, 100 do owner.sync() end
  vim.defer_fn = defer
  assert(scheduled_delay == 17, "refresh did not schedule the 60 Hz target")
  assert(#requests == expected - 1, "burst published immediately")
  assert(vim.wait(1000, function() return #requests == expected end, 1), "burst did not publish")
  assert((requests[expected].time - started) / 1e6 >= 1000 / 60, "refresh exceeded the 60 Hz target")
end
burst(2)
for _ = 1, 100 do owner.sync() end
local patch = {}
for revision = 1, 64 do
  patch[revision] = { document = owner.document, base = revision - 1, next = revision,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    block_edit = {}, removed_block = {}, metadata_edit = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "frame " .. revision } } },
  }
end
local observations = 0
local timer = vim.uv.new_timer()
timer:start(0, 1, vim.schedule_wrap(function()
  observations = observations + 1
  local text = vim.api.nvim_buf_get_lines(transcript, 0, -1, false)
  assert(#text == 1 and (text[1] == "frame 0" or text[1] == "frame 64"), "partial batch reached the main loop")
end))
local completed = vim.uv.hrtime()
local initial_publications = publications
requests[2].callback({ patch = patch })
assert(owner.transcript.revision == 64)
assert(publications == initial_publications + 1, "batch notified before its final revision")
assert(vim.wait(1000, function() return #requests == 3 end, 1), "in-flight burst lost its trailing update")
assert((requests[3].time - completed) / 1e6 >= 1000 / 60, "trailing publication exceeded the 60 Hz target")
requests[3].callback({ patch = {} })
timer:stop()
timer:close()
assert(observations > 0)
input.attach(state, function() end)
---@type string[]
local prompt_rows = {}
for index = 1, 5000 do prompt_rows[index] = string.rep("complete prompt ", 5) end
local prompt = table.concat(prompt_rows, "\n")
assert(#prompt > 65536)
vim.api.nvim_buf_set_lines(composer, 0, -1, false, prompt_rows)
local submitted = false
require("forge.views.harness.prompt_submission").start(state,
  { text = prompt, context = assert(owner.capture_prompt()) }, function(result, failure)
  assert(result and not failure, failure)
  submitted = true
end)
local submission = host.requests[#host.requests]
assert(submission.parameters.action.kind == "submit"
  and submission.parameters.action.message.text == prompt, "large composer draft lost content")
host.complete(submission)
assert(submitted, "large composer submission did not finish")
owner.sync()
assert(owner.close())
local closed_count = #requests
vim.wait(100, function() return false end, 1)
assert(#requests == closed_count, "closed presentation dispatched a pending refresh")
print("harness_publication: passed, atomic 64-patch batch and 60 Hz target with trailing updates")
vim.cmd("qa!")
  end },
  { name = "sections", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local expanded, revision, generation = false, 0, 1
local requests, notices = {}, {}
local held, deferred = {}, false
local owner
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local function snapshot(document)
  local value = { document = document, revision = revision, block = {
    { id = "heading", text = { "Tools" }, metadata = {
      node = { id = "tools", kind = "tool_group", lifecycle = "settled", generation = generation, order = 0, more = false,
        content_revision = revision, loaded_rows = expanded and 1 or 0, loaded_bytes = 0,
        display = expanded and "full" or "heading", default_display = "heading", expansion = expanded },
      target = {}, decoration = {}, editable_region = {},
      section = { { id = "tools", revision = revision, open = expanded, more = false } },
      fold = { { id = "tools", start = { row = 0, column = 0 },
        ["end"] = { block = expanded and "body" or "heading", position = { row = 1, column = 0 } }, closed = not expanded } },
    } },
    { id = "body", text = { expanded and "loaded only on demand" or "" },
      metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
  if not expanded then table.remove(value.block) end
  return value
end
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  requests[#requests + 1] = params
  if deferred and (params.operation == "node" or params.operation == "sync") then
    held[#held + 1] = { params = params, callback = callback }
    return
  end
  if params.operation == "background_terminals" then callback({ supported = false })
  elseif params.operation == "open" then
    vim.schedule(function() callback({ transcript = snapshot(params.document) }) end)
  elseif params.operation == "node" then
    expanded = params.expanded
    revision = revision + 1
    callback({})
  elseif params.operation == "sync" then callback({ snapshot = snapshot(params.document) })
  else callback({}) end
end
local function respond(failure)
  local pending = assert(table.remove(held, 1), "no held request")
  if failure then pending.callback(nil, failure, { code = "request_failed" })
  elseif pending.params.operation == "node" then
    if pending.params.generation == generation then expanded = pending.params.expanded end
    revision = revision + 1
    pending.callback({})
  else pending.callback({ snapshot = snapshot(pending.params.document) }) end
  return pending.params.operation
end
local function body_loaded()
  return table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("loaded only", 1, true) ~= nil
end
local ok, failure = xpcall(function()
  owner = require("forge.views.harness.presentation").open({
    session_id = "sections", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) notices[#notices + 1] = message end,
  }, function() end)
  assert(vim.wait(1000, function() return owner.ready end, 1))
  assert(owner.transcript.block.heading.metadata.node.display == "heading")
  assert(not table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("loaded only", 1, true))
  local loading_namespace = vim.api.nvim_get_namespaces()["ForgeHarnessLoading" .. transcript]
  local collapsed_lines = vim.api.nvim_buf_get_lines(transcript, 0, -1, false)
  deferred = true
  assert(owner.toggle_heading(window))
  assert(owner.transcript.block.heading.metadata.node.display == "heading", "unloaded fold opened before delivery")
  assert(owner.transcript.fold_loading[window].tools, "opening did not publish loading state")
  assert(#vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, {}) == 0,
    "pending expansion displayed a loading marker")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), collapsed_lines),
    "pending expansion replaced the collapsed heading")
  assert(not body_loaded())
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(owner.transcript.block.heading.metadata.node.display == "heading", "fold opened on acknowledgement before content")
  assert(#vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, {}) == 0,
    "acknowledged expansion displayed a loading marker before content arrived")
  assert(not body_loaded())
  assert(respond() == "sync")
  assert(vim.wait(1000, function()
    return body_loaded() and owner.transcript.block.heading.metadata.node.display == "full"
  end, 1), "opening the native fold did not demand its body")
  assert(not owner.transcript.fold_loading[window], "loaded fold retained loading state")
  assert(vim.api.nvim_win_get_cursor(window)[1] == 1,
    "section expansion followed the tail instead of retaining its heading")
  deferred = false
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function()
    return not body_loaded()
  end, 1), "closing the native fold retained its body")
  deferred = true
  assert(owner.toggle_heading(window))
  assert(owner.toggle_heading(window))
  assert(not owner.transcript.fold_loading[window], "second toggle did not cancel loading")
  assert(respond() == "node")
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(vim.wait(1000, function() return not owner.syncing and not owner.applying end, 1))
  assert(owner.transcript.block.heading.metadata.node.display == "heading" and not body_loaded(), "cancelled delivery reopened the fold")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(owner.toggle_heading(window))
  respond("fixture delivery failure")
  assert(owner.transcript.block.heading.metadata.node.display == "heading" and not body_loaded())
  assert(not owner.transcript.fold_loading[window], "failed request retained loading state")
  local failure_marks = vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, { details = true })
  assert(#failure_marks == 1 and failure_marks[1][4].virt_text[1][1]:find("Section unavailable", 1, true),
    "failed expansion lost its retry feedback")
  assert(#notices == 0 and owner.section_failure.tools:find("fixture delivery failure", 1, true))
  assert(not require("forge.views.harness.health").notice({ presentation = owner }), "section failure escaped its local scope")
  deferred = false
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return body_loaded() and owner.transcript.block.heading.metadata.node.display == "full" end, 1), "failed opening was not retryable")
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return not body_loaded() end, 1))
  deferred = true
  assert(owner.toggle_heading(window))
  generation = generation + 1
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(vim.wait(1000, function() return #held > 0 end, 1), "retired node generation did not reconcile the opening")
  assert(held[1].params.operation == "node" and held[1].params.generation == generation)
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(body_loaded() and not owner.section_failure.tools, "generation replacement became a section failure")
  local sequence = 0
  for _, request in ipairs(requests) do
    if request.operation == "node" then
      assert(request.sequence > sequence)
      sequence = request.sequence
    end
  end
end, debug.traceback)
if owner then owner.close() end
assert(ok, failure)
print("harness_sections: passed")
vim.cmd("qa!")
  end },
  { name = "timeline_position", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local request_for, host_is_accepting = client.request, client.host_is_accepting
local pending = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  pending[#pending + 1] = { params = params, callback = callback }
end

local function take(operation)
  assert(vim.wait(1000, function() return #pending > 0 end, 1), "missing " .. operation)
  local request = table.remove(pending, 1)
  assert(request and request.params.operation == operation, "expected " .. operation .. ", got " .. vim.inspect(request and request.params))
  return request
end

local function snapshot(document, revision, count)
  local rows = {}
  for row = 1, count do rows[row] = "source row " .. row end
  return { document = document, revision = revision, block = {
    { id = "body", text = rows, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
end

local success, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local owner = require("forge.views.harness.presentation").open({
    session_id = "position", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, message) assert(value and not message) end)
  assert(vim.wo[window].signcolumn == "yes:1" and vim.wo[window].statuscolumn:find("forge.nodes", 1, true)
    and not vim.wo[window].foldenable and vim.wo[window].foldcolumn == "0",
    "initial transcript must use the shared fold marker column")
  local opening = take("open")
  opening.callback({ transcript = snapshot(opening.params.document, 0, 80),
    composer = snapshot(opening.params.composer, 0, 1) })
  local revision = 0
  local function sync(count)
    revision = revision + 1
    take("sync").callback({ snapshot = snapshot(opening.params.document, revision, count) })
  end
  vim.fn.winrestview({ lnum = 40, col = 4, topline = 30 })
  local parent = vim.fn.winsaveview()
  owner.select_agent("child")
  vim.fn.winrestview({ lnum = 45, col = 5, topline = 34 })
  parent = vim.fn.winsaveview()
  take("select_agent").callback({})
  vim.fn.winrestview({ lnum = 48, col = 6, topline = 36 })
  parent = vim.fn.winsaveview()
  sync(8)
  vim.api.nvim_win_set_cursor(window, { 3, 2 })
  owner.select_agent(nil)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }), "parent cursor was lost")
  assert(vim.fn.winsaveview().topline == parent.topline, "parent viewport was lost")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }), "child cursor was lost")

  -- A selection queued during refresh captures the outgoing view only after that refresh settles.
  owner.sync()
  assert(vim.wait(1000, function() return #pending == 1 end, 1))
  owner.select_agent(nil)
  assert(#pending == 1, "timeline selection raced a refresh")
  sync(8)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }))
  assert(#pending == 0)
  owner.highlight()
  local highlighting = take("highlight")
  owner.highlight()
  assert(#pending == 0, "syntax analysis was started twice")
  owner.select_agent("child")
  assert(#pending == 0, "timeline selection bypassed an in-flight presentation request")
  highlighting.callback({})
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }),
    "timeline selection lost the saved child position")
  assert(#pending == 0)

  owner.select_agent(nil)
  take("select_agent").callback({})
  local prepared = take("sync")
  revision = revision + 1
  prepared.callback({ snapshot = snapshot(opening.params.document, revision, 6000) })
  assert(owner.transcript.update_pending, "large timeline did not yield during preparation")
  vim.api.nvim_win_set_cursor(window, { 5, 1 })
  assert(vim.wait(10000, function() return not owner.transcript.update_pending end, 1))
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }), "return did not restore parent history")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 5, 1 }),
    "timeline switch lost movement made during asynchronous preparation")

  owner.select_agent(nil)
  take("select_agent").callback({})
  vim.api.nvim_win_set_cursor(window, { 6, 2 })
  take("sync").callback({ patch = {} })
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 6, 2 }),
    "unchanged timeline switch did not capture its outgoing view")
  owner.close()
  take("close").callback({})
end, debug.traceback)
client.request, client.host_is_accepting = request_for, host_is_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_timeline_position: passed")
vim.cmd("qa!")
  end },
  { name = "transcript_follow", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request
local original_accepting = client.host_is_accepting
local request_list = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  if params.operation == "resize" then callback({}) return end
  request_list[#request_list + 1] = { params = params, callback = callback }
end

local completion_blocks = {}
local function snapshot(document, revision, line_list)
  local block_list = {}
  for row, line in ipairs(line_list) do
    block_list[#block_list + 1] = { id = "row:" .. row, text = { line },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
    if line:find("Working", 1, true) == 1 then
      block_list[#block_list].metadata.status = { row = 0, animated = true }
    end
  end
  vim.list_extend(block_list, vim.deepcopy(completion_blocks))
  return { document = document, revision = revision, block = block_list }
end

local owner
local success, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  vim.wo[window].wrap = true
  vim.cmd("belowright 3new")
  local composer = vim.api.nvim_get_current_buf()
  local composer_window = vim.api.nvim_get_current_win()
  local line_list = {}
  for row = 1, 60 do line_list[row] = "response row " .. row end
  line_list[#line_list + 1] = "Working · 1s"
  line_list[#line_list + 1] = "Inspecting the current thought"
  owner = require("forge.views.harness.presentation").open({
    session_id = "follow-test", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, composer_window = composer_window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, error_message) assert(value and not error_message) end)
  local opening = table.remove(request_list, 1)
  opening.callback({ transcript = snapshot(opening.params.document, 0, line_list) })
  assert(vim.wait(1000, function() return owner.ready end, 1), "opening did not finish")
  assert(vim.api.nvim_get_current_win() == composer_window, "opening stole composer focus")
  assert(vim.api.nvim_win_get_cursor(window)[1] == #line_list, "opening did not position the initial tail")

  local revision = 0
  local function await_publication()
    assert(vim.wait(1000, function()
      return owner.failure or owner.transcript.revision == revision and not owner.applying
    end, 1), owner.transcript.diagnostic or "transcript publication did not finish")
    assert(not owner.failure, owner.failure)
    assert(owner.transcript.status == "Applied", owner.transcript.diagnostic)
  end
  local function take_refresh()
    owner.sync()
    assert(vim.wait(1000, function()
      return #request_list > 0 and request_list[#request_list].params.operation == "sync"
    end, 1), "missing timeline refresh: " .. vim.inspect({ failure = owner.failure, requests = request_list,
      applying = owner.applying, syncing = owner.syncing }))
    return table.remove(request_list)
  end
  local function refresh()
    local pending = take_refresh()
    revision = revision + 1
    pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
    await_publication()
  end
  local function expect_position(target_window, expected, label)
    local actual = vim.api.nvim_win_get_cursor(target_window)
    assert(vim.deep_equal(actual, expected), label .. ": " .. vim.inspect(actual))
  end

  vim.api.nvim_win_set_cursor(window, { #line_list, 4 })
  local thought_cursor = vim.api.nvim_win_get_cursor(window)
  line_list[#line_list] = "Inspecting the current thought and its updated status"
  refresh()
  expect_position(window, thought_cursor, "thought snapshot moved the cursor to its end")
  assert(vim.api.nvim_get_current_win() == composer_window, "thought snapshot stole composer focus")

  local pending = take_refresh()
  line_list[#line_list] = string.rep("wrapped thought ", 80) .. "end"
  revision = revision + 1
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = #line_list, next_rows = #line_list, base_blocks = #line_list, next_blocks = #line_list,
    block_edit = {}, removed_block = {},
    text_edit = { { start_row = #line_list - 1, removed_rows = 1, text = { line_list[#line_list] } } },
    metadata_edit = { { block = "row:" .. #line_list, row_count = 1,
      metadata = { target = {}, decoration = {}, editable_region = {} } } },
  } } })
  expect_position(window, thought_cursor, "wrapped thought patch moved the cursor")

  owner.follow_tail()
  local previous_tail = vim.api.nvim_win_get_cursor(window)
  line_list[#line_list + 1] = "new output after the thought"
  refresh()
  expect_position(window, previous_tail, "append resumed automatic following")

  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  vim.cmd("normal! zt")
  local reading_view = vim.fn.winsaveview()
  line_list[#line_list] = "updated background output"
  refresh()
  expect_position(window, { 12, 4 }, "focused history cursor moved")
  assert(vim.fn.winsaveview().topline == reading_view.topline, "history viewport moved")

  pending = take_refresh()
  vim.api.nvim_win_set_cursor(window, { 15, 5 })
  vim.api.nvim_set_current_win(composer_window)
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  expect_position(window, { 15, 5 }, "delayed response lost movement before publication")

  local second_window = vim.api.nvim_open_win(transcript, false, { split = "right" })
  owner.refresh_views()
  assert(vim.wait(1000, function() return #request_list > 0 end, 1), "missing second view request")
  local view_request = table.remove(request_list)
  assert(view_request.params.operation == "open_view")
  view_request.callback({})
  vim.api.nvim_win_set_cursor(second_window, { #line_list, 3 })
  line_list[#line_list] = "updated output shown in both windows"
  refresh()
  expect_position(window, { 15, 5 }, "sibling publication moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "sibling publication moved the tail cursor")
  pending = take_refresh()
  pending.callback({ patch = {} })
  expect_position(window, { 15, 5 }, "empty refresh moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "empty refresh moved the tail cursor")
  assert(vim.api.nvim_get_current_win() == composer_window, "refresh changed focused window")

  pending = take_refresh()
  revision = revision + 1
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = #line_list, next_rows = #line_list, base_blocks = #line_list, next_blocks = #line_list,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = "row:61", row_count = 1,
      metadata = { target = {}, decoration = {}, editable_region = {}, status = { row = 0, animated = false } } } },
  } } })
  expect_position(window, { 15, 5 }, "status metadata moved the history cursor")
  expect_position(second_window, { #line_list, 3 }, "status metadata moved the tail cursor")

  local layout = require("forge.views.harness.layout")
  vim.api.nvim_win_set_config(second_window, { relative = "editor", row = 0, col = 0,
    width = 30, height = 3, style = "minimal" })
  layout.configure_transcript_window(window)
  vim.wo[window].winbar = "Harness"
  layout.attach_scroll_boundary(transcript, window)
  vim.api.nvim_win_set_height(window, 10)

  ---@param identity string
  ---@param lines string[]
  local function begin_response(identity, lines)
    completion_blocks = {
      { id = identity .. ":prompt", text = { "Prompt for " .. identity }, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity .. ":prompt", kind = "message", lifecycle = "settled", display = "full",
          default_display = "full", generation = 1, more = false, content_revision = 1,
          loaded_rows = 1, loaded_bytes = 24, order = 0 },
        layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessPrompt" } },
      } },
      { id = identity .. ":summary", text = { "Exchange activity" }, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity, kind = "exchange", lifecycle = "live", display = "full", generation = 1,
          default_display = "full", more = false, content_revision = 1, loaded_rows = 1, loaded_bytes = 17, order = 0 },
      } },
      { id = identity .. ":response", text = lines, metadata = {
        target = {}, decoration = {}, editable_region = {},
        node = { id = identity .. ":response", kind = "message", lifecycle = "live",
          display = "full", default_display = "full", generation = 1, more = false, content_revision = 1,
          loaded_rows = #lines, loaded_bytes = #table.concat(lines, "\n"), order = 1 },
        layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessResponse" } },
      } },
    }
    refresh()
    vim.api.nvim_win_set_cursor(window, { 12, 4 })
    vim.api.nvim_win_call(window, function() vim.cmd("normal! zt") end)
  end

  local function finish_response()
    completion_blocks[2].metadata.node.lifecycle = "settled"
    refresh()
  end

  local function expect_prompt_at_top(target_window, label)
    vim.cmd("redraw")
    vim.api.nvim_win_call(target_window, function()
      local view = vim.fn.winsaveview()
      assert(view.topline == #line_list + 1 and view.skipcol == 0, label .. ": " .. vim.inspect(view))
    end)
    assert(vim.api.nvim_get_current_win() == composer_window, label .. ": focus changed")
  end

  local function expect_revealed(label)
    local first = #line_list + 2
    local last = vim.api.nvim_buf_line_count(transcript)
    vim.cmd("redraw")
    vim.api.nvim_win_call(window, function()
      assert(vim.fn.line("w0") <= first and vim.fn.line("w$") == last, label)
      local start = vim.fn.winsaveview().topline - 1
      local measured = vim.api.nvim_win_text_height(window, { start_row = start, end_row = last - 1 })
      assert(measured.all <= vim.api.nvim_win_get_height(window) - 1, label .. ": wrapped tail clipped")
    end)
    assert(vim.api.nvim_get_current_win() == composer_window, label .. ": focus changed")
  end

  begin_response("live-progress", { "Running tool" })
  completion_blocks[3].metadata.node.kind = "tool"
  completion_blocks[3].metadata.node.parent = completion_blocks[2].metadata.node.id
  completion_blocks[3].metadata.layout = nil
  refresh()
  expect_revealed("live tool without a response did not follow from the composer")
  expect_position(window, { vim.api.nvim_buf_line_count(transcript), #"Running tool" - 1 },
    "live tool did not position the tail")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  pending = take_refresh()
  pending.callback({ patch = {} })
  expect_position(window, { 12, 4 }, "unchanged publication replayed live following")

  pending = take_refresh()
  vim.api.nvim_set_current_win(window)
  vim.cmd("normal! vll")
  local progress_cursor, progress_anchor = vim.api.nvim_win_get_cursor(window), vim.fn.getpos("v")
  local progress_view = vim.fn.winsaveview()
  completion_blocks[3].text = { "Running tool with more output" }
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_position(window, progress_cursor, "live update ignored transcript focus at publication")
  assert(vim.fn.mode() == "v" and vim.deep_equal(vim.fn.getpos("v"), progress_anchor),
    "live following changed the reading selection")
  assert(vim.fn.winsaveview().topline == progress_view.topline, "live following moved the focused viewport")
  vim.cmd("normal! \27")

  pending = take_refresh()
  vim.api.nvim_set_current_win(composer_window)
  completion_blocks[3].text = { "Tool finished", "Next tool is running", string.rep("output ", 20) .. "end" }
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_revealed("live following used request-time rather than publication-time focus")
  assert(vim.api.nvim_win_get_cursor(second_window)[1] == vim.api.nvim_buf_line_count(transcript),
    "live following omitted the sibling transcript window")

  -- A permission review opened over the composer mid-stream takes focus; the tail must keep following.
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  vim.api.nvim_set_current_win(composer_window)
  local popup = vim.api.nvim_open_win(vim.api.nvim_create_buf(false, true), true, { relative = "editor",
    row = 1, col = 1, width = 20, height = 2, style = "minimal" })
  completion_blocks[3].text = { "Tool finished", "Permission requested", string.rep("detail ", 20) .. "end" }
  refresh()
  assert(vim.api.nvim_get_current_win() == popup, "live following stole focus from the permission popup")
  assert(vim.api.nvim_win_get_cursor(window)[1] == vim.api.nvim_buf_line_count(transcript),
    "a focused popup over the composer stopped live following")
  vim.api.nvim_win_close(popup, true)
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  popup = vim.api.nvim_open_win(vim.api.nvim_create_buf(false, true), true, { relative = "editor",
    row = 1, col = 1, width = 20, height = 2, style = "minimal" })
  completion_blocks[3].text[2] = "Permission granted"
  refresh()
  expect_position(window, { 12, 4 }, "a popup opened from transcript history pulled the reader to the tail")
  vim.api.nvim_win_close(popup, true)
  vim.api.nvim_set_current_win(composer_window)

  begin_response("short", { "First response line", "Last response line" })
  expect_position(window, { 12, 4 }, "streaming response moved history")
  finish_response()
  expect_revealed("completion outside transcript did not reveal the response from history")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  refresh()
  expect_position(window, { 12, 4 }, "repeated completion replayed reveal")

  begin_response("focused", { "Response while selecting" })
  pending = take_refresh()
  vim.api.nvim_set_current_win(window)
  vim.cmd("normal! vll")
  local selected_cursor, selected_anchor = vim.api.nvim_win_get_cursor(window), vim.fn.getpos("v")
  local selected_view = vim.fn.winsaveview()
  completion_blocks[2].metadata.node.lifecycle = "settled"
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_position(window, selected_cursor, "completion moved focused selection")
  assert(vim.fn.mode() == "v" and vim.deep_equal(vim.fn.getpos("v"), selected_anchor),
    "completion changed the selection anchor or mode")
  assert(vim.fn.winsaveview().topline == selected_view.topline, "completion moved focused viewport")
  vim.cmd("normal! \27")
  vim.api.nvim_set_current_win(composer_window)
  refresh()
  expect_position(window, selected_cursor, "leaving transcript retried a skipped reveal")

  begin_response("focus-left", { "Focus changed before publication" })
  vim.api.nvim_set_current_win(window)
  pending = take_refresh()
  vim.api.nvim_set_current_win(composer_window)
  completion_blocks[2].metadata.node.lifecycle = "settled"
  revision = revision + 1
  pending.callback({ snapshot = snapshot(opening.params.document, revision, line_list) })
  await_publication()
  expect_revealed("completion used request-time focus instead of publication-time focus")

  local height = vim.api.nvim_win_get_height(window) - 1
  local exact = {}
  for row = 1, height - 2 do exact[row] = "Response line " .. row end
  begin_response("exact-fit", exact)
  finish_response()
  expect_revealed("exact-height response did not fit")
  expect_prompt_at_top(second_window, "small sibling window did not reveal its prompt")

  exact[#exact + 1] = "One row beyond the available height"
  begin_response("one-row-over", exact)
  finish_response()
  expect_prompt_at_top(window, "exchange one row taller than the window hid its prompt")

  local long_response = {}
  for row = 1, 100 do long_response[row] = "Response line " .. row end
  vim.wo[window].scrolloff = 3
  begin_response("long", long_response)
  finish_response()
  expect_prompt_at_top(window, "100-line response did not start at its prompt")
  expect_prompt_at_top(second_window, "100-line response did not respect sibling geometry")
  vim.api.nvim_win_set_cursor(window, { 12, 4 })
  refresh()
  expect_position(window, { 12, 4 }, "repeated long completion replayed reveal")

  -- Steering consumed after a long answer starts a new reply; completion must reveal that reply
  -- from its steering row rather than return the reader to the original prompt.
  begin_response("steered", long_response)
  local steering_row = #line_list + 1 + 1 + #long_response + 1
  table.insert(completion_blocks, { id = "steered:steering", text = { "Steering: make it short" }, metadata = {
    target = {}, decoration = {}, editable_region = {},
    node = { id = "steered:steering", kind = "message", lifecycle = "settled", display = "full",
      default_display = "full", generation = 1, more = false, content_revision = 1,
      loaded_rows = 1, loaded_bytes = 23, order = 2 },
    layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessPrompt" } },
  } })
  table.insert(completion_blocks, { id = "steered:reply", text = { "Short reply" }, metadata = {
    target = {}, decoration = {}, editable_region = {},
    node = { id = "steered:reply", kind = "message", lifecycle = "settled", display = "full",
      default_display = "full", generation = 1, more = false, content_revision = 1,
      loaded_rows = 1, loaded_bytes = 11, order = 3 },
    layout = { indent = 2, marker = { text = "*", capture = "ForgeHarnessResponse" } },
  } })
  finish_response()
  vim.cmd("redraw")
  vim.api.nvim_win_call(window, function()
    assert(vim.fn.line("w0") <= steering_row and vim.fn.line("w$") == vim.api.nvim_buf_line_count(transcript),
      "steered completion hid the reply to the steering: " .. vim.inspect(vim.fn.winsaveview()))
  end)

  begin_response("too-tall", { string.rep("wrapped response ", vim.api.nvim_win_get_width(window) * height) })
  finish_response()
  expect_prompt_at_top(window, "oversized wrapped response did not start at its prompt")

  begin_response("folded", { "Preview of a longer response" })
  completion_blocks[3].metadata.node.display = "preview"
  completion_blocks[3].metadata.node.more = true
  finish_response()
  expect_position(window, { 12, 4 }, "folded response was mistaken for a complete response")

  begin_response("metadata-only", { "Response was already streamed" })
  completion_blocks[2].metadata.node.lifecycle = "settled"
  pending = take_refresh()
  revision = revision + 1
  local count = vim.api.nvim_buf_line_count(transcript)
  pending.callback({ patch = { {
    document = opening.params.document, base = revision - 1, next = revision,
    base_rows = count, next_rows = count, base_blocks = #line_list + 3, next_blocks = #line_list + 3,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = completion_blocks[2].id, row_count = 1, metadata = completion_blocks[2].metadata } },
  } } })
  await_publication()
  expect_revealed("metadata-only completion did not reveal the streamed answer")

  begin_response("switched", { "Historical response in another timeline" })
  owner.select_agent("child")
  assert(vim.wait(1000, function()
    local request = request_list[#request_list]
    if request and request.params.operation == "select_agent" then return true end
    if request and request.params.operation == "sync" then
      table.remove(request_list).callback({ patch = {} })
    end
    return false
  end, 1), "missing timeline selection")
  local selection_request = table.remove(request_list)
  assert(selection_request.params.operation == "select_agent")
  selection_request.callback({})
  finish_response()
  expect_position(window, { 12, 4 }, "switching timelines replayed historical completion")
  refresh()
  expect_position(window, { 12, 4 }, "switched history was not baselined")

  vim.api.nvim_win_close(second_window, true)
  assert(owner.close())
end, debug.traceback)
if owner and not owner.closed then owner.close() end
client.request, client.host_is_accepting = original_request, original_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1")
else print("harness_transcript_follow OK") vim.cmd("qa!") end
  end },
  { name = "update_scheduling", run = function()
vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local client = require("forge.client")
local perf = require("forge.infra.perf")
local measured_patch
perf.enabled = function(scope) return scope == "harness" end
perf.event = function(_, event, fields)
  if event == "ui.buffer.patch" then measured_patch = perf.payload(fields) end
end
local requests, notices = {}, {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
local owner, publications
local function open()
  publications = 0
  local transcript, composer = vim.api.nvim_create_buf(false, true), vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = "scheduled",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) notices[#notices + 1] = message end,
    on_update = function() publications = publications + 1 end,
  }, function(value, failure) assert(value and not failure, failure) end)
  requests[#requests].callback({ transcript = { document = owner.document, revision = 0,
    block = { { id = "body", text = { "initial" },
      metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
end
local function refresh()
  local count = #requests
  owner.sync()
  assert(vim.wait(1000, function()
    return #requests > count and requests[#requests].params.operation == "sync"
  end, 1), "refresh did not dispatch after its publication interval")
  return requests[#requests]
end
local function snapshot(revision)
  local block = {}
  for index = 1, 2048 do
    block[index] = { id = "body:" .. index, text = { "prepared " .. index },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
  end
  return { document = owner.document, revision = revision, block = block }
end
local function patch(base)
  return { document = owner.document, base = base, next = base + 1,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    text_edit = { { start_row = 0, removed_rows = 1, text = { "update " .. (base + 1) } } },
    block_edit = {}, removed_block = {}, metadata_edit = {} }
end
local success, failure = xpcall(function()
  open()
  local pending = refresh()
  local observed_revision
  vim.schedule(function()
    observed_revision = owner.transcript.revision
    assert(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false)[1] == "initial",
      "preparation exposed an unfinished publication")
  end)
  pending.callback({ snapshot = snapshot(50) })
  assert(owner.transcript.revision == 0 and owner.transcript.update_pending and owner.syncing,
    "large snapshot preparation did not yield before publication")
  local count = #requests
  owner.sync()
  assert(#requests == count, "a second sync overtook pending buffer updates")
  assert(vim.wait(3000, function() return owner.transcript.revision == 50 end, 1), "snapshot preparation stalled")
  assert(observed_revision == 0, "queued editor work waited for preparation to finish")
  assert(publications == 1, "preparation notified before its atomic publication")
  local timing = owner.transcript.update_timing
  assert(timing.slices > 1, "snapshot preparation used one unbounded callback")
  assert(timing.maximum_prepare_ms < 50, "snapshot preparation exceeded the per-slice budget")
  assert(timing.maximum_commit_ms < 250, "atomic snapshot publication exceeded the commit budget")
  assert(vim.wait(1000, function() return requests[#requests].params.revision == 50 end, 1),
    "trailing refresh did not dispatch")
  assert(requests[#requests].params.revision == 50, "follow-up sync used an intermediate revision")
  requests[#requests].callback({ patch = {} })
  assert(not owner.syncing and not owner.applying)
  owner.close()
  requests[#requests].callback({})

  open()
  refresh().callback({ snapshot = snapshot(50) })
  assert(owner.transcript.update_pending and owner.transcript.revision == 0)
  local closed = owner
  local closed_revision = owner.transcript.revision
  owner.close()
  assert(vim.wait(1000, function() return not closed.transcript.update_pending end, 1), "closed view retained an update job")
  assert(closed.transcript.revision == closed_revision, "preparation published after its view closed")
  assert(requests[#requests].params.operation == "close", "closing remained blocked behind buffer updates")
  requests[#requests].callback({})

  open()
  refresh().callback({ snapshot = snapshot(50) })
  assert(owner.transcript.update_pending and owner.transcript.revision == 0)
  local replaced_revision, previous_generation = owner.transcript.revision, client.host_get_generation
  local replaced_generation = client.host_get_generation() + 1
  client.host_get_generation = function() return replaced_generation end
  assert(vim.wait(1000, function() return not owner.transcript.update_pending end, 1), "replaced host retained an update job")
  assert(owner.transcript.revision == replaced_revision, "old host preparation reached the replacement view")
  assert(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false)[1] == "initial",
    "host replacement changed the last committed text")
  owner.close()
  client.host_get_generation = previous_generation

  open()
  refresh().callback({ patch = { patch(0) } })
  for _, name in ipairs({ "preflight_ms", "text_ms", "metadata_ms", "view_capture_ms", "view_restore_ms" }) do
    assert(type(measured_patch[name]) == "number", "patch diagnostics omitted " .. name)
  end
  local failed_request, previous_notices = refresh(), #notices
  local invalid = patch(1)
  invalid.base = 99
  failed_request.callback({ patch = { invalid, patch(2) } })
  assert(vim.wait(1000, function() return not owner.applying end, 1), "failed patch retained an update job")
  assert(owner.transcript.status == "Desynchronized" and #notices == previous_notices + 1, "failed patch was not surfaced")
  assert(requests[#requests].params.operation == "snapshot", "failed patch did not request recovery")
  requests[#requests].callback({ document = owner.document, revision = 3,
    block = { { id = "body", text = { "recovered" },
      metadata = { target = {}, decoration = {}, editable_region = {} } } } })
  assert(owner.transcript.status == "Applied" and owner.transcript.revision == 3,
    "snapshot recovery did not restore the committed document")
  owner.close()
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_update_scheduling: passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
