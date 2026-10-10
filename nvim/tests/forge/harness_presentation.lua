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
  requests[1].callback({ transcript = history })
  assert(attached == owner and owner.ready)
  assert(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)[1] == "native transcript")
  assert(vim.bo[composer].modifiable and not vim.bo[transcript].modifiable)
  owner.activate(function() error("plain transcript text was activated") end)
  assert(#requests == 1, "plain transcript text dispatched an action without a target")
  owner.sync()
  owner.sync()
  assert(#requests == 1, "refresh bypassed the publication interval")
  assert(vim.wait(500, function() return #requests == 2 end, 1))
  requests[2].callback({ snapshot = vim.NIL, patch = {} })
  assert(owner.transcript.status == "Applied", "null snapshot desynchronized incremental transcript updates")
  assert(#requests == 2, "one burst dispatched redundant refreshes")
  assert(vim.api.nvim_win_get_cursor(window)[1] == 1, "background sync moved a reader away from history")
  vim.api.nvim_buf_set_text(composer, 0, 0, 0, 5, { "first typed prompt" })
  assert(#requests == 2, "typing sent a composer request")
  owner.follow_tail()
  assert(vim.api.nvim_win_get_cursor(window)[1] == 3, "explicit agent action did not follow the tail")
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  owner.submit(function() end)
  assert(vim.api.nvim_win_get_cursor(window)[1] == 3, "explicit submission did not resume following the transcript tail")
  local submission = requests[3]
  assert(submission.method == "prompt.submit" and submission.params.text == "first typed prompt")
  assert(submission.params.submission.document == opened.document and not submission.params.composer)
  local function transition(state, token)
    return owner.receive({ kind = "prompt_submission", data = { document = opened.document,
      token = token or submission.params.submission.token, state = state } })
  end
  assert(not transition("accepted", 99), "stale acceptance cleared a draft")
  assert(transition("accepted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "")
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "first typed prompt")
  submission.callback(nil, "turn_retracted")

  owner.submit(function() end)
  submission = requests[#requests]
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "newer draft", "λ" })
  assert(transition("accepted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "acceptance overwrote newer typing")
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "newer draft", "retraction overwrote newer typing")
  submission.callback(nil, "turn_retracted")

  owner.submit(function() end)
  submission = requests[#requests]
  assert(submission.params.text == "newer draft\nλ", "submission lost multiline Unicode text")
  assert(transition("accepted"))
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "typed after acceptance" })
  assert(transition("retracted"))
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance")
  submission.callback(nil, "turn_retracted")

  local cancellation
  owner.submit(function(_, _, detail) cancellation = detail end)
  submission = requests[#requests]
  submission.callback(nil, "interrupted", { code = "turn_cancelled" })
  assert(cancellation and cancellation.code == "turn_cancelled", "composer dropped structured cancellation")
  owner.submit(function() end)
  submission = requests[#requests]
  submission.callback(nil, "admission failed")
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "typed after acceptance", "failure lost the draft")
  local request_count = #requests
  for _, source in ipairs({ { " " }, { string.rep("x", 65537) }, vim.fn["repeat"]({ "row" }, 4097) }) do
    vim.api.nvim_buf_set_lines(composer, 0, -1, false, source)
    local rejected
    owner.submit(function(result, submission_error) rejected = not result and submission_error end)
    assert(rejected and #requests == request_count, "invalid draft reached Rust")
  end
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "draft at close" })
  owner.submit(function() error("closed presentation delivered a late callback") end)
  submission = requests[#requests]
  assert(owner.close())
  assert(requests[#requests].params.operation == "close")
  assert(not transition("accepted"), "closed presentation accepted a late submission event")
  submission.callback({})
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
  initial.callback({ transcript = snapshot(initial.params.document, "retained history") })
  local function refresh()
    local count = #requests
    owner.sync()
    assert(vim.wait(500, function() return #requests > count end, 1))
    return requests[#requests]
  end
  for _ = 1, 3 do
    refresh().callback(nil, "transcript projection requires reopening: duplicate block identity")
  end
  assert(#notices == 1, "repeated refresh failure flooded notifications: " .. vim.inspect(notices))
  assert(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)[1] == "retained history")
  refresh().callback({ patch = {} })
  refresh().callback(nil, "transcript projection requires reopening: duplicate block identity")
  assert(#notices == 2, "a failure after successful recovery was suppressed")
  assert(owner.close())
end, debug.traceback)
client.request_for = original_request
client.host_accepting = original_accepting
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_presentation OK") vim.cmd("qa!") end
