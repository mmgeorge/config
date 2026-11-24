-- Unit tests for `forge.views.plan_review.document` (document.lua).
local finish_input = dofile("nvim/lua/forge/tests/support/plan_comment_input.lua")
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["document"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local generation = 1
  client.host_get_generation = function() return generation end
  client.host_is_accepting = function() return true end
  local requests = {}
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local params = request_specification.params or {}

    callback = encode_delivery(callback)
    assert(method == "session.document" or method == "session.plan.entity.rename")
    requests[#requests + 1] = { method = method, params = params, callback = callback }
  end
  local path = temp.file("plan.md", { "# Native plan", "", "Task" })
  vim.cmd("edit " .. vim.fn.fnameescape(path))
  local native_buffer = vim.api.nvim_get_current_buf()
  vim.wo.number = false
  vim.wo.relativenumber = true
  vim.wo.signcolumn = "yes"
  vim.wo.statuscolumn = "%l "
  vim.wo.conceallevel = 2
  vim.wo.concealcursor = "nc"
  vim.wo.breakindent = true
  vim.wo.breakindentopt = "shift:0"
  local owner, attached
  MiniTest.finally(function() if owner then pcall(owner.close) end end)
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
  }, function(value, error_message) assert(not error_message, error_message) attached = value end)
  local open = requests[1].params
  local initial = { path = path, version = 1, saved_source_digest = "saved",
    annotation = {}, source_row = {
      { id = "source:1", text = "# Native plan", source_line = 1, block = "plan:source", position = { row = 0, column = 0 }, target = "plan:source:1", metadata = { gutter = { { position = { row = 0, column = 0 }, priority = 100, chunk = { { text = " 1 + ", capture = "Normal" } } } } } },
      { id = "source:2", text = "", source_line = 2, block = "plan:source", position = { row = 1, column = 0 }, metadata = {} },
      { id = "source:3", text = "Task", source_line = 3, block = "plan:source", position = { row = 2, column = 0 }, metadata = {} },
    },
    snapshot = { document = open.document, revision = 0, block = { { id = "plan:source",
      text = { "# Native plan", "", "Task" }, metadata = { gutter = { { position = { row = 0, column = 0 }, priority = 100, chunk = { { text = " 1 + ", capture = "Normal" } } } }, decoration = {}, editable_region = {}, target = {
        { id = "plan:source:1", range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 13 } } },
      } } } } } }
  requests[1].callback(initial)
  assert(attached == owner and owner.saved_source_digest == "saved")
  local gutter_count = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(native_buffer, -1, 0, -1, { details = true })) do
      if mark[4].virt_text and mark[4].virt_text[1][1] == " 1 + " then gutter_count = gutter_count + 1 end
  end
  eq(gutter_count, 1, { fail_reason = "plan comment projection duplicated the snapshot gutter" })
  assert(not vim.wo.number and not vim.wo.relativenumber and vim.wo.signcolumn == "yes"
    and vim.wo.statuscolumn == "", "plan review did not hide editor line numbers")
  assert(vim.wo.conceallevel == 3 and vim.wo.concealcursor == "", "plan review did not apply Markdown concealment")
  assert(vim.wo.breakindent and vim.wo.breakindentopt == "shift:0", "plan review lost source continuation indentation")
  local alternate = buffer_support.scratch()
  vim.api.nvim_win_set_buf(0, alternate)
  vim.wo.breakindent = false
  vim.wo.breakindentopt = "shift:2"
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(vim.wo.breakindent and vim.wo.breakindentopt == "shift:0", "plan review lost indentation on re-entry")
  vim.api.nvim_win_set_buf(0, alternate)
  assert(not vim.wo.breakindent and vim.wo.breakindentopt == "shift:2", "plan review leaked indentation on release")
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(not vim.bo[native_buffer].modifiable and vim.fs.normalize(vim.api.nvim_buf_get_name(native_buffer)) == vim.fs.normalize(path))
  eq(vim.fn.readfile(path), { "# Native plan", "", "Task" }, { fail_reason = "projection overwrote its physical source" })
  local buffer_view = require("forge.buffer_view")
  local capture_viewport = buffer_view.capture_viewport
  local capture_count = 0
  buffer_view.capture_viewport = function(...)
    capture_count = capture_count + 1
    return capture_viewport(...)
  end
  owner.replica.sequence.node["plan:source"].entry.metadata.collapse = {
    { id = "declaration", closed = true, opening = { block = "plan:source", position = { row = 0, column = 0 } } },
  }
  local toggled
  owner.action("toggle_declaration", function(value, message) assert(not message, message) toggled = value end)
  eq(capture_count, 0, { fail_reason = "declaration toggle captured its viewport before the response" })
  local updated = vim.deepcopy(initial)
  updated.snapshot.revision = 1
  updated.patch = {}
  requests[#requests].callback(updated)
  buffer_view.capture_viewport = capture_viewport
  assert(toggled and capture_count == 1, "declaration toggle did not capture at publication")
  local settled = {}
  local before = #requests
  for _, name in ipairs({ "first", "second", "third" }) do
    assert(owner.submit("session.plan.entity.rename", { name = name }, function(_, problem)
      assert(not problem, problem)
      settled[#settled + 1] = name
    end))
  end
  assert(#requests == before + 1 and #owner.pending_operation == 2,
    "explicit plan edits overwrote one another while an edit was pending")
  for index, name in ipairs({ "first", "second", "third" }) do
    assert(requests[before + index].params.name == name, "plan edits changed order")
    requests[before + index].callback({})
  end
  assert(vim.deep_equal(settled, { "first", "second", "third" }) and #owner.pending_operation == 0)
  local selected
  owner.action("open", function(value) selected = value end)
  local action = requests[#requests]
  eq(action.params.input.target, "plan:source:1")
  assert(owner.is_current(action.params.input))
  local original_window = owner.current_view().window
  vim.api.nvim_exec_autocmds("FocusLost", {})
  vim.cmd("tabnew")
  assert(owner.is_current(action.params.input), "switching tabs invalidated the captured plan view")
  vim.cmd("tabclose")
  vim.cmd("vsplit")
  owner.refresh_views()
  assert(owner.current_view().id ~= action.params.input.view)
  assert(owner.is_current(action.params.input), "another plan split invalidated the captured view")
  local captured_window
  owner.action("open", function() end, original_window)
  captured_window = requests[#requests].params.input
  assert(captured_window.view == action.params.input.view, "explicit reference action used the focused split")
  assert(not owner.is_current(action.params.input), "a newer action retained the older input sequence")
  action = requests[#requests]
  vim.cmd("close")
  vim.api.nvim_exec_autocmds("FocusGained", {})
  assert(owner.is_current(action.params.input), "focus return invalidated the captured view")
  vim.api.nvim_win_set_cursor(original_window, { 1, 1 })
  assert(not owner.is_current(action.params.input), "cursor-bound action survived a cursor change")
  assert(owner.is_current(action.params.input, false), "reference snapshot depended on the native cursor column")
  vim.api.nvim_win_set_cursor(original_window, { 1, 0 })
  assert(not owner.is_current(vim.tbl_extend("force", action.params.input, { document = "other" })))
  assert(not owner.is_current(vim.tbl_extend("force", action.params.input, { view = "other" })))
  local preview_result
  owner.action("reveal_reference:usage", function(result) preview_result = result end, original_window)
  local preview = requests[#requests]
  vim.api.nvim_win_set_cursor(original_window, { 1, 1 })
  preview.callback({ jump = { block = "plan:source", position = { row = 0, column = 0 } } })
  assert(preview_result and preview_result.jump, "cursor adjustment cancelled a named reference response")
  preview_result = nil
  owner.action("reveal_reference:usage", function(result) preview_result = result end, original_window)
  preview = requests[#requests]
  require("forge.input").capture(owner.replica, owner.current_view(original_window), "open")
  preview.callback({ jump = { block = "plan:source", position = { row = 0, column = 0 } } })
  assert(preview_result and preview_result.cancelled, "superseded preview did not settle its cancellation")
  owner.action("open", function(value) selected = value end, original_window)
  action = requests[#requests]
  vim.bo[native_buffer].modifiable = true
  local previous_text = buffer_support.line(native_buffer, 1)
  vim.api.nvim_buf_set_text(native_buffer, 0, 0, 0, 0, { "new " })
  assert(not owner.is_current(action.params.input), "follow-up effect retained authority after newer physical typing")
  action.callback({ json_path = "/title" })
  eq(selected, nil, { fail_reason = "late source action replaced newer physical typing" })
  assert(owner.close(), "closing should retain the unsaved native buffer")
  assert(not owner.closed, "closing an unsaved draft disposed its owner")
  generation = 2
  assert(owner.close())
  assert(vim.api.nvim_buf_is_valid(native_buffer) and buffer_support.line(native_buffer, 1) == "new " .. previous_text)
end

T["drafts"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local pending = {}
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    local method = request_specification.method
    local params = request_specification.params or {}

    -- Comment buffers attach text correction; leaving its requests pending keeps it out of this fixture.
    if vim.startswith(method, "text.") then return end
    callback = encode_delivery(callback)
    assert(method == "session.document" or method == "session.plan.changes.request")
    pending[#pending + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
  end
  local native_buffer = buffer_support.scratch(nil, { name = vim.fn.tempname() .. ".md" })
  vim.api.nvim_win_set_buf(0, native_buffer)
  vim.bo[native_buffer].bufhidden = "hide"
  local owner
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = error,
  }, function(value, error_message) assert(not error_message, error_message) owner = value end)
  local source = {}
  for row = 1, 105 do
    source[row] = { id = "source:" .. row, target = "source:" .. row, text = "Source row " .. row, source_line = row,
      block = "plan:source", position = { row = row - 1, column = 0 }, metadata = {} }
  end
  local text = {}
  for _, row in ipairs(source) do text[#text + 1] = row.text end
  pending[1].callback({ saved_source_digest = "saved", annotation = {}, source_row = source,
    snapshot = { document = owner.document, revision = 0, block = {
      { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  vim.api.nvim_win_set_cursor(0, { 44, 0 })
  owner.action("comment", function() end)
  finish_input({ "first", "", "λ\r", "" })
  eq(#pending, 1, { fail_reason = "creating a local plan annotation sent a request" })
  local comments = require("forge.draft_comments")
  assert(comments.focus(native_buffer, comments.capture(native_buffer)[1].id))
  local body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  eq(comments.capture(native_buffer)[1].source.body, "first\n\nλ\r\n", { fail_reason = "capture changed raw draft bytes" })
  vim.cmd("write")
  assert(#pending == 2 and pending[2].params.operation == "plan_save_annotations")
  eq(pending[2].params.annotation[1].source.body, "first\n\nλ\r\n")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 4, false, { "explicit second" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  vim.cmd("write")
  eq(#pending, 2, { fail_reason = "concurrent save bypassed the explicit operation queue" })
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "newer unsaved" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[2].callback({ saved = true })
  eq(#pending, 3)
  eq(pending[3].params.annotation[1].source.body, "explicit second", { fail_reason = "queued save recaptured later unsaved typing" })
  pending[3].callback({ saved = true })
  assert(vim.bo[native_buffer].modified, "save completion cleared newer unsaved typing")
  eq(comments.capture(native_buffer)[1].source.body, "newer unsaved")
  vim.cmd("write")
  eq(#pending, 4)
  local submitted
  owner.submit("session.plan.changes.request", { comment = "revise" }, function(result, error_message)
    assert(not error_message, error_message)
    submitted = result
  end)
  eq(#pending, 4, { fail_reason = "submission bypassed an active save" })
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "typing after submit" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[4].callback({ saved = true })
  assert(#pending == 5 and pending[5].method == "session.plan.changes.request")
  eq(pending[5].params.draft_annotation[1].source.body, "newer unsaved", { fail_reason = "queued submission recaptured typing" })
  pending[5].callback({ submitted = true })
  assert(submitted.submitted and vim.bo[native_buffer].modified)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
  eq(#pending, 5, { fail_reason = "collapse sent a request" })
  local compact_row = buffer_support.find_row(native_buffer, "typing after submit")
  vim.api.nvim_win_set_cursor(0, { assert(compact_row), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
  eq(#pending, 5, { fail_reason = "focus sent a request" })
  eq(comments.capture(native_buffer)[1].source.body, "typing after submit")
  assert(owner.close() and not owner.closed, "closing collected an unsaved annotation")
  local alternate = buffer_support.scratch()
  vim.wo.statuscolumn = "%s"
  vim.api.nvim_win_set_buf(0, alternate)
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(not vim.wo.number and not vim.wo.relativenumber and vim.wo.statuscolumn == "",
    "reopening the draft restored editor line numbers")
  eq(comments.capture(native_buffer)[1].source.body, "typing after submit", { fail_reason = "hide and reopen lost the draft" })
  owner.submit("session.plan.changes.request", { comment = "reentrant completion" }, function(result, error_message)
    assert(result and not error_message, error_message)
    vim.cmd("write")
  end)
  eq(#pending, 6)
  vim.cmd("write")
  assert(comments.focus(native_buffer, comments.capture(native_buffer)[1].id))
  body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "callback capture" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[6].callback({ submitted = true })
  -- Explicit operations run in FIFO order: the save queued before the callback keeps its capture,
  -- and the callback's save follows with the newer typing.
  eq(#pending, 7, { fail_reason = "completion callback dispatched overlapping explicit operations" })
  assert(pending[7].params.annotation[1].source.body == "typing after submit",
    "queued save recaptured typing that followed it")
  pending[7].callback({ saved = true })
  assert(#pending == 8 and pending[8].params.annotation[1].source.body == "callback capture",
    "completion callback save did not follow the queued save")
  pending[8].callback({ saved = true })
  assert(not vim.bo[native_buffer].modified)
end

T["questions"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local pending = {}
  local saves = {}
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback, progress)
    local method = request_specification.method
    local params = request_specification.params or {}

    callback = encode_delivery(callback)
    if method == "session.document" and params.operation == "plan_save_annotations" then
      saves[#saves + 1] = vim.deepcopy(params)
      callback({ saved = true })
      return
    end
    pending[#pending + 1] = { method = method, params = vim.deepcopy(params), callback = callback, progress = progress }
  end
  local buffer = buffer_support.scratch(nil, { name = vim.fn.tempname() .. ".md" })
  vim.api.nvim_win_set_buf(0, buffer)
  local notices = {}
  local owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = function(message) notices[#notices + 1] = message end }, function(_, failure) assert(not failure, failure) end)
  local opened = { saved_source_digest = "saved", annotation = {}, source_row = {
    { id = "source", target = "source", text = "pub struct State;", source_line = 1,
      block = "plan:source", position = { row = 0, column = 0 }, metadata = {} },
  }, snapshot = { document = owner.document, revision = 0, block = {
    { id = "plan:source", text = { "pub struct State;" }, metadata = { target = {}, editable_region = {}, decoration = {} } },
  } } }
  pending[1].callback(opened)
  local comments = require("forge.draft_comments")
  local function replace_body(body)
    local captured = comments.capture(buffer)[1]
    assert(comments.focus(buffer, captured.id))
    local row = vim.api.nvim_win_get_cursor(0)[1] - 1
    vim.api.nvim_buf_set_lines(buffer, row, row + 1, false, { body })
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
  end
  local function leave_and_return(expected)
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    assert(buffer_support.text(buffer):find(expected, 1, true), "leaving question lost " .. expected)
    local question_row = buffer_support.find_row(buffer, "Why own this state?")
    if question_row then
      vim.api.nvim_win_set_cursor(0, { question_row, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    end
    assert(buffer_support.text(buffer):find(expected, 1, true), "returning to question lost " .. expected)
  end
  owner.action("question", function() end)
  finish_input({ "Why own this state?" })
  assert(buffer_support.text(buffer):find("Plan question", 1, true), "question used the ordinary comment heading")
  vim.cmd("write")
  eq(pending[2].method, "session.plan.question.answer", { fail_reason = "saving did not ask the LLM" })
  eq(saves[1].annotation[1].kind, "question", { fail_reason = "question kind was lost in the saved annotation" })
  eq(pending[2].params.question_ids[1], "1", { fail_reason = "answer request lost its saved question identity" })
  assert(buffer_support.text(buffer):find("Generating response", 1, true), "request omitted persistent progress")
  pending[2].progress({ question_id = "other", model = "wrong-model", effort = "low" })
  assert(not buffer_support.text(buffer):find("wrong-model", 1, true), "another question changed this pending answer")
  pending[2].progress({ question_id = "1", model = "test-ask-model", effort = "high" })
  assert(buffer_support.text(buffer):find("Generating response · test-ask-model · Thinking: high", 1, true),
    "request did not display its resolved model and thinking")
  leave_and_return("Generating response · test-ask-model · Thinking: high")
  local function response(question, answer)
    return { annotation = { { id = "1", kind = "question", source = { start_line = 1, end_line = 1, body = question },
      reply = { question_body = question, body = answer, duration_ms = 3210 } } } }
  end
  pending[2].callback(response("Why own this state?", "It gives one component ownership. Callers use its public interface."))
  assert(owner.ready and not owner.closed, "answer closed the review")
  assert(not vim.bo[buffer].modified, "answer made a saved question dirty")
  assert(buffer_support.text(buffer):find("Thought for 3 seconds", 1, true) and buffer_support.text(buffer):find("It gives one component ownership.", 1, true), "answer did not attach with its recorded duration")
  eq(comments.capture(buffer)[1].source.body, "Why own this state?", { fail_reason = "reply entered the saved question body" })
  leave_and_return("It gives one component ownership.")
  vim.cmd("write")
  assert(#saves == 2 and #pending == 2, "saving an answered question asked it again")
  replace_body("Why this interface?")
  vim.cmd("write")
  eq(pending[3].method, "session.plan.question.answer", { fail_reason = "editing an answered question did not ask again" })
  replace_body("Newer unsaved question?")
  pending[3].callback(response("Why this interface?", "This is an older answer."))
  assert(vim.bo[buffer].modified and comments.capture(buffer)[1].source.body == "Newer unsaved question?", "late answer overwrote newer typing")
  assert(not buffer_support.text(buffer):find("This is an older answer.", 1, true), "late answer appeared beneath a different question")
  vim.cmd("write")
  pending[4].callback(nil, "Provider unavailable")
  assert(#notices == 1 and notices[1] == "Provider unavailable", "provider failure was not surfaced")
  assert(not vim.bo[buffer].modified and comments.capture(buffer)[1].source.body == "Newer unsaved question?" and not owner.saving,
    "provider failure lost the saved question or blocked retry")
  assert(not buffer_support.text(buffer):find("Generating response", 1, true), "provider failure retained a generating placeholder")
  assert(buffer_support.text(buffer):find("No answer · failed: Provider unavailable · <C-s> to regenerate", 1, true),
    "provider failure left the question without an explicit failed state")
  vim.cmd("write")
  eq(pending[5].method, "session.plan.question.answer", { fail_reason = "retry did not resubmit the unanswered question" })
  pending[5].callback(response("Newer unsaved question?", "The interface establishes the ownership boundary."))
  pending[4].progress({ question_id = "1", model = "obsolete-model", effort = "low" })
  assert(not buffer_support.text(buffer):find("obsolete-model", 1, true), "retired request progress replaced the completed answer")
  assert(not vim.bo[buffer].modified and buffer_support.text(buffer):find("ownership boundary.", 1, true), "retry did not retain the answer")
  owner.action("toggle_visibility", function(_, failure) assert(not failure, failure) end)
  local refreshed = vim.deepcopy(opened)
  refreshed.patch = {}
  refreshed.annotation = response("Newer unsaved question?", "unused").annotation
  refreshed.annotation[1].reply = nil
  pending[6].callback(refreshed)
  assert(buffer_support.text(buffer):find("ownership boundary.", 1, true), "source refresh discarded the retained answer")
  local answer_row = buffer_support.find_row(buffer, "ownership boundary.")
  if answer_row then
    vim.api.nvim_win_set_cursor(0, { answer_row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    owner.sync_editability()
    assert(not vim.bo[buffer].modifiable, "view adapter made the focused answer editable")
  end
  owner.action("question", function() end)
  finish_input({ "Which getter should I use?" })
  local captured = comments.capture(buffer)
  assert(#captured == 2 and captured[2].parent_id == "1", "A on the answer did not create a linked follow-up")
  vim.cmd("write")
  assert(pending[7].method == "session.plan.question.answer" and saves[#saves].annotation[2].parent_id == "1" and pending[7].params.question_ids[1] == "2",
    "question request omitted the follow-up identity")
  local threaded = response("Newer unsaved question?", "The interface establishes the ownership boundary.")
  threaded.annotation[2] = { id = "2", kind = "question", parent_id = "1",
    source = { start_line = 1, end_line = 1, body = "Which getter should I use?" },
    reply = { question_body = "Which getter should I use?", body = "Use the state getter.", duration_ms = 1000 } }
  pending[7].callback(threaded)
  assert(not vim.bo[buffer].modified and buffer_support.text(buffer):find("Use the state getter.", 1, true), "follow-up answer was not retained")
  assert(buffer_support.text(buffer):find("Thought for 1 second", 1, true), "one-second duration was not rendered correctly")
  local follow_up_row = buffer_support.find_row(buffer, "Use the state getter.")
  if follow_up_row then
    vim.api.nvim_win_set_cursor(0, { follow_up_row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  end
  owner.action("comment", function() end)
  finish_input({ "Retain that getter." })
  eq(comments.capture(buffer)[3].parent_id, "2", { fail_reason = "C on a follow-up answer lost the conversation" })
  owner.close()
  vim.api.nvim_buf_delete(buffer, { force = true })
end

T["question_whose_answer_did_not_survive_shows_interrupted"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local pending = {}
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    pending[#pending + 1] = { params = request_specification.params or {}, callback = encode_delivery(callback) }
  end
  local buffer = buffer_support.scratch(nil, { name = vim.fn.tempname() .. ".md" })
  vim.api.nvim_win_set_buf(0, buffer)
  local owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = function() end }, function(_, failure) assert(not failure, failure) end)
  -- The review reopens after an abrupt close: the saved question has no reply, and no answer job
  -- runs in this editor.
  pending[1].callback({ saved_source_digest = "saved", source_row = {
    { id = "source", target = "source", text = "pub struct State;", source_line = 1,
      block = "plan:source", position = { row = 0, column = 0 }, metadata = {} },
  }, annotation = { { id = "1", kind = "question", source = { start_line = 1, end_line = 1, body = "Why own this state?" } } },
  snapshot = { document = owner.document, revision = 0, block = {
    { id = "plan:source", text = { "pub struct State;" }, metadata = { target = {}, editable_region = {}, decoration = {} } },
  } } })
  assert(buffer_support.text(buffer):find("No answer · interrupted · <C-s> to regenerate", 1, true),
    "a question whose answer did not survive showed no state: " .. buffer_support.text(buffer))
  owner.close()
  vim.api.nvim_buf_delete(buffer, { force = true })
end

-- A comment on a row with no source identity is refused, and the refusal changes nothing: the
-- review used to open the comment editor for the comment it had just refused, which left Insert
-- mode in the plan buffer and stacked another window on a second try.
T["refused_comment_leaves_mode_and_windows_unchanged"] = function()
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local pending = {}
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    if vim.startswith(request_specification.method, "text.") then return end
    pending[#pending + 1] = { params = request_specification.params or {}, callback = encode_delivery(callback) }
  end
  local notices = dofile("nvim/lua/forge/tests/support/notify.lua").capture({ notifications = true })
  local buffer = buffer_support.scratch(nil, { name = vim.fn.tempname() .. ".md" })
  vim.api.nvim_win_set_buf(0, buffer)
  vim.bo[buffer].bufhidden = "hide"
  local owner
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = error,
  }, function(value, failure) assert(not failure, failure) owner = value end)
  -- Row 2 is a blank separator that maps to no source line.
  local text = { "Source row 1", "", "Source row 3" }
  local source = {
    { id = "source:1", target = "source:1", text = text[1], source_line = 1, block = "plan:source",
      position = { row = 0, column = 0 }, metadata = {} },
    { id = "separator", text = text[2], block = "plan:source", position = { row = 1, column = 0 }, metadata = {} },
    { id = "source:3", target = "source:3", text = text[3], source_line = 3, block = "plan:source",
      position = { row = 2, column = 0 }, metadata = {} },
  }
  pending[1].callback({ saved_source_digest = "saved", annotation = {}, source_row = source,
    snapshot = { document = owner.document, revision = 0, block = {
      { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  local blank_row = buffer_support.find_row(buffer, "Source row 3") - 1
  vim.api.nvim_win_set_cursor(0, { blank_row, 0 })
  local windows = #vim.api.nvim_list_wins()
  local received = 0
  for _ = 1, 2 do
    local requested = owner.action("comment", function() received = received + 1 end)
    eq(requested, false, { fail_reason = "a refused comment reported that it was requested" })
  end
  vim.wait(20, nil, 5)
  eq(received, 0, { fail_reason = "a refused comment opened the comment editor" })
  eq(vim.fn.mode(), "n", { fail_reason = "a refused comment left Normal mode" })
  eq(#vim.api.nvim_list_wins(), windows, { fail_reason = "a refused comment opened a window" })
  eq(#require("forge.draft_comments").capture(buffer), 0)
  eq(notices:messages(), { "Selected rows have no source identity", "Selected rows have no source identity" })
  owner.close()
  vim.api.nvim_buf_delete(buffer, { force = true })
end

---Open a plan review whose saves answer at once, with `annotation` already saved. The plan holds
---the rows of `source_text`, or the single row `Source row 1`.
---@param annotation table[]
---@param source_text? string[]
---@return table owner, integer buffer, table[] saves
local function open_saving_review(annotation, source_text)
  source_text = source_text or { "Source row 1" }
  local source_row = {}
  for index, text in ipairs(source_text) do
    source_row[index] = { id = "source:" .. index, target = "source:" .. index, text = text, source_line = index,
      block = "plan:source", position = { row = index - 1, column = 0 }, metadata = {} }
  end
  local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
  local client = require("forge.client")
  local pending, saves = {}, {}
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    local params = request_specification.params or {}
    callback = encode_delivery(callback)
    if request_specification.method == "session.document" and params.operation == "plan_save_annotations" then
      saves[#saves + 1] = vim.deepcopy(params)
      callback({ saved = true })
      return
    end
    pending[#pending + 1] = { params = params, callback = callback }
  end
  local buffer = buffer_support.scratch(nil, { name = vim.fn.tempname() .. ".md" })
  vim.api.nvim_win_set_buf(0, buffer)
  local owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" }, notice = error,
  }, function(_, failure) assert(not failure, failure) end)
  pending[1].callback({ saved_source_digest = "saved", annotation = annotation, source_row = source_row,
    snapshot = { document = owner.document, revision = 0, block = {
      { id = "plan:source", text = source_text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  MiniTest.finally(function()
    pcall(owner.close, true)
    pcall(vim.api.nvim_buf_delete, buffer, { force = true })
  end)
  return owner, buffer, saves
end

-- Saving a comment that holds no text saves nothing, as the queue editor does with an emptied
-- entry: a new empty comment is dropped, and a saved comment edited to empty is removed. The
-- review used to save the empty comment, which then reopened with the review as an empty box.
T["saving_an_empty_comment_saves_nothing"] = function()
  local comments = require("forge.draft_comments")
  local owner, buffer, saves = open_saving_review({})
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  assert(owner.action("comment", function() end), "the comment was refused")
  finish_input({ "" })
  vim.cmd("write")
  eq(saves[#saves].annotation, {}, { fail_reason = "an empty new comment was saved" })
  eq(comments.capture(buffer), {}, { fail_reason = "the empty comment stayed in the review" })

  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  owner.action("comment", function() end)
  finish_input({ "Keep this row." })
  vim.cmd("write")
  eq(#saves[#saves].annotation, 1, { fail_reason = "a comment with text was not saved" })
  local saved = comments.capture(buffer)[1]
  assert(comments.focus(buffer, saved.id))
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_lines(buffer, row, row + 1, false, { "   " })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
  vim.cmd("write")
  eq(saves[#saves].annotation, {}, { fail_reason = "a comment edited to empty was saved" })
  eq(comments.capture(buffer), {}, { fail_reason = "a comment edited to empty stayed in the review" })
  assert(not vim.bo[buffer].modified, "removing the emptied comment left the review modified")
end

T["every card row opens a follow-up at the footer"] = function()
  local comments = require("forge.draft_comments")
  local owner, buffer = open_saving_review({
    { id = "1", kind = "question", source = { start_line = 1, end_line = 1, body = "Why this row?" },
      reply = { question_body = "Why this row?", body = "This is the answer.", duration_ms = 1000 } },
  })
  local parent = vim.api.nvim_get_current_win()
  local range = owner.comment_state.range_list[1]
  local lines = buffer_support.lines(buffer)
  local width = vim.fn.strdisplaywidth(lines[range.last_row + 1]) - 4
  for row = range.first_row, range.last_row do
    vim.api.nvim_win_set_cursor(parent, { row + 1, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    eq(buffer_support.lines(buffer), lines)
    assert(owner.action(row % 2 == 0 and "comment" or "question", function() end))
    local window = vim.api.nvim_get_current_win()
    local config = vim.api.nvim_win_get_config(window)
    eq({ config.win, config.bufpos, config.width }, { parent, { range.last_row, 0 }, width })
    eq(vim.api.nvim_win_get_cursor(parent)[1], range.last_row + 1)
    vim.cmd("redraw")
    local position = vim.fn.screenpos(parent, range.last_row + 1, 3)
    eq(vim.api.nvim_win_get_position(window), { position.row, position.col - 1 })
    finish_input({ "Discard this follow-up" }, "<Esc>")
    eq(#comments.capture(buffer), 1)
  end
  assert(owner.action("comment", function() end))
  finish_input({ "First line", "Second line" }, "<C-s>")
  local captured = comments.capture(buffer)
  eq({ captured[1].source.body, captured[2].parent_id, captured[2].source.body },
    { "Why this row?", "1", "First line\nSecond line" })
  eq(vim.bo[buffer].modified, false)
  -- Replying to the original message starts at the bottom of the whole thread.
  vim.api.nvim_win_set_cursor(parent, { range.first_row + 1, 0 })
  assert(owner.action("comment", function() end))
  local window = vim.api.nvim_get_current_win()
  local thread_footer = owner.comment_state.range_list[2].last_row
  eq(vim.api.nvim_win_get_config(window).bufpos, { thread_footer, 0 })
  local long_answer = vim.tbl_map(tostring, vim.fn.range(1, vim.api.nvim_win_get_height(parent) * 2))
  comments.update(buffer, { { id = "1", replies = { { heading = "Answer", body_lines = long_answer } } } })
  thread_footer = owner.comment_state.range_list[2].last_row
  eq(vim.api.nvim_win_get_config(window).bufpos, { thread_footer, 0 })
  vim.cmd("redraw")
  local position = vim.fn.screenpos(parent, thread_footer + 1, 3)
  assert(position.row > 0, "the expanded answer pushed the popup anchor off screen")
  eq(vim.api.nvim_win_get_position(window), { position.row, position.col - 1 })
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { "Another follow-up" })
  vim.cmd("write")
  eq(comments.capture(buffer)[3].parent_id, "1")
end

-- Every question-and-answer block draws in the same collapsed box after a save, including the
-- block whose follow-up comment was just saved. The save collapsed that block but placed the
-- cursor on the collapsed block's first row, so the editor's next CursorMoved focused it again
-- and redrew it as the plain editor while the other block kept its box.
T["saved_follow_up_draws_its_block_like_the_others"] = function()
  local owner, buffer = open_saving_review({
    { id = "1", kind = "question", source = { start_line = 1, end_line = 1, body = "Why prorate?" },
      reply = { question_body = "Why prorate?", body = "Prorating keeps charged amounts.", duration_ms = 8000 } },
    { id = "2", kind = "question", source = { start_line = 3, end_line = 3, body = "Why sum duplicates?" },
      reply = { question_body = "Why sum duplicates?", body = "Summing bounds returned units.", duration_ms = 8000 } },
  }, { "Source row 1", "Source row 2", "Source row 3" })
  ---@return string[] block_headings the first row of each question block, in document order
  local function block_headings()
    local headings = {}
    for _, line in ipairs(buffer_support.lines(buffer)) do
      if line:find("Plan question", 1, true) then headings[#headings + 1] = line end
    end
    return headings
  end
  local function boxed(heading) return heading:find("╭─ Plan question", 1, true) ~= nil end
  for index, heading in ipairs(block_headings()) do
    assert(boxed(heading), ("question block %d opened without its box: %s"):format(index, heading))
  end
  for _, answer in ipairs({ "Prorating keeps charged amounts.", "Summing bounds returned units." }) do
    vim.api.nvim_win_set_cursor(0, { buffer_support.find_row(buffer, answer), 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    assert(owner.action("comment", function() end), "the follow-up comment was refused")
    finish_input({ "Follow up on: " .. answer }, "<C-s>")
    -- The editor reports the cursor the save placed; a block it rests on may not reopen.
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
    local headings = block_headings()
    eq(#headings, 2, { fail_reason = "a question block disappeared after the save" })
    eq(vim.tbl_map(boxed, headings), { true, true },
      { fail_reason = "the blocks drew in different styles after saving a follow-up to " .. answer
        .. ":\n" .. table.concat(headings, "\n") })
    assert(vim.iter(owner.comment_state.range_list):all(function(range) return range.compact end),
      "a saved block kept its editor decorations")
  end
end

return T
