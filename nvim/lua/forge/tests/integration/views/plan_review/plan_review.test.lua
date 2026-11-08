local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["plan_optimistic_edit"] = function()
  local preview = require("forge.views.plan_review.test_preview")
  local source = {
    { block = "unrelated", text = "src/first.rs" },
    { block = "plan:section:tests", text = " Tests · 2 new · 1 modified · 1 removed · 1 reused ", test_summary = { new = 2, modified = 1, removed = 1, reused = 1 } },
    { block = "plan:tests:file:first", text = "Modified src/first.rs", test_file = "src/first.rs" },
  }
  local selected = {}
  local function test(file, name, change)
    local selection = { file = file, name = name }
    source[#source + 1] = { block = file .. name, text = name, test = selection, test_change = change }
    source[#source + 1] = { block = file .. name .. ":description", text = "  Description", test = selection, test_change = change }
    return selection
  end
  local function counts(row, summary)
    row.test_summary = summary
    row.text = row.text .. " +" .. summary.new .. " -" .. summary.removed
    local column = assert(row.text:find("+", 1, true)) - 1
    row.metadata = { decoration = {
      { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = column } }, capture = "ForgeStatusPath", priority = 100 },
      { range = { start = { row = 0, column = column }, ["end"] = { row = 0, column = column + 2 } }, capture = "ForgeAddRange", priority = 100 },
    } }
  end
  counts(source[3], { new = 1, modified = 1, removed = 0, reused = 0 })
  selected[#selected + 1] = test("src/first.rs", "first", "new")
  selected[#selected + 1] = test("src/first.rs", "second", "modified")
  source[#source + 1] = { block = "plan:tests:file:second", text = "New tests/second.rs", test_file = "tests/second.rs" }
  counts(source[#source], { new = 1, modified = 0, removed = 1, reused = 1 })
  local retained = test("tests/second.rs", "keep", "new")
  selected[#selected + 1] = test("tests/second.rs", "third", "removed")
  selected[#selected + 1] = test("tests/second.rs", "fourth", "reused")
  for index, row in ipairs(source) do row.source_line = index row.position = { row = 0, column = 0 } end
  local baseline = vim.deepcopy(source)
  local projected = preview.project(source, selected)
  assert(vim.deep_equal(source, baseline), "preview mutated the canonical source")
  assert(projected[2].text == " Tests · 1 new ", projected[2].text)
  local heading = projected[2]
  for _, span in ipairs(heading.metadata.decoration) do
    local text = heading.text:sub(span.range.start.column + 1, span.range["end"].column)
    if span.capture == "RenderMarkdownH1" then assert(text == " Tests ", "counts retained the section background") end
    if text == "1 new" then assert(span.capture == "ForgeAddRange", "new count lost its foreground color") end
  end
  assert(projected[3].hidden and not projected[1].hidden, "empty file removal hid unrelated text")
  assert(projected[8].text == "New tests/second.rs +1 -0", "file counts included descriptions or modified tests")
  assert(projected[8].test_summary.removed == 0, "file summary did not update")
  for index, row in ipairs(projected) do
    assert(row.source_line == source[index].source_line and row.block == source[index].block, "preview changed source identity")
    if row.test and row.test.name ~= "keep" then assert(row.hidden, "deleted test description remained visible") end
  end
  selected[#selected + 1] = retained
  projected = preview.project(source, selected)
  assert(projected[2].text == " Tests · None planned ", projected[2].text)
  assert(projected[2].metadata.decoration[3].capture == "Comment", "empty inventory must use neutral text")
  assert(projected[8].hidden, "last test deletion retained its empty file heading")

  local folded = vim.deepcopy(source)
  folded[2].test_summary.reused = 8
  local with_folded = preview.project(folded, selected)
  assert(with_folded[2].text == " Tests · 7 reused ", "deletion lost tests inside collapsed files")

  local rename = require("forge.views.plan_review.entity_rename")
  local popup = require("forge.infra.popup_window")
  local native = buffer.scratch({ "Original", "use Original" })
  vim.api.nvim_win_set_buf(0, native)
  local window = vim.api.nvim_get_current_win()
  vim.wo[window].conceallevel, vim.wo[window].concealcursor = 1, "n"
  local current = true
  local review = { buf = native, win = window, owner = {
    document = "rename-preview", is_current = function() return current end,
    replica = { physical_row = function(row) return row end,
      sequence = { position = function(_, block) return 0, block == "first" and 0 or 1 end } },
  } }
  local selection = { symbol = "original", name = "Original", expected_version = 1,
    preview = { { block = "first", column = 0, length = 8 }, { block = "second", column = 4, length = 8 } } }
  local input, answer, finish, committed
  popup.incremental_input = function(options, callback) input, answer = options, callback end
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  local function marks() return vim.api.nvim_buf_get_extmarks(native, namespace, 0, -1, { details = true }) end
  local function commit(name, settle) committed, finish = name, settle end
  rename.proposed(review, selection, {}, commit)
  input.on_change("Renamed ")
  answer("Renamed ")
  assert(committed == "Renamed" and #marks() == 2, "confirmation cleared the pending semantic preview")
  assert(marks()[1][4].virt_text[1][1] == "Renamed", "pending preview did not normalize the submitted name")
  assert(vim.wo[window].conceallevel == 2 and vim.wo[window].concealcursor == "nvic")
  assert(buffer.line(native, 1) == "Original", "preview changed canonical bytes")
  finish()
  finish()
  assert(#marks() == 0 and vim.wo[window].conceallevel == 1 and vim.wo[window].concealcursor == "n",
    "rollback did not restore display options")

  committed = nil
  rename.proposed(review, selection, {}, commit)
  input.on_change("Cancelled")
  answer(nil)
  assert(not committed and #marks() == 0, "cancellation submitted or retained a preview")
  rename.proposed(review, selection, {}, commit)
  input.on_change("Original ")
  answer("Original ")
  assert(not committed and #marks() == 0, "unchanged normalized name was submitted")

  local notices = notify.capture()
  rename.proposed(review, selection, {}, commit)
  input.on_change("Stale")
  current = false
  answer("Stale")
  assert(not committed and #marks() == 0 and notices[#notices].message:find("Plan changed", 1, true))
  current = true
  rename.proposed(review, selection, {}, function() error("submission failed") end)
  input.on_change("Broken")
  answer("Broken")
  assert(#marks() == 0 and notices[#notices].message:find("submission failed", 1, true), "submission error stranded preview")
  rename.proposed(review, selection, {}, commit)
  input.on_change("Pending")
  answer("Pending")
  assert(#marks() == 2)
  vim.api.nvim_buf_delete(native, { force = true })
  finish()
  assert(vim.wo[window].conceallevel ~= 2 and vim.wo[window].concealcursor ~= "nvic", "buffer wipe stranded conceal options")
end

T["recovery"] = function()
  local client = require("forge.client")
  local generation = 1
  client.host_get_generation = function() return generation end
  client.host_is_accepting = function() return true end
  local function hold() end
  local pending = mock_client.respond({ ["session.document"] = hold, ["session.plan.changes.request"] = hold },
    { wrap_callback = encode_delivery, copy_params = true })
  local native_buffer = buffer.scratch(nil, { name = temp.file("review.md") })
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

  vim.cmd("stopinsert")
  local comments = require("forge.draft_comments")
  local body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "draft", "λ\r", "" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  local plan = { id = "plan", review_digest = "canonical", working_path = vim.api.nvim_buf_get_name(native_buffer) }
  local harness = require("forge.session").harness
  harness.session = { id = "replacement-session" }
  local previous = { plan = plan, owner = owner, buf = native_buffer, win = vim.api.nvim_get_current_win(),
    tab = vim.api.nvim_get_current_tabpage() }
  harness.plan_review = previous
  local before = buffer.lines(native_buffer)
  owner.annotation_saving, owner.saving = true, true
  vim.cmd("write")
  assert(owner.pending_operation[1] and owner.pending_operation[1].capture[1].source.body == "draft\nλ\r\n")
  generation = 2
  require("forge.views.plan_review.native_controller").open(plan)
  assert(#pending == 2)
  assert(vim.deep_equal(buffer.lines(native_buffer), before),
    "recovery replaced the retained draft before host validation")
  assert(vim.bo[native_buffer].modified, "recovery cleared dirty state before host validation")
  vim.bo[native_buffer].modifiable = true
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 3, false, { "newer draft", "λ\r", "" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  local replacement = harness.plan_review
  local response = { saved_source_digest = "saved", annotation = {}, source_row = source,
    public_only = true, snapshot = { document = pending[2].spec.params.document, revision = 0, block = {
      { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } }
  pending[2].callback(response)
  assert(replacement.owner.ready)
  assert(comments.capture(native_buffer)[1].source.body == "newer draft\nλ\r\n",
    "recovery restored an older capture instead of current typing")
  assert(vim.bo[native_buffer].modified, "recovered draft became its own saved baseline")
  assert(replacement.owner.document ~= owner.document)
  assert(#pending == 3 and pending[3].spec.params.operation == "plan_save_annotations", vim.inspect(pending))
  assert(pending[3].spec.params.annotation[1].source.body == "draft\nλ\r\n",
    "recovery recaptured newer typing for the pending save")
  pending[3].callback({})
  assert(comments.capture(native_buffer)[1].source.body == "newer draft\nλ\r\n")
  assert(vim.bo[native_buffer].modified)

  local retained_text = buffer.lines(native_buffer)
  local notices = notify.capture()
  generation = 3
  require("forge.views.plan_review.native_controller").open(plan)
  assert(#pending == 4)
  pending[4].callback(nil, "physical source conflict")
  assert(harness.plan_review == replacement, "failed rebind discarded the retained owner")
  assert(vim.deep_equal(buffer.lines(native_buffer), retained_text), "failed rebind changed draft text")
  assert(vim.bo[native_buffer].modified)
  assert(notices[#notices] and notices[#notices].message:find("physical source conflict", 1, true))
  generation = 2
  replacement.owner.annotation_saving, replacement.owner.saving = true, true
  local submitted
  replacement.owner.submit("session.plan.changes.request", { comment = "Revision requested" }, function(result, failure)
    assert(result and not failure, failure)
    submitted = true
  end)
  assert(replacement.owner.pending_operation[1])
  local frozen = vim.deepcopy(replacement.owner.pending_operation[1].params)
  generation = 4
  require("forge.views.plan_review.native_controller").open(plan)
  assert(#pending == 5)
  local submission_owner = harness.plan_review.owner
  local reopened = vim.deepcopy(response)
  reopened.snapshot.document = pending[5].spec.params.document
  pending[5].callback(reopened)
  assert(#pending == 6 and pending[6].spec.method == "session.plan.changes.request")
  local recovered_submission = pending[6].spec.params
  assert(recovered_submission.review.document == submission_owner.document)
  assert(recovered_submission.review.view == submission_owner.view.id)
  assert(recovered_submission.review.revision == submission_owner.replica.revision)
  assert(vim.deep_equal(recovered_submission.draft_annotation, frozen.draft_annotation))
  assert(recovered_submission.comment == frozen.comment)
  pending[6].callback({})
  assert(submitted and not submission_owner.submission_pending)
  assert(comments.focus(native_buffer, comments.capture(native_buffer)[1].id))
  local closing_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  vim.api.nvim_buf_set_lines(native_buffer, closing_row, closing_row + 1, false, { "Save before this buffer closes" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  vim.cmd("write")
  assert(pending[7].spec.params.operation == "plan_save_annotations" and submission_owner.annotation_saving)
  vim.api.nvim_buf_delete(native_buffer, { force = true })
  pending[7].callback({ saved = true })
  assert(not submission_owner.annotation_saving and not submission_owner.saving,
    "closed review retained a completed annotation save")
  assert(submission_owner.close(), "invalid review buffer prevented owner cleanup")
end

T["plan_test_delete"] = function()
  local client = require("forge.client")
  client.host_get_generation = function() return 1 end
  client.host_is_accepting = function() return true end
  local state = require("forge.session").harness
  state.session = { id = "tests-session" }
  state.active_plan = { id = "unrelated-current-plan" }
  state.transcript_win = vim.api.nvim_get_current_win()
  local notices = notify.capture()
  local path = temp.file("plan.md", { "canonical fixture" })
  local version, pending = 1, nil
  local heading = "Tests"
  local tests = {
    { file = "src/game.rs", name = "first" }, { file = "src/game.rs", name = "second" },
    { file = "tests/game.rs", name = "first" }, { file = "tests/game.rs", name = "fourth" },
    { file = "tests/game.rs", name = "keep" },
  }
  package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end }
  local opened, closed = 0, 0
  local function presentation(document)
    local source, blocks = {}, {}
    local function row(text, test)
      local id = test and test.file .. ":" .. test.name .. ":" .. text
        or text:find(".rs", 1, true) and "plan:tests:file:" .. text or text
      local metadata = { decoration = {}, editable_region = {}, target = {
        { id = id, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } },
      } }
      source[#source + 1] = { id = id, text = text, test = test, test_file = not test and text:find(".rs", 1, true) and text or nil, source_line = #source + 1,
        block = id, target = id, position = { row = 0, column = 0 }, metadata = metadata }
      blocks[#blocks + 1] = { id = id, text = { text }, metadata = metadata }
    end
    row(heading)
    local file
    for _, test in ipairs(tests) do
      if file ~= test.file then file = test.file row(file) end
      row("  " .. test.name, test)
      row("    Expected result", test)
    end
    return { version = version, saved_source_digest = "saved:" .. version, annotation = {}, source_row = source,
      snapshot = { document = document, revision = version - 1, block = blocks } }
  end
  local function hold(request_specification, callback)
    pending = { method = request_specification.method, params = request_specification.params or {},
      callback = encode_delivery(callback) }
  end
  local function document(request_specification, callback)
    local params = request_specification.params or {}
    callback = encode_delivery(callback)
    if params.operation == "plan_action" then
      callback({ rename = { symbol = "tests", name = heading, expected_version = version,
        preview = { { block = heading, column = 0, length = #heading } } } }) return
    end
    if params.operation == "plan_save_annotations" then
      assert(params.saved_source_digest == "saved:" .. version, "queued save used a stale source digest")
      assert(params.annotation[1].source.start_line == 1 and params.annotation[1].source.body == "Keep this new feedback",
        "queued save lost or misplaced the newer draft")
    end
    if params.operation == "plan_close" then closed = closed + 1 end
    if params.operation ~= "plan_open" then callback({}) return end
    opened = opened + 1
    callback(presentation(params.document))
  end
  mock_client.respond({ ["session.plan.test.delete"] = hold, ["session.plan.entity.rename"] = hold,
    ["session.document"] = document })
  local native = require("forge.views.plan_review.native_controller")
  local function open()
    native.open({ id = "plan", state = "awaiting_review", document_version = version,
      working_path = path, review_digest = "digest:" .. version })
    assert(state.plan_review, table.concat(notices:messages(), "\n"))
    return state.plan_review
  end
  local function select(review)
    vim.api.nvim_win_set_cursor(review.win, { 3, 0 })
    vim.cmd("normal! V8j")
    assert(vim.fn.mode() == "V")
    keymap.invoke("j", { mode = "x" })
    assert(vim.bo.filetype == "ForgeConfirm", "selection did not open confirmation")
    local text = buffer.text(0)
    assert(text:find("Delete 4 planned tests?", 1, true), text)
    assert(pending == nil, "deletion started before confirmation")
  end
  local review = open()
  assert(vim.fn.maparg("j", "n", false, true).buffer ~= 1, "normal navigation was replaced")
  select(review)
  keymap.invoke("n")
  assert(pending == nil and #tests == 5, "cancel mutated the inventory")
  select(review)
  review.owner.view.sequence = review.owner.view.sequence + 1
  keymap.invoke("y")
  assert(pending == nil and notices[#notices].message:find("Plan changed", 1, true), "stale confirmation was accepted")
  select(review)
  keymap.invoke("y")
  assert(pending and #pending.params.tests == 4)
  assert(pending.params.expected_version == 1 and pending.params.draft_source_digest == "saved:1")
  assert(pending.params.tests[1].file == "src/game.rs" and pending.params.tests[3].file == "tests/game.rs",
    "same-name tests from separate files were conflated")
  local original_source = review.owner.source
  local optimistic = buffer.text(review.buf)
  assert(not optimistic:find("fourth", 1, true) and not optimistic:find("src/game.rs", 1, true)
    and optimistic:find("keep", 1, true), "test deletion waited for the provider")
  assert(review.owner.version == 1 and review.owner.source == original_source, "preview changed the canonical version")
  local comments = require("forge.draft_comments")
  comments.add(review.buf, { id = "rollback-feedback", source_line = 1, body = "Feedback typed while deleting" }, false)
  vim.cmd("stopinsert")
  pending.callback(nil, "test deletion failed")
  pending = nil
  assert(not require("forge.views.harness.activity").busy(state) and review.owner.attached(), "failed deletion stranded the review")
  assert(comments.capture(review.buf)[1].source.body == "Feedback typed while deleting", "rollback lost newer feedback")
  assert(vim.api.nvim_get_current_line() == "Feedback typed while deleting", "rollback moved the current comment cursor")
  comments.delete_at_cursor(review.buf)
  assert(vim.deep_equal(buffer.lines(review.buf),
    vim.tbl_map(function(row) return row.text end, review.owner.source)), "failed deletion did not roll back")
  select(review)
  keymap.invoke("y")
  tests = { tests[5] }
  version = 2
  local owner, window, tab = review.owner, review.win, review.tab
  local writes = {}
  vim.api.nvim_buf_attach(review.buf, false, { on_lines = function(_, _, _, first, last, next_last)
    writes[#writes + 1] = { first, last, next_last }
  end })
  vim.api.nvim_win_set_cursor(window, { 3, 2 })
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:2" }, review = presentation(owner.document) })
  assert(owner.version == 2, table.concat(notices:messages(), "\n"))
  assert(not require("forge.views.harness.activity").busy(state))
  assert(state.plan_review == review and review.owner == owner and owner.attached(), "deletion replaced the document owner")
  assert(review.win == window and review.tab == tab and vim.api.nvim_get_current_win() == window, "deletion reopened the view")
  assert(opened == 1 and closed == 0, "deletion reopened the native document")
  assert(#writes == 0, "success rewrote an already correct optimistic deletion")
  assert(vim.api.nvim_get_current_line():find("keep", 1, true), "cursor must follow the retained test")
  local rendered = buffer.text(state.plan_review.buf)
  assert(rendered:find("keep", 1, true) and not rendered:find("fourth", 1, true), rendered)

  pending = nil
  local popup = require("forge.infra.popup_window")
  popup.incremental_input = function(options, callback)
    options.on_change("ReviewedTests")
    callback("ReviewedTests")
  end
  review.command_set.action_by_id.rename_entity.run({})
  assert(pending and pending.method == "session.plan.entity.rename" and pending.params.name == "ReviewedTests")
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  local function rename_marks() return vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, { details = true }) end
  assert(#rename_marks() == 1 and rename_marks()[1][4].virt_text[1][1] == "ReviewedTests",
    "confirmed rename lost its optimistic preview")
  assert(buffer.line(review.buf, 1) == "Tests", "preview mutated canonical rename text")
  pending.callback(nil, "rename failed")
  assert(#rename_marks() == 0 and not require("forge.views.harness.activity").busy(state), "failed rename did not roll back")
  assert(buffer.line(review.buf, 1) == "Tests")
  popup.incremental_input = function(options, callback) options.on_change("ReviewedTests") callback("ReviewedTests") end
  review.command_set.action_by_id.rename_entity.run({})
  assert(#rename_marks() == 1, "retry did not preview rename")
  heading, version = "ReviewedTests", 3
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:3" }, review = presentation(owner.document) })
  assert(state.plan_review == review and review.owner == owner and owner.version == 3,
    "rename replaced the review instead of updating it")
  assert(buffer.line(review.buf, 1) == heading)
  assert(#rename_marks() == 0, "successful rename retained its overlay")
  assert(opened == 1 and closed == 0, "rename reopened the native document")
  assert(state.active_plan.id == "unrelated-current-plan", "editing a review changed the current plan")

  pending = nil
  vim.api.nvim_win_set_cursor(window, { 3, 0 })
  vim.cmd("normal! V")
  keymap.invoke("j", { mode = "x" })
  keymap.invoke("y")
  assert(pending and pending.params.expected_version == 3 and pending.params.draft_source_digest == "saved:3",
    "the next deletion used the previous plan revision")
  assert(not buffer.text(review.buf):find("tests/game.rs", 1, true),
    "last test deletion retained its file heading while waiting")
  comments.add(review.buf, { id = "late", source_line = 1, body = "Keep this new feedback" }, false)
  vim.cmd("stopinsert")
  vim.cmd("write")
  assert(owner.pending_operation, "the save should queue behind deletion")
  local focused_text = vim.api.nvim_get_current_line()
  assert(focused_text == "Keep this new feedback", "comment fixture did not focus its editable body")
  tests, version = {}, 4
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:4" }, review = presentation(owner.document) })
  assert(owner.version == 4 and owner.attached(), table.concat(notices:messages(), "\n"))
  assert(comments.capture(review.buf)[1].source.body == "Keep this new feedback", "deletion lost newer typing")
  assert(vim.api.nvim_get_current_line() == focused_text, "deletion moved the cursor out of the current comment")
  assert(not vim.bo[review.buf].modified, "the queued save did not settle")
  assert(opened == 1 and closed == 0, "subsequent deletion reopened the native document")
end

return T
