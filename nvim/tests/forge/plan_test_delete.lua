local encode_delivery = dofile("nvim/tests/forge/support/plan_delivery.lua")
vim.loader.enable(false)
local client = require("forge.client")
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
local state = require("forge.session").harness
state.session = { id = "tests-session" }
state.active_plan = { id = "unrelated-current-plan" }
state.transcript_win = vim.api.nvim_get_current_win()
local notices = {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "canonical fixture" }, path)
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
client.request_for = function(_, method, params, callback)
  callback = encode_delivery(callback)
  if method == "plan.tests.delete" or method == "plan.entity.rename" then
    pending = { method = method, params = params, callback = callback } return
  end
  assert(method == "harness.document")
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
local native = require("forge.views.plan_review.native_controller")
local function open()
  native.open({ id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:" .. version })
  assert(state.plan_review, table.concat(notices, "\n"))
  return state.plan_review
end
local function select(review)
  vim.api.nvim_win_set_cursor(review.win, { 3, 0 })
  vim.cmd("normal! V8j")
  assert(vim.fn.mode() == "V")
  local mapping = vim.fn.maparg("j", "x", false, true)
  assert(type(mapping.callback) == "function", "visual j was not bound")
  mapping.callback()
  assert(vim.bo.filetype == "ForgeConfirm", "selection did not open confirmation")
  local text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(text:find("Delete 4 planned tests?", 1, true), text)
  assert(pending == nil, "deletion started before confirmation")
end
local ok, failure = xpcall(function()
  local review = open()
  assert(vim.fn.maparg("j", "n", false, true).buffer ~= 1, "normal navigation was replaced")
  select(review)
  vim.fn.maparg("n", "n", false, true).callback()
  assert(pending == nil and #tests == 5, "cancel mutated the inventory")
  select(review)
  review.owner.view.sequence = review.owner.view.sequence + 1
  vim.fn.maparg("y", "n", false, true).callback()
  assert(pending == nil and notices[#notices]:find("Plan changed", 1, true), "stale confirmation was accepted")
  select(review)
  vim.fn.maparg("y", "n", false, true).callback()
  assert(pending and #pending.params.tests == 4)
  assert(pending.params.expected_version == 1 and pending.params.draft_source_digest == "saved:1")
  assert(pending.params.tests[1].file == "src/game.rs" and pending.params.tests[3].file == "tests/game.rs",
    "same-name tests from separate files were conflated")
  local original_source = review.owner.source
  local optimistic = table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n")
  assert(not optimistic:find("fourth", 1, true) and not optimistic:find("src/game.rs", 1, true)
    and optimistic:find("keep", 1, true), "test deletion waited for the backend")
  assert(review.owner.version == 1 and review.owner.source == original_source, "preview changed the canonical version")
  local comments = require("forge.draft_comments")
  comments.add(review.buf, { id = "rollback-feedback", source_line = 1, body = "Feedback typed while deleting" }, false)
  vim.cmd("stopinsert")
  pending.callback(nil, "test deletion failed")
  pending = nil
  assert(not state.busy and review.owner.attached(), "failed deletion stranded the review")
  assert(comments.capture(review.buf)[1].source.body == "Feedback typed while deleting", "rollback lost newer feedback")
  assert(vim.api.nvim_get_current_line() == "Feedback typed while deleting", "rollback moved the current comment cursor")
  comments.delete_at_cursor(review.buf)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false),
    vim.tbl_map(function(row) return row.text end, review.owner.source)), "failed deletion did not roll back")
  select(review)
  vim.fn.maparg("y", "n", false, true).callback()
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
  assert(owner.version == 2, table.concat(notices, "\n"))
  assert(not state.busy)
  assert(state.plan_review == review and review.owner == owner and owner.attached(), "deletion replaced the document owner")
  assert(review.win == window and review.tab == tab and vim.api.nvim_get_current_win() == window, "deletion reopened the view")
  assert(opened == 1 and closed == 0, "deletion reopened the native document")
  assert(#writes == 0, "success rewrote an already correct optimistic deletion")
  assert(vim.api.nvim_get_current_line():find("keep", 1, true), "cursor must follow the retained test")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(state.plan_review.buf, 0, -1, false), "\n")
  assert(rendered:find("keep", 1, true) and not rendered:find("fourth", 1, true), rendered)

  pending = nil
  local popup = require("forge.infra.popup_window")
  local previous_input = popup.incremental_input
  popup.incremental_input = function(options, callback)
    options.on_change("ReviewedTests")
    callback("ReviewedTests")
  end
  review.command_set.action_by_id.rename_entity.run({})
  popup.incremental_input = previous_input
  assert(pending and pending.method == "plan.entity.rename" and pending.params.name == "ReviewedTests")
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  local function rename_marks() return vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, { details = true }) end
  assert(#rename_marks() == 1 and rename_marks()[1][4].virt_text[1][1] == "ReviewedTests",
    "confirmed rename lost its optimistic preview")
  assert(vim.api.nvim_buf_get_lines(review.buf, 0, 1, false)[1] == "Tests", "preview mutated canonical rename text")
  pending.callback(nil, "rename failed")
  assert(#rename_marks() == 0 and not state.busy, "failed rename did not roll back")
  assert(vim.api.nvim_buf_get_lines(review.buf, 0, 1, false)[1] == "Tests")
  popup.incremental_input = function(options, callback) options.on_change("ReviewedTests") callback("ReviewedTests") end
  review.command_set.action_by_id.rename_entity.run({})
  popup.incremental_input = previous_input
  assert(#rename_marks() == 1, "retry did not preview rename")
  heading, version = "ReviewedTests", 3
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:3" }, review = presentation(owner.document) })
  assert(state.plan_review == review and review.owner == owner and owner.version == 3,
    "rename replaced the review instead of updating it")
  assert(vim.api.nvim_buf_get_lines(review.buf, 0, 1, false)[1] == heading)
  assert(#rename_marks() == 0, "successful rename retained its overlay")
  assert(opened == 1 and closed == 0, "rename reopened the native document")
  assert(state.active_plan.id == "unrelated-current-plan", "editing a review changed the current plan")

  pending = nil
  vim.api.nvim_win_set_cursor(window, { 3, 0 })
  vim.cmd("normal! V")
  vim.fn.maparg("j", "x", false, true).callback()
  vim.fn.maparg("y", "n", false, true).callback()
  assert(pending and pending.params.expected_version == 3 and pending.params.draft_source_digest == "saved:3",
    "the next deletion used the previous plan revision")
  assert(not table.concat(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false), "\n"):find("tests/game.rs", 1, true),
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
  assert(owner.version == 4 and owner.attached(), table.concat(notices, "\n"))
  assert(comments.capture(review.buf)[1].source.body == "Keep this new feedback", "deletion lost newer typing")
  assert(vim.api.nvim_get_current_line() == focused_text, "deletion moved the cursor out of the current comment")
  assert(not vim.bo[review.buf].modified, "the queued save did not settle")
  assert(opened == 1 and closed == 0, "subsequent deletion reopened the native document")
end, debug.traceback)
vim.fn.delete(path)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("plan_test_delete passed")
vim.cmd("qa!")
