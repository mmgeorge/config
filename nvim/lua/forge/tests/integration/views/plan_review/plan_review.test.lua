local finish_input = dofile("nvim/lua/forge/tests/support/plan_comment_input.lua")
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- requires-forge-host
T["directory_planning"] = function()
  vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/render-markdown.nvim")
  local started = host.start({ config = true })
  local workspace = temp.directory("planning-workspace")
  vim.fn.writefile({ '{"checks":{}}' }, workspace .. "/.forge.json")
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { provider = "mock" }, diff_logging = false, harness_logging = false })
  local state = require("forge.session").harness
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  require("forge.views.harness").open()
  wait_until(function() return state.session ~= nil end, "planning host did not open", {
    timeout_ms = 4000, errors = errors,
    context = function() return tostring(started.client._client and started.client._client.stderr) end,
  })

  local function request(method, params)
    local result, failure, done
    started.client.request({ method = method, params = params, session_id = state.session.id },
      function(value, problem) result, failure, done = value, problem, true end)
    wait_until(function() return done end, method .. " did not settle")
    assert(not failure, tostring(failure))
    return result
  end
  request("session.prompt.submit", { operation_id = "planning-access", text = "/yolo" })
  request("session.prompt.submit", { operation_id = "planning-draft", text = "/plan Add an isolated callable" })
  local plan
  wait_until(function()
    local task = state.session.active_task
    plan = task and task.plan and task.plan.artifact or nil
    return plan and plan.state == "awaiting_review"
  end, "physical draft did not reach review", { errors = errors,
    context = function() return vim.inspect(state.session.active_task) end })

  local directory_list = vim.fn.glob(vim.fs.joinpath(started.data, "**", "plan-workspaces", state.session.id, plan.id), true, true)
  MiniTest.expect.equality(#directory_list, 1)
  local directory = directory_list[1]
  local entry_list = vim.fn.readdir(directory)
  table.sort(entry_list)
  MiniTest.expect.equality(entry_list, { "docs", "files" })
  local proposal = vim.fs.joinpath(directory, "files", "src", "change.rs")
  assert(table.concat(vim.fn.readfile(proposal), "\n"):find("requested_change", 1, true))
  assert(not plan.working_path:find(directory, 1, true), "review storage aliases the editable draft")
  assert(vim.fn.filereadable(workspace .. "/src/change.rs") == 0, "planning wrote repository implementation")

  local function open_review()
    require("forge.views.plan_review.native_controller").open(plan)
    wait_until(function() return state.plan_review and state.plan_review.owner.ready end,
      "submitted plan did not open", { errors = errors })
    return state.plan_review
  end
  local review = open_review()
  local function tests_visible()
    return vim.iter(review.owner.source):any(function(row) return row.block == "plan:section:tests" end)
  end
  MiniTest.expect.equality({ review.public_only, review.owner.show_tests, review.owner.show_imports }, { false, false, false })
  MiniTest.expect.equality(tests_visible(), false)
  keymap.invoke("ot", { buffer = review.buf })
  wait_until(tests_visible, "ot did not show the Tests section", { errors = errors })
  keymap.invoke("oi", { buffer = review.buf })
  keymap.invoke("ov", { buffer = review.buf })
  wait_until(function() return review.public_only and review.owner.show_imports end,
    "oi and ov did not update their independent filters", { errors = errors })
  MiniTest.expect.equality(tests_visible(), true)
  for _, key in ipairs({ "ot", "oi", "ov" }) do keymap.invoke(key, { buffer = review.buf }) end
  wait_until(function()
    return not review.public_only and not review.owner.show_imports and not tests_visible()
  end, "visibility filters did not return to their defaults", { errors = errors })
  local function expect_section_spacing()
    local heading = assert(buffer.find_row(review.buf, "Viewed Changes ("), "viewed heading is missing")
    MiniTest.expect.equality(buffer.line(review.buf, heading - 1), "")
  end
  expect_section_spacing()
  local function file_section()
    local section
    for _, line in ipairs(buffer.lines(review.buf)) do
      local heading = vim.trim(line):match("^(.-) %(%d+%):$")
      if heading == "Unviewed Changes" or heading == "Viewed Changes" then section = heading end
      if line:find("src/change.rs", 1, true) and line:find("New", 1, true) then return section end
    end
  end
  MiniTest.expect.equality(file_section(), "Unviewed Changes")
  local function toggle_file()
    assert(vim.fn.search([[^.*src/change\.rs]], "cw") > 0, "file heading is missing")
    keymap.invoke("<Tab>", { buffer = review.buf })
  end
  local function declaration_visible()
    return buffer.text(review.buf):find("pub fn requested_change", 1, true) ~= nil
  end
  MiniTest.expect.equality(declaration_visible(), false)
  local function deferred()
    for _, row in ipairs(review.owner.source) do
      if row.block:match("^plan:design:file:.*:hunk:deferred$") then return row.block end
    end
  end
  local placeholder = assert(deferred(), "folded open eagerly delivered the file body")
  toggle_file()
  keymap.invoke("S", { buffer = review.buf })
  wait_until(declaration_visible, "unfold did not load the file body", { errors = errors })
  MiniTest.expect.equality(deferred(), nil)
  wait_until(function() return file_section() == "Viewed Changes" end, "S did not move the expanded file", { errors = errors })
  expect_section_spacing()
  local signature = assert(buffer.find_row(review.buf, "pub fn requested_change();"), "review lost the declaration terminator")
  MiniTest.expect.equality(buffer.find_row(review.buf, "Algorithm"), nil)
  local body = assert(buffer.find_row(review.buf, "return none"), "algorithm should open in full")
  vim.api.nvim_win_set_cursor(review.win, { body, 0 })
  keymap.invoke("<Tab>", { buffer = review.buf })
  wait_until(function() return not buffer.find_row(review.buf, "return none") end,
    "collapsing the callable left its body visible", { errors = errors })
  MiniTest.expect.equality(vim.trim(buffer.line(review.buf, signature)), "pub fn requested_change();")
  vim.api.nvim_win_set_cursor(review.win, { signature, 0 })
  keymap.invoke("<Tab>", { buffer = review.buf })
  wait_until(function() return buffer.find_row(review.buf, "return none") end,
    "expanding the callable did not restore its body", { errors = errors })
  local retained_buffer = review.buf
  keymap.invoke("q", { buffer = retained_buffer })
  local retained_tick = vim.api.nvim_buf_get_changedtick(retained_buffer)
  local filetype_events = 0
  vim.api.nvim_create_autocmd("FileType", { buffer = retained_buffer, callback = function()
    filetype_events = filetype_events + 1
  end })
  review = open_review()
  MiniTest.expect.equality(review.buf, retained_buffer)
  MiniTest.expect.equality(filetype_events, 0)
  MiniTest.expect.equality(vim.api.nvim_buf_get_changedtick(retained_buffer), retained_tick)
  MiniTest.expect.equality(file_section(), "Viewed Changes")
  MiniTest.expect.equality(declaration_visible(), true)
  local comments = require("forge.draft_comments")
  local function add_message(key, source, message)
    assert(vim.fn.search(source, "cw") > 0, "annotation source is missing")
    keymap.invoke(key, { buffer = review.buf })
    finish_input(vim.split(message, "\n", { plain = true }))
    local draft = comments.capture(review.buf)
    return draft[#draft].id
  end
  local question_body = "Why does this declaration need its own file?"
  local question_id = add_message("A", "/// Apply the requested behavior", question_body)
  add_message("C", "pub fn requested_change", "Keep this declaration.\n\nKeep its documentation.\n\n```rust\nuse scene::Scene;\n```")
  vim.api.nvim_win_set_cursor(review.win, { assert(buffer.find_row(review.buf, "Keep this declaration.")), 0 })
  vim.cmd("normal! 0")
  local content_column = vim.fn.wincol()
  vim.cmd("normal! j0")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review.buf })
  MiniTest.expect.equality(vim.api.nvim_get_current_line(), "")
  MiniTest.expect.equality(vim.fn.wincol(), content_column, { fail_reason = "the blank comment row placed the cursor outside its gutter" })
  local rendered
  require("forge.render.harness.markdown").after_render(review.buf, function(done) rendered = done end)
  wait_until(function() return rendered ~= nil end, "comment Markdown did not settle")
  local fence_row = assert(buffer.find_row(review.buf, "use scene::Scene;")) - 2
  local layout_namespace = vim.api.nvim_get_namespaces()["render-markdown.nvim"]
  local fence_marks = vim.api.nvim_buf_get_extmarks(review.buf, layout_namespace, { fence_row, 0 }, { fence_row, -1 }, { details = true })
  assert(vim.iter(fence_marks):any(function(mark) return mark[4].virt_text or mark[4].conceal end),
    "expanded comment displayed a literal Rust fence")
  local draft = comments.capture(review.buf)
  assert(vim.fn.search([[src/change\.rs]], "cw") > 0)
  local file_row = vim.api.nvim_win_get_cursor(review.win)[1]
  keymap.invoke("U", { buffer = review.buf })
  wait_until(function() return file_section() == "Unviewed Changes" end, "U did not return the file", { errors = errors })
  MiniTest.expect.equality(vim.api.nvim_win_get_cursor(review.win)[1], file_row,
    { fail_reason = "moving the last expanded file scrolled to its new section" })
  MiniTest.expect.equality(comments.capture(review.buf), draft)
  MiniTest.expect.equality(vim.bo[review.buf].modified, true)
  local answer = require("forge.tests.support.mock_provider").hold(state.session.id, nil, true)
  vim.cmd("write")
  local request = answer:wait()
  assert(request.prompt:find(question_body, 1, true), "mock provider held a different request")
  for _, move in ipairs({ { "S", "Viewed Changes" }, { "U", "Unviewed Changes" } }) do
    assert(vim.fn.search([[src/change\.rs]], "cw") > 0)
    keymap.invoke(move[1], { buffer = review.buf })
    wait_until(function() return file_section() == move[2] end, "file did not move while the answer was pending", { errors = errors })
    assert(review.owner.pending_reply_by_id[question_id], "moving the file dropped the pending question")
    assert(buffer.find_row(review.buf, "Generating response"), "the moved question lost its progress row")
    MiniTest.expect.equality(comments.capture(review.buf), draft)
  end
  local progress_row = assert(buffer.find_row(review.buf, "Generating response"))
  vim.api.nvim_win_set_cursor(review.win, { progress_row, 0 })
  keymap.invoke("<Tab>", { buffer = review.buf })
  MiniTest.expect.equality(buffer.find_row(review.buf, question_body), nil, { fail_reason = "Tab inside the pending answer did not fold the question thread" })
  assert(buffer.find_row(review.buf, "Keep this declaration."), "folding the question hid another comment")
  answer:release()
  wait_until(function()
    local reply = review.owner.reply_by_id[question_id]
    return not review.owner.saving and not vim.bo[review.buf].modified and reply and #reply.body > 0
  end, "moved file's comment and question did not save and receive an answer", { errors = errors })
  local reply = review.owner.reply_by_id[question_id]
  MiniTest.expect.equality(reply.question_body, question_body)
  MiniTest.expect.equality(buffer.find_row(review.buf, question_body), nil)
  MiniTest.expect.equality(vim.bo[review.buf].modified, false)
  MiniTest.expect.equality(comments.capture(review.buf), draft)
  keymap.invoke("<Tab>", { buffer = review.buf })
  local question_row = assert(buffer.find_row(review.buf, question_body), "moved question is missing")
  assert(buffer.find_row(review.buf, "Mock response:", { first_row = question_row + 1 }),
    "answer did not render below the moved question")
  local boxed_lines = buffer.lines(review.buf)
  vim.api.nvim_win_set_cursor(review.win, { question_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = review.buf })
  MiniTest.expect.equality(buffer.lines(review.buf), boxed_lines, { fail_reason = "hovering opened the saved question for editing" })
  local answer_line = buffer.line(review.buf, question_row + 1)
  assert(vim.startswith(reply.body, answer_line) and vim.startswith(answer_line, "Mock response:"))
  vim.cmd("normal! 0Vj")
  MiniTest.expect.equality(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = "V" }),
    { question_body, answer_line }, { fail_reason = "card decorations entered the native line selection" })
  keymap.invoke("C", { buffer = review.buf, mode = "x" })
  local composer = vim.api.nvim_get_current_buf()
  MiniTest.expect.equality(vim.bo[composer].filetype, "ForgePlanCommentInput")
  MiniTest.expect.equality(buffer.lines(composer), { "> " .. question_body, "> " .. answer_line, "", "" })
  keymap.invoke("<Esc>", { buffer = composer })
  MiniTest.expect.equality(comments.capture(review.buf), draft)
  toggle_file()
  keymap.invoke("q", { buffer = review.buf })
  review = open_review()
  MiniTest.expect.equality(declaration_visible(), false)
  MiniTest.expect.equality(deferred(), placeholder)
  MiniTest.expect.equality(comments.capture(review.buf), draft)
  MiniTest.expect.equality(review.owner.reply_by_id[question_id], reply)
  assert(not vim.bo[review.buf].modifiable, "reviewed declarations became an editable proposal")
  toggle_file()
  wait_until(declaration_visible, "reopened review did not show the declaration")
  MiniTest.expect.equality(buffer.find_row(review.buf, question_body), nil, { fail_reason = "reopened plan expanded an existing comment" })
  MiniTest.expect.equality(#review.owner.comment_state.range_list, 2)
  for _, range in ipairs(review.owner.comment_state.range_list) do MiniTest.expect.equality(range.folded, true) end
  toggle_file()
  local function schema()
    local result, failure, done
    review.owner.action("schema", function(value, problem) result, failure, done = value, problem, true end)
    wait_until(function() return done end, "review schema did not settle")
    assert(not failure, tostring(failure))
    assert(result.schema, "review action omitted its captured schema")
    return result.schema.block
  end
  local submitted_schema = schema()
  assert(vim.json.encode(submitted_schema):find("requested_change", 1, true), "schema omitted the submitted declarations")
  local submitted_lines = buffer.lines(review.buf)
  vim.fn.writefile({ "/// Revise the proposal.", "pub fn changed();" }, proposal)
  MiniTest.expect.equality(schema(), submitted_schema)
  MiniTest.expect.equality(buffer.lines(review.buf), submitted_lines)
  errors:assert_none()
  assert(host.stop(), "planning host did not stop")
end

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
  local selection = { symbol = "original", name = "Original", expected_version = 1, existing_names = { "Original" },
    preview = { { block = "first", column = 0, length = 8 }, { block = "second", column = 4, length = 8 } } }
  local input, answer, finish, committed
  popup.incremental_input = function(options, callback) input, answer = options, callback end
  -- The reviewer confirms each rename as soon as the confirmation opens.
  require("forge.infra.confirm").open = function(_, on_yes) on_yes() end
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

  finish_input({ "draft", "λ\r", "" })
  local comments = require("forge.draft_comments")
  assert(comments.focus(native_buffer, comments.capture(native_buffer)[1].id))
  local body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
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
      callback({ rename = { symbol = "tests", name = heading, existing_names = { heading }, expected_version = version,
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
  vim.api.nvim_win_set_cursor(review.win, { assert(buffer.find_row(review.buf, "Feedback typed while deleting")), 0 })
  local comment_line = vim.api.nvim_get_current_line()
  pending.callback(nil, "test deletion failed")
  pending = nil
  assert(not require("forge.views.harness.activity").busy(state) and review.owner.attached(), "failed deletion stranded the review")
  assert(comments.capture(review.buf)[1].source.body == "Feedback typed while deleting", "rollback lost newer feedback")
  assert(vim.api.nvim_get_current_line() == comment_line, "rollback moved the current comment cursor")
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
  -- The reviewer confirms each rename as soon as the confirmation opens; the later test
  -- deletion answers the real confirmation.
  local confirm = require("forge.infra.confirm")
  local open_confirmation = confirm.open
  confirm.open = function(_, on_yes) on_yes() end
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
  confirm.open = open_confirmation

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
  local card = owner.comment_state.range_list[1]
  assert(card.compact and vim.api.nvim_win_get_cursor(window)[1] == card.first_row + 1,
    "comment fixture did not select the boxed feedback")
  tests, version = {}, 4
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:4" }, review = presentation(owner.document) })
  assert(owner.version == 4 and owner.attached(), table.concat(notices:messages(), "\n"))
  assert(comments.capture(review.buf)[1].source.body == "Keep this new feedback", "deletion lost newer typing")
  assert(vim.iter(owner.comment_state.range_list):any(function(range)
    return tostring(range.annotation.id) == "late" and range.compact
  end), "the queued save lost the feedback card")
  MiniTest.expect.equality(comments.source_line_at_cursor(review.buf), 1,
    { fail_reason = "the queued save moved the cursor away from the comment's source line" })
  assert(not vim.bo[review.buf].modified, "the queued save did not settle")
  assert(opened == 1 and closed == 0, "subsequent deletion reopened the native document")
end

T["card navigation and native visual selection exclude the frame"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(72, 28, { rgb = true })
  child.lua([[vim.api.nvim__inspect_cell(1, 0, 0)]])
  child.lua([[
    vim.opt.rtp:prepend(...)
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0x000000 })
    vim.api.nvim_set_hl(0, "Visual", { bg = 0x123456 })
    local buf, win = vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win()
    require("forge.draft_comments").attach(buf, win, { "plan" }, {
      { id = "1", source_line = 1, body = "Why café?", replies = {
        { heading = "Thought for 3 seconds", body_lines = { "Because.", "", "Last line." } },
      } },
    }, { markdown = true, popup_input = true })
    vim.api.nvim_win_set_cursor(win, { 2, 0 })
  ]], { vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p")) })
  child.api.nvim_win_set_cursor(0, { 3, 0 })
  local content_column = child.fn.wincol()
  child.type_keys("j", "0")
  MiniTest.expect.equality(child.fn.wincol(), content_column)
  for _, motion in ipairs({ "h", "l", "$", "0" }) do
    child.type_keys(motion)
    MiniTest.expect.equality(child.fn.wincol(), content_column, { fail_reason = "blank row after " .. motion })
  end
  child.type_keys("v", "y")
  MiniTest.expect.equality(child.fn.getreg('"'), "\n")
  child.type_keys("V", "y")
  MiniTest.expect.equality(child.fn.getreg('"', 1, true), { "" })
  child.type_keys("v", "j", "y")
  MiniTest.expect.equality(child.fn.getreg('"', 1, true), { "", "L" })
  child.api.nvim_win_set_cursor(0, { 3, 4 })
  child.type_keys("j", "j")
  MiniTest.expect.equality(child.api.nvim_win_get_cursor(0), { 5, 4 })
  MiniTest.expect.equality(child.lua_get("vim.wo.virtualedit"), "")
  child.type_keys("k", "v")
  child.lua([[
    local target = assert(require("forge.draft_comments").input_target(vim.api.nvim_get_current_buf()))
    require("forge.views.plan_review.comment_input").open(target, false, function() end)
  ]])
  MiniTest.expect.equality(child.api.nvim_buf_get_lines(0, 0, -1, true), { ">", "", "" })
  child.type_keys("<C-c>")
  child.api.nvim_win_set_cursor(0, { 2, 0 })
  child.type_keys("0", "h")
  MiniTest.expect.equality(child.api.nvim_win_get_cursor(0), { 2, 0 })
  child.type_keys("V", "j")
  local painted = child.lua([[
    vim.cmd("redraw!")
    local position = vim.fn.screenpos(vim.api.nvim_get_current_win(), 2, 1)
    local function cell(line, column)
      local value = vim.api.nvim__inspect_cell(1, line - 1, column - 1)
      assert(value and value[2], vim.inspect({ position, line, column, value }))
      return value
    end
    return {
      body = cell(position.row, position.curscol)[2].background,
      border = cell(position.row, position.curscol - 2)[2].background == 0x123456,
      padding = cell(position.row, position.curscol + 12)[2].background == 0x123456,
      divider = cell(position.row + 1, position.curscol)[2].background == 0x123456,
    }
  ]])
  MiniTest.expect.equality(painted, { body = 0x123456, border = false, padding = false, divider = false })
  child.type_keys("y")
  MiniTest.expect.equality(child.fn.getreg('"', 1, true), { "Why café?", "Because." })
  child.type_keys("0", "w", "v", "e", "y")
  MiniTest.expect.equality(child.fn.getreg('"'), "café")

  -- A reversed character selection captures exactly its UTF-8 bytes for the popup.
  child.type_keys("e", "v", "b")
  child.lua([[
    local target = assert(require("forge.draft_comments").input_target(vim.api.nvim_get_current_buf()))
    _G.card_parent = target.window
    require("forge.views.plan_review.comment_input").open(target, true, function() end)
  ]])
  MiniTest.expect.equality(child.api.nvim_buf_get_lines(0, 0, -1, true), { "> café", "", "" })
  MiniTest.expect.equality(child.api.nvim_win_get_cursor(0), { 3, 0 })
  local placement = child.lua([[
    vim.cmd("redraw!")
    local last = vim.fn.screenpos(card_parent, 5, 1)
    local popup = vim.api.nvim_win_get_position(0)
    local footer = vim.api.nvim__inspect_cell(1, last.row, last.curscol - 3)
    return { footer[1], popup[1] - last.row, popup[2] - (last.curscol - 3) }
  ]])
  MiniTest.expect.equality(placement, { "╰", 1, 0 })
end

T["visual S and U capture every selected file once"] = function()
  local a, b, c = "plan:design:file:a", "plan:design:file:b", "plan:design:file:c"
  local host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua").start({
    lines = { "Unviewed", "A", "A body", "B", "Viewed", "C" },
    block_id = { "plan:section:changes:unviewed", a, a .. ":hunk:1:row:1", b, "plan:section:changes:viewed", c },
  })
  local review = host.open()
  vim.api.nvim_win_set_cursor(review.win, { 2, 0 })
  vim.cmd("normal! V2j")
  keymap.invoke("S", { buffer = review.buf, mode = "x" })
  MiniTest.expect.equality(host.actions[1].selected_files, { a, b })
  host.complete_move(1, { 1, 5, 2, 3, 4, 6 })
  vim.api.nvim_win_set_cursor(review.win, { 5, 0 })
  vim.cmd("normal! v2k")
  keymap.invoke("U", { buffer = review.buf, mode = "x" })
  MiniTest.expect.equality(host.actions[2].selected_files, { a, b })
  MiniTest.expect.equality(#host.actions, 2)
end

T["marking a file viewed keeps the next file in place on screen"] = function()
  local lines = vim.tbl_map(function(row) return ("File %02d"):format(row) end, vim.fn.range(1, 75))
  local ids = vim.tbl_map(function(row) return "plan:design:file:" .. row end, vim.fn.range(1, #lines))
  lines[65], ids[65] = "Viewed Changes", "plan:section:changes:viewed"
  local host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua").start({ lines = lines, block_id = ids })
  local review = host.open()
  vim.wo[review.win].scrolloff = 0
  vim.fn.winrestview({ lnum = 23, col = 2, topline = 18 })
  keymap.invoke("S", { buffer = review.buf })
  -- The user can scroll while the host is moving the file.
  vim.fn.winrestview({ lnum = 23, topline = 20 })
  local before = vim.fn.winsaveview().topline
  local order = vim.fn.range(1, #lines)
  table.insert(order, 70, table.remove(order, 23))
  host.complete_move(1, order)
  vim.cmd("redraw!")
  MiniTest.expect.equality(vim.api.nvim_get_current_line(), "File 24")
  MiniTest.expect.equality(vim.api.nvim_win_get_cursor(review.win), { 23, 2 })
  MiniTest.expect.equality(vim.fn.winsaveview().topline, before)

  -- If the moved file is the top row, anchoring the viewport to its identity would follow it.
  vim.fn.winrestview({ lnum = 70, topline = 70 })
  keymap.invoke("U", { buffer = review.buf })
  host.complete_move(2, vim.fn.range(1, #lines))
  vim.cmd("redraw!")
  MiniTest.expect.equality(vim.api.nvim_get_current_line(), "File 70")
  MiniTest.expect.equality(vim.fn.winsaveview().topline, 70)
end

T["Enter opens modified and deleted files from their plan headings"] = function()
  local modified = temp.file("modified.rs", { "pub fn modified() {}" })
  local deleted = temp.file("deleted.rs", { "pub fn deleted() {}" })
  local first, second = "plan:design:file:modified", "plan:design:file:deleted"
  local host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua").start({
    lines = { "Modified modified.rs", "Deleted deleted.rs" }, block_id = { first, second },
    open_path = { [first] = modified, [second] = deleted },
  })
  local review = host.open()
  for row, path in ipairs({ modified, deleted }) do
    vim.api.nvim_set_current_win(review.win)
    vim.api.nvim_win_set_cursor(review.win, { row, 0 })
    keymap.invoke("<CR>", { buffer = review.buf })
    local destination = vim.api.nvim_get_current_buf()
    MiniTest.expect.equality(vim.fs.normalize(vim.api.nvim_buf_get_name(destination)), vim.fs.normalize(path))
    MiniTest.expect.equality(vim.api.nvim_win_get_cursor(0), { 1, 0 })
    MiniTest.expect.equality(vim.api.nvim_win_get_buf(review.win), review.buf)
    vim.cmd("tabclose")
  end
end

T["behavior-only signature occupies one display row and keeps baseline comments"] = function()
  local marks = vim.json.decode(table.concat(vim.fn.readfile(
    "nvim/lua/forge/tests/fixtures/protocol/plan-review-behavior-signature.json"), "\n"))
  local host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua").start({
    lines = { "Changed file", "pub fn app() -> App;", "pub fn app() -> App;", "Install the plugin." },
    metadata = { [2] = { conceal = marks.baseline }, [3] = { gutter = marks.signature } },
  })
  host.annotation = { { id = "original", kind = "comment",
    source = { start_line = 2, end_line = 2, body = "Keep this entry point." } } }
  local review = host.open()
  vim.api.nvim_win_set_cursor(review.win, { 1, 0 })
  vim.cmd("redraw!")
  local rows = vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)
  MiniTest.expect.equality(#vim.tbl_filter(function(text) return text:find("pub fn app() -> App;", 1, true) end, rows), 1)
  local proposed = assert(buffer.find_row(review.buf, "pub fn app() -> App;")) - 1
  MiniTest.expect.equality(vim.api.nvim_win_text_height(review.win, { start_row = proposed, end_row = proposed }).all, 1)
  MiniTest.expect.equality(review.owner.comment_state.range_list[1].annotation.body, "Keep this entry point.")
  vim.api.nvim_win_call(review.win, function() vim.cmd("normal! j") end)
  MiniTest.expect.equality(vim.api.nvim_win_get_cursor(review.win)[1], review.owner.comment_state.range_list[1].first_row + 1)
end

T["expanded answer paints fenced Rust inside the card borders"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(100, 32, { rgb = true })
  child.lua([[vim.api.nvim__inspect_cell(1, 0, 0)]])
  child.lua([[
    vim.opt.runtimepath:prepend(...)
    local plugins = vim.fn.stdpath("data") .. "/lazy/"
    vim.opt.runtimepath:append(plugins .. "render-markdown.nvim")
    vim.opt.runtimepath:append(plugins .. "nvim-treesitter/runtime")
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/site")
    require("render-markdown").setup({})
    vim.api.nvim_set_hl(0, "@markup.strong.markdown_inline", { fg = "#22cc44", bold = true })
    vim.api.nvim_set_hl(0, "@keyword.import.rust", { fg = "#ffcc22" })
    vim.api.nvim_set_hl(0, "@string.rust", { fg = "#1199ff" })
    vim.api.nvim_set_hl(0, "ForgeWalkthroughComment", { fg = "#999999" })
    local buf, win = vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win()
    vim.bo[buf].filetype = "ForgePlan"
    local code = 'use scene::Scene;'
    local lines = { "```rust", code, 'let a = "source";', "```" }
    local source = vim.tbl_map(function(line) return { text = line, metadata = { markdown = true } } end, lines)
    local renderer = require("forge.render.harness.markdown")
    require("forge.draft_comments").attach(buf, win, lines, {
      { id = "q", source_line = 2, body = "Show **both** versions.", replies = {
        { heading = "Thought for 7 seconds", body_lines = {
          "Current code", "```rust", "use engine::scene::Scene;", 'let a = "before";', "```",
          "Proposed code", "```rust", code, 'let a = "after";', "```",
        } },
      } },
    }, { markdown = true, popup_input = true, after_render = function(_, _, projection)
      renderer.render(buf, win, require("forge.views.plan_review.markdown").ranges(source, projection))
    end })
    renderer.after_render(buf, function(done) _G.rendered = done end)
  ]], { vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p")) })
  wait_until(function() return child.lua_get("rendered == true") end, "Rust answer did not render")
  local cells = child.lua([[
    vim.cmd("redraw!")
    local function cell(row, col)
      return vim.api.nvim__inspect_cell(1, row - 1, col - 1)
    end
    local function color(row, col)
      local pos = vim.fn.screenpos(vim.api.nvim_get_current_win(), row, col)
      return cell(pos.row, pos.curscol)[2].foreground
    end
    local result = { color(2, 1), color(6, 1), color(7, 10), color(11, 1), color(12, 10), color(14, 10), color(3, 8) }
    for _, row in ipairs({ 5, 10 }) do
      local pos = vim.fn.screenpos(vim.api.nvim_get_current_win(), row, 1)
      local text = {}
      for col = 5, 88 do text[#text + 1] = cell(pos.row, col)[1] end
      result[#result + 1] = { cell(pos.row, 3)[1], table.concat(text):find("```", 1, true) == nil,
        table.concat(text):find("rust", 1, true) ~= nil, cell(pos.row, 88)[1] }
    end

    local last_code = vim.fn.screenpos(vim.api.nvim_get_current_win(), 12, 1)
    result[#result + 1] = { cell(last_code.row + 1, 3)[1], cell(last_code.row + 1, 88)[1] }
    return result
  ]])
  MiniTest.expect.equality(cells, { 0xffcc22, 0xffcc22, 0x1199ff, 0xffcc22, 0x1199ff, 0x1199ff, 0x22cc44,
    { "│", true, true, "│" }, { "│", true, true, "│" }, { "╰", "╯" } })
  local popup_offset = child.lua([[
    local parent = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_cursor(parent, { 12, 0 })
    local target = assert(require("forge.draft_comments").input_target(vim.api.nvim_get_current_buf()))
    require("forge.views.plan_review.comment_input").open(target, true, function() end)
    vim.cmd("redraw!")
    local code = vim.fn.screenpos(parent, 12, 1)
    return vim.api.nvim_win_get_position(0)[1] - code.row
  ]])
  MiniTest.expect.equality(popup_offset, 1)
end

T["question_preserves_fenced_code_rendering"] = function()
  vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/render-markdown.nvim")
  require("render-markdown").setup({})
  local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
  local question = "Why **local** here?"
  local example = '    local example = "two  spaces"'
  local plan_host = plan_review_host.start({ markdown = true, lines = {
    "```lua", "local before = 1", "local after = 2", "```", "", "See usage below.",
    "", "```text", "application command", "```",
  } })
  plan_host.annotation = {
    { id = "question", kind = "question", source = { start_line = 2, end_line = 2, body = question },
      reply = { question_body = question, body = table.concat({
        "Call `example()` like this.", "", "```lua", example, "```",
      }, "\n"), duration_ms = 1000 } },
  }
  local review = plan_host.open()
  vim.api.nvim_win_set_cursor(review.win, { review.owner.comment_state.range_list[1].first_row + 1, 0 })
  keymap.invoke("<Tab>", { buffer = review.buf })
  local function wait_for_markdown()
    local rendered
    require("forge.render.harness.markdown").after_render(review.buf, function(success) rendered = success end)
    wait_until(function() return rendered ~= nil end, "plan Markdown did not finish rendering")
    MiniTest.expect.equality(rendered, true)
  end
  wait_for_markdown()

  local parser = vim.treesitter.get_parser(review.buf, "markdown")
  local lua = assert(parser:children().lua, "the code fence did not inject Lua")
  local code_by_row = {}
  for _, tree in pairs(lua:trees()) do
    for _, range in ipairs(tree:included_ranges(true)) do
      for row = range[1], range[4] - (range[5] == 0 and 1 or 0) do
        local line = buffer.line(review.buf, row + 1)
        local first = row == range[1] and range[2] or 0
        local last = row == range[4] and range[5] or #line
        if first < last then code_by_row[row] = (code_by_row[row] or "") .. line:sub(first + 1, last) end
      end
    end
  end
  local code_rows = vim.tbl_keys(code_by_row)
  table.sort(code_rows)
  MiniTest.expect.equality(vim.tbl_map(function(row) return code_by_row[row] end, code_rows),
    { "local before = 1", example, "local after = 2" })

  local function has_capture(text, language, name)
    local row = assert(buffer.find_row(review.buf, text)) - 1
    local col = assert(buffer.line(review.buf, row + 1):find(text, 1, true)) - 1
    return vim.iter(vim.treesitter.get_captures_at_pos(review.buf, row, col)):any(function(capture)
      return capture.lang == language and capture.capture == name
    end)
  end
  MiniTest.expect.equality(has_capture("local** here?", "markdown_inline", "markup.strong"), true)
  MiniTest.expect.equality(has_capture("example()`", "markdown_inline", "markup.raw"), true)

  local namespace = vim.api.nvim_get_namespaces()["render-markdown.nvim"]
  local closing_row = assert(buffer.find_row(review.buf, "local after = 2"))
  local next_fence_row = assert(buffer.find_row(review.buf, "```text")) - 1
  local question_row = assert(buffer.find_row(review.buf, question)) - 1
  local answer_fence = assert(buffer.find_row(review.buf, example)) - 2
  local concealed = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(review.buf, namespace, 0, -1, { details = true })) do
    if mark[2] == question_row then
      MiniTest.expect.no_equality(mark[4].hl_group, "RenderMarkdownCode",
        { fail_reason = "the surrounding plan fence painted its code background over the question" })
    end
    if mark[4].conceal == "" then concealed[mark[2]] = true end
  end
  MiniTest.expect.equality(concealed[answer_fence], true)
  MiniTest.expect.equality(concealed[answer_fence + 2], true)
  MiniTest.expect.equality(concealed[closing_row], true)
  MiniTest.expect.equality(concealed[next_fence_row], true)

  local comments = require("forge.draft_comments")
  assert(comments.focus(review.buf, "question"))
  wait_for_markdown()
  MiniTest.expect.equality(has_capture("example()`", "markdown_inline", "markup.raw"), true)
  MiniTest.expect.equality(has_capture('local example = "two  spaces"', "lua", "keyword"), true)
end

return T
