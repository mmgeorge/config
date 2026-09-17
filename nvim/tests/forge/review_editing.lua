vim.opt.runtimepath:append("nvim")
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
local hold_edits, pending_edit = false, nil
adapter._set_runner_for_test(function(method, params, callback)
  if method == "review.open" then callback({ document = "editing", field = vim.deepcopy(field) })
  elseif method == "github.actor" then callback({ login = "viewer" })
  elseif method == "review.reconcile" then reconcile_callback = callback
  elseif method == "review.header" then callback({ ready = true })
  elseif method == "review.materialize" then callback({ snapshot = snapshot(), field = vim.deepcopy(field) })
  elseif method == "review.view" or method == "review.close" then callback({})
  elseif method == "review.load" then callback({ diagnostic = {} })
  elseif method == "review.act" then actions = actions + 1 error("editable text dispatched an action")
  elseif method == "review.region_edit" then
    if hold_edits then
      local receive = callback
      callback = function(result) pending_edit = function() receive(result) end end
    end
    local before = snapshot()
    local selected
    for _, value in ipairs(field) do if value.region == params.region then selected = value end end
    assert(selected and params.base == selected.revision)
    selected.text, selected.sequence, selected.revision = params.text, params.sequence, params.base + 1
    local start, previous_count = 0, 0
    for _, value in ipairs(before.block) do
      if value.id == "region:" .. params.region then previous_count = #value.text break end
      start = start + #value.text
    end
    local changed = field_block(selected)
    local count = 0
    for _, value in ipairs(before.block) do count = count + #value.text end
    revision = revision + 1
    callback({ document = "editing", region = params.region, sequence = params.sequence, revision = selected.revision,
      patch = { document = "editing", base = revision - 1, next = revision, base_rows = count,
        next_rows = count - previous_count + #changed.text, base_blocks = 6, next_blocks = 6,
        removed_block = {}, block_edit = {},
        text_edit = { { start_row = start, removed_rows = previous_count, text = changed.text } },
        metadata_edit = { { block = changed.id, row_count = #changed.text, metadata = changed.metadata } },
      } })
  elseif method == "review.save" then
    saves = saves + 1
    local captured = vim.deepcopy(field)
    save_callback = function(failure, outcome)
      if failure then callback(nil, failure) return end
      if outcome == "missing" then callback({}) return end
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
assert(vim.wait(1000, function() return state.shown and state.view_ready end))
assert(warmed_users == 1 and warmed_issues == 1 and cache.user_completion_enabled(state.replica.buffer))
local function cursor(row, column)
  vim.api.nvim_win_set_cursor(0, { row, column or 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.replica.buffer })
end
local function keys(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = state.replica.buffer })
end
local function marker_rows()
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.replica.buffer,
    vim.api.nvim_get_namespaces().ForgeReviewDirty, 0, -1, { details = true })) do
    local line = vim.api.nvim_buf_get_lines(state.replica.buffer, mark[2], mark[2] + 1, false)[1]
    if line == "Description:" then
      assert(mark[3] == #"Description" and mark[4].virt_text_pos == "inline")
      assert(mark[4].virt_text[1][1] == "*", "description marker must render before its colon")
    else
      assert(mark[4].virt_text[1][1] == " *")
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
  assert(vim.api.nvim_win_get_cursor(0)[2] == #vim.api.nvim_get_current_line() - 1)
  assert(vim.fn.getcurpos()[4] == 0, "End moved into virtual whitespace")
  local entered, insert_failure = false, nil
  vim.api.nvim_create_autocmd("InsertEnter", { buffer = state.replica.buffer, once = true, callback = function()
    entered = true
    local success, failure = pcall(function()
      assert(vim.api.nvim_win_get_cursor(0)[2] == 0, "i did not move to the input start")
      assert(vim.deep_equal(marker_rows(), { entry.row }), "editing marker missing")
      local marks = vim.api.nvim_buf_get_extmarks(state.replica.buffer, state.replica.namespace,
        { entry.row - 1, 0 }, { entry.row - 1, -1 }, { details = true })
      assert(marks[1][4].virt_text[1][1] == entry.label .. "*: ")
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
  local before = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)
  keys("i<BS><Esc>")
  assert(vim.wait(1000, function() return not state.replica.editable.native.rejecting end))
  assert(not state.replica.editable.fault, state.replica.editable.fault)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), before),
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
assert(vim.wait(1000, function() return not require("forge.editable").suspend_generated_text(state.replica.editable) end))
cursor(2)
assert(not vim.bo.modifiable, "read-only metadata was unlocked")
cursor(6)
keys("<C-S>")
assert(not vim.bo.modified and #marker_rows() == 0, "save waited for GitHub before clearing markers")
assert(vim.wait(1000, function() return saves == 1 end))
assert(state.saving and not state.save_pending, "in-flight save was not retained locally")
adapter.refresh(state)
assert(state.refresh_pending and not state.rendering, "refresh raced a pending save")
save_callback("injected save rejection")
assert(vim.wait(1000, function() return not state.saving and not state.rendering end))
assert(vim.bo.modified and #marker_rows() == 3, "rejection cleared unsaved state")
keys("<C-S>")
assert(not vim.bo.modified and #marker_rows() == 0)
assert(vim.wait(1000, function() return saves == 2 end))
cursor(1)
keys("A newer<Esc>")
assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "new typing stayed optimistically clean")
save_callback()
assert(vim.wait(1000, function() return not state.saving and not state.rendering end))
assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "save cleared text typed after submission")
keys("A<C-S><Esc>")
assert(vim.wait(1000, function() return saves == 3 end))
save_callback()
assert(vim.wait(1000, function() return not vim.bo.modified and #marker_rows() == 0 end))
keys("A write<Esc>")
vim.cmd("write")
assert(not vim.bo.modified and #marker_rows() == 0, ":w waited for its response")
assert(vim.wait(1000, function() return saves == 4 end))
save_callback()
assert(vim.wait(1000, function() return not vim.bo.modified and #marker_rows() == 0 end))
assert(vim.wait(1000, function() return not state.saving and not state.reconciling end))
hold_edits = true
keys("A delayed<Esc>")
adapter.save(state)
assert(not vim.bo.modified and #marker_rows() == 0, "save waited for the local edit acknowledgement")
assert(state.save_pending and not state.saving and saves == 4)
hold_edits = false
assert(pending_edit, "edit was not flushed on save")
pending_edit()
assert(vim.wait(1000, function() return saves == 5 end))
adapter.save(state)
assert(saves == 5 and not state.save_pending, "unchanged save duplicated an in-flight request")
keys("A queued<Esc>")
adapter.save(state)
assert(state.save_pending and saves == 5 and not vim.bo.modified and #marker_rows() == 0)
keys("A latest<Esc>")
adapter.save(state)
assert(state.save_pending and saves == 5 and not vim.bo.modified)
save_callback()
assert(vim.wait(1000, function() return saves == 6 end), "queued save was dropped")
assert(not vim.bo.modified and #marker_rows() == 0, "older completion restored queued markers")
keys("A unsaved<Esc>")
save_callback()
assert(vim.wait(1000, function() return not state.saving and not state.reconciling end))
assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "queued completion cleared newer typing")
for _, outcome in ipairs({ "rejected", "outcome_unknown", "missing" }) do
  adapter.save(state)
  assert(not vim.bo.modified and #marker_rows() == 0)
  assert(vim.wait(1000, function() return state.saving ~= nil end))
  keys("A retry<Esc>")
  adapter.save(state)
  assert(state.save_pending and not vim.bo.modified)
  local before = saves
  save_callback(nil, outcome)
  if outcome == "outcome_unknown" then
    assert(vim.wait(1000, function() return reconcile_callback ~= nil end))
    assert(state.save_recovering and vim.bo.modifiable, "recovery disabled native editing")
    reconcile_callback(nil, "injected observation failure")
    assert(vim.wait(1000, function() return not state.save_recovering end))
    assert(state.save_uncertain and vim.bo.modifiable, "failed recovery disabled editing")
    reconcile_callback = nil
    keys("gR")
    assert(vim.wait(1000, function() return reconcile_callback ~= nil end), "gR did not retry field recovery")
    reconcile_callback({ field = vim.deepcopy(field), uncertain = false })
    assert(vim.wait(1000, function() return not state.save_recovering end))
  end
  assert(vim.wait(1000, function() return not state.saving and not state.reconciling end))
  assert(not state.save_pending and saves == before, "failed save automatically retried a queued request")
  assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "failure lost unsaved state")
end
keys("A rollback<Esc>")
adapter.save(state)
assert(vim.wait(1000, function() return state.saving ~= nil end))
reconcile_callback = nil
save_callback(nil, "outcome_unknown")
assert(vim.wait(1000, function() return reconcile_callback ~= nil end))
reconcile_callback({ field = vim.deepcopy(field), uncertain = false })
assert(vim.wait(1000, function()
  return not state.save_recovering and not require("forge.editable").suspend_generated_text(state.replica.editable)
end))
assert(vim.api.nvim_get_current_line() == field[1].baseline, "unchanged rejected capture did not roll back to observed text")
assert(not vim.bo.modified and #marker_rows() == 0)
cursor(6)
keys("A<CR><CR><Esc>")
local description = field[3].text:gsub("[\r\n]+$", "")
for _, region in ipairs({ "reviewers", "title" }) do
  local bounds = state.replica.editable.native.anchor[region]
  vim.api.nvim_buf_set_text(state.replica.buffer, bounds.finish.row, bounds.finish.column,
    bounds.finish.row, bounds.finish.column, { "", "", "" })
end
adapter.save(state)
assert(not vim.bo.modified and #marker_rows() == 0, "trim delayed optimistic save")
for _, region in ipairs({ "title", "reviewers" }) do
  local bounds = state.replica.editable.native.anchor[region]
  assert(bounds.start.row == bounds.finish.row, "save retained trailing lines in " .. region)
end
local anchor = state.replica.editable.native.anchor.body
assert(table.concat(vim.api.nvim_buf_get_text(state.replica.buffer, anchor.start.row, anchor.start.column,
  anchor.finish.row, anchor.finish.column, {}), "\n") == description, "save retained trailing description lines")
assert(vim.wait(1000, function() return state.saving ~= nil end))
assert(field[3].text == description and description:find("\nSecond line", 1, true), "save changed internal paragraphs")
save_callback()
assert(vim.wait(1000, function() return not state.saving and not state.reconciling end))
local function completion(row, text, source)
  cursor(row)
  keys("A " .. text .. " <Esc>")
  local line = vim.api.nvim_get_current_line()
  vim.api.nvim_win_set_cursor(0, { row, #line - 1 })
  assert(source:enabled(), "completion source disabled in editable field")
  local result
  source:get_completions({}, function(value) result = value end)
  assert(vim.wait(1000, function() return result ~= nil end))
  return result.items
end
local users = require("forge.views.pr.reviewer_source").new()
assert(completion(1, "@al", users)[1].textEdit.newText == "@alice")
assert(#completion(1, "@vi", users) == 2, "self mention was hidden outside reviewer input")
assert(#completion(3, "@al", users) == 1, "self reviewer remained selectable")
cursor(3)
keys("A @ViEwEr<Esc>")
local before_self = saves
adapter.save(state)
assert(saves == before_self and not state.save_pending and vim.bo.modified, "self reviewer was submitted")
assert(completion(6, "@al", users)[1].textEdit.newText == "@alice")
local issues = require("github.issue_source").new()
assert(completion(6, "#issue", issues)[1].textEdit.newText == "#42")
assert(warmed_users == 1 and warmed_issues == 1, "completion performed another remote refresh")
assert(actions == 0 and vim.deep_equal(errors, { "injected save rejection", "Review save was rejected",
  "injected observation failure. Press gR to retry recovery. Your edits are retained.",
  "Missing native review save result", "You cannot request a review from yourself (@viewer)" }), vim.inspect(errors))
require("forge.editable").flush(state.replica.editable)
assert(vim.wait(1000, function() return not require("forge.editable").suspend_generated_text(state.replica.editable) end))
vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
adapter._set_runner_for_test(nil)
print("review_editing: immediate save markers, queued saves, reconciliation failures, native fields, and completion passed")
