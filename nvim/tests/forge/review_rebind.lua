vim.loader.enable(false)
local buffer = require("forge.buffer")
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
local replica = buffer.open(snapshot.document, { editable = {} })
assert(buffer.apply_snapshot(replica, snapshot).kind == "Applied")
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, replica.buffer)
local state = { active = true, shown = true, explicit_repository = true, document = snapshot.document,
  window = window, replica = replica, view = { id = "local-view" },
  notice = function(message) error(message) end }
local anchor = { revision = string.rep("a", 40), path = "src/assets.rs", side = "right", first_line = 44, last_line = 44 }
comments.attach(state, { snapshot = snapshot, comment = {}, inline_anchor = { ["inline:assets"] = anchor } })
vim.api.nvim_win_set_cursor(window, { 2, 0 })
assert(review.add_comment(state), "inline comment creation must use the local renderer")
vim.wait(20)
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
assert(vim.api.nvim_buf_get_lines(replica.buffer, 0, 1, false)[1] == "Unsaved title",
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
  if method == "review.open" then
    entered_batched = false
    replacement_document = "replacement-" .. tostring(vim.uv.hrtime())
    callback({ document = replacement_document, uncertain = uncertain, field = {
      { region = "title", baseline = baseline, revision = 0, uncertain = uncertain } } })
  elseif method == "review.header" or method == "review.load" or method == "review.view" or method == "review.section" then callback({})
  elseif method == "review.materialize" then
    assert(entered_batched, "replacement presentation omitted batched-review restoration")
    delayed = function()
      local replacement = vim.deepcopy(snapshot)
      replacement.document = replacement_document
      callback({ snapshot = replacement, field = {
        { region = "title", baseline = baseline, revision = 0, uncertain = uncertain } },
        comment = {}, inline_anchor = { ["inline:assets"] = replacement_anchor } })
    end
  elseif method == "review.close" then closed = params.document callback({})
  elseif method == "review.begin_batched" then entered_batched = true callback({ mode = "batched" })
  elseif method == "review.transition" then
    assert(params.document == state.document, "queued lifecycle retained the obsolete host identity")
    transitioned = params.document
    callback({ lifecycle = { state = "CLOSED", is_draft = false }, fresh_required = false })
  elseif method == "review.save" then queued_save, complete_save = vim.deepcopy(params), callback
  else error("unexpected recovery request " .. method) end
end)
local retained_buffer = state.replica.buffer
local recovered
assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
assert(vim.wait(1000, function() return delayed ~= nil end))
vim.bo[retained_buffer].modifiable = true
vim.api.nvim_buf_set_text(retained_buffer, 0, 0, 0, #"Unsaved title", { "Latest title" })
delayed()
assert(vim.wait(1000, function() return recovered ~= nil end))
assert(state.replica.buffer == retained_buffer and state.document == replacement_document)
assert(not state.host_lost and not state.rebinding)
assert(vim.wait(1000, function() return state.view_ready end), "replacement host view did not attach")
assert(vim.api.nvim_buf_get_lines(retained_buffer, 0, 1, false)[1] == "Latest title")
local capture_by_region = {}
for _, capture in ipairs(editable.capture_draft(state.replica.editable)) do capture_by_region[capture.region] = capture end
assert(capture_by_region.title.text == "Latest title")
assert(capture_by_region[region].text == "Sp\nsecond line αβ\n")
assert(vim.bo[retained_buffer].modified)
assert(vim.wait(1000, function() return queued_save ~= nil end), "replacement host did not resume the queued capture")
assert(queued_save.capture[1].text == "Unsaved title", "recovery recaptured newer typing for the queued save")
assert(queued_save.document == state.document and queued_save.capture[1].document == state.document)
assert(queued_save.capture[1].sequence < capture_by_region.title.sequence)
complete_save({ snapshot = { uncertain = false, field = {
  { region = "title", baseline = "Unsaved title", revision = 1 } } }, remote = { outcome = "confirmed" } })
assert(vim.wait(1000, function() return not state.saving end))
state.host_lost, delayed, recovered = true, nil, nil
baseline = "Unsaved title"
state.pending_operation = { kind = "lifecycle", method = "review.transition",
  params = { document = state.document, desired = "CLOSED" } }
assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
assert(vim.wait(1000, function() return delayed ~= nil end))
delayed()
assert(vim.wait(1000, function() return recovered and transitioned == state.document end))
assert(editable.capture_draft(state.replica.editable, { title = true })[1].text == "Latest title")
assert(vim.bo[retained_buffer].modified)
local previous_document = state.document
local previous_text = vim.api.nvim_buf_get_lines(retained_buffer, 0, -1, false)
baseline, delayed = "Remote changed", nil
local conflict
assert(review.rebind(state, function(value, failure) assert(not value) conflict = failure end))
assert(vim.wait(1000, function() return delayed ~= nil end))
delayed()
assert(vim.wait(1000, function() return conflict ~= nil end))
assert(closed == replacement_document and state.document == previous_document)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(retained_buffer, 0, -1, false), previous_text))
assert(vim.bo[retained_buffer].modified)
baseline, delayed = "Unsaved title", nil
replacement_anchor = vim.deepcopy(anchor)
replacement_anchor.revision = string.rep("b", 40)
conflict = nil
assert(review.rebind(state, function(value, failure) assert(not value) conflict = failure end))
assert(vim.wait(1000, function() return delayed ~= nil end))
delayed()
assert(vim.wait(1000, function() return conflict ~= nil end))
assert(conflict:find("inline revision", 1, true))
assert(closed == replacement_document and state.document == previous_document)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(retained_buffer, 0, -1, false), previous_text))
replacement_anchor, uncertain, delayed, queued_save = anchor, true, nil, nil
state.pending_operation = { kind = "save", text = { title = "Latest title" },
  capture = editable.capture_draft(state.replica.editable, { title = true }) }
recovered = nil
assert(review.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
assert(vim.wait(1000, function() return delayed ~= nil end))
delayed()
assert(vim.wait(1000, function() return recovered and state.view_ready end))
assert(state.save_uncertain and state.pending_operation and not queued_save,
  "uncertain replacement host published a queued capture")
assert(state.pending_operation.capture[1].text == "Latest title")
assert(vim.bo[retained_buffer].modified)
local original_attach = comments.attach
local protected_document = state.document
local protected_text = vim.api.nvim_buf_get_lines(retained_buffer, 0, -1, false)
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
assert(vim.wait(1000, function() return delayed ~= nil end))
delayed()
assert(vim.wait(1000, function() return conflict ~= nil end))
comments.attach = original_attach
assert(state.document == protected_document and state.replica.buffer == retained_buffer)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(retained_buffer, 0, -1, false), protected_text))
assert(vim.bo[retained_buffer].modified)
assert(not vim.wo[window].number and vim.wo[window].relativenumber)
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), protected_cursor))
print("review_rebind: current fields, raw local comments, retained buffer, and baseline conflicts passed")
