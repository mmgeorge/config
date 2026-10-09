vim.loader.enable(false)
local client = require("forge.client")
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
local state = require("forge.session").harness
state.session = { id = "tests-session" }
state.transcript_win = vim.api.nvim_get_current_win()
local notices = {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "canonical fixture" }, path)
local version, pending = 1, nil
local tests = {
  { file = "src/game.rs", name = "first" }, { file = "src/game.rs", name = "second" },
  { file = "tests/game.rs", name = "first" }, { file = "tests/game.rs", name = "fourth" },
  { file = "tests/game.rs", name = "keep" },
}
package.loaded["forge.views.harness.controller"] = { refresh_winbar = function() end }
client.request_for = function(_, method, params, callback)
  if method == "plan.tests.delete" then pending = { params = params, callback = callback } return end
  assert(method == "harness.document")
  if params.operation ~= "plan_open" then callback({}) return end
  local source, blocks = {}, {}
  local function row(text, test)
    local id = "row:" .. #source
    local metadata = { decoration = {}, editable_region = {}, target = {} }
    source[#source + 1] = { id = id, text = text, test = test, source_line = #source + 1,
      block = id, position = { row = 0, column = 0 }, metadata = metadata }
    blocks[#blocks + 1] = { id = id, text = { text }, metadata = metadata }
  end
  row("Tests")
  local file
  for _, test in ipairs(tests) do
    if file ~= test.file then file = test.file row(file) end
    row("  " .. test.name, test)
    row("    Expected result", test)
  end
  callback({ version = version, saved_source_digest = "saved:" .. version, annotation = {}, source_row = source,
    snapshot = { document = params.document, revision = 0, block = blocks } })
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
  pending.callback(nil, "test deletion failed")
  pending = nil
  assert(not state.busy and review.owner.attached(), "failed deletion stranded the review")
  select(review)
  vim.fn.maparg("y", "n", false, true).callback()
  tests = { tests[5] }
  version = 2
  pending.callback({ plan = { id = "plan", state = "awaiting_review", document_version = version,
    working_path = path, review_digest = "digest:2" } })
  assert(vim.wait(1000, function() return state.plan_review and state.plan_review.owner.version == 2 end))
  assert(not state.busy)
  local rendered = table.concat(vim.api.nvim_buf_get_lines(state.plan_review.buf, 0, -1, false), "\n")
  assert(rendered:find("keep", 1, true) and not rendered:find("fourth", 1, true), rendered)
end, debug.traceback)
vim.fn.delete(path)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("plan_test_delete passed")
vim.cmd("qa!")
