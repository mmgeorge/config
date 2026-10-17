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
selected[#selected + 1] = test("src/first.rs", "first", "new")
selected[#selected + 1] = test("src/first.rs", "second", "modified")
source[#source + 1] = { block = "plan:tests:file:second", text = "New tests/second.rs", test_file = "tests/second.rs" }
local retained = test("tests/second.rs", "keep", "new")
selected[#selected + 1] = test("tests/second.rs", "third", "removed")
selected[#selected + 1] = test("tests/second.rs", "fourth", "reused")
for index, row in ipairs(source) do row.source_line = index row.position = { row = 0, column = 0 } end
local baseline = vim.deepcopy(source)
local projected = preview.project(source, selected)
assert(vim.deep_equal(source, baseline), "preview mutated the canonical source")
assert(projected[2].text == " Tests · 1 new ", projected[2].text)
assert(projected[3].hidden and not projected[1].hidden, "empty file removal hid unrelated text")
for index, row in ipairs(projected) do
  assert(row.source_line == source[index].source_line and row.block == source[index].block, "preview changed source identity")
  if row.test and row.test.name ~= "keep" then assert(row.hidden, "deleted test description remained visible") end
end
selected[#selected + 1] = retained
projected = preview.project(source, selected)
assert(projected[2].text == " Tests · None planned ", projected[2].text)
assert(projected[8].hidden, "last test deletion retained its empty file heading")

local folded = vim.deepcopy(source)
folded[2].test_summary.reused = 8
local with_folded = preview.project(folded, selected)
assert(with_folded[2].text == " Tests · 7 reused ", "deletion lost tests inside collapsed files")

local rename = require("forge.views.plan_review.entity_rename")
local popup = require("forge.infra.popup_window")
local native = vim.api.nvim_create_buf(false, true)
vim.api.nvim_win_set_buf(0, native)
vim.api.nvim_buf_set_lines(native, 0, -1, false, { "Original", "use Original" })
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
assert(vim.api.nvim_buf_get_lines(native, 0, 1, false)[1] == "Original", "preview changed canonical bytes")
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

local notices = {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
rename.proposed(review, selection, {}, commit)
input.on_change("Stale")
current = false
answer("Stale")
assert(not committed and #marks() == 0 and notices[#notices]:find("Plan changed", 1, true))
current = true
rename.proposed(review, selection, {}, function() error("submission failed") end)
input.on_change("Broken")
answer("Broken")
assert(#marks() == 0 and notices[#notices]:find("submission failed", 1, true), "submission error stranded preview")
rename.proposed(review, selection, {}, commit)
input.on_change("Pending")
answer("Pending")
assert(#marks() == 2)
vim.api.nvim_buf_delete(native, { force = true })
finish()
assert(vim.wo[window].conceallevel ~= 2 and vim.wo[window].concealcursor ~= "nvic", "buffer wipe stranded conceal options")
print("plan_optimistic_edit passed")
vim.cmd("qa!")
