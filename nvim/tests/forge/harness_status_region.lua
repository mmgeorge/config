vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local layout = require("forge.views.harness.layout")
local region = require("forge.views.harness.status_region")
local document = buffer.open("status-region", {})
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, document.buffer)
layout.configure_transcript_window(window)
vim.cmd("belowright 3new")
local composer = vim.api.nvim_get_current_win()
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusRegion")
local revision = -1

local function commit(history, status, staging)
  revision = revision + 1
  local function metadata()
    return { target = {}, decoration = {}, editable_region = {}, fold = {} }
  end
  local status_metadata = metadata()
  status_metadata.status = { row = 1, animated = status[2] ~= "Idle" }
  local blocks = { { id = "history", text = history, metadata = metadata() }, {
    id = "phase", text = status, metadata = status_metadata,
  } }
  if staging then blocks[#blocks + 1] = { id = "harness:staging", text = staging, metadata = metadata() } end
  assert(buffer.apply_snapshot(document, { document = document.document, revision = revision, block = blocks }).kind == "Applied")
  region.align(document, window)
end

local function assert_bottom()
  local height = vim.api.nvim_win_get_height(window) - (vim.wo[window].winbar ~= "" and 1 or 0)
  local measured = vim.api.nvim_win_text_height(window, { start_row = 0 }).all
  assert(measured == height, ("status region used %d rows in a %d-row pane"):format(measured, height))
  assert(vim.api.nvim_get_current_win() == composer, "status alignment stole composer focus")
end

commit({ "short response" }, { "", "Idle" })
assert_bottom()
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(document.buffer, 0, -1, false), { "short response", "", "Idle" }),
  "viewport padding became searchable document text")
local initial_padding = vim.api.nvim_buf_get_extmark_by_id(document.buffer, namespace, 1, { details = true })
region.align(document, window)
assert(vim.deep_equal(initial_padding, vim.api.nvim_buf_get_extmark_by_id(document.buffer, namespace, 1, { details = true })),
  "repeated refresh accumulated spacing")

commit({ "short response" }, { "", "Working · 2s", "Checks running · 0/1 finished", "  Cargo tests · 1s" },
  { "", "Queued follow-up inputs", "• Keep the full queue visible", "• " .. string.rep("wrapped follow-up ", 10) })
assert_bottom()
local status_row = region.location(document)
assert(vim.api.nvim_buf_get_lines(document.buffer, status_row, status_row + 1, false)[1] == "Working · 2s")
assert(vim.api.nvim_buf_get_lines(document.buffer, -2, -1, false)[1]:find("wrapped follow-up", 1, true))
vim.api.nvim_win_set_height(composer, 7)
region.align(document, window)
assert_bottom()
vim.wo[window].winbar = "Harness"
region.align(document, window)
assert_bottom()
vim.wo[window].winbar = ""

local history = {}
for index = 1, 100 do history[index] = "history " .. index end
commit(history, { "", "Working · 3s" }, { "", "Queued follow-up inputs", "• Later" })
assert(#vim.api.nvim_buf_get_extmarks(document.buffer, namespace, 0, -1, {}) == 0,
  "long transcripts retained unnecessary padding")
vim.api.nvim_win_set_cursor(window, { 12, 3 })
vim.api.nvim_win_call(window, function() vim.cmd("normal! zt") end)
local before = vim.api.nvim_win_call(window, vim.fn.winsaveview)
region.align(document, window)
assert(vim.deep_equal(before, vim.api.nvim_win_call(window, vim.fn.winsaveview)),
  "status alignment moved a reader's cursor or viewport")

commit({ "final response" }, { "", "Idle" })
assert_bottom()
region.clear(document.buffer)
assert(#vim.api.nvim_buf_get_extmarks(document.buffer, namespace, 0, -1, {}) == 0,
  "closing retained status spacing")
buffer.close(document)
print("harness_status_region: passed")
vim.cmd("qa!")
