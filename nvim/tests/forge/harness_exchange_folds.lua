vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local replica = buffer.open("exchange-folds", {})

---@param revision integer
---@param closed boolean
local function apply(revision, closed)
  local captured = folds.capture(replica)
  local result = buffer.apply_snapshot(replica, {
    document = replica.document,
    revision = revision,
    block = { {
      id = "exchange-summary",
      text = { "Thought for 2s", "Inspecting files", "Ran cargo test" },
      metadata = { target = {}, decoration = {}, editable_region = {}, fold = { {
        id = "exchange",
        start = { row = 0, column = 0 },
        ["end"] = { block = "exchange-summary", position = { row = 3, column = 0 } },
        closed = closed,
      } } },
    } },
  })
  assert(result.kind == "Applied")
  folds.restore(replica, captured)
end

apply(0, false)
vim.api.nvim_win_set_buf(0, replica.buffer)
folds.attach(replica, vim.api.nvim_get_current_win())
assert(vim.fn.foldclosed(1) == -1, "running exchange must open automatically")
apply(1, true)
assert(vim.fn.foldclosed(1) == 1, "completed exchange must close automatically")
vim.cmd("1foldopen")
apply(2, true)
assert(vim.fn.foldclosed(1) == -1, "explicitly opened exchange must stay open")
apply(3, false)
apply(4, true)
assert(vim.fn.foldclosed(1) == -1, "user preference must survive changed defaults")
folds.detach(replica)

local transcript_window = vim.api.nvim_get_current_win()
folds.attach(replica, transcript_window)
vim.cmd("belowright new")
local composer_window = vim.api.nvim_get_current_win()
apply(5, false)
apply(6, true)
vim.api.nvim_win_call(transcript_window, function()
  assert(vim.fn.foldlevel(1) > 0, "background transcript must retain native fold ranges")
end)
assert(vim.api.nvim_get_current_win() == composer_window, "fold refresh moved composer focus")
folds.detach(replica)
print("exchange automatic and explicit folds passed")
vim.cmd("qa!")
