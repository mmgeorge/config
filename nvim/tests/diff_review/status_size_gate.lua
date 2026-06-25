package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local diff_review = require("diff_review")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

-- The viewport/lazy virtualization was removed; the per-file size gate is the only
-- big-diff bounding mechanism. These assert the pure gate decision + the prewarm cap.
local function assert_size_gate_decision()
  local defer = diff_review._status_size_gate_should_defer
  assert_true(defer(0, 9999, 1, 0, 40) == false, "size gate should never defer the first hunk")
  assert_true(defer(10, 10, 2, 0, 40) == false, "size gate should keep rendering while under budget")
  assert_true(defer(10, 9000, 2, 0, 40) == true, "size gate should defer a hunk that would overshoot the budget")
  assert_true(defer(40, 1, 5, 0, 40) == true, "size gate should defer once the budget is reached")
  assert_true(defer(9000, 9000, 5, 8, 40) == false, "size gate should render force-loaded hunks past the budget")
  assert_true(defer(9000, 9000, 5, 0, nil) == false, "size gate should be disabled when the budget is nil")
  assert_true(diff_review._status_file_render_row_budget() ~= nil, "size gate budget should follow the configured threshold")
end

local function assert_prewarm_hunk_budget()
  local capped = diff_review._status_prewarm_hunk_budget(273, { status_cursor_prewarm_max_hunks = 12 })
  assert_true(capped == 12, "large-file cursor prewarm should cap warmed hunks at the configured budget")
  local small = diff_review._status_prewarm_hunk_budget(5, { status_cursor_prewarm_max_hunks = 12 })
  assert_true(small == 5, "small files should warm every hunk")
  local disabled = diff_review._status_prewarm_hunk_budget(273, { status_cursor_prewarm_max_hunks = 0 })
  assert_true(disabled == 0, "a zero budget should disable cursor-driven file prewarm")
end

local function run()
  assert_size_gate_decision()
  assert_prewarm_hunk_budget()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
