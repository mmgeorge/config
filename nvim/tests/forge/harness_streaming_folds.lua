vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local replica = buffer.open("streaming-exchange", {})
vim.api.nvim_set_current_buf(replica.buffer)
local transcript_window = vim.api.nvim_get_current_win()
folds.attach(replica, transcript_window)
assert(buffer.apply_snapshot(replica, { document = replica.document, revision = 0, block = {} }).kind == "Applied")
vim.cmd("belowright new")
local composer_window = vim.api.nvim_get_current_win()
vim.cmd("startinsert")

vim.defer_fn(function()
  local ok, failure = xpcall(function()
    assert(vim.api.nvim_get_mode().mode == "i", "streaming fixture must run with the composer in Insert mode")
    local metadata = { target = {}, decoration = {}, editable_region = {}, fold = { {
      id = "exchange", start = { row = 0, column = 0 },
      ["end"] = { block = "summary", position = { row = 3, column = 0 } }, closed = false,
    } } }
    local patch = {
      document = replica.document, base = 0, next = 1, base_rows = 0, next_rows = 3,
      base_blocks = 0, next_blocks = 1, removed_block = {},
      block_edit = { { start_block = 0, removed_blocks = 0, inserted = { "summary" } } },
      text_edit = { { start_row = 0, removed_rows = 0, text = { "Thinking", "tool", "output" } } },
      metadata_edit = { { block = "summary", row_count = 3, metadata = metadata } },
    }
    assert(buffer.apply_patch(replica, patch).kind == "Applied")
    vim.api.nvim_win_call(transcript_window, function()
      assert(vim.fn.foldlevel(1) == 1, "streaming activity has no native fold")
      assert(vim.fn.foldclosed(1) == -1, "running exchange must remain expanded")
    end)
    metadata.fold[1].closed = true
    patch.base, patch.next, patch.base_rows, patch.base_blocks = 1, 2, 3, 1
    patch.block_edit = {}
    patch.text_edit = { { start_row = 0, removed_rows = 1, text = { "Thought" } } }
    assert(buffer.apply_patch(replica, patch).kind == "Applied")
    vim.api.nvim_win_call(transcript_window, function()
      assert(vim.fn.foldclosed(1) == 1, "completed activity must close while the composer retains Insert mode")
      vim.cmd("1foldopen")
    end)
    patch.base, patch.next = 2, 3
    patch.text_edit = { { start_row = 2, removed_rows = 1, text = { "updated output" } } }
    assert(buffer.apply_patch(replica, patch).kind == "Applied")
    vim.api.nvim_win_call(transcript_window, function()
      assert(vim.fn.foldclosed(1) == -1, "streaming refresh lost an explicitly expanded activity")
    end)
    assert(vim.api.nvim_get_current_win() == composer_window, "fold refresh moved composer focus")
    assert(vim.api.nvim_get_mode().mode == "i", "fold refresh ended composer insertion")
  end, debug.traceback)
  if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
  print("harness streaming folds passed")
  vim.cmd("qa!")
end, 50)
