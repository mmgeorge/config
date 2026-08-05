package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local diff_review = require("diff_review")
local entry_nav = require("diff_review.views.status.entry_nav")
local session = require("diff_review.session")
local syntax_engine = require("diff_review.render.syntax_engine")
local size_gate = require("diff_review.views.status.size_gate")
local status_render = require("diff_review.views.status.status_render")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_deleted_file_preview_gate()
  assert_true(
    size_gate._status_deleted_file_preview_omission({ git_status = "D", removed = 1000 }) == nil,
    "deleted files at the line limit should retain their preview"
  )
  assert_true(
    size_gate._status_deleted_file_preview_omission({ git_status = "D", removed = 1001 }) == 1001,
    "deleted files over the line limit should omit their preview"
  )
  assert_true(
    size_gate._status_deleted_file_preview_omission({ status = "removed", removed = 1001 }) == 1001,
    "provider deleted-file states should use the same preview gate"
  )
  assert_true(
    size_gate._status_deleted_file_preview_omission({ git_status = "M", removed = 1001 }) == nil,
    "large modified files should retain the normal size-gated preview"
  )
end

local function assert_cursor_file_prewarm_gate()
  local previous_status = session.status
  local original_file_prewarm = syntax_engine.prewarm_file_diff_syntax
  local original_hunk_prewarm = syntax_engine.prewarm_diff_syntax
  local prewarm_count = 0
  syntax_engine.prewarm_file_diff_syntax = function() prewarm_count = prewarm_count + 1 end
  syntax_engine.prewarm_diff_syntax = function() prewarm_count = prewarm_count + 1 end
  session.status = { folds = {} }

  local gate_ok, gate_error = xpcall(function()
    entry_nav._status_prewarm_entry_syntax({
      id = "file:deleted.txt",
      kind = "file",
      file = { filename = "deleted.txt", git_status = "D", added = 0, removed = 5 },
      default_folded = false,
    })
    assert_true(prewarm_count == 0, "expanded deleted files should never prewarm syntax")

    entry_nav._status_prewarm_entry_syntax({
      id = "hunk:deleted.txt:1",
      kind = "hunk",
      file = { filename = "deleted.txt", git_status = "D", added = 0, removed = 5 },
      hunk = { diff = "@@ -1 +0,0 @@\n-deleted", staged = false },
    })
    assert_true(prewarm_count == 0, "deleted-file hunk rows should never prewarm syntax")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:small.txt",
      kind = "file",
      file = { filename = "small.txt", git_status = "M", added = 60, removed = 39 },
      default_folded = true,
    })
    assert_true(prewarm_count == 1, "collapsed files below a 100-line delta should prewarm syntax")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:boundary.txt",
      kind = "file",
      file = { filename = "boundary.txt", git_status = "M", added = 60, removed = 40 },
      default_folded = true,
    })
    assert_true(prewarm_count == 1, "collapsed files at a 100-line delta should not prewarm syntax")

    session.status.folds["file:boundary.txt"] = false
    entry_nav._status_prewarm_entry_syntax({
      id = "file:boundary.txt",
      kind = "file",
      file = { filename = "boundary.txt", git_status = "M", added = 60, removed = 40 },
      default_folded = true,
    })
    assert_true(prewarm_count == 2, "expanded non-deleted files should prewarm past the delta limit")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:new.txt",
      kind = "file",
      file = { filename = "new.txt", git_status = "A", added = 99, removed = 0 },
      default_folded = true,
    })
    assert_true(prewarm_count == 3, "collapsed new files below 100 lines should prewarm syntax")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:new-boundary.txt",
      kind = "file",
      file = { filename = "new-boundary.txt", git_status = "A", added = 100, removed = 0 },
      default_folded = true,
    })
    assert_true(prewarm_count == 3, "collapsed new files at 100 lines should require expansion")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:new-lower-bound.txt",
      kind = "file",
      file = {
        filename = "new-lower-bound.txt",
        git_status = "A",
        added = 99,
        removed = 0,
        line_stats_complete = false,
      },
      default_folded = true,
    })
    assert_true(prewarm_count == 3, "collapsed new files with lower-bound stats should require expansion")

    entry_nav._status_prewarm_entry_syntax({
      id = "file:unknown.txt",
      kind = "file",
      file = { filename = "unknown.txt", git_status = "M" },
      default_folded = true,
    })
    assert_true(prewarm_count == 3, "collapsed files without known stats should require expansion")
  end, debug.traceback)

  session.status = previous_status
  syntax_engine.prewarm_file_diff_syntax = original_file_prewarm
  syntax_engine.prewarm_diff_syntax = original_hunk_prewarm
  if not gate_ok then error(gate_error, 0) end
end

local function assert_deleted_file_preview_render()
  local previous_status = session.status
  local file = {
    filename = "D:/repo/deleted.txt",
    relpath = "deleted.txt",
    section_name = "unstaged",
    added = 0,
    removed = 1001,
    hunks = { { diff = "must not render", staged = false, added = 0, removed = 1001 } },
    untracked = false,
    status = "D",
    git_status = "D",
  }
  session.status = {
    buf = nil,
    cwd = "D:/repo",
    view_kind = "status",
    lines = {},
    entries = {},
    highlights = {},
    line_highlights = {},
    extmarks = {},
    folds = {},
    fancy_rows = {},
  }
  local render_ok, render_error = xpcall(function()
    status_render.status_render_file(file, nil, nil, nil, nil, { force_open = true })
    assert_true(#session.status.lines == 2, "omitted deleted file should render only its header and omission row")
    assert_true(
      session.status.lines[2] == "Preview omitted — deleted file has 1001 lines",
      "deleted-file omission message mismatch: " .. tostring(session.status.lines[2])
    )
    assert_true(session.status.entries[1].preview_omitted == true, "deleted-file header should block cursor prewarm")
    assert_true(file.diff_source_id == nil, "omitted deleted file should not initialize diff source state")
  end, debug.traceback)
  session.status = previous_status
  if not render_ok then error(render_error, 0) end
end

-- The viewport/lazy virtualization was removed; the per-file size gate is the only
-- big-diff bounding mechanism. These assert the pure gate decision + the prewarm cap.
local function assert_size_gate_decision()
  local defer = size_gate._status_size_gate_should_defer
  assert_true(defer(0, 9999, 1, 0, 40) == false, "size gate should never defer the first hunk")
  assert_true(defer(10, 10, 2, 0, 40) == false, "size gate should keep rendering while under budget")
  assert_true(defer(10, 9000, 2, 0, 40) == true, "size gate should defer a hunk that would overshoot the budget")
  assert_true(defer(40, 1, 5, 0, 40) == true, "size gate should defer once the budget is reached")
  assert_true(defer(9000, 9000, 5, 8, 40) == false, "size gate should render force-loaded hunks past the budget")
  assert_true(defer(9000, 9000, 5, 0, nil) == false, "size gate should be disabled when the budget is nil")
  assert_true(size_gate._status_file_render_row_budget() ~= nil, "size gate budget should follow the configured threshold")
end

local function assert_prewarm_hunk_budget()
  local capped = syntax_engine.status_prewarm_hunk_budget(273, { status_cursor_prewarm_max_hunks = 12 })
  assert_true(capped == 12, "large-file cursor prewarm should cap warmed hunks at the configured budget")
  local small = syntax_engine.status_prewarm_hunk_budget(5, { status_cursor_prewarm_max_hunks = 12 })
  assert_true(small == 5, "small files should warm every hunk")
  local disabled = syntax_engine.status_prewarm_hunk_budget(273, { status_cursor_prewarm_max_hunks = 0 })
  assert_true(disabled == 0, "a zero budget should disable cursor-driven file prewarm")
end

local function run()
  assert_size_gate_decision()
  assert_deleted_file_preview_gate()
  assert_cursor_file_prewarm_gate()
  assert_deleted_file_preview_render()
  assert_prewarm_hunk_budget()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
