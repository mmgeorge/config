local render_orchestrator = require("diff_review.views.status.render_orchestrator")
local status_render = require("diff_review.views.status.status_render")
local git_data = require("diff_review.git.git_data")
local file_body = require("diff_review.git.file_body")
local syntax_engine = require("diff_review.render.syntax_engine")
local session = require("diff_review.session")

local function assert_true(value, message)
  if not value then error(message or "assertion failed", 2) end
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10)
  assert_true(ok, message)
end

local function context_result()
  return {
    label = "fn example",
    start_row = 0,
    end_row = 2,
    start_text = "fn example() {",
    end_text = "}",
    start_segments = { { text = "fn example() {", hl_group = "Function" } },
    end_segments = { { text = "}", hl_group = "Delimiter" } },
    path_start_rows = {},
    path_end_rows = {},
    sibling_before_rows = {},
    sibling_after_rows = {},
    ancestor_boundaries = {},
  }
end

local function with_stubs(fn)
  local original_diff_hunks_for_file = render_orchestrator.diff_hunks_for_file
  local original_compute_hunk_context_async = git_data.compute_hunk_context_async
  local pending_context_callbacks = {}
  render_orchestrator.diff_hunks_for_file = function(file)
    return {
      {
        pos = 1,
        old_start = 1,
        old_count = 1,
        new_start = 1,
        new_count = 1,
        added = 1,
        removed = 1,
        staged = false,
        diff = ("@@ -1 +1 @@\n-old\n+new\n"),
        file = file.filename,
      },
    }
  end
  git_data.compute_hunk_context_async = function(filename, line, cb)
    pending_context_callbacks[#pending_context_callbacks + 1] = {
      filename = filename,
      line = line,
      cb = cb,
    }
  end
  syntax_engine.clear_context_cache()
  local ok, err = pcall(fn, pending_context_callbacks)
  syntax_engine.clear_context_cache()
  render_orchestrator.diff_hunks_for_file = original_diff_hunks_for_file
  git_data.compute_hunk_context_async = original_compute_hunk_context_async
  if not ok then error(err, 0) end
end

local function run()
  with_stubs(function(pending_context_callbacks)
    local buf = vim.api.nvim_create_buf(false, true)
    local state = { buf = buf, view_kind = "status" }
    session.status = state
    session.states = session.states or {}
    session.states[buf] = state
    local ready_count = 0
    local entry = {
      id = "file:unstaged:D:/tmp/context.rs",
      kind = "file",
      file = {
        filename = "D:/tmp/context.rs",
        relpath = "context.rs",
        section_name = "unstaged",
      },
    }

    local deferred = status_render.status_prepare_file_expansion_context(entry, state, function()
      ready_count = ready_count + 1
    end)

    assert_true(deferred == true, "file expansion should defer while context is pending")
    assert_true(ready_count == 0, "file expansion opened before context finished")
    assert_true(#pending_context_callbacks > 0, "file expansion did not request hunk context")

    for _, request in ipairs(pending_context_callbacks) do
      request.cb(context_result())
    end

    wait_for(function()
      return ready_count == 1
    end, "file expansion did not continue after context finished")

    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  local original_diff_hunks_for_file = render_orchestrator.diff_hunks_for_file
  local original_load_async = file_body.load_async
  local pending_body_callback = nil
  local load_count = 0
  render_orchestrator.diff_hunks_for_file = function(file) return file.hunks or {} end
  file_body.load_async = function(_, _, callback)
    load_count = load_count + 1
    pending_body_callback = callback
  end

  local buf = vim.api.nvim_create_buf(false, true)
  local file = {
    filename = "D:/tmp/added.rs",
    relpath = "added.rs",
    section_name = "staged",
    added = 1,
    removed = 0,
    hunks = {},
    untracked = false,
    status = "A.",
    git_status = "A",
    preview_state = "unloaded",
    preview_source = "index_added",
    line_stats_complete = true,
  }
  local state = {
    buf = buf,
    cwd = "D:/tmp",
    view_kind = "status",
    sections = { { files = { file } } },
  }
  session.status = state
  session.states[buf] = state
  session.file_diffs = {}
  session.file_hunk_staged = {}
  local ready_count = 0
  local entry = { id = "file:staged:D:/tmp/added.rs", kind = "file", file = file }

  assert_true(status_render.status_prepare_file_expansion_context(entry, state, function() ready_count = ready_count + 1 end), "lazy body load did not defer expansion")
  assert_true(status_render.status_prepare_file_expansion_context(entry, state, function() ready_count = ready_count + 1 end), "duplicate lazy body load did not remain deferred")
  assert_true(load_count == 1, "duplicate expansion started more than one body load")
  pending_body_callback({
    state = "loaded",
    hunks = {},
    diff = false,
    added = 2,
    removed = 0,
    line_stats_complete = true,
    binary = false,
  })
  wait_for(function() return ready_count == 1 end, "latest expansion did not resume after lazy body load")
  assert_true(file.preview_state == "loaded", "lazy body result did not update the canonical file")
  assert_true(file.added == 2, "lazy body result did not replace compact startup stats")

  vim.api.nvim_buf_delete(buf, { force = true })
  render_orchestrator.diff_hunks_for_file = original_diff_hunks_for_file
  file_body.load_async = original_load_async
end

run()
vim.cmd("qa!")
