package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local annotation_model = require("diff_review.render.annotations")
local hunk_index = require("diff_review.render.hunk_index")
local layout = require("diff_review.render.layout")
local mutation_queue = require("diff_review.render.mutation_queue")
local row_tree = require("diff_review.render.row_tree")
local source_model = require("diff_review.render.source")
local source_loader = require("diff_review.render.source_loader")
local syntax_context = require("diff_review.render.syntax_context")
local text_snapshot = require("diff_review.render.text_snapshot")
local decoration = require("diff_review.render.decoration")
local view_command_set = require("diff_review.shared.view_command_set")
local view_controller = require("diff_review.shared.view_controller")
local region_model = require("diff_review.render.region")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error((message or "values were not equal") .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual), 2)
  end
end

local function assert_text_snapshot()
  local snapshot = text_snapshot.from_text("one\r\ntwo\nthree\n")
  assert_equal(snapshot.line_count, 3, "snapshot line count")
  assert_equal(text_snapshot.line_text(snapshot, 1), "one", "CRLF line should strip carriage return")
  assert_equal(text_snapshot.line_text(snapshot, 2), "two", "LF line")
  assert_equal(text_snapshot.line_text(snapshot, 3), "three", "trailing newline should not add an extra row")

  local empty_line = text_snapshot.from_text("\n")
  assert_equal(empty_line.line_count, 1, "single newline should represent one empty source line")
  assert_equal(text_snapshot.line_text(empty_line, 1), "", "empty source line")
end

local function assert_row_index()
  local index = layout.new_row_height_index({ 1, 4, 0, 2 })
  assert_equal(index.total, 7, "initial total height")
  assert_equal(layout.item_at_row(index, 1), 1, "first row item")
  assert_equal(layout.item_at_row(index, 3), 2, "middle row item")
  assert_equal(layout.item_at_row(index, 7), 4, "last row item")
  assert_equal(layout.row_start_for_item(index, 4), 6, "row start")
  layout.set_height(index, 2, 1)
  assert_equal(index.total, 4, "updated total height")
  assert_equal(layout.item_at_row(index, 3), 4, "zero-height item should not be selected")

  local view_layout = layout.new_view_layout({ "a", "b", "c" }, { 1, 4, 2 })
  assert_equal(view_layout.span_by_key.c.start_row, 6, "initial span start")
  layout.set_span_height(view_layout, "b", 2)
  assert_equal(view_layout.row_index.total, 5, "targeted layout total")
  assert_equal(view_layout.span_by_key.b.height, 2, "targeted span height")
  assert_equal(view_layout.span_by_key.c.start_row, 4, "following span should shift by delta")
  assert_equal(layout.item_index(view_layout, "c"), 3, "layout key index")

  local body_layout = layout.new_file_body_layout({
    { key = "hunk:1", kind = "hunk", height = 3, metadata = { label = "one" } },
    { key = "annotation:1", kind = "annotation", height = 2 },
    { key = "hunk:2", kind = "hunk", height = 4 },
  })
  assert_equal(body_layout.view.row_index.total, 9, "body layout total")
  assert_equal(body_layout.block_by_key["hunk:1"].metadata.label, "one", "body block metadata")
  layout.set_block_height(body_layout, "annotation:1", 1)
  assert_equal(body_layout.view.row_index.total, 8, "body layout targeted height")
  local spans = layout.body_spans_in_row_range(body_layout, 2, 4)
  assert_equal(#spans, 2, "body range should intersect two blocks")
  assert_equal(spans[1].key, "hunk:1", "first intersected body span")
  assert_equal(spans[2].key, "annotation:1", "second intersected body span")
end

local function assert_source_state()
  local registry = source_model.new_registry()
  source_model.set_kind_policy(registry, "commit", { jump = true })
  local source = source_model.ensure_source(registry, {
    id = "commit:abc123",
    kind = "commit",
    lazy = true,
    metadata = { subject = "feat: test" },
  })
  assert_true(not source.loaded, "lazy source should start unloaded")
  assert_equal(source.metadata.subject, "feat: test", "source metadata should initialize from handle")
  assert_true(source_model.policy(registry, "commit:abc123").jump, "kind policy should resolve")

  local file = source_model.ensure_file(source, "src\\main.lua", {
    stage_state = "readonly",
    added = 2,
  })
  assert_equal(file.key, "commit:abc123\0src/main.lua", "file key should include normalized path")
  source_model.set_text(file, "new", "alpha\nbeta", "abc123")
  source_model.add_raw_hunk(file, {
    id = "hunk:1",
    diff = "@@ -0,0 +1,2 @@\n+alpha\n+beta",
    patch_text = "@@ -0,0 +1,2 @@\n+alpha\n+beta",
    old_start = 0,
    old_count = 0,
    new_start = 1,
    new_count = 2,
    added = 2,
    removed = 0,
  })
  assert_equal(file.new_text.line_count, 2, "new text snapshot")
  assert_equal(#file.hunks, 1, "raw hunk storage")
  source_model.invalidate_paths(registry, "commit:abc123", "src/main.lua")
  assert_equal(#source_model.invalidated_paths(registry, "commit:abc123"), 1, "invalidated path count")
  assert_true(file.stale, "invalidated file should be stale")
  source_model.clear_invalidated_paths(registry, "commit:abc123")
  assert_equal(#source_model.invalidated_paths(registry, "commit:abc123"), 0, "invalidated paths should clear")

  local raw_hunks = source_model.raw_hunks_from_diff(
    "diff --git a/src/main.lua b/src/main.lua\n--- a/src/main.lua\n+++ b/src/main.lua\n@@ -1,1 +1,2 @@\n old\n+new",
    { id_prefix = "raw:test", staged = true }
  )
  assert_equal(#raw_hunks, 1, "raw hunk parse count")
  assert_equal(raw_hunks[1].old_start, 1, "raw old start")
  assert_equal(raw_hunks[1].new_count, 2, "raw new count")
  assert_equal(raw_hunks[1].added, 1, "raw added count")

  local chunk_lines = {
    "diff --git a/src/main.lua b/src/main.lua",
    "--- a/src/main.lua",
    "+++ b/src/main.lua",
    "@@ -1,40 +1,40 @@",
  }
  for index = 1, 40 do
    chunk_lines[#chunk_lines + 1] = "+" .. tostring(index)
  end
  local chunk = source_model.hunk_chunks({
    diff = table.concat(chunk_lines, "\n"),
    raw_hunks = raw_hunks,
  }, "hunk:key", 20)
  assert_true(#chunk >= 2, "small chunk size should split hunk")
  assert_true(chunk[2].hide_header == true, "later lazy chunks hide duplicate header")

  local deferred_hunk, _, syntax_offsets = source_model.deferred_hunk_chunk({
    diff = table.concat(chunk_lines, "\n"),
    raw_hunks = raw_hunks,
  }, "hunk:key", 21, 10, 2)
  assert_true(deferred_hunk ~= nil, "deferred chunk should materialize requested body slice")
  assert_equal(syntax_offsets.old, 0, "deferred additions should not advance old syntax row offset")
  assert_equal(syntax_offsets.new, 20, "deferred chunk should expose new syntax row offset")
  assert_true(deferred_hunk.diff:find("@@ %-1,0 %+21,10 @@", 1, false) ~= nil, "deferred chunk should preserve new line start")
  assert_true(deferred_hunk.diff:find("+21", 1, true) ~= nil, "deferred chunk should include first requested body row")
  assert_true(deferred_hunk.diff:find("+30", 1, true) ~= nil, "deferred chunk should include last requested body row")
  assert_true(deferred_hunk.diff:find("+31", 1, true) == nil, "deferred chunk should stop at requested body count")

  local indexed = hunk_index.ensure({
    diff = table.concat(chunk_lines, "\n"),
    raw_hunks = raw_hunks,
  })
  local indexed_hunk, _, indexed_offsets = hunk_index.chunk(indexed, "hunk:key", 21, 10, 2)
  assert_true(indexed_hunk ~= nil, "indexed hunk should materialize requested body slice")
  assert_equal(indexed_offsets.new, 20, "indexed chunk should expose new syntax offset")
  assert_true(indexed_hunk.diff:find("+30", 1, true) ~= nil, "indexed chunk should include requested rows")
end

local function assert_source_loader()
  local registry = source_model.new_registry()
  local file = source_loader.ensure_file(registry, {
    id = "commit:def456",
    kind = "commit",
    lazy = true,
  }, "src\\lazy.lua", {
    stage_state = "readonly",
  })
  assert_equal(file.key, "commit:def456\0src/lazy.lua", "source loader should normalize file key")
  source_loader.replace_file_hunks(file, "diff --git a/src/lazy.lua b/src/lazy.lua\n--- a/src/lazy.lua\n+++ b/src/lazy.lua\n@@ -0,0 +1,1 @@\n+lazy", {
    id_prefix = "lazy",
  })
  assert_equal(#file.hunks, 1, "source loader should replace raw hunks")
  assert_true(file.hunk_index_by_id["lazy:1"] ~= nil, "source loader should create hunk index")
end

local function assert_text_loader()
  local registry = source_model.new_registry()
  local source = source_model.ensure_source(registry, { id = "pr:1", kind = "pr" })
  local file = source_model.ensure_file(source, "src/main.lua")
  local load_count = 0
  source_model.set_text_loader(file, "new", function(done)
    load_count = load_count + 1
    done(true, "one\ntwo\n", "rev-a")
  end)
  local first_line = nil
  source_model.ensure_text(file, "new", function(ok, snapshot)
    assert_true(ok, "text loader should succeed")
    first_line = text_snapshot.line_text(snapshot, 1)
  end)
  assert_equal(first_line, "one", "loaded text snapshot line")
  source_model.ensure_text(file, "new", function(ok, snapshot)
    assert_true(ok, "cached text should succeed")
    assert_equal(text_snapshot.line_text(snapshot, 2), "two", "cached text snapshot line")
  end)
  assert_equal(load_count, 1, "text loader should cache snapshot")
end

local function assert_source_path_reload()
  local registry = source_model.new_registry()
  local reloaded_paths = nil
  local source = source_model.ensure_source(registry, {
    id = "unstaged",
    kind = "unstaged",
    reload_paths = function(reload_source, paths, done)
      reloaded_paths = vim.deepcopy(paths)
      local file = source_model.ensure_file(reload_source, "src/main.lua", { stage_state = "unstaged" })
      file.pending = false
      file.stale = false
      done(true)
    end,
  })
  local file = source_model.ensure_file(source, "src/main.lua", { stage_state = "unstaged" })
  local callback_ok = nil
  source_model.reload_paths(registry, "unstaged", "src\\main.lua", function(ok)
    callback_ok = ok
  end)
  assert_true(callback_ok == true, "reload callback should succeed")
  assert_equal(reloaded_paths[1], "src/main.lua", "reload path should normalize")
  assert_true(not file.pending, "reloaded file should not be pending")
  assert_true(not file.stale, "reloaded file should not be stale")
end

local function assert_mutation_queue()
  local queue = mutation_queue.new()
  local event = {}
  mutation_queue.enqueue(queue, function(done)
    event[#event + 1] = "first"
    done()
  end)
  mutation_queue.enqueue(queue, function(done)
    event[#event + 1] = "second"
    done()
  end)
  mutation_queue.on_idle(queue, function()
    event[#event + 1] = "idle"
  end)
  assert_equal(table.concat(event, ","), "first,second,idle", "mutation queue should run FIFO then idle")
  assert_true(not mutation_queue.pending(queue), "queue should be idle")
end

local function assert_annotation_index()
  local index = annotation_model.new_index()
  annotation_model.upsert(index, {
    id = "comment-1",
    path = "src\\main.lua",
    side = "RIGHT",
    line = 12,
    body = "initial",
    editable = true,
    state = "clean",
  })
  assert_equal(#annotation_model.by_anchor(index, "src/main.lua", "RIGHT", 12), 1, "anchor lookup")
  assert_equal(#annotation_model.dirty(index), 0, "clean annotation should not be dirty")
  local draft = annotation_model.upsert(index, {
    id = "comment-2",
    path = "src/main.lua",
    side = "new",
    line = 13,
    body = "draft",
    editable = true,
    state = "new",
  })
  assert_equal(draft.side, "RIGHT", "new side should normalize to RIGHT")
  assert_equal(#annotation_model.by_anchor(index, "src/main.lua", "RIGHT", 13), 1, "normalized side anchor lookup")
  assert_equal(#annotation_model.dirty(index), 1, "new annotation should require sync")
  annotation_model.upsert(index, {
    id = "comment-3",
    path = "src/main.lua",
    side = "RIGHT",
    line = 14,
    body = "",
    editable = true,
    state = "deleted",
  })
  assert_equal(#annotation_model.dirty(index), 2, "deleted annotation should require sync")
  annotation_model.mark_dirty(index, "comment-1", annotation_model.body_from_buffer_lines({ "line one\r", "line two" }))
  local dirty = annotation_model.dirty(index)
  assert_equal(#dirty, 3, "dirty annotation count")
  assert_equal(index.by_id["comment-1"].body, "line one\nline two", "dirty body should normalize carriage returns")
  annotation_model.replace_body(index, "comment-1", { "edited\r", "body" })
  assert_equal(index.by_id["comment-1"].body, "edited\nbody", "replace body should normalize carriage returns")
  annotation_model.mark_clean(index, "comment-1")
  assert_equal(#annotation_model.dirty(index), 2, "clean annotation count")
end

local function assert_row_tree()
  local tree = row_tree.from_status_state({
    lines = { "head", "file", "tail" },
    entries = {
      [2] = { id = "file:one", kind = "file" },
    },
    highlights = {
      { line = 2, start_col = 0, end_col = 4, hl_group = "Title" },
    },
    content_lengths = {
      [2] = 4,
    },
  })
  assert_equal(tree.layout.row_index.total, 3, "row tree total")
  assert_true(row_tree.span(tree, "file:one") ~= nil, "row tree should index entry ids")
  row_tree.set_row_count(tree, "file:one", 5)
  assert_equal(tree.layout.row_index.total, 7, "row tree targeted height update")
  local span_list = row_tree.spans_in_range(tree, 2, 4)
  assert_equal(span_list[1].key, "file:one", "row tree range should find expanded span")
end

local function assert_syntax_context()
  local context = syntax_context.new("src/main.lua")
  local snapshot = text_snapshot.from_text("local value = 1")
  syntax_context.set_snapshot(context, "new", snapshot, "worktree")
  assert_true(not syntax_context.has_tree(context, "new"), "syntax context should start without parsed tree")
  assert_equal(context.side_by_name.new.revision, "worktree", "syntax context should retain side revision")
end

local function assert_decoration()
  local cache = decoration.new_cache()
  assert_true(decoration.cache_get(cache, "f", 1, 10) == nil, "empty cache should miss")
  assert_equal(decoration.cache_stats(cache).misses, 1, "miss should be counted")
  decoration.cache_put(cache, "f", 1, 10, { highlights = { { 0, 4, "Keyword" } } })
  local spans = decoration.cache_get(cache, "f", 1, 10)
  assert_true(spans ~= nil and spans.highlights[1][3] == "Keyword", "cached spans should return")
  assert_equal(decoration.cache_stats(cache).hits, 1, "hit should be counted")
  decoration.cache_put(cache, "f", 2, 10, { highlights = {} })
  assert_true(decoration.cache_get(cache, "f", 1, 10) == nil, "newer revision should evict stale revision")
  decoration.cache_invalidate(cache, "f")
  assert_true(decoration.cache_get(cache, "f", 2, 10) == nil, "invalidate should clear the file cache")

  local provider = decoration.new_provider(7, 99, decoration.new_cache())
  decoration.set_visible_window(provider, 5, 200)
  assert_equal(provider.visible_top, 5, "visible top should be recorded")
  assert_equal(provider.visible_bottom, 200, "visible bottom should be recorded")

  local compute_calls = 0
  local function resolve(row)
    if row == 0 then return nil end
    return { file_key = "f", revision = 3, line = row, side = "new", kind = "diff_line_range" }
  end
  local function compute(request)
    compute_calls = compute_calls + 1
    return { highlights = { { 0, request.line, "Added" } } }
  end
  assert_true(decoration.decorate_row(provider, 0, resolve, compute) == nil, "unresolved row should decorate nothing")
  local first = decoration.decorate_row(provider, 12, resolve, compute)
  assert_true(first ~= nil and first.highlights[1][2] == 12, "cache miss should compute spans")
  assert_equal(compute_calls, 1, "compute should run once on a miss")
  local second = decoration.decorate_row(provider, 12, resolve, compute)
  assert_true(second ~= nil, "second decorate should hit the cache")
  assert_equal(compute_calls, 1, "compute should not re-run on a cache hit")
end

local function assert_syntax_highlights()
  local text = "local value = 1"
  local snapshot = text_snapshot.from_text(text)
  local context = syntax_context.new("hl-test")
  syntax_context.set_snapshot(context, "new", snapshot, "rev")
  assert_true(syntax_context.highlights(context, "new", 0, 0) == nil, "highlights should defer until a tree is attached")

  local parser_ok, parser = pcall(vim.treesitter.get_string_parser, text, "lua")
  if not (parser_ok and parser) then return end
  local tree = (parser:parse() or {})[1]
  local query_ok, query = pcall(vim.treesitter.query.get, "lua", "highlights")
  if not (tree and query_ok and query) then return end

  syntax_context.set_tree(context, "new", tree, query, text)
  local hl = syntax_context.highlights(context, "new", 0, 0)
  assert_true(hl ~= nil and hl[0] ~= nil, "highlights should return spans once the tree is ready")
  local spans = hl[0].highlights
  assert_true(#spans >= 1, "the 'local value = 1' line should have at least one highlight span")
  local covers_keyword = false
  for _, span in ipairs(spans) do
    if span[1] <= 0 and span[2] >= 5 and type(span[3]) == "string" and span[3]:sub(1, 1) == "@" then
      covers_keyword = true
    end
  end
  assert_true(covers_keyword, "a highlight span should cover the 'local' keyword (cols 0..5)")
  assert_true(syntax_context.highlights(context, "old", 0, 0) == nil, "the unparsed old side should still defer")
end

local function assert_view_command_set()
  local set = view_command_set.new()
  local ran = nil
  view_command_set.register(set, "stage", function(context) ran = context.entry end)
  view_command_set.register(set, "discard", function() end, { enabled = function(context) return context.allowed == true end })
  assert_equal(#view_command_set.command_ids(set), 2, "command set should track registered ids in order")
  assert_true(view_command_set.dispatch(set, "stage", { entry = "hunk-1" }), "known command should dispatch")
  assert_equal(ran, "hunk-1", "dispatch should pass context to the action")
  assert_true(view_command_set.dispatch(set, "missing", {}) == false, "unknown command should not dispatch")
  assert_true(view_command_set.dispatch(set, "discard", { allowed = false }) == false, "disabled command should not dispatch")
  assert_true(view_command_set.dispatch(set, "discard", { allowed = true }), "enabled command should dispatch")
end

local function assert_view_controller()
  view_controller.reset()
  local rendered = nil
  view_controller.register(view_controller.new({
    view_kind = "status",
    after_render = function(state) rendered = state.buf end,
  }))
  assert_true(view_controller.resolve("status") ~= nil, "registered controller should resolve by view kind")
  assert_true(view_controller.resolve("pr") == nil, "unregistered view kind should resolve to nil")
  assert_true(view_controller.for_state({ view_kind = "status" }) ~= nil, "for_state should resolve from a state")
  assert_true(view_controller.run_hook("status", "after_render", { buf = 7 }), "defined hook should run")
  assert_equal(rendered, 7, "hook should receive the state")
  assert_true(view_controller.run_hook("status", "sources", {}) == false, "undefined hook should be a no-op")
  view_controller.reset()
  assert_true(view_controller.resolve("status") == nil, "reset should clear the registry")
end

local function assert_region()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "header", "body one", "body two", "footer" })
  local ns = vim.api.nvim_create_namespace("diff_review_region_test")
  local region = region_model.new(buf, ns, 1, 2, { region_kind = "comment_body", editable = true })
  assert_equal(region_model.read_text(region), "body one\nbody two", "region should read its anchored lines")
  assert_true(not region_model.is_dirty(region), "a fresh region should not be dirty")
  vim.api.nvim_buf_set_text(buf, 2, 8, 2, 8, { " edited" })
  assert_true(region_model.is_dirty(region), "an edited region should be dirty")
  assert_equal(region_model.read_text(region), "body one\nbody two edited", "region text should follow edits")
  vim.api.nvim_buf_set_lines(buf, 0, 0, false, { "new top" })
  local first_row = select(1, region_model.range(region))
  assert_equal(first_row, 2, "region start anchor should shift after an insert above")
  region_model.mark_saved(region)
  assert_true(not region_model.is_dirty(region), "mark_saved should reset the baseline")
  vim.api.nvim_buf_delete(buf, { force = true })
end

local function assert_annotation_sync_queue()
  local handler_done = nil
  local queue = annotation_model.new_sync_queue(function(_, done)
    handler_done = done
  end)
  local edited = { id = "c1", body = "edited", base_body = "old", remote_body = "old", state = "dirty" }
  annotation_model.enqueue_sync(queue, edited)
  assert_equal(edited.state, "syncing", "enqueue should move the annotation to syncing")
  assert_true(queue.running, "queue should run while a sync is in flight")
  handler_done(true, { id = "remote-1", url = "https://x/1" })
  assert_equal(edited.state, "clean", "successful sync should clean the annotation")
  assert_equal(edited.base_body, "edited", "success should adopt the synced body as base")
  assert_equal(edited.remote_body, "edited", "success should adopt the synced body as remote")
  assert_equal(edited.remote_id, "remote-1", "success should store the remote id")
  assert_equal(edited.remote_url, "https://x/1", "success should store the remote url")
  assert_true(not queue.running, "queue should idle after completion")

  local failing = { id = "c2", body = "draft", base_body = "", state = "new" }
  annotation_model.enqueue_sync(queue, failing)
  handler_done(false, nil, "boom")
  assert_equal(failing.state, "failed", "failed sync should mark the annotation failed")
  assert_equal(failing.sync_error, "boom", "failed sync should record the error")
  assert_equal(failing.base_body, "", "failed sync should keep the unsynced body")

  local stale = { id = "c3", body = "v1", state = "dirty" }
  annotation_model.enqueue_sync(queue, stale)
  stale.sync_operation_id = 999
  handler_done(true)
  assert_true(stale.state ~= "clean", "a stale completion should be ignored")
end

local function assert_walkthrough_source()
  local source = source_model.new_walkthrough_source({ id = "walkthrough:1", kind = "walkthrough" }, {
    base_source_ids = { "staged", "unstaged" },
    file_order = { "a.lua", "b.lua" },
  })
  assert_equal(source.handle.kind, "walkthrough", "walkthrough source should carry the walkthrough kind")
  assert_equal(#source.base_source_ids, 2, "walkthrough source should reference base sources without duplicating text")
  source_model.add_walkthrough_step(source, { id = "step-1", file = "a.lua" })
  source_model.add_walkthrough_step(source, { id = "step-2", file = "b.lua" })
  assert_equal(#source.step_annotations, 2, "walkthrough steps should be stored as annotations")
  assert_equal(source.navigation_index["step-2"], 2, "navigation index should map a step id to its position")
end

local function run()
  assert_decoration()
  assert_syntax_highlights()
  assert_view_command_set()
  assert_view_controller()
  assert_region()
  assert_annotation_sync_queue()
  assert_walkthrough_source()
  assert_text_snapshot()
  assert_row_index()
  assert_source_state()
  assert_source_loader()
  assert_text_loader()
  assert_source_path_reload()
  assert_mutation_queue()
  assert_annotation_index()
  assert_row_tree()
  assert_syntax_context()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
