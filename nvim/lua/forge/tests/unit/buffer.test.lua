-- Unit tests for `forge.buffer` (buffer.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["lifecycle"] = function()
  local replica = require("forge.buffer")
  vim.v.errmsg = ""
  for _, external in ipairs({ true, false }) do
    local session = replica.open("wipe:" .. tostring(external))
    local called = false
    vim.api.nvim_create_autocmd("BufWipeout", { buffer = session.buffer, once = true, callback = function()
      called = true
      assert(session.status == "Closed", "owner callback ran before replica invalidation")
      replica.close(session)
    end })
    if external then
      vim.api.nvim_set_current_buf(session.buffer)
      vim.cmd("bwipeout!")
    else replica.close(session) end
    assert(called and session.status == "Closed" and not vim.api.nvim_buf_is_valid(session.buffer))
    assert(session.lifecycle_autocmd == nil)
  end
  for _, mode in ipairs({ "physical", "generated" }) do
    local physical = vim.api.nvim_create_buf(true, false)
    buffer_support.set(physical, { "Retained user text" })
    vim.b[physical].forge_native_document = "previous-owner"
    local options = { buffer = physical, [mode] = true,
      expected_changedtick = vim.api.nvim_buf_get_changedtick(physical) }
    local session = replica.open(mode, options)
    local lifecycle = session.lifecycle_autocmd
    replica.close(session)
    assert(vim.api.nvim_buf_is_valid(physical), mode .. " deleted caller-owned text")
    assert(buffer_support.line(physical, 1) == "Retained user text")
    assert(vim.b[physical].forge_native_document == "previous-owner")
    assert(session.lifecycle_autocmd == nil)
    for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ event = "BufWipeout", buffer = physical })) do
      assert(autocmd.id ~= lifecycle, "closed replica retained its lifecycle callback")
    end
    vim.api.nvim_buf_delete(physical, { force = true })
  end
  local invalidated = replica.open("generation")
  local lifecycle = invalidated.lifecycle_autocmd
  replica.invalidate(invalidated)
  assert(vim.api.nvim_buf_is_valid(invalidated.buffer) and invalidated.status == "Closed")
  assert(vim.b[invalidated.buffer].forge_native_document == nil and invalidated.lifecycle_autocmd == nil)
  for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ event = "BufWipeout", buffer = invalidated.buffer })) do
    assert(autocmd.id ~= lifecycle, "invalidation retained old generation callback")
  end
  local replacement = replica.open("replacement", { buffer = invalidated.buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(invalidated.buffer) })
  assert(replica.apply_snapshot(replacement, { document = "replacement", revision = 0, block = {
    { id = "body", text = { "Owned generated text" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }).kind == "Applied")
  replica.close(replacement)
  assert(not vim.api.nvim_buf_is_valid(replacement.buffer), "adopted generated buffer was not released")
  assert(vim.v.errmsg == "", vim.v.errmsg)
end

T["fold_failure_recovery"] = function()
  local buffer = require("forge.buffer")
  local notices, recovered = {}, 0
  local owner = buffer.open("projection-failure", {notice=function(value) notices[#notices+1]=value end,
    recover=function() recovered=recovered+1 end})
  vim.api.nvim_set_current_buf(owner.buffer)
  local source = { document=owner.document, revision=0, block={
    {id="body",text={"heading","body","tail"},metadata={target={},decoration={},editable_region={},fold={
      {id="section",start={row=0,column=0},["end"]={block="body",position={row=3,column=0}},closed=false}}}},
  }}
  assert(buffer.apply_snapshot(owner,source).kind=="Applied")
  local native = vim.api.nvim_buf_set_lines
  local injected = false
  vim.api.nvim_buf_set_lines = function(...)
    injected=true
    error("injected projection write failure")
  end
  assert(not buffer.set_expansion(owner,"section",false))
  vim.api.nvim_buf_set_lines=native
  assert(injected and owner.status=="Desynchronized")
  assert(not owner.applying and not vim.bo[owner.buffer].modifiable)
  assert(#notices==1 and recovered==1, "failure must report once and admit recovery")
  assert(owner.projection.choice.section==nil, "failed publication retained expansion intent")
  assert(buffer.apply_snapshot(owner,source).kind=="Applied")
  assert(owner.row_count==3)
  assert(buffer.set_expansion(owner,"section",false) and owner.row_count==1)
  assert(buffer.set_expansion(owner,"section",true) and owner.row_count==3)
  buffer.close(owner)
end

T["replica"] = function()
  local replica = require("forge.buffer")
  local editable = require("forge.editable")
  local recovery = 0
  local session = replica.open("document", { recover = function() recovery = recovery + 1 end })
  MiniTest.finally(function()
    pcall(function()
      editable.detach(session.editable)
      session.editable = editable.new(session.document)
      replica.close(session)
    end)
  end)

  local function block(id, text)
    return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {} } }
  end

  local snapshot = { document = "document", revision = 0, block = { block("first", { "one" }), block("last", { "last" }) } }
  snapshot.block[2].metadata.decoration = {
    { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 4 } }, capture = "String", priority = 10 },
  }
  local patch = {
    document = "document", base = 0, next = 1, base_rows = 2, next_rows = 3, base_blocks = 2, next_blocks = 2,
    text_edit = { { start_row = 0, removed_rows = 1, text = { "one", "new" } } },
    block_edit = {}, removed_block = {}, metadata_edit = {
      { block = "first", row_count = 2, metadata = block("first", {}).metadata },
    },
  }

  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  local handle = session.marks.last[1]
  assert(replica.apply_patch(session, patch).kind == "Applied")
  assert(session.revision == 1 and not vim.bo[session.buffer].modifiable)
  eq(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "one", "new", "last" })
  assert(session.marks.last[1] == handle, "unchanged decoration was recreated")
  assert(vim.api.nvim_buf_get_extmark_by_id(session.buffer, session.namespace, handle, {})[1] == 2)
  assert(replica.apply_patch(session, patch).kind == "Desynchronized")
  assert(session.revision == 1 and recovery == 1)
  snapshot.revision = 2
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")

  local two_edits = vim.deepcopy(patch)
  two_edits.base, two_edits.next = 2, 3
  two_edits.next_rows = 2
  two_edits.metadata_edit = {}
  two_edits.text_edit = {
    { start_row = 1, removed_rows = 1, text = { "tail" } },
    { start_row = 0, removed_rows = 1, text = { "head" } },
  }
  local native_set_text = vim.api.nvim_buf_set_text
  local calls = 0
  vim.api.nvim_buf_set_text = function(...)
    calls = calls + 1
    if calls == 2 then error("injected second-edit failure") end
    return native_set_text(...)
  end
  local result = replica.apply_patch(session, two_edits)
  vim.api.nvim_buf_set_text = native_set_text
  assert(result.kind == "Desynchronized" and result.diagnostic:find("injected second-edit failure", 1, true))
  assert(session.revision == 2, "partial application acknowledged target revision")
  eq(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "one", "tail" })
  assert(#vim.api.nvim_buf_get_extmarks(session.buffer, session.namespace, 0, -1, {}) == 0)
  snapshot.revision = 4
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  editable.register(session.editable, "body", 0)
  editable.record(session.editable, "body", { "unsaved" })
  assert(replica.apply_snapshot(session, snapshot).kind == "Deferred")
  assert(replica.apply_patch(session, two_edits).kind == "Deferred")
  assert(replica.close(session).kind == "Deferred")
  assert(vim.api.nvim_buf_is_valid(session.buffer))
  session.editable = editable.new("document")

  snapshot.revision, snapshot.block = 5, {}
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  local insertion = {
    document = "document", base = 5, next = 6, base_rows = 0, next_rows = 1, base_blocks = 0, next_blocks = 1,
    text_edit = { { start_row = 0, removed_rows = 0, text = { "first" } } },
    block_edit = { { start_block = 0, removed_blocks = 0, inserted = { "first" } } }, removed_block = {},
    metadata_edit = { { block = "first", row_count = 1, metadata = block("first", {}).metadata } },
  }
  assert(replica.apply_patch(session, insertion).kind == "Applied")
  eq(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "first" })
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_lines(session.buffer, 0, -1, true, { "external change" })
  insertion.base, insertion.next = 6, 7
  assert(replica.apply_patch(session, insertion).kind == "Desynchronized")
  assert(session.diagnostic:find("changedtick", 1, true))
  assert(session.revision == 6)

  replica.close(session)
  local sent = {}
  session = replica.open("editable", { editable = { send = function(request)
    sent[#sent + 1] = request
    return true
  end } })
  local source = block("body", { "original" })
  source.metadata.editable_region = {
    { id = "body", revision = 0, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 8 } } },
  }
  local initial = { document = "editable", revision = 0, block = { source } }
  assert(replica.apply_snapshot(session, initial).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "typed" })
  assert(editable.suspend_generated_text(session.editable))
  local captured = editable.capture_draft(session.editable)
  assert(#sent == 0 and captured[1].text == "typed", "native typing must not send an edit request")
  assert(replica.apply_snapshot(session, initial).kind == "Deferred", "generated snapshots must preserve pending drafts")
  editable.saved_capture(session.editable, captured)
  source.text = { "typed" }
  source.metadata.editable_region[1].revision = 1
  source.metadata.editable_region[1].range["end"].column = 5
  initial.revision = 1
  assert(replica.apply_snapshot(session, initial).kind == "Applied")
  assert(not editable.suspend_generated_text(session.editable))
  assert(session.editable.sequence == captured[1].sequence, "generated snapshot was recaptured as local typing")
  assert(not session.editable.fault)
  assert(session.editable.native and session.editable.native.active)
  assert(vim.api.nvim_buf_get_lines(session.buffer, 0, 1, true)[1] == "typed")
  local deep = replica.open("deep-layout", {})
  local capture = "String"
  local deep_block = block("deep", { "row" })
  deep_block.metadata.layout = { indent = 600, source_indent = 0, marker = { text = "•", capture = capture } }
  deep_block.metadata.decoration = {
    { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 3 } }, capture = capture, priority = 1 },
  }
  assert(replica.apply_snapshot(deep, { document = "deep-layout", revision = 0, block = { deep_block } }).kind == "Applied")
  assert(vim.api.nvim_buf_get_lines(deep.buffer, 0, -1, true)[1] == "row")
  replica.close(deep)
end

T["transfer"] = function()
  local buffer = require("forge.buffer")
  local editable = require("forge.editable")
  local existing = buffer_support.scratch()
  local owner, replacement
  MiniTest.finally(function()
    if replacement then pcall(buffer.invalidate, replacement) end
    if owner then pcall(buffer.invalidate, owner) end
  end)
  local function snapshot(document, text)
    return { document = document, revision = 0, block = {
      { id = "body", text = text, metadata = { target = {}, decoration = {}, editable_region = {
        { id = "draft", revision = 0, range = { start = { row = 0, column = 0 },
          ["end"] = { row = #text - 1, column = #text[#text] } } },
      } } },
    } }
  end
  local function open(document)
    return buffer.open(document, { buffer = existing, generated = true,
      expected_changedtick = vim.api.nvim_buf_get_changedtick(existing),
      editable = { delay = 1000, max_delay = 1000, send = function() return true end } })
  end
  owner = open("old-generation")
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "draft" })).kind == "Applied")
  vim.bo[existing].modifiable = true
  vim.api.nvim_buf_set_name(existing, "ForgeTransferFixture")
  local name = vim.api.nvim_buf_get_name(existing)
  local tick = vim.api.nvim_buf_get_changedtick(existing)
  buffer.close(owner, { preserve_buffer = true })
  assert(owner.status == "Closed" and vim.api.nvim_buf_is_valid(existing))
  assert(vim.api.nvim_buf_get_changedtick(existing) == tick and vim.api.nvim_buf_get_name(existing) == name)
  assert(vim.bo[existing].modifiable and not vim.b[existing].forge_native_document)
  owner = open("collected-generation")
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "draft" })).kind == "Applied")
  vim.bo[existing].modifiable = true
  vim.api.nvim_buf_set_text(existing, 0, 0, 0, 5, { "newer", "🙂draft" })
  local pending = editable.capture_draft(owner.editable)[1]
  assert(pending, owner.editable.fault or "native edit did not enter admission")
  tick = vim.api.nvim_buf_get_changedtick(existing)
  assert(buffer.close(owner, { preserve_buffer = true }).kind == "Deferred")
  assert(owner.editable.native, "ordinary close detached unacknowledged source")
  buffer.invalidate(owner)
  assert(owner.status == "Closed" and owner.editable.native == nil)
  assert(vim.api.nvim_buf_get_changedtick(existing) == tick)
  eq(vim.api.nvim_buf_get_lines(existing, 0, -1, true), { "newer", "🙂draft" })
  replacement = open("replacement-generation")
  assert(buffer.apply_snapshot(replacement, snapshot(replacement.document, { "newer", "🙂draft" })).kind == "Applied")
  local replacement_tick = vim.api.nvim_buf_get_changedtick(existing)
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "stale" })).kind == "Closed")
  assert(buffer.apply_snapshot_part(owner, {}).kind == "Closed")
  assert(buffer.apply_patch(owner, {}).kind == "Closed")
  buffer.invalidate(owner)
  assert(vim.b[existing].forge_native_document and replacement.editable.native)
  assert(vim.api.nvim_buf_get_changedtick(existing) == replacement_tick, "old owner mutated replacement source")
end

T["cooperative_snapshot"] = function()
  local buffer = require("forge.buffer")
  local folds = require("forge.nodes")
  local input = require("forge.input")
  local block = {}
  for index = 1, 3000 do
    local id = "history:" .. index
    local text = {}
    for row = 1, 10 do text[row] = "history " .. index .. " row " .. row end
    block[index] = { id = id, text = text, metadata = {
      target = {}, decoration = {}, editable_region = {},
      fold = { { id = "fold:" .. index, start = { row = 0, column = 0 },
        ["end"] = { block = id, position = { row = 10, column = 0 } }, closed = true } },
    } }
  end
  local replica = buffer.open("cooperative-snapshot", { preserve_view = true, editable = { send = function() return true end } })
  vim.api.nvim_set_current_buf(replica.buffer)
  local view = input.open(replica, vim.api.nvim_get_current_win())
  local tick, result = 0, nil
  local timer = vim.uv.new_timer()
  timer:start(0, 1, vim.schedule_wrap(function() tick = tick + 1 end))
  buffer.apply_async(replica, { document = replica.document, revision = 7, block = block },
    function() return true end, function(adopted) result = adopted end)
  assert(replica.update_pending, "large snapshot finished without yielding")
  assert(not input.capture(replica, view, "activate"), "partial snapshot accepted input")
  wait_until(function() return result ~= nil end, "snapshot did not settle", { timeout_ms = 4000 })
  assert(result.kind == "Applied", result.diagnostic)
  assert(tick > 20, "snapshot blocked the main loop")
  assert(replica.revision == 7 and replica.row_count == 3000)
  assert(vim.api.nvim_buf_get_lines(replica.buffer, 2999, 3000, true)[1] == "history 3000 row 1")
  assert(folds.closed(replica, "fold:3000"), "scheduled projection lost the last node")
  assert(not vim.bo[replica.buffer].modifiable)
  assert(replica.update_timing.maximum_prepare_ms < 50, "snapshot preparation exceeded 50ms")
  assert(replica.update_timing.maximum_commit_ms < 250, "atomic snapshot commit exceeded 250ms")
  local snapshot_timing = vim.deepcopy(replica.update_timing)
  local recovering
  vim.api.nvim_win_set_cursor(view.window, { 1001, 0 })
  local write_lines = vim.api.nvim_buf_set_lines
  local recovery_writes = 0
  vim.api.nvim_buf_set_lines = function(native, ...)
    local result = write_lines(native, ...)
    if native == replica.buffer then
      recovery_writes = recovery_writes + 1
    end
    return result
  end
  local recovery_block = { { id = "new-heading", text = { "New heading", "New context" },
    metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } } }
  vim.list_extend(recovery_block, block)
  buffer.apply_async(replica, { document = replica.document, revision = 8, block = recovery_block },
    function() return true end, function(adopted) recovering = adopted end)
  vim.api.nvim_win_set_cursor(view.window, { 91, 0 })
  wait_until(function() return recovering ~= nil end, "recovery snapshot did not settle", { timeout_ms = 4000 })
  vim.api.nvim_buf_set_lines = write_lines
  assert(recovering.kind == "Applied", recovering.diagnostic)
  assert(vim.api.nvim_win_get_cursor(view.window)[1] == 93,
    "recovery lost the reader's latest cursor identity when rows were inserted above it")
  assert(vim.api.nvim_get_current_line() == "history 91 row 1", "recovery selected a different history entry")
  assert(replica.update_timing.maximum_prepare_ms < 50, "recovery preparation exceeded 50ms")
  assert(recovery_writes == 1, "snapshot cleared or incrementally refilled the live buffer")
  assert(folds.closed(replica, "fold:3000"))
  local patch_result
  local replacement = {}
  for row = 1, 30000 do replacement[row] = "expanded output row " .. row end
  buffer.apply_async(replica, {
    document = replica.document, base = 8, next = 9,
    base_rows = 30002, next_rows = 30000, base_blocks = 3001, next_blocks = 1,
    block_edit = { { start_block = 0, removed_blocks = 3001, inserted = { "expanded" } } },
    removed_block = vim.tbl_map(function(entry) return entry.id end, recovery_block),
    metadata_edit = { { block = "expanded", row_count = 30000, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } } },
    text_edit = { { start_row = 0, removed_rows = 30002, text = replacement } },
  }, function() return true end, function(adopted) patch_result = adopted end)
  wait_until(function() return patch_result ~= nil end, "replacement patch did not settle", { timeout_ms = 4000 })
  assert(patch_result.kind == "Applied", patch_result.diagnostic)
  assert(replica.update_timing.maximum_prepare_ms < 50, "replacement preparation exceeded 50ms")
  assert(replica.revision == 9 and replica.sequence:count() == 1)
  assert(vim.api.nvim_buf_get_lines(replica.buffer, 29999, 30000, true)[1] == replacement[30000])
  timer:stop()
  timer:close()
  input.close(view)
  buffer.close(replica)

  local cancelled = buffer.open("cancelled-snapshot")
  local alive, cancellation = true, nil
  buffer.apply_async(cancelled, { document = cancelled.document, revision = 1, block = block },
    function() return alive end, function(adopted) cancellation = adopted end)
  alive = false
  wait_until(function() return cancellation ~= nil end, "cancelled snapshot did not report its result")
  assert(cancellation.kind ~= "Applied", "cancelled work published a revision")
  assert(cancelled.revision == nil and cancelled.status ~= "Applied")
  buffer.close(cancelled)
  local closing = buffer.open("closed-snapshot")
  buffer.apply_async(closing, { document = closing.document, revision = 1, block = block },
    function() return true end, function() end)
  buffer.close(closing)
  wait_until(function() return not closing.update_pending end, "closed replica kept its snapshot update pending")
  assert(closing.status == "Closed")
  local interrupted = buffer.open("interrupted-snapshot")
  local mutation_alive, interrupted_result = true, nil
  local original_set_lines = vim.api.nvim_buf_set_lines
  local writes = 0
  vim.api.nvim_buf_set_lines = function(native, ...)
    local result = original_set_lines(native, ...)
    if native == interrupted.buffer then
      writes = writes + 1
      if writes == 1 then mutation_alive = false end
    end
    return result
  end
  buffer.apply_async(interrupted, { document = interrupted.document, revision = 1, block = block },
    function() return mutation_alive end, function(adopted) interrupted_result = adopted end)
  wait_until(function() return interrupted_result ~= nil end, "interrupted snapshot did not settle", { timeout_ms = 4000 })
  vim.api.nvim_buf_set_lines = original_set_lines
  assert(interrupted_result.kind == "Applied" and interrupted.revision == 1,
    "cancellation inside the commit exposed an unfinished publication")
  assert(writes == 1, "atomic snapshot used more than one text replacement")
  assert(not vim.bo[interrupted.buffer].modifiable)
  buffer.close(interrupted)
end

-- An owner that stops being alive mid-apply, as a presentation does when its host stops, withdraws
-- the update. The replica keeps its last applied revision and decorations, and neither notices the
-- user nor requests recovery, because nothing about the document went wrong.
T["withdrawn_apply"] = function()
  local buffer = require("forge.buffer")
  local notices, recoveries = {}, 0
  local replica = buffer.open("withdrawn-snapshot", {
    notice = function(message) notices[#notices + 1] = message end,
    recover = function() recoveries = recoveries + 1 end,
  })
  local function block_list(count, label)
    local block = {}
    for index = 1, count do
      block[index] = { id = label .. ":" .. index, text = { label .. " row " .. index }, metadata = {
        target = {}, decoration = {}, editable_region = {},
      } }
    end
    return block
  end
  eq(buffer.apply_snapshot(replica, { document = replica.document, revision = 1, block = block_list(1, "applied") }).kind,
    "Applied")
  vim.api.nvim_buf_set_extmark(replica.buffer, replica.namespace, 0, 0, { end_col = 1 })
  local marks_before = #vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, {})
  assert(marks_before > 0, "fixture decoration was not installed")
  local alive, withdrawn = true, nil
  buffer.apply_async(replica, { document = replica.document, revision = 2, block = block_list(3000, "pending") },
    function() return alive end, function(result) withdrawn = result end)
  assert(replica.update_pending, "large snapshot finished without yielding")
  alive = false
  wait_until(function() return withdrawn ~= nil end, "withdrawn snapshot did not report its result")
  eq(withdrawn.kind, "Cancelled")
  eq(replica.status, "Applied")
  eq(replica.revision, 1)
  eq(#vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, {}), marks_before)
  eq(notices, {})
  eq(recoveries, 0)
  assert(not vim.bo[replica.buffer].modifiable)
  buffer.close(replica)
end

T["mixed_edit_patch"] = function()
  local buffer = require("forge.buffer")
  local editable = require("forge.editable")
  local function metadata(text, revision)
    return { target = {}, decoration = {}, editable_region = {
      { id = "title", revision = revision, range = { start = { row = 0, column = 0 },
        ["end"] = { row = #text - 1, column = #text[#text] } } },
    } }
  end
  local session = buffer.open("mixed-capture", { editable = {} })
  local block = { { id = "title", text = { "Original" }, metadata = metadata({ "Original" }, 0) } }
  for index = 1, 10000 do
    block[#block + 1] = { id = "source:" .. index, text = { "untouched" },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
  end
  assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = block }).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "Captured", "title" })
  local captured = editable.capture_draft(session.editable)
  vim.api.nvim_buf_set_text(session.buffer, 1, 5, 1, 5, { " newer" })
  local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  local patch = { document = session.document, base = 0, next = 1, base_rows = 10001, next_rows = 10001,
    base_blocks = 10001, next_blocks = 10001, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 5000, removed_rows = 1, text = { "generated" } } }, metadata_edit = {} }
  for index = 1, 300 do
    local deferred = vim.deepcopy(patch)
    deferred.base, deferred.next = index - 1, index
    assert(buffer.apply_patch(session, deferred).kind == "Deferred")
  end
  editable.saved_capture(session.editable, captured)
  assert(buffer.apply_patch(session, patch).kind == "Deferred")
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick)
  assert(captured[1].text == "Captured\ntitle")
  local latest = editable.capture_draft(session.editable)
  assert(latest[1].text == "Captured\ntitle newer")
  editable.saved_capture(session.editable, latest)
  block[1] = { id = "title", text = { "Captured", "title newer" }, metadata = metadata({ "Captured", "title newer" }, 2) }
  assert(buffer.apply_snapshot(session, { document = session.document, revision = 2, block = block }).kind == "Applied")
  patch.base, patch.next, patch.base_rows, patch.next_rows = 2, 3, 10002, 10002
  patch.text_edit[1].start_row = 5001
  local untouched = session.block["source:9000"]
  local attachment = session.editable.native
  local get_lines, broad_read = vim.api.nvim_buf_get_lines, false
  vim.api.nvim_buf_get_lines = function(buf, start, finish, strict)
    if buf == session.buffer and (finish == -1 or finish - start > 1024) then broad_read = true end
    return get_lines(buf, start, finish, strict)
  end
  local result = buffer.apply_patch(session, patch)
  vim.api.nvim_buf_get_lines = get_lines
  assert(result.kind == "Applied", vim.inspect(result))
  assert(not broad_read, "generated patch copied unrelated source rows")
  assert(session.block["source:9000"] == untouched and session.editable.native == attachment)
  local malformed = vim.deepcopy(patch)
  malformed.base, malformed.next = 3, 4
  malformed.text_edit[1].start_row = 99999
  local before = get_lines(session.buffer, 0, -1, true)
  assert(buffer.apply_patch(session, malformed).kind == "Desynchronized")
  eq(get_lines(session.buffer, 0, -1, true), before)
  buffer.close(session)
end

T["composer_interleave"] = function()
  local buffer = require("forge.buffer")
  local editable = require("forge.editable")
  local function snapshot(revision, text)
    return { document = "composer", revision = revision, block = {
      { id = "body", text = text, metadata = { target = {}, decoration = {}, editable_region = {
        { id = "composer", revision = revision, range = { start = { row = 0, column = 0 },
          ["end"] = { row = #text - 1, column = #text[#text] } } },
      } } },
    } }
  end
  for _, completion_first in ipairs({ false, true }) do
    local session = buffer.open("composer", { editable = {} })
    assert(buffer.apply_snapshot(session, snapshot(0, { "submitted" })).kind == "Applied")
    vim.bo[session.buffer].modifiable = true
    vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "accepted" })
    local captured = editable.capture_draft(session.editable)
    vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "newer", "draft" })
    local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
    if completion_first then editable.saved_capture(session.editable, captured) end
    assert(buffer.apply_snapshot(session, snapshot(1, { "" })).kind == "Deferred")
    if not completion_first then editable.saved_capture(session.editable, captured) end
    assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick)
    eq(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "newer", "draft" })
    assert(captured[1].text == "accepted")
    assert(editable.capture_draft(session.editable)[1].base == 1)
    editable.saved_capture(session.editable, editable.capture_draft(session.editable))
    assert(buffer.apply_snapshot(session, snapshot(2, { "newer", "draft" })).kind == "Applied")
    assert(buffer.apply_snapshot(session, snapshot(3, { "" })).kind == "Applied")
    eq(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "" })
    buffer.close(session)
  end
end

T["fold_lifecycle"] = function()
  local buffer = require("forge.buffer")
  local folds = require("forge.nodes")
  local owner = buffer.open("harness-fold-lifecycle")
  vim.api.nvim_set_current_buf(owner.buffer)
  local function metadata(id, endpoint, rows)
    return { target = {}, decoration = {}, editable_region = {}, fold = id and {
      { id = id, start = { row = 0, column = 0 },
        ["end"] = { block = endpoint, position = { row = rows, column = 0 } }, closed = false },
    } or {} }
  end
  local function snapshot(revision, tool_rows)
    return { document = owner.document, revision = revision, block = {
      { id = "prompt", text = { "Accept plan" }, metadata = metadata() },
      { id = "summary", text = { "Plan implementation" }, metadata = metadata("exchange", "tools", #tool_rows) },
      { id = "thought", text = { "Inspecting the workspace" }, metadata = metadata() },
      { id = "tools", text = tool_rows, metadata = metadata("tools", "tools", #tool_rows) },
      { id = "status", text = { "Implementing" }, metadata = metadata() },
    } }
  end
  local tool_rows = { "Running 1 tool", "Get-Content src/lib.rs", "no output" }
  assert(buffer.apply_snapshot(owner, snapshot(0, tool_rows)).kind == "Applied")
  local first_window = vim.api.nvim_get_current_win()
  folds.attach(owner, first_window)
  vim.cmd("vsplit")
  local second_window = vim.api.nvim_get_current_win()
  folds.attach(owner, second_window)

  local function check()
    local exchange_closed = folds.closed(owner, "exchange")
    local tools_closed = folds.closed(owner, "tools")
    local expected = exchange_closed and 3 or tools_closed and 5 or 4 + #tool_rows
    assert(vim.api.nvim_buf_line_count(owner.buffer) == expected)
    for _, window in ipairs({ first_window, second_window }) do
      assert(not vim.wo[window].foldenable)
    end
    eq(owner.projection.source.block.tools.text, tool_rows)
  end

  local function patch(text_edit, metadata_edit, next_rows)
    local result = buffer.apply_patch(owner, {
      document = owner.document, base = owner.revision, next = owner.revision + 1,
      base_rows = owner.projection.source.row_count, next_rows = next_rows or owner.projection.source.row_count,
      base_blocks = 5, next_blocks = 5, block_edit = {}, removed_block = {},
      text_edit = text_edit, metadata_edit = metadata_edit or {},
    })
    assert(result.kind == "Applied", vim.inspect(result))
    check()
  end

  check()
  for step = 1, 12 do
    folds.set_open(owner, first_window, "exchange", step % 2 == 0)
    folds.set_open(owner, second_window, "tools", step % 3 == 0)
    tool_rows[#tool_rows] = step % 2 == 0 and "" or ("output " .. step .. " λ")
    patch({ { start_row = 2 + #tool_rows, removed_rows = 1, text = { tool_rows[#tool_rows] } } })
    patch({ { start_row = 1, removed_rows = 1, text = { "Plan implementation " .. step .. "s" } } })
  end

  for _, count in ipairs({ 5, 2, 4, 3 }) do
    local previous_count = #tool_rows
    tool_rows = { "Ran 1 tool" }
    for index = 2, count do tool_rows[index] = "output " .. index end
    patch({ { start_row = 3, removed_rows = previous_count, text = tool_rows } }, {
      { block = "tools", row_count = count, metadata = metadata("tools", "tools", count) },
      { block = "summary", row_count = 1, metadata = metadata("exchange", "tools", count) },
    }, 4 + count)
    folds.release(second_window)
    folds.attach(owner, second_window)
    check()
  end
  assert(buffer.apply_snapshot(owner, snapshot(owner.revision + 1, tool_rows)).kind == "Applied")
  check()
  buffer.close(owner)
end

return T
