-- Unit tests for `forge.status_render` (status_render.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local fixture = dofile("nvim/lua/forge/tests/support/status_fixture.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["status_body_ordering"] = function()
  local render = require("forge.status_render")
  local nodes = require("forge.nodes")
  local notices = {}
  local replica = render.open("status-body-ordering", {
    notice = function(message) notices[#notices + 1] = message end,
  })
  vim.api.nvim_set_current_buf(replica.buffer)
  local recovery_count = 0
  replica.recover_body = function(file, generation)
    assert(file == 1 and generation == 2)
    recovery_count = recovery_count + 1
  end
  ---@param revision integer
  ---@param content string
  ---@return table
  local function delivery(revision, content)
    local value = fixture.body(replica.document, { content }, { more = true })
    value.generation, value.snapshot.document, value.snapshot.revision = 2, "body:1:2", revision
    return value
  end
  ---@param base integer
  ---@return table
  local function patch_delivery(base)
    return { document = replica.document, file = 1, generation = 2, more = true,
      patch = { document = "body:1:2", base = base, next = base + 1 } }
  end
  ---@param base integer
  ---@param body table
  ---@return table
  local function update(base, body)
    local record = fixture.file()
    record.generation = 2
    return { document = replica.document, phase = "context", operation_id = 0,
      delta = { document = replica.document, base = base, next = base + 1,
        removed = {}, section = {}, file = { record }, pending = {} },
      body = { body } }
  end

  assert(render.apply_snapshot(replica, fixture.snapshot(replica.document)).kind == "Applied")
  assert(nodes.set_open(replica, 0, "file:1", true))
  assert(render.apply_body(replica, fixture.body(replica.document, { "initial source" }, { more = true })).kind == "Applied")
  assert(render.apply_body(replica, patch_delivery(4)).kind == "Discarded",
    "future generation arrived before its inventory")
  assert(render.apply_update(replica, update(0, delivery(4, "syntax source"))).kind == "Applied")
  local before, changedtick = buffer_support.lines(replica.buffer), replica.changedtick
  assert(render.apply_body(replica, patch_delivery(5)).kind == "Recovering", "revision gap did not request recovery")
  assert(replica.status == "Applied" and replica.file[1].body.revision == 4)
  assert(recovery_count == 1 and #notices == 0)
  assert(replica.changedtick == changedtick and vim.deep_equal(before, buffer_support.lines(replica.buffer)), "recovery changed the usable view")
  assert(not nodes.closed(replica, "file:1"), "recovery collapsed the file")

  assert(render.apply_snapshot(replica, replica.inventory).kind == "Applied")
  assert(render.apply_body(replica, patch_delivery(6)).kind == "Recovering")
  assert(recovery_count == 1, "inventory replacement lost in-flight body recovery")
  assert(render.apply_body(replica, delivery(7, "recovered source")).kind == "Applied")
  replica.file[1].recovering = nil
  before, changedtick = buffer_support.lines(replica.buffer), replica.changedtick
  assert(render.apply_body(replica, patch_delivery(5)).kind == "Applied")
  assert(render.apply_body(replica, delivery(6, "stale source")).kind == "Applied")
  assert(replica.file[1].body.revision == 7 and replica.changedtick == changedtick)
  eq(before, buffer_support.lines(replica.buffer), { fail_reason = "late delivery replaced newer source" })
  assert(render.apply_update(replica, update(1, delivery(6, "stale event source"))).kind == "Applied")
  assert(replica.file[1].body.revision == 7 and vim.deep_equal(before, buffer_support.lines(replica.buffer)),
    "late context event replaced a newer body snapshot")

  local next_delivery = patch_delivery(7)
  local patch = next_delivery.patch
  patch.base_rows, patch.next_rows, patch.base_blocks, patch.next_blocks = 1, 1, 1, 1
  patch.text_edit = { { start_row = 0, removed_rows = 1, text = { "continued source" } } }
  patch.block_edit, patch.removed_block = {}, {}
  patch.metadata_edit = { { block = "body:1", row_count = 1, metadata = fixture.metadata("hunk:1", 1) } }
  assert(render.apply_body(replica, next_delivery).kind == "Applied")
  assert(replica.file[1].body.revision == 8 and buffer_support.text(replica.buffer):find("continued source", 1, true))

  local invalid = vim.deepcopy(next_delivery)
  invalid.patch.base, invalid.patch.next, invalid.patch.base_rows = 8, 9, 2
  before = buffer_support.lines(replica.buffer)
  assert(render.apply_body(replica, invalid).kind == "Desynchronized")
  assert(#notices == 1 and notices[1]:find("patch base counts differ", 1, true))
  assert(recovery_count == 2 and vim.deep_equal(before, buffer_support.lines(replica.buffer)), "invalid patch replaced the prior view")
  assert(render.apply_body(replica, delivery(8, "continued source")).kind == "Applied",
    "desynchronized body rejected its recovery snapshot")
  assert(replica.status == "Applied" and not nodes.closed(replica, "file:1"))
  render.close(replica)
end

T["status_fold_boundaries"] = function()
  local render = require("forge.status_render")
  local input = require("forge.input")
  local notices = {}
  local replica = render.open("status-fold-boundaries", { notice = function(message) notices[#notices + 1] = message end })
  vim.api.nvim_set_current_buf(replica.buffer)
  local function record(id)
    return { id = id, generation = 1, section = "unstaged", change = "modified", path = id .. ".lua",
      header = fixture.header(id .. ".lua"), untracked = false, stats = { state = "unknown" } }
  end
  assert(render.apply_snapshot(replica, { document = replica.document, revision = 0, view = { kind = "status" },
    head = { state = "attached", reference = "refs/heads/main", object = "abc123" }, context = vim.NIL,
    section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1), record(2) } }).kind == "Applied")
  local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
  assert(require("forge.nodes").set_open(replica, 0, "file:1", true))
  local function metadata(definitions)
    return { target = {}, decoration = {}, editable_region = {}, fold = definitions or {} }
  end
  assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
    snapshot = { document = "body:1:1", revision = 1, block = {
      { id = "hunk", text = { "@@ hunk" }, metadata = metadata({ { id = "hunk", start = { row = 0, column = 0 },
        ["end"] = { block = "end", position = { row = 0, column = 0 } }, closed = false } }) },
      { id = "content", text = { "first", "second" }, metadata = metadata() },
      { id = "end", text = {}, metadata = metadata() },
    } } }).kind == "Applied", table.concat(notices, "\n"))

  local previous = 2
  for _, count in ipairs({ 5, 1, 0, 4 }) do
    local text = {}
    for row = 1, count do text[row] = "line " .. row end
    local body = replica.file[1].body
    assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
      patch = { document = body.document, base = body.revision, next = body.revision + 1,
        base_rows = previous + 1, next_rows = count + 1, base_blocks = 3, next_blocks = 3,
        text_edit = { { start_row = 1, removed_rows = previous, text = text } }, block_edit = {}, removed_block = {},
        metadata_edit = { { block = "content", row_count = count, metadata = metadata() } } },
    }).kind == "Applied", table.concat(notices, "\n"))
    assert(replica.row_count == 6 + count)
    assert(not require("forge.nodes").closed(replica,"file:1"))
    assert(require("forge.nodes").closed(replica,"file:2"))
    previous = count
  end
  assert(#notices == 0, table.concat(notices, "\n"))
  local body = replica.file[1].body
  local revision, text = body.revision, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
  local invalid = render.apply_body(replica, { document = replica.document, file = 1, generation = 1,
    patch = { document = body.document, base = revision, next = revision + 1,
      base_rows = 5, next_rows = 6, base_blocks = 3, next_blocks = 3,
      text_edit = { { start_row = 1, removed_rows = 4, text = { "replacement" } } }, block_edit = {}, removed_block = {},
      metadata_edit = { { block = "content", row_count = 5, metadata = metadata() } } },
  })
  assert(invalid.kind == "Desynchronized" and #notices == 1, "invalid patch did not report failure")
  assert(body.revision == revision and vim.deep_equal(text, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)),
    "invalid patch changed the committed body")
  assert(require("forge.nodes").closed(replica,"file:2"), "invalid patch changed expansion state")
  input.close(view)
  render.close(replica)
end

T["status_gutter_relocation"] = function()
  local render = require("forge.status_render")
  local failures = {}
  for _, moved_id in ipairs({ 1, 3 }) do
    local replica = render.open("gutter-relocation-" .. moved_id, {})
    local ok, failure = xpcall(function()
      vim.api.nvim_win_set_buf(0, replica.buffer)
      vim.wo.wrap = true

      local function record(id, section)
        return { id = id, generation = 1, section = section, change = "modified", path = "source_" .. id .. ".rs", untracked = false, stats = { state = "exact", added = 30, deleted = 0 },
          header = fixture.header("source_" .. id .. ".rs", 30, 0) }
      end

      local function delivery(id)
        local text, gutter = {}, {}
        for row = 0, 29 do
          text[#text + 1] = ("let source_%d_row_%d = %d;"):format(id, row, row)
          gutter[#gutter + 1] = { position = { row = row, column = 0 }, priority = 100,
            chunk = { { text = ("%d + "):format(row + 1), capture = "DiffAdd" } } }
        end
        return { document = replica.document, file = id, generation = 1, more = false, state = { state = "ready" },
          snapshot = { document = ("body:%d:1"):format(id), revision = 1, block = {
            { id = "source-body:" .. id, text = text, metadata = { target = {}, decoration = {}, editable_region = {}, gutter = gutter } },
          } } }
      end

      local function check_gutters(phase)
        local count = 0
        for id, handles in pairs(replica.marks) do
          local entry = replica.block[id]
          local _, start = replica.sequence:position(id)
          for ordinal, handle in ipairs(handles) do
            local mark = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, replica.namespace, handle, { details = true })
            if mark[3].virt_text then
              count = count + 1
              local expected = start + entry.metadata.gutter[ordinal].position.row
              assert(mark[1] == expected, ("%s: %s gutter %d collapsed onto row %d, expected %d"):format(phase, id, ordinal, mark[1], expected))
            end
          end
        end
        assert(count == 90, phase .. ": missing or duplicated gutters: " .. count)
        local installed = 0
        for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, { details = true })) do
          if mark[4].virt_text then installed = installed + 1 end
        end
        assert(installed == count, ("%s: %d orphan gutters remain outside the renderer's mark ownership"):format(phase, installed - count))
      end

      assert(render.apply_snapshot(replica, { document = replica.document, revision = 0, view = { kind = "status" },
        head = { state = "attached", reference = "main", object = "abc" },
        section = { { kind = "unstaged", file = { 1, 2, 3 } } },
        file = { record(1, "unstaged"), record(2, "unstaged"), record(3, "unstaged") }, pending = {} }).kind == "Applied")
      for id = 1, 3 do
        assert(render.apply_body(replica, delivery(id)).kind == "Applied")
        assert(require("forge.buffer").set_expansion(replica, "file:" .. id, true))
      end
      check_gutters("initial")
      assert(render.apply_update(replica, { document = replica.document, operation_id = 1, phase = "accepted", diagnostic = {}, body = {},
        delta = { document = replica.document, base = 0, next = 1, removed = {}, pending = { 1 },
          file = { record(moved_id, "staged") }, section = { { kind = "staged", file = { moved_id } },
            { kind = "unstaged", file = moved_id == 1 and { 2, 3 } or { 1, 2 } } } } }).kind == "Applied")
      vim.cmd("redraw!")
      check_gutters("staged relocation")
      replica.status = "Desynchronized"
      assert(render.apply_snapshot(replica, replica.inventory).kind == "Applied")
      vim.cmd("redraw!")
      check_gutters("snapshot recovery after staging")
      for cycle = 1, 5 do
        for _, stage in ipairs({ false, true }) do
          local revision = replica.revision
          local section = stage and {
            { kind = "staged", file = { moved_id } },
            { kind = "unstaged", file = moved_id == 1 and { 2, 3 } or { 1, 2 } },
          } or {
            { kind = "staged", file = {} },
            { kind = "unstaged", file = { 1, 2, 3 } },
          }
          assert(render.apply_update(replica, {
            document = replica.document, operation_id = cycle + 1, phase = "accepted", diagnostic = {}, body = {},
            delta = { document = replica.document, base = revision, next = revision + 1, removed = {}, pending = {},
              file = { record(moved_id, stage and "staged" or "unstaged") }, section = section },
          }).kind == "Applied")
          vim.cmd("redraw!")
          check_gutters((stage and "stage" or "unstage") .. " cycle " .. cycle)
        end
      end
    end, debug.traceback)
    render.close(replica)
    if not ok then failures[#failures + 1] = failure end
  end
  assert(#failures == 0, table.concat(failures, "\n\n"))
end

T["status_header_width"] = function()
  local render = require("forge.status_render")
  local input = require("forge.input")
  local replica = render.open("header-width", { notice = error })
  vim.cmd("vsplit")
  vim.api.nvim_win_set_width(0, 40)
  vim.api.nvim_set_current_buf(replica.buffer)
  local subject = "refactor: " .. string.rep("界 change ", 30)
  local head = { oid = string.rep("a", 40), reference = "feature", subject = subject }
  local remote = { oid = head.oid, reference = "origin/feature", subject = subject }
  local context = { workspace = "fixture", branch = "feature", head = head, upstream = remote, push = remote,
    recent = {}, issues = {} }
  replica.width = 40
  assert(render.apply_snapshot(replica, fixture.snapshot(replica.document, { context = context })).kind == "Applied")
  local view = input.open(replica, 0, { margin = 0 })
  assert(vim.wo.wrap and vim.wo.linebreak and not vim.wo.breakindent and vim.wo.statuscolumn == "")

  local original = buffer_support.lines(replica.buffer)
  local function verify()
    for _, role in ipairs({ "head", "upstream", "push" }) do
      local block = replica.source_entry["status:context:" .. role]
      local _, start = replica.sequence:position("status:context:" .. role)
      assert(#block.chunk == 1, "header was split into physical buffer lines")
      local parts = {}
      for _, chunk in ipairs(block.chunk[1]) do parts[#parts + 1] = chunk[1] end
      local line = table.concat(parts)
      local value = line:sub(9)
      local reference = role == "head" and "feature       " or "origin/feature"
      assert(value == vim.fn.strcharpart("aaaaaaa " .. reference .. " " .. subject, 0, 99) .. "…", value)
      assert(vim.fn.strchars(value) == 100)
      assert(buffer_support.line(replica.buffer, start + 1) == line)
      assert(block.chunk[1][2][2] == "ForgeStatusObjectId", "SHA highlight was lost")
      assert(replica.inventory.context[role].subject == subject, "clamping changed the retained commit")
    end
  end
  verify()
  local _, start = replica.sequence:position("status:context:head")
  local height = vim.api.nvim_win_text_height(0, { start_row = start, end_row = start }).all
  assert(height > 1, "narrow window did not soft-wrap the header")
  vim.api.nvim_win_set_cursor(0, { start + 1, 0 })
  vim.cmd("normal! j")
  assert(vim.api.nvim_win_get_cursor(0)[1] == start + 2, "native down movement stopped in a generated continuation")
  vim.cmd("only")
  render.resize(replica, vim.api.nvim_get_current_win())
  verify()
  eq(original, buffer_support.lines(replica.buffer), { fail_reason = "resizing rewrote header text" })
  for _, state in ipairs({ "error", "unavailable" }) do
    render.present_context(replica, { pr = { state = state, text = "" }, about = { state = "error", text = "" } })
    local _, pr_start = replica.sequence:position("status:context:pr")
    local pr_line = buffer_support.line(replica.buffer, pr_start + 1)
    assert(pr_line:match("^PR:%s+none$"), "failed PR lookup was not rendered as none: " .. pr_line)
    local pr_chunk = replica.source_entry["status:context:pr"].chunk[1]
    assert(pr_chunk[#pr_chunk][2] == "ForgeStatusObjectId", "absent PR retained error highlighting")
    assert(replica.presentation.pr.state == state, "display fallback changed the lookup state")
    local _, about_start = replica.sequence:position("status:context:about")
    local about_line = buffer_support.line(replica.buffer, about_start + 1)
    assert(about_line:match("^About:%s+error$"), "PR fallback hid a commit generation error")
  end
  render.present_context(replica, { about = { state = "generating", text = "old message", model = "luna · thinking low · fast" } })
  local _, generating_start = replica.sequence:position("status:context:about")
  local generating_line = buffer_support.line(replica.buffer, generating_start + 1)
  assert(generating_line:find("Generating with luna · thinking low · fast", 1, true), generating_line)
  assert(not generating_line:find("old message", 1, true), "generation retained the previous About subject")
  input.close(view)
  require("forge.buffer").close(replica)
end

T["status_optimistic"] = function()
  local render = require("forge.status_render")
  local input = require("forge.input")
  local notices = {}
  local replica = render.open("optimistic-render", { notice = function(message) notices[#notices + 1] = message end })
  vim.api.nvim_win_set_buf(0, replica.buffer)

  ---@param id integer
  ---@param generation integer
  ---@param section string
  ---@return table
  local function record(id, generation, section)
    return { id = id, generation = generation, section = section, change = "modified", path = id == 2 and "unrelated.lua" or "sample.lua", untracked = false, stats = { state = "exact", added = 1, deleted = 1 },
      header = fixture.header(id == 2 and "unrelated.lua" or "sample.lua", 1, 1) }
  end

  ---@param id integer
  ---@param generation integer
  ---@param group string[]
  ---@return table
  local function delivery(id, generation, group)
    local block = {}
    for _, name in ipairs(group) do
      local header = fixture.metadata("group:" .. name, 1)
      header.fold = { { id = "header:" .. name, start = { row = 0, column = 0 }, ["end"] = { block = "body:" .. name, position = { row = 1, column = 0 } }, closed = false } }
      block[#block + 1] = { id = "header:" .. name, text = { "@@ " .. name }, metadata = header }
      block[#block + 1] = { id = "body:" .. name, text = { "changed " .. name }, metadata = fixture.metadata("hunk:" .. name, 1) }
    end
    return { document = replica.document, file = id, generation = generation, more = false, state = { state = "ready" }, snapshot = { document = ("body:%d:%d"):format(id, generation), revision = 1, block = block } }
  end

  local snapshot = { document = replica.document, revision = 0, view = { kind = "status" }, head = { state = "attached", reference = "main", object = "abc" },
    section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1, 1, "unstaged"), record(2, 1, "unstaged") }, pending = {} }
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
  local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
  assert(require("forge.nodes").set_open(replica, vim.api.nvim_get_current_win(), "file:1", true))
  assert(render.apply_body(replica, delivery(1, 1, { "A", "B" })).kind == "Applied")
  vim.api.nvim_win_set_cursor(0, { 7, 0 })
  assert(require("forge.nodes").set_open(replica, 0, "header:B", false))
  assert(require("forge.nodes").closed(replica, "header:B"))
  local writes = 0
  vim.api.nvim_buf_attach(replica.buffer, false, { on_lines = function() writes = writes + 1 end })
  local untouched = replica.file[2]
  local accepted = { document = replica.document, operation_id = 0, phase = "accepted", diagnostic = {}, delta = { document = replica.document, base = 0, next = 1,
    removed = {}, file = { record(1, 2, "unstaged"), record(3, 1, "staged") }, section = { { kind = "staged", file = { 3 } } }, pending = { 0 } },
    body = { delivery(1, 2, { "B" }), delivery(3, 1, { "staged-A" }) } }
  assert(render.apply_update(replica, accepted).kind == "Applied", table.concat(notices, "\n"))
  assert(replica.file[2].record == untouched.record and replica.file[2].body == untouched.body)
  assert(replica.inventory.pending[1] == 0)
  local _, header_row = replica.sequence:position("header:B")
  assert(require("forge.nodes").closed(replica, "header:B"), "surviving hunk lost its expansion state")
  assert(vim.api.nvim_win_get_cursor(0)[1] == header_row + 1, "cursor left its surviving hunk")
  local changedtick = vim.api.nvim_buf_get_changedtick(replica.buffer)
  local previous_writes = writes
  local settled = { document = replica.document, operation_id = 0, phase = "settled", diagnostic = {}, body = {}, delta = { document = replica.document,
    base = 1, next = 2, removed = {}, file = {}, section = {}, pending = {} } }
  assert(render.apply_update(replica, settled).kind == "Applied")
  assert(writes == previous_writes and vim.api.nvim_buf_get_changedtick(replica.buffer) == changedtick, "matching settlement rewrote the buffer")
  assert(render.apply_update(replica, accepted).kind == "Discarded", "duplicate acceptance was applied twice")
  local before = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
  local invalid = vim.deepcopy(accepted)
  invalid.delta.base, invalid.delta.next = 2, 3
  invalid.body[1].generation = 99
  assert(render.apply_update(replica, invalid).kind == "Desynchronized")
  eq(before, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), { fail_reason = "invalid atomic update changed text" })
  assert(replica.revision == 2 and #notices == 1)
  input.close(view)
  render.close(replica)

  for _, direction in ipairs({ "stage", "unstage" }) do
    for _, location in ipairs({ "file", "body", "tail" }) do
      local source_section = direction == "stage" and "unstaged" or "staged"
      local destination_section = direction == "stage" and "staged" or "unstaged"
      replica = render.open("cursor-" .. direction .. "-" .. location, {
        notice = function(message) notices[#notices + 1] = message end,
      })
      vim.api.nvim_win_set_buf(0, replica.buffer)
      assert(render.apply_snapshot(replica, {
        document = replica.document, revision = 0, view = { kind = "status" },
        head = { state = "attached", reference = "main", object = "abc" },
        section = { { kind = source_section, file = { 1, 2 } } },
        file = { record(1, 1, source_section), record(2, 1, source_section) }, pending = {},
      }).kind == "Applied")
      local cursor_block = "file:1"
      if location ~= "file" then
        assert(require("forge.nodes").set_open(replica, 0, cursor_block, true))
        local group = location == "tail" and { "removed", "middle", "last" } or { "removed" }
        assert(render.apply_body(replica, delivery(1, 1, group)).kind == "Applied")
        cursor_block = location == "tail" and "body:last" or "body:removed"
      end
      local _, cursor_row = replica.sequence:position(cursor_block)
      vim.api.nvim_win_set_cursor(0, { cursor_row + 1, 2 })
      local position = vim.api.nvim_win_get_cursor(0)
      local viewport = vim.fn.winsaveview()
      assert(render.apply_update(replica, {
        document = replica.document, operation_id = 1, phase = "accepted", diagnostic = {}, body = {},
        delta = {
          document = replica.document, base = 0, next = 1, removed = { 1 },
          file = { record(3, 1, destination_section) },
          section = {
            { kind = source_section, file = { 2 } },
            { kind = destination_section, file = { 3 } },
          },
          pending = { 1 },
        },
      }).kind == "Applied")
      local expected_row = math.min(position[1], vim.api.nvim_buf_line_count(replica.buffer))
      assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), { expected_row, position[2] }),
        direction .. " moved the cursor from the removed " .. location)
      assert(vim.fn.winsaveview().topline == viewport.topline,
        direction .. " scrolled the viewport")
      local settled_position = vim.api.nvim_win_get_cursor(0)
      assert(render.apply_update(replica, {
        document = replica.document, operation_id = 1, phase = "settled", diagnostic = {}, body = {},
        delta = { document = replica.document, base = 1, next = 2,
          removed = {}, file = {}, section = {}, pending = {} },
      }).kind == "Applied")
      assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), settled_position),
        direction .. " settlement moved the cursor")
      render.close(replica)
    end
  end
  assert(#notices == 1, table.concat(notices, "\n"))
end

T["status_snapshot_publication"] = function()
  local render = require("forge.status_render")
  local notices = {}

  local function snapshot(replica, revision, section)
    local value = fixture.snapshot(replica.document, { revision = revision })
    value.section = { { kind = section or "unstaged", file = {} } }
    value.file = {}
    for ordinal = 1, 75 do
      local record = fixture.file(("source_%02d.lua"):format(ordinal), ordinal)
      record.section = section or "unstaged"
      value.file[ordinal], value.section[1].file[ordinal] = record, ordinal
    end
    return value
  end

  local function open(name)
    local replica = render.open(name, { notice = function(message) notices[#notices + 1] = message end })
    vim.api.nvim_set_current_buf(replica.buffer)
    assert(render.apply_snapshot(replica, snapshot(replica, 0)).kind == "Applied")
    return replica
  end

  local function paused_snapshot(replica, value)
    local original_clock, original_schedule = vim.uv.hrtime, vim.schedule
    local instant, continuation = original_clock(), nil
    local pending = { completed = 0 }
    vim.uv.hrtime = function() instant = instant + 5000000 return instant end
    vim.schedule = function(callback)
      assert(not continuation, "snapshot queued multiple continuations before yielding")
      continuation = callback
    end
    local success, failure = pcall(render.apply_snapshot, replica, value, function(result)
      pending.completed = pending.completed + 1
      pending.result = result
    end)
    vim.uv.hrtime, vim.schedule = original_clock, original_schedule
    assert(success, failure)
    assert(continuation and not pending.result, "snapshot did not pause during preparation")
    pending.resume = continuation
    return pending
  end

  local function stage_delta(replica)
    local staged = snapshot(replica, 1, "staged")
    return { document = replica.document, base = 0, next = 1, file = staged.file,
      removed = {}, pending = {}, section = {
        { kind = "unstaged", file = {} }, staged.section[1],
      } }
  end

  local function assert_preserved(replica, pending)
    local text = buffer_support.lines(replica.buffer)
    local cursor = vim.api.nvim_win_get_cursor(0)
    local viewport = vim.fn.winsaveview()
    local changedtick, publication = replica.changedtick, replica.publication_revision
    pending.resume()
    assert(pending.completed == 1 and pending.result.kind == "Superseded", "outdated snapshot did not finish once")
    assert(replica.changedtick == changedtick and replica.publication_revision == publication,
      "outdated snapshot published another frame")
    eq(text, buffer_support.lines(replica.buffer), { fail_reason = "outdated snapshot replaced text" })
    eq(cursor, vim.api.nvim_win_get_cursor(0), { fail_reason = "outdated snapshot moved the cursor" })
    assert(viewport.topline == vim.fn.winsaveview().topline, "outdated snapshot moved the viewport")
  end

  for _, publication in ipairs({ "patch", "mutation" }) do
    local replica = open("snapshot-" .. publication)
    local pending = paused_snapshot(replica, snapshot(replica, 0))
    local delta = stage_delta(replica)
    local result = publication == "patch" and render.apply_patch(replica, delta)
      or render.apply_update(replica, { document = replica.document, delta = delta, body = {}, phase = "accepted" })
    assert(result.kind == "Applied")
    assert_preserved(replica, pending)
    assert(replica.inventory.revision == 1 and #replica.inventory.file == 75)
    assert(replica.inventory.section[1].kind == "staged", "staged section disappeared")
    render.close(replica)
  end

  for _, ahead in ipairs({ false, true }) do
    local replica = open(ahead and "snapshot-ahead" or "snapshot-body")
    assert(render.apply_body(replica, fixture.body(replica.document, { "original body" })).kind == "Applied")
    local pending = paused_snapshot(replica, snapshot(replica, ahead and 2 or 0))
    local body = fixture.body(replica.document, { "newer body" })
    body.snapshot.revision = 2
    assert(render.apply_body(replica, body).kind == "Applied")
    if ahead then
      pending.resume()
      wait_until(function() return pending.result ~= nil end, "needed snapshot was not prepared again")
      assert(pending.completed == 1 and pending.result.kind == "Applied")
      assert(replica.inventory.revision == 2, "needed inventory revision was discarded")
    else
      assert_preserved(replica, pending)
    end
    assert(replica.file[1].body.revision == 2, "snapshot replaced a newer diff body")
    render.close(replica)
  end

  local replica = open("snapshot-replaced")
  local pending = paused_snapshot(replica, snapshot(replica, 1))
  assert(render.apply_snapshot(replica, snapshot(replica, 2, "staged")).kind == "Applied")
  assert_preserved(replica, pending)
  assert(replica.inventory.revision == 2)
  assert(render.apply_snapshot(replica, snapshot(replica, 0)).kind == "Superseded", "late snapshot was admitted")
  render.close(replica)

  replica = open("snapshot-closed")
  pending = paused_snapshot(replica, snapshot(replica, 1))
  render.close(replica)
  pending.resume()
  assert(pending.completed == 1 and pending.result.kind == "Closed", "closing lost the pending completion callback")
  assert(#notices == 0, table.concat(notices, "\n"))
end

T["semantic"] = function()
  local root = vim.fn.getcwd()
  local render = require("forge.status_render")
  local input = require("forge.input")
  local notices = {}
  local replica = render.open("semantic-test", { notice = function(message) notices[#notices + 1] = message end })
  vim.api.nvim_win_set_buf(0, replica.buffer)
  local function record(id, path)
    return { id = id, generation = 1, section = "unstaged", change = "modified", path = path,
      header = fixture.header(path),
      untracked = false, stats = { state = "unknown" } }
  end
  local snapshot = { document = replica.document, revision = 0, view = { kind = "status" },
    head = { state = "attached", reference = "refs/heads/main", object = "abc123" }, context = vim.NIL,
    section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1, "a.lua"), record(2, "雪.lua") } }
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied", table.concat(notices, "\n"))
  local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
  eq(buffer_support.lines(replica.buffer), { "refs/heads/main abc123", "", "Unstaged changes (2):", "Modified a.lua", "Modified 雪.lua" })
  assert(require("forge.nodes").closed(replica,"file:1"), "file does not start collapsed")
  assert(not require("forge.nodes").closed(replica,"section:unstaged"), "section starts collapsed")
  vim.api.nvim_win_set_cursor(0, { 4, 0 })
  local captured = assert(input.capture(replica, view, "demand"))
  assert(captured.location.kind == "file" and captured.location.id == 1 and captured.block == nil)
  assert(require("forge.nodes").set_open(replica,0,"file:1",true))
  local header_metadata = fixture.metadata("group:10", 1)
  header_metadata.fold = { { id = "hunk-header:11", start = { row = 0, column = 0 },
    ["end"] = { block = "body:12", position = { row = 2, column = 0 } }, closed = false } }
  local delivery = { document = replica.document, file = 1, generation = 1, more = true, state = { state = "partial" },
    snapshot = { document = "body:1:1", revision = 1, block = {
      { id = "hunk-header:11", text = { "@@ +1 -1" }, metadata = header_metadata },
      { id = "body:12", text = { "old", "new" }, metadata = fixture.metadata("hunk:13", 2) },
    } } }
  assert(render.apply_body(replica, delivery).kind == "Applied", table.concat(notices, "\n"))
  eq(buffer_support.lines(replica.buffer), { "refs/heads/main abc123", "", "Unstaged changes (2):", "Modified a.lua", "@@ +1 -1", "old", "new", "Modified 雪.lua" })
  assert(replica.revision == 0, "body changed inventory revision")
  assert(not require("forge.nodes").closed(replica,"file:1"), "body expansion closed its file")
  local location = render.locate(replica, 6, 1)
  assert(location.location.kind == "body" and location.location.file == 1 and location.location.position.row == 1)
  assert(location.target == "hunk:13")
  assert(select(2, replica.sequence:position("file:2")) == 7)
  local selected = render.selection(replica, 4, 6)
  assert(#selected.target == 2 and selected.target[1].target == "group:10" and selected.target[2].target == "hunk:13")
  local body = replica.file[1].body
  local changed = record(1, "a.lua")
  changed.stats = { state = "exact", added = 2, deleted = 1 }
  changed.header = fixture.header("a.lua", 2, 1)
  assert(render.apply_patch(replica, { document = replica.document, base = 0, next = 1, removed = {}, section = {}, file = { changed } }).kind == "Applied")
  assert(replica.file[1].body == body and buffer_support.lines(replica.buffer)[4] == "Modified a.lua +2 -1", "count update lost body")
  local patch = { document = "body:1:1", base = 1, next = 2, base_rows = 3, next_rows = 4, base_blocks = 2, next_blocks = 3,
    text_edit = { { start_row = 3, removed_rows = 0, text = { "tail" } } },
    block_edit = { { start_block = 2, removed_blocks = 0, inserted = { "body:14" } } },
    metadata_edit = { { block = "body:14", row_count = 1, metadata = fixture.metadata("hunk:13", 1) } }, removed_block = {} }
  assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1, patch = patch }).kind == "Applied", table.concat(notices, "\n"))
  assert(buffer_support.lines(replica.buffer)[8] == "tail" and buffer_support.lines(replica.buffer)[9] == "Modified 雪.lua")
  changed = vim.deepcopy(changed)
  changed.generation = 2
  assert(render.apply_patch(replica, { document = replica.document, base = 1, next = 2, removed = {}, section = {}, file = { changed } }).kind == "Applied")
  assert(replica.file[1].body == nil and #buffer_support.lines(replica.buffer) == 6, "changed generation retained stale body")
  assert(render.apply_body(replica, delivery).kind == "Discarded", "late body crossed generation")
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  assert(input.capture(replica, view, "navigate").location.kind == "boundary")
  assert(render.locate(replica, 4, 0).target == nil, "placeholder selected its file")
  local restored = vim.deepcopy(replica.inventory)
  vim.bo[replica.buffer].modifiable = true
  buffer_support.set(replica.buffer, { "external change" })
  vim.bo[replica.buffer].modifiable = false
  assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 2 }).kind == "Desynchronized")
  assert(render.apply_snapshot(replica, restored).kind == "Applied", "inventory recovery rejected external text replacement")
  assert(#buffer_support.lines(replica.buffer) == 6 and buffer_support.lines(replica.buffer)[4] == "Modified a.lua +2 -1")
  assert(#notices == 1 and notices[1]:find("externally", 1, true))
  notices = {}
  assert(#notices == 0, table.concat(notices, "\n"))
  input.close(view)
  render.close(replica)

  local context = { workspace = root, branch = "main", head = { oid = string.rep("a", 40), reference = "main", subject = "雪の修正 é" },
    recent = { { oid = string.rep("a", 40), reference = "main", subject = "雪の修正 é" } }, issues = { 12 } }
  replica = render.open("context-test", { notice = function(message) notices[#notices + 1] = message end })
  vim.api.nvim_win_set_buf(0, replica.buffer)
  replica.width = 22
  snapshot = vim.deepcopy(snapshot)
  snapshot.document, snapshot.context = replica.document, context
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
  local initial_context_entry = replica.root:at(0)
  render.present_context(replica, { pr = { state = "fetching", text = "" }, about = { state = "none", text = "" } })
  assert(replica.root:at(0) == initial_context_entry, "unchanged context rebuilt the document")
  local root_sequence, file_header = replica.root, replica.file[1].header
  local original_revision = replica.revision
  render.present_context(replica, { pr = { state = "ready", text = "日本語を含む長い題名 é é é" }, about = { state = "ready", text = "summary\nbody" } })
  assert(replica.root == root_sequence and replica.file[1].header == file_header, "context rebuilt file inventory")
  assert(replica.revision == original_revision, "local presentation changed native revision")
  local pr = replica.source_entry["status:context:pr"]
  assert(pr.row_count == 1, "context uses native soft wrapping")
  for index, text in ipairs(pr.text) do
    local rendered = {}
    for _, chunk in ipairs(pr.chunk[index]) do rendered[#rendered + 1] = chunk[1] end
    assert(table.concat(rendered) == text, "highlight byte ranges lost wrapped Unicode text")
  end
  replica.width = 80
  render.present_context(replica, replica.presentation)
  assert(replica.block["status:context:pr"].row_count == 1)
  assert(replica.root == root_sequence and #notices == 0, table.concat(notices, "\n"))
  render.close(replica)

  replica = render.open("relative-path-test", { notice = function(message) notices[#notices + 1] = message end })
  replica.display_cwd, replica.workspace = vim.fs.joinpath(root, "nvim"), root
  snapshot = fixture.snapshot(replica.document, { path = "a.lua" })
  snapshot.context = { workspace = root, head = vim.NIL, branch = "main", upstream = vim.NIL, push = vim.NIL,
    recent = {}, issues = {} }
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied", table.concat(notices, "\n"))
  assert(vim.tbl_contains(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), "Modified ../a.lua"))
  assert(replica.file[1].display_path == "../a.lua")
  assert(replica.file[1].record.path == "a.lua", "display formatting changed the native action path")
  local relative_delivery = { document = replica.document, file = 1, generation = 1, more = false,
    state = { state = "ready" }, snapshot = { document = "body:1:1", revision = 1,
      block = { { id = "relative-body", text = { "body" }, metadata = fixture.metadata("relative-hunk", 1) } } } }
  assert(render.apply_body(replica, relative_delivery).kind == "Applied")
  assert(vim.tbl_contains(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), "Modified ../a.lua"),
    "body delivery reset the relative file path")
  local untracked_snapshot = fixture.snapshot(replica.document, {
    revision = 1, path = "new",
  })
  untracked_snapshot.file[1].section, untracked_snapshot.file[1].untracked = "untracked", true
  table.insert(untracked_snapshot.file[1].header, { text = " new", capture = "ForgeStatusPath" })
  assert(render.apply_snapshot(replica, untracked_snapshot).kind == "Applied")
  assert(vim.tbl_contains(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), "Modified ../new new"),
    "path formatting changed the untracked-file marker")
  local relative_case = {
    { cwd = root, path = "nvim/lua/source.lua", expected = "nvim/lua/source.lua" },
    { cwd = vim.fs.joinpath(root, "nvim"), path = "nvim/lua/source.lua", expected = "lua/source.lua" },
    { cwd = vim.fs.joinpath(root, "nvim/lua"), path = "nvim/tests/missing 雪.lua", expected = "../tests/missing 雪.lua" },
    { cwd = vim.fs.joinpath(root, "nvim/lua"), path = "nvim-other/source.lua", expected = "../../nvim-other/source.lua" },
  }
  for _, case in ipairs(relative_case) do
    replica.display_cwd = case.cwd
    local path_snapshot = fixture.snapshot(replica.document, {
      revision = replica.revision + 1, path = case.path,
      view = { kind = "comparison", title = "HEAD", path = case.path, worktree = true },
    })
    path_snapshot.file[1].header = fixture.header(case.path, 2, 1)
    assert(render.apply_snapshot(replica, path_snapshot).kind == "Applied", table.concat(notices, "\n"))
    local path_lines = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
    assert(vim.tbl_contains(path_lines, "Modified " .. case.expected .. " +2 -1"), vim.inspect(path_lines))
    assert(vim.tbl_contains(path_lines, "File:   " .. case.expected), "comparison retained its repository-relative path")
    assert(replica.inventory.file[1].path == case.path, "relative display changed repository identity")
  end
  render.close(replica)
end

return T
