vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local render = require("forge.status_render")
local failures = {}
for _, moved_id in ipairs({ 1, 3 }) do
  local replica = render.open("gutter-relocation-" .. moved_id, {})
  local ok, failure = xpcall(function()
    vim.api.nvim_win_set_buf(0, replica.buffer)
    vim.wo.wrap = true

    local function record(id, section)
      return { id = id, generation = 1, section = section, change = "modified", path = "source_" .. id .. ".rs", untracked = false, stats = { state = "exact", added = 30, deleted = 0 } }
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
    for id = 1, 3 do assert(render.apply_body(replica, delivery(id)).kind == "Applied") end
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
print("status gutter relocation passed")
