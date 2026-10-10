vim.loader.enable(false)
local replica = require("forge.buffer")
local gutter = require("forge.gutter")
local commands = require("forge.document_commands")
local owner = replica.open("row-prefix")
local selection, clipboard
local original_clipboard = vim.g.clipboard
vim.g.clipboard = { name = "Prefix fixture", copy = {
  ["+"] = function(lines) clipboard = lines end, ["*"] = function() end,
}, paste = { ["+"] = function() return { {}, "V" } end, ["*"] = function() return { {}, "V" } end } }

local function metadata(text, indent, source_indent)
  local result = { target = {}, decoration = {}, editable_region = {},
    layout = { indent = indent, source_indent = source_indent }, gutter = {} }
  for row = 0, #text - 1 do
    result.gutter[#result.gutter + 1] = { position = { row = row, column = 0 }, priority = 100,
      chunk = { { text = "  8 ", capture = "LineNr" }, { text = "+ ", capture = "DiffAdd" } } }
  end
  return result
end

local function inline_marks()
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(owner.buffer, owner.namespace, 0, -1, { details = true })) do
    if mark[4].virt_text_pos == "inline" then result[mark[2]] = (result[mark[2]] or 0) + 1 end
  end
  return result
end

local ok, failure = xpcall(function()
  vim.api.nvim_set_current_buf(owner.buffer)
  vim.wo.virtualedit = "all"
  selection = commands.attach_selection(owner, { normalize = true })
  local text = { "pub struct Player;", "    let nested = 1;", "", "\tlet tabbed = 2;", 'let 名字 = "🙂";' }
  for _, indent in ipairs({ 2, 4, 6 }) do
    local revision = indent
    assert(replica.apply_snapshot(owner, { document = owner.document, revision = revision, block = {
      { id = "source", text = text, metadata = metadata(text, indent) },
    } }).kind == "Applied")
    local expected = string.rep(" ", indent - 2) .. "  8 + "
    local marks = inline_marks()
    for row, source in ipairs(text) do
      local bounds = assert(gutter.bounds(owner, row))
      assert(bounds.width == #expected and bounds.column == 0, "prefix width omitted timeline indentation")
      assert(#bounds.insertion == 1 and marks[row - 1] == 1, "prefix has competing inline decorations")
      local insertion = bounds.insertion[1]
      assert(insertion.chunk[#insertion.chunk].capture == "DiffAdd", "gutter capture was replaced")
      if indent > 2 then assert(insertion.chunk[1].capture == "Normal", "outer indentation inherited diff color") end
      vim.api.nvim_win_set_cursor(0, { row, 0 })
      gutter.normalize(owner, false)
      assert(vim.fn.getcurpos()[4] == #expected, "cursor geometry differs from prefix")
      vim.cmd("normal! yy")
      assert(vim.fn.getreg('"') == source .. "\n", "ordinary yank included UI decoration")
      selection.start()
      vim.fn.maparg("<Space>l", "x", false, true).callback()
      assert(vim.deep_equal(clipboard, { expected .. source, "" }), "copy omitted or duplicated prefix")
    end
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(owner.buffer, 0, -1, true), text), "layout changed source bytes")
  end

  local text = { "  already indented", "tail" }
  local details = metadata(text, 4, 2)
  details.gutter[#details.gutter + 1] = { position = { row = 0, column = 2 }, priority = 100,
    chunk = { { text = "→ ", capture = "Comment" } } }
  details.gutter[#details.gutter + 1] = { position = { row = 0, column = 0 }, priority = 200,
    placement = "sign", chunk = { { text = "▾", capture = "Comment" } } }
  assert(replica.apply_snapshot(owner, { document = owner.document, revision = 8, block = {
    { id = "source", text = text, metadata = details },
  } }).kind == "Applied")
  local bounds = gutter.bounds(owner, 1)
  assert(bounds.width == 6 and #bounds.insertion == 2, "materialized indentation or sign counted twice")
  assert(bounds.insertion[2].position.column == 2 and bounds.insertion[2].width == 2)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  selection.start()
  vim.fn.maparg("<Space>l", "x", false, true).callback()
  assert(clipboard[1] == "  8 +   → already indented", "nonzero source insertion moved")

  local updated = metadata({ "tail", "more" }, 6)
  assert(replica.apply_patch(owner, { document = owner.document, base = 8, next = 9,
    base_rows = 2, next_rows = 2, base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = 2, text = { "tail", "more" } } },
    metadata_edit = { { block = "source", row_count = 2, metadata = updated } },
  }).kind == "Applied")
  assert(gutter.bounds(owner, 1).width == 10 and inline_marks()[0] == 1, "replacement retained stale prefix")

  selection.close()
  selection = nil
  replica.close(owner)
  owner = replica.open("folded-prefix")
  vim.api.nvim_set_current_buf(owner.buffer)
  local heading = { target = {}, decoration = {}, editable_region = {}, fold = {
    { id = "body", start = { row = 0, column = 0 },
      ["end"] = { block = "source", position = { row = 2, column = 0 } }, closed = false },
  } }
  local applied = replica.apply_snapshot(owner, { document = owner.document, revision = 10, block = {
    { id = "heading", text = { "Source" }, metadata = heading },
    { id = "source", text = { "tail", "more" }, metadata = updated },
  } })
  assert(applied.kind == "Applied", vim.inspect(applied))
  assert(replica.set_expansion(owner, "body", false))
  assert(owner.row_count == 1)
  assert(replica.set_expansion(owner, "body", true))
  assert(owner.row_count == 3 and gutter.bounds(owner, 2).width == 10 and inline_marks()[1] == 1,
    "fold rematerialization lost the shared prefix")
end, debug.traceback)
if selection then selection.close() end
replica.close(owner)
vim.g.clipboard = original_clipboard
assert(ok, failure)

local runtime = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2))))
local job = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
assert(job > 0)
local function run(source, arguments) return vim.rpcrequest(job, "nvim_exec_lua", source, arguments or {}) end
ok, failure = xpcall(function()
  vim.rpcrequest(job, "nvim_ui_attach", 44, 16, { rgb = true })
  run([[
    local runtime = ...
    vim.opt.rtp:prepend(runtime)
    vim.o.laststatus, vim.o.showtabline, vim.o.showmode, vim.o.ruler = 0, 0, false, false
    _G.fixture = require("forge.buffer").open("render-prefix")
    vim.api.nvim_set_current_buf(fixture.buffer)
    vim.wo.number, vim.wo.relativenumber, vim.wo.signcolumn, vim.wo.foldcolumn = false, false, "no", "0"
    vim.wo.linebreak, vim.wo.breakindent = true, true
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0x000000 })
    vim.api.nvim_set_hl(0, "DiffAdd", { fg = 0x00ff00, bg = 0x002800 })
  ]], { runtime })
  for _, width in ipairs({ 32, 60 }) do
    vim.rpcrequest(job, "nvim_ui_try_resize", width, 16)
    for _, wrapped in ipairs({ false, true }) do
      for _, indent in ipairs({ 2, 4, 6 }) do
        local screen = run([[
          local indent, wrapped = ...
          vim.wo.wrap = wrapped
          local text = { "pub struct Player;", "    nested source line with several words and a long wrapped ending" }
          local metadata = { target = {}, editable_region = {}, decoration = {},
            layout = { indent = indent }, gutter = {} }
          for row = 0, 1 do
            metadata.gutter[#metadata.gutter + 1] = { position = { row = row, column = 0 }, priority = 100,
              chunk = { { text = "  8 + ", capture = "DiffAdd" } } }
            metadata.decoration[#metadata.decoration + 1] = { priority = 90, capture = "DiffAdd",
              range = { start = { row = row, column = 0 }, ["end"] = { row = row + 1, column = 0 } } }
          end
          assert(require("forge.buffer").apply_snapshot(fixture, { document = fixture.document,
            revision = (fixture.revision or 0) + 1, block = { { id = "source", text = text, metadata = metadata } } }).kind == "Applied")
          vim.cmd("normal! gg0")
          vim.cmd("redraw!")
          local rows = {}
          for row = 1, 4 do
            local cells = {}
            for column = 1, vim.o.columns do cells[#cells + 1] = vim.fn.screenstring(row, column) end
            rows[row] = table.concat(cells)
          end
          return rows
        ]], { indent, wrapped })
        assert(vim.trim(screen[1]) == "8 + pub struct Player;", "gutter split source from indentation: " .. vim.inspect(screen))
        assert(screen[1]:sub(1, indent + 4) == string.rep(" ", indent - 2) .. "  8 + ",
          "outer indentation followed the gutter: " .. screen[1])
        if wrapped then
          assert(not screen[3]:find("8 +", 1, true), "wrapped continuation repeated line number")
        end
      end
  end
  end
end, debug.traceback)
vim.fn.jobstop(job)
assert(ok, failure)
print("row_prefix: shared rendering, cursor, copy, replacement, folds, and wrapping passed")
vim.cmd("qa!")
