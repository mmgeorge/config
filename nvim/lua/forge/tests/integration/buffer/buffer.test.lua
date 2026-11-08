-- Integration tests for the `buffer` area. Each scenario row runs in its own headless Neovim:
-- `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`. Tests that launch the Forge host
-- assume `rust-build` has produced a current executable.
local scenarios = {
  { name = "document_host_generation", run = function()
local fixture = dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/support/status_fixture.lua")
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local generation, accepting = 1, true
local requests, notices = {}, {}
local subscription = {}
local delayed
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
package.loaded["forge.client"] = {
  host_get_generation = function() return generation end,
  host_is_accepting = function() return accepting end,
  document_events_subscribe = function(document, callback)
    subscription[document] = callback
    return function() subscription[document] = nil end
  end,
  request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    assert(accepting, "document request resurrected a stopped host")
    requests[#requests + 1] = { method, params.operation }
    if params.operation == "open" then
      local snapshot = { document = params.document, revision = 1, block = {
        { id = "source", text = { "captured source" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
      } }
      if method == "repository.status.document" then callback(fixture.snapshot(params.document, { revision = 1 }))
      else callback({ title = "HEAD", object = "HEAD", revision = "HEAD", snapshot = snapshot, more = false }) end
    elseif params.operation == "refresh" then delayed = callback
    elseif params.operation == "close_view" then callback(nil)
    else callback({}) end
  end,
}
local status = require("forge.status")
local source = require("forge.source_document")
local parent = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return parent.replica.status == "Applied" and not parent.request_active end))
local child = source.open({ workspace = vim.fn.getcwd(), path = "history.txt", revision = "HEAD" })
assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == child.replica.buffer end))
accepting = false
local before = #requests
source.close(child)
assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == parent.replica.buffer end))
vim.wait(30, function() return false end)
assert(#requests == before, "source close or status redisplay contacted a dead host")
assert(#notices == 0, table.concat(notices, "\n"))
generation, accepting = 2, true
status.demand(parent)
status.refresh(parent)
assert(vim.wait(1000, function() return #notices > 0 end))
assert(notices[#notices]:find("refresh or reopen", 1, true))
assert(#requests == before, "stale status request reached the replacement host")
local notice_count = #notices
status.action(parent, "open")
assert(#notices == notice_count + 1 and notices[#notices]:find("refresh or reopen", 1, true))
status.close(parent)
assert(vim.wait(1000, function() return not parent.request_active end))
local replacement = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return replacement.replica.status == "Applied" and not replacement.request_active end))
assert(replacement.host_generation == 2)
status.refresh(replacement)
assert(delayed)
generation = 3
delayed({ document = replacement.document, base = 1, next = 2 })
assert(vim.wait(1000, function() return not replacement.request_active end))
assert(replacement.replica.revision == 1, "old-generation response modified the replica")
status.close(replacement)
assert(vim.wait(1000, function() return not replacement.request_active end))
local late_source = source.open({ workspace = vim.fn.getcwd(), path = "late.txt", revision = "HEAD" })
generation = 4
assert(vim.wait(1000, function() return not late_source.active end))
assert(late_source.replica.status == "Closed", "late source open remained usable after host collection")
assert(next(subscription) == nil, "closed status retained its document subscription")
print("native document host generation lifecycle passed")
  end },
  { name = "markdown_viewport", run = function()
vim.loader.enable(false)
local data = vim.fn.stdpath("data")
vim.opt.runtimepath:append(data .. "/site")
vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
require("render-markdown").setup(require("plugins.markdown")[1].opts())
local client = require("forge.client")
local requests = {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/lua/forge/tests/support/presentation_status.lua")(params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
package.loaded["forge.views.harness.terminals"] = { watch = function() return { close = function() end } end }
local markdown = require("forge.render.harness.markdown")
local render, rendered = markdown.render, 0
markdown.render = function(...)
  rendered = rendered + 1
  return render(...)
end
-- Count sequence visits only inside Markdown viewport selection; other refresh observers, such as
-- the response reveal, walk the sequence for their own purposes.
local viewport, markdown_visits = markdown.viewport, 0
markdown.viewport = function(document, ...)
  local before = document.sequence.visits
  local range_list = viewport(document, ...)
  markdown_visits = markdown_visits + document.sequence.visits - before
  return range_list
end
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local owner = require("forge.views.harness.presentation").open({ session_id = "markdown-viewport",
  transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
  is_alive = function() return true end, notice = function(message) error(message) end,
}, function(value, failure) assert(value, failure) end)
local block = {}
for index = 1, 3000 do
  local text = {}
  for row = 1, 10 do text[row] = "settled row " .. index .. ":" .. row end
  block[index] = { id = "history:" .. index, text = text,
    metadata = { target = {}, decoration = {}, editable_region = {}, fold = {}, markdown = index == 1 or index == 3000 } }
end
block[1].text[1], block[3000].text[1] = "# First message", "# Last message"
requests[1].callback({ transcript = { document = owner.document, revision = 0, block = block } })
assert(vim.wait(10000, function() return owner.ready end, 1), "snapshot did not settle")
vim.cmd("redraw")
assert(owner.markdown_ranges[1].id == "history:1" and #owner.markdown_ranges == 1)
for revision = 1, 100 do
  markdown_visits = 0
  local count = #requests
  owner.sync()
  assert(vim.wait(1000, function() return #requests > count end, 1), "refresh did not leave the frame queue")
  requests[#requests].callback({ patch = { {
    document = owner.document, base = revision - 1, next = revision,
    base_rows = 30000, next_rows = 30000, base_blocks = 3000, next_blocks = 3000,
    block_edit = {}, removed_block = {}, metadata_edit = {},
    text_edit = { { start_row = 29999, removed_rows = 1, text = { "output " .. revision } } },
  } } })
  assert(owner.transcript.revision == revision)
  assert(markdown_visits < 300, "Markdown traversed off-screen history")
end
-- Every buffer change re-renders the visible Markdown, because an edit outside a message can
-- extend Tree-sitter's included region at its boundary; only the newly visible range is checked.
local rendered_before_scroll = rendered
vim.api.nvim_win_set_cursor(window, { 29991, 0 })
vim.cmd("normal! zt")
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = transcript })
assert(vim.wait(1000, function() return owner.markdown_ranges[1].id == "history:3000" end, 1))
assert(rendered > rendered_before_scroll, "scrolling did not render the newly visible message")
vim.cmd("split")
local second = vim.api.nvim_get_current_win()
owner.views[second] = require("forge.input").open(owner.transcript, second)
vim.api.nvim_win_set_cursor(second, { 1, 0 })
vim.cmd("normal! zt")
owner.resize()
assert(#owner.markdown_ranges == 2, "split windows lost one visible Markdown region")
assert(owner.markdown_ranges[1].id == "history:1" and owner.markdown_ranges[2].id == "history:3000")
owner.close()
print("markdown_viewport OK: 30,000 rows, 100 retained-message updates, scrolling and splits")
vim.cmd("qa!")
  end },
  { name = "row_prefix", run = function()
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

local runtime = vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p"))
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
  end },
  { name = "source", run = function()
vim.loader.enable(false)

local root = vim.fs.normalize(vim.fn.getcwd())
local fixture = vim.fn.tempname()
local data_directory = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local executable_name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local build = require("forge.builder").binary_path()
assert(vim.fn.executable(build) == 1, "build the selected Forge host before this integration test")
assert(vim.fn.mkdir(data_directory, "p") == 1)
local executable = data_directory .. "/" .. executable_name
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
executable = dofile("nvim/lua/forge/tests/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  if kind == "data" then return data_directory end
  return original_stdpath(kind)
end
package.loaded["forge.builder"] = {
  ensure = function(callback)
    vim.schedule(function() callback({ ok = true, path = executable }) end)
    return function() end
  end,
}
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local source = require("forge.source_document")
local notices, state = {}, nil
local function git(arguments)
  local command = { "git", "-C", fixture }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.test" })
  local lines = {}
  for index = 1, 700 do lines[index] = ("local value_%d = %d"):format(index, index) end
  assert(vim.fn.writefile(lines, fixture .. "/source file.lua") == 0)
  git({ "add", "source file.lua" })
  git({ "commit", "--quiet", "-m", "Source fixture" })
  local origin = vim.api.nvim_get_current_buf()
  state = source.open({ workspace = fixture, path = "source file.lua", revision = "HEAD", line = 700,
    is_current = function() return vim.api.nvim_get_current_buf() == origin end,
    on_error = function(message) notices[#notices + 1] = tostring(message) end })
  assert(vim.wait(10000, function()
    return #notices > 0 or (state.replica.status == "Applied" and state.replica.row_count == 700 and not state.more)
  end, 10), "source delivery did not finish")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "source did not bind its initiating window")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), lines), "native source rows changed")
  assert(vim.api.nvim_win_get_cursor(0)[1] == 700, "requested source row was not reached across batches")
  assert(vim.bo[state.replica.buffer].readonly and not vim.bo[state.replica.buffer].modifiable)
  assert(vim.b[state.replica.buffer].forge_native_document, "native source ownership missing")
  assert(not client._client.harness_ready, "source open initialized Harness")
  local snapshot
  client.request({ method = "repository.source.document", params = { operation = "snapshot", document = state.document } },
    function(result, request_failure)
      if request_failure then notices[#notices + 1] = tostring(request_failure) else snapshot = result end
    end)
  assert(vim.wait(3000, function() return snapshot ~= nil or #notices > 0 end, 10), "native source snapshot did not arrive")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(snapshot.document == state.document and snapshot.revision == state.replica.revision)
  assert(snapshot.more == false and snapshot.diff_view == nil, "plain source snapshot acquired diff controls")
  local captured = {}
  for _, block in ipairs(snapshot.block) do vim.list_extend(captured, block.text) end
  assert(vim.deep_equal(captured, lines), "source recovery snapshot differs from captured rows")
  local closed = false
  source.close(state, function() closed = true end)
  assert(vim.wait(3000, function() return closed end, 10), "native source close did not settle")
  state = nil
end, debug.traceback)
if state then source.close(state) end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
vim.fn.delete(fixture, "rf")
vim.fn.delete(data_directory, "rf")
if not ok or not collected then
  vim.api.nvim_err_writeln(failure or "source host process was not collected")
  vim.cmd("cquit 1")
end
print("source_host OK: 700 exact rows, multiple native batches, no Harness session")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
