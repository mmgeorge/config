local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local status_fixture = dofile("nvim/lua/forge/tests/support/status_fixture.lua")
local presentation_status = dofile("nvim/lua/forge/tests/support/presentation_status.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["document_host_generation"] = function()
  local generation, accepting = 1, true
  local requests = {}
  local notices = notify.capture()
  local subscription = {}
  local delayed
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
        if method == "repository.status.document" then callback(status_fixture.snapshot(params.document, { revision = 1 }))
        else callback({ title = "HEAD", object = "HEAD", revision = "HEAD", snapshot = snapshot, more = false }) end
      elseif params.operation == "refresh" then delayed = callback
      elseif params.operation == "close_view" then callback(nil)
      else callback({}) end
    end,
  }
  local status = require("forge.status")
  local source = require("forge.source_document")
  local parent = status.open({ workspace = vim.fn.getcwd() })
  wait_until(function() return parent.replica.status == "Applied" and not parent.request_active end,
    "status document did not apply its first snapshot")
  local child = source.open({ workspace = vim.fn.getcwd(), path = "history.txt", revision = "HEAD" })
  wait_until(function() return vim.api.nvim_get_current_buf() == child.replica.buffer end,
    "source document did not become the current buffer")
  accepting = false
  local before = #requests
  source.close(child)
  wait_until(function() return vim.api.nvim_get_current_buf() == parent.replica.buffer end,
    "closing the source did not redisplay the status document")
  vim.wait(30, function() return false end, 5)
  assert(#requests == before, "source close or status redisplay contacted a dead host")
  notices:assert_none()
  generation, accepting = 2, true
  status.demand(parent)
  status.refresh(parent)
  wait_until(function() return #notices > 0 end, "stale status refresh did not report the replaced host")
  assert(notices[#notices].message:find("refresh or reopen", 1, true))
  assert(#requests == before, "stale status request reached the replacement host")
  local notice_count = #notices
  status.action(parent, "open")
  assert(#notices == notice_count + 1 and notices[#notices].message:find("refresh or reopen", 1, true))
  status.close(parent)
  wait_until(function() return not parent.request_active end, "closing the stale status did not settle")
  local replacement = status.open({ workspace = vim.fn.getcwd() })
  wait_until(function() return replacement.replica.status == "Applied" and not replacement.request_active end,
    "replacement status document did not apply its snapshot")
  assert(replacement.host_generation == 2)
  status.refresh(replacement)
  assert(delayed)
  generation = 3
  delayed({ document = replacement.document, base = 1, next = 2 })
  wait_until(function() return not replacement.request_active end, "old-generation refresh response did not settle")
  assert(replacement.replica.revision == 1, "old-generation response modified the replica")
  status.close(replacement)
  wait_until(function() return not replacement.request_active end, "closing the replacement status did not settle")
  local late_source = source.open({ workspace = vim.fn.getcwd(), path = "late.txt", revision = "HEAD" })
  generation = 4
  wait_until(function() return not late_source.active end, "late source open did not settle after host collection")
  assert(late_source.replica.status == "Closed", "late source open remained usable after host collection")
  assert(next(subscription) == nil, "closed status retained its document subscription")
end

T["markdown_viewport"] = function()
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

    callback = presentation_status(params, callback)
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
  local transcript = buffer.scratch()
  local composer = buffer.scratch()
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
  wait_until(function() return owner.ready end, "snapshot did not settle", { timeout_ms = 4000 })
  vim.cmd("redraw")
  assert(owner.markdown_ranges[1].id == "history:1" and #owner.markdown_ranges == 1)
  -- A highlighter that hides whole lines parses each visible row inside the buffer edit itself.
  local parser = assert(vim.treesitter.get_parser(transcript, "markdown"))
  local parse, edit_parses, editing = parser.parse, 0, false
  parser.parse = function(tree, range, on_parse)
    if editing then edit_parses = edit_parses + 1 end
    return parse(tree, range, on_parse)
  end
  local set_text = vim.api.nvim_buf_set_text
  vim.api.nvim_buf_set_text = function(...)
    editing = true
    local ok, failure = pcall(set_text, ...)
    editing = false
    assert(ok, failure)
  end
  MiniTest.finally(function() vim.api.nvim_buf_set_text = set_text end)
  for revision = 1, 100 do
    markdown_visits = 0
    local count = #requests
    owner.sync()
    wait_until(function() return #requests > count end, "refresh did not leave the frame queue")
    requests[#requests].callback({ patch = { {
      document = owner.document, base = revision - 1, next = revision,
      base_rows = 30000, next_rows = 30000, base_blocks = 3000, next_blocks = 3000,
      block_edit = {}, removed_block = {}, metadata_edit = {},
      text_edit = { { start_row = 29999, removed_rows = 1, text = { "output " .. revision } } },
    } } })
    assert(owner.transcript.revision == revision)
    assert(markdown_visits < 300, "Markdown traversed off-screen history")
  end
  -- Scrolling brings another message within a page of the window, which always needs a pass. The
  -- message is rendered before it is visible, so its rows do not change height once it scrolls in.
  local rendered_before_scroll = rendered
  vim.api.nvim_win_set_cursor(window, { 29985, 0 })
  vim.cmd("normal! zb")
  assert(vim.fn.line("w$") < 29991, "the last message is already visible")
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = transcript })
  wait_until(function() return owner.markdown_ranges[1].id == "history:3000" end,
    "scrolling did not select the Markdown range of the message below the window")
  assert(rendered > rendered_before_scroll, "scrolling did not render the approaching message")
  vim.cmd("split")
  local second = vim.api.nvim_get_current_win()
  owner.views[second] = require("forge.input").open(owner.transcript, second)
  vim.api.nvim_win_set_cursor(second, { 1, 0 })
  vim.cmd("normal! zt")
  owner.resize()
  assert(#owner.markdown_ranges == 2, "split windows lost one visible Markdown region")
  assert(owner.markdown_ranges[1].id == "history:1" and owner.markdown_ranges[2].id == "history:3000")
  -- With concealment on, a highlighter that hides whole lines answers each visible row's
  -- concealment by parsing inside the edit, so one drawn window is enough to expose it.
  assert(vim.treesitter.highlighter.active[transcript], "transcript lost its Tree-sitter highlighter")
  vim.wo[second].conceallevel = 3
  vim.cmd("redraw")
  vim.bo[transcript].modifiable, vim.bo[transcript].readonly = true, false
  edit_parses = 0
  vim.api.nvim_buf_set_text(transcript, 1, 0, 1, 0, { "changed " })
  assert(edit_parses == 0, "a transcript edit ran " .. edit_parses .. " synchronous Tree-sitter parses")
  -- A pass is skipped while the parser's regions still equal the message ranges. An edit in
  -- another block keeps them equal; rows inserted exactly at a message's end stretch its region.
  local markdown = require("forge.render.harness.markdown")
  owner.resize()
  wait_until(function() return markdown.current(transcript, owner.markdown_ranges) end,
    "a settled render was not reported current")
  local message_end = owner.markdown_ranges[1].after0
  vim.api.nvim_buf_set_lines(transcript, message_end + 3, message_end + 4, true, { "elsewhere" })
  assert(markdown.current(transcript, owner.markdown_ranges), "an edit outside every message required a pass")
  vim.api.nvim_buf_set_lines(transcript, message_end, message_end, true, { "appended at the boundary" })
  assert(not markdown.current(transcript, owner.markdown_ranges), "a stretched parser region went undetected")
  owner.close()
end

T["markdown_follows_scroll_inside_a_message"] = function()
  local data = vim.fn.stdpath("data")
  vim.opt.runtimepath:append(data .. "/site")
  vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
  vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
  require("render-markdown").setup({})
  local client = require("forge.client")
  local requests = {}
  client.host_is_accepting = function() return true end
  client.request = function(request_specification, callback)
    local params = request_specification.params or {}
    callback = presentation_status(params, callback)
    requests[#requests + 1] = { params = params, callback = callback }
  end
  package.loaded["forge.views.harness.terminals"] = { watch = function() return { close = function() end } end }
  local transcript, composer = buffer.scratch(), buffer.scratch()
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local owner = require("forge.views.harness.presentation").open({ session_id = "markdown-scroll",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, failure) assert(value, failure) end)
  -- One message far taller than the window, with a heading on every row so each row carries a mark.
  local text = {}
  for row = 1, 600 do text[row] = "# Heading " .. row end
  requests[1].callback({ transcript = { document = owner.document, revision = 0, block = {
    { id = "message", text = text, metadata = { target = {}, decoration = {}, editable_region = {}, markdown = true } },
  } } })
  wait_until(function() return owner.ready end, "snapshot did not settle")
  local namespace = vim.api.nvim_get_namespaces()["render-markdown.nvim"]
  local function rendered(row)
    return #vim.api.nvim_buf_get_extmarks(transcript, namespace, { row - 1, 0 }, { row - 1, -1 }, {}) > 0
  end
  wait_until(function() return rendered(1) end, "the visible rows did not render")
  local height = vim.api.nvim_win_get_height(window)
  assert(rendered(2 * height - 1), "rows within a window height below the window were not rendered")
  -- A scroll that leaves the cursor in the window fires no cursor event, and the message near the
  -- window is the same one, so only the scroll itself can ask for the rows now approaching.
  local pass_ranges = owner.markdown_ranges
  vim.api.nvim_win_set_cursor(window, { 3 * height, 0 })
  vim.cmd("normal! zb")
  vim.api.nvim_exec_autocmds("WinScrolled", {})
  wait_until(function() return rendered(4 * height - 1) end, "a scroll inside one message did not render the approaching rows")
  assert(owner.markdown_ranges == pass_ranges, "the scroll ran a full Markdown pass")
  owner.close()
end

T["row_prefix"] = function()
  local replica = require("forge.buffer")
  local gutter = require("forge.gutter")
  local commands = require("forge.document_commands")
  local owner = replica.open("row-prefix")
  local selection, clipboard
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

  MiniTest.finally(function()
    if selection then pcall(selection.close) end
    pcall(replica.close, owner)
  end)
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
      keymap.invoke("<Space>l", { mode = "x" })
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
  keymap.invoke("<Space>l", { mode = "x" })
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

  local runtime = vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p"))
  local child = MiniTest.new_child_neovim()
  child.start()
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(44, 16, { rgb = true })
  child.lua([[
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
    child.api.nvim_ui_try_resize(width, 16)
    for _, wrapped in ipairs({ false, true }) do
      for _, indent in ipairs({ 2, 4, 6 }) do
        child.lua([[
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
        ]], { indent, wrapped })
        local screenshot = child.get_screenshot()
        local screen = {}
        for row = 1, 4 do screen[row] = table.concat(screenshot.text[row]) end
        assert(vim.trim(screen[1]) == "8 + pub struct Player;", "gutter split source from indentation: " .. vim.inspect(screen))
        assert(screen[1]:sub(1, indent + 4) == string.rep(" ", indent - 2) .. "  8 + ",
          "outer indentation followed the gutter: " .. screen[1])
        if wrapped then
          assert(not screen[3]:find("8 +", 1, true), "wrapped continuation repeated line number")
        end
      end
    end
  end
end

T["source"] = function()
  local source, state
  -- Close the source and stop the host before the temporary directories registered below are deleted.
  MiniTest.finally(function()
    if state then pcall(source.close, state) end
    pcall(host.stop)
  end)
  local client = host.start().client
  source = require("forge.source_document")
  local notices = {}
  local lines = {}
  for index = 1, 700 do lines[index] = ("local value_%d = %d"):format(index, index) end
  local fixture = git_repository.create({ files = { ["source file.lua"] = lines }, message = "Source fixture" }).path
  local origin = vim.api.nvim_get_current_buf()
  state = source.open({ workspace = fixture, path = "source file.lua", revision = "HEAD", line = 700,
    is_current = function() return vim.api.nvim_get_current_buf() == origin end,
    on_error = function(message) notices[#notices + 1] = tostring(message) end })
  wait_until(function()
    return state.replica.status == "Applied" and state.replica.row_count == 700 and not state.more
  end, "source delivery did not finish", { timeout_ms = 4000, errors = notices })
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
  wait_until(function() return snapshot ~= nil end, "native source snapshot did not arrive",
    { timeout_ms = 3000, errors = notices })
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(snapshot.document == state.document and snapshot.revision == state.replica.revision)
  assert(snapshot.more == false and snapshot.diff_view == nil, "plain source snapshot acquired diff controls")
  local captured = {}
  for _, block in ipairs(snapshot.block) do vim.list_extend(captured, block.text) end
  assert(vim.deep_equal(captured, lines), "source recovery snapshot differs from captured rows")
  local closed = false
  source.close(state, function() closed = true end)
  wait_until(function() return closed end, "native source close did not settle", { timeout_ms = 3000 })
  state = nil
  assert(host.stop(), "source host process was not collected")
end

return T
