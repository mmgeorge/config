-- Unit tests for `forge.walkthrough` (walkthrough.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["composite"] = function()
  local source = require("forge.source_document")
  local notices, deferred = {}, nil
  local state
  local function snapshot(document, rows, target)
    return { document = document, revision = 1, block = {
      { id = "body", text = rows, metadata = { decoration = {}, editable_region = {}, target = target and {
        { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = #rows, column = 0 } } },
      } or {} } },
    } }
  end
  package.loaded["forge.client"] = { request = function(request_specification, callback)
    local params = request_specification.params or {}

    if params.operation == "open" then
      callback({ inventory_state = "disabled", snapshot = snapshot(params.document, { "Annotated change" }, "change") })
    elseif params.operation == "annotation_source" then
      deferred = callback
    else callback(vim.NIL) end
  end }
  source._set_runner_for_test(function(_, params, callback)
    if params.operation == "change" then
      assert(params.review == true)
      callback({ title = "source.rs", object = "captured", revision = "captured", source_row = 1, more = false, review = true,
        state = { state = "ready" }, snapshot = snapshot(params.document, { "first source line", "second source line" }),
        annotation = snapshot(params.annotation_document, { "changed source", "╭──────────╮", "│ annotation │", "╰──────────╯" }, "source-coordinate") })
    else callback({}) end
  end)
  local walkthrough = require("forge.walkthrough")
  MiniTest.finally(function() if state then pcall(walkthrough.close, state) end end)
  local function settled()
    wait_until(function() return not state.opening and not state.pending and #state.queue == 0 end,
      "walkthrough did not settle", { errors = notices })
  end
  state = walkthrough.open({ on_error = function(message) notices[#notices + 1] = message end })
  settled()
  walkthrough.open_change(state)
  settled()
  local owner = state.source[1]
  assert(owner.review and owner.source.active and owner.annotation)
  assert(#vim.api.nvim_list_wins() == 2, "composite opened extra source/annotation panes")
  assert(vim.api.nvim_get_current_buf() == owner.annotation.buffer)
  assert(#vim.fn.win_findbuf(owner.source.replica.buffer) == 0, "exact source remained visible beside composite")
  keymap.invoke("o")
  assert(deferred)
  deferred({ source_row = 1 })
  deferred = nil
  settled()
  assert(vim.api.nvim_get_current_buf() == owner.source.replica.buffer)
  assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "source coordinate was reconstructed from generated rows")
  keymap.invoke("q")
  settled()
  assert(vim.api.nvim_get_current_buf() == owner.annotation.buffer and owner.source.active)
  keymap.invoke("o")
  assert(deferred)
  vim.cmd("enew")
  local unrelated = vim.api.nvim_get_current_buf()
  deferred({ source_row = 0 })
  deferred = nil
  settled()
  assert(vim.api.nvim_get_current_buf() == unrelated, "late source action stole unrelated buffer")
  vim.api.nvim_win_set_buf(owner.window, owner.annotation.buffer)
  settled()
  keymap.invoke("q")
  settled()
  assert(not owner.active and not owner.source.active)
  assert(#vim.api.nvim_list_wins() == 1)
  assert(#notices == 0, table.concat(notices, "\n"))
end

T["panes"] = function()
  local source = require("forge.source_document")
  local notices = {}
  local annotation_width = {}
  local main_width
  local state
  local function snapshot(document, text, target)
    return { document = document, revision = 1, block = {
      { id = "body", text = text, metadata = { decoration = {}, editable_region = {}, target = target or {} } },
    } }
  end
  package.loaded["forge.client"] = { request = function(request_specification, callback)
    local params = request_specification.params or {}

    if params.operation == "open" then
      callback({ inventory_state = "disabled", snapshot = snapshot(params.document, { "Annotated change" }, {
        { id = "change", range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 16 } } },
      }) })
    elseif params.operation == "view" then
      main_width = params.width.columns
      callback(vim.NIL)
    elseif params.operation == "annotation_view" then
      annotation_width[params.annotation] = params.width.columns
      callback(vim.NIL)
    elseif params.operation == "annotation_close" then
      annotation_width[params.annotation] = nil
      callback(vim.NIL)
    else callback(vim.NIL) end
  end }
  source._set_runner_for_test(function(_, params, callback)
    if params.operation == "change" then
      callback({ title = "source.rs", object = "same-object", revision = "captured", source_row = 0, more = false, review = true,
        state = { state = "ready" }, snapshot = snapshot(params.document, { "fn current() {}" }),
        annotation = snapshot(params.annotation_document, { "Source annotation" }) })
    else callback({}) end
  end)
  local walkthrough = require("forge.walkthrough")
  MiniTest.finally(function() if state then pcall(walkthrough.close, state) end end)
  local function settled()
    wait_until(function() return not state.opening and not state.pending and #state.queue == 0 end,
      "walkthrough did not settle", { errors = notices })
  end
  local function open_source()
    vim.api.nvim_set_current_win(state.window)
    settled()
    walkthrough.open_change(state)
    settled()
    local owner = state.source[#state.source]
    assert(owner.active and owner.source and owner.annotation, "source pair did not open")
    assert(#vim.api.nvim_list_wins() >= 2)
    return owner
  end
  vim.v.errmsg = ""
  state = walkthrough.open({ on_error = function(message) notices[#notices + 1] = message end })
  settled()
  for _, action in ipairs({ "review_q", "review_window", "source_buffer", "review_buffer" }) do
    local owner = open_source()
    assert(annotation_width[owner.annotation.document] == require("forge.width").capture(owner.annotation_window).columns,
      "annotation used the parent walkthrough width")
    vim.api.nvim_win_set_width(owner.annotation_window, 22)
    vim.api.nvim_set_current_win(owner.annotation_window)
    vim.api.nvim_exec_autocmds("WinResized", {})
    settled()
    assert(annotation_width[owner.annotation.document] == require("forge.width").capture(owner.annotation_window).columns,
      "annotation resize did not request native reflow")
    assert(vim.wo[owner.annotation_window].wrap and vim.wo[owner.annotation_window].linebreak)
    assert(main_width == require("forge.width").capture(state.window).columns,
      "resizing a focused annotation did not reflow its main sibling")
    if action == "review_q" then keymap.invoke("q", { window = owner.annotation_window })
    elseif action == "review_window" then vim.api.nvim_win_close(owner.annotation_window, true)
    elseif action == "source_buffer" then vim.api.nvim_buf_delete(owner.source.replica.buffer, { force = true })
    else vim.api.nvim_buf_delete(owner.annotation.buffer, { force = true }) end
    wait_until(function() return not owner.active end, action .. " retained the pair")
    settled()
    assert(#vim.api.nvim_list_wins() == 1, action .. " retained an owned pane")
    assert(vim.api.nvim_get_current_win() == state.window, action .. " did not return focus")
    assert(vim.api.nvim_get_current_buf() == state.replica.buffer)
    assert(not vim.api.nvim_buf_is_valid(owner.source.replica.buffer))
    assert(not vim.api.nvim_buf_is_valid(owner.annotation.buffer))
    assert(annotation_width[owner.annotation.document] == nil, action .. " retained native annotation ownership")
  end
  local first = open_source()
  local second = open_source()
  assert(first.source ~= second.source, "independent annotations shared source lifecycle")
  assert(#vim.api.nvim_list_wins() == 3)
  keymap.invoke("q", { window = first.annotation_window })
  settled()
  assert(second.active and second.source.active and #vim.api.nvim_list_wins() == 2,
    "closing one annotation disposed another source pair")
  keymap.invoke("?", { window = second.annotation_window })
  assert(vim.bo.filetype == "ForgeHelp")
  assert(buffer_support.text(0):find("Close", 1, true))
  keymap.invoke("q")
  walkthrough.close(state)
  assert(not second.active and not second.source.active)
  assert(#vim.api.nvim_list_wins() == 1, "main close retained source panes")
  state = walkthrough.open({ on_error = function(message) notices[#notices + 1] = message end })
  settled()
  local wiped = open_source()
  vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
  wait_until(function() return not state.active end, "main buffer wipe retained ownership")
  assert(not wiped.active and not wiped.source.active and #vim.api.nvim_list_wins() == 1)
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(vim.v.errmsg == "", vim.v.errmsg)
end

return T
