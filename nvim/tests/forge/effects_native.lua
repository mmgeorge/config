vim.loader.enable(false)
local replica = require("forge.buffer")
local input = require("forge.input")
local effects = require("forge.effects")
local session = replica.open("effects")
local filename = vim.fn.tempname() .. " space [literal].txt"
vim.fn.writefile({ "source", "界value" }, filename)
local ok, failure = xpcall(function()
  assert(replica.apply_snapshot(session, { document = "effects", revision = 0, block = {
    { id = "body", text = { "item" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }).kind == "Applied")
  vim.api.nvim_set_current_buf(session.buffer)
  local view = input.open(session, 0)
  assert(input.capture(session, view, "open"))
  local effect = { id = "open", document = session.document, revision = 0, view = view.id, sequence = view.sequence,
    kind = "open_file", path = filename, row = 1, column = 3, layout = "split" }
  local stale = vim.deepcopy(effect)
  stale.sequence = 0
  assert(effects.apply(session, view, stale) == "Discarded")
  vim.api.nvim_win_set_cursor(0, { 1, 1 })
  assert(effects.apply(session, view, effect) == "Discarded", "navigation ignored a newer native cursor movement")
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  local windows = #vim.api.nvim_list_wins()
  local result, diagnostic = effects.apply(session, view, effect)
  assert(result == "Applied", diagnostic)
  assert(#vim.api.nvim_list_wins() == windows + 1)
  local found = false
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_buf_get_name(vim.api.nvim_win_get_buf(window)) == filename then
      assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 2, 3 }))
      assert(vim.wo[window].foldexpr ~= "v:lua.require'forge.folds'.expression()", "source inherited document fold ownership")
      found = true
    end
  end
  assert(found, "source path did not open literally")
  assert(effects.apply(session, view, effect) == "Duplicate")
  local opened_url
  local original_open = vim.ui.open
  vim.ui.open = function(url)
    opened_url = url
    return true
  end
  local browser = {
    id = "browser",
    document = session.document,
    revision = 0,
    view = view.id,
    sequence = view.sequence,
    kind = "browser",
    url = "https://github.example.test/owner/repository/issues/7",
  }
  local browser_result, browser_diagnostic = effects.apply(session, view, browser)
  vim.ui.open = original_open
  assert(browser_result == "Applied", browser_diagnostic)
  assert(opened_url == browser.url, "browser effect did not preserve its URL")
  assert(effects.apply(session, view, browser) == "Duplicate")
  local opened_commit
  local source_document = package.loaded["forge.source_document"]
  package.loaded["forge.source_document"] = {
    open_commit = function(options) opened_commit = options end,
  }
  local commit = {
    id = "commit",
    document = session.document,
    revision = 0,
    view = view.id,
    sequence = view.sequence,
    kind = "open_commit",
    workspace = vim.fs.normalize(vim.fn.getcwd()),
    oid = string.rep("a", 40),
  }
  local commit_result, commit_diagnostic = effects.apply(session, view, commit)
  package.loaded["forge.source_document"] = source_document
  assert(commit_result == "Applied", commit_diagnostic)
  assert(opened_commit.workspace == commit.workspace and opened_commit.oid == commit.oid)
  assert(opened_commit.window == view.window and type(opened_commit.is_current) == "function")
  assert(opened_commit.is_current(), "commit document lost its captured review view")
  assert(replica.apply_snapshot(session, { document = "effects", revision = 1, block = {
    { id = "body", text = { "header", "file", "hunk", "body" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }).kind == "Applied")
  vim.api.nvim_win_call(view.window, function()
    vim.wo.foldmethod = "manual"
    vim.cmd("2,4fold")
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    assert(vim.fn.foldclosed(3) == 2)
  end)
  assert(input.capture(session, view, "navigate"))
  local navigation = { id = "navigate", document = session.document, revision = 1, view = view.id,
    sequence = view.sequence, kind = "cursor", block = "body", position = { row = 2, column = 0 } }
  local foreground = vim.api.nvim_get_current_win()
  assert(effects.apply(session, view, navigation) == "Applied")
  assert(vim.api.nvim_get_current_win() == foreground, "cursor effect changed the foreground window")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), { 3, 0 }))
  assert(vim.api.nvim_win_call(view.window, function() return vim.fn.foldclosed(3) end) == -1,
    "cursor effect left its destination hidden in a closed fold")
  input.close(view)
end, debug.traceback)
replica.close(session)
vim.fn.delete(filename)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
