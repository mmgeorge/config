local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks({ post_case = function() vim.fn.setqflist({}, "f") end }) })

T["quickfix_preserves_severity_and_known_positions"] = function()
  local root = vim.fn.tempname()
  vim.fn.mkdir(root, "p")
  MiniTest.finally(function() vim.fn.delete(root, "rf") end)
  vim.fn.writefile({ "aπz", "end" }, root .. "/source file.rs")
  require("forge.views.harness.source_actions").quickfix(root, {
    title = "Compiler",
    items = {
      { path = "source file.rs", line = 1, column = 4, severity = "error", message = "wrong type [E0001]" },
      { path = "source file.rs", line = vim.NIL, column = vim.NIL, severity = "warning", message = "file warning" },
      { path = "source file.rs", line = 2, column = 1, severity = "info", message = "context" },
      { path = "source file.rs", line = 2, column = 1, severity = "hint", message = "hint" },
    },
  })
  local list = vim.fn.getqflist({ title = 0, items = 0, winid = 0 })
  eq(list.title, "Compiler")
  assert(list.winid > 0, "quickfix window did not open")
  eq(#list.items, 4)
  for index, kind in ipairs({ "E", "W", "I", "I" }) do eq(list.items[index].type, kind) end
  eq({ list.items[1].lnum, list.items[1].col, list.items[1].text }, { 1, 4, "wrong type [E0001]" })
  eq({ list.items[2].lnum, list.items[2].col }, { 0, 0 })
  eq(vim.fs.normalize(vim.api.nvim_buf_get_name(list.items[1].bufnr)), vim.fs.normalize(root .. "/source file.rs"))
  eq(vim.fn.readfile(root .. "/source file.rs"), { "aπz", "end" })
end

T["source_navigation_clamps_stale_positions_without_splitting_utf8"] = function()
  local root = vim.fn.tempname()
  vim.fn.mkdir(root, "p")
  MiniTest.finally(function() vim.fn.delete(root, "rf") end)
  vim.fn.writefile({ "aπz", "end" }, root .. "/source.rs")
  local actions = require("forge.views.harness.source_actions")
  actions.open_file(root, { path = "source.rs", line = 1, column = 4 })
  eq(vim.api.nvim_win_get_cursor(0), { 1, 3 })
  actions.open_file(root, { path = "source.rs", line = 1, column = 3 })
  eq(vim.api.nvim_win_get_cursor(0), { 1, 1 })
  actions.open_file(root, { path = "source.rs", line = 999, column = 999 })
  eq(vim.api.nvim_win_get_cursor(0), { 2, 2 })
  actions.open_file(root, { path = "source.rs" })
  eq(vim.api.nvim_win_get_cursor(0), { 1, 0 })
end

T["empty_result_keeps_the_existing_quickfix_list"] = function()
  vim.fn.setqflist({}, " ", { title = "Existing", items = { { filename = "kept.rs", lnum = 1, text = "keep" } } })
  local before = vim.fn.getqflist({ id = 0, items = 0, title = 0 })
  require("forge.views.harness.source_actions").quickfix(vim.fn.getcwd(), { title = "Empty", items = {} })
  eq(vim.fn.getqflist({ id = 0, items = 0, title = 0 }), before)
end

T["source_and_quickfix_failures_are_reported"] = function()
  local notifications = require("forge.infra.notifications")
  local actions = require("forge.views.harness.source_actions")
  local errors, warnings = {}, {}
  notifications.error = function(message) errors[#errors + 1] = message end
  notifications.warn = function(message) warnings[#warnings + 1] = message end
  local before = vim.api.nvim_get_current_buf()
  actions.open_file(vim.fn.tempname(), { path = "missing.rs", line = 1, column = 1 })
  eq(vim.api.nvim_get_current_buf(), before)
  assert(warnings[1]:find("missing.rs", 1, true))
  local setqflist = vim.fn.setqflist
  vim.fn.setqflist = function() error("fixture quickfix failure") end
  actions.quickfix(vim.fn.getcwd(), { title = "Failure", items = {
    { path = "source.rs", line = 1, column = 1, severity = "error", message = "failure" },
  } })
  vim.fn.setqflist = setqflist
  assert(errors[1]:find("fixture quickfix failure", 1, true))
end

return T
