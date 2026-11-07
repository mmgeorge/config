vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
-- `:ForgePermissions` opened from a Harness window, whose buffer is pinned with 'winfixbuf', shows
-- the permissions document in a new tab instead of failing with E1513.
local document_path = vim.fn.tempname() .. ".json"
package.loaded["forge.client"] = {
  request = function(request, callback)
    assert(request.method == "permissions.document.open", request.method)
    callback({ path = document_path, source = "{}" })
  end,
}
local failures = {}
package.loaded["forge.infra.notifications"] = {
  error = function(message) failures[#failures + 1] = tostring(message) end,
  info = function() end,
}
local ok, failure = xpcall(function()
  local pinned_window = vim.api.nvim_get_current_win()
  local pinned_buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(pinned_window, pinned_buffer)
  vim.wo[pinned_window].winfixbuf = true
  local tab_count = #vim.api.nvim_list_tabpages()
  require("forge.views.permissions").open()
  assert(#failures == 0, table.concat(failures, "\n"))
  assert(vim.api.nvim_win_get_buf(pinned_window) == pinned_buffer, "the pinned window lost its buffer")
  assert(#vim.api.nvim_list_tabpages() == tab_count + 1, "the permissions document did not open in a new tab")
  local shown = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
  assert(vim.fs.normalize(shown) == vim.fs.normalize(document_path), "the new tab does not show the document: " .. shown)
  assert(not vim.wo.winfixbuf, "the document window inherited 'winfixbuf'")
end, debug.traceback)
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("permissions_view_winfixbuf: passed")
vim.cmd("qa!")
