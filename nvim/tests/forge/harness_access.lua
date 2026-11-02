vim.loader.enable(false)
local access = require("forge.views.harness.access")
local root = vim.fn.tempname()
assert(vim.fn.mkdir(root, "p") == 1)
local nested = root .. "/nested directory"
assert(vim.fn.mkdir(nested, "p") == 1)
local state = { session = { id = "access", workspace = root, access = {
  sandbox = true, read_access = "workspace", write_access = "workspace", isolation_mode = "elevated",
  readable_directory = { vim.uv.fs_realpath(root) }, writable_directory = {},
} } }
local failures, applied, returned = {}, 0, 0
local pending, deferred
local original_error = require("forge.infra.notifications").error
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
local function apply(policy, done)
  applied = applied + 1
  if deferred then pending = { policy = policy, done = done } return end
  state.session.access = policy
  done(true)
end
local function edit(editor, value)
  local anchor = editor.editable.native.anchor.directories
  vim.api.nvim_buf_set_text(editor.buffer, anchor.start.row, anchor.start.column,
    anchor.finish.row, anchor.finish.column, vim.split(value, "\n", { plain = true }))
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = editor.buffer })
end
local function open(operation)
  return access.directories(state, apply, function() returned = returned + 1 end, operation)
end
local original_tab = vim.api.nvim_get_current_tabpage()
local editor = open("write")
assert(vim.bo[editor.buffer].buftype == "acwrite")
assert(vim.api.nvim_get_current_tabpage() ~= original_tab)
assert(editor:values().directories == "")
edit(editor, nested .. "\n\n" .. root .. "/missing")
editor:save()
assert(applied == 0 and #failures == 1 and failures[1]:find("Line 3:", 1, true))
assert(editor:dirty() and editor:values().directories:find("missing", 1, true))
edit(editor, "  " .. nested .. "  \n\n" .. root .. "\n" .. nested)
vim.api.nvim_exec_autocmds("BufWriteCmd", { buffer = editor.buffer })
assert(applied == 1 and not editor:dirty())
assert(vim.deep_equal(state.session.access.writable_directory, { vim.uv.fs_realpath(nested), vim.uv.fs_realpath(root) }))
assert(vim.deep_equal(state.session.access.readable_directory, { vim.uv.fs_realpath(root) }))
assert(state.session.access.sandbox and state.session.access.write_access == "workspace")
editor:close()
assert(returned == 1 and vim.api.nvim_get_current_tabpage() == original_tab)
editor = open("write")
edit(editor, "")
editor:save()
assert(applied == 2 and #state.session.access.writable_directory == 0)
editor:close()
editor = open("read")
assert(editor:values().directories == vim.uv.fs_realpath(root))
edit(editor, nested)
deferred = true
editor:save()
editor:save()
assert(applied == 3 and editor.pending, "pending writes must not be submitted twice")
pending.done(false)
assert(not editor.pending and editor:dirty(), "failed writes must retain the editable draft")
editor:save()
edit(editor, root)
state.session.access = pending.policy
pending.done(true)
assert(editor:dirty(), "saving an earlier draft cleared newer edits")
assert(state.session.access.readable_directory[1] == vim.uv.fs_realpath(nested))
deferred = false
editor:save()
assert(applied == 5 and not editor:dirty())
assert(state.session.access.readable_directory[1] == vim.uv.fs_realpath(root))
assert(#state.session.access.writable_directory == 0)
editor:close()
editor = open("read")
edit(editor, nested)
state.session = { id = "other" }
editor:save()
assert(applied == 5 and editor:dirty(), "stale editor changed another session")
assert(failures[#failures] == "This session is no longer active.")
editor:replace({ directories = root })
editor:close()
assert(returned == 3, "stale editor reopened the previous session config")
state.session = { id = "preferences", access = { sandbox = false } }
state.access_preference = { sandbox = true, isolation_mode = "mxc", read_access = "full",
  write_access = "off", readable_directory = {}, writable_directory = {} }
editor = open("read")
edit(editor, nested)
editor:save()
assert(state.session.access.sandbox and state.session.access.isolation_mode == "mxc",
  "directory editing overwrote saved sandbox preferences with effective access")
assert(state.session.access.read_access == "full" and state.session.access.write_access == "off")
editor:close()
require("forge.infra.notifications").error = original_error
vim.fn.delete(root, "rf")
print("harness_access: passed")
vim.cmd("qa!")
