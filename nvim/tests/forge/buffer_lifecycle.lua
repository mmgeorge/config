vim.loader.enable(false)
local replica = require("forge.buffer")
local success, failure = xpcall(function()
  vim.v.errmsg = ""
  for _, external in ipairs({ true, false }) do
    local session = replica.open("wipe:" .. tostring(external))
    local called = false
    vim.api.nvim_create_autocmd("BufWipeout", { buffer = session.buffer, once = true, callback = function()
      called = true
      assert(session.status == "Closed", "owner callback ran before replica invalidation")
      replica.close(session)
    end })
    if external then
      vim.api.nvim_set_current_buf(session.buffer)
      vim.cmd("bwipeout!")
    else replica.close(session) end
    assert(called and session.status == "Closed" and not vim.api.nvim_buf_is_valid(session.buffer))
    assert(session.lifecycle_autocmd == nil)
  end
  for _, mode in ipairs({ "physical", "generated" }) do
    local physical = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(physical, 0, -1, false, { "Retained user text" })
    vim.b[physical].forge_native_document = "previous-owner"
    local options = { buffer = physical, [mode] = true,
      expected_changedtick = vim.api.nvim_buf_get_changedtick(physical) }
    local session = replica.open(mode, options)
    local lifecycle = session.lifecycle_autocmd
    replica.close(session)
    assert(vim.api.nvim_buf_is_valid(physical), mode .. " deleted caller-owned text")
    assert(vim.api.nvim_buf_get_lines(physical, 0, -1, false)[1] == "Retained user text")
    assert(vim.b[physical].forge_native_document == "previous-owner")
    assert(session.lifecycle_autocmd == nil)
    for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ event = "BufWipeout", buffer = physical })) do
      assert(autocmd.id ~= lifecycle, "closed replica retained its lifecycle callback")
    end
    vim.api.nvim_buf_delete(physical, { force = true })
  end
  local invalidated = replica.open("generation")
  local lifecycle = invalidated.lifecycle_autocmd
  replica.invalidate(invalidated)
  assert(vim.api.nvim_buf_is_valid(invalidated.buffer) and invalidated.status == "Closed")
  assert(vim.b[invalidated.buffer].forge_native_document == nil and invalidated.lifecycle_autocmd == nil)
  for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ event = "BufWipeout", buffer = invalidated.buffer })) do
    assert(autocmd.id ~= lifecycle, "invalidation retained old generation callback")
  end
  local replacement = replica.open("replacement", { buffer = invalidated.buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(invalidated.buffer) })
  assert(replica.apply_snapshot(replacement, { document = "replacement", revision = 0, block = {
    { id = "body", text = { "Owned generated text" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }).kind == "Applied")
  replica.close(replacement)
  assert(not vim.api.nvim_buf_is_valid(replacement.buffer), "adopted generated buffer was not released")
  assert(vim.v.errmsg == "", vim.v.errmsg)
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("buffer_lifecycle OK")
vim.cmd("qa!")
