vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")

local commands = require("forge.document_commands")
local config = require("forge.infra.config")
local buffer = require("forge.buffer")

config.setup({})
local replica = buffer.open("amend-command")
replica.status = "Applied"
local called = 0
local owner = commands.attach(replica, {
  view = "status",
  handler = { commit_amend = function() called = called + 1 end },
})

vim.api.nvim_buf_call(replica.buffer, function()
  local mapping = vim.fn.maparg("ca", "n", false, true)
  assert(type(mapping.callback) == "function", "status ca mapping is absent")
  mapping.callback()
end)
assert(called == 1, "status ca did not dispatch amend")
assert(vim.iter(owner.binding):any(function(binding)
  return binding.spec.id == "commit_amend" and binding.keys[1] == "ca"
end), "amend command is absent from status bindings")

owner.close()
print("commit_amend_command OK")
vim.cmd("qa!")
