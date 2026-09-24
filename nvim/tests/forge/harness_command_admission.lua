vim.loader.enable(false)
local source = require("forge.views.harness.completion.command_source")
assert(not source.accepts_prompt("/sess"))
assert(not source.accepts_prompt("  /sess extra text"))
assert(not source.accepts_prompt("/"))
assert(source.accepts_prompt("/sessions anything /unrecognized"))
assert(source.accepts_prompt("/plan arbitrary prompt"))
assert(source.accepts_prompt("Explain /sess"))
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue = {}
for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/sess" })
  submit()
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "/sess")
  assert(#state.queue == 0)
end
print("harness_command_admission: passed")
