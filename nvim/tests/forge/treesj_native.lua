vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local invocation_count = 0
package.loaded.treesj = { toggle = function() invocation_count = invocation_count + 1 end }
local mapping = require("plugins.join").keys[1][2]
local buffer = vim.api.nvim_get_current_buf()
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "first", "second", "last" })
vim.b[buffer].forge_native_document = true
mapping()
assert(invocation_count == 0, "native document invoked parser-dependent formatting")
assert(vim.api.nvim_win_get_cursor(0)[1] == 3, "native G did not reach final row")
vim.b[buffer].forge_native_document = nil
mapping()
assert(invocation_count == 1, "ordinary source lost syntax join mapping")
print("treesj native document guard passed")
