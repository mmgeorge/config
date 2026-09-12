vim.loader.enable(false)

local native_syntax = require("forge.native_syntax")
local original_start = vim.treesitter.start
local started = {}

vim.treesitter.start = function(buffer)
  started[#started + 1] = buffer
end

local ok, failure = xpcall(function()
  local native = vim.api.nvim_create_buf(false, true)
  local ordinary = vim.api.nvim_create_buf(false, true)
  vim.b[native].forge_native_document = true
  vim.bo[native].indentexpr = "native-host-indent"

  assert(not native_syntax.global_parser_allowed(native), "native buffer permitted the global parser")
  assert(not native_syntax.attach_global_parser(native), "native buffer attached a global parser")
  assert(#started == 0, "native buffer called vim.treesitter.start")
  assert(vim.bo[native].indentexpr == "native-host-indent", "native buffer changed host indentation")

  assert(native_syntax.global_parser_allowed(ordinary), "ordinary buffer bypassed the global parser")
  assert(native_syntax.attach_global_parser(ordinary), "ordinary buffer did not attach the global parser")
  assert(#started == 1 and started[1] == ordinary, "ordinary buffer did not start its parser")
  assert(vim.bo[ordinary].indentexpr == "v:lua.require'nvim-treesitter'.indentexpr()", "ordinary buffer missed Tree-sitter indentation")

  vim.api.nvim_buf_delete(native, { force = true })
  vim.api.nvim_buf_delete(ordinary, { force = true })
end, debug.traceback)

vim.treesitter.start = original_start
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
