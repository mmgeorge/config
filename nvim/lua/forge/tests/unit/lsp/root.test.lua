local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["marker_priority_matches_editor_configuration"] = function()
  local directory = temp.directory("root-markers")
  vim.fn.mkdir(directory .. "/child/.git", "p")
  vim.fn.writefile({ "{}" }, directory .. "/.luarc.json")
  local result
  require("forge.lsp.root").resolve({ name = "lua", root_markers = { { ".luarc.json" }, { ".git" } } },
    directory .. "/child/init.lua", nil, function(root) result = root end)
  eq(require("forge.lsp.source").canonical(result), require("forge.lsp.source").canonical(directory))
end

T["cancelled_root_callback_cannot_publish"] = function()
  local receive_root, result
  local buffer = vim.api.nvim_create_buf(false, true)
  MiniTest.finally(function() pcall(vim.api.nvim_buf_delete, buffer, { force = true }) end)
  local cancel = require("forge.lsp.root").resolve({ name = "lua", root_dir = function(actual, receive)
    eq(actual, buffer)
    receive_root = receive
  end }, "/project/init.lua", buffer, function(root) result = root end)
  cancel()
  receive_root("/project")
  eq(result, nil)
  eq(vim.api.nvim_buf_is_valid(buffer), true)
end

T["root_timeout_is_explicit_and_deterministic"] = function()
  local expire, failure
  stub(vim.uv, "new_timer", function()
    local closed = false
    return { start = function(_, _, _, callback) expire = callback end, stop = function() end,
      close = function() closed = true end, is_closing = function() return closed end }
  end)
  local buffer = vim.api.nvim_create_buf(false, true)
  MiniTest.finally(function() pcall(vim.api.nvim_buf_delete, buffer, { force = true }) end)
  require("forge.lsp.root").resolve({ name = "lua", root_dir = function() end }, "/project/init.lua", buffer,
    function(_, message) failure = message end)
  expire()
  vim.wait(0, function() return failure ~= nil end)
  eq(failure, "timeout: selected LSP root resolution exceeded 60 seconds")
end

return T
