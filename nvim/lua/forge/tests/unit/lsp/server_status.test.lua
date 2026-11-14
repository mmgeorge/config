local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["rust_requires_observed_healthy_quiescence"] = function()
  local client = { name = "rust_analyzer", id = 12 }
  stub(vim.lsp, "get_client_by_id", function() return client end)
  local calls = 0
  stub(vim.lsp.handlers, "experimental/serverStatus", function() calls = calls + 1 end)
  local status = require("forge.lsp.server_status")
  status.setup()
  eq(status.ready(client), false)
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "ok", quiescent = false }, { client_id = 12 })
  eq(status.ready(client), false)
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "ok", quiescent = true }, { client_id = 12 })
  eq(status.ready(client), true)
  eq(calls, 2)
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "warning", quiescent = true, message = "build scripts failed" }, { client_id = 12 })
  local ready_with_warning, failed_with_warning, warning = status.ready(client)
  eq(ready_with_warning, true)
  eq(failed_with_warning, nil)
  eq(warning:find("build scripts failed", 1, true) ~= nil, true)
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "error", quiescent = true, message = "load failed" }, { client_id = 12 })
  local ready, failure = status.ready(client)
  eq(ready, false)
  eq(failure:find("load failed", 1, true) ~= nil, true)
  client = { name = "rust_analyzer", id = 12 }
  eq(status.ready(client), false)
end

T["client_handler_is_preserved_and_wrapped_once"] = function()
  local calls = 0
  local client = { name = "rust_analyzer", id = 13, handlers = {
    ["experimental/serverStatus"] = function() calls = calls + 1 end,
  } }
  stub(vim.lsp, "get_client_by_id", function() return client end)
  stub(vim.lsp.handlers, "experimental/serverStatus", nil)
  local status = require("forge.lsp.server_status")
  status.observe(client)
  local handler = client.handlers["experimental/serverStatus"]
  status.observe(client)
  eq(client.handlers["experimental/serverStatus"], handler)
  handler(nil, { health = "ok", quiescent = true }, { client_id = 13 })
  eq(calls, 1)
  eq(status.ready(client), true)
end

return T
