-- Unit tests for `forge.lsp.diagnostics` (diagnostics.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["lsp_diagnostics"] = function()
  local diagnostics = require("forge.lsp.diagnostics")
  local buffer = buffer_support.scratch({ "local value = 1" })
  local calls, cancelled, published = {}, {}, 0
  local client = { id = 984, server_capabilities = {}, handlers = {
    ["textDocument/diagnostic"] = function() published = published + 1 end,
    ["workspace/diagnostic/refresh"] = function() end,
  } }
  function client:request(method, params, receive)
    calls[#calls + 1] = { method = method, params = vim.deepcopy(params), receive = receive }
    return true, #calls
  end
  function client:cancel_request(id) cancelled[id] = true end
  local results = {}
  local function request()
    local index = #results + 1
    results[index] = false
    local accepted, cancel = diagnostics.request(client, buffer, {
      textDocument = { uri = vim.uri_from_bufnr(buffer) },
    }, function(failure, result) results[index] = { failure = failure, result = result } end)
    assert(accepted)
    return index, cancel
  end
  local function settled_result(index)
    wait_until(function() return results[index] ~= false end, "diagnostic request " .. index .. " did not settle")
    assert(not results[index].failure, vim.inspect(results[index].failure))
    return results[index].result
  end
  local function invalidate()
    vim.api.nvim_exec_autocmds("LspNotify", { data = { client_id = client.id, method = "workspace/didChangeWatchedFiles" } })
  end
  local first, second = request(), request()
  eq(#calls, 1, { fail_reason = "concurrent requests must share a pull" })
  local items = { { range = { start = { line = 0, character = 0 }, ["end"] = { line = 0, character = 1 } },
    severity = 1, message = "fixture" } }
  calls[1].receive(nil, { kind = "full", resultId = "first", items = items })
  assert(settled_result(first).items == items and settled_result(second).items == items)
  local cached = request()
  assert(settled_result(cached).items == items and #calls == 1, "unchanged source must reuse its report")
  invalidate()
  local refreshed = request()
  assert(#calls == 2 and calls[2].params.previousResultId == "first")
  calls[2].receive(nil, { kind = "unchanged", resultId = "second" })
  assert(settled_result(refreshed).items == items, "unchanged report lost its prior diagnostics")
  client.handlers["workspace/diagnostic/refresh"]()
  local abandoned, cancel = request()
  local survivor = request()
  cancel()
  assert(not cancelled[3], "one consumer cancelled another consumer's request")
  calls[3].receive(nil, { kind = "full", resultId = "empty", items = {} })
  assert(#settled_result(survivor).items == 0 and results[abandoned] == false)
  invalidate()
  local stale = request()
  invalidate()
  calls[4].receive(nil, { kind = "full", resultId = "stale", items = items })
  eq(results[stale].failure.code, -32801, { fail_reason = "dependency changes accepted an obsolete result" })
  local native = { kind = "full", resultId = "native", items = items }
  vim.api.nvim_exec_autocmds("LspRequest", { data = { client_id = client.id, request_id = 99,
    request = { method = "textDocument/diagnostic", type = "pending" } } })
  client.handlers["textDocument/diagnostic"](nil, native, {
    client_id = client.id, request_id = 99, bufnr = buffer, params = {},
  })
  eq(published, 1)
  local native_reuse = request()
  assert(settled_result(native_reuse).items == items and #calls == 4, "editor diagnostic report was not reused")
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "local value = 2" })
  local _, cancel_last = request()
  eq(#calls, 5, { fail_reason = "changed buffer reused old diagnostics" })
  cancel_last()
  assert(cancelled[5], "unowned diagnostic request was not cancelled")
end

return T
