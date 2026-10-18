local diagnostics = require("forge.lsp.diagnostics")
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "local value = 1" })
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
local function await(index)
  assert(vim.wait(1000, function() return results[index] ~= false end, 5))
  assert(not results[index].failure, vim.inspect(results[index].failure))
  return results[index].result
end
local function invalidate()
  vim.api.nvim_exec_autocmds("LspNotify", { data = { client_id = client.id, method = "workspace/didChangeWatchedFiles" } })
end
local success, failure = xpcall(function()
  local first, second = request(), request()
  assert(#calls == 1, "concurrent requests must share a pull")
  local items = { { range = { start = { line = 0, character = 0 }, ["end"] = { line = 0, character = 1 } },
    severity = 1, message = "fixture" } }
  calls[1].receive(nil, { kind = "full", resultId = "first", items = items })
  assert(await(first).items == items and await(second).items == items)
  local cached = request()
  assert(await(cached).items == items and #calls == 1, "unchanged source must reuse its report")
  invalidate()
  local refreshed = request()
  assert(#calls == 2 and calls[2].params.previousResultId == "first")
  calls[2].receive(nil, { kind = "unchanged", resultId = "second" })
  assert(await(refreshed).items == items, "unchanged report lost its prior diagnostics")
  client.handlers["workspace/diagnostic/refresh"]()
  local abandoned, cancel = request()
  local survivor = request()
  cancel()
  assert(not cancelled[3], "one consumer cancelled another consumer's request")
  calls[3].receive(nil, { kind = "full", resultId = "empty", items = {} })
  assert(#await(survivor).items == 0 and results[abandoned] == false)
  invalidate()
  local stale = request()
  invalidate()
  calls[4].receive(nil, { kind = "full", resultId = "stale", items = items })
  assert(results[stale].failure.code == -32801, "dependency changes accepted an obsolete result")
  local native = { kind = "full", resultId = "native", items = items }
  vim.api.nvim_exec_autocmds("LspRequest", { data = { client_id = client.id, request_id = 99,
    request = { method = "textDocument/diagnostic", type = "pending" } } })
  client.handlers["textDocument/diagnostic"](nil, native, {
    client_id = client.id, request_id = 99, bufnr = buffer, params = {},
  })
  assert(published == 1)
  local native_reuse = request()
  assert(await(native_reuse).items == items and #calls == 4, "editor diagnostic report was not reused")
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "local value = 2" })
  local _, cancel_last = request()
  assert(#calls == 5, "changed buffer reused old diagnostics")
  cancel_last()
  assert(cancelled[5], "unowned diagnostic request was not cancelled")
end, debug.traceback)
vim.api.nvim_buf_delete(buffer, { force = true })
assert(success, failure)
print("lsp_diagnostics: passed")
