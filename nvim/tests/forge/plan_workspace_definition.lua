local definition = require("forge.views.plan_review.workspace_definition")
local directory = vim.fn.tempname()
assert(vim.fn.mkdir(directory, "p") == 1)
local source_path, target_path = directory .. "/source.ts", directory .. "/target.ts"
local contents = 'const note = "𝄞"; value.count;\r\n'
local descriptor = { path = source_path, digest = vim.fn.sha256(contents), line = 1,
  column = assert(contents:find("count", 1, true)) - 1, filetype = "typescript" }
local function write(path, text)
  local file = assert(io.open(path, "wb"))
  file:write(text)
  file:close()
end
write(source_path, contents)
write(target_path, "export interface Value { count: number }\r\n")
local original_clients, original_attached, original_attach = vim.lsp.get_clients, vim.lsp.buf_is_attached, vim.lsp.buf_attach_client
local request_count, attachment_count, cancelled, source_buffer = 0, 0, 0, nil
local handler, submitted, delayed
local client = { id = 951, name = "first", config = { root_dir = directory, filetypes = { "typescript" } }, offset_encoding = "utf-16" }
function client:is_stopped() return false end
function client:supports_method(method) return method == "textDocument/definition" end
function client:cancel_request() cancelled = cancelled + 1 end
function client:request(method, params, callback, buffer)
  assert(method == "textDocument/definition")
  assert(params.textDocument.uri == vim.uri_from_fname(source_path))
  assert(params.position.character == vim.str_utfindex(contents, "utf-16", descriptor.column, false))
  assert(vim.bo[buffer].fileformat == "dos")
  source_buffer, request_count, submitted = buffer, request_count + 1, callback
  if not delayed then vim.schedule(function() handler(callback) end) end
  return true, request_count
end
vim.lsp.get_clients = function() return { client } end
vim.lsp.buf_is_attached = function() return false end
vim.lsp.buf_attach_client = function(_, identity) assert(identity == client.id) attachment_count = attachment_count + 1 return true end
local autostarts = 0
local group = vim.api.nvim_create_augroup("PlanDefinitionAutostartTest", { clear = true })
vim.api.nvim_create_autocmd("FileType", { group = group, callback = function() autostarts = autostarts + 1 end })
local range = { start = { line = 0, character = 25 }, ["end"] = { line = 0, character = 30 } }
local function request(current)
  local completed, target, failure, message = false
  local cancel = definition.request(descriptor, current or function() return true end, function(location, error, notice)
    target, failure, message, completed = location, error, notice, true
  end)
  return function()
    assert(vim.wait(6000, function() return completed end, 10), "definition callback did not settle")
    return target, failure, message
  end, cancel
end
local success, failure = xpcall(function()
  handler = function(callback) callback(nil, { uri = vim.uri_from_fname(target_path), range = range }) end
  local await = request()
  local target, error = await()
  assert(not error, error)
  assert(target and target.line == 1 and target.column == 25, vim.inspect(target))
  assert(vim.fs.abspath(target.path) == vim.fs.normalize(target.path), "definition path is not normalized")
  assert(not vim.api.nvim_buf_is_valid(source_buffer), "request leaked its hidden source buffer")
  handler = function(callback) callback(nil, { { targetUri = vim.uri_from_fname(target_path), targetSelectionRange = range, targetRange = range } }) end
  await = request()
  assert(await())
  assert(request_count == 2 and attachment_count == 2, "a repeated jump reused a cached result")
  assert(autostarts == 0, "loading hidden source triggered FileType autostart")
  handler = function(callback) callback(nil, {}) end
  await = request()
  local empty, empty_error, notice = await()
  assert(not empty and not empty_error and notice:find("No definition", 1, true))
  handler = function(callback) callback({ message = "injected server failure" }) end
  await = request()
  local _, server_error = await()
  assert(server_error == "injected server failure")
  handler = function(callback) callback({ code = -32801, message = "content modified" }) end
  await = request()
  local retry, retry_error, retry_notice = await()
  assert(not retry and not retry_error and retry_notice:find("Retry the jump", 1, true))
  delayed = true
  local current = true
  await = request(function() return current end)
  assert(vim.wait(1000, function() return vim.api.nvim_buf_is_valid(source_buffer) end, 10))
  current = false
  local stale, stale_error, stale_message = await()
  assert(not stale and not stale_error and stale_message == "cancelled")
  submitted(nil, { uri = vim.uri_from_fname(target_path), range = range })
  await = request()
  local timed_out, timeout_error = await()
  assert(not timed_out and timeout_error:find("timed out", 1, true))
  assert(cancelled > 0, "timed out requests were not cancelled")
  delayed = false
  write(source_path, contents .. "// changed\n")
  await = request()
  local changed, changed_error, changed_notice = await()
  assert(not changed and not changed_error and changed_notice:find("Source changed", 1, true))
  write(source_path, contents)
  vim.lsp.get_clients = function() return {} end
  await = request()
  local unavailable, unavailable_error, unavailable_notice = await()
  assert(not unavailable and not unavailable_error and unavailable_notice:find("No running", 1, true))
  local other = setmetatable({ id = 952, name = "second" }, { __index = client })
  vim.lsp.get_clients = function() return { client, other } end
  await = request()
  local ambiguous, ambiguous_error, ambiguous_notice = await()
  assert(not ambiguous and not ambiguous_error and ambiguous_notice == "ambiguous_server: first, second")
  vim.lsp.get_clients = function() return { client } end
  contents = contents .. string.rep("// complete source padding\r\n", 50000)
  descriptor.digest = vim.fn.sha256(contents)
  write(source_path, contents)
  write(target_path, "export interface Value { count: number }\r\n/*"
    .. string.rep("complete target ", 1200000) .. "*/\r\n")
  handler = function(callback) callback(nil, { uri = vim.uri_from_fname(target_path), range = range }) end
  await = request()
  local large, large_error = await()
  assert(large and not large_error and large.column == 25,
    large_error or "large source or target rejected during definition lookup")
end, debug.traceback)
vim.lsp.get_clients, vim.lsp.buf_is_attached, vim.lsp.buf_attach_client = original_clients, original_attached, original_attach
vim.api.nvim_del_augroup_by_id(group)
vim.fn.delete(directory, "rf")
assert(success, failure)
print("plan_workspace_definition: passed")
