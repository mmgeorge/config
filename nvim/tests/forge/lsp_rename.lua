local source = require("forge.lsp.source")
local rename = require("forge.lsp.rename")
local startup = require("forge.lsp.startup")
local original_wait, original_clients, original_attached = startup.wait, vim.lsp.get_clients, vim.lsp.buf_is_attached
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local filename = root .. "/source.lua"
vim.fn.writefile({ "local value = 1", "print(value)" }, filename)
local buffer = vim.fn.bufadd(filename)
vim.fn.bufload(buffer)
vim.bo[buffer].filetype = "lua"
local handler, notices = nil, {}
local client = { id = 9001, name = "rename-fixture", root_dir = root, config = { root_dir = root, filetypes = { "lua" } },
  offset_encoding = "utf-16", server_capabilities = { workspace = { fileOperations = {
    willRename = { filters = { { pattern = { glob = "**/*.lua" } } } },
    didRename = { filters = { { pattern = { glob = "**/*.lua" } } } },
  } } } }
function client:is_stopped() return false end
function client:supports_method() return true end
function client:cancel_request() end
function client:notify(method, params) notices[#notices + 1] = { method = method, params = params } end
function client:request(method, params, callback)
  vim.schedule(function() handler(method, params, callback) end)
  return true, 1
end
startup.wait = function(_, path, callback)
  assert(path == filename, "rename started workspace discovery instead of using its target")
  callback()
  return function() end
end
vim.lsp.get_clients = function() return { client } end
vim.lsp.buf_is_attached = function() return true end
local function prepare(id, request)
  local result, failure, done
  local cancel = rename.prepare(id, root, request, function(paths, receive)
    local allowed = {}
    for _, path in ipairs(paths) do allowed[path] = true end
    receive(allowed)
  end, function(_, _, receive) receive({ declarations = {}, complete = true }) end,
  function(value, error) result, failure, done = value, error, true end)
  assert(vim.wait(2000, function() return done end, 5), "rename did not settle")
  return result, failure, cancel
end
local function edits(name)
  return { changes = { [vim.uri_from_fname(filename)] = {
    { range = { start = { line = 0, character = 6 }, ["end"] = { line = 0, character = 11 } }, newText = name },
    { range = { start = { line = 1, character = 6 }, ["end"] = { line = 1, character = 11 } }, newText = name },
  } } }
end
local ok, failure = xpcall(function()
  handler = function(method, params, callback)
    if method == "textDocument/prepareRename" then callback(nil, { defaultBehavior = true })
    else assert(method == "textDocument/rename"); assert(params.newName == "count"); callback(nil, edits("count")) end
  end
  local result, error = prepare("symbol", { kind = "symbol", target = { path = filename, line = 1, column = 7 }, new_name = "count" })
  assert(result, error)
  assert(#result.operations == 1 and result.operations[1].after == "local count = 1\nprint(count)\n", vim.inspect(result))
  assert(not vim.bo[buffer].modifiable, "prepared buffer was not held")
  assert(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1] == "local value = 1", "preparation mutated source")
  rename.release("symbol", false)
  assert(vim.bo[buffer].modifiable, "release did not restore editability")

  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "local dirty = 1" })
  result, error = prepare("dirty", { kind = "symbol", target = { path = filename, line = 1, column = 7 }, new_name = "count" })
  assert(not result and error:find("unsaved_changes", 1, true), tostring(error))
  vim.api.nvim_buf_call(buffer, function() vim.cmd("silent edit!") end)

  handler = function(method, _, callback) assert(method == "workspace/willRenameFiles"); callback(nil, nil) end
  result, error = prepare("file", { kind = "file", old_path = filename, new_path = root .. "/renamed.lua" })
  assert(result and result.operations[1].kind == "rename", error)
  rename.release("file", false)
  assert(#notices == 0, "aborted move emitted notification")

  client.server_capabilities = {}
  result, error = prepare("unsupported", { kind = "file", old_path = filename, new_path = root .. "/renamed.lua" })
  assert(not result and error:find("unsupported:", 1, true), tostring(error))
  assert(vim.bo[buffer].modifiable)
  assert(vim.uv.fs_stat(filename), "unsupported request moved source")
end, debug.traceback)
rename.close()
startup.wait, vim.lsp.get_clients, vim.lsp.buf_is_attached = original_wait, original_clients, original_attached
vim.api.nvim_buf_delete(buffer, { force = true })
vim.fn.delete(root, "rf")
assert(ok, failure)
print("lsp_rename_symbol: ok")
