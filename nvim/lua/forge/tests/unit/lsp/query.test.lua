local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["workspace_diagnostics_refresh_authorized_changed_files_before_collection"] = function()
  local directory = temp.directory("diagnostic-refresh")
  local path = directory .. "/source.rs"
  local unrelated = directory .. "/notes.md"
  vim.fn.writefile({ "const VALUE: bool = true;" }, path)
  vim.fn.writefile({ "notes" }, unrelated)
  local client = { id = 995, root_dir = directory, config = { filetypes = { "rust" } } }
  local calls, authorized, response = {}, {}, nil
  stub(require("forge.lsp.workspace"), "resolve", function(_, _, receive)
    receive({ client = client, selection = { path = directory, language = "rust" } })
    return function() end
  end)
  stub(require("forge.lsp.semantic"), "request", function(_, arguments, _, receive)
    if arguments.path then eq(authorized[arguments.path], true) end
    calls[#calls + 1] = arguments.path or "workspace"
    receive({ rows = {}, source = "workspace" })
    return function() end
  end)
  local request = vim.json.decode(table.concat(vim.fn.readfile(
    "nvim/lua/forge/tests/fixtures/protocol/lsp-diagnostic-refresh.json"), "\n"))
  request.workspace.path = directory
  request.diagnostic_paths = { path, path, unrelated }
  require("forge.lsp.query").request(directory, request, function(paths, receive)
    local allowed = {}
    for _, target in ipairs(paths) do authorized[target], allowed[target] = true, true end
    receive(allowed)
  end, function() error("empty diagnostics requested source analysis") end,
  function(result, failure) eq(failure, nil) response = result end)
  eq(calls, { path, "workspace" })
  eq(response.rows, {})
end

T["failed_file_refresh_does_not_return_empty_workspace_success"] = function()
  local directory = temp.directory("diagnostic-refresh-failure")
  local path = directory .. "/source.rs"
  vim.fn.writefile({ "const VALUE: bool = true;" }, path)
  local client = { id = 996, root_dir = directory, config = { filetypes = { "rust" } } }
  stub(require("forge.lsp.workspace"), "resolve", function(_, _, receive)
    receive({ client = client, selection = { path = directory, language = "rust" } })
    return function() end
  end)
  stub(require("forge.lsp.semantic"), "request", function(_, arguments, _, receive)
    eq(arguments.path, path)
    receive(nil, "server_error: fixture failure")
    return function() end
  end)
  local result, failure
  require("forge.lsp.query").request(directory, {
    operation = "diagnostics", workspace = { path = directory, language = "rust" }, diagnostic_paths = { path },
  }, function(paths, receive) receive({ [paths[1]] = true }) end, function() end,
  function(reply, message) result, failure = reply, message end)
  eq(result, nil)
  eq(failure, "server_error: fixture failure")
end

T["denied_changed_file_is_never_sent_to_the_language_server"] = function()
  local directory = temp.directory("diagnostic-denied")
  local path = directory .. "/source.rs"
  vim.fn.writefile({ "fn main() {}" }, path)
  local client = { id = 994, root_dir = directory, config = { filetypes = { "rust" } } }
  stub(require("forge.lsp.workspace"), "resolve", function(_, _, receive)
    receive({ client = client, selection = { path = directory, language = "rust" } })
    return function() end
  end)
  stub(require("forge.lsp.semantic"), "request", function() error("denied source reached server") end)
  local failure
  require("forge.lsp.query").request(directory, {
    operation = "diagnostics", workspace = { path = directory, language = "rust" }, diagnostic_paths = { path },
  }, function(_, receive) receive({}) end, function() end,
  function(_, message) failure = message end)
  eq(failure, "access_denied: " .. path)
end

return T
