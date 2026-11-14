local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["manifest_inventory_does_not_start_servers"] = function()
  local root = temp.directory("inventory")
  for _, directory in ipairs({ "/nested", "/node_modules/dependency", "/other/.git" }) do vim.fn.mkdir(root .. directory, "p") end
  vim.fn.writefile({ "{}" }, root .. "/package.json")
  vim.fn.writefile({ "[package]" }, root .. "/nested/Cargo.toml")
  vim.fn.writefile({ "{}" }, root .. "/node_modules/dependency/package.json")
  vim.fn.writefile({ "{}" }, root .. "/other/package.json")
  stub(vim.lsp, "start", function() error("listing started a server") end)
  stub(vim.lsp, "get_configs", function() error("listing resolved a server") end)
  local result, failure
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, path in ipairs(paths) do allowed[path] = true end receive(allowed)
  end, function() return true end, function(value, message) result, failure = value, message end)
  wait_until(function() return result or failure end, "inventory did not finish")
  eq(failure, nil)
  eq(#result.rows, 2)
  eq(result.rows[1].path:match("Cargo.toml$"), "Cargo.toml")
end

T["configured_preference_resolves_ambiguity"] = function()
  local workspace = require("forge.lsp.workspace")
  stub(vim.lsp, "get_configs", function() return {
    { name = "first", filetypes = { "typescript" } },
    { name = "second", filetypes = { "javascript" } },
  } end)
  stub(require("forge"), "config", {})
  local selected, failure = workspace.config("/project/package.json")
  eq(selected, nil)
  eq(failure:match("^ambiguous_server:") ~= nil, true)
  require("forge").config.lsp = { workspace_servers = { typescript = "second" } }
  eq(workspace.config("/project/package.json").name, "second")
end

T["resolve_keeps_caller_pending_until_startup_answers"] = function()
  local root = temp.directory("pending-startup")
  local manifest = root .. "/Cargo.toml"
  vim.fn.writefile({ "[package]" }, manifest)
  stub(vim.lsp, "get_configs", function() return { { name = "rust", filetypes = { "rust" } } } end)
  local answer
  stub(require("forge.lsp.startup"), "selected", function(_, _, receive) answer = receive return function() end end)
  local completed = false
  require("forge.lsp.workspace").resolve(manifest, function(paths, receive) receive({ [paths[1]] = true }) end,
    function(value, failure) eq(failure, nil) eq(value.client.id, 12) completed = true end)
  eq(completed, false)
  answer({ id = 12 })
  eq(completed, true)
end

return T
