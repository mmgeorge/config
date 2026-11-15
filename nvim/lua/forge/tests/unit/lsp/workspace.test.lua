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
  local result, failure
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, path in ipairs(paths) do allowed[path] = true end receive(allowed)
  end, function() return true end, function(value, message) result, failure = value, message end)
  wait_until(function() return result or failure end, "inventory did not finish")
  eq(failure, nil)
  eq(#result.rows, 2)
  eq(result.rows[1].path, require("forge.lsp.source").canonical(root))
  eq(result.rows[1].text, "typescript")
  eq(result.rows[2].text, "rust")
end

T["configured_preference_resolves_ambiguity"] = function()
  local workspace = require("forge.lsp.workspace")
  stub(vim.lsp, "get_configs", function() return {
    { name = "first", filetypes = { "typescript" } },
    { name = "second", filetypes = { "javascript" } },
  } end)
  stub(require("forge"), "config", {})
  local selected, failure = workspace.config({path = "/project", language = "typescript"})
  eq(selected, nil)
  eq(failure:match("^ambiguous_server:") ~= nil, true)
  require("forge").config.lsp = { workspace_servers = { typescript = "second" } }
  eq(workspace.config({path = "/project", language = "typescript"}).name, "second")
end

T["resolve_keeps_caller_pending_until_startup_answers"] = function()
  local root = temp.directory("pending-startup")
  local manifest = root .. "/Cargo.toml"
  vim.fn.writefile({ "[package]" }, manifest)
  stub(vim.lsp, "get_configs", function() return { { name = "rust", filetypes = { "rust" } } } end)
  local answer
  stub(require("forge.lsp.startup"), "selected", function(_, _, receive) answer = receive return function() end end)
  local completed = false
  require("forge.lsp.workspace").resolve({path = root, language = "rust"}, function(paths, receive) receive({ [paths[1]] = true }) end,
    function(value, failure) eq(failure, nil) eq(value.client.id, 12) completed = true end)
  eq(completed, false)
  answer({ id = 12 })
  eq(completed, true)
end

T["lua_workspace_uses_configured_root_without_starting_server"] = function()
  local root = temp.directory("lua-inventory")
  vim.fn.mkdir(root .. "/lua", "p")
  vim.fn.writefile({ "{}" }, root .. "/.luarc.json")
  vim.fn.writefile({ "return {}" }, root .. "/lua/example.lua")
  stub(vim.lsp, "get_configs", function() return {
    { name = "lua-fixture", filetypes = { "lua" }, root_markers = { ".luarc.json" } },
  } end)
  stub(vim.lsp, "start", function() error("discovery started a server") end)
  local result, failure
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, path in ipairs(paths) do allowed[path] = true end receive(allowed)
  end, function() return true end, function(value, message) result, failure = value, message end)
  wait_until(function() return result or failure end, "Lua inventory did not finish")
  eq(failure, nil)
  eq(#result.rows, 1)
  eq(result.rows[1].path, require("forge.lsp.source").canonical(root))
  eq(result.rows[1].text, "lua")
end

T["lua_selection_preserves_settings_and_uses_source_anchor"] = function()
  local root = temp.directory("lua-selection")
  local path = root .. "/init.lua"
  vim.fn.writefile({ "return {}" }, path)
  local config = { name = "lua-fixture", filetypes = { "lua" }, root_dir = root, settings = { Lua = { runtime = { version = "LuaJIT" } } } }
  stub(vim.lsp, "get_configs", function() return { config } end)
  stub(vim.lsp, "get_clients", function() return {} end)
  stub(require("forge.lsp.startup"), "selected", function(anchor, selected, receive)
    eq(anchor, require("forge.lsp.source").canonical(path))
    eq(selected, config)
    receive({ id = 42 })
    return function() end
  end)
  local result, failure
  require("forge.lsp.workspace").resolve({ path = root, language = "lua" }, function(paths, receive)
    local allowed = {} for _, requested in ipairs(paths) do allowed[requested] = true end receive(allowed)
  end, function(value, message) result, failure = value, message end)
  wait_until(function() return result or failure end, "Lua selection did not finish")
  eq(failure, nil)
  eq(result.client.id, 42)
end

T["lua_discovery_distinguishes_nested_and_shared_roots"] = function()
  local root = temp.directory("lua-roots")
  for _, directory in ipairs({ "/one", "/two", "/nested" }) do
    vim.fn.mkdir(root .. directory, "p")
    vim.fn.writefile({ "return {}" }, root .. directory .. "/init.lua")
  end
  vim.fn.writefile({ "{}" }, root .. "/.luarc.json")
  vim.fn.writefile({ "{}" }, root .. "/nested/.luarc.json")
  stub(vim.lsp, "get_configs", function() return {
    { name = "lua-fixture", filetypes = { "lua" }, root_markers = { ".luarc.json" } },
  } end)
  local result, failure
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, requested in ipairs(paths) do allowed[requested] = true end receive(allowed)
  end, function() return true end, function(value, message) result, failure = value, message end)
  wait_until(function() return result or failure end, "nested discovery did not finish")
  eq(failure, nil)
  eq(#result.rows, 2)
  eq(result.rows[1].path, require("forge.lsp.source").canonical(root))
  eq(result.rows[2].path, require("forge.lsp.source").canonical(root .. "/nested"))
end

T["workspace_pagination_preserves_selector_pairs"] = function()
  local root = temp.directory("workspace-pages")
  vim.fn.writefile({ "{}" }, root .. "/package.json")
  vim.fn.writefile({ "[package]" }, root .. "/Cargo.toml")
  local result
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, requested in ipairs(paths) do allowed[requested] = true end receive(allowed)
  end, function() return true end, function(value) result = value end, { limit = 1, offset = 1 })
  wait_until(function() return result end, "workspace page did not finish")
  eq(#result.rows, 1)
  eq(result.rows[1].text, "typescript")
  eq(result.next_offset, nil)
end

T["missing_lua_configuration_is_an_explicit_discovery_warning"] = function()
  local root = temp.directory("lua-unavailable")
  vim.fn.writefile({ "return {}" }, root .. "/init.lua")
  stub(vim.lsp, "get_configs", function() return {} end)
  local result
  require("forge.lsp.workspace").list(root, function(paths, receive)
    local allowed = {} for _, requested in ipairs(paths) do allowed[requested] = true end receive(allowed)
  end, function() return true end, function(value) result = value end)
  wait_until(function() return result end, "unavailable Lua discovery did not finish")
  eq(#result.rows, 0)
  eq(result.warnings[1]:find("unavailable: no enabled LSP configuration for lua", 1, true), 1)
end

return T
