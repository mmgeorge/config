-- Unit tests for `forge.views.plan_review.workspace_symbol` (workspace_symbol.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["plan_review_workspace_symbol"] = function()
  local lookup = require("forge.views.plan_review.workspace_symbol")
  local root = temp.directory("workspace")
  local path = root .. "/source.rs"
  vim.fn.writefile({ "é struct Health;" }, path)
  local responses, requests, selected_count = {}, {}, 0
  local client = { id = 1, name = "fixture", root_dir = root, config = {}, offset_encoding = "utf-16" }
  function client:is_stopped() return false end
  function client:supports_method() return true end
  function client:cancel_request() end
  function client:request(method, params, receive)
    requests[#requests + 1] = { method = method, params = params }
    local response = table.remove(responses, 1)
    vim.schedule(function() receive(response.failure, response.result) end)
    return true, #requests
  end
  vim.lsp.get_clients = function() return { client } end
  vim.ui.select = function(choices, _, receive) selected_count = #choices receive(choices[2]) end
  local function symbol(name, line, container)
    return { name = name, containerName = container, kind = 23,
      location = { uri = vim.uri_from_fname(path), range = { start = { line = line or 0, character = 9 },
        ["end"] = { line = line or 0, character = 15 } } } }
  end
  local function run(query, response, current)
    responses = { response }
    local done, target, failure, message = false
    lookup.request(vim.tbl_extend("force", { name = "Health", workspace = root, excluded = {} }, query or {}),
      current or function() return true end, function(found, error, information)
        done, target, failure, message = true, found, error, information
      end)
    wait_until(function() return done end, "lookup did not settle")
    return target, failure, message
  end
  local target, error = run(nil, { result = { symbol("HealthExtra"), symbol("Health"), symbol("Health") } })
  assert(target and not error and target.column == 10 and target.capture == "@type", "exact matching, symbol kind or UTF-16 conversion failed")
  eq(requests[1].params.query, "Health")
  eq(selected_count, 0, { fail_reason = "duplicates opened an ambiguity picker" })
  target, error = run({ name = "combat::Health" }, { result = { symbol("Health", 0, "combat"), symbol("Health", 0, "other") } })
  assert(target and not error, "qualified lookup did not match exact container")
  local _, _, message = run({ excluded = { path } }, { result = { symbol("Health") } })
  assert(message:find("No exact", 1, true), "workspace fallback selected a modified plan file")
  _, error = run(nil, { failure = { message = "server failed" } })
  assert(error:find("server failed", 1, true), "request failure became an empty result")
  _, _, message = run(nil, { result = { symbol("Health") } }, function() return false end)
  eq(message, "cancelled", { fail_reason = "stale lookup navigated" })
  local second_path = root .. "/other.rs"
  vim.fn.writefile({ "é struct Health;" }, second_path)
  local other = symbol("Health")
  other.location.uri = vim.uri_from_fname(second_path)
  target, error = run(nil, { result = { symbol("Health"), other } })
  assert(target and not error and selected_count == 2, "ambiguous exact matches did not use a picker")
end

return T
