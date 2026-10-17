vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local lookup = require("forge.views.plan_review.workspace_symbol")
local root = vim.fn.tempname()
vim.fn.mkdir(root, "p")
local path = root .. "/source.rs"
vim.fn.writefile({ "é struct Health;" }, path)
local original_clients, original_select = vim.lsp.get_clients, vim.ui.select
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
  assert(vim.wait(1000, function() return done end, 10), "lookup did not settle")
  return target, failure, message
end
local success, failure = xpcall(function()
  local target, error = run(nil, { result = { symbol("HealthExtra"), symbol("Health"), symbol("Health") } })
  assert(target and not error and target.column == 10 and target.capture == "@type", "exact matching, symbol kind or UTF-16 conversion failed")
  assert(requests[1].params.query == "Health")
  assert(selected_count == 0, "duplicates opened an ambiguity picker")
  target, error = run({ name = "combat::Health" }, { result = { symbol("Health", 0, "combat"), symbol("Health", 0, "other") } })
  assert(target and not error, "qualified lookup did not match exact container")
  local _, _, message = run({ excluded = { path } }, { result = { symbol("Health") } })
  assert(message:find("No exact", 1, true), "workspace fallback selected a modified plan file")
  _, error = run(nil, { failure = { message = "server failed" } })
  assert(error:find("server failed", 1, true), "request failure became an empty result")
  _, _, message = run(nil, { result = { symbol("Health") } }, function() return false end)
  assert(message == "cancelled", "stale lookup navigated")
  local second_path = root .. "/other.rs"
  vim.fn.writefile({ "é struct Health;" }, second_path)
  local other = symbol("Health")
  other.location.uri = vim.uri_from_fname(second_path)
  target, error = run(nil, { result = { symbol("Health"), other } })
  assert(target and not error and selected_count == 2, "ambiguous exact matches did not use a picker")
end, debug.traceback)
vim.lsp.get_clients, vim.ui.select = original_clients, original_select
vim.fn.delete(root, "rf")
assert(success, failure)
print("plan_review_workspace_symbol: passed")
vim.cmd("qa!")
