local paths = require("forge.lsp.paths")
local function node(name) return { path = "/repo/" .. name .. ".rs", line = 1, column = 1, text = name } end
local graph = { start = { "left", "right" }, left = { "start", "end" }, right = { "end" }, ["end"] = {} }
local requests = 0
local function search(options, failure_after)
  local result, partial, failure, reasons
  local arguments = vim.tbl_extend("force", { from = node("start"), to = node("end"), max_depth = 3, max_paths = 10 }, options or {})
  paths.search(arguments, function(request, receive)
    requests = requests + 1
    if failure_after and requests > failure_after then receive({ rows = {} }, "budget_exhausted: fixture") return end
    local name = request.path:match("([^/]+)%.rs$")
    if request.operation == "call_root" then receive({ rows = { node(name) } })
    else
      local root = node(name)
      root.is_root = true
      local rows = { root }
      for _, child in ipairs(graph[name]) do rows[#rows + 1] = node(child) end
      receive({ rows = rows })
    end
  end, function(found, incomplete, error, detail) result, partial, failure, reasons = found, incomplete, error, detail end)
  return result, partial, failure, reasons
end
local result, partial, failure = search()
assert(not failure and not partial and #result == 2)
assert(result[1][2].text == "left" and result[2][2].text == "right")
assert(#result[1] == 3 and #result[2] == 3, "cycle entered a returned path")
result, partial = search({ max_depth = 1 })
assert(#result == 0 and not partial)
result, partial = search({ max_paths = 1 })
assert(#result == 1 and partial)
requests = 0
local reasons
result, partial, failure, reasons = search({}, 3)
assert(#result == 0 and partial and not failure and reasons[1] == "LSP request limit reached. Search incomplete.")
result = search({ to = node("start") })
assert(#result == 1 and #result[1] == 1)
print("lsp_paths: passed")
