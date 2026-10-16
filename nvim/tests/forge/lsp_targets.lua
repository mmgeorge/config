local resolver = require("forge.lsp.target")
local snapshot = {
  path = "/repo/store.rs", complete = true,
  lines = { "impl First {", "  fn insert() {}", "}", "impl Second {", "  fn insert() {}", "}", "  α.insert(); β.insert();" },
  declarations = {
    { name = "First", kind = "impl_item", parent = vim.NIL },
    { name = "insert", parent = 0, line = 2, column = 6 },
    { name = "Second", kind = "impl_item", parent = vim.NIL },
    { name = "insert", parent = 2, line = 5, column = 6 },
    { name = "First", kind = "struct_item", parent = vim.NIL, line = 1, column = 6 },
  },
}
local queries = 0
local function resolve(target)
  local result, failure
  resolver.resolve("/repo", target, function(_, receive) receive(snapshot) end, function(request, receive)
    queries = queries + 1
    assert(request.operation == "workspace_symbols")
    receive({ rows = { { path = snapshot.path, line = 2, column = 6 }, { path = snapshot.path, line = 5, column = 6 } } })
  end, function(value, error) result, failure = value, error end)
  return result, failure
end
local structure = assert(resolve({ path = snapshot.path, symbol = "First" }))
assert(structure.line == 1, "impl container collided with its type declaration")
local result, failure = resolve({ path = snapshot.path, symbol = "insert" })
assert(not result and failure:find("First::insert", 1, true) and failure:find("Second::insert", 1, true), failure)
assert(queries == 0, "ambiguous file lookup issued a semantic query")
result = assert(resolve({ path = snapshot.path, symbol = "Second::insert" }))
assert(result.line == 5 and result.column == 6)
result = assert(resolve({ symbol = "First::insert" }))
assert(result.line == 2 and queries == 1)
result, failure = resolve({ path = snapshot.path, line = 7, symbol = "insert" })
assert(not result and failure:find("7:5", 1, true) and failure:find("7:17", 1, true), failure)
result = assert(resolve({ path = snapshot.path, line = 7, column = 17, symbol = "insert" }))
assert(result.column == 17)
result, failure = resolve({ path = snapshot.path, line = 7, column = 1, symbol = "insert" })
assert(not result and failure:find("target_mismatch", 1, true), failure)
result = assert(resolve({ path = snapshot.path, line = 7, column = 17 }))
assert(result.column == 17)
print("lsp_targets: passed")
