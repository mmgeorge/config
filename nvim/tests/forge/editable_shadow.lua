vim.loader.enable(false)
local Shadow = require("forge.editable_shadow")
local text = {}
for index = 1, 30000 do text[index] = "row " .. index end
local shadow = Shadow.new(text)
shadow.sequence.visits = 0
shadow:splice(2, 1, { "λ", "new" })
assert(shadow.sequence.visits < 150, "shadow splice traversed unrelated rows")
assert(shadow:row(2) == "λ" and shadow:row(4) == "row 4")
table.remove(text, 3)
table.insert(text, 3, "new")
table.insert(text, 3, "λ")
math.randomseed(42)
for _ = 1, 100 do
  local start = math.random(0, #text)
  local removed = math.min(math.random(0, 100), #text - start)
  local inserted = {}
  for index = 1, math.random(0, 100) do inserted[index] = tostring(index) end
  shadow:splice(start, removed, inserted)
  for _ = 1, removed do table.remove(text, start + 1) end
  for index = #inserted, 1, -1 do table.insert(text, start + 1, inserted[index]) end
  assert(vim.deep_equal(shadow:text(), text), "shadow splice changed retained rows")
end
shadow:splice(0, shadow:rows(), {})
assert(shadow:rows() == 0)
shadow:splice(0, 0, { "restored" })
assert(shadow:row(0) == "restored")
print("editable_shadow: passed")
