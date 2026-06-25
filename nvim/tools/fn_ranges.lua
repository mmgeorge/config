-- Print authoritative top-level function ranges for a Lua file using treesitter.
-- Usage: nvim --headless -l nvim/tools/fn_ranges.lua <path>
-- Output: one line per top-level function: <name>\t<start_line>\t<end_line> (1-based, inclusive)
local path = vim.v.argv[#vim.v.argv]
local source = table.concat(vim.fn.readfile(path), "\n")
local parser = vim.treesitter.get_string_parser(source, "lua")
local tree = parser:parse()[1]
local root = tree:root()

--- Return the dotted/colon name text for a function node's name field, or nil.
local function name_text(node)
  if not node then return nil end
  return vim.treesitter.get_node_text(node, source)
end

local out = {}
for child in root:iter_children() do
  local kind = child:type()
  if kind == "function_declaration" then
    -- function M._foo() ... end  /  local function foo() ... end
    local name_node = child:field("name")[1]
    local name = name_text(name_node) or "?"
    local s, _, e, _ = child:range()
    out[#out + 1] = ("%s\t%d\t%d"):format(name, s + 1, e + 1)
  elseif kind == "variable_declaration" or kind == "assignment_statement" then
    -- local foo = function() end  /  foo = function() end (forward-assigned)
    local value = child:field("value")[1] or (child:field("rhs") and child:field("rhs")[1])
    if value and value:type() == "function_definition" then
      local name_field = child:field("name")[1]
        or (child:field("lhs") and child:field("lhs")[1])
      local name = name_text(name_field) or "?"
      local s, _, e, _ = child:range()
      out[#out + 1] = ("%s\t%d\t%d"):format(name, s + 1, e + 1)
    end
  end
end
io.write(table.concat(out, "\n") .. "\n")
