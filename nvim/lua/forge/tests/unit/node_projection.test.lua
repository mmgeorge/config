-- Unit tests for `forge.node_projection` (node_projection.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---Project a heading block whose fold covers one indented body block.
---@param closed boolean
---@return table heading Projected heading block.
---@return table mapping Visible row spans by block.
local function project_section(closed)
  local buffer = require("forge.buffer")
  local projection = require("forge.node_projection")
  local function metadata(fold)
    return { target = {}, decoration = {}, editable_region = {}, fold = fold }
  end
  local snapshot = {
    document = "plan-review",
    revision = 1,
    block = {
      {
        id = "heading",
        text = { "  Accesses" },
        metadata = metadata({ {
          id = "plan:section:accesses",
          start = { row = 0, column = 0 },
          ["end"] = { block = "body", position = { row = 1, column = 0 } },
          closed = closed,
          heading_marker = true,
        } }),
      },
      { id = "body", text = { "    RunSession { phase, score }" }, metadata = metadata({}) },
    },
  }
  local owner = projection.new(buffer.fragment(snapshot, true))
  local projected, mapping = projection.render(owner)
  return projected.block[1], mapping
end

T["heading_marker_shows_open_state_before_heading_text"] = function()
  local heading, mapping = project_section(false)
  local marker = vim.tbl_filter(function(gutter) return gutter.placement == "inline" end, heading.metadata.gutter)
  eq(#marker, 1)
  eq(marker[1].position, { row = 0, column = 2 })
  eq(marker[1].chunk[1].text, "▾ ")
  eq(mapping.body ~= nil, true)
end

T["heading_marker_shows_closed_state_and_hides_body"] = function()
  local heading, mapping = project_section(true)
  local marker = vim.tbl_filter(function(gutter) return gutter.placement == "inline" end, heading.metadata.gutter)
  eq(marker[1].chunk[1].text, "▸ ")
  eq(mapping.body, nil)
end

return T
