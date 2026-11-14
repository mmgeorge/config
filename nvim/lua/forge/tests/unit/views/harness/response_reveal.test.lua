-- Unit tests for `forge.views.harness.response_reveal` (response_reveal.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---Build a committed transcript replica whose blocks occupy one row each.
---@param buffer integer
---@param blocks {id: string, node: table, marker?: string, row_count?: integer}[]
local function document(buffer, blocks)
  local rows, row = {}, 0
  for _, block in ipairs(blocks) do
    rows[block.id] = row
    row = row + (block.row_count or 1)
  end
  local sequence = {}
  function sequence:count() return #blocks end
  function sequence:at(index)
    local block = blocks[index + 1]
    return { id = block.id, entry = { row_count = block.row_count or 1, metadata = { node = block.node,
      layout = block.marker and { marker = { text = "●", capture = block.marker } } or nil } } }
  end
  function sequence:position(id) return nil, rows[id] end
  return { buffer = buffer, revision = 1, sequence = sequence, physical_row = function(value) return value end }
end

-- An unconfirmed steering row carries its own marker capture, so the reveal of a completed
-- exchange anchors on the original prompt instead of input no report showed the model read.
T["unconfirmed_steering_never_anchors_the_reveal"] = function()
  local Reveal = require("forge.views.harness.response_reveal")
  local transcript = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(transcript, 0, -1, false, vim.tbl_map(tostring, vim.fn.range(1, 40)))
  local blocks = function(steering_marker)
    return {
      { id = "prompt", node = { id = "prompt", kind = "message" }, marker = "ForgeHarnessPrompt" },
      { id = "exchange", node = { id = "exchange", kind = "exchange", lifecycle = "settled" } },
      { id = "steering", node = { id = "steering", kind = "message" }, marker = steering_marker },
      { id = "response", node = { id = "response", kind = "message", display = "full" },
        marker = "ForgeHarnessResponse", row_count = 37 },
    }
  end
  local function revealed_row(steering_marker)
    vim.cmd("only")
    local view_window = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(view_window, transcript)
    vim.api.nvim_win_set_height(view_window, 5)
    vim.cmd("belowright new")
    vim.api.nvim_win_set_cursor(view_window, { 40, 0 })
    Reveal.new():publish(document(transcript, blocks(steering_marker)), { [view_window] = true })
    return vim.api.nvim_win_get_cursor(view_window)[1]
  end
  -- The consumed steering row anchors the reveal, which proves the fixture reaches that path.
  eq(revealed_row("ForgeHarnessPrompt"), 3)
  eq(revealed_row("ForgeHarnessUnconfirmedPrompt"), 1)
end

return T
