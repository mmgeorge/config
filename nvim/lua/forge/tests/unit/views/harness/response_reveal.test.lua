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


-- A queued successor replaces the finished exchange at the tail within a few owner steps, so
-- revealing the finished prompt first moved the transcript twice at every long hand-off.
T["settlement_with_a_pending_successor_is_never_revealed"] = function()
  local Reveal = require("forge.views.harness.response_reveal")
  local transcript = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(transcript, 0, -1, false, vim.tbl_map(tostring, vim.fn.range(1, 40)))
  local function blocks(lifecycle)
    return {
      { id = "prompt", node = { id = "prompt", kind = "message" }, marker = "ForgeHarnessPrompt" },
      { id = "exchange", node = { id = "exchange", kind = "exchange", lifecycle = lifecycle } },
      { id = "response", node = { id = "response", kind = "message", display = "full" },
        marker = "ForgeHarnessResponse", row_count = 38 },
    }
  end
  local view_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(view_window, transcript)
  vim.api.nvim_win_set_height(view_window, 5)
  vim.cmd("belowright new")
  vim.api.nvim_win_set_cursor(view_window, { 40, 0 })
  local reveal = Reveal.new()
  reveal:publish(document(transcript, blocks("open")), { [view_window] = true })
  reveal:publish(document(transcript, blocks("settled")), { [view_window] = true }, false, true)
  eq(vim.api.nvim_win_get_cursor(view_window)[1], 40)
  -- The skipped reveal stays skipped once the hand-off flag clears.
  reveal:publish(document(transcript, blocks("settled")), { [view_window] = true })
  eq(vim.api.nvim_win_get_cursor(view_window)[1], 40)
end

-- A hidden tab's transcript still synchronizes, so its completion was observed and marked seen
-- while no window of the current tab showed it; showing the tab afterwards revealed nothing.
T["completion_in_a_hidden_tab_is_revealed_when_the_tab_is_shown"] = function()
  local Reveal = require("forge.views.harness.response_reveal")
  local transcript = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(transcript, 0, -1, false, vim.tbl_map(tostring, vim.fn.range(1, 40)))
  local function blocks(lifecycle)
    return {
      { id = "prompt", node = { id = "prompt", kind = "message" }, marker = "ForgeHarnessPrompt" },
      { id = "exchange", node = { id = "exchange", kind = "exchange", lifecycle = lifecycle } },
      { id = "response", node = { id = "response", kind = "message", display = "full" },
        marker = "ForgeHarnessResponse", row_count = 38 },
    }
  end
  local transcript_tab = vim.api.nvim_get_current_tabpage()
  local view_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(view_window, transcript)
  vim.api.nvim_win_set_height(view_window, 5)
  vim.cmd("belowright new")
  local composer_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_cursor(view_window, { 40, 0 })
  local reveal = Reveal.new(composer_window)
  reveal:publish(document(transcript, blocks("live")), { [view_window] = true })
  vim.api.nvim_win_set_cursor(view_window, { 30, 0 })
  vim.cmd("tabnew")
  reveal:publish(document(transcript, blocks("settled")), { [view_window] = true })
  eq(vim.api.nvim_win_get_cursor(view_window)[1], 30)

  vim.api.nvim_set_current_tabpage(transcript_tab)
  vim.api.nvim_set_current_win(composer_window)
  reveal:publish(document(transcript, blocks("settled")), { [view_window] = true })
  eq(vim.api.nvim_win_call(view_window, function() return vim.fn.line("w0") end), 1)
  -- The deferred reveal runs once; a later publication keeps the reader's position.
  vim.api.nvim_win_set_cursor(view_window, { 30, 0 })
  reveal:publish(document(transcript, blocks("settled")), { [view_window] = true })
  eq(vim.api.nvim_win_get_cursor(view_window)[1], 30)
end

return T
