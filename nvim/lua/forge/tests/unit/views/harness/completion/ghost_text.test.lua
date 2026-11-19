-- Unit tests for `forge.views.harness.completion.ghost_text` (ghost_text.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---Select `item` in a stand-in for blink.cmp, whose text-edit library returns the item's own edit.
local function select_item(item)
  package.loaded["blink.cmp"] = { get_selected_item = function() return item end }
  package.loaded["blink.cmp.lib.text_edits"] = { get_from_item = function(selected) return vim.deepcopy(selected.textEdit) end }
  MiniTest.finally(function()
    package.loaded["blink.cmp"], package.loaded["blink.cmp.lib.text_edits"] = nil, nil
  end)
end

local function file_item(path, start_character, end_character)
  return { label = "@" .. path, textEdit = { newText = "@" .. path,
    range = { start = { line = 0, character = start_character }, ["end"] = { line = 0, character = end_character } } } }
end

T["ghost_text_shows_only_completions_that_extend_the_typed_text"] = function()
  local ghost_text = require("forge.views.harness.completion.ghost_text")
  local composer = buffer.scratch()
  vim.api.nvim_set_current_buf(composer)
  buffer.set(composer, { "Read @money" })
  vim.api.nvim_win_set_cursor(0, { 1, 11 })

  -- A fuzzy match inserts text the user did not type, so its suffix would render as `@moneyoney.ts`.
  select_item(file_item("src/money.ts", 5, 11))
  eq(ghost_text.enabled(), false)
  select_item(file_item("money.ts", 5, 11))
  eq(ghost_text.enabled(), true)
  select_item(nil)
  eq(ghost_text.enabled(), true)
end

return T
