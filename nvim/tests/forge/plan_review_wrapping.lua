vim.loader.enable(false)
if not vim.g.forge_wrapping_grid then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 120, 40, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_wrapping_grid = true
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
      dofile('nvim/tests/forge/plan_review_wrapping.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("plan_review_wrapping: prose indentation, diff backgrounds, and continuation markers passed at 40, 80, and 120 columns")
  return
end

local client = require("forge.client")
local encode_delivery = dofile("nvim/tests/forge/support/plan_delivery.lua")
local request
client.host_get_generation = function() return 1 end
client.host_is_accepting = function() return true end
client.request = function(request_specification, receive)
  local params = request_specification.params or {}
   request = { params = params, receive = encode_delivery(receive) } end
local buffer = vim.api.nvim_get_current_buf()
local window = vim.api.nvim_get_current_win()
local owner = require("forge.views.plan_review.document").attach({ session_id = "wrapping", buffer = buffer,
  window = window, plan = { id = "plan", review_digest = "digest" } }, function(_, failure) assert(not failure, failure) end)
local text = "Integrate combat with directional dash, health and immunity, timed collection combos, and the ten-orb victory condition. "
local block, source = {}, {}
local changes = {
  [3] = { marker = "~", background = "ForgeModifyBg", number = "ForgeModifyLineNr", color = 0x2a1f00 },
  [4] = { marker = "+", background = "ForgeAddBg", number = "ForgeAddLineNr", color = 0x002800 },
  [5] = { marker = "-", background = "ForgeDeleteBg", number = "ForgeDeleteLineNr", color = 0x200000 },
}
for index, prefix in ipairs({ "", "  - ", "    ", "    ", "    " }) do
  local id = "row-" .. index
  local line = prefix .. text .. text
  local metadata = { decoration = {}, target = {}, editable_region = {} }
  local change = changes[index]
  if change then
    metadata.gutter = { { position = { row = 0, column = 0 }, priority = 200,
      chunk = { { text = "       ", capture = change.background },
        { text = change.marker, capture = change.number }, { text = "  ", capture = change.background } } } }
    metadata.decoration = { { range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } },
      capture = change.background, priority = 100 } }
  end
  block[index] = { id = id, text = { line }, metadata = metadata }
  source[index] = { id = id, block = id, text = line, position = { row = 0, column = 0 },
    source_line = index, metadata = metadata }
end
request.receive({ path = vim.fn.tempname() .. ".md", version = 1, saved_source_digest = "saved", annotation = {},
  source_row = source, snapshot = { document = request.params.document, revision = 0, block = block } })
for _, change in pairs(changes) do
  vim.api.nvim_set_hl(0, change.background, { bg = change.color })
  vim.api.nvim_set_hl(0, change.number, { fg = 0xffb86c })
end
for _, width in ipairs({ 40, 80, 120 }) do
  vim.o.columns = width
  for row, expected_indent in ipairs({ 0, 2, 14, 14, 14 }) do
    vim.api.nvim_win_set_cursor(window, { row, 0 })
    vim.cmd("normal! zt")
    vim.cmd("redraw!")
    local first = vim.fn.screenpos(window, row, 1)
    local line = vim.api.nvim_buf_get_lines(buffer, row - 1, row, false)[1]
    local last = vim.fn.screenpos(window, row, #line)
    assert(last.row > first.row, "fixture must wrap")
    for screen_row = first.row + 1, last.row do
      local cells = {}
      for column = 1, width do
        local cell = vim.api.nvim__inspect_cell(1, screen_row - 1, column - 1)
        cells[column] = cell[1]
        if changes[row] then
          assert(cell[2].background == changes[row].color, "diff background gap at " .. screen_row .. ":" .. column .. " " .. vim.inspect(cell))
        end
      end
      if changes[row] then
        assert(cells[8] == changes[row].marker, "continuation lost its change marker")
        cells[8] = " "
      end
      assert(table.concat(cells):find("%S") == expected_indent + 1,
        "continuation inherited another row's indent: " .. vim.inspect({ width, row, table.concat(cells) }))
    end
  end
end
local copied
local original_setreg = vim.fn.setreg
vim.fn.setreg = function(register, rows, kind)
  assert(register == "+" and kind == "V")
  copied = rows
end
vim.api.nvim_win_set_cursor(window, { 3, 0 })
owner.gutter_selection.start()
vim.fn.maparg("<Space>l", "x", false, true).callback()
vim.fn.setreg = original_setreg
assert(copied and #copied == 1 and copied[1] == "       ~  " .. source[3].text,
  "copying a displayed gutter duplicated its reserved whitespace")
owner.close()
