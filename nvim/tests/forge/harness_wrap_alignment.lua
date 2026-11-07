local fixture_path = debug.getinfo(1, "S").source:sub(2)

if vim.g.forge_wrap_alignment_child then
  local layout = require("forge.views.harness.layout")
  local replica = require("forge.buffer")
  vim.o.showtabline, vim.o.laststatus = 0, 0
  vim.wo.showbreak = ">>"
  local _, transcript_window, composer_buffer, composer_window = layout.open("wrap-alignment")
  local owner = replica.open("wrap-alignment")
  vim.api.nvim_win_set_buf(transcript_window, owner.buffer)
  layout.configure_transcript_window(transcript_window)
  local function metadata(indent, marker)
    return { target = {}, decoration = {}, editable_region = {},
      layout = { indent = indent, source_indent = indent - 2,
        marker = marker and { text = marker, capture = "Normal" } or nil } }
  end
  assert(replica.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
    { id = "response", text = { string.rep("response ", 30) }, metadata = metadata(2) },
    { id = "thought", text = { "  " .. string.rep("thought ", 30) }, metadata = metadata(4, "○") },
    { id = "tool", text = { "    └   7: first", "        8: second", "        9: third", "       10: fourth" },
      metadata = metadata(6) },
  } }).kind == "Applied")
  require("forge.input").open(owner, transcript_window, { margin = 0,
    columns = { signcolumn = "yes:1", statuscolumn = "%s" },
    wrapping = layout.transcript_wrapping })
  vim.api.nvim_buf_set_lines(composer_buffer, 0, -1, false, { string.rep("composer ", 25) })
  layout.resize_composer(composer_buffer, composer_window)
  vim.api.nvim_set_current_win(transcript_window)
  vim.api.nvim_win_set_cursor(transcript_window, { 1, 0 })
  return
end

for _, width in ipairs({ 80, 100 }) do
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0, "could not start the alignment fixture")
  local success, failure = xpcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", width, 35, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
      vim.g.forge_wrap_alignment_child = true
      dofile(...)
    ]], { fixture_path })
    local screen = vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.cmd("redraw!")
      local rows = {}
      for row = 1, vim.o.lines do
        local cells = {}
        for column = 1, vim.o.columns do cells[#cells + 1] = vim.fn.screenstring(row, column) end
        rows[#rows + 1] = table.concat(cells)
      end
      return rows
    ]], {})
    local rendered = table.concat(screen, "\n")
    for word, expected in pairs({ response = 3, thought = 5, composer = 3 }) do
      local count = 0
      for _, row in ipairs(screen) do
        local start = row:find(word, 1, true)
        if start then
          local column = vim.fn.strdisplaywidth(row:sub(1, start - 1)) + 1
          assert(column == expected, word .. " starts at " .. column .. " instead of " .. expected .. "\n" .. rendered)
          count = count + 1
        end
      end
      assert(count >= 2, word .. " did not wrap\n" .. rendered)
    end
    for _, number in ipairs({ "7:", "8:", "9:", "10:" }) do
      local found = false
      for _, row in ipairs(screen) do
        local start = row:find(number, 1, true)
        if start then
          local column = vim.fn.strdisplaywidth(row:sub(1, start - 1)) + #number
          assert(column == 12, "tool output line-number columns differ\n" .. rendered)
          found = true
        end
      end
      assert(found, "tool output line missing: " .. number .. "\n" .. rendered)
    end
  end, debug.traceback)
  vim.fn.jobstop(child)
  assert(success, failure)
end
print("harness_wrap_alignment: response, thought, tool output, and composer align at 80 and 100 columns")
