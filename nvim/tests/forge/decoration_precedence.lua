local original_provider = vim.api.nvim_set_decoration_provider
local original_extmark = vim.api.nvim_buf_set_extmark
local provider
vim.api.nvim_set_decoration_provider = function(_, callbacks) provider = callbacks end
package.loaded['forge.decorations'] = nil
local decorations = require('forge.decorations')
vim.api.nvim_set_decoration_provider = original_provider

local succeeded, failure = xpcall(function()
  local spans = {}
  for index = 1, 24 do
    spans[#spans + 1] = {
      range = { start = { row = 0, column = index % 3 }, ['end'] = { row = 0, column = 12 } },
      capture = 'Capture' .. index, priority = 100,
    }
  end
  local entry = { metadata = { visible_decoration = spans } }
  decorations.prepare(entry)
  local sequence = {}
  function sequence:locate() return { id = 'body', entry = entry } end
  function sequence:position() return 1, 0 end
  local session = { buffer = 123, status = 'Applied', editable = {}, sequence = sequence }
  decorations.attach(session)
  local emitted = {}
  vim.api.nvim_buf_set_extmark = function(_, _, _, _, options)
    emitted[#emitted + 1] = options.hl_group
  end
  local gutter = require('forge.gutter')
  local original_highlight = gutter.highlight
  gutter.highlight = function() end
  for _ = 1, 3 do
    emitted = {}
    decorations.prepare(entry)
    provider.on_line(nil, 1000, session.buffer, 0)
    assert(#emitted == #spans)
    for index, capture in ipairs(emitted) do
      assert(capture == spans[index].capture, 'equal-priority host capture precedence changed')
    end
  end
  gutter.highlight = original_highlight
  decorations.detach(session)
end, debug.traceback)
vim.api.nvim_buf_set_extmark = original_extmark
if not succeeded then
  io.stderr:write(tostring(failure), '\n')
  vim.cmd('cquit 1')
end
print('decoration precedence passed')
vim.cmd('qa!')
