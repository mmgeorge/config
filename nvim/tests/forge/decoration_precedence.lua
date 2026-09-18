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

  local captures = { 'ForgeAddBg', 'ForgeDeleteBg', 'ForgeInlineAddBg', 'ForgeInlineDeleteBg', 'String', 'RenderMarkdownCode' }
  local expected = { true, true, false, false, false, true }
  local backgrounds = {}
  for index, capture in ipairs(captures) do
    backgrounds[index] = { range = { start = { row = 0, column = 0 }, ['end'] = { row = 1, column = 0 } },
      capture = capture, priority = 90 }
  end
  entry.metadata.visible_decoration = backgrounds
  decorations.prepare(entry)
  decorations.attach(session)
  gutter.highlight = function() end
  emitted = {}
  vim.api.nvim_buf_set_extmark = function(_, _, _, _, options) emitted[#emitted + 1] = options end
  provider.on_line(nil, 1000, session.buffer, 0)
  for index, options in ipairs(emitted) do
    assert(options.hl_eol == expected[index], 'viewport background width changed for ' .. captures[index])
    assert(options.priority == 90, 'background width changed decoration precedence')
  end
  assert(#emitted == #captures)
  gutter.highlight = original_highlight
  decorations.detach(session)
  vim.api.nvim_buf_set_extmark = original_extmark

  local replica = require('forge.buffer')
  local persistent = replica.open('full-width-diff')
  local text = { 'short', '', string.rep('wrapped code ', 20) }
  local metadata = { decoration = backgrounds, target = {}, editable_region = {} }
  assert(replica.apply_snapshot(persistent, { document = persistent.document, revision = 0,
    block = { { id = 'diff', text = text, metadata = metadata } } }).kind == 'Applied')
  for index, handle in ipairs(persistent.marks.diff) do
    local mark = vim.api.nvim_buf_get_extmark_by_id(persistent.buffer, persistent.namespace, handle, { details = true })
    assert((mark[3].hl_eol or false) == expected[index], 'persistent background width changed for ' .. captures[index])
  end
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(persistent.buffer, 0, -1, false), text),
    'background rendering inserted padding into source text')
  replica.close(persistent)
end, debug.traceback)
vim.api.nvim_buf_set_extmark = original_extmark
if not succeeded then
  io.stderr:write(tostring(failure), '\n')
  vim.cmd('cquit 1')
end
print('decoration precedence passed')
vim.cmd('qa!')
