vim.opt.runtimepath:prepend('nvim')
vim.loader.enable(false)
local command = require('forge.render.harness.command')
local tool = require('forge.render.harness.tool')
local fixtures = vim.json.decode(table.concat(vim.fn.readfile('nvim/tests/forge/fixtures/command_display.json'), '\n'))
for _, fixture in ipairs(fixtures) do
  assert(command.display(fixture.source, fixture.shell) == fixture.expected, fixture.source)
  local expected = fixture.expected:gsub('%s+', ' ')
  local call = { title = fixture.source, shell = fixture.shell, kind = 'command', started_at_ms = 0, completed_at_ms = 1000 }
  assert(tool.heading(call):find(expected, 1, true), fixture.source)
  assert(tool.heading_lines(call, 1000)[1].text:find(expected, 1, true), fixture.source)
end
print('harness_command_display: passed ' .. #fixtures .. ' cross-platform fixtures')
vim.cmd('qa!')
