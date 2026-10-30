vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
local context = require('forge.text.context')
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
local index = context.index(buffer)
local owner = { kind = 'prompt' }

local function assert_synchronized()
  local actual = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  assert(index.shadow:rows() == #actual, vim.inspect({ actual = actual, shadow = index.shadow:text() }))
  assert(vim.deep_equal(index.shadow:text(), actual))
end

local function capture(row, column)
  local result, failure, done
  context.capture(index, { row = row, column = column, after = { row = row, column = column } }, owner,
    function() return true end, function(value, message) result, failure, done = value, message, true end)
  assert(vim.wait(1000, function() return done end, 5))
  assert(failure == nil, failure)
  return result
end

for _, replacement in ipairs({ {}, { '' } }) do
  local lines = {}
  for row = 1, 150 do lines[row] = 'Please preserve this line.' end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  assert_synchronized()
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, replacement)
  assert_synchronized()
  assert(index.shadow:row(0) == '')
  assert(capture(0, 0) == nil)
  assert(capture(149, 4) == nil, 'deleted row produced context')
  assert(capture(-1, 0) == nil, 'negative row produced context')
  assert(not context.allowed(index, owner, { row = 149, column = 0 }, { row = 149, column = 4 }))
  assert(context.allowed(index, owner, { row = 0, column = 0 }, { row = 0, column = 0 }))
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { 'Please recieve ' })
  assert_synchronized()
  assert(capture(0, 14).original == 'recieve', 'capture did not recover after clearing')
end

vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { 'first', 'second', 'third' })
vim.api.nvim_buf_set_lines(buffer, 1, 2, false, {})
assert_synchronized()
assert(vim.deep_equal(index.shadow:text(), { 'first', 'third' }), 'partial deletion duplicated a surviving line')
vim.api.nvim_buf_set_lines(buffer, 0, 0, false, {})
assert_synchronized()
vim.api.nvim_buf_set_lines(buffer, 0, 1, false, {})
assert_synchronized()
assert(vim.deep_equal(index.shadow:text(), { 'third' }), 'prefix deletion duplicated the next line')
vim.cmd('normal! ggVGd')
assert_synchronized()
assert(capture(0, 0) == nil)

local lines = {}
for row = 1, 96 do lines[row] = 'Continuous prose stays available.' end
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
local tick = vim.api.nvim_buf_get_changedtick(buffer)
local result, failure, done
context.capture(index, { row = 63, column = 16, after = { row = 63, column = 16 } }, owner,
  function() return vim.api.nvim_buf_get_changedtick(buffer) == tick end,
  function(value, message) result, failure, done = value, message, true end)
assert(not done, 'fixture did not reach asynchronous context capture')
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, {})
assert_synchronized()
assert(vim.wait(1000, function() return done end, 5))
assert(result == nil and failure == nil, failure or 'stale capture survived deletion')
print('text empty buffer: complete deletion, partial deletion, stale rows, and capture cancellation passed')
vim.cmd('qa!')
