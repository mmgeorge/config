vim.loader.enable(false)
local replica = require('forge.buffer')
local owner = replica.open('gutter-shift')
local buffer = owner.buffer
local metadata = { target = {}, decoration = {}, editable_region = {} }
local response_metadata = vim.deepcopy(metadata)
response_metadata.gutter = { { position = { row = 0, column = 0 },
  chunk = { { text = '▸ ', capture = 'Normal' } }, priority = 100 } }
local snapshot = { document = 'gutter-shift', revision = 0, block = {
  { id = 'tool', text = { 'preview' }, metadata = metadata },
  { id = 'response', text = { 'answer' }, metadata = response_metadata },
} }
assert(replica.apply_snapshot(owner, snapshot).kind == 'Applied')
local result = replica.apply_patch(owner, { document = 'gutter-shift', base = 0, next = 1,
  base_rows = 2, next_rows = 4, base_blocks = 2, next_blocks = 2,
  block_edit = {}, removed_block = {},
  text_edit = { { start_row = 0, removed_rows = 1, text = { 'preview', 'more', 'last' } } },
  metadata_edit = { { block = 'tool', row_count = 3, metadata = metadata } },
})
assert(result.kind == 'Applied', vim.inspect(result))
local position = vim.api.nvim_buf_get_extmark_by_id(buffer, owner.namespace, owner.marks.response[1], {})
assert(position[1] == 3, 'response gutter moved into the expanded tool: ' .. vim.inspect(position))
replica.close(owner)
print('buffer_gutter_shift: passed')
