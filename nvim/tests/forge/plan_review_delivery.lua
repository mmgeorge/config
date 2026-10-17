vim.loader.enable(false)
local delivery = require("forge.views.plan_review.delivery")
local metadata = { target = {}, editable_region = {}, decoration = {} }
local state = {}
local snapshot = { document = "review", revision = 0, block = {} }
local anchors = {}
for index = 1, 1000 do
  local id = "declaration:" .. index
  snapshot.block[index] = { id = id, text = { "retained declaration " .. index }, metadata = metadata }
  anchors[index] = { id = id .. ":0", source_line = index }
end
local _, source, fragment = delivery.apply(state, { snapshot = snapshot, source_edit = { upsert = anchors, remove = {} } })
assert(#source == 1000)
local retained = fragment.block["declaration:500"]
local patch = { document = "review", base = 0, next = 1, base_rows = 1000, next_rows = 1000, base_blocks = 1000, next_blocks = 1000,
  text_edit = { { start_row = 998, removed_rows = 1, text = { "last change" } }, { start_row = 1, removed_rows = 1, text = { "first change" } } },
  metadata_edit = {}, removed_block = {}, block_edit = {} }
local updated, rows = delivery.apply(state, { patch = patch, source_edit = { upsert = {}, remove = {} } })
assert(updated.revision == 1 and rows[2].text == "first change" and rows[999].text == "last change")
assert(state.fragment.block["declaration:500"] == retained, "retained declaration was reconstructed")
assert(#rows == 1000 and rows[500].source_line == 500)
local accepted = pcall(delivery.apply, state, { patch = patch, source_edit = { upsert = {}, remove = {} } })
assert(not accepted and state.fragment.revision == 1, "stale patch changed retained source")
local no_change, retained_rows = delivery.apply(state, { patch = vim.NIL, source_edit = { upsert = { { id = "declaration:500:0", source_line = 0 } }, remove = {} } })
assert(no_change.revision == 1 and retained_rows[500].source_line == 0)
assert(not pcall(delivery.apply, state, { snapshot = snapshot, source_edit = { upsert = {}, remove = {} } }), "update accepted a replacement snapshot")
print("plan_review_delivery: distant patches, retained blocks, sparse anchors, no-op updates, and stale revisions passed")
