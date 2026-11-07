vim.loader.enable(false)
local threshold = require("forge.views.harness.compaction_threshold")

-- A 258.4k context window cannot use the 512k preset: Harness rejects any threshold at or above
-- the reported capacity, so cycling below the smallest preset must not wrap to an invalid value.
local capacity = 258400
assert(threshold.change(64000, -1, capacity) == 256000,
  "cycling below 64k offered a threshold above the model context capacity")
assert(threshold.change(256000, 1, capacity) == 64000,
  "cycling above 256k offered a threshold above the model context capacity")
assert(vim.deep_equal(threshold.options(capacity), { 64000, 128000, 256000 }))

-- Without a reported capacity, every preset remains selectable because Harness accepts any
-- positive threshold until the provider reports the context window.
assert(threshold.change(64000, -1, nil) == 512000)
assert(vim.deep_equal(threshold.options(nil), { 64000, 128000, 256000, 512000 }))

-- A capacity below every preset leaves no valid choice, and the value stays unchanged.
assert(vim.deep_equal(threshold.options(32000), {}))
assert(threshold.change(64000, 1, 32000) == 64000)

print("harness_compaction_threshold: ok")
