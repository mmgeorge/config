vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local order = require("forge.views.harness.model_order")
local function identifiers(models)
  return vim.tbl_map(function(model) return model.id end, models)
end
local source = { { id = "model-z" }, { id = "model-a" }, { id = "model-m" }, { id = "auto" } }
local original = vim.deepcopy(source)
assert(vim.deep_equal(identifiers(order.order(source)), identifiers(source)))
assert(vim.deep_equal(identifiers(order.order(source, { ["model-a"] = true, ["auto"] = true, missing = true })),
  { "model-a", "auto", "model-z", "model-m" }))
assert(vim.deep_equal(identifiers(order.order(source, {})), identifiers(source)))
assert(vim.deep_equal(source, original), "ordering changed the normalized provider catalog")
print("harness_model_order: passed")
vim.cmd("qa!")
