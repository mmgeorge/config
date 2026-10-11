vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local order = require("forge.views.harness.model_order")
local function identifiers(model_list)
  return vim.tbl_map(function(model) return model.id end, model_list)
end
local source = vim.tbl_map(function(identifier) return { id = identifier } end, {
  "unknown-z", "gpt-5.9", "claude-haiku-5.5", "gemini-3.7-flash", "auto",
  "gpt-5.10", "claude-sonnet-5.5", "claude-opus-5.5", "gemini-3.8-flash", "unknown-a",
})
local original = vim.deepcopy(source)
local sorted = { "auto", "claude-opus-5.5", "claude-sonnet-5.5", "claude-haiku-5.5",
  "gpt-5.10", "gpt-5.9", "gemini-3.8-flash", "gemini-3.7-flash", "unknown-z", "unknown-a" }
assert(vim.deep_equal(identifiers(order.order(source, "copilot")), sorted))
assert(vim.deep_equal(identifiers(order.order(source, "codex")), identifiers(source)))
assert(vim.deep_equal(identifiers(order.order(source, "mock")), identifiers(source)))
local pinned = { ["gpt-5.9"] = true, ["gpt-5.10"] = true, ["unavailable"] = true }
assert(vim.deep_equal(identifiers(order.order(source, "copilot", pinned)), {
  "gpt-5.10", "gpt-5.9", "auto", "claude-opus-5.5", "claude-sonnet-5.5", "claude-haiku-5.5",
  "gemini-3.8-flash", "gemini-3.7-flash", "unknown-z", "unknown-a",
}))
assert(vim.deep_equal(identifiers(order.order(source, "codex", pinned)), {
  "gpt-5.9", "gpt-5.10", "unknown-z", "claude-haiku-5.5", "gemini-3.7-flash", "auto",
  "claude-sonnet-5.5", "claude-opus-5.5", "gemini-3.8-flash", "unknown-a",
}))
assert(vim.deep_equal(identifiers(order.order(source, "codex", {})), identifiers(original)))
assert(vim.deep_equal(source, original), "ordering changed the provider catalog")
print("harness_model_order: passed")
vim.cmd("qa!")
