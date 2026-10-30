vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local rules = require("forge.text.harper_rules")
local path = vim.fn.tempname()

local function load()
  local result
  rules.load(path, function(value, failure) result = { value = value, failure = failure } end)
  assert(vim.wait(2000, function() return result ~= nil end, 5), "rule read did not finish")
  return result
end

local function disable(rule)
  local result
  rules.disable(path, rule, function(failure) result = { failure = failure } end)
  assert(vim.wait(2000, function() return result ~= nil end, 5), "rule write did not finish")
  return result.failure
end

assert(vim.tbl_isempty(assert(load().value)), "missing rule files must start empty")
vim.fn.writefile({ "{}" }, path)
assert(vim.tbl_isempty(assert(load().value)), "empty JSON object is not a rule list")
local completed = 0
for _, rule in ipairs({ "RepeatedWords", "LongSentences", "RepeatedWords" }) do
  rules.disable(path, rule, function(failure)
    assert(not failure, failure)
    completed = completed + 1
  end)
end
assert(vim.wait(2000, function() return completed == 3 end, 5))
assert(vim.deep_equal(load().value, { RepeatedWords = false, LongSentences = false }))
package.loaded["forge.text.harper_rules"] = nil
rules = require("forge.text.harper_rules")
assert(load().value.RepeatedWords == false, "rules did not survive reloading their owner")

local saved = vim.fn.readfile(path)
local buffer = vim.api.nvim_create_buf(true, false)
vim.api.nvim_buf_set_name(buffer, path)
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "unsaved rules" })
vim.bo[buffer].modified = true
assert(disable("SpellCheck"):find("Save the modified", 1, true))
assert(vim.deep_equal(vim.fn.readfile(path), saved))
vim.api.nvim_buf_delete(buffer, { force = true })
vim.fn.writefile({ "" }, path .. ".lock")
assert(disable("SpellCheck"):find("Cannot lock", 1, true))
vim.fn.delete(path .. ".lock")
assert(disable("not a rule"))

local rename = vim.uv.fs_rename
vim.uv.fs_rename = function(_, _, callback) callback("injected rename failure") end
assert(disable("SpellCheck"):find("injected rename failure", 1, true))
vim.uv.fs_rename = rename
assert(vim.deep_equal(vim.fn.readfile(path), saved), "failed publication destroyed saved rules")
assert(vim.fn.filereadable(path .. ".lock") == 0)
assert(#vim.fn.glob(path .. ".tmp.*", false, true) == 0)

for _, content in ipairs({ "not json", "[]", '{"SpellCheck":true}', '{"not a rule":false}' }) do
  vim.fn.writefile({ content }, path)
  assert(load().failure, "invalid configuration was accepted: " .. content)
  assert(disable("RepeatedWords"), "invalid configuration was overwritten")
  assert(vim.fn.readfile(path)[1] == content)
end
local large = {}
for index = 1, 5000 do large["Rule" .. index] = false end
vim.fn.writefile({ vim.json.encode(large) }, path)
assert(vim.deep_equal(load().value, large), "chunked reading lost rule entries")
vim.fn.delete(path)
print("Harper rules: persistent, serialized, atomic, conflict-safe settings passed")
vim.cmd("qa!")
