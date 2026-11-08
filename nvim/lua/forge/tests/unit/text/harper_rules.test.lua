-- Unit tests for `forge.text.harper_rules` (harper_rules.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harper_rules"] = function()
  local rules = require("forge.text.harper_rules")
  local path = temp.directory("harper-rules") .. "/rules.json"

  local function load()
    local result
    rules.load(path, function(value, failure) result = { value = value, failure = failure } end)
    wait_until(function() return result ~= nil end, "rule read did not finish", { timeout_ms = 2000 })
    return result
  end

  local function disable(rule)
    local result
    rules.disable(path, rule, function(failure) result = { failure = failure } end)
    wait_until(function() return result ~= nil end, "rule write did not finish", { timeout_ms = 2000 })
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
  wait_until(function() return completed == 3 end, "concurrent rule writes did not all finish", { timeout_ms = 2000 })
  eq(load().value, { RepeatedWords = false, LongSentences = false })
  package.loaded["forge.text.harper_rules"] = nil
  rules = require("forge.text.harper_rules")
  eq(load().value.RepeatedWords, false, { fail_reason = "rules did not survive reloading their owner" })

  local saved = vim.fn.readfile(path)
  local buffer = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buffer, path)
  buffer_support.set(buffer, { "unsaved rules" })
  vim.bo[buffer].modified = true
  assert(disable("SpellCheck"):find("Save the modified", 1, true))
  eq(vim.fn.readfile(path), saved)
  vim.api.nvim_buf_delete(buffer, { force = true })
  vim.fn.writefile({ "" }, path .. ".lock")
  assert(disable("SpellCheck"):find("Cannot lock", 1, true))
  vim.fn.delete(path .. ".lock")
  assert(disable("not a rule"))

  local rename = vim.uv.fs_rename
  vim.uv.fs_rename = function(_, _, callback) callback("injected rename failure") end
  assert(disable("SpellCheck"):find("injected rename failure", 1, true))
  vim.uv.fs_rename = rename
  eq(vim.fn.readfile(path), saved, { fail_reason = "failed publication destroyed saved rules" })
  eq(vim.fn.filereadable(path .. ".lock"), 0)
  eq(#vim.fn.glob(path .. ".tmp.*", false, true), 0)

  for _, content in ipairs({ "not json", "[]", '{"SpellCheck":true}', '{"not a rule":false}' }) do
    vim.fn.writefile({ content }, path)
    assert(load().failure, "invalid configuration was accepted: " .. content)
    assert(disable("RepeatedWords"), "invalid configuration was overwritten")
    assert(vim.fn.readfile(path)[1] == content)
  end
  local large = {}
  for index = 1, 5000 do large["Rule" .. index] = false end
  vim.fn.writefile({ vim.json.encode(large) }, path)
  eq(load().value, large, { fail_reason = "chunked reading lost rule entries" })
end

return T
