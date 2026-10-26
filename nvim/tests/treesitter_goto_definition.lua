vim.loader.enable(false)

local first_usage = {}
local middle_usage = {}
local last_usage = {}
local definition = {}
local scope = {}
local current_node = middle_usage
local usage_list = { first_usage, middle_usage, last_usage }
local destination
local buffer = vim.api.nvim_get_current_buf()

package.loaded["nvim-treesitter.ts_utils"] = {
  get_node_at_cursor = function() return current_node end,
  goto_node = function(node) destination = node end,
}
package.loaded["nvim-treesitter.locals"] = {
  find_definition = function(node, requested_buffer)
    assert(node == current_node and requested_buffer == buffer, "navigation lost its source context")
    return definition, scope
  end,
  find_usages = function(requested_definition, requested_scope, requested_buffer)
    assert(requested_definition == definition and requested_scope == scope and requested_buffer == buffer,
      "usage lookup lost the resolved definition context")
    return usage_list
  end,
}
package.loaded["nvim-treesitter.configs"] = {}
local navigation = require("treesitter_goto_definition")

local success, failure = xpcall(function()
  navigation.goto_next_usage(buffer)
  assert(destination == last_usage, "next usage did not select the following node")
  destination = nil
  navigation.goto_previous_usage(buffer)
  assert(destination == first_usage, "previous usage did not select the preceding node")

  current_node = last_usage
  navigation.goto_next_usage(buffer)
  assert(destination == first_usage, "next usage did not wrap to the first node")
  current_node = first_usage
  navigation.goto_previous_usage(buffer)
  assert(destination == last_usage, "previous usage did not wrap to the last node")

  usage_list = { first_usage }
  navigation.goto_next_usage()
  assert(destination == first_usage, "single usage did not retain its node with the default buffer")

  destination = nil
  current_node = {}
  navigation.goto_next_usage(buffer)
  assert(destination == nil, "unrecognized cursor node navigated to an unrelated usage")
  current_node = first_usage
  usage_list = {}
  navigation.goto_previous_usage(buffer)
  assert(destination == nil, "empty usage list attempted navigation")
  current_node = nil
  navigation.goto_next_usage(buffer)
  assert(destination == nil, "missing cursor node attempted navigation")
end, debug.traceback)

if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1")
else print("treesitter_goto_definition: passed") vim.cmd("qa!") end
