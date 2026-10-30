vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local path = vim.fn.tempname()
vim.fn.writefile({ "{}" }, path)
local config = { enabled = true, cmd = { vim.v.progpath }, disabled_rules = path, settings = {} }
package.loaded["forge.infra.config"] = { options = { harper = config, text_correction = { wordlist = false } } }
package.loaded["forge.text.wordlist"] = { harper_dictionary = function(_, callback) callback("/fixture/dictionary.txt") end }
local errors, notifications = {}, {}
package.loaded["forge.infra.notifications"] = { error = function(message) errors[#errors + 1] = message end }
local captured
local client = { id = 1, is_stopped = function() return false end,
  notify = function(_, method) notifications[#notifications + 1] = method return true end }
vim.lsp.start = function(settings) captured = settings return client.id end
vim.lsp.get_client_by_id = function() return client end
vim.lsp.diagnostic.on_publish_diagnostics = function() end
vim.lsp.rpc.start = function()
  return { notify = function() return true end, request = function(_, _, callback)
    callback(nil, { { title = "Ignore Harper error.", command = "HarperIgnoreLint", arguments = {} } })
    return true
  end }
end
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
vim.api.nvim_buf_set_name(buffer, "ForgeTest://rules")
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "The the sentence.", "Other sentence." })
local harper = require("forge.text.harper")
local owner = { kind = "prose", root = vim.fn.getcwd() }
harper.attach(buffer, owner)
assert(vim.wait(2000, function() return captured ~= nil end, 5))
local connection = captured.cmd({})
local range = { start = { line = 0, character = 0 }, ["end"] = { line = 0, character = 7 } }
local function publish()
  local diagnostics = {}
  for _, value in ipairs({
    { source = "Harper", code = "RepeatedWords" },
    { source = "Harper", code = "RepeatedWords" },
    { source = "Other", code = "SpellCheck" },
    { source = "Harper", code = 123 },
    { source = "Harper", code = "../bad" },
    { source = "Harper", message = "Disable LongSentences" },
  }) do
    value.range = range
    diagnostics[#diagnostics + 1] = value
  end
  diagnostics[#diagnostics + 1] = { source = "Harper", code = "LongSentences",
    range = { start = { line = 1, character = 0 }, ["end"] = { line = 1, character = 5 } } }
  captured.handlers["textDocument/publishDiagnostics"](nil,
    { uri = vim.uri_from_bufnr(buffer), diagnostics = diagnostics }, { client_id = 1 })
end
local function actions(only)
  local result
  connection.request("textDocument/codeAction", {
    textDocument = { uri = vim.uri_from_bufnr(buffer) },
    range = { start = { line = 0, character = 4 }, ["end"] = { line = 0, character = 4 } },
    context = { diagnostics = {}, only = only },
  }, function(failure, value) assert(not failure, failure) result = value end)
  return result
end
publish()
local offered = actions()
assert(#offered == 2, vim.inspect(offered))
assert(offered[1].title == "Ignore Harper error.", "the new action displaced existing suggestions")
assert(offered[2].title == 'Disable Harper rule "RepeatedWords" globally')
assert(#actions({ "source" }) == 1, "source-only requests exposed a quick fix")
offered = actions({ "quickfix" })
owner.guard = function() return false end
assert(#actions() == 0, "protected prose exposed rule actions")
local protected = offered[2].command
captured.commands[protected.command](protected)
assert(#errors == 1 and errors[1]:find("outside editable prose", 1, true))
assert(vim.fn.readfile(path)[1] == "{}", "newly protected prose persisted a rule")
owner.guard = nil
offered = actions()
local stale = offered[2].command
vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "The new sentence." })
captured.commands[stale.command](stale)
assert(#errors == 2 and errors[2]:find("outdated", 1, true))
assert(vim.fn.readfile(path)[1] == "{}", "stale action persisted a rule")
assert(#actions() == 1, "stale diagnostic metadata produced a rule action")

publish()
config.disabled_rules = false
assert(#actions() == 1, "disabled persistence still exposed rule actions")
config.disabled_rules = path
local action = actions()[2].command
captured.commands[action.command](action)
assert(vim.wait(2000, function()
  return client.settings and client.settings["harper-ls"].linters.RepeatedWords == false
end, 5), vim.inspect(errors))
assert(vim.json.decode(table.concat(vim.fn.readfile(path), "\n")).RepeatedWords == false)
assert(vim.tbl_contains(notifications, "workspace/didChangeConfiguration"))
assert(vim.tbl_contains(notifications, "textDocument/didChange"))
assert(#errors == 2, vim.inspect(errors))
local previous_settings = vim.deepcopy(client.settings)
local previous_notifications = #notifications
vim.fn.writefile({ "malformed settings" }, path)
harper.refresh_settings()
assert(vim.wait(2000, function() return #errors == 3 end, 5))
assert(vim.deep_equal(client.settings, previous_settings), "malformed reload replaced usable settings")
assert(#notifications == previous_notifications, "malformed reload was published to Harper")
vim.fn.delete(path)
print("Harper rule actions: exact metadata, deduplication, cursor ranges, stale actions, and refresh passed")
vim.cmd("qa!")
