-- Unit tests for `forge.text.harper` (harper.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["rule_actions"] = function()
  local path = temp.file("disabled-rules.json", { "{}" })
  local config = { enabled = true, cmd = { vim.v.progpath }, disabled_rules = path, settings = {} }
  package.loaded["forge.infra.config"] = { options = { harper = config, text_correction = { wordlist = false } } }
  package.loaded["forge.text.wordlist"] = { harper_dictionary = function(_, callback) callback("/fixture/dictionary.txt") end }
  local notifications = {}
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
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
  local buffer = buffer_support.scratch({ "The the sentence.", "Other sentence." }, { name = "ForgeTest://rules" })
  vim.api.nvim_set_current_buf(buffer)
  local harper = require("forge.text.harper")
  local owner = { kind = "prose", root = vim.fn.getcwd() }
  harper.attach(buffer, owner)
  wait_until(function() return captured ~= nil end, "Harper attach did not start a language client", { timeout_ms = 2000 })
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
  eq(#offered, 2, { fail_reason = vim.inspect(offered) })
  eq(offered[1].title, "Ignore Harper error.", { fail_reason = "the new action displaced existing suggestions" })
  eq(offered[2].title, 'Disable Harper rule "RepeatedWords" globally')
  eq(#actions({ "source" }), 1, { fail_reason = "source-only requests exposed a quick fix" })
  offered = actions({ "quickfix" })
  owner.guard = function() return false end
  eq(#actions(), 0, { fail_reason = "protected prose exposed rule actions" })
  local protected = offered[2].command
  captured.commands[protected.command](protected)
  assert(#errors == 1 and errors[1].message:find("outside editable prose", 1, true))
  eq(vim.fn.readfile(path)[1], "{}", { fail_reason = "newly protected prose persisted a rule" })
  owner.guard = nil
  offered = actions()
  local stale = offered[2].command
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "The new sentence." })
  captured.commands[stale.command](stale)
  assert(#errors == 2 and errors[2].message:find("outdated", 1, true))
  eq(vim.fn.readfile(path)[1], "{}", { fail_reason = "stale action persisted a rule" })
  eq(#actions(), 1, { fail_reason = "stale diagnostic metadata produced a rule action" })

  publish()
  config.disabled_rules = false
  eq(#actions(), 1, { fail_reason = "disabled persistence still exposed rule actions" })
  config.disabled_rules = path
  local action = actions()[2].command
  captured.commands[action.command](action)
  wait_until(function()
    return client.settings and client.settings["harper-ls"].linters.RepeatedWords == false
  end, "disabling a rule did not reach Harper's settings",
    { timeout_ms = 2000, context = function() return vim.inspect(errors:messages()) end })
  eq(vim.json.decode(table.concat(vim.fn.readfile(path), "\n")).RepeatedWords, false)
  assert(vim.tbl_contains(notifications, "workspace/didChangeConfiguration"))
  assert(vim.tbl_contains(notifications, "textDocument/didChange"))
  eq(#errors, 2, { fail_reason = vim.inspect(errors:messages()) })
  local previous_settings = vim.deepcopy(client.settings)
  local previous_notifications = #notifications
  vim.fn.writefile({ "malformed settings" }, path)
  harper.refresh_settings()
  wait_until(function() return #errors == 3 end, "malformed rule settings did not report an error", { timeout_ms = 2000 })
  eq(client.settings, previous_settings, { fail_reason = "malformed reload replaced usable settings" })
  assert(#notifications == previous_notifications, "malformed reload was published to Harper")
end

T["dictionary_actions"] = function()
  local wordlist_path = temp.file("wordlist.txt", { "# Version-controlled vocabulary", "repo" })
  local cache = temp.directory("cache")
  local stdpath = vim.fn.stdpath
  vim.fn.stdpath = function(kind) return kind == "cache" and cache or stdpath(kind) end
  local text_correction = { wordlist = wordlist_path, terminology = {}, project_terms = {} }
  package.loaded["forge.infra.config"] = { options = { text_correction = text_correction,
    harper = { enabled = true, cmd = { vim.v.progpath }, disabled_rules = false, settings = { linters = { SpellCheck = true } } } } }
  package.loaded["forge.text"] = { invalidate = function() end }
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local notifications = {}
  local captured
  local client = { id = 1, is_stopped = function() return false end,
    notify = function(_, method, params) notifications[#notifications + 1] = { method = method, params = params } return true end }
  vim.lsp.start = function(settings) captured = settings client.settings = settings.settings return client.id end
  vim.lsp.get_client_by_id = function() return client end
  local buffer = buffer_support.scratch({ "Use recieve spelling." }, { name = "ForgeTest://dictionary" })
  vim.api.nvim_set_current_buf(buffer)
  local uri = vim.uri_from_bufnr(buffer)
  local word_range = { start = { line = 0, character = 4 }, ["end"] = { line = 0, character = 11 } }
  -- Harper offers its user, workspace, and file dictionaries in both command shapes beside an edit.
  vim.lsp.rpc.start = function()
    return { notify = function() return true end, request = function(_, _, callback)
      callback(nil, {
        { title = 'Replace with "receive"', edit = { changes = { [uri] = { { range = word_range, newText = "receive" } } } } },
        { title = "Add to user dictionary", command = { command = "HarperAddToUserDict", arguments = { "recieve" } } },
        { title = "Add to workspace dictionary", command = { command = "HarperAddToWSDict", arguments = { "recieve" } } },
        { title = "Add to file dictionary", command = "HarperAddToFileDict", arguments = { "recieve" } },
      })
      return true
    end }
  end
  local harper = require("forge.text.harper")
  harper.attach(buffer, { kind = "prose", root = vim.fn.getcwd() })
  wait_until(function() return captured ~= nil end, "Harper attach did not start a language client", { timeout_ms = 2000 })
  local initial_dictionary = captured.settings["harper-ls"].userDictPath
  assert(vim.fs.normalize(initial_dictionary) ~= vim.fs.normalize(wordlist_path), "Harper received the commented source wordlist")
  assert(vim.tbl_contains(vim.fn.readfile(initial_dictionary), "repo"), "published dictionary omitted the configured wordlist")
  local connection = captured.cmd({})
  local function titles()
    local offered
    connection.request("textDocument/codeAction", { textDocument = { uri = uri }, range = word_range,
      context = { diagnostics = {} } }, function(failure, value) assert(not failure, failure) offered = value end)
    return offered, vim.tbl_map(function(action) return action.title end, offered)
  end

  local offered, offered_titles = titles()
  eq(offered_titles, { 'Replace with "receive"', 'Add "recieve" to the global dictionary' })
  text_correction.wordlist = false
  eq(select(2, titles()), { 'Replace with "receive"' }, { fail_reason = "a disabled wordlist still offered dictionary actions" })
  text_correction.wordlist = wordlist_path
  offered = titles()
  captured.commands[offered[2].command.command](offered[2].command)
  wait_until(function() return client.settings["harper-ls"].userDictPath ~= initial_dictionary end,
    "adding a word did not refresh Harper's dictionary", { timeout_ms = 2000, context = function() return vim.inspect(errors:messages()) end })
  local saved = vim.fn.readfile(wordlist_path)
  eq(saved[1], "# Version-controlled vocabulary", { fail_reason = "adding a word rewrote the wordlist's comments" })
  assert(vim.tbl_contains(saved, "recieve"), vim.inspect(saved))
  assert(vim.tbl_contains(vim.fn.readfile(client.settings["harper-ls"].userDictPath), "recieve"),
    "the refreshed Harper dictionary omitted the accepted word")
  eq(vim.tbl_map(function(notification) return notification.method end, notifications),
    { "workspace/didChangeConfiguration", "textDocument/didChange" })
  eq(notifications[2].params.contentChanges[1].text, "Use recieve spelling.",
    { fail_reason = "Harper did not recheck the unsaved editor text" })
  eq(#errors, 0, { fail_reason = vim.inspect(errors:messages()) })
end

return T
