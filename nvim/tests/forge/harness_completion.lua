vim.loader.enable(false)

local blink_root = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "blink.cmp")
assert(vim.fn.isdirectory(blink_root) == 1, "harness_completion requires the installed Blink plugin")
vim.opt.runtimepath:append(blink_root)

local source_config = require("plugins.blink")[1].opts.sources
require("blink.cmp.config").merge_with({ sources = source_config })
local sources = require("blink.cmp.sources.lib")
local session = require("forge.session")
local client = require("forge.client")
local original_request, original_system = client.request, vim.system

---@param source table
---@param text string
---@return table[]
local function complete(source, text)
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { text })
  vim.api.nvim_win_set_cursor(0, { 1, #text })
  assert(source:enabled(), "completion did not activate for " .. text)
  local result
  source:get_completions({}, function(value) result = value end)
  assert(vim.wait(1000, function() return result ~= nil end), "completion timed out for " .. text)
  return result.items
end

---@param items table[]
---@param label string
---@return table
local function item(items, label)
  for _, entry in ipairs(items) do
    if entry.label == label then return entry end
  end
  error("missing completion " .. label)
end

local ok, failure = xpcall(function()
  local _, _, composer = require("forge.views.harness.layout").open("completion-test")
  assert(vim.api.nvim_get_current_buf() == composer)
  vim.o.virtualedit = "onemore"
  assert(vim.deep_equal(sources.get_enabled_provider_ids("default"), {
    "harness_commands", "harness_files",
  }), "the real composer did not select its Harness completion providers")

  session.harness.session = { id = "completion-test", backend = "codex", workspace = "fixture-workspace" }
  session.harness.capability = {
    model_selection = true, effort_selection = true,
    catalog = { skill = true, mcp = true },
  }
  session.harness.model_backend = "codex"
  session.harness.model_list = { { id = "fixture-model", reasoning = { "low", "high" } } }
  client.request = function(method, _, callback)
    assert(method == "backend.skills", "unexpected completion request " .. method)
    vim.schedule(function()
      callback({ { name = "fixture-skill", enabled = true, user_invocable = true } })
    end)
  end
  vim.system = function(command, options, callback)
    assert(command[1] == "git" and command[2] == "ls-files")
    assert(options.cwd == "fixture-workspace")
    vim.schedule(function() callback({ code = 0, stdout = "src/main.rs\n", stderr = "" }) end)
  end

  local commands = require(source_config.providers.harness_commands.module).new()
  local files = require(source_config.providers.harness_files.module).new()
  item(complete(commands, "/"), "/plan")
  assert(vim.tbl_contains(sources.get_trigger_characters("default"), "/"))
  item(complete(commands, "/"), "/model")
  item(complete(commands, "/b"), "/bg")
  item(complete(commands, "/rec"), "/recap")
  item(complete(commands, "/mode "), "write")
  session.harness.agent = { run = { { id = "child", definition = "explorer", state = "ready" } }, exchange = {} }
  session.harness.timeline = { { kind = "agent_lifecycle", run = session.harness.agent.run[1],
    exchange = { { id = "child-exchange", agent_id = "child", state = "running", turn = {} } }, agent = {} } }
  item(complete(commands, "/agent "), "a")
  session.harness.timeline[1].exchange[1].state = "complete"
  assert(#complete(commands, "/agent ") == 1, "completed child retained its running alias")
  item(complete(commands, "/model "), "fixture-model")
  local effort = complete(commands, "/model fixture-model ")
  assert(#effort == 2)
  item(effort, "high")
  local skill = item(complete(commands, "$fixture"), "$fixture-skill")
  assert(skill.textEdit.range.start.character == 1)
  assert(skill.textEdit.newText == "fixture-skill ")
  local file = item(complete(files, "inspect @src"), "@src/main.rs")
  vim.lsp.util.apply_text_edits({ file.textEdit }, composer, "utf-8")
  assert(vim.api.nvim_get_current_line() == "inspect @src/main.rs", "file completion changed the prompt prefix")

  vim.bo.filetype = "lua"
  assert(not vim.tbl_contains(sources.get_enabled_provider_ids("default"), "harness_commands"),
    "Harness commands leaked into ordinary source buffers")
end, debug.traceback)

client.request, vim.system = original_request, original_system
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_completion OK")
vim.cmd("qa!")
