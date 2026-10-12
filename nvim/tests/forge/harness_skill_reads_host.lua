vim.loader.enable(false)
local repository = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h:h:h")
vim.opt.runtimepath:prepend(repository .. "/nvim")
local backend = vim.g.forge_skill_test_backend or "codex"
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data .. "/forge", "p")
local bundle = workspace .. "/.rulesync/skills/fixture"
vim.fn.mkdir(bundle .. "/scripts", "p")
vim.fn.writefile({ "---", "name: fixture", "targets: ['*']", "description: Skill-read verification fixture.",
  "---", "This fixture contains no further task instructions." }, bundle .. "/SKILL.md")
vim.fn.writefile({ "print('SKILL_SCRIPT_VERIFIED')" }, bundle .. "/scripts/check.py")
local policy = vim.json.encode({ permission = { read = { ["*"] = "ask" }, bash = { ["*"] = "ask" } } })
vim.fn.writefile({ policy }, data .. "/forge/permissions.json")
local original_stdpath = vim.fn.stdpath
local binary = require("forge.builder").binary_path()
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(binary, executable))
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors, approvals = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
local function request(method, parameter)
  local settled, result, failure
  client.request(method, parameter or {}, function(value, error)
    result, failure, settled = value, error, true
  end)
  assert(vim.wait(method == "prompt.submit" and 90000 or 15000, function()
    for _, approval in ipairs(state.approval or {}) do approvals[approval.id] = true end
    return settled
  end, 10), method .. " timed out: " .. vim.inspect(errors))
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { config_dir = data, backend = backend,
    backends = vim.g.forge_skill_test_launch } })
  require("forge.views.harness").open()
  assert(vim.wait(15000, function() return state.session or #errors > 0 end, 10), "host startup timed out")
  assert(state.session, table.concat(errors, "\n"))
  request("session.configure", { access = { sandbox = false, write_access = "workspace",
    writable_directory = {}, windows_sandbox = "elevated" } })
  request("trace.configure", { enabled = true })
  local source = bundle .. "/SKILL.md"
  local read = vim.fn.has("win32") == 1 and ("Get-Content -LiteralPath '" .. source .. "'")
    or ("cat '" .. source .. "'")
  local script = "python '" .. bundle .. "/scripts/check.py'"
  request("prompt.submit", { text = "For this verification, use shell tools to run exactly these commands in order: "
    .. read .. " then " .. read .. " then " .. script .. ". Read the skill file twice using tools. "
    .. "Do not invoke the skill as a slash command. Do not inspect other paths, run other commands, write files, "
    .. "or call MCP tools. Reply exactly SKILL_READ_VERIFIED after the script prints SKILL_SCRIPT_VERIFIED." })
  assert(next(approvals) == nil, "selected skill access opened approval: " .. vim.inspect(approvals))
  assert(#errors == 0, table.concat(errors, "\n"))
  local snapshot = request("state.get")
  local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  assert(snapshot.status.kind == "idle", "provider execution did not settle")
  local function script_output(value)
    if type(value) ~= "table" then return false end
    for _, key in ipairs({ "aggregatedOutput", "combinedOutput" }) do
      if type(value[key]) == "string" and value[key]:find("SKILL_SCRIPT_VERIFIED", 1, true) then return true end
    end
    for _, child in pairs(value) do if script_output(child) then return true end end
    return false
  end
  local script_verified = false
  for _, line in ipairs(vim.fn.readfile(request("trace.status").path)) do
    local record = vim.json.decode(line)
    if script_output(record.payload) then script_verified = true break end
  end
  assert(script_verified, "provider command output did not confirm the bundled script\n" .. text)
  assert(text:find("Read skill · fixture", 1, true), "confirmed skill read has no timeline event")
  local diamonds = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.transcript_buf,
    state.presentation.transcript.namespace, 0, -1, { details = true })) do
    local decoration = mark[4]
    local line = vim.api.nvim_buf_get_lines(state.transcript_buf, mark[2], mark[2] + 1, false)[1] or ""
    if decoration.sign_text and vim.trim(decoration.sign_text) == "◇" and line:find("Read skill · fixture", 1, true) then
      diamonds = diamonds + 1
    end
  end
  assert(diamonds == 1, "expected one skill diamond, observed " .. diamonds)
  assert(table.concat(vim.fn.readfile(data .. "/forge/permissions.json"), "\n") == policy,
    "runtime grants changed the permission document")
  print("HARNESS_SKILL_READ_PASS " .. backend .. " diamonds=" .. diamonds .. " approvals=0")
end, debug.traceback)
local stopped = false
client.stop(nil, function() stopped = true end)
vim.wait(10000, function() return stopped end, 10)
vim.fn.stdpath = original_stdpath
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
vim.cmd("qa!")
