-- Saved Background controls survive fresh hosts and preserve chat settings. The case runs each
-- phase in a fresh child Neovim: `save` changes the controls, and `reopen` reads them back.
if vim.g.forge_commit_settings_phase then
  vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
  vim.loader.enable(false)
  local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
  local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
  local host = dofile("nvim/lua/forge/tests/support/host.lua")
  local stage = vim.g.forge_commit_settings_phase
  local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
  local data = assert(vim.g.forge_commit_settings_data)
  local workspace = vim.fs.joinpath(data, stage)
  vim.fn.mkdir(workspace, "p")
  local stdpath = vim.fn.stdpath
  vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
  package.loaded["forge.builder"] = { ensure = function(callback)
    callback({ ok = true, path = executable })
    return function() end
  end }
  require("forge").setup({ harness = { provider = "mock" },
    background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
  vim.fn.chdir(workspace)
  local client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
  local state = require("forge.session").harness
  local picker = require("forge.views.picker")
  local settings = require("forge.views.harness.settings")
  local errors = {}
  require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
  local function option(id)
    local instance = picker._state_for_test()
    if not instance then return end
    for _, row in ipairs(instance.spec.page_list[1].option_list) do
      for _, field in ipairs(row.fields or {}) do if field.id == id then return field end end
    end
  end
  -- The settings picker renders only the rows its window fits; the default 24-line
  -- headless screen hides the Background section below Main and Ask.
  vim.o.lines = 60
  require("forge").open_harness()
  wait_until(function() return state.ready and state.session end, "Harness did not start", { timeout_ms = 4000, errors = errors })
  local before = { model = state.session.model, effort = state.session.effort, service_tier = state.session.service_tier }
  settings.open(state, { window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win })
  wait_until(function() local model = option("background-model") return model and not model.disabled_reason end, "Background controls did not load", { timeout_ms = 4000, errors = errors })
  local expected = stage == "save" and "low" or "medium"
  assert(option("background-model").value:find("mock-model", 1, true))
  assert(option("background-thinking").value:find(expected, 1, true), vim.inspect(option("background-thinking")))
  assert(option("background-speed").value == (stage == "save" and "default" or "fast"))
  assert(buffer.text(picker._state_for_test().buf):find("Background", 1, true), "Background controls did not render")
  if stage == "save" then
    picker._state_for_test().spec.on_field_action({ field = option("background-thinking") }, "change", 1)
    wait_until(function() return option("background-thinking").value:find("medium", 1, true) ~= nil end, "Thinking did not save", { timeout_ms = 4000, errors = errors })
    picker._state_for_test().spec.on_field_action({ field = option("background-speed") }, "change", 1)
    wait_until(function() return option("background-speed").value == "fast" end, "Speed did not save", { timeout_ms = 4000, errors = errors })
  end
  assert(state.session.model == before.model and state.session.effort == before.effort, "Commit settings changed the chat")
  assert(state.session.service_tier == before.service_tier, "Background speed changed the chat")
  local generator = require("forge.integrations.ai_commit")
  local original_generation_request = client.generation_request
  client.generation_request = function(_, callback, _, selection)
    local generation = assert(client.generation_get_config(selection))
    assert(generation.service_tier == "fast", "saved Background speed did not reach generation")
    callback({ state = "ready", message = "test: use saved preferences" })
  end
  local generated
  generator.ensure(workspace, { ref = "HEAD", force = true }, function(result) generated = result end)
  wait_until(function() return generated ~= nil end, "Generation did not read saved settings", { timeout_ms = 4000, errors = errors })
  assert(generated.state == "ready" and generated.model == "mock-model · thinking medium · fast", vim.inspect(generated))
  client.generation_request = original_generation_request
  picker.close(false)
  assert(host.stop(), "Host did not stop")
  return
end

local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---@param value string
---@return string
local function vim_string(value) return "'" .. value:gsub("'", "''") .. "'" end

T["commit_settings"] = function()
  local script = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
  local data = temp.directory("data")
  for _, phase in ipairs({ "save", "reopen" }) do
    local result = vim.system({ vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE",
      "--cmd", "let g:forge_commit_settings_phase = " .. vim_string(phase),
      "--cmd", "let g:forge_commit_settings_data = " .. vim_string(data),
      "-l", script }, { text = true, timeout = 25000 }):wait()
    assert(result.code == 0, phase .. ": " .. (result.stderr or "") .. (result.stdout or ""))
  end
end

return T
