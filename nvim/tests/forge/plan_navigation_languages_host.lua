vim.loader.enable(false)
local root = vim.fn.getcwd()
local language = assert(vim.g.forge_test_language, "set forge_test_language to rust, typescript, or lua")
local workspace = vim.g.forge_test_scratch and vim.g.forge_test_scratch .. "/workspace" or vim.fn.tempname()
local data = vim.g.forge_test_scratch and vim.g.forge_test_scratch .. "/data" or vim.fn.tempname()
local fixture = root .. "/nvim/tests/forge/fixtures/plan_navigation/" .. language
local function copy(source, destination)
  assert(vim.fn.mkdir(destination, "p") == 1)
  for name, kind in vim.fs.dir(source) do
    if kind == "directory" then copy(source .. "/" .. name, destination .. "/" .. name)
    else assert(vim.uv.fs_copyfile(source .. "/" .. name, destination .. "/" .. name)) end
  end
end
copy(fixture, workspace)
assert(vim.fn.mkdir(data, "p") == 1)
vim.fn.mkdir(workspace .. "/src", "p")
vim.fn.writefile({ "pub fn existing_change() {}" }, workspace .. "/src/change.rs")
if language == "rust" then
  vim.fn.writefile({ "pub mod change;" }, workspace .. "/src/lib.rs", "a")
  assert(vim.uv.fs_copyfile(root .. "/nvim/rust/forge/rust-toolchain.toml", workspace .. "/rust-toolchain.toml"))
else
  vim.fn.writefile({ "[package]", 'name = "mock-plan-bootstrap"', 'version = "0.1.0"', 'edition = "2021"',
    "[lib]", 'path = "src/change.rs"', "[workspace]" }, workspace .. "/Cargo.toml")
end
vim.fn.writefile({vim.json.encode({ checks = vim.empty_dict() })}, workspace .. "/.forge.json")
local function command(arguments, cwd)
  local result = vim.system(arguments, { cwd = cwd, text = true }):wait(30000)
  assert(result.code == 0, (result.stderr or "") .. (result.stdout or ""))
  return result.stdout
end
command({ "git", "init", "--quiet" }, workspace)
if language == "typescript" then
  local npm = vim.fs.joinpath(vim.fs.dirname(vim.fn.exepath("node")), "node_modules", "npm", "bin", "npm-cli.js")
  command({ "node", npm, "install", "--offline", "--ignore-scripts", "--no-audit", "--no-fund" }, workspace)
end
local executable = assert(vim.g.forge_test_executable)
local generator = assert(vim.g.forge_test_fixture_generator)
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors, notices = {}, {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) else notices[#notices + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local function await(predicate, message)
  assert(vim.wait(30000, function() return #errors > 0 or predicate() end, 10), message .. "\n" .. table.concat(notices, "\n"))
  assert(#errors == 0, table.concat(errors, "\n"))
end
local servers = {
  rust = { file = "src/lib.rs", filetype = "rust", cmd = { "rust-analyzer" },
    init_options = { cargo = { buildScripts = { enable = false }, extraArgs = { "--offline" } }, procMacro = { enable = false }, check = { enable = false }, checkOnSave = false },
    checks = { { "run", "external.count", "dependency/src/lib.rs" }, { "run", "value.count", "src/model.rs" }, { "run", "Mode::Ready", "src/model.rs" } } },
  typescript = { file = "src/main.ts", filetype = "typescript", cmd = { assert(vim.g.forge_test_tsgo), "--lsp", "--stdio" },
    checks = { { "RenderState.run", "external.read", "dependency/index.d.ts" }, { "RenderState.run", "options.$view", "src/model.ts" },
      { "RenderState.run", "options.$view.scale", "src/model.ts" }, { "RenderState.run", "this.getStage()?.labelManager.symbolFader", "src/model.ts" } } },
  lua = { file = "main.lua", filetype = "lua", cmd = { "lua-language-server", "--logpath=" .. data .. "/lua-lsp" },
    settings = { Lua = { workspace = { checkThirdParty = false }, telemetry = { enable = false }, runtime = { version = "LuaJIT" } } },
    checks = { { "run", "Planned.new", "model.lua" }, { "run", "External.read", "dependency/init.lua" } } },
}
local spec = assert(servers[language])
if language == "rust" then
  spec.cmd = { vim.g.forge_test_rust_analyzer or vim.trim(command({ vim.fn.has("win32") == 1 and "rustup.exe" or "rustup", "which", "rust-analyzer" }, root)) }
end
local language_client
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan exercise existing source navigation" })
  require("forge.views.harness.controller").submit()
  await(function() return not state.busy and state.active_plan end, "mock plan did not arrive")
  local seeded = command({ generator, data .. "/forge/harness", workspace, state.session.id, state.active_plan.id, language }, workspace)
  local plan = vim.json.decode(seeded)
  local canonical = vim.json.decode(table.concat(vim.fn.readfile(plan.working_path:gsub("%.md$", ".json"), "b"), "\n"))
  state.active_plan = plan
  local source_path = workspace .. "/" .. spec.file
  -- A running project server starts on another real buffer. The plan source itself stays hidden.
  local project_buffer = vim.api.nvim_create_buf(false, false)
  vim.api.nvim_buf_set_name(project_buffer, workspace .. "/lsp-anchor." .. (language == "typescript" and "ts" or language == "rust" and "rs" or "lua"))
  vim.bo[project_buffer].filetype = spec.filetype
  local identity = assert(vim.lsp.start({ name = "fixture-" .. language, cmd = spec.cmd, root_dir = workspace,
    filetypes = { spec.filetype }, init_options = spec.init_options, settings = spec.settings }, { bufnr = project_buffer }))
  language_client = assert(vim.lsp.get_client_by_id(identity))
  await(function() return language_client.initialized end, "language server did not initialize")
  require("forge.views.plan_review").open(plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "PlanReview did not open")
  local review = state.plan_review
  review.command_set.action_by_id.toggle_public.run({})
  await(function() return review.public_only == false end, "could not expose captured calls")
  local requests, lookups, completions, receipts = 0, {}, {}, {}
  local original_request = client.request_for
  client.request_for = function(session, method, params, callback)
    if method == "harness.document" and params.operation == "plan_definition" then
      completions[#completions + 1] = params
    end
    return original_request(session, method, params, function(result, error)
      if result and result.workspace_definition then lookups[#lookups + 1] = result.workspace_definition end
      callback(result, error)
      if method == "harness.document" and params.operation == "plan_definition" then
        receipts[#receipts + 1] = { result = result, failure = error }
      end
    end)
  end
  local original_definition = language_client.request
  language_client.request = function(server, method, ...)
    if method == "textDocument/definition" then requests = requests + 1 end
    return original_definition(server, method, ...)
  end
  local navigation_checks = {}
  for _, check in ipairs(spec.checks) do
    for _, action in ipairs({ "jump_entity", "open" }) do
      navigation_checks[#navigation_checks + 1] = { check[1], check[2], check[3], action }
    end
  end
  for _, check in ipairs(navigation_checks) do
    local name
    for _, body in ipairs(canonical.design.proposed_calls[spec.file]) do
      if body.owner == check[1] then
        for _, occurrence in ipairs(body.call or {}) do
          if occurrence.source and occurrence.source.expression == check[2] then name = occurrence.name break end
        end
      end
    end
    assert(name, "source occurrence was not captured: " .. check[2])
    local call_row
    local function find_call()
      for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        if row:match("^%s+" .. vim.pesc(name) .. "$") then return index, row end
      end
    end
    local row_text
    call_row, row_text = find_call()
    if not call_row then
      for index, row in ipairs(vim.api.nvim_buf_get_lines(review.buf, 0, -1, false)) do
        local short_owner = check[1]:match("[^.:]+$")
        if row:find(short_owner .. "(", 1, true) then
          vim.api.nvim_set_current_win(review.win)
          vim.api.nvim_win_set_cursor(review.win, { index, 0 })
          review.command_set.action_by_id.toggle.run({})
          break
        end
      end
      await(function() call_row, row_text = find_call() return call_row ~= nil end, "captured call is absent: " .. check[2])
    end
    local completed = #receipts
    local attempts = 0
    repeat
      attempts = attempts + 1
      notices = {}
      vim.api.nvim_set_current_win(review.win)
      vim.api.nvim_win_set_cursor(review.win, { call_row, #row_text - #name })
      review.command_set.action_by_id[check[4]].run({})
      await(function() return #receipts > completed or #notices > 0 end, "definition did not settle")
      if #receipts == completed then vim.wait(100) end
    until #receipts > completed or attempts == 15
    assert(#receipts > completed, "definition remained unavailable: " .. check[2]
      .. "\n" .. table.concat(notices, "\n") .. "\nrequests=" .. requests .. " lookups=" .. #lookups)
    local receipt = receipts[#receipts]
    assert(not receipt.failure, receipt.failure)
    assert(receipt.result.jump or receipt.result.source, "no navigation destination returned")
    assert(not receipt.result.declarations, "definition returned a declaration popup")
    for _, window in ipairs(vim.api.nvim_list_wins()) do
      assert(vim.api.nvim_win_get_config(window).relative == "", "definition opened a floating window")
    end
    local target = vim.fs.normalize(vim.uv.fs_realpath(completions[#completions].target.path))
    assert(target:sub(-#check[3]) == check[3], check[2] .. " resolved to " .. target)
    print(language .. " " .. check[4] .. ": " .. check[2] .. " -> " .. check[3])
    await(function() return not review.cancel_definition end, "definition request did not release")
    vim.api.nvim_set_current_win(review.win)
    if receipt.result.source then
      assert(vim.fs.normalize(vim.api.nvim_buf_get_name(vim.api.nvim_win_get_buf(review.win)))
        == vim.fs.normalize(receipt.result.source.path), "source effect did not open the definition file")
      assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win),
        { receipt.result.source.line, receipt.result.source.column }), "source effect did not select the definition token")
    elseif receipt.result.jump then
      local jump = receipt.result.jump
      local _, start = review.owner.replica.sequence:position(jump.block)
      local row = require("forge.buffer").physical_row(review.owner.replica, start)
      assert(vim.deep_equal(vim.api.nvim_win_get_cursor(review.win),
        { row + jump.position.row + 1, jump.position.column }), "plan effect did not select the definition token")
    end
    if vim.api.nvim_win_get_buf(review.win) ~= review.buf then
      vim.cmd("normal! \15")
      await(function() return vim.api.nvim_win_get_buf(review.win) == review.buf end, "jumplist did not return to PlanReview")
    end
  end
  assert(requests >= #spec.checks and #lookups >= #spec.checks, "navigation bypassed the project language server")
  assert(#vim.lsp.get_clients() == 1 and vim.lsp.get_clients()[1].id == identity, "navigation started another language server")
  assert(vim.fn.filereadable(source_path) == 1)
  client.request_for = original_request
  print("plan_navigation_languages_host: " .. language .. " passed, " .. requests .. " definition requests")
end, debug.traceback)
if success and vim.g.forge_test_manual then
  _G.forge_navigation_test = { workspace = workspace, data = data, language_client = language_client }
  print("Plan navigation fixture ready for Terminal MCP inspection")
  return
end
if language_client then language_client:stop(true) end
require("forge.views.harness.workspace").release(state)
client.stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
if not success then print("Retained fixtures: " .. workspace .. " | " .. data) end
assert(success, failure)
