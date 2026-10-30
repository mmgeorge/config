vim.loader.enable(false)
if not vim.g.forge_provider_stream_grid then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0, "could not start isolated provider-stream editor")
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 100, 36, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_provider_stream_grid = true
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
      dofile('nvim/tests/forge/support/provider_stream_host.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("provider_stream_host: passed at 100x36")
  return
end

local plugin_root = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy")
vim.opt.runtimepath:append(vim.fs.joinpath(vim.fn.stdpath("data"), "site"))
vim.opt.runtimepath:append(plugin_root .. "/render-markdown.nvim")
vim.opt.runtimepath:append(plugin_root .. "/nvim-treesitter")
require("render-markdown").setup(require("plugins.markdown")[1].opts())
vim.cmd("runtime plugin/render-markdown.lua")

local root, workspace, data = vim.fn.getcwd(), vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = {
  ensure = function(callback)
    callback({ ok = true, path = binary })
    return function() end
  end,
}
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state, errors = require("forge.session").harness, {}
local backend_errors = {}
local unsubscribe_errors = client.session_events_subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "error" then
    backend_errors[#backend_errors + 1] = payload
  end
end)
require("forge.infra.notifications").error = function(message)
  errors[#errors + 1] = message
end
local controller = require("forge.views.harness.controller")
local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")

---@param predicate fun(): boolean
---@param reason string
local function await(predicate, reason)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), reason .. ": " .. vim.inspect({
    ready = state.ready, session_id = state.session and state.session.id,
    presentation_ready = state.presentation and state.presentation.ready, status = state.status,
    transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false) or nil,
  }))
  assert(#errors == 0, table.concat(errors, "\n"))
end

---@return table[]
local function transcript_nodes()
  local nodes = {}
  local transcript = state.presentation and state.presentation.transcript
  if transcript then
    for index = 0, transcript.sequence:count() - 1 do
      nodes[#nodes + 1] = transcript.sequence:at(index)
    end
  end
  return nodes
end

---@return integer
local function response_count()
  local count = 0
  for _, node in ipairs(transcript_nodes()) do
    local marker = node.entry.metadata.layout and node.entry.metadata.layout.marker
    if marker and marker ~= vim.NIL and marker.capture == "ForgeHarnessResponse" then count = count + 1 end
  end
  return count
end

---@return integer
local function settled_activity_count()
  local count = 0
  for _, node in ipairs(transcript_nodes()) do
    local metadata = node.entry.metadata.node
    if metadata and metadata.kind == "exchange" and metadata.lifecycle == "settled" then count = count + 1 end
  end
  return count
end

local function expand_activity()
  for _, node in ipairs(transcript_nodes()) do
    local metadata = node.entry.metadata.node
    if metadata and metadata.kind == "exchange" and metadata.display ~= "full" then
      local target_id = metadata.id
      local _, row = state.presentation.transcript.sequence:position(node.id)
      vim.api.nvim_set_current_win(state.transcript_win)
      vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
      assert(state.presentation.toggle_heading(state.transcript_win), "activity heading could not expand")
      local expanded = vim.wait(15000, function()
        for _, updated in ipairs(transcript_nodes()) do
          local current = updated.entry.metadata.node
          if current and current.id == target_id then
            return current and current.display == "full"
          end
        end
        return false
      end, 10)
      local current = {}
      for _, updated in ipairs(transcript_nodes()) do
        local candidate = updated.entry.metadata.node
        if candidate and candidate.kind == "exchange" then current[#current + 1] = candidate end
      end
      assert(expanded, "activity expansion did not publish: " .. vim.inspect({
        target = target_id, nodes = current, failure = state.presentation.failure,
        section_failure = state.presentation.section_failure,
      }))
    end
  end
end

---@param expected integer
local function assert_content(expected)
  local identities, responses, repeated_text = {}, 0, 0
  for _, node in ipairs(transcript_nodes()) do
    assert(not identities[node.id], "duplicate transcript block identity: " .. node.id)
    identities[node.id] = true
    local marker = node.entry.metadata.layout and node.entry.metadata.layout.marker
    if marker and marker ~= vim.NIL then
      if marker.capture == "ForgeHarnessResponse" then responses = responses + 1 end
      if marker.capture == "ForgeHarnessCommentary" then
        local _, row = state.presentation.transcript.sequence:position(node.id)
        local heading = vim.api.nvim_buf_get_lines(state.transcript_buf, row, row + 1, false)[1]
        assert(heading and heading:find("%S"), "empty commentary heading remains in the transcript")
      end
    end
  end
  local lines = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
  for _, line in ipairs(lines) do
    if line:find("Checked all callers", 1, true) then repeated_text = repeated_text + 1 end
  end
  assert(responses == expected, "synthetic response count differs: " .. responses)
  assert(repeated_text == expected * 2, "commentary and synthetic response text were not both retained: "
    .. repeated_text .. ": " .. vim.inspect(lines))
end

local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({
    harness = {
      backend = "mock",
      backends = { mock = { command = { "provider-stream" }, selectable = true } },
    },
  })
  require("forge").open_harness()
  await(function()
    return state.ready and state.session and state.presentation and state.presentation.ready
  end, "provider-stream Harness did not open")
  local hold = mock.hold(state.session.id, "chat")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Inspect callers" })
  controller.submit()
  hold:wait()
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 0 })
  local cursor = vim.api.nvim_win_get_cursor(state.transcript_win)
  local viewport = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  hold:release()
  await(function()
    return not state.busy and response_count() == 1 and settled_activity_count() == 1
      and not state.presentation.applying and state.status and state.status.kind == "idle"
  end, "provider stream did not settle")
  vim.cmd("redraw!")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(state.transcript_win), cursor), "status refresh moved the cursor")
  local refreshed = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  assert(refreshed.topline == viewport.topline, "status refresh moved the viewport")
  expand_activity()
  assert_content(1)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Inspect another invocation" })
  controller.submit()
  await(function()
    return not state.busy and response_count() == 2 and settled_activity_count() == 2
      and not state.presentation.applying and state.status and state.status.kind == "idle"
  end, "following provider invocation did not settle")
  expand_activity()
  assert_content(2)
  local session_id = state.session.id
  assert(require("forge.shared.view_command_set").dispatch(controller.command_set(), "close", state),
    "Harness close action was unavailable")
  require("forge.views.harness.session_navigation").resume(session_id)
  await(function()
    state = require("forge.session").harness
    return state.ready and state.session and state.session.id == session_id
      and state.presentation and state.presentation.ready and response_count() == 2
  end, "provider transcript did not reopen from persisted state for " .. session_id)
  expand_activity()
  assert_content(2)
end, debug.traceback)

local function cleanup()
  unsubscribe_errors()
  require("forge.views.harness.workspace").release(state)
  if state.presentation then state.presentation.close() end
  client.host_stop()
  local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
  vim.fn.chdir(root)
  vim.fn.stdpath = original_stdpath
  assert(collected, "provider-stream host was not collected")
end
if success and vim.g.forge_provider_stream_keep_open then
  _G.forge_provider_stream_fixture = { cleanup = cleanup, assert_content = assert_content, state = state }
  return
end
cleanup()
assert(success, tostring(failure) .. ": " .. vim.inspect(backend_errors))
