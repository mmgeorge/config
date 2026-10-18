vim.loader.enable(false)
if not vim.g.forge_message_folds_grid then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 80, 36, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_message_folds_grid = true
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
      dofile('nvim/tests/forge/harness_message_folds_host.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("harness_message_folds_host: passed at 80x36")
  vim.cmd("qa!")
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
vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = {} } }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = binary }) return function() end end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state, errors = require("forge.session").harness, {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local controller = require("forge.views.harness.controller")
local function await(predicate, reason)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), reason)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function messages(capture)
  local found = {}
  local transcript = state.presentation and state.presentation.transcript
  if not transcript then return found end
  for index = 0, transcript.sequence:count() - 1 do
    local node = transcript.sequence:at(index)
    local marker = node.entry.metadata.layout and node.entry.metadata.layout.marker
    if marker and marker ~= vim.NIL and marker.capture == capture then found[#found + 1] = node end
  end
  return found
end
local function toggle(node)
  local _, row = state.presentation.transcript.sequence:position(node.id)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
  assert(state.presentation.toggle_heading(state.transcript_win), "message did not accept an expansion action")
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  await(function() return state.ready and state.session and state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "first", "second", "third", "fourth", "fifth" })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 1 end, "first response did not settle")
  local prompt, response = messages("ForgeHarnessPrompt")[1], messages("ForgeHarnessResponse")[1]
  assert(prompt.entry.row_count == 3 and prompt.entry.metadata.node.display == "preview", "sent prompt was not folded to three rows")
  assert(response.entry.row_count == 5 and response.entry.metadata.node.display == "full", "latest response folded before another exchange")
  toggle(prompt)
  await(function() return messages("ForgeHarnessPrompt")[1].entry.row_count == 5 end, "prompt did not expand")
  toggle(response)
  await(function() return messages("ForgeHarnessResponse")[1].entry.metadata.node.display == "preview" end, "response did not collapse")
  toggle(messages("ForgeHarnessResponse")[1])
  await(function() return messages("ForgeHarnessResponse")[1].entry.row_count == 5 end, "response did not expand")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "next exchange" })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 2 end, "second response did not settle")
  response = messages("ForgeHarnessResponse")[1]
  assert(response.entry.row_count == 3 and response.entry.metadata.node.display == "preview", "new exchange did not fold the prior response")
  local _, row = state.presentation.transcript.sequence:position(response.id)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { row + 1, 0 })
  vim.cmd("normal! zt")
  vim.cmd("redraw!")
  local screen = vim.fn.screenpos(state.transcript_win, row + 1, 1)
  assert(vim.fn.screenstring(screen.row, 1) == "▸", "folded response retained its circle marker")
  toggle(response)
  await(function() return messages("ForgeHarnessResponse")[1].entry.row_count == 5 end, "prior response lost its retained text")
  assert(messages("ForgeHarnessPrompt")[1].entry.row_count == 5, "new exchange discarded explicit prompt expansion")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { string.rep("wrapped words λ ", 70) })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 3 end, "wrapped response did not settle")
  assert(messages("ForgeHarnessResponse")[1].entry.row_count == 5, "new exchange closed an older manually expanded response")
  toggle(messages("ForgeHarnessResponse")[3])
  await(function() return messages("ForgeHarnessResponse")[3].entry.metadata.node.display == "preview" end, "wrapped response did not fold")
  local wrapped = messages("ForgeHarnessResponse")[3]
  local _, start = state.presentation.transcript.sequence:position(wrapped.id)
  vim.api.nvim_win_set_cursor(state.transcript_win, { start + 1, 0 })
  vim.cmd("normal! zt")
  vim.cmd("redraw!")
  local text = vim.api.nvim_buf_get_lines(state.transcript_buf, start, start + wrapped.entry.row_count, false)
  local first = vim.fn.screenpos(state.transcript_win, start + 1, 1)
  local last = vim.fn.screenpos(state.transcript_win, start + #text, #text[#text])
  assert(last.row - first.row == 2, "wrapped preview must occupy exactly three display rows: " .. vim.inspect({ first, last, text }))
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, {
    "Submitted the plan for review with three gameplay systems:", "",
    "- Rechargeable directional dash with swept collision.",
    "- Shield rewards every third orb.", "- Escalating chaser waves at 15 and 30 seconds.", "",
    "The plan includes checks.",
  })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 4 end, "list response did not settle")
  local list_response = messages("ForgeHarnessResponse")[4]
  local _, list_start = state.presentation.transcript.sequence:position(list_response.id)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { list_start + 1, 0 })
  vim.cmd("normal! zt")
  local namespace = require("render-markdown.core.ui").ns
  local function render_response()
    local current = messages("ForgeHarnessResponse")[4]
    require("forge.render.harness.markdown").render(state.transcript_buf, state.transcript_win, {
      { first0 = list_start, after0 = list_start + current.entry.row_count },
    })
  end
  render_response()
  local function bullet_count()
    local count = 0
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.transcript_buf, namespace, 0, -1, { details = true })) do
      for _, chunk in ipairs(mark[2] >= list_start and mark[4].virt_text or {}) do
        if chunk[1]:find("●", 1, true) then count = count + 1 end
      end
    end
    return count
  end
  vim.cmd("redraw!")
  await(function() return bullet_count() == 3 end, "expanded response bullets did not render")
  toggle(list_response)
  await(function() return messages("ForgeHarnessResponse")[4].entry.metadata.node.display == "preview" end, "list response did not collapse")
  vim.wait(200)
  require("render-markdown.core.ui").update(state.transcript_buf, state.transcript_win, "CursorMoved", false)
  vim.wait(30)
  vim.cmd("redraw!")
  assert(bullet_count() == 1, "collapsed response retained hidden bullet decorations: " .. bullet_count())
  local preview_row = vim.fn.screenpos(state.transcript_win, list_start + 3, 1).row
  assert(preview_row > 0, "list preview was not rendered on the attached grid")
  local rendered = ""
  for column = 1, 80 do rendered = rendered .. vim.fn.screenstring(preview_row, column) end
  assert(rendered:find("Rechargeable directional dash", 1, true), "visible list item was lost")
  toggle(messages("ForgeHarnessResponse")[4])
  await(function() return messages("ForgeHarnessResponse")[4].entry.metadata.node.display == "full" end, "list response did not reopen")
  render_response()
  await(function() return bullet_count() == 3 end, "reopening did not restore the complete list")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "message-fold host was not collected")
