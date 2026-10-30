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
vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = binary }) return function() end end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state, errors = require("forge.session").harness, {}
local backend_errors = {}
local unsubscribe_errors = client.session_events_subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "error" then
    backend_errors[#backend_errors + 1] = payload
  end
end)
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local controller = require("forge.views.harness.controller")
local function settled()
  local presentation = state.presentation
  if presentation and presentation.applying then return false end
  local transcript = presentation and presentation.transcript
  if transcript then
    for index = 0, transcript.sequence:count() - 1 do
      local node = transcript.sequence:at(index).entry.metadata.node
      if node and node.kind == "exchange" and node.lifecycle ~= "settled" then return false end
    end
  end
  return true
end
local function await(predicate, reason)
  local ready = vim.wait(15000, function() return #errors > 0 or predicate() and settled() end, 10)
  assert(ready, reason .. ": " .. vim.inspect({
    busy = state.busy, status = state.status, diagnostic = state.diagnostic,
    session_id = state.session and state.session.id,
    backend_errors = backend_errors,
    transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf)
      and vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false) or nil,
  }))
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
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "next one", "next two", "next three", "next four", "next five" })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 2 end, "second response did not settle")
  response = messages("ForgeHarnessResponse")[1]
  assert(response.entry.row_count == 5 and response.entry.metadata.node.display == "full", "next exchange folded the most recent prior response")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "third exchange" })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 3 end, "third response did not settle")
  response = messages("ForgeHarnessResponse")[1]
  assert(response.entry.row_count == 5 and response.entry.metadata.node.display == "full", "third prompt folded one of the two latest prior responses")
  assert(messages("ForgeHarnessResponse")[2].entry.row_count == 5, "third prompt folded the latest prior response")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "fourth exchange" })
  controller.submit()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 4 end, "fourth response did not settle")
  response = messages("ForgeHarnessResponse")[1]
  assert(response.entry.row_count == 3 and response.entry.metadata.node.display == "preview", "fourth prompt did not fold the third-to-last response")
  assert(messages("ForgeHarnessResponse")[2].entry.row_count == 5, "fourth prompt folded the second-to-last response")
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
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 5 end, "wrapped response did not settle")
  assert(messages("ForgeHarnessResponse")[1].entry.row_count == 5, "new exchange closed an older manually expanded response")
  assert(messages("ForgeHarnessResponse")[2].entry.row_count == 3, "fifth prompt did not fold the second response")
  toggle(messages("ForgeHarnessResponse")[5])
  await(function() return messages("ForgeHarnessResponse")[5].entry.metadata.node.display == "preview" end, "wrapped response did not fold")
  local wrapped = messages("ForgeHarnessResponse")[5]
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
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 6 end, "list response did not settle")
  local list_response = messages("ForgeHarnessResponse")[6]
  local _, list_start = state.presentation.transcript.sequence:position(list_response.id)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { list_start + 1, 0 })
  vim.cmd("normal! zt")
  local namespace = require("render-markdown.core.ui").ns
  local function render_response()
    local current = messages("ForgeHarnessResponse")[6]
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
  await(function() return messages("ForgeHarnessResponse")[6].entry.metadata.node.display == "preview" end, "list response did not collapse")
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
  toggle(messages("ForgeHarnessResponse")[6])
  await(function() return messages("ForgeHarnessResponse")[6].entry.metadata.node.display == "full" end, "list response did not reopen")
  render_response()
  await(function() return bullet_count() == 3 end, "reopening did not restore the complete list")

  local mock = dofile(root .. "/nvim/tests/forge/support/mock_provider.lua")
  local hold = mock.hold(state.session.id, "chat")
  vim.api.nvim_set_current_win(state.composer_win)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Reveal this completed response", "Including its last line" })
  controller.submit()
  hold:wait()
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 0 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  hold:release()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 7 end, "reveal response did not settle")
  vim.cmd("redraw!")
  local revealed = messages("ForgeHarnessResponse")[7]
  local _, revealed_row = state.presentation.transcript.sequence:position(revealed.id)
  local final_row = vim.api.nvim_buf_line_count(state.transcript_buf)
  assert(vim.api.nvim_win_get_cursor(state.transcript_win)[1] == final_row,
    "native completion did not reveal the response while composer was focused")
  assert(vim.fn.screenpos(state.transcript_win, revealed_row + 1, 1).row > 0
    and vim.fn.screenpos(state.transcript_win, final_row, 1).row > 0,
    "native completion clipped the response after status layout and Markdown rendering")
  assert(vim.api.nvim_get_current_win() == state.composer_win, "native completion stole focus")

  hold = mock.hold(state.session.id, "chat")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Keep the active selection" })
  controller.submit()
  hold:wait()
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 0 })
  vim.cmd("normal! ztvll")
  local selected_cursor, selected_anchor = vim.api.nvim_win_get_cursor(state.transcript_win), vim.fn.getpos("v")
  local selected_topline = vim.fn.winsaveview().topline
  hold:release()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 8 end, "selected response did not settle")
  assert(vim.fn.mode() == "v" and vim.deep_equal(vim.fn.getpos("v"), selected_anchor)
    and vim.deep_equal(vim.api.nvim_win_get_cursor(state.transcript_win), selected_cursor)
    and vim.fn.winsaveview().topline == selected_topline, "native completion changed the active selection or viewport")
  vim.cmd("normal! \27")

  hold = mock.hold(state.session.id, "chat")
  vim.api.nvim_set_current_win(state.composer_win)
  local long_response = {}
  for row = 1, 100 do long_response[row] = "Long response line " .. row end
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, long_response)
  controller.submit()
  hold:wait()
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 0 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  hold:release()
  await(function() return not state.busy and #messages("ForgeHarnessResponse") == 9 end, "long response did not settle")
  vim.cmd("redraw!")
  local _, prompt_row = state.presentation.transcript.sequence:position(messages("ForgeHarnessPrompt")[9].id)
  vim.api.nvim_win_call(state.transcript_win, function()
    local view = vim.fn.winsaveview()
    assert(view.topline == prompt_row + 1 and view.skipcol == 0,
      "long native completion did not place its prompt at the top: " .. vim.inspect({
        view = view, prompt_row = prompt_row, response = messages("ForgeHarnessResponse")[9].entry.metadata.node,
        response_rows = messages("ForgeHarnessResponse")[9].entry.row_count,
        scrolloff = vim.wo[state.transcript_win].scrolloff,
        last_row = vim.api.nvim_buf_line_count(state.transcript_buf),
        height = vim.api.nvim_win_get_height(state.transcript_win),
      }))
  end)
  assert(vim.fn.screenpos(state.transcript_win, vim.api.nvim_buf_line_count(state.transcript_buf), 1).row == 0,
    "long native completion jumped to the end of the response")
  assert(vim.api.nvim_get_current_win() == state.composer_win, "long native completion stole focus")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
unsubscribe_errors()
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "message-fold host was not collected")
dofile(root .. "/nvim/tests/forge/support/provider_stream_host.lua")
