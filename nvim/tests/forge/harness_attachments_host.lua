vim.loader.enable(false)
if not vim.g.forge_attachments_grid and not vim.g.forge_attachments_manual then
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = pcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 100, 32, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", [[
      vim.g.forge_attachments_grid = true
      vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
      dofile('nvim/tests/forge/harness_attachments_host.lua')
    ]], {})
  end)
  vim.fn.jobstop(child)
  assert(success, failure)
  print("harness_attachments_host: passed at 100x32")
  vim.cmd("qa!")
  return
end

local root = vim.fn.getcwd()
local temporary = vim.fs.joinpath(vim.fn.stdpath("cache"), "forge-attachment-tests", tostring(vim.uv.hrtime()))
local workspace, data = temporary .. "/workspace", temporary .. "/data"
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local owned_root = vim.uv.fs_realpath(temporary)
vim.fn.writefile({ vim.json.encode({ checks = { version = 2, definitions = {} } }) }, workspace .. "/.forge.json")
local image_path = workspace .. "/screen shot.png"
local image_file = assert(io.open(image_path, "wb"))
assert(image_file:write(vim.base64.decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+j5V8AAAAASUVORK5CYII=")))
assert(image_file:close())
local document_path = workspace .. "/report.pdf"
vim.fn.writefile({ "%PDF-1.7", "attachment fixture", "%%EOF" }, document_path)
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(vim.g.forge_test_executable or require("forge.builder").binary_path(), binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = binary }) return function() end end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local attachment = require("forge.views.harness.attachment")
local failures, warnings = {}, {}
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
local function await(predicate, reason)
  assert(vim.wait(15000, function() return #failures > 0 or predicate() end, 10), reason)
  assert(#failures == 0, table.concat(failures, "\n"))
end
local function composer()
  return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  await(function() return state.ready and state.session and state.presentation and state.presentation.ready end, "Harness did not open")
  vim.api.nvim_set_current_win(state.composer_win)
  local started = vim.uv.hrtime()
  vim.api.nvim_paste('"' .. image_path .. '"', false, -1)
  local paste_ms = (vim.uv.hrtime() - started) / 1000000
  assert(attachment.pending(state.composer_buf), "file import did not run asynchronously")
  controller.submit()
  assert(#warnings == 1 and not state.busy, "submission consumed an unfinished attachment")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "Inspect " })
  await(function() return not attachment.pending(state.composer_buf) end, "image import did not finish")
  local image_prompt = composer()
  assert(image_prompt == "Inspect [Image #1]", image_prompt)
  local image_attachment = attachment.capture(state.composer_buf).attachments[1]
  assert(image_attachment and image_attachment.media_type == "image/png")
  assert(vim.fn.delete(image_path) == 0)

  vim.cmd("redraw!")
  local screen = vim.fn.screenpos(state.composer_win, 1, 1)
  assert(screen.row > 0, "composer is outside the attached editor grid")
  local rendered = ""
  for column = 1, 100 do rendered = rendered .. vim.fn.screenstring(screen.row, column) end
  assert(rendered:find("Inspect [Image #1]", 1, true) and not rendered:find("forge-attachment", 1, true),
    "attachment metadata was visible on the editor grid: " .. rendered)

  vim.api.nvim_win_set_cursor(state.composer_win, { 1, 10 })
  vim.api.nvim_feedkeys("yiA", "xt", false)
  local image_reference = vim.fn.getreg('"')
  assert(image_reference == "[Image #1]", "yank copied non-text payload")
  state.busy = true
  controller.queue_submit()
  assert(#state.queue == 1 and state.queue[1].text == image_prompt and #state.queue[1].attachments == 1, "queued prompt lost the attachment")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Unsent draft" })
  controller.edit_last_queued()
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "Edited: " })
  controller.submit()
  assert(state.queue[1].text == "Edited: " .. image_prompt and #state.queue[1].attachments == 1 and composer() == "Unsent draft", "queue edit lost its attachment or original draft")
  state.busy = false
  controller.drain()
  await(function() return not state.busy and #state.queue == 0 end, "queued attachment prompt did not settle")
  assert(state.session.name == "Edited: Inspect [Image #1]", "session title exposed attachment metadata: " .. state.session.name)
  await(function()
    for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line:find("Edited: Inspect [Image #1]", 1, true) and not line:find("Mock response", 1, true) then return true end
    end
    return false
  end, "prompt preview wrapped hidden attachment metadata")

  local literal = "Explain [Image #1](forge-attachment:broken) and <image path=missing.png> as source text"
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { literal })
  attachment.restore(state.composer_buf, {})
  assert(#attachment.capture(state.composer_buf).attachments == 0)
  controller.submit()
  await(function() return not state.busy end, "literal source prompt did not settle")
  await(function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"):find("forge-attachment:broken", 1, true) ~= nil
  end, "literal attachment syntax disappeared from the transcript")

  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "" })
  vim.api.nvim_win_set_cursor(state.composer_win, { 1, 0 })
  vim.api.nvim_paste('"' .. document_path .. '"', false, -1)
  await(function() return not attachment.pending(state.composer_buf) end, "PDF import did not finish")
  local document_reference = composer()
  assert(document_reference:match("^%[PDF #%d+%]$"), "PDF was not imported as an attachment")
  local document_attachment = attachment.capture(state.composer_buf).attachments[1]
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Image: " .. image_reference, "Document: " .. document_reference })
  image_attachment.start, image_attachment["end"] = 7, 7 + #image_reference
  document_attachment.start = #("Image: " .. image_reference .. "\nDocument: ")
  document_attachment["end"] = document_attachment.start + #document_reference
  attachment.restore(state.composer_buf, { image_attachment, document_attachment })
  assert(#attachment.capture(state.composer_buf).attachments == 2)
  vim.cmd("redraw!")
  print(string.format("attachment paste dispatch: %.3f ms", paste_ms))
  vim.g.forge_attachment_fixture = { workspace = workspace, data = data, image = image_reference, document = document_reference }
end, debug.traceback)

local function cleanup()
  require("forge.views.harness.workspace").release(state)
  if state.presentation then state.presentation.close() end
  client.stop()
  local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
  vim.fn.chdir(root)
  vim.fn.stdpath = original_stdpath
  assert(not vim.uv.fs_stat(temporary) or vim.uv.fs_realpath(temporary) == owned_root, "attachment fixture cleanup escaped its temporary root")
  if success then
    assert(vim.wait(5000, function()
      if not vim.uv.fs_stat(temporary) then return true end
      assert(vim.uv.fs_realpath(temporary) == owned_root, "attachment fixture cleanup escaped its temporary root")
      return vim.fn.delete(temporary, "rf") == 0
    end, 20), "attachment fixture cleanup failed")
  else print("attachment failure artifacts: " .. temporary) end
  return collected
end
if success and vim.g.forge_attachments_manual then
  _G.forge_attachment_cleanup = cleanup
  return
end
assert(cleanup(), "attachment host was not collected")
assert(success, tostring(failure) .. "\nArtifacts: " .. temporary)
