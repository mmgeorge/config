if vim.g.forge_attachments_grid then
  local root = vim.fn.getcwd()
  local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
  local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
  local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
  local fixture_evidence = dofile("nvim/lua/forge/tests/support/evidence_directory.lua")("harness-attachments")
  local temporary = fixture_evidence.path
  local workspace, data = temporary .. "/workspace", temporary .. "/data"
  vim.fn.mkdir(workspace, "p")
  vim.fn.mkdir(data, "p")
  local owned_root = vim.uv.fs_realpath(temporary)
  vim.fn.writefile({ vim.json.encode({ checks = vim.empty_dict() }) }, workspace .. "/.forge.json")
  local image_path = workspace .. "/screen shot.png"
  local image_file = assert(io.open(image_path, "wb"))
  assert(image_file:write(vim.base64.decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+j5V8AAAAASUVORK5CYII=")))
  assert(image_file:close())
  local document_path = workspace .. "/report.pdf"
  vim.fn.writefile({ "%PDF-1.7", "attachment fixture", "%%EOF" }, document_path)
  -- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
  local binary = dofile("nvim/lua/forge/tests/support/host_executable.lua")()
  local original_stdpath = vim.fn.stdpath
  vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
  package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = binary }) return function() end end }
  local client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
  local state = require("forge.session").harness
  local controller = require("forge.views.harness.controller")
  local attachment = require("forge.views.harness.attachment")
  local failures, warnings = {}, {}
  require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
  require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
  local wait_options = { timeout_ms = 4000, errors = failures }
  local success, failure = xpcall(function()
    vim.fn.chdir(workspace)
    require("forge").setup({ harness = { provider = "mock" } })
    require("forge").open_harness()
    wait_until(function() return state.ready and state.session and state.presentation and state.presentation.ready end,
      "Harness did not open", wait_options)
    vim.api.nvim_set_current_win(state.composer_win)
    vim.api.nvim_paste('"' .. image_path .. '"', false, -1)
    assert(attachment.pending(state.composer_buf), "file import did not run asynchronously")
    controller.submit()
    assert(#warnings == 1 and not require("forge.views.harness.activity").busy(state), "submission consumed an unfinished attachment")
    vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "Inspect " })
    wait_until(function() return not attachment.pending(state.composer_buf) end, "image import did not finish", wait_options)
    local image_prompt = buffer.text(state.composer_buf)
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

    local selected = attachment.capture(state.composer_buf).attachments[1]
    vim.api.nvim_win_set_cursor(state.composer_win, { 1, selected.start })
    vim.cmd("normal! v")
    vim.api.nvim_win_set_cursor(state.composer_win, { 1, selected["end"] - 1 })
    keymap.feed("y")
    local image_reference = vim.fn.getreg('"')
    assert(image_reference == "[Image #1]", "yank copied non-text payload")
    local mock = dofile(root .. "/nvim/lua/forge/tests/support/mock_provider.lua")
    local held = mock.hold(state.session.id, "chat")
    local captured = attachment.capture(state.composer_buf)
    buffer.set(state.composer_buf, { "Attachment editing fixture" })
    controller.submit()
    assert(held:wait().prompt == "Attachment editing fixture")
    buffer.set(state.composer_buf, { image_prompt })
    attachment.restore(state.composer_buf, captured.attachments)
    controller.queue_submit()
    assert(#state.queue == 1 and state.queue[1].text == image_prompt and #state.queue[1].attachments == 1, "queued prompt lost the attachment")
    buffer.set(state.composer_buf, { "Unsent draft" })
    controller.edit_queued(-1)
    vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "Edited: " })
    controller.submit()
    assert(state.queue[1].text == "Edited: " .. image_prompt and #state.queue[1].attachments == 1
      and buffer.text(state.composer_buf) == "Unsent draft", "queue edit lost its attachment or original draft")
    -- Editor drafts await host acknowledgement in `input.drafts`; the committed edit replaces the
    -- confirmed host message that carries the attachment.
    wait_until(function()
      for _, draft in ipairs(state.input.drafts) do
        if draft.draft_state == "pending" then return false end
      end
      return not state.queue_edit and vim.iter(state.input.confirmed):any(function(message)
        return message.text == "Edited: " .. image_prompt and #(message.attachments or {}) == 1
      end)
    end, "attachment edit was not acknowledged", wait_options)
    held:release()
    wait_until(function() return not require("forge.views.harness.activity").busy(state) and #state.queue == 0 end,
      "queued attachment prompt did not settle", wait_options)
    assert(state.session.name == "Attachment editing fixture", "queued input replaced the conversation name")
    wait_until(function()
      for _, line in ipairs(buffer.lines(state.transcript_buf)) do
        if line:find("Edited: Inspect [Image #1]", 1, true) and not line:find("Mock response", 1, true) then return true end
      end
      return false
    end, "prompt preview wrapped hidden attachment metadata", wait_options)

    local literal = "Explain [Image #1](forge-attachment:broken) and <image path=missing.png> as source text"
    buffer.set(state.composer_buf, { literal })
    attachment.restore(state.composer_buf, {})
    assert(#attachment.capture(state.composer_buf).attachments == 0)
    controller.submit()
    wait_until(function() return not require("forge.views.harness.activity").busy(state) end,
      "literal source prompt did not settle", wait_options)
    wait_until(function()
      return buffer.text(state.transcript_buf):find("forge-attachment:broken", 1, true) ~= nil
    end, "literal attachment syntax disappeared from the transcript", wait_options)

    buffer.set(state.composer_buf, { "" })
    vim.api.nvim_win_set_cursor(state.composer_win, { 1, 0 })
    vim.api.nvim_paste('"' .. document_path .. '"', false, -1)
    wait_until(function() return not attachment.pending(state.composer_buf) end, "PDF import did not finish", wait_options)
    local document_reference = buffer.text(state.composer_buf)
    assert(document_reference:match("^%[PDF #%d+%]$"), "PDF was not imported as an attachment")
    local document_attachment = attachment.capture(state.composer_buf).attachments[1]
    buffer.set(state.composer_buf, { "Image: " .. image_reference, "Document: " .. document_reference })
    image_attachment.start, image_attachment["end"] = 7, 7 + #image_reference
    document_attachment.start = #("Image: " .. image_reference .. "\nDocument: ")
    document_attachment["end"] = document_attachment.start + #document_reference
    attachment.restore(state.composer_buf, { image_attachment, document_attachment })
    assert(#attachment.capture(state.composer_buf).attachments == 2)
    vim.cmd("redraw!")
  end, debug.traceback)

  require("forge.views.harness.workspace").release(state)
  if state.presentation then state.presentation.close() end
  client.host_stop()
  local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
  vim.fn.chdir(root)
  vim.fn.stdpath = original_stdpath
  assert(not vim.uv.fs_stat(temporary) or vim.uv.fs_realpath(temporary) == owned_root, "attachment fixture cleanup escaped its temporary root")
  if success then
    local release_failure
    wait_until(function()
      release_failure = fixture_evidence.release(true)
      return release_failure == nil
    end, "attachment fixture evidence was not released", { timeout_ms = 4000, context = function() return tostring(release_failure) end })
  end
  assert(collected, "attachment host was not collected")
  assert(success, tostring(failure) .. "\nArtifacts: " .. temporary)
  return
end

local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harness_attachments"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE", "-i", "NONE", "--noplugin" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(100, 32, { rgb = true })
  child.lua([[
    vim.g.forge_attachments_grid = true
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
    dofile('nvim/lua/forge/tests/integration/views/harness/harness_attachments.test.lua')
  ]])
end

return T
