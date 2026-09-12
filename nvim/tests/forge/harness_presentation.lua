vim.loader.enable(false)
local client = require("forge.client")
local original_request = client.request_for
local original_accepting = client.host_accepting
client.host_accepting = function() return true end
local requests = {}
client.request_for = function(session, method, params, callback)
  assert(session == "test-session" and (method == "harness.document" or method == "prompt.submit"))
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
local function snapshot(document, text, editable)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = editable and {
      { id = "composer", revision = 0, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } },
    } or {} } } } }
end
local owner
local ok, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "draft" })
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local attached
  owner = require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) error(message) end,
  }, function(value, error_message) assert(not error_message, error_message) attached = value end)
  local opened = requests[1].params
  assert(opened.initial[1] == "draft")
  requests[1].callback({ transcript = snapshot(opened.document, "native transcript"), composer = snapshot(opened.composer, "draft", true) })
  assert(attached == owner and owner.ready)
  assert(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)[1] == "native transcript")
  assert(vim.bo[composer].modifiable and not vim.bo[transcript].modifiable)
  owner.sync()
  owner.sync()
  assert(#requests == 2, "transcript sync requests were concurrent")
  requests[2].callback({ patch = {} })
  assert(#requests == 3)
  requests[3].callback({ patch = {} })
  vim.api.nvim_buf_set_text(composer, 0, 0, 0, 5, { "first typed prompt" })
  require("forge.editable").flush(owner.composer.editable)
  local edit = requests[4].params.edit
  assert(edit.base == 0 and edit.sequence == 1, "initial draft consumed local edit sequence")
  local typed_tick = vim.api.nvim_buf_get_changedtick(composer)
  local edited_metadata = snapshot(opened.composer, "first typed prompt", true).block[1].metadata
  edited_metadata.editable_region[1].revision = 1
  requests[4].callback({ accepted = true, acknowledgement = { document = opened.composer, region = "composer", revision = 1, sequence = 1 },
    patch = { document = opened.composer, base = 0, next = 1, base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
      block_edit = {}, removed_block = {}, text_edit = { { start_row = 0, removed_rows = 1, text = { "first typed prompt" } } },
      metadata_edit = { { block = "body", row_count = 1, metadata = edited_metadata } },
    },
  })
  assert(owner.composer.revision == 1 and vim.bo[composer].modifiable)
  assert(vim.api.nvim_buf_get_changedtick(composer) == typed_tick, "composer acknowledgement rewrote typing")
  owner.submit(function() end)
  assert(requests[5].method == "prompt.submit" and requests[5].params.composer.revision == 1)
  assert(not requests[5].params.text, "composer submitted independently parsed Lua text")
  local cleared_metadata = snapshot(opened.composer, "", true).block[1].metadata
  cleared_metadata.editable_region[1].revision = 2
  assert(owner.receive({ kind = "composer_patch", data = { document = opened.composer, base = 1, next = 2,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "" } } },
    metadata_edit = { { block = "body", row_count = 1, metadata = cleared_metadata } },
  } }))
  assert(vim.bo[composer].modifiable and vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "")
  assert(owner.close())
  assert(requests[6].params.operation == "close")
  assert(not vim.api.nvim_buf_is_valid(transcript) and not vim.api.nvim_buf_is_valid(composer))

  transcript = vim.api.nvim_create_buf(false, true)
  composer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, transcript)
  local rejected
  require("forge.views.harness.presentation").open({ session_id = "test-session",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end,
  }, function(value, error_message) assert(not value) rejected = error_message end)
  local delayed = requests[7]
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, { "new startup draft" })
  local changedtick = vim.api.nvim_buf_get_changedtick(composer)
  delayed.callback({ transcript = snapshot(delayed.params.document, "stale transcript"),
    composer = snapshot(delayed.params.composer, "", true) })
  assert(rejected and rejected:find("startup text changed", 1, true))
  assert(vim.api.nvim_buf_is_valid(composer) and vim.api.nvim_buf_get_changedtick(composer) == changedtick)
  assert(vim.api.nvim_buf_get_lines(composer, 0, -1, false)[1] == "new startup draft")
  assert(requests[8].params.operation == "close")
  vim.api.nvim_buf_delete(transcript, { force = true })
  vim.api.nvim_buf_delete(composer, { force = true })
end, debug.traceback)
client.request_for = original_request
client.host_accepting = original_accepting
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else print("harness_presentation OK") vim.cmd("qa!") end
