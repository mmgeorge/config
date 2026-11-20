-- Unit tests for `forge.views.harness.prompt_submission` (views/harness/prompt_submission.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- A composer draft that opens with empty lines sends its text from the first nonblank line; the
-- prompt row otherwise drew its marker alone above the text. Attachment offsets follow the text,
-- while the draft kept for restoration keeps its own offsets.
T["composer_prompt_is_sent_without_leading_blank_lines"] = function()
  local input_state = require("forge.views.harness.input_state")
  local submitted, submitted_attachments
  stub(input_state, "submit", function(_, text, attachments) submitted, submitted_attachments = text, attachments end)
  local composer = buffer.scratch()
  local source = { "", "  ", "Spawn [Image #1] agents.", "" }
  local image = { kind = "image", start = 10, ["end"] = 20 }
  vim.api.nvim_buf_set_lines(composer, 0, -1, false, source)
  require("forge.views.harness.prompt_submission").start({}, {
    source = source,
    attachments = { image },
    context = { document = "composer", buffer = composer, is_current = function() return false end },
  }, function() end)
  eq(submitted, "Spawn [Image #1] agents.\n")
  eq(submitted:sub(submitted_attachments[1].start + 1, submitted_attachments[1]["end"]), "[Image #1]")
  eq({ image.start, image["end"] }, { 10, 20 })
end

return T
