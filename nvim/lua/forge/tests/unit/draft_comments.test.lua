-- Unit tests for `forge.draft_comments` (draft_comments.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["draft_comment_threads"] = function()
  local comments = require("forge.draft_comments")
  local buffer = buffer_support.scratch()
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, buffer)
  local question = { id = 1, source_line = 1, end_source_line = 1, kind = "question", heading = "Custom question", body = "Why this boundary?" }
  local ordinary = { id = 2, source_line = 1, end_source_line = 1, body = "Keep this API." }
  local state = comments.attach(buffer, window, { "pub struct Boundary;", "Next source row" }, { question, ordinary }, { guard_source = true })
  comments.update(buffer, { { id = "1", replies_body = question.body, replies = {
    { heading = "First answer", body_lines = { "The owner isolates state." } },
    { heading = "Second answer", body_lines = { "Callers use the public interface." } },
  } } })
  assert(buffer_support.text(buffer):find("Custom question", 1, true) and buffer_support.text(buffer):find("Plan comment", 1, true), "per-comment headings did not share the renderer")
  assert(buffer_support.text(buffer):find("First answer", 1, true) and buffer_support.text(buffer):find("Second answer", 1, true), "multiple attached replies were lost")
  local function move_to(label)
    local row = buffer_support.find_row(buffer, label)
    if not row then error("missing " .. label) end
    vim.api.nvim_win_set_cursor(window, { row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buffer })
  end
  move_to("The owner isolates state.")
  assert(question.focused and not vim.bo[buffer].modifiable, "reply did not expand its thread while remaining readonly")
  local lines = buffer_support.lines(buffer)
  for row, line in ipairs(lines) do
    if line:find("First answer", 1, true) then
      assert(lines[row - 1] == question.body, "question and answer retained duplicate divider rows")
    elseif line:find("Second answer", 1, true) then
      assert(lines[row - 1] == "The owner isolates state.", "attached answers retained duplicate divider rows")
    end
  end
  comments.delete_at_cursor(buffer)
  assert(#comments.capture(buffer) == 2, "deleting a reply deleted its parent")
  assert(comments.focus(buffer, "1"), "numeric and string identities did not match")
  assert(buffer_support.text(buffer):find("The owner isolates state.", 1, true), "focusing the parent discarded replies")
  move_to("Callers use the public interface.")
  assert(not vim.bo[buffer].modifiable, "attached reply became editable")
  assert(comments.capture(buffer)[1].source.body == "Why this boundary?", "capture merged answer text into the question")
  comments.focus(buffer, 1)
  local position = vim.api.nvim_win_get_cursor(window)
  vim.api.nvim_buf_set_text(buffer, position[1] - 1, 0, position[1] - 1, 0, { "Revised: " })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
  local captured = comments.capture(buffer)
  assert(captured[1].kind == "question" and captured[1].source.body == "Revised: Why this boundary?", "question capture lost its tag or edited body")
  comments.update(buffer, { { id = 1, replies_body = "Why this boundary?", replies = { { heading = "Old answer", body_lines = { "Stale response" } } } } })
  assert(not buffer_support.text(buffer):find("Stale response", 1, true), "an answer to old text appeared under a changed question")
  comments.update(buffer, { { id = 1, heading = "Renamed question", replies_body = captured[1].source.body,
    replies = { { heading = "Updated answer", body_lines = { "Current response." } } } } })
  assert(buffer_support.text(buffer):find("Renamed question", 1, true) and buffer_support.text(buffer):find("Current response.", 1, true), "thread presentation did not update")
  assert(comments.capture(buffer)[2].source.body == ordinary.body, "another comment sharing the source line was changed")
  move_to("Current response.")
  comments.add_at_cursor(buffer, false, { kind = "question", heading = "Plan question" })
  local followup = state.annotation_list[3]
  assert(followup.parent_id == "1" and followup.source_line == 1, "answer follow-up lost its thread or source")
  local position = vim.api.nvim_win_get_cursor(window)
  vim.api.nvim_buf_set_lines(buffer, position[1] - 1, position[1], false, { "How do callers observe it?" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
  comments.update(buffer, { { id = followup.id, replies_body = followup.body,
    replies = { { heading = "Plan answer", body_lines = { "Use the getter." } } } } })
  move_to("Use the getter.")
  comments.add_at_cursor(buffer, false)
  local change = state.annotation_list[4]
  assert(change.parent_id == tostring(followup.id), "change request did not attach to the selected answer")
  local position = vim.api.nvim_win_get_cursor(window)
  vim.api.nvim_buf_set_lines(buffer, position[1] - 1, position[1], false, { "Add that getter." })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
  move_to("Next source row")
  move_to("Current response.")
  assert(question.focused and followup.focused and change.focused and not ordinary.focused,
    "focusing an earlier answer failed to expand exactly its conversation")
  assert(not vim.bo[buffer].modifiable, "earlier answer became editable")
  local captured = comments.capture(buffer)
  assert(captured[3].parent_id == "1" and captured[4].parent_id == tostring(followup.id), "capture dropped thread links")
  assert(buffer_support.text(buffer):find("Use the getter.", 1, true) and buffer_support.text(buffer):find("Add that getter.", 1, true), "follow-up chain disappeared")
  local lines = buffer_support.lines(buffer)
  for row, line in ipairs(lines) do
    if line:find("Plan question", 1, true) then
      assert(lines[row - 1] == "Current response.", "follow-up question retained a duplicate border")
    elseif line:find("Plan comment", 1, true) and lines[row + 1] == "Add that getter." then
      assert(lines[row - 1] == "Use the getter.", "follow-up comment retained a duplicate border")
    end
  end
  move_to("Next source row")
  local top_count, bottom_count = 0, 0
  for _, line in ipairs(buffer_support.lines(buffer)) do
    if line:find("╭", 1, true) then top_count = top_count + 1 end
    if line:find("╰", 1, true) then bottom_count = bottom_count + 1 end
    if line:find("Plan question", 1, true) then assert(line:find("├", 1, true), "follow-up question began a separate box") end
  end
  assert(top_count == 2 and bottom_count == 2, "collapsed conversation did not merge independently of unrelated comments")
  move_to("Use the getter.")
  assert(not vim.bo[buffer].modifiable and followup.focused and change.focused, "merged compact answer lost readonly thread focus")
end

return T
