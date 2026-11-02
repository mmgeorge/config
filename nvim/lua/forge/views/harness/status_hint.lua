local M = {}
local keymaps = require("forge.shared.keymaps")
local display_text = require("forge.render.display_text")

---@class HarnessStatusContext
---@field status_notice? table
---@field message_notice? table
---@field restore_recovery? table
---@field rename_status? string
---@field background_terminals? table
---@field recap? table
---@field questions? ForgeQuestionView

---Compose complete status text without mutating the native snapshot.
---@param snapshot table
---@param context HarnessStatusContext
---@param commands ForgeViewCommandSet
---@param width? integer Available display columns, including status indentation.
---@return table
function M.compose(snapshot, context, commands, width)
  local content = { lines = {}, highlights = {} }
  local pending_questions = {}
  for _, question in ipairs((context.questions or {}).questions or {}) do
    if question.resolution.kind == "pending" then pending_questions[#pending_questions + 1] = question end
  end
  local questions_drawn = false
  local function hints(surface)
    local text = {}
    for _, entry in ipairs(keymaps.view_hint_entries("harness", commands, {}, surface)) do
      text[#text + 1] = entry.key .. " " .. entry.label
    end
    return table.concat(text, " · ")
  end
  local function append(text, capture)
    for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
      content.lines[#content.lines + 1] = "  " .. line
      content.highlights[#content.highlights + 1] = { row = #content.lines - 1, capture = capture }
    end
  end
  ---Append two-line question previews without changing their full picker content.
  local function append_questions()
    if questions_drawn or #pending_questions == 0 then return end
    questions_drawn = true
    local preview_width = math.max(1, (width or vim.o.columns) - 2)
    for index, question in ipairs(pending_questions) do
      local last = index == #pending_questions
      local prefix = last and "└─ " or "├─ "
      local continuation = last and "   " or "│  "
      local lines = display_text.wrap(question.content.question:gsub("%s+", " "), preview_width, prefix, continuation)
      if #lines > 2 then
        lines[2] = display_text.take_prefix(lines[2], preview_width - 1).prefix:gsub("%s+$", "") .. "…"
      end
      for line_index = 1, math.min(2, #lines) do append(lines[line_index], "ForgeStatusHint") end
    end
  end
  for _, block in ipairs(snapshot.block or {}) do
    local metadata = block.metadata or {}
    local status = metadata.status
    local rendered_rows = {}
    for index, line in ipairs(block.text) do
      local row = #content.lines
      rendered_rows[index] = row
      append(line, "ForgeTimelineStatus")
      if type(status) == "table" and index == status.row + 1 then
        if status.animated then content.animated = row else content.waiting = row end
        local surface = status.hint
        if surface == "question" and #pending_questions > 0 then surface = "pending_question" end
        local suffix = type(surface) == "string" and hints(surface .. "_status") or ""
        if not questions_drawn and #pending_questions > 0 and surface ~= "pending_question" and surface ~= "permission" then
          local question_hint = hints("pending_question_status")
          if question_hint ~= "" then suffix = suffix .. (suffix ~= "" and " · " or "") .. question_hint end
        end
        if suffix ~= "" then content.lines[row + 1] = content.lines[row + 1] .. " · " .. suffix end
        append_questions()
      end
    end
    for _, decoration in ipairs(metadata.decoration or {}) do
      for row = decoration.range.start.row, decoration.range["end"].row - 1 do
        local rendered_row = rendered_rows[row + 1]
        content.highlights[rendered_row + 1] = { row = rendered_row, capture = decoration.capture }
      end
    end
  end
  local notice = context.status_notice
  if notice then
    content.animated, content.waiting = nil, nil
    local row = #content.lines
    local suffix = notice.hint and hints(notice.hint .. "_status") or ""
    append(notice.text .. (suffix ~= "" and " · " .. suffix or ""), notice.failed and "ForgeHarnessToolFailure" or "ForgeStatusHint")
    if notice.animated then content.animated = row end
    if notice.waiting then content.waiting = row end
  end
  if not questions_drawn and #pending_questions > 0 then
    local suffix = hints("pending_question_status")
    append("Questions" .. (suffix ~= "" and " · " .. suffix or ""), "ForgeStatusHint")
    append_questions()
  end
  if context.message_notice then append(context.message_notice.text, context.message_notice.failed and "ForgeHarnessToolFailure" or "ForgeStatusHint") end
  if context.restore_recovery then append("Restore interrupted. Open Harness undo to continue or recover pre-restore files.", "ErrorMsg") end
  if context.rename_status then append(context.rename_status, "ForgeStatusHint") end
  local inventory = context.background_terminals
  local count = inventory and inventory.supported and #(inventory.terminal or {}) or 0
  local terminal = inventory and inventory.unavailable and "Terminal status unavailable"
    or inventory and inventory.starting and "Terminal status: starting"
    or count > 0 and ("%d terminal%s running"):format(count, count == 1 and "" or "s") or nil
  if terminal then
    local suffix = count > 0 and hints("terminal_status") or ""
    append(terminal .. (suffix ~= "" and " · " .. suffix or ""), "ForgeStatusHint")
  end
  if context.recap then
    local text = context.recap.loading and "Loading..." or context.recap.text
    if text then append("Recap: " .. text, "ForgeHarnessRecap") end
  end
  return content
end

return M
