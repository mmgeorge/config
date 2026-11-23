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
  local content = { lines = {}, highlights = {}, spinners = {} }
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
  local notice = context.status_notice
  ---Append the local notice, which takes the status row's icon because it explains that row.
  local function append_notice()
    if not notice then return end
    content.animated, content.waiting = nil, nil
    local row = #content.lines
    local suffix = notice.hint and hints(notice.hint .. "_status") or ""
    append(notice.text .. (suffix ~= "" and " · " .. suffix or ""), notice.failed and "ForgeHarnessToolFailure" or "ForgeStatusHint")
    if notice.animated then content.animated = row end
    if notice.waiting then content.waiting = row end
    notice = nil
  end
  for _, block in ipairs(snapshot.block or {}) do
    local metadata = block.metadata or {}
    local status = metadata.status
    local rendered_rows = {}
    local column_shift = {}
    local thought_rows, thought = {}, {}
    if type(status) == "table" then
      for _, decoration in ipairs(metadata.decoration or {}) do
        if decoration.capture == "ForgeHarnessCommentary" then
          for row = decoration.range.start.row, decoration.range["end"].row - 1 do
            thought_rows[row + 1] = true
            thought[#thought + 1] = block.text[row + 1]
          end
        end
      end
    end
    for index, line in ipairs(block.text) do
      if not thought_rows[index] then
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
          local preview = table.concat(thought, " ")
          local available = (width or vim.o.columns) - vim.fn.strdisplaywidth(content.lines[row + 1] .. " │ ")
          if preview ~= "" and available > 0 then
            local split = display_text.take_prefix(preview, available)
            if split.remainder ~= "" then
              preview = display_text.take_prefix(preview, available - vim.fn.strdisplaywidth("…")).prefix .. "…"
            end
            local column = #content.lines[row + 1]
            content.lines[row + 1] = content.lines[row + 1] .. " │ " .. preview
            content.highlights[#content.highlights + 1] = {
              row = row, column = column, end_column = #content.lines[row + 1], capture = "ForgeHarnessCommentary",
            }
          end
        end
      end
    end
    if type(status) == "table" and #pending_questions > 0 and not questions_drawn then
      local final_branch
      for index, line in ipairs(block.text) do
        if rendered_rows[index] and line:sub(1, #"└─ ") == "└─ " then final_branch = index end
      end
      if final_branch then
        local row = rendered_rows[final_branch]
        content.lines[row + 1] = content.lines[row + 1]:gsub("^  └─ ", "  ├─ ", 1)
        for index = final_branch + 1, #block.text do
          local child_row = rendered_rows[index]
          if child_row and block.text[index]:sub(1, 3) == "   " then
            content.lines[child_row + 1] = content.lines[child_row + 1]:gsub("^     ", "  │  ", 1)
            column_shift[child_row] = #"│" - 1
          end
        end
      end
      append_questions()
    end
    -- A running child's reasoning stays on its own row, clipped to the window so the status
    -- split never grows; the host leaves these rows unwrapped so the column is exact.
    for _, decoration in ipairs(metadata.decoration or {}) do
      local rendered_row = rendered_rows[decoration.range.start.row + 1]
      if decoration.capture == "ForgeHarnessAgentThought" and rendered_row then
        local line = content.lines[rendered_row + 1]
        local column = decoration.range.start.column + 2 + (column_shift[rendered_row] or 0)
        local head, thought = line:sub(1, column), line:sub(column + 1)
        local available = (width or vim.o.columns) - vim.fn.strdisplaywidth(head)
        if available <= vim.fn.strdisplaywidth("…") then
          content.lines[rendered_row + 1] = (head:gsub(" │ $", ""))
        else
          if display_text.take_prefix(thought, available).remainder ~= "" then
            thought = display_text.take_prefix(thought, available - vim.fn.strdisplaywidth("…")).prefix .. "…"
          end
          content.lines[rendered_row + 1] = head .. thought
          content.highlights[#content.highlights + 1] = {
            row = rendered_row, column = column, end_column = #content.lines[rendered_row + 1],
            capture = "ForgeHarnessAgentThought",
          }
        end
      end
    end
    for _, decoration in ipairs(metadata.decoration or {}) do
      if decoration.capture == "ForgeTimelineStatusSpinner" then
        local rendered_row = rendered_rows[decoration.range.start.row + 1]
        if rendered_row then
          local column = decoration.range.start.column + 2 + (column_shift[rendered_row] or 0)
          content.spinners[#content.spinners + 1] = { row = rendered_row, column = column }
        end
      end
      for row = decoration.range.start.row, decoration.range["end"].row - 1 do
        local rendered_row = decoration.capture ~= "ForgeHarnessAgentThought" and rendered_rows[row + 1]
        if rendered_row then
          for _, highlight in ipairs(content.highlights) do
            if highlight.row == rendered_row and highlight.column == nil then highlight.capture = decoration.capture break end
          end
        end
      end
    end
    -- The notice explains the phase row, so it follows that block. Appended after every block,
    -- it sat below the staged input and moved whenever a queued row came or went.
    if type(status) == "table" then append_notice() end
  end
  append_notice()
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
