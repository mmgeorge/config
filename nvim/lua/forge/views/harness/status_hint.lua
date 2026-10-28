local M = {}
local keymaps = require("forge.shared.keymaps")

---@class HarnessStatusContext
---@field status_notice? table
---@field message_notice? table
---@field restore_recovery? table
---@field rename_status? string
---@field background_terminals? table
---@field recap? table

---Compose complete status text without mutating the native snapshot.
---@param snapshot table
---@param context HarnessStatusContext
---@param commands ForgeViewCommandSet
---@return table
function M.compose(snapshot, context, commands)
  local content = { lines = {}, highlights = {} }
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
  for _, block in ipairs(snapshot.block or {}) do
    local start = #content.lines
    for _, line in ipairs(block.text) do append(line, "ForgeTimelineStatus") end
    local metadata = block.metadata or {}
    for _, decoration in ipairs(metadata.decoration or {}) do
      for row = decoration.range.start.row, decoration.range["end"].row - 1 do
        content.highlights[start + row + 1] = { row = start + row, capture = decoration.capture }
      end
    end
    local status = metadata.status
    if type(status) == "table" then
      local row = start + status.row
      if status.animated then content.animated = row end
      if not status.animated then content.waiting = row end
      local suffix = type(status.hint) == "string" and hints(status.hint .. "_status") or ""
      if suffix ~= "" then content.lines[row + 1] = content.lines[row + 1] .. " · " .. suffix end
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
