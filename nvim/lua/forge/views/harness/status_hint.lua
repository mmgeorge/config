local M = {}
local buffer = require("forge.buffer")
local keymaps = require("forge.shared.keymaps")
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local spinner = require("forge.render.harness.timeline_status")
---@class HarnessStatusAnimation
---@field timer uv.uv_timer_t
---@field row integer
---@field options table
---@field revision integer
---@field sequence table
---@type table<integer, HarnessStatusAnimation>
local animation = {}

---Render transient footer content after workflow status without changing timeline text.
---@param transcript table
---@param row integer
---@param width integer
---@param terminal_chunks table[]?
local function render_footer(transcript, row, width, terminal_chunks)
  local lines = {}
  if transcript.restore_recovery then
    lines[#lines + 1] = { { "Restore interrupted. Open Harness undo to continue or recover pre-restore files.", "ErrorMsg" } }
  end
  if transcript.rename_status then
    lines[#lines + 1] = { { transcript.rename_status, "ForgeStatusHint" } }
  end
  if terminal_chunks then
    local text = {}
    for _, chunk in ipairs(terminal_chunks) do text[#text + 1] = chunk[1] end
    for _, line in ipairs(require("forge.render.display_text").wrap(table.concat(text), width, "", "")) do
      lines[#lines + 1] = { { line, "ForgeStatusHint" } }
    end
  end
  if transcript.recap then
    local text = transcript.recap.loading and "Loading..." or transcript.recap.text
    if text then
      lines[#lines + 1] = { { "", "ForgeStatusHint" } }
      for index, line in ipairs(require("forge.render.display_text").wrap(text, width, "Recap: ", "       ")) do
        lines[#lines + 1] = {
          { index == 1 and "Recap: " or "       ", "ForgeStatusHint" },
          { line:sub(8), "ForgeHarnessRecap" },
        }
      end
    end
  end
  if #lines > 0 then
    vim.api.nvim_buf_set_extmark(transcript.buffer, namespace, row + 1, 0, {
      id = 3, virt_lines = lines, virt_lines_above = true,
    })
  end
end

local function stop_animation(target_buffer)
  local owner = animation[target_buffer]
  if owner then
    owner.timer:stop()
    owner.timer:close()
    animation[target_buffer] = nil
  end
end

---@param target_buffer integer
function M.clear(target_buffer)
  stop_animation(target_buffer)
  if vim.api.nvim_buf_is_valid(target_buffer) then
    vim.api.nvim_buf_clear_namespace(target_buffer, namespace, 0, -1)
  end
end

local function status_location(transcript)
  local cached = transcript.status_hint_location
  if not cached or cached.revision ~= transcript.revision or cached.sequence ~= transcript.sequence then
    cached = { revision = transcript.revision, sequence = transcript.sequence }
    transcript.status_hint_location = cached
    for index = transcript.sequence:count() - 1, 0, -1 do
      local node = transcript.sequence:at(index)
      local status = node.entry.metadata.status
      if status and status ~= vim.NIL then
        cached.block, cached.offset, cached.status = node.id, status.row, status
        break
      end
      if not node.id:find(":implementation:", 1, true) then break end
    end
  end
  if not cached.status then return nil end
  local _, source_row = transcript.sequence:position(cached.block)
  return buffer.physical_row(transcript, source_row + cached.offset), cached.status
end

local function animate(transcript, row, options)
  local target_buffer = transcript.buffer
  stop_animation(target_buffer)
  local owner = { row = row, options = options, revision = transcript.revision, sequence = transcript.sequence }
  local function draw()
    if animation[target_buffer] ~= owner then return end
    if not vim.api.nvim_buf_is_valid(target_buffer) or transcript.status ~= "Applied" or transcript.revision ~= owner.revision
      or transcript.sequence ~= owner.sequence then
      stop_animation(target_buffer)
      if vim.api.nvim_buf_is_valid(target_buffer) then
        vim.api.nvim_buf_del_extmark(target_buffer, namespace, 1)
      end
      return
    end
    local position = vim.api.nvim_buf_get_extmark_by_id(target_buffer, namespace, 1, {})
    if #position > 0 then owner.row = position[1] end
    if owner.options.end_row then owner.options.end_row = owner.row end
    owner.options.sign_text = spinner.frame_at(vim.uv.now())
    vim.api.nvim_buf_set_extmark(target_buffer, namespace, owner.row, 0, owner.options)
  end
  owner.timer = vim.uv.new_timer()
  animation[target_buffer] = owner
  draw()
  owner.timer:start(120, 120, vim.schedule_wrap(draw))
end

local function hint_chunks(commands, context, width)
  local entries = keymaps.view_hint_entries("harness", commands, {}, context)
  local formatted = keymaps.render_hintbar(entries, width, { inline = true })
  local evaluated = vim.api.nvim_eval_statusline(formatted, { maxwidth = width, highlights = true })
  local chunks = {}
  for index, highlight in ipairs(evaluated.highlights) do
    local following = evaluated.highlights[index + 1]
    local finish = following and following.start or #evaluated.str
    if finish > highlight.start then
      chunks[#chunks + 1] = { evaluated.str:sub(highlight.start + 1, finish), highlight.group }
    end
  end
  return chunks
end

local function append_hint(chunks, extra)
  if #extra == 0 then return end
  if #chunks > 0 then chunks[#chunks + 1] = { " · ", "ForgeStatusHint" } end
  vim.list_extend(chunks, extra)
end

---@param transcript table
---@param commands table
---@param width integer
function M.render(transcript, commands, width)
  local target_buffer = transcript.buffer
  if not vim.api.nvim_buf_is_valid(target_buffer) then M.clear(target_buffer) return end
  M.clear(target_buffer)
  local last_row = vim.api.nvim_buf_line_count(target_buffer) - 1
  local row, status
  if transcript.status == "Applied" and not transcript.applying then
    row, status = status_location(transcript)
  end
  if row and (row < 0 or row > last_row) then
    buffer.fail_apply(transcript, "Harness status target is outside the committed buffer")
    return
  end
  local notice = transcript.status_notice
  if notice then
    local text = notice.text:gsub("%s+", " ")
    local capture = notice.failed and "ForgeHarnessToolFailure" or "ForgeStatusHint"
    if row then
      local rendered = vim.fn.strcharpart(text, 0, math.max(1, width - 1))
      rendered = rendered .. string.rep(" ", math.max(0, width - vim.fn.strdisplaywidth(rendered)))
      local options = { id = 1, virt_text = { { rendered, capture } },
        virt_text_win_col = 0, priority = 200, sign_hl_group = capture }
      if notice.waiting then options.sign_text = "◷" end
      if notice.animated then animate(transcript, row, options)
      else vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, options) end
    else
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, last_row, 0, {
        id = 1, virt_lines = { { { (notice.waiting and "◷ " or "") .. text, capture } } },
      })
    end
    return
  end
  local inventory = transcript.background_terminals
  local terminal_count = inventory and inventory.supported and #(inventory.terminal or {}) or 0
  local terminal_text = inventory and inventory.unavailable and "Terminal status unavailable"
    or terminal_count > 0 and ("%d terminal%s running"):format(terminal_count, terminal_count == 1 and "" or "s") or nil
  local terminal_chunks = terminal_text and { { terminal_text, "ForgeStatusHint" } } or nil
  if terminal_count > 0 then append_hint(terminal_chunks, hint_chunks(commands, "terminal_status", width)) end
  local context = status and status.hint ~= vim.NIL and status.hint or nil
  if row and status.animated then
    local session = require("forge.session").harness.session or {}
    local capture = require("forge.infra.highlights").harness_mode(session.execution_mode)
    local text = vim.api.nvim_buf_get_lines(target_buffer, row, row + 1, false)[1]
    animate(transcript, row, {
      id = 1, sign_hl_group = capture,
      end_row = row, end_col = #text, hl_group = capture, priority = 110,
    })
  end
  if row and not status.animated and (context == "question" or context == "review") then
    vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
      id = 1, sign_text = "◷", sign_hl_group = "ForgeStatusHint", priority = 110,
    })
  end
  if context then
    local chunks = hint_chunks(commands, context .. "_status", width)
    if context == "working" and terminal_chunks then
      append_hint(terminal_chunks, chunks)
    elseif #chunks > 0 then
      table.insert(chunks, 1, { " · ", "ForgeStatusHint" })
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
        id = 2, virt_text = chunks, virt_text_pos = "eol", hl_mode = "combine",
      })
    end
  end
  render_footer(transcript, last_row, width, terminal_chunks)
end

return M
