local M = {}
local buffer = require("forge.buffer")
local keymaps = require("forge.shared.keymaps")
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local spinner = require("forge.render.harness.timeline_status")
---@type table<integer, uv.uv_timer_t>
local animation = {}

---Render transient footer content after workflow status without changing timeline text.
---@param transcript table
---@param row integer
---@param width integer
---@param terminal_chunks table[]?
local function render_footer(transcript, row, width, terminal_chunks)
  local lines = {}
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

---@param target_buffer integer
function M.clear(target_buffer)
  local timer = animation[target_buffer]
  if timer then timer:stop() timer:close() animation[target_buffer] = nil end
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
      for _, target in ipairs(node.entry.metadata.target or {}) do
        if target.id:match(":working$") or target.id:match(":question$") or target.id:match(":review%-plan$") then
          cached.block, cached.offset, cached.target = node.id, target.range.start.row, target.id
          break
        end
      end
      if cached.target or not node.id:find(":implementation:", 1, true) then break end
    end
  end
  if not cached.target then return nil end
  local _, source_row = transcript.sequence:position(cached.block)
  return buffer.physical_row(transcript, source_row + cached.offset), cached.target
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
  vim.api.nvim_buf_clear_namespace(target_buffer, namespace, 0, -1)
  local last_row = vim.api.nvim_buf_line_count(target_buffer) - 1
  local row, target
  if transcript.status == "Applied" and not transcript.applying then
    row, target = status_location(transcript)
  else
    M.clear(target_buffer)
  end
  if row and (row < 0 or row > last_row) then
    M.clear(target_buffer)
    buffer.fail_apply(transcript, "Harness status target is outside the committed buffer")
    return
  end
  local working = target and target:match(":working$")
  local notice = transcript.execution_notice or transcript.wait_notice
  if notice then
    M.clear(target_buffer)
    local text = notice:gsub("%s+", " ")
    local capture = transcript.execution_notice and "ForgeHarnessToolFailure" or "ForgeStatusHint"
    if working then
      local rendered = vim.fn.strcharpart(text, 0, math.max(1, width - 1))
      rendered = rendered .. string.rep(" ", math.max(0, width - vim.fn.strdisplaywidth(rendered)))
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
        id = 1, virt_text = { { rendered, capture } },
        virt_text_win_col = 0, priority = 200,
      })
    else
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, last_row, 0, {
        id = 1, virt_lines = { { { text, capture } } },
      })
    end
    return
  end
  local question = target and target:match(":question$")
  local inventory = transcript.background_terminals
  local terminal_count = inventory and inventory.supported and #(inventory.terminal or {}) or 0
  local terminal_text = inventory and inventory.unavailable and "Terminal status unavailable"
    or terminal_count > 0 and ("%d terminal%s running"):format(terminal_count, terminal_count == 1 and "" or "s") or nil
  local terminal_chunks = terminal_text and { { terminal_text, "ForgeStatusHint" } } or nil
  if terminal_count > 0 then append_hint(terminal_chunks, hint_chunks(commands, "terminal_status", width)) end
  if working then
    local session = require("forge.session").harness.session or {}
    local capture = require("forge.infra.highlights").harness_mode(session.execution_mode)
    local text = vim.api.nvim_buf_get_lines(target_buffer, row, row + 1, false)[1]
    vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
      id = 1, sign_text = spinner.frame_at(vim.uv.now()), sign_hl_group = capture,
      end_row = row, end_col = #text, hl_group = capture, priority = 110,
    })
    if not animation[target_buffer] then
      local timer = vim.uv.new_timer()
      animation[target_buffer] = timer
      timer:start(120, 120, vim.schedule_wrap(function()
        if animation[target_buffer] == timer then M.render(transcript, commands, width) end
      end))
    end
  elseif animation[target_buffer] then
    M.clear(target_buffer)
  end
  local context = working and "working_status" or question and "question_status" or target and "review_status"
  if context then
    local chunks = hint_chunks(commands, context, width)
    if working and terminal_chunks then
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
