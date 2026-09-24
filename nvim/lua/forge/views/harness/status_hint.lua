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
---@param terminal_text string?
local function render_footer(transcript, row, width, terminal_text)
  local lines = {}
  if transcript.rename_status then
    lines[#lines + 1] = { { transcript.rename_status, "ForgeStatusHint" } }
  end
  if terminal_text then
    lines[#lines + 1] = { { "", "ForgeStatusHint" } }
    lines[#lines + 1] = { { terminal_text, "ForgeStatusHint" } }
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
    vim.api.nvim_buf_set_extmark(transcript.buffer, namespace, math.max(0, row), 0, { id = 3, virt_lines = lines })
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

---@param transcript table
---@param commands table
---@param width integer
function M.render(transcript, commands, width)
  local target_buffer = transcript.buffer
  if not vim.api.nvim_buf_is_valid(target_buffer) then M.clear(target_buffer) return end
  vim.api.nvim_buf_clear_namespace(target_buffer, namespace, 0, -1)
  local row = vim.api.nvim_buf_line_count(target_buffer) - 1
  local location = buffer.locate(transcript, row, 0)
  local working = location and location.target and location.target:match(":working$")
  local question = location and location.target and location.target:match(":question$")
  local inventory = transcript.background_terminals
  local terminal_count = inventory and inventory.supported and #(inventory.terminal or {}) or 0
  local terminal_text = inventory and inventory.unavailable and "Background terminal status unavailable"
    or terminal_count > 0 and ("%d background terminal%s"):format(terminal_count, terminal_count == 1 and "" or "s") or nil
  if not (working or question or location and location.target and location.target:match(":review%-plan$")) then
    M.clear(target_buffer)
    render_footer(transcript, row, width, terminal_text and (terminal_text .. (terminal_count > 0 and " running" or "")))
    return
  end
  if working then
    local session = require("forge.session").harness.session or {}
    local capture = require("forge.infra.highlights").harness_mode(session.mode or session.execution_mode)
    local text = vim.api.nvim_buf_get_lines(target_buffer, row, row + 1, false)[1]
    local function draw()
      if not vim.api.nvim_buf_is_valid(target_buffer) then M.clear(target_buffer) return end
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
        id = 1, virt_text = { { spinner.frame_at(vim.uv.now()) .. " ", capture } },
        end_row = row, end_col = #text, hl_group = capture, priority = 110,
        virt_text_pos = "inline", hl_mode = "combine",
      })
    end
    draw()
    if not animation[target_buffer] then
      local timer = vim.uv.new_timer()
      animation[target_buffer] = timer
      timer:start(120, 120, vim.schedule_wrap(function()
        if animation[target_buffer] == timer then
          M.render(transcript, commands, width)
        end
      end))
    end
  elseif animation[target_buffer] then
    M.clear(target_buffer)
  end
  render_footer(transcript, row, width)
  local context = working and "working_status" or question and "question_status" or "review_status"
  local entries = keymaps.view_hint_entries("harness", commands, {}, context)
  if #entries == 0 and not terminal_text then return end
  local formatted = keymaps.render_hintbar(entries, width, { inline = true })
  local evaluated = vim.api.nvim_eval_statusline(formatted, { maxwidth = width, highlights = true })
  if evaluated.str == "" and not terminal_text then return end
  local chunks = { { working and " · " or "  ", "ForgeStatusHint" } }
  if terminal_text then
    chunks[#chunks + 1] = { terminal_text .. (evaluated.str ~= "" and " · " or ""), "ForgeStatusHint" }
  end
  for index, highlight in ipairs(evaluated.highlights) do
    local following = evaluated.highlights[index + 1]
    local finish = following and following.start or #evaluated.str
    if finish > highlight.start then
      chunks[#chunks + 1] = { evaluated.str:sub(highlight.start + 1, finish), highlight.group }
    end
  end
  local line = vim.api.nvim_buf_get_lines(target_buffer, row, row + 1, false)[1]
  vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, working and math.max(0, #line - 1) or 0, {
    id = 2, virt_text = chunks, virt_text_pos = working and "inline" or "eol", hl_mode = "combine",
  })
end

return M
