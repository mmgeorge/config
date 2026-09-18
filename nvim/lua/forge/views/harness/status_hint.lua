local M = {}
local buffer = require("forge.buffer")
local keymaps = require("forge.shared.keymaps")
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local spinner = require("forge.render.harness.timeline_status")
---@type table<integer, uv.uv_timer_t>
local animation = {}

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
  if not (working or location and location.target and location.target:match(":review%-plan$")) then
    M.clear(target_buffer)
    return
  end
  if working then
    local function draw()
      if not vim.api.nvim_buf_is_valid(target_buffer) then M.clear(target_buffer) return end
      vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, 0, {
        id = 1, virt_text = { { spinner.frame_at(vim.uv.now()) .. " ", "ForgeTimelineStatusSpinner" } },
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
  local entries = keymaps.view_hint_entries("harness", commands, {}, working and "working_status" or "review_status")
  if #entries == 0 then return end
  local formatted = keymaps.render_hintbar(entries, width, { inline = true })
  local evaluated = vim.api.nvim_eval_statusline(formatted, { maxwidth = width, highlights = true })
  if evaluated.str == "" then return end
  local chunks = { { working and " · " or "  ", "ForgeStatusHint" } }
  for index, highlight in ipairs(evaluated.highlights) do
    local following = evaluated.highlights[index + 1]
    local finish = following and following.start or #evaluated.str
    if finish > highlight.start then
      chunks[#chunks + 1] = { evaluated.str:sub(highlight.start + 1, finish), highlight.group }
    end
  end
  local line = vim.api.nvim_buf_get_lines(target_buffer, row, row + 1, false)[1]
  vim.api.nvim_buf_set_extmark(target_buffer, namespace, row, working and math.max(0, #line - 1) or 0, {
    virt_text = chunks, virt_text_pos = working and "inline" or "eol", hl_mode = "combine",
  })
end

return M
