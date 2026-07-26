local M = {}

---@param value any
---@return string
function M.normalize_body_text(value)
  return (tostring(value or ""):gsub("\r\n", "\n"):gsub("\r", "\n"))
end

---@param body string
---@return string[]
function M.body_lines(body)
  local lines = vim.split(M.normalize_body_text(body), "\n", { plain = true })
  if #lines == 0 then lines = { "" } end
  return lines
end

---@param text string
---@param width integer
---@return string
local function truncate_display(text, width)
  text = tostring(text or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= width then return text end
  local result = ""
  local result_width = 0
  local character_count = vim.fn.strchars(text)
  for character_index = 0, character_count - 1 do
    local character = vim.fn.strcharpart(text, character_index, 1)
    local character_width = vim.fn.strdisplaywidth(character)
    if result_width + character_width > width then break end
    result = result .. character
    result_width = result_width + character_width
  end
  return result
end

---@param win integer?
---@param buf integer?
---@return integer
function M.display_width(win, buf)
  local width = tonumber(vim.o.columns) or 80
  local displayed_win = win
  if not displayed_win or displayed_win <= 0 or not vim.api.nvim_win_is_valid(displayed_win) then
    displayed_win = buf and vim.api.nvim_buf_is_valid(buf) and vim.fn.bufwinid(buf) or nil
  end
  if displayed_win and displayed_win > 0 and vim.api.nvim_win_is_valid(displayed_win) then
    width = vim.api.nvim_win_get_width(displayed_win)
    local wininfo = vim.fn.getwininfo(displayed_win)[1]
    width = width - (tonumber(wininfo and wininfo.textoff) or 0)
  end
  return math.max(40, width - 1)
end

---@param left_text string?
---@param right_text string?
---@param width integer
---@return string
function M.rule_line(left_text, right_text, width)
  left_text = tostring(left_text or "")
  right_text = tostring(right_text or "")
  local fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  if fixed_width > width then
    local available_left_width = math.max(0, width - vim.fn.strdisplaywidth(right_text))
    left_text = truncate_display(left_text, available_left_width)
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  if fixed_width > width then
    right_text = truncate_display(right_text, math.max(0, width - vim.fn.strdisplaywidth(left_text)))
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  return left_text .. ("-"):rep(math.max(width - fixed_width, 0)) .. right_text
end

---@param width integer
---@return string
function M.footer_line(width)
  return ("-"):rep(width)
end

return M
