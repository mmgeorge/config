local M = {}

---@class DiffReviewDisplayTextSplit
---@field prefix string
---@field remainder string

---Split the largest display-width prefix without breaking a character.
---@param value string
---@param width integer
---@return DiffReviewDisplayTextSplit
function M.take_prefix(value, width)
  if width <= 0 or value == "" then return { prefix = "", remainder = value } end
  local prefix = ""
  local character_count = vim.fn.strchars(value)
  for character_index = 0, character_count - 1 do
    local character = vim.fn.strcharpart(value, character_index, 1)
    if vim.fn.strdisplaywidth(prefix .. character) > width then
      return { prefix = prefix, remainder = vim.fn.strcharpart(value, character_index) }
    end
    prefix = prefix .. character
  end
  return { prefix = prefix, remainder = "" }
end

---Wrap text into display-width rows with semantic first and continuation prefixes.
---@param text string
---@param width integer?
---@param first_prefix string?
---@param continuation_prefix string?
---@return string[]
function M.wrap(text, width, first_prefix, continuation_prefix)
  local maximum_width = width or math.huge
  local initial_prefix = first_prefix or ""
  local following_prefix = continuation_prefix or initial_prefix
  local line_list = {}
  local prefix = initial_prefix

  for _, paragraph in ipairs(vim.split(tostring(text or ""), "\n", { plain = true })) do
    if vim.trim(paragraph) == "" then
      line_list[#line_list + 1] = prefix
      prefix = following_prefix
    else
      local current = prefix
      for original_word in paragraph:gmatch("%S+") do
        local word = original_word
        local separator = current == prefix and "" or " "
        if current ~= prefix and vim.fn.strdisplaywidth(current .. separator .. word) > maximum_width then
          line_list[#line_list + 1] = current
          prefix = following_prefix
          current = prefix
          separator = ""
        end
        local available_width = math.max(1, maximum_width - vim.fn.strdisplaywidth(current .. separator))
        while vim.fn.strdisplaywidth(word) > available_width do
          local split = M.take_prefix(word, available_width)
          if split.prefix == "" then break end
          line_list[#line_list + 1] = current .. separator .. split.prefix
          prefix = following_prefix
          current = prefix
          separator = ""
          word = split.remainder
          available_width = math.max(1, maximum_width - vim.fn.strdisplaywidth(prefix))
        end
        current = current .. separator .. word
      end
      line_list[#line_list + 1] = current
      prefix = following_prefix
    end
  end

  if #line_list == 0 then line_list[1] = initial_prefix end
  return line_list
end

return M
