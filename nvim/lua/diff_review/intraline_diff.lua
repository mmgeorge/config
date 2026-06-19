local M = {}

local defaults = {
  max_group_size = 8,
  max_line_length = 400,
  min_shared_chars = 3,
}

local function option_value(opts, key)
  if opts and opts[key] ~= nil then return opts[key] end
  return defaults[key]
end

---@param old_code string
---@param new_code string
---@return integer prefix_len
---@return integer suffix_len
function M.common_affix(old_code, new_code)
  old_code = tostring(old_code or "")
  new_code = tostring(new_code or "")
  local old_len = #old_code
  local new_len = #new_code
  local max_prefix = math.min(old_len, new_len)
  local prefix_len = 0
  while prefix_len < max_prefix and old_code:byte(prefix_len + 1) == new_code:byte(prefix_len + 1) do
    prefix_len = prefix_len + 1
  end

  local max_suffix = math.min(old_len - prefix_len, new_len - prefix_len)
  local suffix_len = 0
  while suffix_len < max_suffix
    and old_code:byte(old_len - suffix_len) == new_code:byte(new_len - suffix_len) do
    suffix_len = suffix_len + 1
  end

  return prefix_len, suffix_len
end

---@param old_line DiffReviewParsedHunkLine
---@param new_line DiffReviewParsedHunkLine
---@param opts? table
---@return table?
function M.compact_pair(old_line, new_line, opts)
  if not (old_line and new_line and old_line.prefix == "-" and new_line.prefix == "+") then return nil end
  local old_code = tostring(old_line.code or "")
  local new_code = tostring(new_line.code or "")
  if old_code == new_code then return nil end
  if #old_code > option_value(opts, "max_line_length") or #new_code > option_value(opts, "max_line_length") then
    return nil
  end

  local prefix_len, suffix_len = M.common_affix(old_code, new_code)
  local old_mid_start = prefix_len + 1
  local old_mid_end = #old_code - suffix_len
  local new_mid_start = prefix_len + 1
  local new_mid_end = #new_code - suffix_len
  local old_mid = old_mid_start <= old_mid_end and old_code:sub(old_mid_start, old_mid_end) or ""
  local new_mid = new_mid_start <= new_mid_end and new_code:sub(new_mid_start, new_mid_end) or ""
  if old_mid == "" and new_mid == "" then return nil end
  if old_mid ~= "" and new_mid ~= "" then return nil end

  local changed_text = old_mid ~= "" and old_mid or new_mid
  if not changed_text:match("%S") then return nil end
  if (prefix_len + suffix_len) < option_value(opts, "min_shared_chars") then return nil end

  if new_mid ~= "" then
    return {
      kind = "replacement",
      display_line = new_line,
      old_lines = { old_line },
      new_lines = { new_line },
      diff_lines = { old_line, new_line },
      inline_spans = {
        {
          start_col = new_mid_start - 1,
          end_col = new_mid_end,
          hl_group = "DiffReviewInlineAddBg",
          kind = "add",
        },
      },
    }
  end

  return {
    kind = "replacement",
    display_line = old_line,
    old_lines = { old_line },
    new_lines = { new_line },
    diff_lines = { old_line, new_line },
    inline_spans = {
      {
        start_col = old_mid_start - 1,
        end_col = old_mid_end,
        hl_group = "DiffReviewInlineDeleteBg",
        kind = "delete",
      },
    },
  }
end

local function append_line_items(items, lines, start_index, end_index)
  for index = start_index, end_index do
    items[#items + 1] = { kind = "line", line = lines[index] }
  end
end

---@param lines DiffReviewParsedHunkLine[]
---@param opts? table
---@return table[]
function M.compact_hunk_lines(lines, opts)
  local items = {}
  local index = 1
  local max_group_size = option_value(opts, "max_group_size")

  while index <= #lines do
    local line = lines[index]
    if line and line.prefix == "-" then
      local delete_start = index
      while index <= #lines and lines[index].prefix == "-" do
        index = index + 1
      end
      local delete_end = index - 1
      local add_start = index
      while index <= #lines and lines[index].prefix == "+" do
        index = index + 1
      end
      local add_end = index - 1
      local delete_count = delete_end - delete_start + 1
      local add_count = add_end >= add_start and (add_end - add_start + 1) or 0

      if add_count == delete_count and add_count > 0 and add_count <= max_group_size then
        for offset = 0, delete_count - 1 do
          local old_line = lines[delete_start + offset]
          local new_line = lines[add_start + offset]
          local replacement = M.compact_pair(old_line, new_line, opts)
          if replacement then
            items[#items + 1] = replacement
          else
            items[#items + 1] = { kind = "line", line = old_line }
            items[#items + 1] = { kind = "line", line = new_line }
          end
        end
      else
        append_line_items(items, lines, delete_start, delete_end)
        if add_count > 0 then append_line_items(items, lines, add_start, add_end) end
      end
    else
      items[#items + 1] = { kind = "line", line = line }
      index = index + 1
    end
  end

  return items
end

return M
