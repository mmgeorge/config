local M = {}

---Shorten workspace file labels with Snacks while retaining other Blink labels.
---@param context blink.cmp.DrawItemContext
---@return string
function M.text(context)
  local default = require("blink.cmp.config.completion.menu").default.draw.components.label
  if context.source_id ~= "harness_files" then return default.text(context) end

  local harness = require("forge.session").harness
  local root = harness.session and harness.session.workspace or vim.fn.getcwd()
  local path = vim.fs.joinpath(root, context.item.label:sub(2))
  local maximum = context.self.components.label.width.max or 60
  -- Reserve the menu padding, file icon, column gap, and scrollbar.
  local width = math.max(1, math.min(maximum, vim.api.nvim_win_get_width(0) - 6, vim.o.columns - 6))
  return "@" .. require("snacks").picker.util.truncpath(path, width - 1, { cwd = root, kind = "center" })
end

---Map fuzzy highlights onto the retained prefix and suffix of a shortened label.
---@param context blink.cmp.DrawItemContext
---@param text string
---@return blink.cmp.DrawHighlight[]
function M.highlight(context, text)
  local default = require("blink.cmp.config.completion.menu").default.draw.components.label
  if context.source_id ~= "harness_files" then return default.highlight(context) end

  local original = context.item.label
  local prefix_length = 0
  while prefix_length < math.min(#original, #text)
    and original:byte(prefix_length + 1) == text:byte(prefix_length + 1) do
    prefix_length = prefix_length + 1
  end
  local suffix_length = 0
  while suffix_length < math.min(#original, #text) - prefix_length
    and original:byte(#original - suffix_length) == text:byte(#text - suffix_length) do
    suffix_length = suffix_length + 1
  end
  local matched = {}
  for _, index in ipairs(context.label_matched_indices) do
    if index < prefix_length then
      matched[#matched + 1] = index
    elseif index >= #original - suffix_length then
      matched[#matched + 1] = #text - #original + index
    end
  end
  local display = vim.tbl_extend("force", context, {
    label = text,
    label_detail = "",
    label_matched_indices = matched,
  })
  return default.highlight(display)
end

return M
