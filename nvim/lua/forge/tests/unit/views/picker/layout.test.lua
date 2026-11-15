local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["expanded content keeps diff styling through wrapping and viewport projection"] = function()
  local layout = require("forge.views.picker.layout")
  local text = "+ " .. string.rep("λ", 90)
  local frame = layout.build({ option_list = { { label = "Modified check", child_content_list = {
    { text = text, preformatted = true, group = "ForgeAddBg", spans = {
      { first = 0, last = #text, group = "ForgeAddLineNr" },
    } },
  } } } }, 1, 30)
  local projected = layout.viewport(frame, 8)
  eq(#frame.content_span_list > 1, true)
  eq(#projected.content_span_list > 0, true)
  for _, span in ipairs(projected.content_span_list) do
    eq(span.last <= #projected.lines[span.line], true)
    eq(span.group, "ForgeAddLineNr")
  end
  eq(projected.content_range[1].group, "ForgeAddBg")
  eq(projected.content_range[1].priority, 110)
end

T["detail fields align nested labels and list continuations"] = function()
  local layout = require("forge.views.picker.layout")
  local frame = layout.build({ option_list = { { label = "Check", child_field_list = {
    { label = "adapter", value = "script" },
    { label = "  command", value = "nu  checks.nu" },
    { label = "paths", value = "lua/**" },
    { label = "", value = "tests/**" },
  } } } }, 1, 80)
  eq(vim.list_slice(frame.lines, 2, frame.body_end), {
    "    adapter    script",
    "      command  nu  checks.nu",
    "    paths      lua/**",
    "               tests/**",
  })
  eq(frame.primary_range[1].last, 1)
  eq(frame.child_range[1], { first = 2, last = 5 })
end

T["detail values wrap without losing unicode or moving their column"] = function()
  local layout = require("forge.views.picker.layout")
  local value = string.rep("界", 18)
  local frame = layout.build({ option_list = { { label = "Check", child_field_list = {
    { label = "paths", value = value },
  } } } }, 1, 30)
  local fragments = {}
  for _, line in ipairs(vim.list_slice(frame.lines, 2, frame.body_end)) do
    eq(vim.fn.strdisplaywidth(line) <= 26, true)
    fragments[#fragments + 1] = line:sub(12)
  end
  eq(table.concat(fragments), value)
  eq(frame.lines[2]:sub(1, 11), "    paths  ")
  eq(frame.lines[3]:sub(1, 11), string.rep(" ", 11))
end

T["long detail labels stack above values in narrow pickers"] = function()
  local layout = require("forge.views.picker.layout")
  local value = "all-content-remains-visible"
  local frame = layout.build({ option_list = { { label = "Check", child_field_list = {
    { label = "  unusually_long_field_name", value = value },
  } } } }, 1, 24)
  local content = {}
  for _, line in ipairs(vim.list_slice(frame.lines, 2, frame.body_end)) do
    eq(vim.fn.strdisplaywidth(line) <= 20, true)
    content[#content + 1] = line:gsub("^%s+", "")
  end
  eq(table.concat(content), "unusually_long_field_name" .. value)
end

T["ordinary child lines retain their existing rendering"] = function()
  local layout = require("forge.views.picker.layout")
  local frame = layout.build({ option_list = { { label = "Check", child_line_list = { "Old detail" } } } }, 1, 80)
  eq(frame.lines[2], "    Old detail")
  eq(frame.child_range[1], { first = 2, last = 2 })
end

return T
