vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
vim.o.columns = 130
local approval = require("forge.views.harness.approval")
local picker = require("forge.views.picker")
local command_detail = require("forge.views.harness.command_detail")
require("forge.infra.highlights").setup()

---@param source string
---@param fragment string
---@return ForgeApprovalCommand
local function context(source, fragment)
  local start, final = assert(source:find(fragment, 1, true))
  return {
    source = source, shell = "powershell",
    focus_range_list = { { start = start - 1, ["end"] = final } },
    highlight_list = { { range = { start = start - 1, ["end"] = final }, group = "ForgeHarnessCommand", priority = 1 } },
  }
end

---@param instance table
---@param group string
---@return string
local function marked_text(instance, group)
  local result = {}
  for _, span in ipairs(instance.frame.content_span_list) do
    if span.group == group then result[#result + 1] = instance.frame.lines[span.line]:sub(span.first + 1, span.last) end
  end
  return table.concat(result)
end

---@param key string
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(type(mapping.callback) == "function", "missing picker binding: " .. key)
  mapping.callback()
end

---@param choice_id string
local function choose(choice_id)
  local instance = picker._state_for_test()
  local page = instance.spec.page_list[instance.state.page_index]
  for _, option in ipairs(page.option_list) do
    if option.value == choice_id then
      invoke(option.key)
      if not option.confirm_on_key then invoke("<CR>") end
      return
    end
  end
  error("missing choice " .. choice_id)
end

---@type ForgeApprovalRequest
local request = { id = "approval", item_list = {} }
local complete_command = "rustc --version; cargo --version; rustup show active-toolchain"
for index, command in ipairs({ "rustc --version", "cargo --version", "rustup show active-toolchain" }) do
  request.item_list[index] = {
    id = tostring(index), title = "Run command", detail = command,
    command_list = { context(complete_command, command) },
    choice_list = {
      { id = "allow_once", label = "Allow once" },
      { id = "allow_exact", label = "Always allow exact: shell " .. command },
      { id = "deny_once", label = "Deny once" },
      { id = "cancel", label = "Reject all" },
    },
  }
end

local ok, failure = pcall(function()
  local interrupted, closed, submitted = 0, 0, 0
  local accepted = false
  ---@type ForgeApprovalHost
  local host = {
    transcript_win = vim.api.nvim_get_current_win(),
    interrupt = function() interrupted = interrupted + 1 end,
    closed = function() closed = closed + 1 end,
    resolve = function(id, answer_list, callback)
      submitted = submitted + 1
      assert(id == request.id)
      assert(vim.deep_equal(answer_list, {
        { item_id = "1", choice_id = "allow_exact" },
        { item_id = "2", choice_id = "allow_once" },
        { item_id = "3", choice_id = "deny_once" },
      }), "each selected scope must remain attached to its command")
      callback(accepted)
    end,
  }
  approval.open(request, host)
  local instance = picker._state_for_test()
  assert(#instance.spec.page_list == 3, "each unapproved command needs its own page")
  local text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(text:find("rustc --version", 1, true))
  assert(text:find("cargo --version", 1, true) and text:find("rustup show active-toolchain", 1, true), "the original command context must remain visible")
  assert(not text:find("Run command", 1, true), "command permissions need no redundant label")
  assert(marked_text(instance, "ForgePermissionTarget") == "rustc --version")
  local target_style = vim.api.nvim_get_hl(0, { name = "ForgePermissionTarget", link = false })
  assert(target_style.underline and not target_style.fg, "the approval marker must retain syntax foreground colors")
  local highlighted = 0
  local base_priority, syntax_priority
  for _, namespace in pairs(vim.api.nvim_get_namespaces()) do
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(instance.buf, namespace, 0, -1, { details = true })) do
      if mark[4].hl_group == "ForgePermissionTarget" then highlighted = highlighted + 1 end
      if mark[4].hl_group == "ForgePickerText" then base_priority = math.max(base_priority or 0, mark[4].priority) end
      if mark[4].hl_group == "ForgeHarnessCommand" then syntax_priority = math.min(syntax_priority or 65535, mark[4].priority) end
    end
  end
  assert(highlighted > 0, "the real renderer must apply the approval marker")
  assert(base_priority and syntax_priority and base_priority < syntax_priority, "whole-line text cannot override shell syntax colors")
  choose("allow_exact")
  assert(picker._state_for_test().state.page_index == 2)
  assert(marked_text(picker._state_for_test(), "ForgePermissionTarget") == "cargo --version", "paging must move the marker to the current command")
  choose("allow_once")
  assert(picker._state_for_test().state.page_index == 3)
  choose("deny_once")
  assert(submitted == 0, "staged choices must not grant permission")
  assert(picker._state_for_test().spec.page_list[1].id == "review")
  choose("revise")
  assert(picker._state_for_test().state.page_index == 1)
  choose("allow_exact")
  assert(picker._state_for_test().spec.page_list[1].id == "review")
  choose("submit")
  assert(submitted == 1 and approval.is_open(), "failed submission must remain reviewable")
  accepted = true
  choose("submit")
  assert(submitted == 2 and not approval.is_open())

  host.resolve = function(_, answer_list, callback)
    assert(#answer_list == 3)
    for _, answer in ipairs(answer_list) do assert(answer.choice_id == "cancel") end
    callback(true)
  end
  approval.open(request, host)
  choose("allow_exact")
  choose("cancel")
  assert(not approval.is_open(), "cancel must close without saving staged choices")

  host.resolve = function() error("interrupt must not approve or reject the tool") end
  approval.open(request, host)
  local previous_closed = closed
  invoke("<C-c>")
  assert(interrupted == 1 and closed == previous_closed + 1 and not approval.is_open())

  local command = [[ForEach-Object { Get-ChildItem "$_.FullName" -Directory -Filter "bevy*0.19.1"; Write-Output 'done; a|b' }]]
  assert(command_detail.format(context(command, command)).text == table.concat({
    "ForEach-Object {",
    [[  Get-ChildItem "$_.FullName" -Directory -Filter "bevy*0.19.1";]],
    [[  Write-Output 'done; a|b']],
    "}",
  }, "\n"), "formatting must keep quotes, quoted separators, and the closing brace")
  for _, fixture in ipairs({
    { shell = "powershell", source = "foreach ($iteration in 1..1) { whoami.exe /groups }", expected = "foreach ($iteration in 1..1) {\n  whoami.exe /groups\n}" },
    { shell = "nushell", source = "[1] | each {|entry| probe `a path` $entry }", expected = "[1] |\neach {|entry|\n  probe `a path` $entry\n}" },
    { shell = "bash", source = [[probe \{literal\} 'a;b|c'; probe two]], expected = [[probe \{literal\} 'a;b|c';]] .. "\nprobe two" },
    { shell = "zsh", source = [[probe `print 'a;b'`; probe two]], expected = [[probe `print 'a;b'`;]] .. "\nprobe two" },
    { shell = "powershell", source = "probe 'a`'; probe two", expected = "probe 'a`';\nprobe two" },
  }) do
    local command_context = context(fixture.source, fixture.source)
    command_context.shell = fixture.shell
    assert(command_detail.format(command_context).text == fixture.expected, "formatting must respect " .. fixture.shell .. " quoting and closures")
  end
  local long_command = "ForEach-Object {\n"
  for index = 1, 40 do long_command = long_command .. ("  Write-Output 'line %d';\n"):format(index) end
  long_command = long_command .. "}"
  local detail_request = vim.deepcopy(request)
  detail_request.item_list[1].detail = long_command
  detail_request.item_list[1].command_list = { context(long_command, "ForEach-Object") }
  approval.open(detail_request, host)
  instance = picker._state_for_test()
  assert(#instance.frame.lines <= vim.api.nvim_win_get_height(instance.win), "details must fit the popup")
  assert(instance.frame.content_offset == 0)
  local first_selection = instance.state.selected_index_by_page["1"]
  for _ = 1, 40 do invoke("<PageDown>") end
  instance = picker._state_for_test()
  text = table.concat(vim.api.nvim_buf_get_lines(instance.buf, 0, -1, false), "\n")
  assert(text:find("line 40", 1, true) and text:find("\n  }", 1, true), "the end of long commands must be reachable")
  assert(text:find("Allow once", 1, true), "scrolling details must retain permission choices")
  assert(instance.state.selected_index_by_page["1"] == first_selection, "scrolling details must not select a decision")
  invoke("<Right>")
  assert(picker._state_for_test().frame.content_offset == 0, "each command page starts at its first line")
  approval.close()

  local contextual_source = [[Write-Output 'Get-ChildItem λ'; ForEach-Object { Get-ChildItem λ -Directory -Filter 'bevy*0.19.1'; Write-Output 'done' }]]
  local active_fragment = [[Get-ChildItem λ -Directory -Filter 'bevy*0.19.1']]
  local formatted = command_detail.format(context(contextual_source, active_fragment))
  assert(formatted.text:find("Write-Output 'Get-ChildItem λ'", 1, true) and formatted.text:find("Write-Output 'done'", 1, true))
  local layout = require("forge.views.picker.layout")
  for _, width in ipairs({ 30, 60, 128 }) do
    local frame = layout.build({ content_list = { formatted }, option_list = {} }, 1, width)
    local selected, syntax = {}, {}
    for _, span in ipairs(frame.content_span_list) do
      local value = frame.lines[span.line]:sub(span.first + 1, span.last)
      if span.group == "ForgePermissionTarget" then selected[#selected + 1] = value end
      if span.group == "ForgeHarnessCommand" then syntax[#syntax + 1] = value end
    end
    assert(table.concat(selected) == active_fragment, "wrapping must mark only the active executable occurrence")
    assert(table.concat(syntax) == active_fragment, "wrapping must preserve syntax offsets alongside the marker")
  end

  detail_request.item_list[1].command_list = { context(long_command, "Write-Output 'line 40'") }
  approval.open(detail_request, host)
  instance = picker._state_for_test()
  assert(instance.frame.content_offset > 0, "long context must initially reveal the command being reviewed")
  assert(marked_text(instance, "ForgePermissionTarget") == "Write-Output 'line 40'")
  invoke("<PageUp>")
  instance = picker._state_for_test()
  assert(marked_text(instance, "ForgePermissionTarget") == "", "off-screen approval markers must not move to unrelated rows")
  invoke("<PageDown>")
  assert(marked_text(picker._state_for_test(), "ForgePermissionTarget") == "Write-Output 'line 40'")
  approval.close()

  for _, width in ipairs({ 30, 60, 128 }) do
    local frame = layout.build({
      content_list = { { text = "  λ" .. string.rep("x", 200) .. "END", preformatted = true } }, option_list = {},
    }, 1, width)
    local rendered = {}
    for index = frame.content_range[1].first, frame.content_range[1].last do
      local line = frame.lines[index]
      assert(vim.fn.strdisplaywidth(line) <= width - 4, "long unbroken command text must wrap")
      rendered[#rendered + 1] = line:gsub("%s", "")
    end
    assert(table.concat(rendered) == "λ" .. string.rep("x", 200) .. "END", "wrapping must retain every character")
  end
end)

if not ok then
  print(failure)
  vim.cmd("cquit 1")
end
print("harness_approval: passed")
vim.cmd("qa!")
