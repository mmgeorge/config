local editable = require("forge.editable")
local field_renderer = require("forge.views.picker.field")
local keymaps = require("forge.shared.keymaps")
local command_set = require("forge.shared.view_command_set")
local notifications = require("forge.infra.notifications")

---@class ForgeFormField
---@field id string
---@field label string
---@field value string
---@field multiline? boolean
---@field readonly? boolean
---@field choices? string[]|fun(values: table<string, string>, done: fun(values: string[]?, failure?: string))
---@field mark? integer
---@field row? integer

---@class ForgeFormOptions
---@field title string
---@field subtitle string
---@field fields ForgeFormField[]
---@field save fun(values: table<string, string>, done: fun(failure?: string))
---@field reload fun(done: fun(values: table<string, string>?, failure?: string))
---@field close fun()

---@class ForgeForm
---@field buffer integer
---@field window integer
---@field tab integer
---@field origin_tab integer
---@field options ForgeFormOptions
---@field fields ForgeFormField[]
---@field baseline table<string, string>
---@field editable table
---@field commands ForgeViewCommandSet
---@field namespace integer
---@field active boolean
---@field pending boolean
---@field generation integer
---@field group integer
local Form = {}
Form.__index = Form

---@return table<string, string>
function Form:values()
  local values = {}
  for _, field in ipairs(self.fields) do
    local anchor = self.editable.native and self.editable.native.anchor[field.id]
    values[field.id] = anchor and table.concat(vim.api.nvim_buf_get_text(self.buffer,
      anchor.start.row, anchor.start.column, anchor.finish.row, anchor.finish.column, {}), "\n") or field.value
  end
  return values
end

---@return boolean
function Form:dirty()
  return not vim.deep_equal(self:values(), self.baseline)
end

---@return ForgeFormField?
function Form:selected()
  if not self.active or vim.api.nvim_get_current_buf() ~= self.buffer then return end
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  for _, field in ipairs(self.fields) do
    local anchor = self.editable.native.anchor[field.id]
    local mark = vim.api.nvim_buf_get_extmark_by_id(self.buffer, self.namespace, field.mark, {})
    if anchor and row >= anchor.start.row and row <= anchor.finish.row
      or not anchor and mark[1] == row then return field end
  end
end

function Form:refresh()
  if not self.active or not vim.api.nvim_buf_is_valid(self.buffer) then return end
  local values, selected = self:values(), self:selected()
  local editing = selected and not selected.readonly and not selected.choices
  for _, field in ipairs(self.fields) do
    local anchor = self.editable.native.anchor[field.id]
    local mark = vim.api.nvim_buf_get_extmark_by_id(self.buffer, self.namespace, field.mark, {})
    local row = anchor and anchor.start.row or mark[1]
    if field.multiline then row = row - 1 end
    local dirty = values[field.id] ~= self.baseline[field.id]
    local label = field.label .. (dirty and "*: " or ": ")
    if field.choices then
      label = label .. field_renderer.render(field.value, selected == field)
    end
    vim.api.nvim_buf_set_extmark(self.buffer, self.namespace, row, 0, {
      id = field.mark, right_gravity = false,
      virt_text = { { label, "ForgeStatusLabel" } }, virt_text_pos = field.multiline and "overlay" or "inline",
    })
  end
  vim.bo[self.buffer].modifiable = editing == true
  vim.bo[self.buffer].modified = not vim.deep_equal(values, self.baseline)
  keymaps.apply_view_winbar(self.window, self.options.title, "form", self.commands,
    self.pending and "Saving…" or (vim.bo[self.buffer].modified and "Unsaved" or ""))
end

---@param values table<string, string>
function Form:replace(values)
  local selected = self.editable.native and self:selected()
  editable.detach(self.editable)
  self.editable = editable.new("form:" .. self.buffer)
  self.baseline = vim.deepcopy(values)
  local lines, anchor = { self.options.title, self.options.subtitle, "" }, {}
  vim.api.nvim_buf_clear_namespace(self.buffer, self.namespace, 0, -1)
  for _, field in ipairs(self.fields) do
    field.value = values[field.id]
    if field.multiline then lines[#lines + 1] = "" end
    local start = #lines
    local rows = field.choices and { "" } or vim.split(field.value, "\n", { plain = true })
    vim.list_extend(lines, rows)
    if not field.readonly and not field.choices then
      editable.register(self.editable, field.id, 0)
      anchor[field.id] = { start = { row = start, column = 0 },
        finish = { row = #lines - 1, column = #rows[#rows] } }
    end
    field.row = field.multiline and start - 1 or start
    lines[#lines + 1] = ""
  end
  vim.bo[self.buffer].modifiable = true
  vim.api.nvim_buf_set_lines(self.buffer, 0, -1, false, lines)
  vim.bo[self.buffer].modified = false
  for _, field in ipairs(self.fields) do
    field.mark = vim.api.nvim_buf_set_extmark(self.buffer, self.namespace, field.row, 0, { right_gravity = false })
  end
  editable.attach(self.editable, self.buffer, anchor, {
    notice = function(message) notifications.error(message, self.options.title) end,
  })
  local field = selected or self.fields[1]
  local position = self.editable.native.anchor[field.id]
  vim.api.nvim_win_set_cursor(self.window, { (position and position.start.row or field.row) + 1, 0 })
  self:refresh()
end

function Form:save()
  if self.pending or not self.active then return end
  if self.editable.fault then notifications.error(self.editable.fault, self.options.title) return end
  local values = self:values()
  for _, field in ipairs(self.fields) do
    if not field.multiline and values[field.id]:find("[\r\n]") then
      notifications.error(field.label .. " must occupy one line", self.options.title)
      return
    end
  end
  local captured = editable.capture_draft(self.editable)
  self.pending = true
  self:refresh()
  self.options.save(values, function(failure)
    if not self.active then return end
    self.pending = false
    if failure then notifications.error(failure, self.options.title)
    else
      self.baseline = values
      editable.saved_capture(self.editable, captured)
    end
    self:refresh()
  end)
end

function Form:reload()
  if self.pending or not self.active then return end
  self.generation = self.generation + 1
  local generation, captured = self.generation, self:values()
  self.options.reload(function(values, failure)
    if not self.active or generation ~= self.generation then return end
    if failure then notifications.error(failure, self.options.title)
    elseif not vim.deep_equal(captured, self:values()) then
      notifications.error("The form changed while reloading. Reload again to discard those edits", self.options.title)
    elseif values then self:replace(values) end
  end)
end

---@param direction integer
function Form:cycle(direction)
  local field = self:selected()
  if not field or not field.choices or self.pending then return end
  self.generation = self.generation + 1
  local generation, values = self.generation, self:values()
  local function choose(choices, failure)
    if not self.active or generation ~= self.generation or self:selected() ~= field
      or not vim.deep_equal(values, self:values()) then return end
    if failure then notifications.error(failure, self.options.title) return end
    if not choices or #choices == 0 then return end
    local position = direction > 0 and 0 or 1
    for index, value in ipairs(choices) do if value == field.value then position = index break end end
    field.value = choices[(position - 1 + direction) % #choices + 1]
    self:refresh()
  end
  if type(field.choices) == "function" then field.choices(values, choose) else choose(field.choices) end
end

function Form:close()
  if self.pending then notifications.error("Wait for the profile save to finish", self.options.title) return end
  if self:dirty() then
    local reload = table.concat(keymaps.view_keys_for("form", "reload"), "/")
    notifications.error("Unsaved changes. Save with :write or reload with " .. reload .. " before closing", self.options.title)
    return
  end
  self.active = false
  editable.detach(self.editable)
  vim.api.nvim_del_augroup_by_id(self.group)
  if vim.api.nvim_tabpage_is_valid(self.tab) and #vim.api.nvim_tabpage_list_wins(self.tab) == 1 then
    vim.api.nvim_set_current_tabpage(self.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_tabpage_is_valid(self.origin_tab) then vim.api.nvim_set_current_tabpage(self.origin_tab) end
  vim.api.nvim_buf_delete(self.buffer, { force = true })
  self.options.close()
end

---@param options ForgeFormOptions
---@return ForgeForm
function Form.open(options)
  local self = setmetatable({ options = options, fields = vim.deepcopy(options.fields), active = true,
    pending = false, generation = 0, origin_tab = vim.api.nvim_get_current_tabpage(), baseline = {},
    namespace = vim.api.nvim_create_namespace("ForgeForm"), editable = editable.new("form:opening") }, Form)
  self.buffer = vim.api.nvim_create_buf(true, false)
  vim.bo[self.buffer].buftype, vim.bo[self.buffer].bufhidden = "acwrite", "hide"
  vim.bo[self.buffer].swapfile, vim.bo[self.buffer].filetype = false, "ForgeForm"
  vim.api.nvim_buf_set_name(self.buffer, "forge://form/" .. self.buffer)
  vim.cmd("tab sbuffer " .. self.buffer)
  self.tab, self.window = vim.api.nvim_get_current_tabpage(), vim.api.nvim_get_current_win()
  for name, value in pairs({ number = false, relativenumber = false, signcolumn = "no", foldcolumn = "0",
    statuscolumn = "", wrap = true, linebreak = true, virtualedit = "" }) do vim.wo[self.window][name] = value end
  self.commands = command_set.new()
  for _, command in ipairs({ { "save", function() self:save() end },
    { "previous_value", function() self:cycle(-1) end }, { "next_value", function() self:cycle(1) end },
    { "reload", function() self:reload() end }, { "close", function() self:close() end },
    { "help", function() keymaps.show_view_help("form", self.commands, self.options.title) end } }) do
    command_set.register(self.commands, command[1], command[2])
  end
  keymaps.setup_view_keymaps(self.buffer, "form", self.commands)
  self.group = vim.api.nvim_create_augroup("ForgeForm" .. self.buffer, { clear = true })
  vim.api.nvim_create_autocmd("BufWriteCmd", { group = self.group, buffer = self.buffer, callback = function() self:save() end })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter", "BufWinEnter", "InsertEnter", "InsertLeave", "TextChanged", "TextChangedI" }, {
    group = self.group, buffer = self.buffer, callback = function() self:refresh() end,
  })
  vim.api.nvim_create_autocmd("WinResized", { group = self.group, callback = function() self:refresh() end })
  vim.api.nvim_create_autocmd("BufWipeout", { group = self.group, buffer = self.buffer, callback = function()
    self.active = false
    editable.detach(self.editable)
    pcall(vim.api.nvim_del_augroup_by_id, self.group)
  end })
  vim.keymap.set("i", "<CR>", function()
    local field = self:selected()
    return field and field.multiline and "<CR>" or ""
  end, { buffer = self.buffer, expr = true, desc = "Insert a line in multiline fields" })
  vim.keymap.set("n", "i", function()
    local field = self:selected()
    return field and not field.multiline and "0i" or "i"
  end, { buffer = self.buffer, expr = true, desc = "Insert at field input" })
  for _, key in ipairs({ "o", "O" }) do
    vim.keymap.set("n", key, function()
      local field = self:selected()
      if not field or not field.multiline then return "" end
      local anchor = self.editable.native.anchor[field.id]
      if key == "o" and vim.api.nvim_win_get_cursor(0)[1] - 1 == anchor.finish.row then return "A<CR>" end
      return key
    end, { buffer = self.buffer, expr = true, desc = "Open a line in multiline fields" })
  end
  local values = {}
  for _, field in ipairs(self.fields) do values[field.id] = field.value end
  self:replace(values)
  return self
end

return Form
