---@class HarnessStatusView
---@field buffer integer
---@field window? integer
---@field native? table
---@field snapshot? table
---@field context HarnessStatusContext
---@field commands ForgeViewCommandSet
---@field options table
---@field content? table
---@field namespace integer
---@field animation_namespace integer
---@field group integer
---@field timer? uv.uv_timer_t
---@field release_geometry? fun()
---@field closed boolean
---@field suspended boolean
---@field resizing? boolean
---@field previous_window? integer
---@field separator? table
---@field details? {window: integer, origin: integer, buffer: integer}
local StatusView = {}
StatusView.__index = StatusView
local replica = require("forge.buffer")
local position = require("forge.buffer_view")
local spinner = require("forge.render.harness.timeline_status")
local hint = require("forge.views.harness.status_hint")

---@param options {transcript: table, transcript_window: integer, composer_window?: integer, alive: fun(): boolean, notice: fun(message: string), recover: fun(document: table)}
---@return HarnessStatusView
function StatusView.new(options)
  local self = setmetatable({ options = options, context = {}, commands = { action_by_id = {}, order = {} },
    buffer = vim.api.nvim_create_buf(false, true), namespace = vim.api.nvim_create_namespace(""),
    animation_namespace = vim.api.nvim_create_namespace(""),
    closed = false, suspended = false }, StatusView)
  vim.bo[self.buffer].buftype = "nofile"
  vim.bo[self.buffer].bufhidden = "hide"
  vim.bo[self.buffer].swapfile = false
  vim.bo[self.buffer].modifiable = false
  vim.bo[self.buffer].filetype = "ForgeHarnessStatus"
  self.group = vim.api.nvim_create_augroup("ForgeHarnessStatus" .. self.buffer, { clear = true })
  if options.composer_window then
    self.release_geometry = require("forge.views.harness.layout").observe_composer(options.composer_window, function() self:resize() end)
  end
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized", "TabEnter", "BufWinEnter" }, {
    group = self.group, callback = function() self:resize() end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", { group = self.group, buffer = self.buffer, once = true,
    callback = function() self:close(true) end })
  vim.api.nvim_create_autocmd("WinLeave", { group = self.group, callback = function()
    local window = vim.api.nvim_get_current_win()
    if window ~= self.window then self.previous_window = window end
  end })
  vim.api.nvim_create_autocmd("WinEnter", { group = self.group, buffer = self.buffer, callback = function()
    if vim.api.nvim_get_current_win() ~= self.window then return end
    -- Neovim 0.12 cycles into non-focusable splits. Redirect only this owned window.
    local target = self.previous_window == options.composer_window and options.transcript_window or options.composer_window
    if target and vim.api.nvim_win_is_valid(target) then vim.api.nvim_set_current_win(target) end
  end })
  return self
end

---@param snapshot table
---@param done fun(ok: boolean)
function StatusView:apply(snapshot, done)
  if self.closed then done(false) return end
  if type(snapshot) ~= "table" or snapshot.document ~= self.options.transcript.document .. ":status" then
    self.options.notice("Harness status response has an invalid document identity")
    done(false)
    return
  end
  if self.snapshot and snapshot.revision < self.snapshot.revision then done(false) return end
  if self.snapshot and self.native.status == "Applied" and snapshot.revision == self.snapshot.revision then done(true) return end
  if not self.native then
    self.native = replica.open(snapshot.document, { filetype = "ForgeHarnessStatus", notice = self.options.notice,
      recover = function() self.options.recover(self.native) end })
  end
  replica.apply_async(self.native, snapshot, function() return not self.closed and self.options.alive() end, function(result)
    if result.kind ~= "Applied" then done(false) return end
    self.snapshot = snapshot
    self:render()
    done(true)
  end)
end

---@param context? HarnessStatusContext
---@param commands? ForgeViewCommandSet
function StatusView:render(context, commands)
  if self.closed then return end
  if context then
    context.background_terminals = self.context.background_terminals
    self.context = context
  end
  if commands then self.commands = commands end
  local content = hint.compose(self.snapshot or { block = {} }, self.context, self.commands)
  if vim.deep_equal(content, self.content) then self:resize() return end
  self.content = content
  vim.bo[self.buffer].modifiable = true
  vim.api.nvim_buf_set_lines(self.buffer, 0, -1, false, #content.lines > 0 and content.lines or { "" })
  vim.bo[self.buffer].modifiable = false
  vim.api.nvim_buf_clear_namespace(self.buffer, self.namespace, 0, -1)
  for _, highlight in ipairs(content.highlights) do
    vim.api.nvim_buf_set_extmark(self.buffer, self.namespace, highlight.row, 0,
      { end_row = highlight.row + 1, hl_group = highlight.capture, hl_eol = true })
  end
  self:update_animation()
  self:resize()
end

function StatusView:update_animation()
  if self.timer then self.timer:stop() self.timer:close() self.timer = nil end
  if not vim.api.nvim_buf_is_valid(self.buffer) then return end
  vim.api.nvim_buf_clear_namespace(self.buffer, self.animation_namespace, 0, -1)
  local content = self.content
  local visible = not self.suspended and self.window and vim.api.nvim_win_is_valid(self.window)
  if content and content.animated and visible then
    local function draw()
      if self.closed or self.suspended or not self.options.alive() then return end
      vim.api.nvim_buf_set_extmark(self.buffer, self.animation_namespace, content.animated, 0,
        { id = 1, virt_text = { { spinner.frame_at(vim.uv.now()), "ForgeTimelineStatusSpinner" } }, virt_text_win_col = 0 })
    end
    self.timer = vim.uv.new_timer()
    draw()
    self.timer:start(120, 120, vim.schedule_wrap(draw))
  elseif content and content.waiting and visible then
    vim.api.nvim_buf_set_extmark(self.buffer, self.animation_namespace, content.waiting, 0,
      { id = 1, virt_text = { { "◷", "ForgeStatusHint" } }, virt_text_win_col = 0 })
  end
end

function StatusView:resize()
  if self.closed or self.resizing then return end
  local options = self.options
  if not vim.api.nvim_win_is_valid(options.transcript_window) then
    if self.window and vim.api.nvim_win_is_valid(self.window) then vim.api.nvim_win_close(self.window, true) end
    self.window = nil
    self:update_animation()
    return
  end
  local composer = options.composer_window
  local visible = not self.suspended and options.alive() and self.content and #self.content.lines > 0
    and composer and vim.api.nvim_win_is_valid(composer)
    and vim.api.nvim_win_get_tabpage(options.transcript_window) == vim.api.nvim_get_current_tabpage()
    and vim.api.nvim_win_get_buf(options.transcript_window) == options.transcript.buffer
  self.resizing = true
  local previous_window = self.window
  local retained = position.capture(options.transcript, function() return true end)
  local ok, failure = xpcall(function()
    if not visible then
      if self.window and vim.api.nvim_win_is_valid(self.window) then vim.api.nvim_win_close(self.window, true) end
      self.window = nil
      if self.separator then
        vim.wo[options.transcript_window].fillchars = self.separator.fillchars
        vim.wo[options.transcript_window].winhighlight = self.separator.winhighlight
        self.separator = nil
      end
      return
    end
    if not self.window or not vim.api.nvim_win_is_valid(self.window) then
      self.window = vim.api.nvim_open_win(self.buffer, false, {
        split = "above", win = composer, height = 1, focusable = false, mouse = false,
      })
      require("forge.views.harness.layout").configure_gutterless_window(self.window)
      local window = self.window
      vim.wo[window].wrap = true
      vim.wo[window].linebreak = true
      vim.wo[window].breakindent = true
      vim.wo[window].breakindentopt = "shift:0"
      vim.wo[window].winbar = ""
      vim.wo[window].winfixbuf = true
      vim.wo[window].winfixheight = true
      vim.wo[window].scrolloff = 0
      vim.wo[window].foldenable = false
      self.separator = { fillchars = vim.wo[options.transcript_window].fillchars,
        winhighlight = vim.wo[options.transcript_window].winhighlight }
      vim.api.nvim_win_call(options.transcript_window, function()
        local fill = vim.opt_local.fillchars:get()
        fill.horiz, fill.horizup, fill.horizdown = " ", " ", " "
        vim.opt_local.fillchars = fill
      end)
      local highlights = self.separator.winhighlight
      vim.wo[options.transcript_window].winhighlight = highlights .. (highlights ~= "" and "," or "") .. "WinSeparator:Normal"
    end
    local wanted = vim.api.nvim_win_text_height(self.window, { start_row = 0 }).all
    local available = vim.api.nvim_win_get_height(options.transcript_window) + vim.api.nvim_win_get_height(self.window) - 1
    local height = math.max(1, math.min(wanted, available))
    if vim.api.nvim_win_get_height(self.window) ~= height then vim.api.nvim_win_set_height(self.window, height) end
  end, debug.traceback)
  position.restore(options.transcript, retained)
  self.resizing = false
  if previous_window ~= self.window then self:update_animation() end
  if not ok then options.notice("Harness status layout failed: " .. failure) end
end

---@param suspended boolean
function StatusView:suspend(suspended)
  self.suspended = suspended
  self:update_animation()
  self:resize()
end

function StatusView:open_details()
  if self.closed or not self.content or #self.content.lines == 0 then return end
  if self.details and vim.api.nvim_win_is_valid(self.details.window) then vim.api.nvim_set_current_win(self.details.window) return end
  local origin = vim.api.nvim_get_current_win()
  vim.cmd("tabnew")
  local window = vim.api.nvim_get_current_win()
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(window, buffer)
  vim.bo[buffer].buftype = "nofile"
  vim.bo[buffer].bufhidden = "wipe"
  vim.bo[buffer].swapfile = false
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, self.content.lines)
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].readonly = true
  vim.wo[window].wrap = true
  vim.wo[window].winbar = "Harness status"
  self.details = { window = window, origin = origin, buffer = buffer }
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_close(window, true) end
    self.details = nil
    if vim.api.nvim_win_is_valid(origin) then vim.api.nvim_set_current_win(origin) end
  end, { buffer = buffer, silent = true, desc = "Close Harness status" })
end

---@param wiping? boolean Whether Neovim already owns display-buffer deletion.
function StatusView:close(wiping)
  if self.closed then return end
  self:suspend(true)
  self.closed = true
  if self.release_geometry then self.release_geometry() end
  if self.timer then self.timer:stop() self.timer:close() self.timer = nil end
  vim.api.nvim_del_augroup_by_id(self.group)
  if self.details and vim.api.nvim_win_is_valid(self.details.window) then vim.api.nvim_win_close(self.details.window, true) end
  if self.native then replica.close(self.native) end
  if not wiping and vim.api.nvim_buf_is_valid(self.buffer) then vim.api.nvim_buf_delete(self.buffer, { force = true }) end
end

return StatusView
